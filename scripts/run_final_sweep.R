#!/usr/bin/env Rscript
# ---------------------------------------------------------------------------
# run_final_sweep.R  (the last compute batch)
#
# Two policies the review requires, on the pinned 4,000-series subsample,
# across all horizons, in ONE invocation:
#
#   fixed_f4                      the rate-matched cadence (~9 searches/series,
#                                 matching the Trigg firing rate), so the
#                                 monitor-vs-clock comparison can be made at
#                                 matched intervention rates (review 3.2)
#   scoregate_aicc_cap8_tau0.8    the score-gap gate that re-selects by AICc
#                                 on fire instead of deploying the validation
#                                 winner, so detector and selector are no
#                                 longer confounded (review 3.1)
#
# Usage (one command, sequential over horizons, chunked, resumable):
#   OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 MKL_NUM_THREADS=1 \
#   Rscript scripts/run_final_sweep.R --data_dir data \
#     --series_ids outputs/horizon_subsample_ids.txt \
#     --horizons 3,6,9,12,18 --n_jobs 8 --chunk 250
#
# Writes outputs/m4_final_hXX/{parts/,progress.log,records.csv,summary.csv}.
# Rerunning the same command resumes from completed chunks.
# ---------------------------------------------------------------------------

suppressWarnings(suppressMessages({
  a <- commandArgs(FALSE)
  f <- sub("^--file=", "", a[grepl("^--file=", a)])
  root <- if (length(f)) dirname(dirname(normalizePath(f[[1]]))) else getwd()
  core <- file.path(root, "R", "adaptive_update.R")
  if (!file.exists(core)) core <- file.path(getwd(), "R", "adaptive_update.R")
  source(core)
}))
requireNamespace("data.table", quietly = TRUE)

Sys.setenv(OMP_NUM_THREADS = "1", OPENBLAS_NUM_THREADS = "1",
           MKL_NUM_THREADS = "1", VECLIB_MAXIMUM_THREADS = "1")

read_ids <- function(path) {
  ids <- trimws(readLines(path, warn = FALSE))
  ids <- ids[nzchar(ids)]
  if (length(ids) &&
      grepl("^(series_id|id|unique_id|m4id)$", ids[[1]], ignore.case = TRUE)) ids <- ids[-1]
  unique(ids)
}

# ---------------------------------------------------------------------------
# Score-gap gate with AICc re-selection on fire (selector-deconfounded).
# Gate logic mirrors the paper's Section 4.2 timeline: at every sixth origin
# (global anchor, idx %% monitor_every == 0), split the 36-observation window
# into a 24-observation base and a 12-observation validation tail, refit the
# deployed form and every candidate on the base, score all on the tail
# (MASE, scale from the base), and fire when the deployed-minus-best gap
# exceeds tau and the best challenger differs from the deployed form. The ONE
# difference from run_adaptive_capped_score: on fire, the form is re-selected
# by AICc on the FULL window, exactly as scheduled and cap-driven
# re-specifications are, so the only thing this policy changes relative to
# fixed_f8 or the Trigg monitor is WHEN it re-selects, never HOW.
# Validation fitting time is charged to the policy. Cap nests the cadence.
# ---------------------------------------------------------------------------
run_scoregate_aicc <- function(series_id, y, cfg, threshold = 0.8, max_age = 8L) {
  y <- clean_numeric(y)
  origins <- make_origins(y, cfg$train_length, cfg$horizon, cfg$n_rounds)
  if (length(origins) == 0L) return(data.frame())
  records <- vector("list", length(origins))
  policy_name <- paste0("scoregate_aicc_cap", max_age, "_tau",
                        format(threshold, trim = TRUE, scientific = FALSE))
  mw <- cfg$monitor_window
  current_form <- NULL
  last_respec_idx <- NA_integer_

  for (idx in seq_along(origins)) {
    origin <- origins[[idx]]
    train <- training_window(y, origin, cfg)
    truth <- y[origin:(origin + cfg$horizon - 1L)]
    extra_seconds <- 0

    respecified <- FALSE
    trigger_reason <- "none"
    triggered_by_score <- FALSE
    triggered_by_cap <- FALSE
    gap <- NA_real_
    age_before_action <- if (is.na(last_respec_idx)) NA_integer_ else as.integer(idx - last_respec_idx)

    due_to_age <- !is.na(last_respec_idx) && ((idx - last_respec_idx) >= max_age)
    monitored <- !is.null(current_form) && (idx %% cfg$monitor_every == 0L) &&
      length(train) >= mw + 2L * cfg$seasonality

    fire <- FALSE
    if (monitored) {
      base <- utils::head(train, length(train) - mw)
      tail_truth <- utils::tail(train, mw)
      dep_fit <- fit_form_es(base, current_form, mw, cfg$seasonality, cfg$clip_nonnegative)
      extra_seconds <- extra_seconds + dep_fit$fit_seconds
      dep_loss <- metric_value(cfg$metric, base, tail_truth, dep_fit$forecast, cfg$seasonality)
      cands <- ets_candidate_models(base, allow_multiplicative = cfg$allow_multiplicative)
      best_loss <- Inf; best_form <- NA_character_
      for (cand in cands) {
        cf <- try(fit_form_es(base, cand, mw, cfg$seasonality, cfg$clip_nonnegative), silent = TRUE)
        if (inherits(cf, "try-error")) next
        extra_seconds <- extra_seconds + cf$fit_seconds
        cl <- metric_value(cfg$metric, base, tail_truth, cf$forecast, cfg$seasonality)
        if (is.finite(cl) && cl < best_loss) { best_loss <- cl; best_form <- cand }
      }
      if (is.finite(dep_loss) && is.finite(best_loss)) {
        gap <- dep_loss - best_loss
        fire <- (gap > threshold) && !identical(best_form, current_form)
      }
    }

    if (is.null(current_form)) {
      outcome <- fit_auto_es(train, cfg$horizon, cfg$seasonality,
                             cfg$allow_multiplicative, cfg$clip_nonnegative)
      current_form <- outcome$form
      respecified <- TRUE; trigger_reason <- "initial"; last_respec_idx <- idx
    } else if (due_to_age || fire) {
      outcome <- fit_auto_es(train, cfg$horizon, cfg$seasonality,
                             cfg$allow_multiplicative, cfg$clip_nonnegative)
      current_form <- outcome$form
      respecified <- TRUE; last_respec_idx <- idx
      if (fire) { triggered_by_score <- TRUE; trigger_reason <- "score" }
      else      { triggered_by_cap <- TRUE;  trigger_reason <- "cap" }
    } else {
      outcome <- fit_form_es(train, current_form, cfg$horizon,
                             cfg$seasonality, cfg$clip_nonnegative)
    }

    loss <- metric_value(cfg$metric, train, truth, outcome$forecast, cfg$seasonality)
    records[[idx]] <- make_record(
      series_id, policy_name, idx - 1L, origin, current_form, truth, outcome$forecast,
      loss, outcome$fit_seconds + extra_seconds, respecified,
      score_gap = gap, threshold = threshold, tau_cost_ratio = threshold,
      trigger_reason = trigger_reason,
      triggered_by_score = triggered_by_score, triggered_by_cap = triggered_by_cap,
      age_before_action = age_before_action)
  }
  data.table::rbindlist(records, fill = TRUE)
}

run_pair_for_series <- function(series_id, values, cfg) {
  y <- clean_numeric(values)
  if (length(y) < cfg$train_length + cfg$horizon) return(data.frame())
  data.table::rbindlist(list(
    run_fixed_frequency(series_id, y, cfg, 4L),
    run_scoregate_aicc(series_id, y, cfg, threshold = 0.8, max_age = 8L)
  ), fill = TRUE)
}

# ------------------------------- driver ------------------------------------
args    <- parse_cli_args()
data_dir <- get_arg(args, "data_dir", "data")
ids_path <- get_arg(args, "series_ids", "outputs/horizon_subsample_ids.txt")
horizons <- as.integer(strsplit(get_arg(args, "horizons", "3,6,9,12,18"), ",")[[1]])
n_jobs   <- get_arg(args, "n_jobs", 8L, "integer")
chunk_sz <- get_arg(args, "chunk", 250L, "integer")

keep <- read_ids(ids_path)
message("Pinned subsample: ", length(keep), " series from ", ids_path)

max_h <- max(horizons)
items_all <- load_m4_monthly(data_dir = data_dir, n_series = -1L, seed = 123L,
                             min_obs = 36L + 36L + max_h - 1L, include_test = TRUE)
present <- vapply(items_all, function(it) as.character(it$series_id), character(1))
items <- items_all[present %in% keep]
message("Matched ", length(items), " of ", length(keep), " pinned series.")
if (length(items) == 0L) stop("No pinned series matched the data.")

for (H in horizons) {
  out_dir <- sprintf("outputs/m4_final_h%02d", H)
  ensure_dir(out_dir); parts_dir <- file.path(out_dir, "parts"); ensure_dir(parts_dir)
  progress_file <- file.path(out_dir, "progress.log")
  cfg <- new_rolling_config(horizon = H, seasonality = 12L, train_length = 36L,
                            n_rounds = 36L, fixed_window = TRUE, metric = "mase",
                            allow_multiplicative = TRUE, clip_nonnegative = FALSE,
                            monitor_window = 12L, monitor_every = 6L)
  total <- length(items)
  chunks <- split(seq_len(total), ceiling(seq_len(total) / max(1L, chunk_sz)))
  t0 <- Sys.time(); done <- 0L; nfail <- 0L
  cat(sprintf("[%s] h%02d start: %d series, %d workers, %d chunks\n",
              format(t0, "%H:%M:%S"), H, total, n_jobs, length(chunks)),
      file = progress_file, append = FALSE)
  for (ci in seq_along(chunks)) {
    part_path <- file.path(parts_dir, sprintf("part_%05d.csv", ci))
    idx <- chunks[[ci]]
    if (file.exists(part_path)) { done <- done + length(idx); next }
    res <- parallel_lapply(items[idx], function(it) {
      out <- try(run_pair_for_series(it$series_id, it$values, cfg), silent = TRUE)
      if (inherits(out, "try-error") || is.null(out) || nrow(out) == 0L)
        list(ok = FALSE) else list(ok = TRUE, rec = out)
    }, n_jobs = n_jobs)
    ok <- vapply(res, function(r) is.list(r) && isTRUE(r$ok), logical(1))
    nfail <- nfail + sum(!ok)
    if (any(ok)) data.table::fwrite(
      data.table::rbindlist(lapply(res[ok], `[[`, "rec"), fill = TRUE), part_path)
    done <- done + length(idx)
    el <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
    line <- sprintf("[%s] h%02d %d/%d (%.1f%%) | %.2f series/s | ETA %.1fm | failed %d\n",
                    format(Sys.time(), "%H:%M:%S"), H, done, total, 100 * done / total,
                    done / max(el, 1e-9), (total - done) / max(done / max(el, 1e-9), 1e-9) / 60, nfail)
    cat(line, file = progress_file, append = TRUE); message(sub("\n$", "", line))
  }
  pf <- sort(list.files(parts_dir, pattern = "^part_[0-9]+\\.csv$", full.names = TRUE))
  recs <- data.table::rbindlist(lapply(pf, data.table::fread), fill = TRUE)
  data.table::fwrite(recs, file.path(out_dir, "records.csv"))
  data.table::fwrite(summarize_records(recs), file.path(out_dir, "summary.csv"))
  message("h", H, " done: ", file.path(out_dir, "summary.csv"))
}
message("Final sweep complete.")
