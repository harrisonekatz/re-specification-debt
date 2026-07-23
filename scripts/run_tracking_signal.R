#!/usr/bin/env Rscript
# ---------------------------------------------------------------------------
# run_tracking_signal.R
#
# Adds classical, one-parameter monitoring triggers to the re-specification
# comparison, per Fotios's suggestion (Trigg 1964; Brown 1963; calibration in
# Cohen, Garman & Gorr 2009). The point is the complexity question: how much
# machinery does the "when to re-specify" decision actually need, with a
# fixed-length update as the benchmark?
#
# Three rungs of a complexity ladder end up in one summary:
#   1. fixed cadence            (no monitoring at all)
#   2. classical tracking signal (Trigg / Brown; one smoothing constant,
#                                 runs on the deployed model's own one-step
#                                 error stream, ~zero extra fitting)
#   3. evidence-gated score gap  (the paper's cap8/tau0.8 trigger; fits the
#                                 full candidate grid at each monitored origin)
#
# The tracking signal only says "the deployed form is out of control." On a
# fire it re-selects by AICc on the full window, exactly like a scheduled or
# capped re-spec, so the ONLY thing that differs from a fixed cadence is the
# timing of the re-selection. That isolates timing, the same logic as the
# paper's Appendix C.2.
#
# Everything reuses adaptive_update.R (loader, ETS grid, MASE, sMAPC, record
# schema, summary), so the output is directly comparable to Table 2.
#
# Usage:
#   Rscript scripts/run_tracking_signal.R --data_dir data \
#       --out_dir outputs/m4_tracking_signal --n_series 300 --n_jobs 4
#   # --n_series -1 runs the full monthly set.
# ---------------------------------------------------------------------------

suppressWarnings(suppressMessages({
  # Locate and source the core implementation regardless of working directory.
  .this_file <- tryCatch({
    a <- commandArgs(FALSE)
    f <- sub("^--file=", "", a[grepl("^--file=", a)])
    if (length(f)) normalizePath(f[[1]]) else NA_character_
  }, error = function(e) NA_character_)
  .root <- if (!is.na(.this_file)) dirname(dirname(.this_file)) else getwd()
  .core <- file.path(.root, "R", "adaptive_update.R")
  if (!file.exists(.core)) .core <- file.path(getwd(), "R", "adaptive_update.R")
  source(.core, chdir = FALSE)
}))

# ---------------------------------------------------------------------------
# Tracking-signal state. Scale-free by construction (a ratio), so a single
# control limit is comparable across every series, the same way a single tau
# is comparable for the score gap.
# ---------------------------------------------------------------------------

new_ts_state <- function() list(E = 0.0, M = 0.0, Q = 0.0, n = 0L)

# Update with one realised one-step forecast error. Non-finite errors are
# skipped and leave the state untouched. The first error seeds the smoothers.
update_ts_state <- function(state, e, a) {
  if (!is.finite(e)) return(state)
  ae <- abs(e)
  if (state$n == 0L) {
    state$E <- e
    state$M <- ae
    state$Q <- e
  } else {
    state$E <- a * e + (1 - a) * state$E   # Trigg: EWMA of signed error
    state$M <- a * ae + (1 - a) * state$M  # EWMA of absolute error (smoothed MAD)
    state$Q <- state$Q + e                 # Brown: cumulative signed error
  }
  state$n <- state$n + 1L
  state
}

# Tracking signal in [-1, 1] (Trigg) or roughly so (Brown). NA until it has a
# positive MAD to divide by.
ts_value <- function(state, method) {
  if (!is.finite(state$M) || state$M <= 0) return(NA_real_)
  num <- if (identical(method, "brown")) state$Q else state$E
  num / state$M
}

# ---------------------------------------------------------------------------
# Policy: keep the deployed form, refit its parameters every period, and watch
# a tracking signal built from the deployed model's own one-step out-of-sample
# errors. When |tracking signal| exceeds the control limit (threshold), re-select
# the form by AICc on the full window and reset the monitor.
#
# Causality: the signal that gates origin t is built only from errors observed
# strictly before t. This origin's error is folded in after the forecast, and
# is used from the next origin onward. No lookahead.
#
# max_age = NA gives a pure trigger; an integer caps the age so the policy nests
# a fixed cadence, mirroring the capped score-gap policy.
# ---------------------------------------------------------------------------

run_adaptive_tracking_signal <- function(series_id, y, cfg, threshold,
                                         method = c("trigg", "brown"),
                                         smoothing = 0.2,
                                         max_age = NA_integer_,
                                         warmup = 3L) {
  requireNamespace("data.table", quietly = TRUE)
  method <- match.arg(method)
  y <- clean_numeric(y)
  origins <- make_origins(y, cfg$train_length, cfg$horizon, cfg$n_rounds)
  records <- vector("list", length(origins))
  if (length(origins) == 0L) return(data.frame())

  threshold <- as.numeric(threshold)
  a <- as.numeric(smoothing)
  warmup <- max(1L, as.integer(warmup))
  cap_int <- suppressWarnings(as.integer(max_age))
  capped <- length(cap_int) == 1L && !is.na(cap_int)
  cap <- if (capped) max(1L, cap_int) else NA_integer_
  cap_tag <- if (capped) paste0("_cap", cap) else ""
  policy_name <- paste0(method, cap_tag, "_tau",
                        format(threshold, trim = TRUE, scientific = FALSE))

  current_form <- NULL
  last_respec_idx <- NA_integer_
  state <- new_ts_state()

  for (idx in seq_along(origins)) {
    origin <- origins[[idx]]
    train <- training_window(y, origin, cfg)
    truth <- y[origin:(origin + cfg$horizon - 1L)]

    respecified <- FALSE
    triggered_by_score <- FALSE   # repurposed here: the monitor fired
    triggered_by_cap <- FALSE
    trigger_reason <- "none"
    age_before_action <- if (is.na(last_respec_idx)) NA_integer_ else as.integer(idx - last_respec_idx)

    # Statistic from errors strictly before this origin.
    ts_stat <- ts_value(state, method)
    due_to_age <- capped && !is.na(last_respec_idx) && ((idx - last_respec_idx) >= cap)
    fire_monitor <- is.finite(ts_stat) && state$n >= warmup && abs(ts_stat) > threshold

    if (is.null(current_form)) {
      outcome <- fit_auto_es(train, cfg$horizon, cfg$seasonality,
                             cfg$allow_multiplicative, cfg$clip_nonnegative)
      current_form <- outcome$form
      respecified <- TRUE
      trigger_reason <- "initial"
      last_respec_idx <- idx
      state <- new_ts_state()
    } else if (due_to_age || fire_monitor) {
      # A monitor fire and a cap both re-select by AICc on the full window,
      # identical to a scheduled re-spec. Only the timing differs.
      outcome <- fit_auto_es(train, cfg$horizon, cfg$seasonality,
                             cfg$allow_multiplicative, cfg$clip_nonnegative)
      current_form <- outcome$form
      respecified <- TRUE
      if (fire_monitor) {
        triggered_by_score <- TRUE
        trigger_reason <- "monitor"
      } else {
        triggered_by_cap <- TRUE
        trigger_reason <- "cap"
      }
      last_respec_idx <- idx
      state <- new_ts_state()   # fresh form starts with a clean monitor
    } else {
      outcome <- fit_form_es(train, current_form, cfg$horizon,
                             cfg$seasonality, cfg$clip_nonnegative)
    }

    loss <- metric_value(cfg$metric, train, truth, outcome$forecast, cfg$seasonality)

    # Fold in this origin's one-step out-of-sample error (used from next origin).
    e1 <- suppressWarnings(as.numeric(truth[[1]]) - as.numeric(outcome$forecast[[1]]))
    state <- update_ts_state(state, e1, a)

    records[[idx]] <- make_record(
      series_id, policy_name, idx - 1L, origin, current_form, truth, outcome$forecast,
      loss, outcome$fit_seconds, respecified,
      score_gap = ts_stat,           # store the monitor statistic that gated this origin
      threshold = threshold,
      tau_cost_ratio = threshold,
      trigger_reason = trigger_reason,
      triggered_by_score = triggered_by_score,
      triggered_by_cap = triggered_by_cap,
      age_before_action = age_before_action
    )
  }

  data.table::rbindlist(records, fill = TRUE)
}

# ---------------------------------------------------------------------------
# Per-series comparison set: the two anchors, a few fixed cadences, the paper's
# evidence-gated score-gap trigger, and the classical monitors (Trigg and Brown,
# capped and uncapped, over a small control-limit sweep).
# ---------------------------------------------------------------------------

run_tracking_comparison_for_series <- function(
    series_id, values, cfg,
    fixed_frequencies = c(6L, 8L, 12L),
    score_gap_thresholds = 0.8, score_gap_caps = 8L,
    ts_thresholds = c(0.40, 0.50, 0.60), ts_smoothing = 0.2,
    ts_caps = list(NA_integer_, 8L),
    ts_methods = c("trigg", "brown"),
    include_full_update = TRUE) {

  requireNamespace("data.table", quietly = TRUE)
  y <- clean_numeric(values)
  if (length(y) < cfg$train_length + cfg$horizon) return(data.frame())

  blocks <- list()
  if (isTRUE(include_full_update)) {
    blocks[[length(blocks) + 1L]] <- run_full_update(series_id, y, cfg)
  }
  blocks[[length(blocks) + 1L]] <- run_parameter_only(series_id, y, cfg)
  for (f in fixed_frequencies) {
    if (as.integer(f) != 1L) {
      blocks[[length(blocks) + 1L]] <- run_fixed_frequency(series_id, y, cfg, f)
    }
  }
  # Complex comparator: the paper's score-gap trigger.
  for (cap in score_gap_caps) {
    for (tau in score_gap_thresholds) {
      blocks[[length(blocks) + 1L]] <- run_adaptive_capped_score(
        series_id, y, cfg, threshold = tau, max_age = cap)
    }
  }
  # Classical monitors.
  for (m in ts_methods) {
    for (cap in ts_caps) {
      for (tau in ts_thresholds) {
        blocks[[length(blocks) + 1L]] <- run_adaptive_tracking_signal(
          series_id, y, cfg, threshold = tau, method = m,
          smoothing = ts_smoothing, max_age = cap)
      }
    }
  }
  data.table::rbindlist(blocks, fill = TRUE)
}

# ---------------------------------------------------------------------------
# Driver
# ---------------------------------------------------------------------------

# Read a fixed set of series IDs to pin the run to (so the horizon sweep uses
# the same subsample at every horizon, the way the paper's Table 3 does).
# Accepts a plain newline list or a CSV with a series_id-style column, and will
# read straight from a run's records.csv (it just pulls the unique IDs).
read_series_ids <- function(path) {
  if (!file.exists(path)) stop("series_ids file not found: ", path)
  first <- tryCatch(readLines(path, n = 1L, warn = FALSE), error = function(e) "")
  if (grepl(",", first)) {
    dt <- data.table::fread(path)
    idcol <- intersect(c("series_id", "id", "unique_id", "M4id"), names(dt))
    ids <- as.character(if (length(idcol)) dt[[idcol[[1]]]] else dt[[1]])
  } else {
    ids <- readLines(path, warn = FALSE)
    if (length(ids) &&
        grepl("^(series_id|id|unique_id|m4id)$", trimws(ids[[1]]), ignore.case = TRUE)) {
      ids <- ids[-1]
    }
  }
  ids <- trimws(ids)
  unique(ids[nzchar(ids)])
}

# Recompute the relative columns against a chosen reference policy, from the
# absolute columns summarize_records already produced. Needed when full_update
# is dropped (the lean sweep), so the numbers rebase on fixed_f8 instead.
rescale_summary <- function(summary, reference_policy = "full_update",
                            alpha = 0, gamma = 0) {
  S <- data.table::as.data.table(data.table::copy(summary))
  ref <- S[policy == reference_policy]
  if (nrow(ref) == 0L) ref <- S[1L]
  rl <- as.numeric(ref$mean_loss[1L])
  rt <- as.numeric(ref$total_fit_seconds[1L])
  ri <- as.numeric(ref$mean_instability[1L])
  S[, relative_loss := if (is.finite(rl) && rl > 0) mean_loss / rl else NA_real_]
  S[, relative_time := if (is.finite(rt) && rt > 0) total_fit_seconds / rt else NA_real_]
  S[, relative_instability := if (is.finite(ri) && ri > 0) mean_instability / ri else NA_real_]
  tc <- S$relative_loss +
    (if (as.numeric(alpha) != 0) as.numeric(alpha) * S$relative_time else 0) +
    (if (as.numeric(gamma) != 0) as.numeric(gamma) * S$relative_instability else 0)
  S[, total_cost_index := tc]
  data.table::setorder(S, total_cost_index, relative_loss, relative_time)
  S[]
}

run_tracking_experiment <- function(
    data_dir = "data",
    out_dir = "outputs/m4_tracking_signal",
    series_ids = NULL,
    n_series = 300L,
    seed = 123L,
    n_rounds = 36L,
    horizon = 3L,
    seasonality = 12L,
    train_length = 36L,
    n_jobs = 1L,
    ts_smoothing = 0.2,
    ts_thresholds = c(0.40, 0.50, 0.60),
    fixed_frequencies = c(6L, 8L, 12L),
    ts_caps = list(NA_integer_, 8L),
    ts_methods = c("trigg", "brown"),
    score_gap_thresholds = 0.8,
    score_gap_caps = 8L,
    include_full_update = TRUE,
    reference_policy = "full_update",
    lean = FALSE,
    progress_chunk = 500L,
    alpha = 0.0,
    gamma = 0.0) {

  check_required_packages()
  requireNamespace("data.table", quietly = TRUE)

  # Prevent the fork + threaded-BLAS deadlock that hangs mclapply at ~0% CPU:
  # force single-threaded linear algebra in the workers. Setting these at launch
  # (OMP_NUM_THREADS=1 etc.) is the reliable lever; this is a runtime backstop.
  Sys.setenv(OMP_NUM_THREADS = "1", OPENBLAS_NUM_THREADS = "1",
             MKL_NUM_THREADS = "1", VECLIB_MAXIMUM_THREADS = "1")
  try(if (requireNamespace("RhpcBLASctl", quietly = TRUE)) {
    RhpcBLASctl::blas_set_num_threads(1L)
    RhpcBLASctl::omp_set_num_threads(1L)
  }, silent = TRUE)

  if (isTRUE(lean)) {
    include_full_update <- FALSE
    fixed_frequencies   <- c(8L)
    ts_caps             <- list(8L)
    ts_thresholds       <- c(0.50, 0.60)
    ts_methods          <- c("trigg", "brown")
    score_gap_thresholds <- 0.8
    score_gap_caps      <- 8L
    if (identical(reference_policy, "full_update")) reference_policy <- "fixed_f8"
    message("Lean profile: full_update dropped. Policies = parameter_only, fixed_f8, ",
            "adaptive_cap8_tau0.8, trigg_cap8 {0.50,0.60}, brown_cap8 {0.50,0.60}. ",
            "Relative numbers rebased on ", reference_policy, ".")
  }

  min_obs_needed <- as.integer(train_length) + as.integer(n_rounds) + as.integer(horizon) - 1L
  items <- load_m4_monthly(
    data_dir = data_dir, n_series = -1L, seed = seed,
    min_obs = min_obs_needed, include_test = TRUE
  )
  if (!is.null(series_ids) && nzchar(series_ids)) {
    keep <- read_series_ids(series_ids)
    present <- vapply(items, function(it) as.character(it$series_id), character(1))
    items <- items[present %in% keep]
    missing <- setdiff(keep, present)
    message("Pinned to ", length(items), " of ", length(keep), " requested series.")
    if (length(missing)) {
      message("  ", length(missing), " requested IDs not eligible/found (e.g. ",
              paste(utils::head(missing, 3L), collapse = ", "), ").")
    }
  } else if (!is.null(n_series) && n_series > 0L && n_series < length(items)) {
    set.seed(seed)
    items <- items[sort(sample(seq_along(items), n_series))]
  }
  if (length(items) == 0L) stop("No M4 items loaded. Check data and configuration.")

  cfg <- new_rolling_config(
    horizon = horizon, seasonality = seasonality, train_length = train_length,
    n_rounds = n_rounds, fixed_window = TRUE, metric = "mase",
    allow_multiplicative = TRUE, clip_nonnegative = FALSE,
    monitor_window = 12L, monitor_every = 6L
  )

  message("Tracking-signal comparison on ", length(items), " series ",
          "(smoothing = ", ts_smoothing, ", limits = ",
          paste(ts_thresholds, collapse = ", "), ").")

  ensure_dir(out_dir)
  parts_dir <- file.path(out_dir, "parts")
  ensure_dir(parts_dir)
  progress_file <- file.path(out_dir, "progress.log")

  run_one <- function(item) {
    out <- try(run_tracking_comparison_for_series(
      series_id = item$series_id, values = item$values, cfg = cfg,
      fixed_frequencies = fixed_frequencies,
      score_gap_thresholds = score_gap_thresholds, score_gap_caps = score_gap_caps,
      ts_thresholds = ts_thresholds, ts_smoothing = ts_smoothing,
      ts_caps = ts_caps, ts_methods = ts_methods,
      include_full_update = include_full_update
    ), silent = TRUE)
    if (inherits(out, "try-error") || is.null(out) || nrow(out) == 0L) {
      return(list(ok = FALSE, id = as.character(item$series_id)))
    }
    list(ok = TRUE, rec = out)
  }

  total <- length(items)
  chunk_size <- max(1L, min(as.integer(progress_chunk), total))
  chunks <- split(seq_len(total), ceiling(seq_len(total) / chunk_size))
  t_start <- Sys.time()
  done <- 0L
  n_failed <- 0L
  ping <- function(msg) { cat(msg, file = progress_file, append = TRUE); message(sub("\n$", "", msg)) }
  cat(sprintf("[%s] start: %d series, %d workers, %d chunks of <=%d\n",
              format(t_start, "%H:%M:%S"), total, n_jobs, length(chunks), chunk_size),
      file = progress_file, append = FALSE)

  for (ci in seq_along(chunks)) {
    idx <- chunks[[ci]]
    part_path <- file.path(parts_dir, sprintf("part_%05d.csv", ci))
    if (file.exists(part_path)) { done <- done + length(idx); next }  # resume
    res <- parallel_lapply(items[idx], run_one, n_jobs = n_jobs)
    ok <- vapply(res, function(r) is.list(r) && isTRUE(r$ok), logical(1))
    n_failed <- n_failed + sum(!ok)
    if (any(ok)) {
      part_dt <- data.table::rbindlist(lapply(res[ok], `[[`, "rec"), fill = TRUE)
      data.table::fwrite(part_dt, part_path)
    }
    done <- done + length(idx)
    el <- as.numeric(difftime(Sys.time(), t_start, units = "secs"))
    rate <- done / max(el, 1e-9)
    eta_m <- (total - done) / max(rate, 1e-9) / 60
    ping(sprintf("[%s] %d/%d (%.1f%%) | %.2f series/s | elapsed %.1fm | ETA %.1fm | failed %d\n",
                 format(Sys.time(), "%H:%M:%S"), done, total, 100 * done / total,
                 rate, el / 60, eta_m, n_failed))
  }

  part_files <- sort(list.files(parts_dir, pattern = "^part_[0-9]+\\.csv$", full.names = TRUE))
  if (length(part_files) == 0L) stop("No records produced (every series failed).")
  records <- data.table::rbindlist(lapply(part_files, data.table::fread), fill = TRUE)
  if (nrow(records) == 0L) stop("No records produced.")

  summary <- summarize_records(records, alpha = alpha, gamma = gamma)
  summary <- rescale_summary(summary, reference_policy, alpha, gamma)
  data.table::fwrite(records, file.path(out_dir, "records.csv"))
  data.table::fwrite(summary, file.path(out_dir, "summary.csv"))
  write_latex_table(summary, file.path(out_dir, "summary_table.tex"))

  message("Wrote ", file.path(out_dir, "records.csv"))
  message("Wrote ", file.path(out_dir, "summary.csv"))
  print(as.data.frame(summary)[, c("policy", "relative_loss", "relative_time",
                                   "relative_instability",
                                   "mean_respecifications_per_series")])
  invisible(summary)
}

# ---------------------------------------------------------------------------
# CLI
# ---------------------------------------------------------------------------

if (!interactive()) {
  args <- parse_cli_args()
  run_tracking_experiment(
    data_dir     = get_arg(args, "data_dir", "data"),
    out_dir      = get_arg(args, "out_dir", "outputs/m4_tracking_signal"),
    series_ids   = get_arg(args, "series_ids", NULL, "character"),
    n_series     = get_arg(args, "n_series", 300L, "integer"),
    seed         = get_arg(args, "seed", 123L, "integer"),
    n_rounds     = get_arg(args, "n_rounds", 36L, "integer"),
    horizon      = get_arg(args, "horizon", 3L, "integer"),
    seasonality  = get_arg(args, "seasonality", 12L, "integer"),
    train_length = get_arg(args, "train_length", 36L, "integer"),
    n_jobs       = get_arg(args, "n_jobs", 1L, "integer"),
    ts_smoothing = get_arg(args, "smoothing", 0.2, "numeric"),
    progress_chunk = get_arg(args, "chunk", 500L, "integer"),
    lean         = as.logical(get_arg(args, "lean", FALSE, "logical")),
    reference_policy = get_arg(args, "reference", "full_update", "character")
  )
}
