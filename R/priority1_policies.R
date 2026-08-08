# priority1_policies.R
#
# Additions for the Priority 1 run of the July 23 review: align the headline
# policies by running the AICc-on-fire score gate against the Trigg monitor
# and the fixed cadences on the full horizon-18-eligible M4 set, with cost,
# search, and form-change accounting.
#
# Source AFTER R/adaptive_update.R. Nothing in that file is modified; the two
# new policies reuse its fitting, monitoring, windowing, and record schema so
# every shared quantity is computed by the same code path as the published
# runs.
#
# New policies
#
#   aicc_gate_cap{A}_tau{T}
#     Detector identical to adaptive_cap{A}_tau{T}: at each monitored origin
#     (every cfg$monitor_every-th origin), split the 36-observation training
#     window into a base of 24 and a validation tail of 12, refit the deployed
#     form on the base, fit every candidate on the base, score everything on
#     the tail, and fire when (deployed loss - best challenger loss) > tau and
#     the challenger differs from the deployed form. The INTERVENTION differs:
#     on fire the form is re-selected by AICc on the full training window (a
#     second complete grid fit on all 36 observations; no computation is
#     reused from the 24-observation validation fits, and both searches are
#     charged to the policy's fit_seconds). The AICc pick may equal the
#     deployed form, which logs as a search without a form change. Age resets
#     on any completed full-window search (initial, cap, or fire), so the
#     capped gate shares its initialization and calendar with fixed_f{A}
#     whenever the trigger stays quiet, exactly as the validation gate does.
#
#   trigg_cap{A}_tau{L}
#     One-parameter tracking signal on the deployed model's own one-step
#     errors. At origin t the error realized since the previous origin is
#     e = y[origin_{t-1}] - f1(origin_{t-1}), the lead-one forecast error of
#     whatever the policy deployed there. Smoothed error E and smoothed
#     absolute error M update with constant alpha; the signal is T = E / M.
#     When |T| >= the control limit the form is re-selected by AICc on the
#     full training window, exactly as a scheduled re-specification would be,
#     and the cap nests the fixed cadence just as the capped score-gap policy
#     does. Monitoring costs no model fits and is checked every
#     signal_every-th origin (default every origin).
#
#     Mechanics match scripts/run_tracking_signal.R behind the manuscript's
#     Section 4.5 exactly: the first error after a reset seeds the smoothers
#     directly (E = e, M = |e|, so the ratio starts at +/-1), later errors
#     update by EWMA with smoothing constant alpha (0.2 in the paper), the
#     monitor is silent for a warmup of three errors since the last reset,
#     the whole state zeroes whenever the form is re-specified for any
#     reason, and the signal is checked at every origin. alpha, warmup, and
#     the check cadence stay exposed as flags for sensitivity checks only;
#     the defaults are the paper's configuration. If the trigg repro gates
#     in scripts/priority1_inference.R still miss, the residual ambiguity is
#     the warmup comparison (fires once three errors have accumulated); diff
#     against the local scripts/run_tracking_signal.R fire condition.

# ---------------------------------------------------------------------------
# AICc-on-fire score gate
# ---------------------------------------------------------------------------

run_aicc_gate_capped <- function(series_id, y, cfg, threshold, max_age) {
  requireNamespace("data.table", quietly = TRUE)
  y <- clean_numeric(y)
  origins <- make_origins(y, cfg$train_length, cfg$horizon, cfg$n_rounds)
  records <- vector("list", length(origins))
  if (length(origins) == 0L) return(data.frame())

  current_form <- NULL
  threshold <- as.numeric(threshold)
  max_age <- max(1L, as.integer(max_age))
  last_respec_idx <- NA_integer_
  policy_name <- paste0(
    "aicc_gate_cap", max_age, "_tau",
    format(threshold, trim = TRUE, scientific = FALSE)
  )

  for (idx in seq_along(origins)) {
    origin <- origins[[idx]]
    train <- training_window(y, origin, cfg)
    truth <- y[origin:(origin + cfg$horizon - 1L)]

    respecified <- FALSE
    score_gap <- NA_real_
    monitor_seconds <- 0.0
    current_validation_loss <- NA_real_
    challenger_validation_loss <- NA_real_
    challenger_form <- NA_character_
    deployed_weight_aicc <- NA_real_
    spec_debt_aicc <- NA_real_
    best_ic_form_aicc <- NA_character_
    best_weight_aicc <- NA_real_
    deployed_weight_bic <- NA_real_
    spec_debt_bic <- NA_real_
    best_ic_form_bic <- NA_character_
    best_weight_bic <- NA_real_
    triggered_by_score <- FALSE
    triggered_by_cap <- FALSE
    trigger_reason <- "none"
    age_before_action <- if (is.na(last_respec_idx)) NA_integer_ else as.integer(idx - last_respec_idx)

    due_to_age <- is.null(current_form) || is.na(last_respec_idx) || ((idx - last_respec_idx) >= max_age)

    if (due_to_age) {
      outcome <- fit_auto_es(train, cfg$horizon, cfg$seasonality, cfg$allow_multiplicative, cfg$clip_nonnegative)
      current_form <- outcome$form
      triggered_by_cap <- !is.na(last_respec_idx)
      trigger_reason <- ifelse(triggered_by_cap, "cap", "initial")
      last_respec_idx <- idx
      respecified <- TRUE
    } else {
      do_monitor <- cfg$monitor_window > 1L &&
        ((idx - 1L) %% max(1L, cfg$monitor_every) == 0L) &&
        length(train) > cfg$monitor_window + max(6L, cfg$seasonality)

      fired <- FALSE
      if (do_monitor) {
        mw <- cfg$monitor_window
        base <- train[seq_len(length(train) - mw)]
        val <- tail(train, mw)
        current_val <- fit_form_es(base, current_form, mw, cfg$seasonality, cfg$clip_nonnegative)
        current_loss <- metric_value(cfg$metric, base, val, current_val$forecast, cfg$seasonality)
        best_val <- select_form_validation(
          base, val, cfg$seasonality,
          metric = cfg$metric,
          allow_multiplicative = cfg$allow_multiplicative,
          clip_nonnegative = cfg$clip_nonnegative,
          current_form = current_form
        )
        monitor_seconds <- as.numeric(current_val$fit_seconds) + as.numeric(best_val$fit_seconds)
        current_validation_loss <- current_loss
        challenger_validation_loss <- best_val$loss
        challenger_form <- as.character(best_val$form)
        deployed_weight_aicc <- best_val$deployed_weight_aicc
        spec_debt_aicc <- best_val$spec_debt_aicc
        best_ic_form_aicc <- best_val$best_ic_form_aicc
        best_weight_aicc <- best_val$best_weight_aicc
        deployed_weight_bic <- best_val$deployed_weight_bic
        spec_debt_bic <- best_val$spec_debt_bic
        best_ic_form_bic <- best_val$best_ic_form_bic
        best_weight_bic <- best_val$best_weight_bic
        if (is.finite(current_loss) && is.finite(best_val$loss)) score_gap <- current_loss - best_val$loss

        if (is.finite(score_gap) && score_gap > threshold &&
            !identical(as.character(best_val$form), as.character(current_form))) {
          # Same firing event as the validation gate. The action differs:
          # re-select by AICc on the full window and deploy that form. The
          # forecast comes from the winning full-window fit itself, the same
          # object a cap-driven search deploys.
          resel <- fit_auto_es(train, cfg$horizon, cfg$seasonality, cfg$allow_multiplicative, cfg$clip_nonnegative)
          outcome <- resel
          outcome$fit_seconds <- as.numeric(resel$fit_seconds) + monitor_seconds
          current_form <- resel$form
          last_respec_idx <- idx
          respecified <- TRUE
          triggered_by_score <- TRUE
          trigger_reason <- "score"
          fired <- TRUE
        }
      }

      if (!fired) {
        outcome <- fit_form_es(train, current_form, cfg$horizon, cfg$seasonality, cfg$clip_nonnegative)
        outcome$fit_seconds <- as.numeric(outcome$fit_seconds) + monitor_seconds
      }
    }

    loss <- metric_value(cfg$metric, train, truth, outcome$forecast, cfg$seasonality)
    records[[idx]] <- make_record(
      series_id, policy_name, idx - 1L, origin, current_form, truth, outcome$forecast,
      loss, outcome$fit_seconds, respecified,
      score_gap = score_gap, threshold = threshold, tau_cost_ratio = threshold,
      trigger_reason = trigger_reason,
      triggered_by_score = triggered_by_score,
      triggered_by_cap = triggered_by_cap,
      age_before_action = age_before_action,
      current_validation_loss = current_validation_loss,
      challenger_validation_loss = challenger_validation_loss,
      challenger_form = challenger_form,
      deployed_weight_aicc = deployed_weight_aicc,
      spec_debt_aicc = spec_debt_aicc,
      best_ic_form_aicc = best_ic_form_aicc,
      best_weight_aicc = best_weight_aicc,
      deployed_weight_bic = deployed_weight_bic,
      spec_debt_bic = spec_debt_bic,
      best_ic_form_bic = best_ic_form_bic,
      best_weight_bic = best_weight_bic
    )
  }

  data.table::rbindlist(records, fill = TRUE)
}

# ---------------------------------------------------------------------------
# Trigg tracking-signal policy
# ---------------------------------------------------------------------------

run_trigg_capped <- function(series_id, y, cfg, control_limit, max_age,
                             alpha_smooth = 0.2,
                             signal_every = 1L,
                             warmup = 3L) {
  requireNamespace("data.table", quietly = TRUE)
  y <- clean_numeric(y)
  origins <- make_origins(y, cfg$train_length, cfg$horizon, cfg$n_rounds)
  records <- vector("list", length(origins))
  if (length(origins) == 0L) return(data.frame())

  current_form <- NULL
  control_limit <- as.numeric(control_limit)
  alpha_smooth <- as.numeric(alpha_smooth)
  signal_every <- max(1L, as.integer(signal_every))
  warmup <- max(1L, as.integer(warmup))
  max_age <- max(1L, as.integer(max_age))
  last_respec_idx <- NA_integer_
  policy_name <- paste0(
    "trigg_cap", max_age, "_tau",
    format(control_limit, trim = TRUE, scientific = FALSE)
  )

  # Tracking-signal state per run_tracking_signal.R: seed on the first error
  # after a reset, EWMA afterwards, zero the state on any re-specification.
  E <- 0.0
  M <- 0.0
  n_since_reset <- 0L
  prev_f1 <- NA_real_

  fold_error <- function(e) {
    if (!is.finite(e)) return(invisible(NULL))
    if (n_since_reset == 0L) {
      E <<- e
      M <<- abs(e)
    } else {
      E <<- alpha_smooth * e + (1 - alpha_smooth) * E
      M <<- alpha_smooth * abs(e) + (1 - alpha_smooth) * M
    }
    n_since_reset <<- n_since_reset + 1L
    invisible(NULL)
  }
  reset_signal <- function() {
    E <<- 0.0
    M <<- 0.0
    n_since_reset <<- 0L
    invisible(NULL)
  }

  for (idx in seq_along(origins)) {
    origin <- origins[[idx]]
    train <- training_window(y, origin, cfg)
    truth <- y[origin:(origin + cfg$horizon - 1L)]

    # Realized one-step error of whatever the policy deployed at the previous
    # origin, folded in before this origin's decision. The signal that gates
    # origin t is built only from errors observed strictly before t; a reset
    # at the previous origin means that origin's error becomes the seed of
    # the fresh state, exactly as in run_tracking_signal.R.
    if (idx >= 2L && is.finite(prev_f1)) {
      fold_error(as.numeric(y[origins[[idx - 1L]]]) - prev_f1)
    }
    signal <- if (is.finite(M) && M > 1e-12) E / M else NA_real_

    respecified <- FALSE
    triggered_by_score <- FALSE
    triggered_by_cap <- FALSE
    trigger_reason <- "none"
    age_before_action <- if (is.na(last_respec_idx)) NA_integer_ else as.integer(idx - last_respec_idx)
    signal_age <- n_since_reset

    due_to_age <- is.null(current_form) || is.na(last_respec_idx) || ((idx - last_respec_idx) >= max_age)
    check_signal <- ((idx - 1L) %% signal_every == 0L) &&
      n_since_reset >= warmup && is.finite(signal) && abs(signal) >= control_limit

    if (due_to_age) {
      outcome <- fit_auto_es(train, cfg$horizon, cfg$seasonality, cfg$allow_multiplicative, cfg$clip_nonnegative)
      current_form <- outcome$form
      triggered_by_cap <- !is.na(last_respec_idx)
      trigger_reason <- ifelse(triggered_by_cap, "cap", "initial")
      last_respec_idx <- idx
      respecified <- TRUE
      reset_signal()
    } else if (check_signal) {
      # Signal fire: re-select by AICc on the full training window, exactly
      # as a scheduled re-specification would be, then reset the monitor.
      outcome <- fit_auto_es(train, cfg$horizon, cfg$seasonality, cfg$allow_multiplicative, cfg$clip_nonnegative)
      current_form <- outcome$form
      last_respec_idx <- idx
      respecified <- TRUE
      triggered_by_score <- TRUE
      trigger_reason <- "signal"
      reset_signal()
    } else {
      outcome <- fit_form_es(train, current_form, cfg$horizon, cfg$seasonality, cfg$clip_nonnegative)
    }

    loss <- metric_value(cfg$metric, train, truth, outcome$forecast, cfg$seasonality)
    rec <- make_record(
      series_id, policy_name, idx - 1L, origin, current_form, truth, outcome$forecast,
      loss, outcome$fit_seconds, respecified,
      threshold = control_limit,
      trigger_reason = trigger_reason,
      triggered_by_score = triggered_by_score,
      triggered_by_cap = triggered_by_cap,
      age_before_action = age_before_action
    )
    rec$tracking_signal <- as.numeric(signal)
    rec$signal_age <- as.integer(signal_age)
    records[[idx]] <- rec

    prev_f1 <- as.numeric(outcome$forecast[1L])
  }

  data.table::rbindlist(records, fill = TRUE)
}

# ---------------------------------------------------------------------------
# Battery and checkpointed driver
# ---------------------------------------------------------------------------

PRIORITY1_POLICIES <- c(
  "parameter_only", "fixed_f4", "fixed_f8",
  "validation_gate", "aicc_gate", "trigg"
)

run_priority1_series <- function(item, cfg, policies,
                                 tau, cap,
                                 trigg_limit, trigg_alpha,
                                 trigg_signal_every, trigg_warmup) {
  requireNamespace("data.table", quietly = TRUE)
  y <- clean_numeric(item$values)
  if (length(y) < cfg$train_length + cfg$horizon) return(data.frame())

  blocks <- list()
  add <- function(b) {
    if (is.data.frame(b) && nrow(b) > 0L) blocks[[length(blocks) + 1L]] <<- b
    invisible(NULL)
  }

  if ("full_update" %in% policies) add(run_full_update(item$series_id, y, cfg))
  if ("parameter_only" %in% policies) add(run_parameter_only(item$series_id, y, cfg))
  if ("fixed_f4" %in% policies) add(run_fixed_frequency(item$series_id, y, cfg, 4L))
  if ("fixed_f8" %in% policies) add(run_fixed_frequency(item$series_id, y, cfg, 8L))
  if ("validation_gate" %in% policies) {
    add(run_adaptive_capped_score(item$series_id, y, cfg, threshold = tau, max_age = cap))
  }
  if ("aicc_gate" %in% policies) {
    add(run_aicc_gate_capped(item$series_id, y, cfg, threshold = tau, max_age = cap))
  }
  if ("trigg" %in% policies) {
    add(run_trigg_capped(
      item$series_id, y, cfg,
      control_limit = trigg_limit, max_age = cap,
      alpha_smooth = trigg_alpha, signal_every = trigg_signal_every,
      warmup = trigg_warmup
    ))
  }

  data.table::rbindlist(blocks, fill = TRUE)
}

safe_priority1_series <- function(item, ...) {
  out <- try(run_priority1_series(item, ...), silent = TRUE)
  if (inherits(out, "try-error")) {
    message("Series failed: ", item$series_id, " | ", as.character(out)[1L])
    return(data.frame())
  }
  out
}

run_priority1_experiment <- function(
    data_dir = "data",
    out_dir = "outputs/m4_priority1",
    n_series = -1L,
    seed = 123L,
    n_rounds = 36L,
    horizon = 18L,
    seasonality = 12L,
    train_length = 36L,
    monitor_window = 12L,
    monitor_every = 6L,
    policies = PRIORITY1_POLICIES,
    tau = 0.8,
    cap = 8L,
    trigg_limit = 0.6,
    trigg_alpha = 0.2,
    trigg_signal_every = 1L,
    trigg_warmup = 3L,
    n_jobs = 1L,
    batch_size = 500L,
    resume = TRUE,
    allow_multiplicative = TRUE,
    items_override = NULL) {

  check_required_packages()
  requireNamespace("data.table", quietly = TRUE)

  known <- c("full_update", PRIORITY1_POLICIES)
  bad <- setdiff(policies, known)
  if (length(bad) > 0L) stop("Unknown policies: ", paste(bad, collapse = ", "))

  # Same eligibility rule as run_m4_experiment: the rolling design needs
  # train_length + n_rounds + horizon - 1 combined observations, which is 89
  # at horizon 18 and should leave 38,134 of the 48,000 M4 monthly series.
  min_obs_needed <- as.integer(train_length) + as.integer(n_rounds) + as.integer(horizon) - 1L
  if (!is.null(items_override)) {
    items <- items_override
  } else {
    items <- load_m4_monthly(
      data_dir = data_dir,
      n_series = -1L,
      seed = seed,
      min_obs = min_obs_needed,
      include_test = TRUE
    )
    if (!is.null(n_series) && n_series > 0L && n_series < length(items)) {
      set.seed(seed)
      idx <- sort(sample(seq_along(items), n_series))
      items <- items[idx]
    }
  }
  if (length(items) == 0L) stop("No M4 items loaded. Check data and configuration.")

  cfg <- new_rolling_config(
    horizon = horizon,
    seasonality = seasonality,
    train_length = train_length,
    n_rounds = n_rounds,
    fixed_window = TRUE,
    metric = "mase",
    allow_multiplicative = allow_multiplicative,
    clip_nonnegative = FALSE,
    monitor_window = monitor_window,
    monitor_every = monitor_every
  )

  ensure_dir(out_dir)
  cfg_lines <- c(
    "priority1 run configuration",
    paste0("timestamp: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
    paste0("data_dir: ", data_dir),
    paste0("eligible_series: ", length(items),
           " (min_obs ", min_obs_needed, ", n_series arg ", n_series, ")"),
    paste0("horizon: ", horizon, " | n_rounds: ", n_rounds,
           " | train_length: ", train_length, " | seasonality: ", seasonality),
    paste0("monitor_window: ", monitor_window, " | monitor_every: ", monitor_every),
    paste0("policies: ", paste(policies, collapse = ",")),
    paste0("tau: ", tau, " | cap: ", cap),
    paste0("trigg_limit: ", trigg_limit, " | trigg_alpha: ", trigg_alpha,
           " | trigg_signal_every: ", trigg_signal_every,
           " | trigg_warmup: ", trigg_warmup),
    paste0("seed: ", seed, " | batch_size: ", batch_size, " | n_jobs: ", n_jobs),
    paste0("R: ", R.version.string,
           " | forecast: ", as.character(utils::packageVersion("forecast")),
           " | data.table: ", as.character(utils::packageVersion("data.table")))
  )
  writeLines(cfg_lines, file.path(out_dir, "run_config.txt"))
  message(paste(cfg_lines, collapse = "\n"))

  run_one <- function(item) {
    safe_priority1_series(
      item, cfg = cfg, policies = policies,
      tau = tau, cap = cap,
      trigg_limit = trigg_limit, trigg_alpha = trigg_alpha,
      trigg_signal_every = trigg_signal_every,
      trigg_warmup = trigg_warmup
    )
  }

  # Checkpointed exactly like run_m4_experiment: contiguous batches of the
  # same deterministic item list, atomic part writes, resume by skipping
  # finished parts. Batching bounds memory and enables restart; it changes
  # no reported number.
  batch_size <- max(1L, as.integer(batch_size))
  parts_dir <- file.path(out_dir, "parts")
  ensure_dir(parts_dir)

  n_items <- length(items)
  starts <- seq.int(1L, n_items, by = batch_size)
  n_batches <- length(starts)
  message("Checkpointed run: ", n_items, " series in ", n_batches,
          " batch(es) of up to ", batch_size, ". Parts in ", parts_dir)

  for (b in seq_len(n_batches)) {
    lo <- starts[[b]]
    hi <- min(lo + batch_size - 1L, n_items)
    part_path <- file.path(parts_dir, sprintf("records_part_%05d.csv", b))
    if (isTRUE(resume) && file.exists(part_path)) {
      message("  batch ", b, "/", n_batches, " (series ", lo, "-", hi, "): exists, skipping")
      next
    }
    t_batch <- Sys.time()
    blocks <- parallel_lapply(items[lo:hi], run_one, n_jobs = n_jobs)
    batch_records <- data.table::rbindlist(blocks, fill = TRUE)
    tmp_path <- paste0(part_path, ".tmp")
    data.table::fwrite(batch_records, tmp_path)
    file.rename(tmp_path, part_path)
    message("  batch ", b, "/", n_batches, " (series ", lo, "-", hi, "): ",
            nrow(batch_records), " rows, ",
            round(as.numeric(difftime(Sys.time(), t_batch, units = "mins")), 1), " min")
    rm(blocks, batch_records)
    gc(verbose = FALSE)
  }

  message("All batches complete. Next: Rscript scripts/priority1_inference.R --run-dir ", out_dir)
  invisible(list(n_series = n_items, n_batches = n_batches, out_dir = out_dir))
}
