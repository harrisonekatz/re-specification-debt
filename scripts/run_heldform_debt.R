# The held-form construct test. The earlier debt correlation read off adaptive policies'
# monitored rows, where the firing rule truncates the debt distribution (every high-debt
# round is the round the policy switches and resets) and, on noisy cells, fires on noise
# spikes, a selection effect strong enough to flip a correlation's sign. That is most likely
# what produced the score-gap sign-flip across the noise axis. This removes it: the form is
# frozen for the whole series (as parameter_only deploys it), debt is computed at every
# monitoring origin so it ACCUMULATES as drift makes the form stale, and nothing is ever
# acted on, so there is no selection. Same engine functions the recorded debt uses, so this
# is the construct measured honestly.
#
# The question, unchanged: does in-sample IC debt of a held form predict its realized
# out-of-sample loss, and does that prediction appear under dynamic drift while staying ~0
# under static misspecification? If yes, the construct is rescued and the boundary is drawn.
# If a frozen, selection-free debt still does not track loss under drift, the construct fails
# in principle, not just as wired, and that is the honest verdict.
#
# Re-fits only the held form (one auto fit per series, one form fit per origin, one grid
# evaluation per monitoring origin), so far cheaper than the full battery.
#
# Usage (repo root, tmux):
#   Rscript scripts/run_heldform_debt.R --n-jobs 8
#   Rscript scripts/run_heldform_debt.R --data data_drift --n-jobs 8

suppressMessages(source("R/adaptive_update.R"))
library(data.table)
args     <- parse_cli_args()
n_jobs   <- get_arg(args, "n_jobs", default = max(1L, parallel::detectCores()), type = "integer")
data_dir <- get_arg(args, "data", default = "data_drift", type = "character")
out_dir  <- get_arg(args, "out_dir", default = "outputs/m4_drift_experiment", type = "character")
gt_path  <- get_arg(args, "ground_truth", default = file.path(data_dir, "ground_truth.csv"), type = "character")
mw_arg   <- get_arg(args, "monitor_window", default = 6L, type = "integer")
me_arg   <- get_arg(args, "monitor_every", default = 3L, type = "integer")
ensure_dir(out_dir)

cfg <- new_rolling_config(
  horizon = 3L, seasonality = 12L, train_length = 36L, n_rounds = 36L, fixed_window = TRUE,
  metric = "mase", allow_multiplicative = TRUE, clip_nonnegative = FALSE,
  monitor_window = mw_arg, monitor_every = me_arg
)

heldform_series <- function(it) {
  y <- clean_numeric(it$values); sid <- it$series_id
  origins <- make_origins(y, cfg$train_length, cfg$horizon, cfg$n_rounds)
  if (length(origins) == 0L) return(NULL)
  held <- NULL; recs <- vector("list", length(origins))
  for (idx in seq_along(origins)) {
    origin <- origins[[idx]]
    train  <- training_window(y, origin, cfg)
    truth  <- y[origin:(origin + cfg$horizon - 1L)]
    if (is.null(held))
      held <- fit_auto_es(train, cfg$horizon, cfg$seasonality, cfg$allow_multiplicative, cfg$clip_nonnegative)$form
    outcome <- fit_form_es(train, held, cfg$horizon, cfg$seasonality, cfg$clip_nonnegative)   # FROZEN form, params re-fit
    loss <- metric_value(cfg$metric, train, truth, outcome$forecast, cfg$seasonality)
    spec_debt_aicc <- NA_real_; score_gap <- NA_real_
    do_monitor <- cfg$monitor_window > 1L &&
      ((idx - 1L) %% max(1L, cfg$monitor_every) == 0L) &&
      length(train) > cfg$monitor_window + max(6L, cfg$seasonality)
    if (do_monitor) {
      mw <- cfg$monitor_window
      base <- train[seq_len(length(train) - mw)]; val <- tail(train, mw)
      bv <- select_form_validation(base, val, cfg$seasonality, metric = cfg$metric,
                                   allow_multiplicative = cfg$allow_multiplicative,
                                   clip_nonnegative = cfg$clip_nonnegative, current_form = held)
      spec_debt_aicc <- bv$spec_debt_aicc                                   # debt of the FROZEN form, never reset
      cv <- fit_form_es(base, held, mw, cfg$seasonality, cfg$clip_nonnegative)
      cl <- metric_value(cfg$metric, base, val, cv$forecast, cfg$seasonality)
      if (is.finite(cl) && is.finite(bv$loss)) score_gap <- cl - bv$loss
    }
    recs[[idx]] <- data.table(series_id = sid, origin_number = idx - 1L,
                              loss = loss, spec_debt_aicc = spec_debt_aicc, score_gap = score_gap)
  }
  data.table::rbindlist(recs)
}

items <- load_m4_monthly(data_dir, n_series = -1L, min_obs = 2L * cfg$train_length)
parts <- parallel::mclapply(items, function(it) tryCatch(heldform_series(it), error = function(e) NULL), mc.cores = n_jobs)
res <- data.table::rbindlist(Filter(Negate(is.null), parts), fill = TRUE)
fwrite(res, file.path(out_dir, "heldform_records.csv"))

gt <- fread(gt_path)
d  <- merge(res[is.finite(spec_debt_aicc)], gt[, .(series_id, drift_mode, noise)], by = "series_id")
cat("=== finite held-form debt rows by cell (frozen form, no selection) ===\n")
print(d[, .(rows = .N), by = .(drift_mode, noise)][order(drift_mode, noise)])
corr <- d[, .(
  spearman_debt_vs_loss     = round(suppressWarnings(cor(spec_debt_aicc, loss,      method = "spearman", use = "complete.obs")), 3),
  spearman_debt_vs_scoregap = round(suppressWarnings(cor(spec_debt_aicc, score_gap, method = "spearman", use = "complete.obs")), 3),
  n = .N), by = .(drift_mode, noise)][order(drift_mode, noise)]
cat("\n=== held-form in-sample debt vs out-of-sample signal ===\n")
cat("construct rescued if debt_vs_loss sits ~0 on static and climbs clearly positive under irregular/regular drift\n\n")
print(corr)
fwrite(corr, file.path(out_dir, "heldform_debt_correlation.csv"))
cat("\nwrote heldform_records.csv and heldform_debt_correlation.csv to", out_dir, "\n")
cat("read debt_vs_loss (realized error), not debt_vs_scoregap (debt's in-sample cousin); the former is the claim.\n")
