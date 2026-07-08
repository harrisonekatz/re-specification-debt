# Full-scale capped-adaptive run on all eligible M4 monthly series.
#
# This is the scaled version of scripts/run_m4_500_capped.R. It runs the SAME
# policies and the SAME rolling-origin protocol, but on every eligible monthly
# series instead of a 500-series random sample. Nothing in the method is
# changed or simplified for scale.
#
# Series selection
#   n_series = -1L runs all eligible series with no subsampling.
#
# Eligibility (state this in the paper)
#   The rolling design (train_length 36, n_rounds 36, horizon 3) requires at
#   least 74 combined observations per series (train_length + n_rounds +
#   horizon - 1). 47,982 of the 48,000 M4 monthly series meet this. The 18
#   series with fewer than 56 training observations are excluded because they
#   cannot support a 36-origin rolling window, not by any arbitrary cut.
#
# Checkpointing
#   batch_size only controls peak memory and restart behaviour. Series are
#   independent and the per-series computation is deterministic, so the
#   combined result is identical to a single-pass run. An interrupted run
#   resumes from the last completed batch in outputs/m4_full_capped/parts/.
#
# Hardware
#   This is a large job: the per-series work matches the 500 run, scaled by
#   roughly 96x in series count. Use a high core count and a high-memory
#   machine (the final summary step loads the full record table into memory).
#
# Example:
#   Rscript scripts/run_m4_full_capped.R --n-jobs 64 --batch-size 1000

source("R/adaptive_update.R")

args <- parse_cli_args()
n_jobs <- get_arg(args, "n_jobs", default = max(1L, parallel::detectCores()), type = "integer")
batch_size <- get_arg(args, "batch_size", default = 1000L, type = "integer")

t0 <- Sys.time()
summary_full <- run_m4_experiment(
  data_dir = "data",
  out_dir = "outputs/m4_full_capped",
  n_series = -1L,
  seed = 123L,
  n_rounds = 36L,
  horizon = 3L,
  seasonality = 12L,
  train_length = 36L,
  fixed_frequencies = 2L:12L,
  adaptive_thresholds = c(0.03, 0.05, 0.10, 0.20, 0.40, 0.80),
  adaptive_caps = c(8L, 12L),
  include_pure_adaptive = FALSE,
  include_capped_adaptive = TRUE,
  monitor_window = 12L,
  monitor_every = 6L,
  n_jobs = n_jobs,
  alpha = 0.0,
  gamma = 0.0,
  allow_multiplicative = TRUE,
  batch_size = batch_size,
  resume = TRUE
)
print(Sys.time() - t0)
print(summary_full)
print(check_policy_forecast_diversity("outputs/m4_full_capped/records.csv"))

# Defensibility check for the reviewers: the share of origin fits, per policy,
# that fell back to seasonal naive because no ETS candidate could be fit. This
# is an audit quantity, not a result. Report it so the empirical claims are not
# silently resting on a pile of failed fits.
recs <- data.table::fread("outputs/m4_full_capped/records.csv")
fallback_share <- recs[, .(
  n_origin_fits = .N,
  snaive_fallbacks = sum(form == "SNAIVE"),
  fallback_pct = round(100 * mean(form == "SNAIVE"), 4)
), by = policy]
print(fallback_share)
data.table::fwrite(fallback_share, "outputs/m4_full_capped/fallback_share.csv")
