# Fast smoke test. This should run before any 100-series or 500-series job.
source("R/adaptive_update.R")

summary_smoke <- run_m4_experiment(
  data_dir = "data",
  out_dir = "outputs/m4_smoke",
  n_series = 10L,
  seed = 123L,
  n_rounds = 12L,
  horizon = 3L,
  seasonality = 12L,
  train_length = 36L,
  fixed_frequencies = c(2L, 3L, 6L),
  adaptive_thresholds = c(0.05, 0.20),
  adaptive_caps = c(6L),
  include_pure_adaptive = FALSE,
  include_capped_adaptive = TRUE,
  monitor_window = 12L,
  monitor_every = 6L,
  n_jobs = 1L,
  alpha = 0.0,
  gamma = 0.0,
  allow_multiplicative = TRUE
)

print(summary_smoke)
print(check_policy_forecast_diversity("outputs/m4_smoke/records.csv"))
