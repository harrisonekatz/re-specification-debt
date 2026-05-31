# Larger 500-series capped-adaptive run. Start this only after the smoke
# and 100-series runs complete cleanly.
source("R/adaptive_update.R")

args <- parse_cli_args()
n_jobs <- get_arg(args, "n_jobs", default = 4L, type = "integer")

t0 <- Sys.time()
summary_500 <- run_m4_experiment(
  data_dir = "data",
  out_dir = "outputs/m4_500_capped",
  n_series = 500L,
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
  allow_multiplicative = TRUE
)
print(Sys.time() - t0)
print(summary_500)
print(check_policy_forecast_diversity("outputs/m4_500_capped/records.csv"))
