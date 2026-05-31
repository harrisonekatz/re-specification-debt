# Main 100-series capped-adaptive calibration.
# Includes full update, parameter-only, fixed frequencies 2 through 12,
# and capped adaptive policies with caps 8 and 12.
source("R/adaptive_update.R")

t0 <- Sys.time()
summary_capped <- run_m4_capped_calibration(
  data_dir = "data",
  out_dir = "outputs/m4_100_capped",
  n_series = 100L,
  seed = 123L,
  n_jobs = 1L
)
print(Sys.time() - t0)
print(summary_capped)
print(check_policy_forecast_diversity("outputs/m4_100_capped/records.csv"))
