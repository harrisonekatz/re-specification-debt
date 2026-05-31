# Reproduces the clean fixed-frequency calibration grid: full update,
# parameter-only, and fixed model-form update frequencies 2 through 12.
source("R/adaptive_update.R")

t0 <- Sys.time()
summary_fixed <- run_m4_fixed_grid_calibration(
  data_dir = "data",
  out_dir = "outputs/m4_100_fixed_grid",
  n_series = 100L,
  seed = 123L,
  n_jobs = 1L
)
print(Sys.time() - t0)
print(summary_fixed)
print(check_policy_forecast_diversity("outputs/m4_100_fixed_grid/records.csv"))
