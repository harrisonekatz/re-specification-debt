# Flexible CLI runner.
# Example:
# Rscript scripts/run_m4_experiment.R --n-series 100 --out-dir outputs/custom --n-jobs 4 --capped true
source("R/adaptive_update.R")
args <- parse_cli_args()

n_series <- get_arg(args, "n_series", default = 100L, type = "integer")
n_rounds <- get_arg(args, "n_rounds", default = 36L, type = "integer")
train_length <- get_arg(args, "train_length", default = 36L, type = "integer")
n_jobs <- get_arg(args, "n_jobs", default = 1L, type = "integer")
out_dir <- get_arg(args, "out_dir", default = "outputs/m4_custom", type = "character")
include_pure <- get_arg(args, "pure_adaptive", default = FALSE, type = "logical")
include_capped <- get_arg(args, "capped_adaptive", default = TRUE, type = "logical")
alpha <- get_arg(args, "alpha", default = 0.0, type = "numeric")
gamma <- get_arg(args, "gamma", default = 0.0, type = "numeric")

fixed <- parse_integer_list(get_arg(args, "fixed_frequencies", default = "2,3,4,5,6,7,8,9,10,11,12", type = "character"))
thresholds <- parse_numeric_list(get_arg(args, "adaptive_thresholds", default = "0.03,0.05,0.10,0.20,0.40,0.80", type = "character"))
caps <- parse_integer_list(get_arg(args, "adaptive_caps", default = "8,12", type = "character"))

summary <- run_m4_experiment(
  data_dir = "data",
  out_dir = out_dir,
  n_series = n_series,
  seed = 123L,
  n_rounds = n_rounds,
  horizon = 3L,
  seasonality = 12L,
  train_length = train_length,
  fixed_frequencies = fixed,
  adaptive_thresholds = thresholds,
  adaptive_caps = caps,
  include_pure_adaptive = include_pure,
  include_capped_adaptive = include_capped,
  monitor_window = 12L,
  monitor_every = 6L,
  n_jobs = n_jobs,
  alpha = alpha,
  gamma = gamma,
  allow_multiplicative = TRUE
)
print(summary)
