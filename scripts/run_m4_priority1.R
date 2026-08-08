# Priority 1 run: align the headline policies.
#
# Runs the AICc-on-fire score gate against the Trigg monitor, the validation
# gate, and the fixed cadences on every horizon-18-eligible M4 monthly series
# (length >= 89: train 36 + rounds 36 + horizon 18 - 1; expect 38,134 of
# 48,000). One pass at horizon 18 stores per-origin forecasts for leads 1
# through 18, so scripts/priority1_inference.R evaluates horizons 3, 6, 9,
# 12, and 18 at the same origins from this single run, along with search
# decomposition, form changes, instability, time, and the lead-specific
# profile. Configuration is frozen at the published values (tau 0.8, cap 8,
# 24/12 base and tail, six-period monitoring calendar); no tuning happens in
# this script.
#
# Policies (default): parameter_only, fixed_f4, fixed_f8,
#   validation_gate (adaptive_cap8_tau0.8), aicc_gate, trigg.
#   full_update is deliberately absent here; it enters through
#   scripts/run_priority1_timing_stratum.R, which re-anchors relative time
#   and instability on a stratified subsample without paying 36 grid searches
#   per series across 38k series.
#
# Cost: roughly 130 to 200 ETS fit attempts per series per gate policy, about
# 900 across the default battery, so on the order of a large fraction of the
# original horizon-three benchmark. Run in tmux, size --n-jobs to the box,
# and let resume handle interruptions.
#
# Usage (repo root):
#   Rscript scripts/run_m4_priority1.R --n-jobs 32 --batch-size 500
#
# Flags:
#   --data DIR            M4 data directory (default "data")
#   --out DIR             output directory (default "outputs/m4_priority1")
#   --n-jobs N            mclapply workers (default all cores)
#   --batch-size N        series per checkpoint part (default 500)
#   --n-series N          subsample for smoke tests; -1 = all eligible
#   --horizon H           default 18; do not change for the Priority 1 table
#   --policies a,b,c      subset of parameter_only,fixed_f4,fixed_f8,
#                         validation_gate,aicc_gate,trigg,full_update
#   --tau X --cap A       frozen at 0.8 and 8
#   --trigg-limit X       control limit (default 0.6)
#   --trigg-alpha X       smoothing constant (default 0.2, the paper's value)
#   --trigg-signal-every N  check cadence for the signal (default 1)
#   --trigg-warmup N      errors required after a reset before firing
#                         (default 3, the paper's three-error warmup)
#
# Trigg mechanics match the manuscript's Section 4.5 and the local
# scripts/run_tracking_signal.R: seed on the first error, EWMA with a = 0.2,
# silent for a three-error warmup, full state reset on any re-specification.
#   --no-resume           recompute existing parts

suppressMessages(source("R/adaptive_update.R"))
suppressMessages(source("R/priority1_policies.R"))

args <- parse_cli_args()
n_jobs <- get_arg(args, "n_jobs", default = max(1L, parallel::detectCores()), type = "integer")
batch_size <- get_arg(args, "batch_size", default = 500L, type = "integer")
data_dir <- get_arg(args, "data", default = "data", type = "character")
out_dir <- get_arg(args, "out", default = "outputs/m4_priority1", type = "character")
n_series <- get_arg(args, "n_series", default = -1L, type = "integer")
horizon <- get_arg(args, "horizon", default = 18L, type = "integer")
seed <- get_arg(args, "seed", default = 123L, type = "integer")
tau <- get_arg(args, "tau", default = 0.8, type = "numeric")
cap <- get_arg(args, "cap", default = 8L, type = "integer")
trigg_limit <- get_arg(args, "trigg_limit", default = 0.6, type = "numeric")
trigg_alpha <- get_arg(args, "trigg_alpha", default = 0.2, type = "numeric")
trigg_signal_every <- get_arg(args, "trigg_signal_every", default = 1L, type = "integer")
trigg_warmup <- get_arg(args, "trigg_warmup", default = 3L, type = "integer")
policies_arg <- get_arg(args, "policies", default = paste(PRIORITY1_POLICIES, collapse = ","), type = "character")
policies <- trimws(strsplit(policies_arg, ",", fixed = TRUE)[[1L]])
resume <- !isTRUE(args$no_resume)

t0 <- Sys.time()
res <- run_priority1_experiment(
  data_dir = data_dir,
  out_dir = out_dir,
  n_series = n_series,
  seed = seed,
  n_rounds = 36L,
  horizon = horizon,
  seasonality = 12L,
  train_length = 36L,
  monitor_window = 12L,
  monitor_every = 6L,
  policies = policies,
  tau = tau,
  cap = cap,
  trigg_limit = trigg_limit,
  trigg_alpha = trigg_alpha,
  trigg_signal_every = trigg_signal_every,
  trigg_warmup = trigg_warmup,
  n_jobs = n_jobs,
  batch_size = batch_size,
  resume = resume,
  allow_multiplicative = TRUE
)
print(Sys.time() - t0)

if (horizon == 18L && n_series < 0L && res$n_series > 10000L && res$n_series != 38134L) {
  warning("Eligible-series count is ", res$n_series,
          ", not the expected 38,134. Check that the M4 download is complete ",
          "before reading any full-scale result.")
}
