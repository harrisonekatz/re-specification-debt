# Priority 1 timing stratum.
#
# The main run in scripts/run_m4_priority1.R measures per-policy fit seconds
# on the full eligible set, but excludes full_update, and different batches
# may land on different machine conditions. This script re-anchors the cost
# columns: it runs the ENTIRE battery, full_update included, on a
# length-stratified subsample of the horizon-18-eligible series, in one
# session on one node, so every relative-time and relative-instability ratio
# in the horizon-18 cost table comes from identical conditions. Loss numbers
# for the paper come from the full run; this stratum exists for the time and
# instability denominators, so run it on a quiet machine.
#
# Stratification: deterministic seed, equal draws from length deciles of the
# eligible pool, so long series (where grid fits cost most) are represented.
#
# Usage (repo root, quiet node):
#   Rscript scripts/run_priority1_timing_stratum.R --n-series 2000 --n-jobs 8
#
# Flags mirror scripts/run_m4_priority1.R; additionally
#   --n-series N   stratum size (default 2000)
#   --out DIR      default "outputs/m4_priority1_timing"

suppressMessages(source("R/adaptive_update.R"))
suppressMessages(source("R/priority1_policies.R"))
suppressMessages(library(data.table))

args <- parse_cli_args()
n_jobs <- get_arg(args, "n_jobs", default = max(1L, parallel::detectCores()), type = "integer")
batch_size <- get_arg(args, "batch_size", default = 250L, type = "integer")
data_dir <- get_arg(args, "data", default = "data", type = "character")
out_dir <- get_arg(args, "out", default = "outputs/m4_priority1_timing", type = "character")
n_series <- get_arg(args, "n_series", default = 2000L, type = "integer")
horizon <- get_arg(args, "horizon", default = 18L, type = "integer")
seed <- get_arg(args, "seed", default = 123L, type = "integer")
tau <- get_arg(args, "tau", default = 0.8, type = "numeric")
cap <- get_arg(args, "cap", default = 8L, type = "integer")
trigg_limit <- get_arg(args, "trigg_limit", default = 0.6, type = "numeric")
trigg_alpha <- get_arg(args, "trigg_alpha", default = 0.2, type = "numeric")
trigg_signal_every <- get_arg(args, "trigg_signal_every", default = 1L, type = "integer")
trigg_warmup <- get_arg(args, "trigg_warmup", default = 3L, type = "integer")

train_length <- 36L; n_rounds <- 36L
min_obs_needed <- train_length + n_rounds + horizon - 1L

items <- load_m4_monthly(data_dir, n_series = -1L, seed = seed,
                         min_obs = min_obs_needed, include_test = TRUE)
if (length(items) == 0L) stop("No eligible series loaded.")

# Length-stratified deterministic sample.
lens <- vapply(items, function(x) length(x$values), integer(1L))
dec <- cut(lens, breaks = unique(quantile(lens, probs = seq(0, 1, 0.1))),
           include.lowest = TRUE, labels = FALSE)
set.seed(seed)
resample <- function(x, k) x[sample.int(length(x), k)]
take <- integer(0)
per <- ceiling(min(n_series, length(items)) / length(unique(dec)))
for (d in sort(unique(dec))) {
  pool <- which(dec == d)
  take <- c(take, resample(pool, min(per, length(pool))))
}
take <- sort(unique(take))
if (length(take) > n_series) take <- sort(resample(take, n_series))
items <- items[take]
message("Timing stratum: ", length(items), " series across ",
        length(unique(dec)), " length deciles.")

# Write the stratum ids for the manifest.
ensure_dir(out_dir)
writeLines(vapply(items, function(x) x$series_id, character(1L)),
           file.path(out_dir, "stratum_series_ids.txt"))

policies <- c("full_update", PRIORITY1_POLICIES)

t0 <- Sys.time()
run_priority1_experiment(
  data_dir = data_dir,
  out_dir = out_dir,
  n_series = -1L,        # selection already applied above via a temp loader?
  seed = seed,
  n_rounds = n_rounds,
  horizon = horizon,
  seasonality = 12L,
  train_length = train_length,
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
  resume = TRUE,
  allow_multiplicative = TRUE,
  items_override = items
)
print(Sys.time() - t0)

# Compact per-policy summary on the stratum: mean fit seconds per series,
# instability, and the ratios anchored at full_update. Small enough to hold
# in memory.
part_files <- sort(list.files(file.path(out_dir, "parts"),
                              pattern = "^records_part_[0-9]+\\.csv$",
                              full.names = TRUE))
recs <- rbindlist(lapply(part_files, fread), fill = TRUE)
per_series_sec <- recs[, .(fit_seconds = sum(fit_seconds, na.rm = TRUE)),
                       by = .(policy, series_id)]
inst <- compute_instability(recs)
summ <- merge(
  per_series_sec[, .(mean_fit_seconds = mean(fit_seconds),
                     total_fit_seconds = sum(fit_seconds),
                     n_series = uniqueN(series_id)), by = policy],
  inst[, .(mean_instability = mean(instability, na.rm = TRUE)), by = policy],
  by = "policy", all.x = TRUE
)
ref_t <- summ[policy == "full_update"]$total_fit_seconds[1L]
ref_i <- summ[policy == "full_update"]$mean_instability[1L]
summ[, relative_time := total_fit_seconds / ref_t]
summ[, relative_instability := mean_instability / ref_i]
setorder(summ, relative_time)
fwrite(summ, file.path(out_dir, "timing_summary.csv"))
print(summ)
message("Wrote ", file.path(out_dir, "timing_summary.csv"),
        " (feeds the cost columns in priority1_inference.R via --timing-dir).")
