# Generate series with OUT-OF-MODEL shocks: discontinuities the ETS candidate
# family has no vocabulary for, which is the regime the real M4 winners pointed at
# (misreports, exogenous events, contamination). This is where reaction could beat a
# schedule for a structural reason, because you can't pre-schedule a response to a
# shift whose timing and size you don't know. Same M4 format as before, so the
# pipeline runs unchanged.
#
# Several shock types on purpose, including ones where reacting should HURT, so this
# is a test and not a setup:
#   control          : no shock. Stable seasonal. Should reproduce the tie.
#   additive_outlier : stochastic single-point spikes (misreporting). Transient, so
#                      a good trigger should NOT overreact. low/high rate.
#   level_shift      : a permanent step at a random time (exogenous regime). Reaction
#                      should help. small/large.
#   transient_burst  : a short run of anomalous obs then recovery (a COVID-like event).
#                      Reaction helps entering, can hurt on the snap back.
#   variance_shift   : volatility regime change (calm -> turbulent).
#   heavy_tail       : Student-t innovations throughout. Occasional large moves inside
#                      the process but outside the model's light-tailed assumption.
#
# Ground truth records the shock time so we can later check whether the trigger fires
# AT the shock or just at the initial fit. Persistent/burst shocks land in the
# end-anchored evaluation region; outliers and heavy tails run throughout.
#
# Usage (repo root):
#   Rscript scripts/simulate_shock_series.R
#   Rscript scripts/simulate_shock_series.R --n_per_cell 800 --seed 11

suppressMessages(source("R/adaptive_update.R"))
library(data.table)
args <- parse_cli_args()
seed       <- get_arg(args, "seed", default = 123L, type = "integer")
n_per_cell <- get_arg(args, "n_per_cell", default = 600L, type = "integer")
out_data   <- get_arg(args, "out_data", default = "data_shock", type = "character")
L          <- get_arg(args, "length", default = 120L, type = "integer")
amp        <- get_arg(args, "amp", default = 0.25, type = "numeric")
sigma      <- get_arg(args, "sigma", default = 0.05, type = "numeric")
shock_at   <- get_arg(args, "shock_at", default = -1L, type = "integer")   # absolute obs index; -1 -> default mid-eval
set.seed(seed)

period <- 12L
shock_t <- if (shock_at > 0L) shock_at else L - 19L   # persistent/burst shocks land here
sshape <- sin(2*pi*(1:period)/period) + 0.4*sin(4*pi*(1:period)/period); sshape <- sshape / sd(sshape)

cells <- list(
  list(type="control",          sev="none"),
  list(type="additive_outlier", sev="low",   rate=0.03),
  list(type="additive_outlier", sev="high",  rate=0.10),
  list(type="level_shift",      sev="small", step=0.30),
  list(type="level_shift",      sev="large", step=0.80),
  list(type="transient_burst",  sev="med",   mag=0.60, blen=6L),
  list(type="variance_shift",   sev="high",  vmult=3.0),
  list(type="heavy_tail",       sev="df3",   df=3)
)

simulate_one <- function(cell) {
  level0 <- runif(1, 800, 1500); t <- 1:L
  signal <- level0 * (1 + amp * sshape[((t - 1) %% period) + 1])
  shock_time <- NA_integer_
  if (cell$type == "heavy_tail") {
    y <- signal * exp(rt(L, cell$df) * sigma)
  } else if (cell$type == "variance_shift") {
    shock_time <- shock_t; sd_t <- rep(sigma, L); sd_t[t >= shock_t] <- sigma * cell$vmult
    y <- signal * exp(rnorm(L, 0, sd_t))
  } else {
    y <- signal * exp(rnorm(L, 0, sigma))
  }
  if (cell$type == "level_shift") {
    shock_time <- shock_t; y[t >= shock_t] <- y[t >= shock_t] + cell$step * level0 * sample(c(1, -1), 1)
  } else if (cell$type == "transient_burst") {
    shock_time <- shock_t; idx <- shock_t:(shock_t + cell$blen - 1L)
    y[idx] <- y[idx] * (1 + cell$mag * sample(c(1, -1), 1))
  } else if (cell$type == "additive_outlier") {
    k <- max(1L, round(cell$rate * L)); pos <- sample(t, k)
    for (p in pos) y[p] <- y[p] * (if (runif(1) < 0.5) runif(1, 2, 4) else runif(1, 0.25, 0.5))
  }
  list(y = pmax(y, 1e-6), shock_time = shock_time)
}

lines <- character(0); gt <- list(); k <- 0L
for (cell in cells) for (j in seq_len(n_per_cell)) {
  k <- k + 1L; id <- paste0("M", k); s <- simulate_one(cell)
  lines <- c(lines, paste(c(id, formatC(s$y, format = "f", digits = 4)), collapse = ","))
  gt[[k]] <- data.table(series_id = id, shock_type = cell$type, severity = cell$sev, shock_time = s$shock_time)
}
ensure_dir(file.path(out_data, "m4"))
writeLines(lines, file.path(out_data, "m4", "monthly_train.csv"))
fwrite(data.table::rbindlist(gt), file.path(out_data, "ground_truth.csv"))
cat(sprintf("wrote %d series (length %d) to %s\n", k, L, file.path(out_data, "m4", "monthly_train.csv")))
print(data.table::rbindlist(lapply(cells, function(c) data.table(type=c$type, severity=c$sev, n=n_per_cell))))
cat("persistent/burst shock at obs", shock_t, "| ground truth:", file.path(out_data, "ground_truth.csv"), "\n")
