# The regime the debt rule should win, built to be findable rather than stumbled into.
# Everything we ran missed it: parametric breaks were a single change full_update just
# re-fits through, contamination was noise restraint ignores. The debt rule needs drift
# that is genuine, persistent, and IRREGULARLY timed: genuine and persistent so reacting
# pays, irregular so no fixed cadence can match it. When drift is uniform you tune a clock
# to it and a cadence wins; when it arrives in unpredictable bursts the accumulated debt
# rises during the bursts, the rule fires there and rests between, and that is the edge.
#
# A latent form state switches between two structurally different forms:
#   A: upward trend, MULTIPLICATIVE seasonality (swing scales with the growing level).
#   B: flat level, ADDITIVE seasonality (constant absolute swing).
# Trend on/off and seasonal A/M are distinct ETS forms, so a form fit in one becomes a
# measurably worse fit in the other. Level is carried across switches, so the change is in
# the dynamics, not a level step (that keeps it distinct from the level_shift experiment).
#
# Falsification grid = {static, irregular, regular} timing x {clean, noisy} estimation.
# The claim under test: the debt-triggered rule wins ONLY in irregular x noisy.
#   - regular timing  -> a matched fixed cadence should take it back (the win was timing).
#   - clean estimation -> full_update should take it back (the win needed the noise: noisy
#     validation makes full_update chase noise and flip-flop forms, so a sticky rule that
#     holds form through the calm spans and fires at switches beats it).
#   - static rows      -> restraint wins; drift, not noise, must be doing the work.
# If the rule wins everywhere, we tuned a knob, not found a regime.
#
# Usage (repo root):
#   Rscript scripts/simulate_drift_regime.R
#   Rscript scripts/simulate_drift_regime.R --n_per_cell 800 --seed 7

suppressMessages(source("R/adaptive_update.R"))
library(data.table)
args <- parse_cli_args()
seed       <- get_arg(args, "seed", default = 123L, type = "integer")
n_per_cell <- get_arg(args, "n_per_cell", default = 600L, type = "integer")
out_data   <- get_arg(args, "out_data", default = "data_drift", type = "character")
L          <- get_arg(args, "length", default = 120L, type = "integer")
set.seed(seed)

period <- 12L
g      <- 0.006                                   # per-period trend growth in regime A
ampA   <- 0.30; ampB <- 0.30
shapeA <- sin(2*pi*(1:period)/period)             # one seasonal shape
shapeB <- sin(2*pi*(1:period)/period + pi/2)      # a different (phase-shifted) shape

switch_times <- function(mode) {
  if (mode == "static")  return(integer(0))
  if (mode == "regular") { k <- 24L; ph <- sample(0:11, 1); s <- seq(k + ph, L - 4L, by = k); return(s) }
  s <- which(runif(L) < 0.033); s <- s[s > 4L & s < L - 2L]      # irregular: low per-period prob, clusters
  if (length(s) > 6L) s <- sort(sample(s, 6L))
  s
}

gen_series <- function(mode, noise) {
  sigma <- if (noise == "noisy") 0.12 else 0.02
  level0 <- runif(1, 800, 1500); sw <- switch_times(mode)
  bounds <- c(1L, sw, L + 1L); regs <- rep(c("A","B"), length.out = length(bounds) - 1L)
  regime <- rep("A", L)
  for (i in seq_len(length(bounds) - 1L)) regime[bounds[i]:(bounds[i+1] - 1L)] <- regs[i]
  y <- numeric(L); level <- level0; amp_abs <- ampB * level0
  for (t in 1:L) {
    m <- ((t - 1L) %% period) + 1L
    if (regime[t] == "A") { level <- level * (1 + g); y[t] <- level * (1 + ampA * shapeA[m]) }
    else { if (t == 1L || regime[t-1L] != "B") amp_abs <- ampB * level; y[t] <- level + amp_abs * shapeB[m] }
  }
  y <- y * exp(rnorm(L, 0, sigma))
  list(y = pmax(y, 1e-6), switches = sw)
}

cells <- list(
  list(mode="static",    noise="clean"), list(mode="static",    noise="noisy"),
  list(mode="irregular", noise="clean"), list(mode="irregular", noise="noisy"),
  list(mode="regular",   noise="clean"), list(mode="regular",   noise="noisy")
)

lines <- character(0); gt <- list(); k <- 0L
for (cell in cells) for (j in seq_len(n_per_cell)) {
  k <- k + 1L; id <- paste0("M", k); s <- gen_series(cell$mode, cell$noise)
  lines <- c(lines, paste(c(id, formatC(s$y, format = "f", digits = 4)), collapse = ","))
  gt[[k]] <- data.table(series_id = id, drift_mode = cell$mode, noise = cell$noise,
                        n_switches = length(s$switches),
                        switch_times = paste(s$switches, collapse = ";"))
}
ensure_dir(file.path(out_data, "m4"))
writeLines(lines, file.path(out_data, "m4", "monthly_train.csv"))
fwrite(data.table::rbindlist(gt), file.path(out_data, "ground_truth.csv"))
cat(sprintf("wrote %d series (length %d) to %s\n", k, L, file.path(out_data, "m4", "monthly_train.csv")))
G <- rbindlist(gt)
print(G[, .(mean_switches = round(mean(n_switches), 2)), by = drift_mode])
cat("ground truth:", file.path(out_data, "ground_truth.csv"), "\n")
