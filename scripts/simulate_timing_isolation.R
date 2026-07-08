# Isolate the timing effect, free of the noise-robustness confound. In the drift grid the
# trigger's edge bundled two things: a large win from noisy validation (which showed up even
# in static/noisy, where there is no drift) and a smaller, clean win under irregular timing.
# This design strips the first away: estimation is CLEAN throughout, drift magnitude and
# switch COUNT are held fixed at four, and the only thing that varies is how regular the
# switch times are. Same A/B form drift as before (trend+multiplicative vs flat+additive).
#
# Regularity dial = jitter J around a base cadence of 24. J=0 puts switches at exactly
# 24,48,72,96 (a fixed cadence of 24 can match them). J=12 spreads each switch uniformly
# across its whole 24-slot (fully irregular, no cadence can match). Static (no switches) is
# the neutral anchor. The claim: cap8 vs its matched fixed_f8 gap moves monotonically from
# >= 0 at J=0 to < 0 as J grows. That swing, with noise and count and magnitude all fixed,
# is the timing effect and nothing else.
#
# Usage (repo root):
#   Rscript scripts/simulate_timing_isolation.R
#   Rscript scripts/simulate_timing_isolation.R --n_per_cell 800

suppressMessages(source("R/adaptive_update.R"))
library(data.table)
args <- parse_cli_args()
seed       <- get_arg(args, "seed", default = 123L, type = "integer")
n_per_cell <- get_arg(args, "n_per_cell", default = 600L, type = "integer")
out_data   <- get_arg(args, "out_data", default = "data_timing", type = "character")
L          <- get_arg(args, "length", default = 120L, type = "integer")
set.seed(seed)

period <- 12L; sigma <- 0.02                      # CLEAN estimation only
g <- 0.006; ampA <- 0.30; ampB <- 0.30
shapeA <- sin(2*pi*(1:period)/period); shapeB <- sin(2*pi*(1:period)/period + pi/2)
base_sw <- c(24L, 48L, 72L, 96L)                  # fixed count, fixed magnitude across all cells

switch_times <- function(jitter_num) {
  if (jitter_num < 0L) return(integer(0))                        # static anchor
  if (jitter_num == 0L) return(base_sw)                         # perfectly regular
  s <- base_sw + round(runif(length(base_sw), -jitter_num, jitter_num))
  sort(pmin(L - 4L, pmax(6L, s)))
}

gen_series <- function(jitter_num) {
  level0 <- runif(1, 800, 1500); sw <- switch_times(jitter_num)
  bounds <- c(1L, sw, L + 1L); regs <- rep(c("A","B"), length.out = length(bounds) - 1L)
  regime <- rep("A", L)
  for (i in seq_len(length(bounds) - 1L)) regime[bounds[i]:(bounds[i+1] - 1L)] <- regs[i]
  y <- numeric(L); level <- level0; amp_abs <- ampB * level0
  for (t in 1:L) {
    m <- ((t - 1L) %% period) + 1L
    if (regime[t] == "A") { level <- level * (1 + g); y[t] <- level * (1 + ampA * shapeA[m]) }
    else { if (t == 1L || regime[t-1L] != "B") amp_abs <- ampB * level; y[t] <- level + amp_abs * shapeB[m] }
  }
  list(y = pmax(y * exp(rnorm(L, 0, sigma)), 1e-6), switches = sw)
}

cells <- list(
  list(label="static", jnum=-1L), list(label="J0_regular", jnum=0L),
  list(label="J4", jnum=4L), list(label="J8", jnum=8L), list(label="J12_irregular", jnum=12L)
)

lines <- character(0); gt <- list(); k <- 0L
for (cell in cells) for (j in seq_len(n_per_cell)) {
  k <- k + 1L; id <- paste0("M", k); s <- gen_series(cell$jnum)
  lines <- c(lines, paste(c(id, formatC(s$y, format = "f", digits = 4)), collapse = ","))
  gt[[k]] <- data.table(series_id = id, jitter = cell$label, jitter_num = cell$jnum,
                        n_switches = length(s$switches), switch_times = paste(s$switches, collapse = ";"))
}
ensure_dir(file.path(out_data, "m4"))
writeLines(lines, file.path(out_data, "m4", "monthly_train.csv"))
fwrite(rbindlist(gt), file.path(out_data, "ground_truth.csv"))
cat(sprintf("wrote %d series (length %d) to %s\n", k, L, file.path(out_data, "m4", "monthly_train.csv")))
print(rbindlist(gt)[, .(mean_switches = round(mean(n_switches), 2)), by = .(jitter, jitter_num)])
cat("ground truth:", file.path(out_data, "ground_truth.csv"), "\n")
