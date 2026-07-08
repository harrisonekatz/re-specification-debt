# Run the same policies on the out-of-model shock series and read out, with the
# guards the last two runs taught us:
#   1. matched cadence: cap8 vs fixed_f8 and cap12 vs fixed_f12, not a mismatched
#      baseline, so the trigger is isolated from the cadence.
#   2. who actually wins each cell, so a "win" has to be the best policy, not just
#      better than one comparator.
#   3. median and 5% trimmed gaps next to the mean, so a heavy tail of a few series
#      can't masquerade as a class effect.
#   4. timing: for shocks with a known time, do the trigger's fires land near the
#      shock (reaction) or at the first monitoring round (initial-fit artifact)? This
#      is the thing the real-data trajectories warned us about.
#
# Usage (repo root, in tmux), after simulate_shock_series.R:
#   Rscript scripts/run_shock_experiment.R --n-jobs 8

suppressMessages(source("R/adaptive_update.R"))
library(data.table)
args <- parse_cli_args()
n_jobs   <- get_arg(args, "n_jobs", default = max(1L, parallel::detectCores()), type = "integer")
data_dir <- get_arg(args, "data", default = "data_shock", type = "character")
out_dir  <- get_arg(args, "out_dir", default = "outputs/m4_shock_experiment", type = "character")
gt_path  <- get_arg(args, "ground_truth", default = file.path(data_dir, "ground_truth.csv"), type = "character")
n_rounds <- 36L; horizon <- 3L; L <- 120L

run_m4_experiment(
  data_dir = data_dir, out_dir = out_dir,
  n_series = -1L, seed = 123L,
  n_rounds = n_rounds, horizon = horizon, seasonality = 12L, train_length = 36L,
  fixed_frequencies = c(6L, 8L, 12L),
  adaptive_thresholds = c(0.2, 0.4, 0.8, 1.5),
  adaptive_caps = c(8L, 12L),
  include_pure_adaptive = TRUE, include_capped_adaptive = TRUE,
  monitor_window = 12L, monitor_every = 6L,
  n_jobs = n_jobs, alpha = 0.0, gamma = 0.0, allow_multiplicative = TRUE
)

gt  <- fread(gt_path)
rec <- fread(file.path(out_dir, "records.csv"),
             select = c("series_id","policy","origin_number","loss","score_gap","triggered_by_score"))
per <- rec[, .(mean_loss = mean(loss, na.rm = TRUE)), by = .(series_id, policy)]
per <- merge(per, gt, by = "series_id")
cellkey <- c("shock_type","severity")

# (1)+(2) per-cell relative loss and the winner
cell <- per[, .(mean_loss = mean(mean_loss)), by = c(cellkey, "policy")]
ref  <- cell[policy == "full_update", .(shock_type, severity, ref = mean_loss)]
cell <- merge(cell, ref, by = cellkey); cell[, relative_loss := mean_loss / ref]
winner <- cell[, .SD[which.min(relative_loss)], by = cellkey][, .(shock_type, severity, winner = policy, winner_rl = round(relative_loss, 5))]
cat("\n================  who wins each cell (relative loss vs full_update)  ================\n")
print(winner[order(shock_type, severity)])

# (3) matched-cadence paired comparison, with median and trimmed guards
matched <- function(ad, fx) {
  w <- dcast(per[policy %in% c(ad, fx)], series_id + shock_type + severity ~ policy, value.var = "mean_loss")
  setnames(w, c(ad, fx), c("a","f"))
  w[, gap := a - f]                       # < 0 means the trigger beats its matched cadence
  w[, .(pair = paste(ad, "vs", fx), n = .N,
        mean_gap = round(mean(gap), 5), se = round(sd(gap)/sqrt(.N), 5),
        t = round(mean(gap)/(sd(gap)/sqrt(.N)), 2),
        median_gap = round(median(gap), 5),
        trimmed_gap = round(mean(gap, trim = 0.05), 5)), by = .(shock_type, severity)]
}
m8  <- matched("adaptive_cap8_tau0.8",  "fixed_f8")
m12 <- matched("adaptive_cap12_tau0.8", "fixed_f12")
cat("\n========  trigger vs MATCHED cadence (negative t beyond -2 = real win; check median/trimmed agree)  ========\n")
print(rbind(m8, m12)[order(shock_type, severity)])

# (4) timing: do fires land at the shock or at the initial fit?
offset <- L - n_rounds - horizon + 1                      # round 1 <-> this origin
a <- rec[policy == "adaptive_cap8_tau0.8"]
a[, round := if (max(origin_number, na.rm = TRUE) > n_rounds + horizon) origin_number - offset else origin_number]
fires <- merge(a[triggered_by_score == 1L, .(series_id, round)], gt, by = "series_id")
fires <- fires[is.finite(shock_time)]
fires[, shock_round := shock_time - offset]
timing <- fires[, .(
  n_fired = .N,
  shock_round = round(median(shock_round), 0),
  median_fire_round = round(median(round), 0),
  frac_at_first_monitor = round(mean(round <= 6L), 2),
  frac_in_reaction_window = round(mean(round >= shock_round - 2L & round <= shock_round + 5L), 2)
), by = .(shock_type, severity)][order(shock_type, severity)]
cat("\n========  fire timing for shocks with a known time  ========\n")
cat("(reaction = fires cluster in the window around shock_round; artifact = fires cluster at the first monitor round, 6)\n")
print(timing)

# timing plot: fire-round distribution per shock type, shock round marked
types <- unique(fires$shock_type)
png(file.path(out_dir, "shock_fire_timing.png"), width = 1300, height = 380 * ceiling(length(types)/3), res = 110)
op <- par(mfrow = c(ceiling(length(types)/3), 3), mar = c(4,4,2,1)); on.exit(par(op), add = TRUE)
for (tp in types) {
  d <- fires[shock_type == tp]; sr <- median(d$shock_round)
  hist(d$round, breaks = seq(0, n_rounds, by = 2), main = tp, xlab = "fire round", col = "grey80", border = "white")
  abline(v = sr, col = "firebrick", lwd = 2); abline(v = 6, col = "steelblue", lty = 2)
}
dev.off()

fwrite(winner, file.path(out_dir, "shock_winner_by_cell.csv"))
fwrite(rbind(m8, m12), file.path(out_dir, "shock_matched_comparison.csv"))
fwrite(timing, file.path(out_dir, "shock_fire_timing.csv"))
cat("\nReading guide:\n")
cat(" - control should tie (winner full_update or a fixed cadence, matched gap ~0).\n")
cat(" - a real win: the trigger is the cell winner AND beats its matched cadence past -2 SE AND median/trimmed agree.\n")
cat(" - timing: frac_in_reaction_window high and frac_at_first_monitor low means the trigger reacts to the shock,\n")
cat("   not the initial fit. The red line (shock) vs blue line (first monitor) in the plot shows it at a glance.\n")
cat("wrote shock_winner_by_cell.csv, shock_matched_comparison.csv, shock_fire_timing.csv, shock_fire_timing.png to ", out_dir, "\n")
