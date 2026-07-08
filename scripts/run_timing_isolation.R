# Run the battery on the clean timing-isolation sweep and read the one thing it is built to
# isolate: the trigger's gap against its MATCHED cadence as switch timing goes from regular
# to irregular, with noise/count/magnitude all held fixed. A monotone slide from >= 0 at
# J0 to < 0 at J12, with median and trim agreeing, is the timing effect, clean.
#
# Usage (repo root, tmux), after simulate_timing_isolation.R:
#   Rscript scripts/run_timing_isolation.R --n-jobs 8

suppressMessages(source("R/adaptive_update.R"))
library(data.table)
args <- parse_cli_args()
n_jobs   <- get_arg(args, "n_jobs", default = max(1L, parallel::detectCores()), type = "integer")
data_dir <- get_arg(args, "data", default = "data_timing", type = "character")
out_dir  <- get_arg(args, "out_dir", default = "outputs/m4_timing_isolation", type = "character")
gt_path  <- get_arg(args, "ground_truth", default = file.path(data_dir, "ground_truth.csv"), type = "character")
debt_policy <- get_arg(args, "debt_policy", default = "adaptive_tau0.4", type = "character")
n_rounds <- 36L; horizon <- 3L; L <- 120L

run_m4_experiment(
  data_dir = data_dir, out_dir = out_dir, n_series = -1L, seed = 123L,
  n_rounds = n_rounds, horizon = horizon, seasonality = 12L, train_length = 36L,
  fixed_frequencies = c(6L, 8L, 12L), adaptive_thresholds = c(0.2, 0.4, 0.8, 1.5),
  adaptive_caps = c(8L, 12L), include_pure_adaptive = TRUE, include_capped_adaptive = TRUE,
  monitor_window = 6L, monitor_every = 3L,
  n_jobs = n_jobs, alpha = 0.0, gamma = 0.0, allow_multiplicative = TRUE
)

gt  <- fread(gt_path)
rec <- fread(file.path(out_dir, "records.csv"),
             select = c("series_id","policy","origin_number","loss","triggered_by_score"))
per <- merge(rec[, .(mean_loss = mean(loss, na.rm = TRUE)), by = .(series_id, policy)], gt, by = "series_id")
jorder <- c("static","J0_regular","J4","J8","J12_irregular")
per[, jitter := factor(jitter, levels = jorder)]

# per-cell relative loss + best adaptive vs best fixed
cell <- per[, .(mean_loss = mean(mean_loss)), by = .(jitter, jitter_num, policy)]
ref  <- cell[policy == "full_update", .(jitter, ref = mean_loss)]
cell <- merge(cell, ref, by = "jitter"); cell[, rl := mean_loss / ref]
winsum <- cell[, {
  ba <- .SD[grepl("^adaptive", policy)][which.min(rl)]; bf <- .SD[grepl("^fixed", policy)][which.min(rl)]
  .(winner = .SD[which.min(rl)]$policy, best_adaptive = ba$policy, best_ada_rl = round(ba$rl, 4),
    best_fixed_rl = round(bf$rl, 4), ada_beats_cadence = ba$rl < bf$rl)
}, by = .(jitter, jitter_num)][order(jitter_num)]
cat("\n==================  clean timing isolation: best adaptive vs best fixed cadence by regularity  ==================\n")
print(winsum)

# THE isolation: matched cadence gap by jitter, with robustness guard
w <- dcast(per[policy %in% c("adaptive_cap8_tau0.8","fixed_f8")], series_id + jitter + jitter_num ~ policy, value.var = "mean_loss")
setnames(w, c("adaptive_cap8_tau0.8","fixed_f8"), c("a","f")); w[, gap := a - f]
matched <- w[, .(n = .N, mean_gap = round(mean(gap),5), t = round(mean(gap)/(sd(gap)/sqrt(.N)),2),
                 median_gap = round(median(gap),5), trimmed_gap = round(mean(gap, trim = 0.05),5)), by = .(jitter, jitter_num)][order(jitter_num)]
cat("\n========  cap8 vs matched fixed_f8 by regularity (should slide from >= 0 at J0 to < 0 at J12)  ========\n")
print(matched)

# fire timing against switches for the pure rule
offset <- L - n_rounds - horizon + 1
sw <- gt[nzchar(switch_times), .(sround = as.integer(strsplit(switch_times, ";")[[1]]) - offset), by = series_id][sround >= 1L & sround <= n_rounds]
a <- rec[policy == debt_policy]
a[, round := if (max(origin_number, na.rm = TRUE) > n_rounds + horizon) origin_number - offset else origin_number]
fires <- merge(a[triggered_by_score == 1L, .(series_id, round)], gt[, .(series_id, jitter, jitter_num)], by = "series_id")
near_switch <- function(sid, r) { s <- sw[series_id == sid]$sround; if (!length(s)) return(FALSE); any(r >= s & r <= s + 4L) }
fires[, near := mapply(near_switch, series_id, round)]
firetime <- fires[, .(fires_per_series = round(.N / uniqueN(gt[jitter == .BY[[1]]]$series_id), 3),
                      frac_near_switch = round(mean(near), 2)), by = .(jitter, jitter_num)][order(jitter_num)]
cat("\n========  ", debt_policy, " fire timing vs switches by regularity  ========\n")
print(firetime)

# trend plot: matched gap vs jitter
png(file.path(out_dir, "timing_isolation_gap.png"), width = 760, height = 460, res = 110)
m <- matched[order(jitter_num)]
plot(m$jitter_num, m$mean_gap, type = "b", pch = 19, xlab = "switch-timing jitter (0 = regular, 12 = irregular; -1 = static)",
     ylab = "cap8 minus matched fixed_f8 loss", main = "Timing effect, clean estimation")
points(m$jitter_num, m$median_gap, type = "b", pch = 1, lty = 2)
abline(h = 0, col = "grey60", lty = 3)
legend("topright", c("mean gap","median gap"), pch = c(19,1), lty = c(1,2), bty = "n")
dev.off()

fwrite(winsum, file.path(out_dir, "timing_winsum.csv"))
fwrite(matched, file.path(out_dir, "timing_matched_by_jitter.csv"))
fwrite(firetime, file.path(out_dir, "timing_firetime.csv"))
cat("\nReading guide:\n")
cat(" - matched gap should be ~0 at static, >= 0 at J0 (regular, cadence matches), and turn negative by J12.\n")
cat("   That monotone slide, with median/trim agreeing and noise held fixed, is the timing effect isolated.\n")
cat(" - ada_beats_cadence should flip to TRUE only as jitter grows.\n")
cat(" - if the gap stays >= 0 even at J12, the clean timing effect does not survive isolation and the drift-grid\n")
cat("   irregular/clean result was carried by something else.\n")
cat("wrote timing_winsum.csv, timing_matched_by_jitter.csv, timing_firetime.csv, timing_isolation_gap.png to ", out_dir, "\n")
