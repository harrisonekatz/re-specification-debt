# Is the near-tie an artifact of forecasting only three steps against a twelve-period season?
# The structural worry: re-specification changes a model's long-range behavior (trend, seasonal
# form), which is the EXTRAPOLATION part of a forecast, and barely touches the short-run
# anchoring that recent data already pins down. At horizon 3 you are almost all anchoring, so a
# stale form and a fresh form give nearly the same forecast and everything looks identical. Past
# the seasonal period the seasonal-form choice finally lives inside a single forecast and the
# trend term has compounded, so the same two forms can diverge sharply. If so, the near-tie is
# horizon-bound, not universal.
#
# Test on the data where the near-tie actually occurred (real M4), holding seasonality at 12 and
# sweeping the horizon. Fix the sample to series long enough for the LONGEST horizon and evaluate
# those same series at every horizon, so only forecast depth changes, not the series in play.
# Headline metric: parameter_only relative loss vs full_update, i.e. the cost of never changing
# form = how much form matters. The prediction is that it is ~flat and near 1 at short horizons
# and rises as the horizon approaches and passes 12.
#
# Usage (repo root, tmux). Point --data at the real M4 monthly directory:
#   Rscript scripts/run_horizon_sweep.R --data data/m4 --n_series 4000 --n-jobs 8

suppressMessages(source("R/adaptive_update.R"))
library(data.table)
args     <- parse_cli_args()
n_jobs   <- get_arg(args, "n_jobs", default = max(1L, parallel::detectCores()), type = "integer")
data_dir <- get_arg(args, "data", default = "data/m4", type = "character")
n_series <- get_arg(args, "n_series", default = 4000L, type = "integer")
seed     <- get_arg(args, "seed", default = 123L, type = "integer")
train_length <- 36L; n_rounds <- 36L; seasonality <- 12L
horizons <- c(3L, 6L, 9L, 12L, 18L)
max_h <- max(horizons); L_min <- train_length + n_rounds + max_h + 2L   # long enough for the deepest horizon

# fixed subsample: only series that survive the longest horizon, same set used at every horizon
items <- load_m4_monthly(data_dir, n_series = -1L, min_obs = L_min)
set.seed(seed); if (length(items) > n_series) items <- items[sample(length(items), n_series)]
sub_dir <- "data_horizon_subsample"; ensure_dir(file.path(sub_dir, "m4"))
writeLines(vapply(items, function(it) paste(c(it$series_id, formatC(it$values, format = "f", digits = 4)), collapse = ","), character(1)),
           file.path(sub_dir, "m4", "monthly_train.csv"))
cat(sprintf("fixed subsample: %d series, each length >= %d (survives horizon %d)\n", length(items), L_min, max_h))

rows <- list()
for (h in horizons) {
  od <- sprintf("outputs/m4_horizon_h%02d", h)
  run_m4_experiment(
    data_dir = sub_dir, out_dir = od, n_series = -1L, seed = seed,
    n_rounds = n_rounds, horizon = h, seasonality = seasonality, train_length = train_length,
    fixed_frequencies = c(8L, 12L), adaptive_thresholds = c(0.8), adaptive_caps = c(8L),
    include_pure_adaptive = TRUE, include_capped_adaptive = TRUE,
    monitor_window = 6L, monitor_every = 3L,
    n_jobs = n_jobs, alpha = 0.0, gamma = 0.0, allow_multiplicative = TRUE
  )
  s <- fread(file.path(od, "summary.csv")); rl <- function(p) { v <- s[policy == p]$relative_loss; if (length(v)) v else NA_real_ }
  rec <- fread(file.path(od, "records.csv"), select = c("series_id","policy","origin_number","loss"))
  w <- dcast(rec[policy %in% c("adaptive_cap8_tau0.8","fixed_f8")], series_id + origin_number ~ policy, value.var = "loss")
  setnames(w, c("adaptive_cap8_tau0.8","fixed_f8"), c("a","f")); w <- w[is.finite(a) & is.finite(f)]; w[, gap := a - f]
  rows[[length(rows) + 1]] <- data.table(
    horizon = h,
    parameter_only_rl = rl("parameter_only"),      # cost of NEVER changing form (the headline)
    fixed_f8_rl = rl("fixed_f8"), fixed_f12_rl = rl("fixed_f12"),
    adaptive_cap8_rl = rl("adaptive_cap8_tau0.8"), adaptive_tau_rl = rl("adaptive_tau0.8"),
    spread = max(rl("parameter_only"), rl("fixed_f8"), rl("fixed_f12")) - 1.0,   # worst policy minus full_update
    matched_mean_gap = mean(w$gap), matched_median_gap = median(w$gap))
  cat(sprintf("  h=%2d done: parameter_only_rl=%.4f  spread=%.4f  matched_mean_gap=%.5f\n",
              h, rl("parameter_only"), rows[[length(rows)]]$spread, mean(w$gap)))
}
H <- rbindlist(rows)[order(horizon)]
cat("\n==================  policy separation vs forecast horizon (seasonality fixed at 12)  ==================\n")
cat("(full_update is the 1.0 reference each row; parameter_only_rl = cost of holding a stale form)\n\n")
print(H)

png("outputs/horizon_sweep.png", width = 1000, height = 430, res = 110)
op <- par(mfrow = c(1, 2), mar = c(4,4,3,1)); on.exit(par(op), add = TRUE)
plot(H$horizon, H$parameter_only_rl, type = "b", pch = 19, xlab = "forecast horizon", ylab = "relative loss vs full_update",
     main = "Cost of a stale form vs horizon", ylim = range(c(1, H$parameter_only_rl, H$fixed_f12_rl)))
lines(H$horizon, H$fixed_f12_rl, type = "b", pch = 1, lty = 2); abline(h = 1, col = "grey70", lty = 3); abline(v = 12, col = "firebrick", lty = 2)
legend("topleft", c("parameter_only","fixed_f12","full_update=1","season=12"), pch = c(19,1,NA,NA), lty = c(1,2,3,2),
       col = c("black","black","grey70","firebrick"), bty = "n")
plot(H$horizon, H$matched_mean_gap, type = "b", pch = 19, xlab = "forecast horizon", ylab = "cap8 minus fixed_f8 loss",
     main = "Trigger vs matched cadence vs horizon"); abline(h = 0, col = "grey70", lty = 3); abline(v = 12, col = "firebrick", lty = 2)
dev.off()

fwrite(H, "outputs/horizon_sweep.csv")
cat("\nReading guide:\n")
cat(" - parameter_only_rl rising as horizon approaches/passes 12 = form matters more at long horizons, the near-tie\n")
cat("   was horizon-bound, and 'less is more' is a short-horizon statement. Flat near 1 = the bunching is real at all horizons.\n")
cat(" - the firebrick line marks the seasonal period 12; a step there is the structural signature.\n")
cat(" - matched_mean_gap turning negative at long horizons = the trigger starts earning its keep once form matters.\n")
cat(" - the operational answer is whatever the curve reads at YOUR horizon (the one feeding earnings/treasury).\n")
cat("wrote horizon_sweep.csv and horizon_sweep.png to outputs/\n")
