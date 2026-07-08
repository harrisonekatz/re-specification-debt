# The reframed leg, shown on one dataset. Two things both get called "debt":
#   - IC debt (spec_debt_aicc): the in-sample information-criterion weight of the deployed
#     form. The cheap surrogate. It failed six ways: flat-to-negative against realized loss,
#     signal dominated by noise level, sign-flipping across the noise axis.
#   - loss-based debt (score_gap): the validation-window gap, deployed form minus best
#     challenger on held-out data. The realized-degradation signal the trigger fires on.
#
# Both are recorded on the same held-form rows (frozen form, no firing selection), so we can
# read each against realized test-window loss on one dataset and show they diverge. The fair-
# comparison point: BOTH are in-window diagnostics computed at the same origin, and realized
# loss is the future test window, so neither has a mechanical edge from sharing the window;
# the question is purely which diagnostic tracks the realized error.
#
# Claim, if it holds: scoregap_vs_loss is positive and climbs under drift while
# icdebt_vs_loss stays ~0/noise, and the two debts decorrelate from each other. That is "the
# loss-based debt works, the IC surrogate does not, and here is where they part."
#
# Usage (repo root), after run_heldform_debt.R has written heldform_records.csv:
#   Rscript scripts/analyze_heldform_divergence.R

suppressMessages(source("R/adaptive_update.R"))
library(data.table)
args    <- parse_cli_args()
out_dir <- get_arg(args, "out_dir", default = "outputs/m4_drift_experiment", type = "character")
data_dir<- get_arg(args, "data",    default = "data_drift", type = "character")
gt_path <- get_arg(args, "ground_truth", default = file.path(data_dir, "ground_truth.csv"), type = "character")

rec <- fread(file.path(out_dir, "heldform_records.csv"))
gt  <- fread(gt_path)
d   <- merge(rec[is.finite(spec_debt_aicc) & is.finite(score_gap) & is.finite(loss)],
             gt[, .(series_id, drift_mode, noise)], by = "series_id")

sp <- function(a, b) round(suppressWarnings(cor(a, b, method = "spearman", use = "complete.obs")), 3)
div <- d[, .(
  icdebt_vs_loss      = sp(spec_debt_aicc, loss),      # the surrogate vs realized error (should stay ~0/noise)
  scoregap_vs_loss    = sp(score_gap,      loss),      # the loss-based debt vs realized error (should climb under drift)
  icdebt_vs_scoregap  = sp(spec_debt_aicc, score_gap), # do the two debts even agree? (divergence)
  n = .N), by = .(drift_mode, noise)][order(drift_mode, noise)]

cat("=== two 'debts' against realized loss, on one dataset (held form, no selection) ===\n")
cat("scoregap_vs_loss = loss-based debt; icdebt_vs_loss = IC surrogate; icdebt_vs_scoregap = do they agree\n\n")
print(div)

# compact contrast: average over drift cells vs static cells, to state the divergence plainly
drift <- div[drift_mode %in% c("irregular","regular")]
stat  <- div[drift_mode == "static"]
cat(sprintf("\nmean over DRIFT cells:  scoregap_vs_loss=%.3f   icdebt_vs_loss=%.3f\n",
            mean(drift$scoregap_vs_loss), mean(drift$icdebt_vs_loss)))
cat(sprintf("mean over STATIC cells: scoregap_vs_loss=%.3f   icdebt_vs_loss=%.3f\n",
            mean(stat$scoregap_vs_loss), mean(stat$icdebt_vs_loss)))

fwrite(div, file.path(out_dir, "heldform_debt_divergence.csv"))
cat("\nReading guide:\n")
cat(" - if scoregap_vs_loss is clearly positive (esp. under drift) while icdebt_vs_loss is ~0/negative,\n")
cat("   the loss-based debt is a working signal and the IC surrogate is not: the reframed leg, on one dataset.\n")
cat(" - icdebt_vs_scoregap weak or sign-unstable = the surrogate has broken from the thing it surrogates.\n")
cat(" - watch noise: if scoregap_vs_loss holds its sign across clean/noisy but icdebt does not, that is the\n")
cat("   cleanest statement of which signal is robust.\n")
cat("wrote heldform_debt_divergence.csv to", out_dir, "\n")
