# Why pairing repairs the noise correctly, and which question the clean result actually
# answers, made visible on the records rather than argued. Three things:
#
#   1. Pairing works because the two arms share most cells EXACTLY. On rounds where the
#      trigger has not fired, trigger and cadence deploy the same form on the same data and
#      produce the same forecast, so the difference is structurally zero, not small. The
#      between-series difficulty that dominates raw loss variance is common to both arms and
#      cancels when differenced. We show this by printing the unpaired SE of the mean gap
#      next to the paired SE: the gap between them is the variance pairing removes.
#
#   2. Restricting to divergent rounds is not cherry-picking. The same-forecast vs
#      different-forecast split is set by the re-specification SCHEDULES, fixed before any
#      loss is seen, so it conditions on treatment-divergence, not on the outcome. The sign
#      test is exactly invariant to the zero rounds (a zero has no sign). The paired t with
#      vs without the zeros is only APPROXIMATELY equal in this regime (tiny mean relative to
#      spread); we print both so the approximation is shown, not asserted.
#
#   3. Frequency is not magnitude. The sign (how often the trigger is worse) can be sharp
#      while the mean and median (by how much) sit at zero: a near-tie. The tail
#      decomposition asks the magnitude question directly: do the largest divergences favor
#      the trigger (tail insurance: loses small often, wins big rarely) or the cadence, and
#      are those large divergences spread across many series or concentrated in a pathological
#      few (the MASE-artifact risk).
#
# Usage (repo root), pointed at any records.csv with series_id/policy/origin_number/loss:
#   Rscript scripts/paired_diagnostics.R --records outputs/m4_full_capped/records.csv
#   Rscript scripts/paired_diagnostics.R --policy_a adaptive_cap8_tau0.8 --policy_b fixed_f8

suppressMessages(source("R/adaptive_update.R"))
library(data.table)
args     <- parse_cli_args()
recpath  <- get_arg(args, "records", default = "outputs/m4_drift_experiment/records.csv", type = "character")
pa       <- get_arg(args, "policy_a", default = "adaptive_cap8_tau0.8", type = "character")
pb       <- get_arg(args, "policy_b", default = "fixed_f8", type = "character")
tail_q   <- get_arg(args, "tail_quantile", default = 0.95, type = "numeric")

# records may be one csv OR a directory of large parts (the streaming full run, ~7GB across 48
# files). Either way we only need two policies, so filter each file to {pa, pb} on read and keep
# just that slice; the other ~15 policies are what make the parts huge and we never hold them.
read_records_2policies <- function(path, keep) {
  files <- if (dir.exists(path)) sort(list.files(path, pattern = "\\.csv$", full.names = TRUE)) else path
  stopifnot(length(files) > 0)
  cols <- c("series_id","policy","origin_number","loss")
  parts <- vector("list", length(files))
  for (i in seq_along(files)) {
    d <- fread(files[i], select = cols)
    parts[[i]] <- d[policy %in% keep]
    if (length(files) > 1L) cat(sprintf("  read %d/%d: %s -> %d kept rows\n", i, length(files), basename(files[i]), nrow(parts[[i]])))
  }
  rbindlist(parts)
}
rec <- read_records_2policies(recpath, c(pa, pb))
w <- dcast(rec[policy %in% c(pa, pb)], series_id + origin_number ~ policy, value.var = "loss")
setnames(w, c(pa, pb), c("a","b")); w <- w[is.finite(a) & is.finite(b)]
w[, gap := a - b]                                  # < 0 = trigger (policy_a) lower loss
n_all <- nrow(w); nz <- w[gap != 0]; n_diff <- nrow(nz)

cat(sprintf("\npaired diagnostics: %s (a) vs %s (b)   [gap = a - b; negative = trigger better]\n", pa, pb))
cat(sprintf("paired rounds: %d | divergent rounds (gap != 0): %d (%.1f%%) | identical-forecast rounds: %.1f%%\n",
            n_all, n_diff, 100*n_diff/n_all, 100*(n_all-n_diff)/n_all))

## 1. why pairing helps: unpaired vs paired SE of the mean gap
se_unpaired <- sqrt(var(w$a)/n_all + var(w$b)/n_all)   # treats arms as independent (wrong here)
se_paired   <- sd(w$gap)/sqrt(n_all)                   # differences cancel shared difficulty
cat(sprintf("\n[1] SE of the mean gap:  unpaired(independent-arms)=%.5f   paired=%.5f   ratio=%.1fx\n",
            se_unpaired, se_paired, se_unpaired/se_paired))
cat("    the ratio is the between-series variance pairing removes; it was never real uncertainty about the gap.\n")

## 2. zeros are inert for the sign test; approximately so for the t
t_all <- mean(w$gap)/(sd(w$gap)/sqrt(n_all))
t_nz  <- mean(nz$gap)/(sd(nz$gap)/sqrt(n_diff))
n_worse <- nz[gap > 0, .N]                              # trigger worse
sign_z  <- (n_worse - n_diff/2) / sqrt(n_diff/4)
cat(sprintf("\n[2] paired t over ALL rounds=%.2f   over divergent-only=%.2f   (close, not identical: mean << spread)\n", t_all, t_nz))
cat(sprintf("    sign on divergent rounds: trigger worse on %d/%d = %.1f%%   (sign-test z = %.1f)\n",
            n_worse, n_diff, 100*n_worse/n_diff, sign_z))

## 3. frequency vs magnitude, then the tail (the insurance question)
cat(sprintf("\n[3] magnitude on divergent rounds:  mean gap=%.5f   median=%.5f   5%%-trimmed mean=%.5f\n",
            mean(nz$gap), median(nz$gap), mean(nz$gap, trim = 0.05)))
thr <- quantile(abs(nz$gap), tail_q)
tail <- nz[abs(gap) >= thr]; bulk <- nz[abs(gap) < thr]
tail_share <- sum(tail$gap) / sum(nz$gap)
cat(sprintf("    top %.0f%% by |gap| (the big divergences): mean gap=%.4f | trigger WINS %.0f%% of them | %.0f%% of total gap-sum sits here\n",
            100*(1-tail_q), mean(tail$gap), 100*mean(tail$gap < 0), 100*tail_share))
cat(sprintf("    bulk (smaller divergences):              mean gap=%.4f | trigger wins %.0f%%\n",
            mean(bulk$gap), 100*mean(bulk$gap < 0)))
cat("    read: mean<0 while trigger worse >50%% of the time = tail insurance (loses small often, wins big rarely).\n")

## artifact check: are the big divergences spread or concentrated?
sc <- tail[, .(mass = sum(abs(gap))), by = series_id][order(-mass)]
cat(sprintf("\n[artifact check] big divergences span %d distinct series; top 10 series hold %.0f%% of the tail |gap| mass\n",
            nrow(sc), 100*sum(head(sc$mass,10))/sum(sc$mass)))
cat("    concentrated in a few series => suspect MASE blowups on degenerate series; spread => more likely real saves.\n")

out <- file.path(dirname(recpath), "paired_diagnostics.csv")
fwrite(data.table(policy_a=pa, policy_b=pb, n_paired=n_all, n_divergent=n_diff,
                  se_unpaired=se_unpaired, se_paired=se_paired,
                  t_all=t_all, t_divergent_only=t_nz, pct_trigger_worse=100*n_worse/n_diff, sign_z=sign_z,
                  mean_gap=mean(nz$gap), median_gap=median(nz$gap), trimmed_gap=mean(nz$gap, trim=0.05),
                  tail_mean_gap=mean(tail$gap), tail_pct_trigger_wins=100*mean(tail$gap<0), tail_share_of_sum=100*tail_share,
                  tail_n_series=nrow(sc), tail_top10_mass_pct=100*sum(head(sc$mass,10))/sum(sc$mass)), out)
cat("\nwrote", out, "\n")
