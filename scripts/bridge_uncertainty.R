#!/usr/bin/env Rscript
# =====================================================================
# bridge_uncertainty.R   (v2, matches your schema; reviewer 3.9)
#
# Puts uncertainty on the Figure 3 / Section 8.5 claim that the score gap
# orders the value of a triggered re-specification and the IC debt does not.
# For the origins the score gap actually fired (triggered_by_score == 1):
#   (A) regress adaptive-minus-fixed loss on the score gap, then on the IC
#       debt, with a series-cluster bootstrap CI and p on each slope.
#       Expect a clearly negative score-gap slope (bigger gap, more benefit)
#       and a near-zero IC-debt slope.
#   (B) the same points binned by each signal, now with counts and cluster
#       bootstrap CIs on the bin mean, to redraw Figure 3 with error bars.
#
# Uses spec_debt_aicc (= -log deployed AICc weight) as the IC signal, which
# is what the text calls "IC-weight debt". Relabel the Figure 3 right axis to
# match and the reviewer's margin-vs-debt ambiguity goes away.
#
# Run from the PROJECT ROOT.
# =====================================================================

suppressWarnings(suppressMessages(library(data.table)))

## ----------------------------- CONFIG -------------------------------
run_dir <- "outputs/m4_full_capped"
policy_adaptive <- "adaptive_cap8_tau0.8"
policy_fixed    <- "fixed_f8"

col_policy   <- "policy"
col_series   <- "series_id"
col_round    <- "origin_number"
col_loss     <- "loss"
col_scoregap <- "score_gap"
col_icdebt   <- "spec_debt_aicc"
col_trigflag <- "triggered_by_score"   # 1 at origins the score gap fired

n_bins  <- 5L
n_boot  <- 2000L
seed    <- 123L
out_reg  <- "outputs/bridge_regression.csv"
out_bins <- "outputs/bridge_bins_ci.csv"
## --------------------------------------------------------------------
set.seed(seed)

policy_file <- function(dir, pol) {
  fs <- list.files(file.path(dir, "by_policy"), pattern = "\\.csv$", full.names = TRUE)
  if (!length(fs)) stop("No by_policy/*.csv under ", dir)
  nm <- vapply(fs, function(f) as.character(fread(f, select = col_policy, nrows = 1L)[[1L]]), character(1))
  m  <- stats::setNames(fs, nm)
  if (!pol %in% names(m)) stop(sprintf("Policy '%s' not found. Have: %s", pol, paste(sort(names(m)), collapse = ", ")))
  m[[pol]]
}

fa <- policy_file(run_dir, policy_adaptive); message("adaptive <- ", basename(fa))
ff <- policy_file(run_dir, policy_fixed);    message("fixed    <- ", basename(ff))

adf <- fread(fa, select = c(col_series, col_round, col_loss, col_scoregap, col_icdebt, col_trigflag))
setnames(adf, c(col_series, col_round, col_loss, col_scoregap, col_icdebt, col_trigflag),
         c("series", "round", "loss_ad", "score_gap", "ic", "trig"))
adf <- adf[trig == 1]

fx <- fread(ff, select = c(col_series, col_round, col_loss))
setnames(fx, c(col_series, col_round, col_loss), c("series", "round", "loss_fix"))

d <- merge(adf[, .(series, round, loss_ad, score_gap, ic)], fx, by = c("series", "round"))
d[, loss_diff := loss_ad - loss_fix]                 # <0 favors re-specifying
d <- d[is.finite(loss_diff) & is.finite(score_gap) & is.finite(ic)]
message("triggered origins used: ", nrow(d), " across ", uniqueN(d$series), " series")

# ---- (A) cluster-bootstrap slope of loss_diff on a signal ----------
boot_slope <- function(dat, signal, B) {
  x <- dat[[signal]]; y <- dat$loss_diff
  b_hat <- unname(coef(lm(y ~ x))[2])
  idx <- split(seq_len(nrow(dat)), dat$series); cl <- names(idx); G <- length(cl)
  bs <- numeric(B)
  for (b in seq_len(B)) {
    rows <- unlist(idx[sample(cl, G, replace = TRUE)], use.names = FALSE)
    bs[b] <- unname(coef(lm(y[rows] ~ x[rows]))[2])
  }
  ci <- quantile(bs, c(0.025, 0.975), names = FALSE)
  data.table(signal = signal, slope = b_hat, ci_lo = ci[1], ci_hi = ci[2],
             boot_p = min(2 * min(mean(bs <= 0), mean(bs >= 0)), 1))
}
reg <- rbindlist(list(boot_slope(d, "score_gap", n_boot), boot_slope(d, "ic", n_boot)))
print(reg); fwrite(reg, out_reg)

# ---- (B) bins with counts and cluster-bootstrap CIs ----------------
boot_bins <- function(dat, signal, k, B) {
  br <- quantile(dat[[signal]], seq(0, 1, length.out = k + 1), names = FALSE, type = 8)
  br[1] <- -Inf; br[length(br)] <- Inf
  dat <- copy(dat)[, bin := cut(get(signal), br, include.lowest = TRUE, labels = FALSE)]
  idx <- split(seq_len(nrow(dat)), dat$series); cl <- names(idx); G <- length(cl)
  boot <- matrix(NA_real_, B, k)
  for (b in seq_len(B)) {
    bb <- dat[unlist(idx[sample(cl, G, replace = TRUE)], use.names = FALSE)]
    mv <- bb[, .(m = mean(loss_diff)), by = bin][order(bin)]
    boot[b, mv$bin] <- mv$m
  }
  s <- dat[, .(n = .N, mean_signal = mean(get(signal)), mean_loss_diff = mean(loss_diff),
               median_loss_diff = median(loss_diff), share_worse = mean(loss_diff > 0)),
           by = bin][order(bin)]
  s[, ci_lo := apply(boot, 2, quantile, 0.025, na.rm = TRUE)[bin]]
  s[, ci_hi := apply(boot, 2, quantile, 0.975, na.rm = TRUE)[bin]]
  s[, signal := signal][]
}
bins <- rbindlist(list(boot_bins(d, "score_gap", n_bins, n_boot), boot_bins(d, "ic", n_bins, n_boot)))
print(bins); fwrite(bins, out_bins)
message("\nWrote ", out_reg, " and ", out_bins,
        ".\nRedraw Figure 3 with ci_lo/ci_hi as error bars; report the score-gap ",
        "slope with its CI and the near-zero IC-debt slope next to it.")
