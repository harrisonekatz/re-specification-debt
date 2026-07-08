#!/usr/bin/env Rscript
# =====================================================================
# verify_paper_numbers.R
#
# Re-derives every number printed in the paper from the persisted output
# files and prints PASS or FAIL per claim. Does not rerun experiments.
# Run from the project root. A missing file skips its block with a note.
# =====================================================================

suppressWarnings(suppressMessages(library(data.table)))

# Self-locate the project root: if outputs/ is not in the working directory
# but sits one level up (run from scripts/), move up before reading paths.
if (!dir.exists("outputs") && dir.exists("../outputs")) setwd("..")
cat("working directory:", getwd(), "\n\n")

## ----------------------------- CONFIG -------------------------------
p_summary   <- "outputs/m4_full_capped/summary.csv"
p_fallback  <- "outputs/m4_full_capped/fallback_share.csv"
p_paired    <- "outputs/m4_full_capped/paired_diagnostics.csv"
p_trig      <- "outputs/m4_full_capped/triggered_subset.csv"
p_bridge    <- "outputs/m4_full_capped/spec_debt_bridge_summary.csv"
p_horizon   <- "outputs/horizon_sweep.csv"
p_series    <- "outputs/series_level_inference.csv"
p_bins      <- "outputs/bridge_bins_ci.csv"
p_reg       <- "outputs/bridge_regression.csv"
p_heldform  <- "outputs/m4_drift_experiment/heldform_debt_divergence.csv"
p_shockwin  <- "outputs/m4_shock_experiment/shock_winner_by_cell.csv"
p_shockmat  <- "outputs/m4_shock_experiment/shock_matched_comparison.csv"
p_timing    <- "outputs/m4_timing_isolation/timing_matched_by_jitter.csv"
## --------------------------------------------------------------------

n_pass <- 0L; n_fail <- 0L; n_skip <- 0L
check <- function(label, got, want, tol = 5e-5) {
  ok <- is.finite(got) && is.finite(want) && abs(got - want) <= tol
  cat(sprintf("%-4s %-72s got %-12.6g want %g\n",
              ifelse(ok, "PASS", "FAIL"), label, got, want))
  if (ok) n_pass <<- n_pass + 1L else n_fail <<- n_fail + 1L
  invisible(ok)
}
check_true <- function(label, cond) {
  cat(sprintf("%-4s %s\n", ifelse(isTRUE(cond), "PASS", "FAIL"), label))
  if (isTRUE(cond)) n_pass <<- n_pass + 1L else n_fail <<- n_fail + 1L
}
have <- function(path, what) {
  if (file.exists(path)) return(TRUE)
  cat(sprintf("SKIP %s (missing %s)\n", what, path)); n_skip <<- n_skip + 1L
  FALSE
}

## ---- Table 2 and 8.3 (summary.csv) ----------------------------------
if (have(p_summary, "Table 2 / 8.3")) {
  s <- fread(p_summary)
  tab2 <- list(  # policy = c(rel loss 4dp, rel time 3dp, rel instab 3dp, respecs 2dp)
    full_update            = c(1.0000, 1.000, 1.000, 36.00),
    fixed_f4               = c(1.0000, 0.282, 0.947,  9.00),
    fixed_f6               = c(1.0004, 0.199, 0.942,  6.00),
    fixed_f9               = c(1.0005, 0.140, 0.933,  4.00),
    fixed_f8               = c(1.0006, 0.167, 0.941,  5.00),
    `adaptive_cap12_tau0.8`= c(1.0021, 0.204, 0.930,  3.22),
    `adaptive_cap8_tau0.8` = c(1.0022, 0.283, 0.937,  5.10),
    fixed_f12              = c(1.0022, 0.114, 0.938,  3.00),
    parameter_only         = c(1.0087, 0.060, 0.921,  1.00))
  for (pol in names(tab2)) {
    r <- s[policy == pol]; w <- tab2[[pol]]
    check(paste("Table 2", pol, "rel loss"),  round(r$relative_loss, 4), w[1], 1e-9)
    check(paste("Table 2", pol, "rel time"),  round(r$relative_time, 3), w[2], 1e-9)
    check(paste("Table 2", pol, "rel instab"),round(r$relative_instability, 3), w[3], 1e-9)
    check(paste("Table 2", pol, "respecs"),   round(r$mean_respecifications_per_series, 2), w[4], 1e-9)
  }
  check("8.1 series count", unique(s$n_series)[1], 47982, 0)
  check("8.1 records per policy", unique(s$n_records)[1], 1727352, 0)
  check("8.3 parameter_only gap (0.87 pct)",
        round(100 * (s[policy == "parameter_only", relative_loss] - 1), 2), 0.87, 1e-9)
  check("8.3 worst policy gap (3.4 pct)",
        round(100 * (max(s$relative_loss) - 1), 1), 3.4, 1e-9)
  fx <- s[grepl("^fixed_f", policy), relative_loss]
  check_true("8.3 fixed cadences within a quarter percent (max <= 1.0025)",
             max(fx) <= 1.0025)
  check("8.3 fixed_f8 compute share (16.7 pct)",
        round(100 * s[policy == "fixed_f8", relative_time], 1), 16.7, 1e-9)
}

## ---- 8.1 fallback disclosure ----------------------------------------
if (have(p_fallback, "8.1 fallback disclosure")) {
  f <- fread(p_fallback)
  check("8.1 min fallbacks (216)", min(f$snaive_fallbacks), 216, 0)
  check("8.1 max fallbacks (276)", max(f$snaive_fallbacks), 276, 0)
  check_true("8.1 fallback share at most 0.016 pct",
             max(f$snaive_fallbacks) / 1727352 <= 0.00016)
}

## ---- 8.4 benchmark paragraph ----------------------------------------
if (have(p_paired, "8.4 agreement split")) {
  pd <- fread(p_paired)[policy_a == "adaptive_cap8_tau0.8" & policy_b == "fixed_f8"]
  check("8.4 divergent share (8.9 pct)",
        round(100 * pd$n_divergent / pd$n_paired, 1), 8.9, 1e-9)
}
if (have(p_trig, "8.4 fired-origin MASE")) {
  tr <- fread(p_trig)
  st <- tr[subset == "score_trigger_records"]
  check("8.4 fired adaptive MASE (0.714)", round(st$mean_adaptive_loss, 3), 0.714, 1e-9)
  check("8.4 fired fixed MASE (0.687)",    round(st$mean_fixed_loss, 3),    0.687, 1e-9)
}
if (have(p_series, "8.4 + Table 3 series-level inference")) {
  sl <- fread(p_series)
  sl[, horizon := as.character(horizon)]
  a3 <- sl[horizon == "3" & set == "all_origins"]
  d3 <- sl[horizon == "3" & set == "divergent_origins"]
  check("8.4 h3 per-series mean (+0.0010)", round(a3$mean_series_diff, 4), 0.0010, 1e-9)
  check("8.4 h3 series t (4.9)",            round(a3$series_t, 1),         4.9,    1e-9)
  check("8.4 h3 boot lo (+0.0006)",         round(a3$boot_lo, 4),          0.0006, 1e-9)
  check("8.4 h3 boot hi (+0.0014)",         round(a3$boot_hi, 4),          0.0014, 1e-9)
  check("8.4 divergent series (12,459)",    d3$n_series,                   12459,  0)
  check("8.4 share favoring cadence (54.8 pct)",
        round(100 * d3$share_series_a_worse, 1), 54.8, 1e-9)
  t3 <- list(`6`  = c(-0.0057, -3.81, -0.0086, -0.0028, 52.3),
             `9`  = c(-0.0066, -4.50, -0.0094, -0.0037, 50.6),
             `12` = c(-0.0051, -3.49, -0.0080, -0.0024, 49.7),
             `18` = c(-0.0038, -2.33, -0.0069, -0.0007, 49.2))
  for (h in names(t3)) {
    r <- sl[horizon == h & set == "all_origins"]
    d <- sl[horizon == h & set == "divergent_origins"]
    w <- t3[[h]]
    check(paste("Table 3 h", h, "mean gap"), round(r$mean_series_diff, 4), w[1], 1e-9)
    check(paste("Table 3 h", h, "series t"), round(r$series_t, 2),         w[2], 1e-9)
    check(paste("Table 3 h", h, "CI lo"),    round(r$boot_lo, 4),          w[3], 1e-4)
    check(paste("Table 3 h", h, "CI hi"),    round(r$boot_hi, 4),          w[4], 1e-4)
    check(paste("Table 3 h", h, "trigger worse pct"),
          round(100 * d$share_series_a_worse, 1), w[5], 1e-9)
  }
}

## ---- 8.4 horizon sweep prose and Figure 2 ---------------------------
if (have(p_horizon, "8.4 horizon sweep")) {
  hs <- fread(p_horizon)
  po <- c(`3` = 1.015, `6` = 1.015, `9` = 1.016, `12` = 1.013, `18` = 1.019)
  mg <- c(`3` = 0.0002, `6` = -0.0057, `9` = -0.0066, `12` = -0.0051, `18` = -0.0038)
  for (h in names(po)) {
    r <- hs[horizon == as.integer(h)]
    check(paste("8.4 parameter-only rl h", h), round(r$parameter_only_rl, 3), po[[h]], 1e-9)
    check(paste("8.4 matched gap h", h),       round(r$matched_mean_gap, 4),  mg[[h]], 1e-9)
  }
}

## ---- Table 4 first block --------------------------------------------
if (have(p_bridge, "Table 4 Spearman block")) {
  b <- fread(p_bridge)
  m <- b[policy == "adaptive_cap8_tau0.8"]
  check("Table 4 Spearman AICc (0.008)", round(m$spearman_score_gap_debt_aicc, 3), 0.008, 1e-9)
  check("Table 4 Spearman BIC (0.075)",  round(m$spearman_score_gap_debt_bic, 3),  0.075, 1e-9)
  allsp <- c(b$spearman_score_gap_debt_aicc, b$spearman_score_gap_debt_bic)
  check_true("Table 4 range 0.003 to 0.16 covers all adaptive policies",
             min(allsp) >= 0.003 - 5e-4 && max(allsp) <= 0.16 + 5e-3)
}

## ---- Table 4 second block: frozen-form drift cells ------------------
if (have(p_heldform, "Table 4 frozen-form block")) {
  hf <- fread(p_heldform)
  dr <- hf[drift_mode %in% c("irregular", "regular")]
  check("Table 4 drift-cell mean corr(score gap, loss) (0.34)",
        round(mean(dr$scoregap_vs_loss), 2), 0.34, 1e-9)
  check("Table 4 drift-cell mean corr(IC debt, loss) (0.03)",
        round(mean(dr$icdebt_vs_loss), 2), 0.03, 1e-9)
  check_true("8.5 IC debt changes sign with noise",
             any(hf$icdebt_vs_loss > 0) && any(hf$icdebt_vs_loss < 0))
  check_true("8.5 score gap sign-stable (all cells positive)",
             all(hf$scoregap_vs_loss > 0))
}

## ---- Figure 3 and the 8.5 slope sentence ----------------------------
if (have(p_bins, "Figure 3 bins")) {
  bn <- fread(p_bins)
  sg <- bn[signal == "score_gap"][order(bin)]
  ic <- bn[signal == "ic"][order(bin)]
  sg_want <- c(0.0569, 0.0387, 0.0258, 0.0224, -0.0128)
  ic_want <- c(0.0032, -0.0066, 0.0485, 0.0161, 0.0697)
  for (i in seq_len(5)) {
    check(paste("Fig 3 score-gap bin", i, "mean"), round(sg$mean_loss_diff[i], 4), sg_want[i], 1e-4)
    check(paste("Fig 3 IC-debt bin", i, "mean"),   round(ic$mean_loss_diff[i], 4), ic_want[i], 1e-4)
  }
  check_true("Fig 3 score-gap bins monotone decreasing",
             all(diff(sg$mean_loss_diff) < 0))
  check_true("Fig 3 only largest-gap bin CI reaches zero or below",
             sg$ci_lo[5] <= 0 && all(sg$ci_lo[1:3] > 0))
}
if (have(p_reg, "8.5 slope sentence")) {
  rg <- fread(p_reg)
  check_true("8.5 IC-debt slope positive with CI above zero (wrong-signed)",
             rg[signal == "ic", slope] > 0 && rg[signal == "ic", ci_lo] > 0)
}

## ---- 8.3 regime sentence (simulated) --------------------------------
if (have(p_shockwin, "8.3 regime cell winners")) {
  w <- fread(p_shockwin)
  check_true("8.3 level shifts won by full_update (both severities)",
             all(w[shock_type == "level_shift", winner] == "full_update"))
  check_true("8.3 transient burst won by parameter_only",
             w[shock_type == "transient_burst", winner] == "parameter_only")
  check_true("8.3 outlier cells won by pure adaptive (rare-firing) policies",
             all(grepl("^adaptive_tau", w[shock_type == "additive_outlier", winner])))
}


## ---- 8.1 absolute anchor ---------------------------------------------
if (file.exists(p_summary)) {
  s2 <- fread(p_summary)
  check("8.1 full_update absolute mean MASE (0.639)",
        round(s2[policy == "full_update", mean_loss], 3), 0.639, 1e-9)
}

## ---- Table 3 new columns: n_diff and sign p --------------------------
if (file.exists(p_series)) {
  sl2 <- fread(p_series); sl2[, horizon := as.character(horizon)]
  nd <- c(`6` = 1485, `9` = 1489, `12` = 1521, `18` = 1511)
  sp <- c(`6` = 0.09, `9` = 0.68, `12` = 0.84, `18` = 0.57)
  for (h in names(nd)) {
    d <- sl2[horizon == h & set == "divergent_origins"]
    check(paste("Table 3 h", h, "n_diff"), d$n_series, nd[[h]], 0)
    check(paste("Table 3 h", h, "sign p"), round(d$sign_p, 2), sp[[h]], 1e-9)
  }
}

## ---- Table 5: cost-winner grid ---------------------------------------
if (file.exists(p_summary)) {
  s3 <- fread(p_summary)
  want <- list(
    "0"    = c("fixed_f4", "fixed_f4", "fixed_f9", "fixed_f9"),
    "0.01" = c("fixed_f9", "fixed_f9", "fixed_f9", "fixed_f9"),
    "0.05" = c("fixed_f9", "fixed_f9", "fixed_f9", "fixed_f9"),
    "0.1"  = c("fixed_f12", "fixed_f12", "fixed_f12", "parameter_only"),
    "0.25" = rep("parameter_only", 4),
    "0.5"  = rep("parameter_only", 4))
  gams <- c(0, 0.01, 0.05, 0.10)
  for (a in names(want)) for (j in seq_along(gams)) {
    al <- as.numeric(a); g <- gams[j]
    win <- s3[which.min(relative_loss + al * relative_time + g * relative_instability), policy]
    check_true(sprintf("Table 5 winner alpha=%s gamma=%s is %s", a, g, want[[a]][j]),
               identical(win, want[[a]][j]))
  }
}

## ---- Appendix C.1: shock matched cells (cap8 vs fixed_f8) ------------
if (have(p_shockmat, "Appendix C.1 matched cells")) {
  sm <- fread(p_shockmat)[pair == "adaptive_cap8_tau0.8 vs fixed_f8"]
  cells <- list(
    c("control", "none",          0.0000,  -0.1),
    c("additive_outlier", "low",  -0.0646, -9.1),
    c("additive_outlier", "high", -0.0489, -8.9),
    c("level_shift", "small",      0.0624,  22.4),
    c("level_shift", "large",      0.0109,  10.5),
    c("transient_burst", "med",    0.0029,   2.4),
    c("variance_shift", "high",   -0.0001,  -1.0),
    c("heavy_tail", "df3",        -0.0023,  -2.3))
  for (cl in cells) {
    r <- sm[shock_type == cl[1] & severity == cl[2]]
    check(paste("C.1 matched", cl[1], cl[2], "mean gap"),
          r$mean_gap, as.numeric(cl[3]), 6e-5)
    check(paste("C.1 matched", cl[1], cl[2], "t"),
          r$t, as.numeric(cl[4]), 0.06)
    check_true(paste("C.1 matched", cl[1], cl[2], "n = 600"), r$n == 600L)
  }
}

## ---- Appendix C.3: held-form six-cell table --------------------------
if (file.exists(p_heldform)) {
  hf2 <- fread(p_heldform)[order(drift_mode, noise)]
  sg_want <- c(0.595, 0.342, 0.205, 0.227, 0.044, 0.408)
  ic_want <- c(0.253, -0.085, -0.077, 0.020, 0.022, -0.354)
  lab <- paste(hf2$drift_mode, hf2$noise)
  for (i in seq_len(6)) {
    check(paste("C.3", lab[i], "score gap vs loss"), hf2$scoregap_vs_loss[i], sg_want[i], 1e-9)
    check(paste("C.3", lab[i], "IC debt vs loss"),   hf2$icdebt_vs_loss[i],   ic_want[i], 1e-9)
  }
  check_true("C.3 monitored origins per cell = 7,200", all(hf2$n == 7200L))
}

## ---- Appendix C.2: timing isolation ----------------------------------
if (have(p_timing, "Appendix C.2 timing isolation")) {
  tm <- fread(p_timing)
  cells <- list(
    c(-1,  0.0000,  -1.00),
    c( 0,  0.08489, 21.08),
    c( 4,  0.00166,  0.33),
    c( 8, -0.03042, -4.70),
    c(12, -0.12450, -11.08))
  for (cl in cells) {
    r <- tm[jitter_num == as.integer(cl[1])]
    check(paste("C.2 jitter", cl[1], "mean gap"), r$mean_gap, cl[2], 1e-5)
    check(paste("C.2 jitter", cl[1], "t"),        r$t,        cl[3], 0.011)
    check_true(paste("C.2 jitter", cl[1], "n = 600"), r$n == 600L)
  }
  check_true("C.2 gap nonnegative under regular switching (J0)",
             tm[jitter_num == 0L, mean_gap] >= 0)
  check_true("C.2 gap negative at full irregularity (J12)",
             tm[jitter_num == 12L, mean_gap] < 0)
  check_true("C.2 mean gap monotone decreasing in jitter",
             all(diff(tm[jitter_num >= 0L][order(jitter_num)]$mean_gap) < 0))
}

cat(sprintf("\n==== %d PASS, %d FAIL, %d skipped blocks ====\n", n_pass, n_fail, n_skip))
if (n_fail > 0L) quit(status = 1L)
