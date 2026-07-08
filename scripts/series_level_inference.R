#!/usr/bin/env Rscript
# =====================================================================
# series_level_inference.R   (v2, matches your real schema)
#
# Replaces the origin-level paired t and sign tests in Section 8.4 with
# inference that respects within-series dependence. For a policy pair at
# each run it reports, over ALL origins and over DIVERGENT origins:
#   (1) series-level paired t on per-series mean loss differences  [primary]
#   (2) sign test on the sign of each series' mean difference
#   (3) Wilcoxon signed-rank on per-series means
#   (4) cluster-robust SE of the origin-level mean, clustered by series
#   (5) a cluster (series) bootstrap 95% CI
#
# Sanity check: the all_origins cr_mean should land near +0.00102 for
# cap8_tau0.8 vs fixed_f8 (the all-rounds mean in paired_diagnostics.csv),
# and cr_se should be far larger than the 0.000122 paired SE reported there.
# That gap is the whole point: honest SEs, smaller |t|.
#
# Reads the origin records however a run stored them: by_policy/ (one file
# per policy), a single records.csv, or parts/. Run from the PROJECT ROOT.
# =====================================================================

suppressWarnings(suppressMessages(library(data.table)))

## ----------------------------- CONFIG -------------------------------
# horizon label -> run directory (not a file). Missing dirs are skipped,
# so this runs on m4_full_capped now; add horizon dirs once confirmed.
run_dirs_all <- list(
  "3"  = "outputs/m4_full_capped",
  "6"  = "outputs/m4_horizon_h06",
  "9"  = "outputs/m4_horizon_h09",
  "12" = "outputs/m4_horizon_h12",
  "18" = "outputs/m4_horizon_h18"
)

policy_a <- "adaptive_cap8_tau0.8"   # the trigger
policy_b <- "fixed_f8"               # its matched cadence
# diff := loss(policy_a) - loss(policy_b); negative favors the trigger.

col_policy <- "policy"
col_series <- "series_id"
col_round  <- "origin_number"        # 0..35 within each series; aligns across policies
col_loss   <- "loss"

n_boot      <- 2000L
tol_diverge <- 1e-8
seed        <- 123L
out_csv     <- "outputs/series_level_inference.csv"
## --------------------------------------------------------------------
set.seed(seed)

# map policy name -> file in a by_policy/ directory by peeking at row 1
policy_file_map <- function(dir) {
  fs <- list.files(dir, pattern = "\\.csv$", full.names = TRUE)
  nm <- vapply(fs, function(f) as.character(fread(f, select = col_policy, nrows = 1L)[[1L]]),
               character(1))
  stats::setNames(fs, nm)
}

# read one policy's (series, round, loss) from whatever the run stored
read_one_policy <- function(run_dir, pol) {
  bp <- file.path(run_dir, "by_policy")
  rc <- file.path(run_dir, "records.csv")
  pt <- file.path(run_dir, "parts")
  if (dir.exists(bp)) {
    map <- policy_file_map(bp)
    if (!pol %in% names(map))
      stop(sprintf("Policy '%s' not in %s.\nAvailable: %s", pol, bp,
                   paste(sort(names(map)), collapse = ", ")))
    message("  ", pol, " <- ", basename(map[[pol]]))
    d <- fread(map[[pol]], select = c(col_series, col_round, col_loss))
  } else if (file.exists(rc)) {
    d <- fread(rc, select = c(col_policy, col_series, col_round, col_loss))
    d <- d[get(col_policy) == pol]
  } else if (dir.exists(pt)) {
    parts <- list.files(pt, pattern = "\\.csv$", full.names = TRUE)
    d <- rbindlist(lapply(parts, function(f)
      fread(f, select = c(col_policy, col_series, col_round, col_loss))[get(col_policy) == pol]))
  } else stop("No by_policy/, records.csv, or parts/ under ", run_dir)
  setnames(d, c(col_series, col_round, col_loss), c("series", "round", "loss"))
  d[, .(series, round, loss)]
}

cluster_robust_mean <- function(d, g) {
  dbar <- mean(d); Sg <- tapply(d - dbar, g, sum)
  N <- length(d); G <- length(Sg)
  se <- sqrt((G / (G - 1)) * sum(Sg^2) / (N^2))
  list(mean = dbar, se = se, t = dbar / se, df = G - 1, G = G, N = N)
}

cluster_boot_ci <- function(d, g, B, probs = c(0.025, 0.975)) {
  idx <- split(seq_along(d), g); cl <- names(idx); G <- length(cl)
  stat <- numeric(B)
  for (b in seq_len(B))
    stat[b] <- mean(d[unlist(idx[sample(cl, G, replace = TRUE)], use.names = FALSE)])
  quantile(stat, probs = probs, names = FALSE)
}

run_block <- function(sub, horizon, label) {
  sm <- sub[, .(s = mean(diff)), by = series]; s <- sm$s; G <- length(s)
  tt <- t.test(s)
  pos <- sum(s > 0); neg <- sum(s < 0); nz <- pos + neg
  bt <- if (nz > 0) binom.test(pos, nz, 0.5) else NULL
  wt <- suppressWarnings(wilcox.test(s))
  cr <- cluster_robust_mean(sub$diff, sub$series)
  ci <- cluster_boot_ci(sub$diff, sub$series, n_boot)
  data.table(
    horizon = horizon, set = label, n_series = G, n_origin_records = nrow(sub),
    mean_series_diff = mean(s), median_series_diff = median(s),
    share_series_a_worse = pos / G,
    series_t = unname(tt$statistic), series_df = unname(tt$parameter), series_p = tt$p.value,
    sign_share_a_worse = if (!is.null(bt)) pos / nz else NA_real_,
    sign_p = if (!is.null(bt)) bt$p.value else NA_real_,
    wilcoxon_p = wt$p.value,
    cr_mean = cr$mean, cr_se = cr$se, cr_t = cr$t, cr_df = cr$df,
    boot_lo = ci[1], boot_hi = ci[2])
}

analyse_run <- function(run_dir, horizon) {
  message("horizon ", horizon, "  (", run_dir, ")")
  a <- read_one_policy(run_dir, policy_a); setnames(a, "loss", "loss_a")
  b <- read_one_policy(run_dir, policy_b); setnames(b, "loss", "loss_b")
  m <- merge(a, b, by = c("series", "round"))
  m[, diff := loss_a - loss_b]
  div <- m[abs(diff) > tol_diverge]
  rbindlist(list(run_block(m, horizon, "all_origins"),
                 if (nrow(div) > 0) run_block(div, horizon, "divergent_origins")))
}

run_dirs <- Filter(dir.exists, run_dirs_all)
skipped  <- setdiff(names(run_dirs_all), names(run_dirs))
if (length(skipped)) message("Skipping missing horizon dirs: ", paste(skipped, collapse = ", "))
if (length(run_dirs) == 0L) stop("None of the run directories exist. Run from the project root.")

results <- rbindlist(lapply(names(run_dirs), function(h) analyse_run(run_dirs[[h]], h)))
print(results); fwrite(results, out_csv)
message("\nWrote ", out_csv,
        "\nReport series_t / series_df / series_p (and sign_p, boot CI) in place ",
        "of the origin-level t and z. all_origins is the policy comparison; ",
        "divergent_origins is the mechanism.")
