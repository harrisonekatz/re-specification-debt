#!/usr/bin/env Rscript
# =====================================================================
# tracking_pairwise_inference.R
#
# Paired within-series inference for the tracking-signal sweep, using the
# same machinery as series_level_inference.R so the numbers sit directly
# alongside Table 3: per-origin losses paired on (series, round), collapsed
# to one mean difference per series, then a cross-series t, a sign test, a
# Wilcoxon, a series-clustered SE, and a series-cluster bootstrap CI.
#
# Runs every pair below at every horizon in one pass.
#   diff := loss(policy_a) - loss(policy_b);  NEGATIVE favors policy_a.
#
# Reads whatever the run stored: records.csv, parts/, or by_policy/.
# Run from the PROJECT ROOT.
#
#   Rscript scripts/tracking_pairwise_inference.R
#   Rscript scripts/tracking_pairwise_inference.R --boot 2000 --out outputs/x.csv
# =====================================================================

suppressWarnings(suppressMessages(library(data.table)))

## ----------------------------- CONFIG -------------------------------
run_dirs_all <- list(
  "3"  = "outputs/m4_tracking_signal_h03",
  "6"  = "outputs/m4_tracking_signal_h06",
  "9"  = "outputs/m4_tracking_signal_h09",
  "12" = "outputs/m4_tracking_signal_h12",
  "18" = "outputs/m4_tracking_signal_h18"
)

# Each row: policy_a vs policy_b. Negative mean favors policy_a.
pairs_all <- list(
  # the simple monitor against its matched cadence
  list(a = "trigg_cap8_tau0.6",    b = "fixed_f8"),
  list(a = "trigg_cap8_tau0.5",    b = "fixed_f8"),
  list(a = "brown_cap8_tau0.5",    b = "fixed_f8"),
  # the paper's trigger against its matched cadence (replicates Table 3)
  list(a = "adaptive_cap8_tau0.8", b = "fixed_f8"),
  # the complexity question: one parameter against fifteen candidates
  list(a = "trigg_cap8_tau0.6",    b = "adaptive_cap8_tau0.8"),
  list(a = "trigg_cap8_tau0.5",    b = "adaptive_cap8_tau0.8"),
  list(a = "brown_cap8_tau0.5",    b = "adaptive_cap8_tau0.8")
)

col_policy <- "policy"
col_series <- "series_id"
col_round  <- "origin_number"
col_loss   <- "loss"

tol_diverge <- 1e-8
## --------------------------------------------------------------------

args <- commandArgs(trailingOnly = TRUE)
getf <- function(flag, default) {
  i <- which(args == paste0("--", flag))
  if (length(i) && length(args) > i[1]) args[i[1] + 1L] else default
}
n_boot  <- as.integer(getf("boot", "2000"))
out_csv <- getf("out", "outputs/tracking_pairwise_inference.csv")
seed    <- as.integer(getf("seed", "123"))
set.seed(seed)

policy_file_map <- function(dir) {
  fs <- list.files(dir, pattern = "\\.csv$", full.names = TRUE)
  nm <- vapply(fs, function(f) as.character(fread(f, select = col_policy, nrows = 1L)[[1L]]),
               character(1))
  stats::setNames(fs, nm)
}

# Read one run once, keeping only the columns and policies we need.
read_run <- function(run_dir, policies) {
  bp <- file.path(run_dir, "by_policy")
  rc <- file.path(run_dir, "records.csv")
  pt <- file.path(run_dir, "parts")
  cols <- c(col_policy, col_series, col_round, col_loss)
  if (file.exists(rc)) {
    d <- fread(rc, select = cols)
    d <- d[get(col_policy) %chin% policies]
  } else if (dir.exists(pt)) {
    fs <- list.files(pt, pattern = "\\.csv$", full.names = TRUE)
    d <- rbindlist(lapply(fs, function(f)
      fread(f, select = cols)[get(col_policy) %chin% policies]))
  } else if (dir.exists(bp)) {
    map <- policy_file_map(bp)
    have <- intersect(policies, names(map))
    d <- rbindlist(lapply(have, function(p) {
      x <- fread(map[[p]], select = c(col_series, col_round, col_loss))
      x[[col_policy]] <- p
      x
    }), fill = TRUE)
  } else stop("No records.csv, parts/, or by_policy/ under ", run_dir)
  setnames(d, c(col_policy, col_series, col_round, col_loss),
           c("policy", "series", "round", "loss"))
  d[, .(policy, series, round, loss)]
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

run_block <- function(sub, horizon, pa, pb, label) {
  sm <- sub[, .(s = mean(diff)), by = series]; s <- sm$s; G <- length(s)
  tt <- t.test(s)
  pos <- sum(s > 0); neg <- sum(s < 0); nz <- pos + neg
  bt <- if (nz > 0) binom.test(pos, nz, 0.5) else NULL
  wt <- suppressWarnings(wilcox.test(s))
  cr <- cluster_robust_mean(sub$diff, sub$series)
  ci <- cluster_boot_ci(sub$diff, sub$series, n_boot)
  data.table(
    horizon = as.integer(horizon), policy_a = pa, policy_b = pb, set = label,
    n_series = G, n_origin_records = nrow(sub), n_diff_series = nz,
    mean_series_diff = mean(s), median_series_diff = median(s),
    series_t = unname(tt$statistic), series_p = tt$p.value,
    share_a_worse = if (!is.null(bt)) pos / nz else NA_real_,
    sign_p = if (!is.null(bt)) bt$p.value else NA_real_,
    wilcoxon_p = wt$p.value,
    cr_mean = cr$mean, cr_se = cr$se, cr_t = cr$t,
    boot_lo = ci[1], boot_hi = ci[2])
}

analyse_pair <- function(dat, horizon, pa, pb) {
  a <- dat[policy == pa, .(series, round, loss_a = loss)]
  b <- dat[policy == pb, .(series, round, loss_b = loss)]
  if (nrow(a) == 0L || nrow(b) == 0L) {
    message("    skip (policy absent): ", pa, " vs ", pb); return(NULL)
  }
  m <- merge(a, b, by = c("series", "round"))
  if (nrow(m) == 0L) { message("    skip (no paired origins)"); return(NULL) }
  m[, diff := loss_a - loss_b]
  div <- m[abs(diff) > tol_diverge]
  rbindlist(list(
    run_block(m, horizon, pa, pb, "all_origins"),
    if (nrow(div) > 0) run_block(div, horizon, pa, pb, "divergent_origins")
  ), fill = TRUE)
}

run_dirs <- Filter(dir.exists, run_dirs_all)
skipped  <- setdiff(names(run_dirs_all), names(run_dirs))
if (length(skipped)) message("Skipping missing horizon dirs: ", paste(skipped, collapse = ", "))
if (length(run_dirs) == 0L) stop("No run directories found. Run from the project root.")

needed <- unique(unlist(lapply(pairs_all, function(p) c(p$a, p$b))))
res <- list()
for (h in names(run_dirs)) {
  message("horizon ", h, "  (", run_dirs[[h]], ")")
  dat <- read_run(run_dirs[[h]], needed)
  have <- unique(dat$policy)
  for (p in pairs_all) {
    if (!all(c(p$a, p$b) %chin% have)) next
    message("  ", p$a, "  vs  ", p$b)
    res[[length(res) + 1L]] <- analyse_pair(dat, h, p$a, p$b)
  }
  rm(dat); invisible(gc())
}

results <- rbindlist(Filter(Negate(is.null), res), fill = TRUE)
if (nrow(results) == 0L) stop("No comparisons produced.")
setorder(results, policy_a, policy_b, horizon, set)
fwrite(results, out_csv)

cat("\n==== PAIRED WITHIN-SERIES RESULTS (all_origins) ====\n")
cat("negative mean favors policy_a; CI is a series-cluster bootstrap\n\n")
show <- results[set == "all_origins"]
for (i in seq_len(nrow(show))) {
  r <- show[i]
  cat(sprintf("h%-2s  %-22s vs %-22s  mean %+.5f  t %+6.2f  CI [%+.5f, %+.5f]  a_worse %.1f%%  sign_p %.3f\n",
              r$horizon, r$policy_a, r$policy_b, r$mean_series_diff, r$series_t,
              r$boot_lo, r$boot_hi, 100 * r$share_a_worse, r$sign_p))
}
cat("\nwrote ", out_csv, "\n", sep = "")
