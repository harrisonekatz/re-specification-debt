#!/usr/bin/env Rscript
# ---------------------------------------------------------------------------
# review_response_analysis.R  (run after any results land; reruns safely)
#
# One command that computes every review item extractable from existing and
# newly produced outputs, skipping anything not yet on disk:
#
#   1. Search-event decomposition per policy per run (review 5.5): initial,
#      monitor/score fires, cap-driven, plus searches that actually changed
#      the deployed form versus returned the same one.
#   2. Paired within-series inference for the review's required contrasts,
#      with sides allowed to come from DIFFERENT run directories (valid
#      because fits are deterministic given series and origin):
#        Trigg vs fixed_f4          rate-matched monitor vs clock (3.2)
#        scoregate_aicc vs Trigg    selector-deconfounded gate vs monitor (3.1)
#        scoregate_aicc vs fixed_f8 deconfounded gate vs its cap-matched clock
#        h18 full-scale trio        when outputs/m4_tracking_signal_h18_full lands
#   3. Robustness of each mean gap (review 6.4): trimmed means, winsorized
#      mean, quantiles, and the mean recomputed without the top 1 percent of
#      series by absolute difference.
#   4. Subsample overlap (review 5.2): pinned ids vs the original horizon
#      subsample when its records are present.
#
#   Rscript scripts/review_response_analysis.R
# Writes outputs/review_decomposition.csv and outputs/review_paired.csv.
# ---------------------------------------------------------------------------

suppressWarnings(suppressMessages(library(data.table)))
set.seed(123)
N_BOOT <- 2000L

# ---- where things live (edit if your layout differs) -----------------------
tracking_h <- function(H) sprintf("outputs/m4_tracking_signal_h%02d", H)
final_h    <- function(H) sprintf("outputs/m4_final_h%02d", H)
FULL_TRACKING <- "outputs/m4_tracking_signal"        # 47,982-series benchmark, h=3
FULL_ORIGINAL <- "outputs/m4_full_capped"            # original full run (fixed_f4 lives here)
H18_FULL      <- "outputs/m4_tracking_signal_h18_full"
PIN_IDS       <- "outputs/horizon_subsample_ids.txt"
ORIG_SUBSAMPLE_DIRS <- c("outputs/m4_horizon_h18", "outputs/m4_horizon_h06")

# ---- readers ---------------------------------------------------------------
read_policy <- function(run_dir, pol, cols = c("policy","series_id","origin_number","loss")) {
  rc <- file.path(run_dir, "records.csv"); pt <- file.path(run_dir, "parts")
  bp <- file.path(run_dir, "by_policy")
  if (file.exists(rc)) {
    d <- fread(rc, select = intersect(cols, names(fread(rc, nrows = 0L))))
    d <- d[policy == pol]
  } else if (dir.exists(pt)) {
    fs <- list.files(pt, pattern = "\\.csv$", full.names = TRUE)
    hdr <- names(fread(fs[[1]], nrows = 0L))
    d <- rbindlist(lapply(fs, function(f) fread(f, select = intersect(cols, hdr))[policy == pol]))
  } else if (dir.exists(bp)) {
    fs <- list.files(bp, pattern = "\\.csv$", full.names = TRUE)
    hit <- NULL
    for (f in fs) if (identical(as.character(fread(f, select="policy", nrows=1L)[[1]]), pol)) { hit <- f; break }
    if (is.null(hit)) return(NULL)
    hdr <- names(fread(hit, nrows = 0L))
    d <- fread(hit, select = intersect(cols, hdr))
    if (!"policy" %in% names(d)) d[, policy := pol]
  } else return(NULL)
  if (nrow(d) == 0L) return(NULL)
  d
}

# ---- decomposition (item 1) ------------------------------------------------
decompose_run <- function(run_dir) {
  cols <- c("policy","series_id","origin_number","respecified","trigger_reason","form")
  rc <- file.path(run_dir, "records.csv"); pt <- file.path(run_dir, "parts")
  d <- if (file.exists(rc)) {
    hdr <- names(fread(rc, nrows = 0L)); fread(rc, select = intersect(cols, hdr))
  } else if (dir.exists(pt)) {
    fs <- list.files(pt, pattern = "\\.csv$", full.names = TRUE)
    hdr <- names(fread(fs[[1]], nrows = 0L))
    rbindlist(lapply(fs, fread, select = intersect(cols, hdr)))
  } else return(NULL)
  if (!"respecified" %in% names(d)) return(NULL)
  setorder(d, policy, series_id, origin_number)
  d[, prev_form := shift(form), by = .(policy, series_id)]
  d[, changed := respecified == 1L & !is.na(prev_form) & form != prev_form]
  agg <- d[, .(
    n_series = uniqueN(series_id),
    searches_per_series = sum(respecified) / uniqueN(series_id),
    changes_per_series  = sum(changed) / uniqueN(series_id)
  ), by = policy]
  if ("trigger_reason" %in% names(d)) {
    tr <- dcast(d[respecified == 1L, .N, by = .(policy, trigger_reason)],
                policy ~ trigger_reason, value.var = "N", fill = 0L)
    ns <- agg[, .(policy, n_series)]
    tr <- merge(tr, ns, by = "policy")
    num <- setdiff(names(tr), c("policy","n_series"))
    tr[, (num) := lapply(.SD, function(x) round(x / n_series, 3)), .SDcols = num]
    tr[, n_series := NULL]
    agg <- merge(agg, tr, by = "policy", all.x = TRUE)
  }
  agg[, run := run_dir][]
}

# ---- inference machinery (identical to tracking_pairwise_inference) --------
cluster_boot_ci <- function(d, g, B = N_BOOT) {
  idx <- split(seq_along(d), g); cl <- names(idx); G <- length(cl)
  stat <- numeric(B)
  for (b in seq_len(B))
    stat[b] <- mean(d[unlist(idx[sample(cl, G, replace = TRUE)], use.names = FALSE)])
  quantile(stat, c(0.025, 0.975), names = FALSE)
}

pair_across <- function(dirA, polA, dirB, polB, label, horizon) {
  a <- read_policy(dirA, polA); b <- read_policy(dirB, polB)
  if (is.null(a) || is.null(b)) { message("  skip (missing): ", label); return(NULL) }
  m <- merge(a[, .(series_id, origin_number, la = loss)],
             b[, .(series_id, origin_number, lb = loss)],
             by = c("series_id", "origin_number"))
  if (nrow(m) == 0L) { message("  skip (no pairs): ", label); return(NULL) }
  m[, diff := la - lb]
  sm <- m[, .(s = mean(diff)), by = series_id]; s <- sm$s; G <- length(s)
  tt <- t.test(s); ci <- cluster_boot_ci(m$diff, m$series_id)
  pos <- sum(s > 0); nz <- sum(s != 0)
  # robustness (review 6.4)
  tr1 <- mean(s, trim = 0.01); tr25 <- mean(s, trim = 0.025); tr5 <- mean(s, trim = 0.05)
  w <- s; cap <- quantile(abs(s), 0.99); w[w >  cap] <- cap; w[w < -cap] <- -cap
  k <- max(1L, ceiling(0.01 * G)); topidx <- order(-abs(s))[seq_len(k)]
  data.table(comparison = label, horizon = horizon, n_series = G,
             mean = mean(s), t = unname(tt$statistic),
             ci_lo = ci[1], ci_hi = ci[2],
             share_a_worse = if (nz > 0) pos / nz else NA_real_,
             median = median(s), q10 = quantile(s, 0.10), q90 = quantile(s, 0.90),
             trim1 = tr1, trim2.5 = tr25, trim5 = tr5, winsor1 = mean(w),
             mean_wo_top1pct = mean(s[-topidx]),
             a = paste0(polA, " @ ", dirA), b = paste0(polB, " @ ", dirB))
}

# ---- run everything --------------------------------------------------------
message("== 1. Search-event decomposition ==")
dec_dirs <- Filter(dir.exists, c(FULL_TRACKING, sapply(c(3,6,9,12,18), tracking_h),
                                 sapply(c(3,6,9,12,18), final_h), H18_FULL))
dec <- rbindlist(Filter(Negate(is.null), lapply(dec_dirs, decompose_run)), fill = TRUE)
if (nrow(dec)) { fwrite(dec, "outputs/review_decomposition.csv"); print(dec) }

message("\n== 2 & 3. Paired inference with robustness ==")
res <- list()
for (H in c(3, 6, 9, 12, 18)) {
  res[[length(res)+1]] <- pair_across(tracking_h(H), "trigg_cap8_tau0.6",
                                      final_h(H), "fixed_f4",
                                      "trigg vs fixed_f4 (rate-matched)", H)
  res[[length(res)+1]] <- pair_across(final_h(H), "scoregate_aicc_cap8_tau0.8",
                                      tracking_h(H), "trigg_cap8_tau0.6",
                                      "scoregate_aicc vs trigg (deconfounded)", H)
  res[[length(res)+1]] <- pair_across(final_h(H), "scoregate_aicc_cap8_tau0.8",
                                      tracking_h(H), "fixed_f8",
                                      "scoregate_aicc vs fixed_f8", H)
}
# full-scale h3 rate-matched, cross-run (no refitting needed)
res[[length(res)+1]] <- pair_across(FULL_TRACKING, "trigg_cap8_tau0.6",
                                    FULL_ORIGINAL, "fixed_f4",
                                    "FULL SCALE trigg vs fixed_f4", 3L)
# h18 full-scale trio, once that run lands
res[[length(res)+1]] <- pair_across(H18_FULL, "trigg_cap8_tau0.6", H18_FULL,
                                    "adaptive_cap8_tau0.8", "FULL h18 trigg vs trigger", 18L)
res[[length(res)+1]] <- pair_across(H18_FULL, "trigg_cap8_tau0.6", H18_FULL,
                                    "fixed_f8", "FULL h18 trigg vs fixed_f8", 18L)
res[[length(res)+1]] <- pair_across(H18_FULL, "adaptive_cap8_tau0.8", H18_FULL,
                                    "fixed_f8", "FULL h18 trigger vs fixed_f8", 18L)
paired <- rbindlist(Filter(Negate(is.null), res), fill = TRUE)
if (nrow(paired)) {
  fwrite(paired, "outputs/review_paired.csv")
  show <- copy(paired)
  num <- names(show)[sapply(show, is.numeric)]
  show[, (num) := lapply(.SD, function(x) round(x, 5)), .SDcols = num]
  print(show[, .(comparison, horizon, mean, t, ci_lo, ci_hi, trim1, mean_wo_top1pct)])
}

message("\n== 4. Subsample overlap ==")
if (file.exists(PIN_IDS)) {
  pin <- unique(trimws(readLines(PIN_IDS, warn = FALSE)))
  got <- FALSE
  for (d in ORIG_SUBSAMPLE_DIRS) {
    rc <- file.path(d, "records.csv"); bp <- file.path(d, "by_policy")
    src <- if (file.exists(rc)) rc else if (dir.exists(bp))
      list.files(bp, pattern = "\\.csv$", full.names = TRUE)[1] else NA
    if (is.na(src) || is.null(src)) next
    orig <- unique(fread(src, select = "series_id")$series_id)
    message(sprintf("pinned n=%d | original(%s) n=%d | OVERLAP = %d",
                    length(pin), d, length(orig), length(intersect(pin, as.character(orig)))))
    got <- TRUE; break
  }
  if (!got) message("original horizon subsample records not found; overlap not computed")
}
message("\nWrote outputs/review_decomposition.csv and outputs/review_paired.csv")
