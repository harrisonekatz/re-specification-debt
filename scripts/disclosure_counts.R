#!/usr/bin/env Rscript
# =====================================================================
# disclosure_counts.R   (v2, matches your schema; reviewer 3.7)
#
# Produces the numbers to disclose in 8.1. Part 1 audits an origin-record
# file (defaults to full_update, which re-selects every round and so exercises
# the most fitting): record and series counts, non-finite losses, and a tally
# of the fitted forms so any fallback form shows up. Part 2 counts zero or
# near-zero seasonal-naive MASE denominators from the raw M4 monthly file.
# Run from the PROJECT ROOT.
# =====================================================================

suppressWarnings(suppressMessages(library(data.table)))

## ----------------------------- CONFIG -------------------------------
run_dir      <- "outputs/m4_full_capped"
audit_policy <- "full_update"
m4_train_csv <- "data/Monthly-train.csv"

col_policy <- "policy"
col_series <- "series_id"
col_loss   <- "loss"
col_form   <- "form"
col_respec <- "respecified"
col_reason <- "trigger_reason"

season    <- 12L
train_len <- 36L
near_zero <- 1e-8
## --------------------------------------------------------------------

# ---- Part 1: origin-record audit -----------------------------------
bp <- file.path(run_dir, "by_policy")
if (dir.exists(bp)) {
  fs <- list.files(bp, pattern = "\\.csv$", full.names = TRUE)
  nm <- vapply(fs, function(f) as.character(fread(f, select = col_policy, nrows = 1L)[[1L]]), character(1))
  fmap <- stats::setNames(fs, nm)
  if (!audit_policy %in% names(fmap)) stop(sprintf("Policy '%s' not found. Have: %s", audit_policy, paste(sort(names(fmap)), collapse = ", ")))
  f <- fmap[[audit_policy]]; message("auditing ", basename(f), " (", audit_policy, ")")
  sel <- intersect(c(col_series, col_loss, col_form, col_respec, col_reason), names(fread(f, nrows = 0L)))
  d <- fread(f, select = sel)

  cat("\n--- record audit (", audit_policy, ") ---\n", sep = "")
  cat("total rows            :", nrow(d), "\n")
  cat("distinct series       :", uniqueN(d[[col_series]]), "\n")
  cat("non-finite loss rows  :", d[!is.finite(get(col_loss)), .N], "\n")
  if (col_form %in% names(d)) {
    cat("empty/blank form rows :", d[get(col_form) == "" | is.na(get(col_form)), .N], "\n")
    cat("fitted-form tally (top 15):\n"); print(d[, .N, by = col_form][order(-N)][1:15])
  }
  if (col_respec %in% names(d)) { cat("respecified tally:\n"); print(d[, .N, by = col_respec]) }
  if (col_reason %in% names(d)) { cat("trigger_reason tally:\n"); print(d[, .N, by = col_reason][order(-N)]) }
} else message("No by_policy/ under ", run_dir, "; skipping Part 1.")

# ---- Part 2: seasonal-naive MASE denominators ----------------------
if (file.exists(m4_train_csv)) {
  raw  <- fread(m4_train_csv)
  vals <- as.matrix(raw[, -1, with = FALSE])
  scale_first <- function(v) {
    v <- v[is.finite(v)]
    if (length(v) <= season) return(NA_real_)
    w <- v[seq_len(min(train_len, length(v)))]
    if (length(w) <= season) return(NA_real_)
    mean(abs(w[(season + 1L):length(w)] - w[seq_len(length(w) - season)]))
  }
  sc <- apply(vals, 1L, scale_first)
  cat("\n--- MASE denominator audit (first training window) ---\n")
  cat("series scored         :", sum(is.finite(sc)), "\n")
  cat("scale == 0            :", sum(sc == 0, na.rm = TRUE), "\n")
  cat("scale <= near_zero    :", sum(sc <= near_zero, na.rm = TRUE), "\n")
  cat("series too short      :", sum(is.na(sc)), "\n")
} else message("M4 training file not at ", m4_train_csv, "; skipping Part 2.")

message("\nAdd to 8.1: record and series counts, any non-finite/fallback fits ",
        "and how they were handled, and the count of zero/near-zero MASE denominators.")
