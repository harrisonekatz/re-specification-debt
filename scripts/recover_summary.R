#!/usr/bin/env Rscript
# ---------------------------------------------------------------------------
# recover_summary.R
#
# Rebuild summary.csv from a run's parts/ checkpoints WITHOUT loading every row
# at once. Use when a run finished (parts/ is full, progress.log hit 100%) but
# the final in-memory combine died, e.g. on memory with tens of millions of rows.
#
# Each series lives entirely within one part, so this reproduces
# summarize_records() exactly while holding one part in memory at a time.
#
# Pass 1 is fast (loss, time, re-specs) and prints the answer immediately.
# Pass 2 computes instability (slower, nested loops) and writes the full summary.
# Ctrl-C after pass 1 and you still have summary_core.csv and the printed table.
#
#   Rscript scripts/recover_summary.R --out_dir outputs/m4_tracking_signal
#   Rscript scripts/recover_summary.R --out_dir outputs/... --instability FALSE
# ---------------------------------------------------------------------------

suppressWarnings(suppressMessages({
  a <- commandArgs(FALSE)
  f <- sub("^--file=", "", a[grepl("^--file=", a)])
  root <- if (length(f)) dirname(dirname(normalizePath(f[[1]]))) else getwd()
  core <- file.path(root, "R", "adaptive_update.R")
  if (!file.exists(core)) core <- file.path(getwd(), "R", "adaptive_update.R")
  source(core)
}))
library(data.table)

args    <- parse_cli_args()
out_dir <- get_arg(args, "out_dir", "outputs/m4_tracking_signal")
do_inst <- as.logical(get_arg(args, "instability", TRUE, "logical"))

parts <- sort(list.files(file.path(out_dir, "parts"),
                         pattern = "^part_[0-9]+\\.csv$", full.names = TRUE))
if (length(parts) == 0L) stop("No parts found in ", file.path(out_dir, "parts"))
message(length(parts), " part files found in ", out_dir, "/parts")

# ---- Pass 1: core metrics (fast) ------------------------------------------
core_cols <- c("policy", "series_id", "loss", "fit_seconds", "respecified")
B <- vector("list", length(parts))
for (i in seq_along(parts)) {
  dt <- fread(parts[[i]], select = core_cols)
  B[[i]] <- dt[, .(sl = sum(loss, na.rm = TRUE), n = .N,
                   sf = sum(fit_seconds, na.rm = TRUE),
                   rs = sum(respecified, na.rm = TRUE),
                   ns = uniqueN(series_id)), by = policy]
  if (i %% 10L == 0L || i == length(parts)) message("  pass 1: ", i, "/", length(parts))
}
Ball <- rbindlist(B)
S <- Ball[, .(mean_loss = sum(sl) / sum(n),
              total_fit_seconds = sum(sf),
              respecifications = sum(rs),
              n_series = sum(ns)), by = policy]

ref <- S[policy == "full_update"]
if (nrow(ref) == 0L) ref <- S[1L]
S[, relative_loss := mean_loss / as.numeric(ref$mean_loss[1L])]
S[, relative_time := total_fit_seconds / as.numeric(ref$total_fit_seconds[1L])]
S[, mean_respecifications_per_series := respecifications / pmax(n_series, 1L)]
setorder(S, relative_loss)
fwrite(S, file.path(out_dir, "summary_core.csv"))

cat("\n==== CORE SUMMARY (loss / time / re-spec) ====\n")
print(as.data.frame(S)[, c("policy", "relative_loss", "relative_time",
                           "mean_respecifications_per_series")], row.names = FALSE)
cat("\nwrote ", file.path(out_dir, "summary_core.csv"), "\n\n", sep = "")
flush(stdout())

if (!isTRUE(do_inst)) quit(save = "no")

# ---- Pass 2: instability (slower) -----------------------------------------
message("computing instability across parts (slower)...")
IB <- vector("list", length(parts))
for (i in seq_along(parts)) {
  dt <- fread(parts[[i]])
  inst <- compute_instability(dt)
  IB[[i]] <- inst[is.finite(instability), .(isum = sum(instability), inn = .N), by = policy]
  if (i %% 10L == 0L || i == length(parts)) message("  pass 2: ", i, "/", length(parts))
}
Iall <- rbindlist(IB)
Isum <- Iall[, .(mean_instability = sum(isum) / sum(inn)), by = policy]
S <- merge(S, Isum, by = "policy", all.x = TRUE)

ri <- S[policy == "full_update"]$mean_instability
ri <- if (length(ri)) as.numeric(ri[1L]) else as.numeric(S$mean_instability[1L])
S[, relative_instability := if (is.finite(ri) && ri > 0) mean_instability / ri else NA_real_]
S[, total_cost_index := relative_loss]   # alpha = gamma = 0, matches the run
setorder(S, total_cost_index, relative_loss, relative_time)

fwrite(S, file.path(out_dir, "summary.csv"))
try(write_latex_table(S, file.path(out_dir, "summary_table.tex")), silent = TRUE)

cat("\n==== FULL SUMMARY ====\n")
print(as.data.frame(S)[, c("policy", "relative_loss", "relative_time",
                           "relative_instability", "mean_respecifications_per_series")],
      row.names = FALSE)
cat("\nwrote ", file.path(out_dir, "summary.csv"), " and summary_table.tex\n", sep = "")
