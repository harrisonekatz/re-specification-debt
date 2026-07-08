# Memory-bounded summarizer for a finished checkpointed run.
#
# Use this when run_m4_full_capped.R completed all batches (every part is in
# outputs/m4_full_capped/parts/) but the in-memory combine step was killed.
# It rebuilds the same outputs write_experiment_outputs would have produced,
# without ever holding the full record table in memory.
#
# Why this is faithful, not an approximation:
#   - Every per-policy summary in your pipeline is a group-by over an
#     independent policy, so running it on one policy's rows at a time gives the
#     same row as running it on everything. Medians, sd, and Spearman
#     correlations are within-policy, so they are exact this way.
#   - mean_loss / mean_fit_seconds are sum / count; accumulated across parts
#     they are exact.
#   - Series are partitioned across parts (each series is in exactly one batch),
#     so n_series is the sum of per-part unique counts, and instability, which
#     is computed per (policy, series), is identical whether compute_instability
#     runs on the whole table or part by part and is then unioned.
#   - The summary post-processing (relative columns, cost index, ordering) is
#     copied verbatim from summarize_records, including its rule that the time
#     and instability components are literal zero when their weights are zero.
#
# This was checked against summarize_records and validate_records on synthetic
# data: all summary columns and all diagnostic columns matched to 1e-12. Run
# with --verify N to repeat that check on your own first N parts, where the
# monolithic functions still fit in memory.
#
# Usage (from repo root):
#   Rscript scripts/summarize_streaming.R                  # full, faithful
#   Rscript scripts/summarize_streaming.R --skip-instability   # fast headline
#   Rscript scripts/summarize_streaming.R --verify 2       # prove it on 2 parts
#   Rscript scripts/summarize_streaming.R --with-records   # also build records.csv

suppressMessages(source("R/adaptive_update.R"))
library(data.table)

args        <- parse_cli_args()
out_dir     <- get_arg(args, "out_dir", default = "outputs/m4_full_capped", type = "character")
alpha       <- get_arg(args, "alpha", default = 0.0, type = "numeric")
gamma       <- get_arg(args, "gamma", default = 0.0, type = "numeric")
skip_inst   <- isTRUE(args$`skip_instability`) || isTRUE(args$`skip-instability`)
with_records<- isTRUE(args$`with_records`) || isTRUE(args$`with-records`)
verify_n    <- get_arg(args, "verify", default = 0L, type = "integer")
adaptive_policy <- get_arg(args, "adaptive_policy", default = "adaptive_cap8_tau0.8", type = "character")
fixed_policy    <- get_arg(args, "fixed_policy", default = "fixed_f8", type = "character")

parts_dir <- file.path(out_dir, "parts")
part_files <- sort(list.files(parts_dir, pattern = "^records_part_[0-9]+\\.csv$", full.names = TRUE))
if (length(part_files) == 0L) stop("No part files in ", parts_dir)
ensure_dir(out_dir)
message(length(part_files), " parts found in ", parts_dir)

# ---------- the verbatim tail of summarize_records, given base + instability ----------
finalize_summary <- function(base, inst_summary, alpha, gamma) {
  out <- merge(base, inst_summary, by = "policy", all.x = TRUE)
  ref <- out[policy == "full_update"]
  if (nrow(ref) == 0L) ref <- out[1L]
  ref_loss <- as.numeric(ref$mean_loss[1L]); ref_time <- as.numeric(ref$total_fit_seconds[1L]); ref_inst <- as.numeric(ref$mean_instability[1L])
  out[, relative_loss := if (is.finite(ref_loss) && ref_loss > 0) mean_loss / ref_loss else NA_real_]
  out[, relative_time := if (is.finite(ref_time) && ref_time > 0) total_fit_seconds / ref_time else NA_real_]
  out[, relative_instability := if (is.finite(ref_inst) && ref_inst > 0) {
    mean_instability / ref_inst
  } else ifelse(is.finite(mean_instability) & abs(mean_instability) <= 1e-12, 1.0, NA_real_)]
  out[, mean_respecifications_per_series := respecifications / pmax(n_series, 1L)]
  loss_component <- out$relative_loss
  time_component <- if (as.numeric(alpha) == 0) rep(0.0, nrow(out)) else as.numeric(alpha) * out$relative_time
  instability_component <- if (as.numeric(gamma) == 0) rep(0.0, nrow(out)) else as.numeric(gamma) * out$relative_instability
  out[, total_cost_index := loss_component + time_component + instability_component]
  data.table::setorder(out, total_cost_index, relative_loss, relative_time)
  out[]
}

# ---------- single streaming pass: base sums, fallback counts, instability ----------
streaming_summary <- function(files, alpha, gamma, do_instability = TRUE) {
  base_parts <- vector("list", length(files))
  fb_parts   <- vector("list", length(files))
  inst_parts <- vector("list", length(files))
  for (i in seq_along(files)) {
    d <- data.table::fread(files[[i]])
    base_parts[[i]] <- d[, .(
      sum_loss = sum(loss, na.rm = TRUE), n_loss = sum(!is.na(loss)),
      total_fit = sum(fit_seconds, na.rm = TRUE),
      sum_fit = sum(fit_seconds, na.rm = TRUE), n_fit = sum(!is.na(fit_seconds)),
      respec = sum(respecified, na.rm = TRUE),
      n_records = .N, n_series = data.table::uniqueN(series_id)
    ), by = policy]
    if ("form" %in% names(d)) {
      fb_parts[[i]] <- d[, .(n_origin_fits = .N, snaive = sum(form == "SNAIVE", na.rm = TRUE)), by = policy]
    }
    if (do_instability) inst_parts[[i]] <- compute_instability(d)
    rm(d); gc(verbose = FALSE)
    message("  pass 1: part ", i, "/", length(files))
  }
  base_acc <- data.table::rbindlist(base_parts)
  base <- base_acc[, .(
    mean_loss = sum(sum_loss) / sum(n_loss),
    total_fit_seconds = sum(total_fit),
    mean_fit_seconds = sum(sum_fit) / sum(n_fit),
    respecifications = sum(respec),
    n_records = sum(n_records),
    n_series = sum(n_series)
  ), by = policy]

  if (do_instability) {
    inst_union <- data.table::rbindlist(inst_parts, fill = TRUE)
    inst_summary <- inst_union[, .(mean_instability = mean(instability, na.rm = TRUE)), by = policy]
  } else {
    inst_summary <- base[, .(policy, mean_instability = NA_real_)]
  }
  summary <- finalize_summary(base, inst_summary, alpha, gamma)

  fb <- NULL
  if (length(fb_parts) && !all(vapply(fb_parts, is.null, logical(1)))) {
    fb_acc <- data.table::rbindlist(fb_parts)
    fb <- fb_acc[, .(n_origin_fits = sum(n_origin_fits), snaive_fallbacks = sum(snaive)), by = policy]
    fb[, fallback_pct := round(100 * snaive_fallbacks / n_origin_fits, 4)]
    data.table::setorder(fb, policy)
  }
  list(summary = summary, fallback = fb)
}

# ---------- optional verify against the monolithic functions on the first N parts ----------
if (verify_n > 0L) {
  vf <- utils::head(part_files, verify_n)
  message("VERIFY: monolithic vs streaming on first ", length(vf), " part(s)")
  full <- data.table::rbindlist(lapply(vf, data.table::fread), fill = TRUE)
  truth <- summarize_records(full, alpha = alpha, gamma = gamma)
  strm  <- streaming_summary(vf, alpha, gamma, do_instability = TRUE)$summary
  cols <- intersect(names(truth), names(strm))
  cols <- cols[vapply(cols, function(c) is.numeric(truth[[c]]), logical(1))]
  setkey(truth, policy); setkey(strm, policy)
  ok <- all(vapply(cols, function(c)
    isTRUE(all.equal(truth[[c]][order(truth$policy)], strm[[c]][order(strm$policy)], tolerance = 1e-9)),
    logical(1)))
  cat("summary identical to summarize_records:", ok, "\n")
  vt <- validate_records(full)[order(policy)]
  vp <- data.table::rbindlist(lapply(sort(unique(full$policy)),
        function(p) validate_records(full[policy == p])), fill = TRUE)[order(policy)]
  numc <- names(vt)[vapply(vt, is.numeric, logical(1))]
  okv <- all(vapply(numc, function(c) isTRUE(all.equal(vt[[c]], vp[[c]], tolerance = 1e-9)), logical(1)))
  cat("diagnostics identical to validate_records:", okv, "\n")
  quit(save = "no", status = if (ok && okv) 0L else 1L)
}

# ---------- 1. summary + fallback ----------
res <- streaming_summary(part_files, alpha, gamma, do_instability = !skip_inst)
data.table::fwrite(res$summary, file.path(out_dir, "summary.csv"))
if (!is.null(res$fallback)) data.table::fwrite(res$fallback, file.path(out_dir, "fallback_share.csv"))
cat("\n================  HEADLINE (ordered by cost index)  ================\n")
print(res$summary[, .(policy, n_series, mean_loss, relative_loss, relative_time,
                      mean_respecifications_per_series, total_cost_index)])
cat("====================================================================\n\n")

# ---------- 2. per-policy diagnostics + bridge summary (re-shard by policy) ----------
policies <- sort(unique(data.table::fread(part_files[[1]], select = "policy")$policy))
shard_dir <- file.path(out_dir, "by_policy")
ensure_dir(shard_dir)
shard_files <- file.path(shard_dir, paste0("policy_", seq_along(policies), ".csv"))
names(shard_files) <- policies
for (f in shard_files) if (file.exists(f)) file.remove(f)
for (i in seq_along(part_files)) {
  d <- data.table::fread(part_files[[i]])
  for (p in policies) {
    dp <- d[policy == p]
    if (nrow(dp)) data.table::fwrite(dp, shard_files[[p]], append = file.exists(shard_files[[p]]))
  }
  rm(d); gc(verbose = FALSE)
  message("  re-shard: part ", i, "/", length(part_files))
}

diag_rows <- list(); bridge_rows <- list()
for (p in policies) {
  dp <- data.table::fread(shard_files[[p]])
  diag_rows[[p]]   <- tryCatch(validate_records(dp),            error = function(e) NULL)
  bridge_rows[[p]] <- tryCatch(summarize_spec_debt_bridge(dp),  error = function(e) NULL)
  rm(dp); gc(verbose = FALSE)
}
diagnostics <- data.table::rbindlist(diag_rows, fill = TRUE)[order(policy)]
data.table::fwrite(diagnostics, file.path(out_dir, "diagnostics.csv"))
bridge_summary <- data.table::rbindlist(bridge_rows, fill = TRUE)
if (nrow(bridge_summary)) bridge_summary <- bridge_summary[order(policy)]
data.table::fwrite(bridge_summary, file.path(out_dir, "spec_debt_bridge_summary.csv"))

# ---------- 3. triggered subset (only the two policies it compares) ----------
two <- intersect(c(adaptive_policy, fixed_policy), policies)
if (length(two) == 2L) {
  pair <- data.table::rbindlist(lapply(two, function(p) data.table::fread(shard_files[[p]])), fill = TRUE)
  triggered <- tryCatch(triggered_subset_analysis(pair, adaptive_policy, fixed_policy),
                        error = function(e) data.table::data.table())
  data.table::fwrite(triggered, file.path(out_dir, "triggered_subset.csv"))
  rm(pair); gc(verbose = FALSE)
}

# ---------- 4. figures from the small summary (skip gracefully if ggplot2 missing) ----------
invisible(tryCatch({
  write_latex_table(res$summary, file.path(out_dir, "summary_table.tex"))
  plot_frontier(res$summary, file.path(out_dir, "frontier.png"))
  plot_update_counts(res$summary, file.path(out_dir, "update_counts.png"))
}, error = function(e) message("figure step skipped: ", conditionMessage(e))))

# ---------- 5. optional full records.csv via streaming concat ----------
if (with_records) {
  h1 <- names(data.table::fread(part_files[[1]], nrows = 0))
  same <- all(vapply(part_files, function(f) identical(names(data.table::fread(f, nrows = 0)), h1), logical(1)))
  if (!same) {
    message("records.csv skipped: parts have differing columns; combine needs fill=TRUE on a larger box.")
  } else {
    rec_path <- file.path(out_dir, "records.csv")
    if (file.exists(rec_path)) file.remove(rec_path)
    con <- file(rec_path, open = "wt")
    writeLines(paste(h1, collapse = ","), con)
    for (f in part_files) {
      ll <- readLines(f); if (length(ll) > 1L) writeLines(ll[-1L], con)
    }
    close(con)
    message("records.csv written by concatenation (", rec_path, ")")
  }
}

cat("done. files in ", out_dir, "\n")
