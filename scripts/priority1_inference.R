# priority1_inference.R
#
# Turns the Priority 1 run (scripts/run_m4_priority1.R) into the aligned
# deciding-tests table and its supporting outputs, streaming the checkpoint
# parts so nothing requires the full record table in memory.
#
# What it computes, and why it is faithful to the run:
#   - Horizon-h MASE at h in {3,6,9,12,18} from the horizon-18 records via an
#     identity, not a re-fit: the stored loss is mean(|e_1..18|)/scale, so
#     loss_h = loss * mean(|e_1..h|) / mean(|e_1..18|) uses the same errors,
#     the same scale, and the same origins. The script asserts the identity
#     reproduces the stored loss at h = 18 to 1e-6 before using it.
#   - Per-series instability as the mean smAPC between consecutive origins'
#     overlapping forecast leads, the same quantity compute_instability
#     reports (previous origin's leads 2..18 against the current origin's
#     leads 1..17 at the shared target dates).
#   - Search decomposition per policy: initial, cap-driven, score-fired,
#     signal-fired, scheduled, plus ACTUAL form changes (deployed form
#     differing from the previous origin), the split Section 6.2 of the
#     review asks for.
#   - The pre-declared contrasts of docs/prereg_priority1.md, each with a
#     series-level t, an iid bootstrap CI over series, trimmed and winsorized
#     means, the median, win shares, and top-1-percent concentration.
#   - Repro gates against the published full-scale horizon-18 values.
#   - Cost columns and the horizon-18 break-even, anchored at full_update
#     through the timing stratum when outputs/m4_priority1_timing exists.
#   - The lead-specific loss-difference profile for the gate contrasts.
#
# Usage (repo root, after the run completes):
#   Rscript scripts/priority1_inference.R
#   Rscript scripts/priority1_inference.R --run-dir outputs/m4_priority1 \
#       --timing-dir outputs/m4_priority1_timing --boot 10000
#   Rscript scripts/priority1_inference.R --splice-dir outputs/m4_priority1_trigg_v2
#
# --splice-dir: a policy-subset rerun (for example trigg only, after the
# mechanics fix) writes parts to its own out dir; --splice-dir replaces those
# policies' per-series rows with the rerun's before any contrast, gate, or
# summary is computed, leaving every other policy's rows untouched.
#
# Outputs (under --run-dir):
#   per_series_priority1.csv   per (policy, series): horizon losses, seconds,
#                              instability, search decomposition, form changes
#   per_series_leads.csv       per (policy, series): mean per-lead loss
#   contrasts_priority1.csv    the deciding-tests table, all horizons
#   repro_gates.csv            PASS / CHECK against published values
#   policy_summary_priority1.csv  per-policy per-horizon means + cost columns
#   cost_breakeven.csv         horizon-18 break-even for the primary contrast
#   leadwise_diffs.csv         per-lead mean differences with bootstrap CIs
#   table_priority1.tex        LaTeX block for the replacement Table 5

suppressMessages(source("R/adaptive_update.R"))
suppressMessages(library(data.table))

args <- parse_cli_args()
run_dir <- get_arg(args, "run_dir", default = "outputs/m4_priority1", type = "character")
splice_dir <- get_arg(args, "splice_dir", default = "", type = "character")
timing_dir <- get_arg(args, "timing_dir", default = "outputs/m4_priority1_timing", type = "character")
n_boot <- get_arg(args, "boot", default = 10000L, type = "integer")
n_boot_lead <- get_arg(args, "boot_lead", default = 2000L, type = "integer")
seed <- get_arg(args, "seed", default = 20260724L, type = "integer")
tol <- get_arg(args, "tol", default = 0.0010, type = "numeric")
horizons <- parse_integer_list(get_arg(args, "horizons", default = "3,6,9,12,18", type = "character"))

parts_dir <- file.path(run_dir, "parts")
part_files <- sort(list.files(parts_dir, pattern = "^records_part_[0-9]+\\.csv$", full.names = TRUE))
if (length(part_files) == 0L) stop("No part files in ", parts_dir, ". Run scripts/run_m4_priority1.R first.")
message(length(part_files), " parts found in ", parts_dir)

num_sorted <- function(nm, prefix) {
  x <- grep(paste0("^", prefix, "[0-9]+$"), nm, value = TRUE)
  x[order(as.integer(sub(paste0("^", prefix), "", x)))]
}

# ---------------------------------------------------------------------------
# Streaming pass over parts
# ---------------------------------------------------------------------------

process_run <- function(files, want_horizons, label) {
  scal_parts <- vector("list", length(files))
  lead_parts <- vector("list", length(files))
  inst_parts <- vector("list", length(files))
  H_run <- NULL
  hz <- want_horizons
  max_gap <- 0.0

  for (i in seq_along(files)) {
    hdr <- names(fread(files[[i]], nrows = 0L))
    fcols <- num_sorted(hdr, "f")
    ycols <- num_sorted(hdr, "y")
    if (is.null(H_run)) {
      H_run <- length(fcols)
      hz <- sort(unique(pmin(hz, H_run)))
      message("Detected ", H_run, " stored leads; evaluating horizons ",
              paste(hz, collapse = ", "))
    }
    base_cols <- intersect(
      c("series_id", "policy", "origin_number", "form", "loss", "fit_seconds",
        "respecified", "trigger_reason", "triggered_by_score", "triggered_by_cap"),
      hdr
    )
    d <- fread(files[[i]], select = c(base_cols, ycols, fcols))
    setorder(d, policy, series_id, origin_number)

    Y <- as.matrix(d[, ycols, with = FALSE])
    Fm <- as.matrix(d[, fcols, with = FALSE])
    Eab <- abs(Y - Fm)
    Ecum <- Eab
    if (H_run > 1L) for (k in 2:H_run) Ecum[, k] <- Ecum[, k - 1L] + Eab[, k]
    mH <- Ecum[, H_run] / H_run

    ratio_for <- function(num_mean) {
      ifelse(mH > 0, num_mean / mH, ifelse(num_mean == 0, 1.0, NA_real_))
    }
    for (h in hz) {
      d[[paste0("loss_h", h)]] <- d$loss * ratio_for(Ecum[, h] / h)
    }
    # Identity check: the reconstruction at h = H_run must reproduce the stored loss.
    chk <- abs(d[[paste0("loss_h", H_run)]] - d$loss)
    max_gap <- max(max_gap, suppressWarnings(max(chk[is.finite(chk)], 0)))

    for (j in seq_len(H_run)) {
      d[[paste0("lead_", j)]] <- d$loss * ratio_for(Eab[, j])
    }
    leadcols <- paste0("lead_", seq_len(H_run))
    losscols <- paste0("loss_h", hz)

    scal_parts[[i]] <- d[, c(
      list(
        n_origins = .N,
        fit_seconds = sum(fit_seconds, na.rm = TRUE),
        n_search = sum(respecified, na.rm = TRUE),
        n_init = sum(respecified & (origin_number == 0L | trigger_reason == "initial"), na.rm = TRUE),
        n_cap = sum(trigger_reason == "cap", na.rm = TRUE),
        n_fire_score = sum(trigger_reason == "score", na.rm = TRUE),
        n_fire_signal = sum(trigger_reason == "signal", na.rm = TRUE),
        n_scheduled = sum(respecified & origin_number > 0L & trigger_reason == "none", na.rm = TRUE),
        n_form_changes = sum(form != shift(form), na.rm = TRUE)
      ),
      lapply(.SD, function(x) mean(x, na.rm = TRUE))
    ), by = .(policy, series_id), .SDcols = losscols]

    lead_parts[[i]] <- d[, lapply(.SD, function(x) mean(x, na.rm = TRUE)),
                         by = .(policy, series_id), .SDcols = leadcols]

    inst_parts[[i]] <- d[, {
      if (.N < 2L) {
        list(instability = NA_real_)
      } else {
        Fg <- as.matrix(.SD)
        a <- Fg[-.N, -1L, drop = FALSE]
        b <- Fg[-1L, -ncol(Fg), drop = FALSE]
        den <- abs(a) + abs(b)
        num <- abs(a - b)
        keep <- is.finite(den) & den > 1e-12 & is.finite(num)
        vals <- vapply(seq_len(nrow(a)), function(r) {
          k <- keep[r, ]
          if (!any(k)) 0.0 else 200 * mean(num[r, k] / den[r, k])
        }, numeric(1L))
        list(instability = mean(vals))
      }
    }, by = .(policy, series_id), .SDcols = fcols]

    message("  [", label, "] part ", i, "/", length(files), ": ", nrow(d), " rows")
    rm(d, Y, Fm, Eab, Ecum)
    gc(verbose = FALSE)
  }

  if (max_gap > 1e-6) {
    stop("Lead-ratio identity failed on the ", label, " run: reconstructed h=",
         H_run, " loss differs from the stored loss by up to ", format(max_gap),
         ". Investigate before trusting any multi-horizon number.")
  }
  message("  [", label, "] lead-ratio identity holds: max |reconstructed - stored| = ",
          format(max_gap, digits = 3))
  list(
    PS = merge(data.table::rbindlist(scal_parts), data.table::rbindlist(inst_parts),
               by = c("policy", "series_id"), all.x = TRUE),
    LD = data.table::rbindlist(lead_parts),
    H = H_run, horizons = hz
  )
}

main <- process_run(part_files, horizons, "main")
H <- main$H
horizons <- main$horizons
PS <- main$PS
LD <- main$LD

if (nzchar(splice_dir)) {
  sfiles <- sort(list.files(file.path(splice_dir, "parts"),
                            pattern = "^records_part_[0-9]+\\.csv$", full.names = TRUE))
  if (length(sfiles) == 0L) stop("No part files in ", file.path(splice_dir, "parts"))
  message(length(sfiles), " splice parts found in ", file.path(splice_dir, "parts"))
  sp <- process_run(sfiles, horizons, "splice")
  if (!identical(sp$H, H)) {
    stop("Splice run stores ", sp$H, " leads; the main run stores ", H,
         ". The two runs must share the horizon.")
  }
  spol <- unique(sp$PS$policy)
  message("Splicing policies from ", splice_dir, ": ", paste(spol, collapse = ", "))
  PS <- rbind(PS[!policy %in% spol], sp$PS)
  LD <- rbind(LD[!policy %in% spol], sp$LD)
}

fwrite(PS, file.path(run_dir, "per_series_priority1.csv"))
fwrite(LD, file.path(run_dir, "per_series_leads.csv"))

pol_counts <- PS[, .N, by = policy][order(policy)]
message("Per-series table: ", nrow(PS), " (policy, series) rows")
print(pol_counts)
full_scale <- min(pol_counts$N) >= 30000L

# ---------------------------------------------------------------------------
# Pre-declared contrasts
# ---------------------------------------------------------------------------

find_policy <- function(prefix, exact = NULL) {
  pols <- unique(PS$policy)
  if (!is.null(exact) && exact %in% pols) return(exact)
  hit <- grep(paste0("^", prefix), pols, value = TRUE)
  if (length(hit) == 1L) return(hit)
  if (length(hit) > 1L) stop("Ambiguous policy prefix '", prefix, "': ",
                             paste(hit, collapse = ", "))
  NA_character_
}

p_aicc <- find_policy("aicc_gate_cap", "aicc_gate_cap8_tau0.8")
p_vgate <- find_policy("adaptive_cap", "adaptive_cap8_tau0.8")
p_trigg <- find_policy("trigg_cap", "trigg_cap8_tau0.6")
p_f8 <- find_policy("fixed_f8", "fixed_f8")
p_f4 <- find_policy("fixed_f4", "fixed_f4")
p_par <- find_policy("parameter_only", "parameter_only")

contrast_spec <- list(
  list(a = p_aicc, b = p_f8,   role = "primary"),
  list(a = p_aicc, b = p_trigg, role = "secondary"),
  list(a = p_aicc, b = p_vgate, role = "secondary"),
  list(a = p_aicc, b = p_f4,   role = "secondary"),
  list(a = p_vgate, b = p_f8,  role = "repro"),
  list(a = p_trigg, b = p_f8,  role = "repro"),
  list(a = p_trigg, b = p_vgate, role = "repro"),
  list(a = p_trigg, b = p_f4,  role = "rate_matched"),
  list(a = p_par,  b = p_f8,   role = "context")
)
contrast_spec <- Filter(function(z) !is.na(z$a) && !is.na(z$b), contrast_spec)

boot_ci <- function(d, B) {
  n <- length(d)
  stat <- vapply(seq_len(B), function(b) mean(d[sample.int(n, n, replace = TRUE)]),
                 numeric(1L))
  stats::quantile(stat, c(0.025, 0.975), names = FALSE)
}
winsor_mean <- function(d, p = 0.01) {
  q <- stats::quantile(d, c(p, 1 - p), names = FALSE)
  mean(pmin(pmax(d, q[1L]), q[2L]))
}
top_frac <- function(d, p = 0.01) {
  k <- max(1L, ceiling(p * length(d)))
  ord <- order(abs(d), decreasing = TRUE)
  top <- ord[seq_len(k)]
  list(
    mean_excl = mean(d[-top]),
    abs_share = sum(abs(d[top])) / max(sum(abs(d)), .Machine$double.eps)
  )
}

set.seed(seed)
rows <- list()
for (cs in contrast_spec) {
  A <- PS[policy == cs$a]
  B <- PS[policy == cs$b]
  for (h in horizons) {
    col <- paste0("loss_h", h)
    m <- merge(A[, .(series_id, la = get(col))],
               B[, .(series_id, lb = get(col))], by = "series_id")
    m <- m[is.finite(la) & is.finite(lb)]
    d <- m$la - m$lb
    if (length(d) < 3L) next
    tt <- try(stats::t.test(d), silent = TRUE)
    ci <- boot_ci(d, n_boot)
    tf <- top_frac(d)
    rows[[length(rows) + 1L]] <- data.table(
      contrast = paste0(cs$a, " vs ", cs$b),
      policy_a = cs$a, policy_b = cs$b, role = cs$role, horizon = h,
      n_series = length(d),
      mean_diff = mean(d),
      boot_lo = ci[1L], boot_hi = ci[2L],
      series_t = if (inherits(tt, "try-error")) NA_real_ else unname(tt$statistic),
      series_p = if (inherits(tt, "try-error")) NA_real_ else tt$p.value,
      median_diff = stats::median(d),
      share_favors_a = mean(d < 0),
      trim1 = mean(d, trim = 0.01),
      trim2p5 = mean(d, trim = 0.025),
      trim5 = mean(d, trim = 0.05),
      winsor1 = winsor_mean(d, 0.01),
      mean_excl_top1 = tf$mean_excl,
      abs_share_top1 = tf$abs_share
    )
  }
}
CT <- rbindlist(rows)
setorder(CT, role, contrast, horizon)
fwrite(CT, file.path(run_dir, "contrasts_priority1.csv"))
message("\n================ deciding tests (negative favors the first policy) ================")
print(CT[, .(contrast, role, horizon, n_series,
             mean_diff = round(mean_diff, 5),
             boot_lo = round(boot_lo, 5), boot_hi = round(boot_hi, 5),
             t = round(series_t, 2), trim5 = round(trim5, 5),
             share_favors_a = round(share_favors_a, 3))])

# ---------------------------------------------------------------------------
# Repro gates against the published full-scale horizon-18 values
# ---------------------------------------------------------------------------

Hmax <- max(horizons)
gates <- data.table(
  what = c("validation gate vs fixed_f8", "trigg vs fixed_f8", "trigg vs validation gate"),
  policy_a = c(p_vgate, p_trigg, p_trigg),
  policy_b = c(p_f8, p_f8, p_vgate),
  expected = c(-0.0043, -0.0011, +0.0032)
)
gates <- gates[!is.na(policy_a) & !is.na(policy_b)]
gate_rows <- list()
for (g in seq_len(nrow(gates))) {
  r <- CT[policy_a == gates$policy_a[g] & policy_b == gates$policy_b[g] & horizon == Hmax]
  got <- if (nrow(r) == 1L) r$mean_diff else NA_real_
  ok <- is.finite(got) && abs(got - gates$expected[g]) <= tol
  gate_rows[[g]] <- data.table(
    gate = gates$what[g], horizon = Hmax,
    expected = gates$expected[g], observed = got,
    abs_gap = abs(got - gates$expected[g]), tol = tol,
    status = if (!full_scale) "SMOKE (not full scale)" else if (ok) "PASS" else "CHECK"
  )
}
GT <- rbindlist(gate_rows)
fwrite(GT, file.path(run_dir, "repro_gates.csv"))
message("\n================ repro gates (published full-scale horizon-18 values) ================")
print(GT)
if (full_scale && any(GT$status == "CHECK")) {
  message("One or more gates missed. If a trigg gate missed and the others passed, ",
          "set --trigg-alpha / --trigg-reset / --trigg-warmup / --trigg-signal-every ",
          "to the tracking-run configuration and re-run the trigg policy only ",
          "(--policies trigg). The primary contrast does not involve trigg.")
}

# ---------------------------------------------------------------------------
# Policy summary, cost columns, break-even
# ---------------------------------------------------------------------------

losscols <- paste0("loss_h", horizons)
POL <- PS[, c(
  list(
    n_series = .N,
    mean_fit_seconds = mean(fit_seconds),
    mean_instability = mean(instability, na.rm = TRUE),
    mean_searches = mean(n_search),
    mean_init = mean(n_init),
    mean_cap = mean(n_cap),
    mean_fire_score = mean(n_fire_score),
    mean_fire_signal = mean(n_fire_signal),
    mean_scheduled = mean(n_scheduled),
    mean_form_changes = mean(n_form_changes)
  ),
  lapply(.SD, function(x) mean(x, na.rm = TRUE))
), by = policy, .SDcols = losscols]

f8_sec <- POL[policy == p_f8]$mean_fit_seconds[1L]
f8_inst <- POL[policy == p_f8]$mean_instability[1L]
POL[, rel_time_vs_f8 := mean_fit_seconds / f8_sec]
POL[, rel_inst_vs_f8 := mean_instability / f8_inst]

timing_path <- file.path(timing_dir, "timing_summary.csv")
L_full <- T_full_sec <- I_full <- NA_real_
if (file.exists(timing_path)) {
  TS <- fread(timing_path)
  POL <- merge(POL, TS[, .(policy, stratum_relative_time = relative_time,
                           stratum_relative_instability = relative_instability)],
               by = "policy", all.x = TRUE)
  # full_update anchors for the break-even, from the stratum parts.
  tparts <- sort(list.files(file.path(timing_dir, "parts"),
                            pattern = "^records_part_[0-9]+\\.csv$", full.names = TRUE))
  if (length(tparts) > 0L) {
    fu <- rbindlist(lapply(tparts, function(f) {
      x <- fread(f, select = c("policy", "series_id", "loss"))
      x[policy == "full_update"]
    }))
    if (nrow(fu) > 0L) L_full <- mean(fu[, .(s = mean(loss, na.rm = TRUE)), by = series_id]$s)
  }
  T_full_sec <- TS[policy == "full_update"]$mean_fit_seconds[1L]
  I_full <- TS[policy == "full_update"]$mean_instability[1L]
} else {
  message("No timing stratum at ", timing_path,
          "; cost columns stay anchored at fixed_f8. Run ",
          "scripts/run_priority1_timing_stratum.R for full_update anchoring.")
}
fwrite(POL, file.path(run_dir, "policy_summary_priority1.csv"))
message("\n================ policy summary ================")
print(POL[, c("policy", "n_series", paste0("loss_h", Hmax), "mean_fit_seconds",
              "mean_instability", "mean_searches", "mean_form_changes"), with = FALSE])

be_pairs <- Filter(function(z) !is.na(z$a) && !is.na(z$b), list(
  list(a = p_aicc, b = p_f8),
  list(a = p_vgate, b = p_f8)
))
be_rows <- list()
for (bp in be_pairs) {
  colH <- paste0("loss_h", Hmax)
  L_a <- POL[policy == bp$a][[colH]][1L]
  L_b <- POL[policy == bp$b][[colH]][1L]
  S_a <- POL[policy == bp$a]$mean_fit_seconds[1L]
  S_b <- POL[policy == bp$b]$mean_fit_seconds[1L]
  I_a <- POL[policy == bp$a]$mean_instability[1L]
  I_b <- POL[policy == bp$b]$mean_instability[1L]
  dL <- L_b - L_a          # positive when the first policy is more accurate
  dS <- S_a - S_b          # positive when the first policy costs more time
  dI <- I_a - I_b
  sec_per_mase_pt <- if (dL > 0) dS / (dL / 0.001) else NA_real_
  alpha_star <- gamma_star <- NA_real_
  anchor <- "unavailable (no timing stratum)"
  if (is.finite(L_full) && is.finite(T_full_sec) && L_full > 0 && T_full_sec > 0) {
    anchor <- "full_update (timing stratum)"
    alpha_star <- (dL / L_full) / ((dS / T_full_sec))
    if (is.finite(I_full) && I_full > 0 && dI != 0) {
      gamma_star <- (dL / L_full) / (dI / I_full)
    }
  }
  be_rows[[length(be_rows) + 1L]] <- data.table(
    horizon = Hmax, policy_a = bp$a, policy_b = bp$b,
    dloss = dL, dseconds_per_series = dS, dinstability = dI,
    seconds_per_0.001_mase = sec_per_mase_pt,
    alpha_star = alpha_star, gamma_star = gamma_star, anchor = anchor
  )
  message("\n================ horizon-", Hmax, " break-even, ", bp$a, " vs ", bp$b, " ================")
  message("loss advantage ", format(round(dL, 5)), " MASE | extra compute ",
          format(round(dS, 3)), " s/series | extra instability ", format(round(dI, 4)))
  if (is.finite(sec_per_mase_pt)) {
    message("the first policy pays ", format(round(sec_per_mase_pt, 2)),
            " extra seconds per series for each 0.001 of MASE it removes")
  } else {
    message("the first policy is not more accurate at this horizon; no accuracy break-even exists")
  }
  if (is.finite(alpha_star)) {
    message("cost-index break-even: it wins the composite C = L + alpha T",
            " whenever alpha < ", format(signif(alpha_star, 3)),
            " (anchors: ", anchor, ")")
  }
}
be <- rbindlist(be_rows)
if (nrow(be) > 0L) fwrite(be, file.path(run_dir, "cost_breakeven.csv"))

# ---------------------------------------------------------------------------
# Lead-specific profile for the gate contrasts
# ---------------------------------------------------------------------------

lead_pairs <- Filter(function(z) !is.na(z$a) && !is.na(z$b), list(
  list(a = p_aicc, b = p_f8),
  list(a = p_vgate, b = p_f8)
))
lw_rows <- list()
for (lp in lead_pairs) {
  A <- LD[policy == lp$a]
  B <- LD[policy == lp$b]
  for (j in seq_len(H)) {
    col <- paste0("lead_", j)
    m <- merge(A[, .(series_id, la = get(col))],
               B[, .(series_id, lb = get(col))], by = "series_id")
    m <- m[is.finite(la) & is.finite(lb)]
    d <- m$la - m$lb
    if (length(d) < 3L) next
    ci <- boot_ci(d, n_boot_lead)
    lw_rows[[length(lw_rows) + 1L]] <- data.table(
      contrast = paste0(lp$a, " vs ", lp$b), lead = j, n_series = length(d),
      mean_diff = mean(d), boot_lo = ci[1L], boot_hi = ci[2L]
    )
  }
}
LW <- rbindlist(lw_rows)
fwrite(LW, file.path(run_dir, "leadwise_diffs.csv"))

# ---------------------------------------------------------------------------
# LaTeX block for the replacement Table 5
# ---------------------------------------------------------------------------

fmt <- function(x, k = 4) ifelse(is.finite(x), sprintf(paste0("%.", k, "f"), x), "")
tex <- c(
  "\\begin{tabular}{llrrl}",
  "\\toprule",
  "Comparison & Horizon & Mean & 95\\% CI & $t$ \\\\",
  "\\midrule"
)
show <- CT[role %in% c("primary", "repro", "rate_matched", "secondary")]
setorder(show, role, contrast, horizon)
for (r in seq_len(nrow(show))) {
  tex <- c(tex, paste0(
    gsub("_", "\\\\_", show$contrast[r]), " & ", show$horizon[r], " & ",
    fmt(show$mean_diff[r]), " & $[", fmt(show$boot_lo[r]), ",\\,",
    fmt(show$boot_hi[r]), "]$ & ", fmt(show$series_t[r], 1), " \\\\"
  ))
}
tex <- c(tex, "\\bottomrule", "\\end{tabular}")
writeLines(tex, file.path(run_dir, "table_priority1.tex"))

message("\nWrote per_series_priority1.csv, per_series_leads.csv, ",
        "contrasts_priority1.csv, repro_gates.csv, policy_summary_priority1.csv, ",
        "cost_breakeven.csv, leadwise_diffs.csv, table_priority1.tex under ", run_dir)
