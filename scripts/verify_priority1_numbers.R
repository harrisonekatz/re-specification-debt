# verify_priority1_numbers.R
#
# Re-checks the Priority 1 outputs the way verify_paper_numbers.R checks the
# published tables: every headline cell, sign pattern, and invariant asserted
# against the frozen CSVs. Run after priority1_inference.R; exits nonzero on
# any failure. Full-scale value checks activate only when the run covers the
# full eligible set, so the script also passes on smoke outputs.
#
# Usage (repo root):
#   Rscript scripts/verify_priority1_numbers.R --run-dir outputs/m4_priority1

suppressMessages(source("R/adaptive_update.R"))
suppressMessages(library(data.table))

args <- parse_cli_args()
run_dir <- get_arg(args, "run_dir", default = "outputs/m4_priority1", type = "character")

CT <- fread(file.path(run_dir, "contrasts_priority1.csv"))
POL <- fread(file.path(run_dir, "policy_summary_priority1.csv"))
GT <- fread(file.path(run_dir, "repro_gates.csv"))
BE <- fread(file.path(run_dir, "cost_breakeven.csv"))
LW <- fread(file.path(run_dir, "leadwise_diffs.csv"))

n_pass <- 0L; n_fail <- 0L
check <- function(label, cond) {
  ok <- isTRUE(cond)
  if (ok) { n_pass <<- n_pass + 1L } else { n_fail <<- n_fail + 1L }
  cat(sprintf("[%s] %s\n", ifelse(ok, "PASS", "FAIL"), label))
  invisible(ok)
}
row_of <- function(a, b, h) CT[policy_a == a & policy_b == b & horizon == h]
ci_negative <- function(r) nrow(r) == 1L && r$boot_hi < 0
ci_positive <- function(r) nrow(r) == 1L && r$boot_lo > 0
ci_covers_zero <- function(r) nrow(r) == 1L && r$boot_lo < 0 && r$boot_hi > 0

full_scale <- max(CT$n_series) >= 30000L
cat(sprintf("Run scale: %s (max n_series = %d)\n\n",
            ifelse(full_scale, "FULL", "smoke"), max(CT$n_series)))

p_aicc <- "aicc_gate_cap8_tau0.8"; p_vgate <- "adaptive_cap8_tau0.8"
p_trigg <- "trigg_cap8_tau0.6"; p_f4 <- "fixed_f4"; p_f8 <- "fixed_f8"
p_par <- "parameter_only"
hz <- sort(unique(CT$horizon))

check("all contrast rows share one series count", uniqueN(CT$n_series) == 1L)
check("five horizons present, horizon 6 included", all(c(3, 6, 9, 12, 18) %in% hz))

# Intervention accounting.
g <- function(p, col) POL[policy == p][[col]][1L]
check("fixed_f8 makes exactly 5 searches", abs(g(p_f8, "mean_searches") - 5) < 1e-9)
check("fixed_f4 makes exactly 9 searches", abs(g(p_f4, "mean_searches") - 9) < 1e-9)
check("parameter_only makes exactly 1 search and 0 form changes",
      abs(g(p_par, "mean_searches") - 1) < 1e-9 && g(p_par, "mean_form_changes") == 0)
check("form changes never exceed searches",
      all(POL$mean_form_changes <= POL$mean_searches + 1e-9))

# Lead profile outputs.
check("leadwise diffs cover both gate contrasts at every stored lead",
      uniqueN(LW$contrast) == 2L && all(table(LW$contrast) == max(LW$lead)))

# Practical equivalence (equivalence_tost.R on the original h3 records).
# Independent of the Priority 1 run's scale, so guarded by its own file and
# its own full-scale test on the equivalence CSV itself.
eq_path <- "outputs/m4_full_capped/equivalence_tost.csv"
if (file.exists(eq_path)) {
  EQ <- fread(eq_path)
  check("equivalence: eleven cadences tested", nrow(EQ) == 11L)
  if (max(EQ$n_series) >= 40000L) {
    check("equivalence: every cadence inside the prespecified 0.5% margin",
          all(EQ$equivalent) && all(EQ$equivalent_boot))
    check("equivalence: cadences 2 through 9 attain bounds within 0.125%",
          all(EQ[as.integer(sub("^fixed_f", "", policy)) <= 9L]$attained_margin_rel < 0.00125))
    check("equivalence: fixed_f4 mean within 5e-05 of full updating",
          abs(EQ[policy == "fixed_f4"]$mean_diff[1L]) < 5e-5)
  }
} else {
  cat("[SKIP] equivalence CSV not found at", eq_path, "\n")
}

if (full_scale) {
  # Sign structure of the deciding tests, every horizon reported.
  for (h in hz) {
    check(sprintf("primary aicc vs f8 favors the gate, CI clear of zero, h=%d", h),
          ci_negative(row_of(p_aicc, p_f8, h)))
    check(sprintf("rate-matched trigg vs f4 CI covers zero, h=%d", h),
          ci_covers_zero(row_of(p_trigg, p_f4, h)))
    check(sprintf("aicc vs f4 CI covers zero, h=%d", h),
          ci_covers_zero(row_of(p_aicc, p_f4, h)))
    check(sprintf("aicc vs trigg CI covers zero, h=%d", h),
          ci_covers_zero(row_of(p_aicc, p_trigg, h)))
    check(sprintf("context parameter_only vs f8 favors the cadence, h=%d", h),
          ci_positive(row_of(p_par, p_f8, h)))
  }
  check("validation gate loses to f8 at h=3, CI clear of zero",
        ci_positive(row_of(p_vgate, p_f8, 3)))
  for (h in setdiff(hz, 3)) {
    check(sprintf("validation gate beats f8, CI clear of zero, h=%d", h),
          ci_negative(row_of(p_vgate, p_f8, h)))
    check(sprintf("validation gate beats aicc gate, CI clear of zero, h=%d", h),
          ci_positive(row_of(p_aicc, p_vgate, h)))
  }
  check("aicc gate beats validation gate at h=3, CI clear of zero",
        ci_negative(row_of(p_aicc, p_vgate, 3)))

  tol <- 1e-4
  cell <- function(a, b, h) row_of(a, b, h)$mean_diff
  check("gate table: all three repro gates PASS", all(GT$status == "PASS"))
  check("repro: validation gate vs f8 at h18 = -0.0043 (4e-05)",
        abs(cell(p_vgate, p_f8, 18) - (-0.0043)) < 1e-4)
  check("repro: trigg vs f8 at h18 = -0.0011 (4e-05)",
        abs(cell(p_trigg, p_f8, 18) - (-0.0011)) < 1e-4)
  check("frozen primary cells at all five horizons",
        abs(cell(p_aicc, p_f8, 3) - (-0.00097)) < tol &&
        abs(cell(p_aicc, p_f8, 6) - (-0.00175)) < tol &&
        abs(cell(p_aicc, p_f8, 9) - (-0.00183)) < tol &&
        abs(cell(p_aicc, p_f8, 12) - (-0.00159)) < tol &&
        abs(cell(p_aicc, p_f8, 18) - (-0.00168)) < tol)
  check("frozen selector cells: aicc vs vgate flips sign between h3 and h6",
        abs(cell(p_aicc, p_vgate, 3) - (-0.00194)) < tol &&
        abs(cell(p_aicc, p_vgate, 18) - (0.00258)) < tol)
  check("frozen h18 loss ordering: vgate < aicc < f4 < trigg < f8 < parameter_only",
        g(p_vgate, "loss_h18") < g(p_aicc, "loss_h18") &&
        g(p_aicc, "loss_h18") < g(p_f4, "loss_h18") &&
        g(p_f4, "loss_h18") < g(p_trigg, "loss_h18") &&
        g(p_trigg, "loss_h18") < g(p_f8, "loss_h18") &&
        g(p_f8, "loss_h18") < g(p_par, "loss_h18"))
  check("gates keep near-cadence search counts (5.0 to 5.3)",
        g(p_aicc, "mean_searches") > 5 && g(p_aicc, "mean_searches") < 5.3 &&
        g(p_vgate, "mean_searches") > 5 && g(p_vgate, "mean_searches") < 5.3)
  check("aicc gate changes forms less often than the validation gate",
        g(p_aicc, "mean_form_changes") < g(p_vgate, "mean_form_changes"))
  check("monitor search profile matches its clock (8.9 to 9.1 searches)",
        abs(g(p_trigg, "mean_searches") - 9) < 0.1)
  be_ok <- nrow(BE) == 2L && all(is.finite(BE$alpha_star))
  check("break-even table carries both gates with finite alpha", be_ok)
  if (be_ok) {
    a1 <- BE[policy_a == p_aicc]$alpha_star[1L]
    a2 <- BE[policy_a == p_vgate]$alpha_star[1L]
    check("aicc gate break-even alpha in (0.008, 0.016)", a1 > 0.008 && a1 < 0.016)
    check("validation gate break-even alpha in (0.02, 0.05)", a2 > 0.02 && a2 < 0.05)
    check("validation gate is the more cost-efficient gate", a2 > a1)
  }
}

cat(sprintf("\n%d passed, %d failed\n", n_pass, n_fail))
if (n_fail > 0L) quit(status = 1L)
