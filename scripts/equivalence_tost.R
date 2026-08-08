# equivalence_tost.R
#
# Practical-equivalence analysis for the claim that sparse fixed cadences
# "match" full model-form updating (review item 5.3). A nonsignificant
# difference does not establish equivalence, so this script runs two
# one-sided tests (TOST) per cadence against full_update on the original
# full-run records: equivalence at level alpha is declared exactly when the
# (1 - 2 alpha) confidence interval for the paired per-series mean
# difference lies inside (-epsilon, +epsilon).
#
# Margin, declared here before the script is run on real records: epsilon is
# 0.5 percent of full updating's mean loss on the panel (--margin-rel 0.005).
# Rationale: the paper's smallest material effect is the staleness cost of
# roughly 1.3 to 1.5 percent, so the margin is about one third of the
# smallest difference the paper treats as economically meaningful. The
# attained margin (the largest absolute bound of the interval, in relative
# terms) is also reported, so the manuscript can state the tighter bound the
# data actually support.
#
# Data: streams parts (or a merged records file) from the original run; only
# series_id, policy, and loss are read, so this takes about a minute on the
# full 47,982-series horizon-3 panel and needs no model fitting.
#
# Usage (repo root):
#   Rscript scripts/equivalence_tost.R --run-dir outputs/m4_full_capped
#
# Flags:
#   --run-dir DIR      run to analyze (default outputs/m4_full_capped)
#   --out DIR          where to write results (default --run-dir)
#   --margin-rel X     epsilon as a fraction of full_update's mean (0.005)
#   --alpha X          TOST level (0.05, so the interval shown is 90 percent)
#   --boot B           bootstrap replicates for a robustness interval (5000)
#   --seed N           bootstrap seed (20260728)
#
# Outputs: equivalence_tost.csv and equivalence_tost.tex under --out.

suppressMessages(source("R/adaptive_update.R"))
suppressMessages(library(data.table))

args <- parse_cli_args()
run_dir <- get_arg(args, "run_dir", default = "outputs/m4_full_capped", type = "character")
out_dir <- get_arg(args, "out", default = run_dir, type = "character")
margin_rel <- get_arg(args, "margin_rel", default = 0.005, type = "numeric")
alpha <- get_arg(args, "alpha", default = 0.05, type = "numeric")
n_boot <- get_arg(args, "boot", default = 5000L, type = "integer")
seed <- get_arg(args, "seed", default = 20260728L, type = "integer")

# Locate records: checkpoint parts first, then a merged file.
files <- sort(list.files(file.path(run_dir, "parts"),
                         pattern = "^records_part_[0-9]+\\.csv$", full.names = TRUE))
if (length(files) == 0L) {
  for (cand in c("records.csv", "records.csv.gz")) {
    p <- file.path(run_dir, cand)
    if (file.exists(p)) { files <- p; break }
  }
}
if (length(files) == 0L) stop("No records found under ", run_dir,
                              " (looked for parts/ and records.csv[.gz]).")
message(length(files), " record file(s) under ", run_dir)

keep_policy <- function(p) p == "full_update" | grepl("^fixed_f[0-9]+$", p)

agg <- rbindlist(lapply(files, function(f) {
  d <- fread(f, select = c("series_id", "policy", "loss"))
  d <- d[keep_policy(policy)]
  d[, .(mean_loss = mean(loss, na.rm = TRUE), n_origins = .N),
    by = .(policy, series_id)]
}))
agg <- agg[, .(mean_loss = weighted.mean(mean_loss, n_origins),
               n_origins = sum(n_origins)), by = .(policy, series_id)]

if (!"full_update" %in% agg$policy) {
  stop("full_update is not in these records; the equivalence question needs it.")
}
FU <- agg[policy == "full_update", .(series_id, full_loss = mean_loss)]
ref_mean <- mean(FU$full_loss, na.rm = TRUE)
eps <- margin_rel * ref_mean
message(sprintf("full_update mean loss %.5f over %d series; epsilon = %.5f (%.2f%%)",
                ref_mean, nrow(FU), eps, 100 * margin_rel))

cadences <- setdiff(unique(agg$policy), "full_update")
cadences <- cadences[order(as.integer(sub("^fixed_f", "", cadences)))]

set.seed(seed)
rows <- list()
for (p in cadences) {
  m <- merge(agg[policy == p, .(series_id, a = mean_loss)], FU, by = "series_id")
  m <- m[is.finite(a) & is.finite(full_loss)]
  d <- m$a - m$full_loss
  n <- length(d)
  if (n < 3L) next
  mn <- mean(d)
  se <- sd(d) / sqrt(n)
  df <- n - 1L
  tcrit <- qt(1 - alpha, df)
  ci_lo <- mn - tcrit * se
  ci_hi <- mn + tcrit * se
  # TOST: H01 mu <= -eps vs mu > -eps; H02 mu >= eps vs mu < eps.
  p1 <- pt((mn + eps) / se, df, lower.tail = FALSE)
  p2 <- pt((mn - eps) / se, df, lower.tail = TRUE)
  tost_p <- max(p1, p2)
  bs <- vapply(seq_len(n_boot), function(b) mean(d[sample.int(n, n, replace = TRUE)]),
               numeric(1L))
  b_lo <- unname(quantile(bs, alpha))
  b_hi <- unname(quantile(bs, 1 - alpha))
  rows[[length(rows) + 1L]] <- data.table(
    policy = p, n_series = n,
    mean_diff = mn, se = se,
    ci90_lo = ci_lo, ci90_hi = ci_hi,
    boot90_lo = b_lo, boot90_hi = b_hi,
    tost_p = tost_p,
    epsilon_abs = eps, epsilon_rel = margin_rel,
    equivalent = (ci_lo > -eps && ci_hi < eps),
    equivalent_boot = (b_lo > -eps && b_hi < eps),
    attained_margin_rel = max(abs(ci_lo), abs(ci_hi)) / ref_mean,
    mean_rel = mn / ref_mean
  )
}
ET <- rbindlist(rows)
fwrite(ET, file.path(out_dir, "equivalence_tost.csv"))

message("\n================ practical equivalence vs full_update ================")
print(ET[, .(policy, n_series,
             mean_diff = round(mean_diff, 5),
             ci90 = sprintf("[%.5f, %.5f]", ci90_lo, ci90_hi),
             tost_p = signif(tost_p, 3),
             equivalent, attained_rel = sprintf("%.3f%%", 100 * attained_margin_rel))])

esc <- function(x) gsub("_", "\\\\_", x)
f5 <- function(x) sprintf("%.5f", x)
tex <- c(
  "% Generated by scripts/equivalence_tost.R; do not edit cells by hand.",
  "\\begin{table}[t]",
  "\\centering",
  "\\small",
  paste0("\\caption{Practical equivalence of fixed cadences to full model-form updating. ",
         "Paired per-series mean differences against full\\_update over ",
         format(max(ET$n_series), big.mark = ","), " series; equivalence at the 0.05 level holds ",
         "when the 90\\% interval lies inside $\\pm\\varepsilon$ with ",
         "$\\varepsilon = ", 100 * margin_rel, "\\%$ of full updating's mean loss, ",
         "about one third of the staleness cost, the smallest effect treated as material. ",
         "TOST $p$ is the larger of the two one-sided $p$-values; the attained bound is the ",
         "largest absolute interval endpoint in relative terms.}"),
  "\\label{tab:equivalence-tost}",
  "\\begin{tabular}{l r c r c r}",
  "\\toprule",
  "Policy & Mean & 90\\% CI & TOST $p$ & Equivalent & Attained \\\\",
  "\\midrule"
)
for (r in seq_len(nrow(ET))) {
  tex <- c(tex, paste0(
    esc(ET$policy[r]), " & ", f5(ET$mean_diff[r]),
    " & $[", f5(ET$ci90_lo[r]), ",\\, ", f5(ET$ci90_hi[r]), "]$",
    " & ", format(signif(ET$tost_p[r], 2)),
    " & ", ifelse(ET$equivalent[r], "yes", "no"),
    " & ", sprintf("%.2f\\%%", 100 * ET$attained_margin_rel[r]), " \\\\"
  ))
}
tex <- c(tex, "\\bottomrule", "\\end{tabular}", "\\end{table}")
writeLines(tex, file.path(out_dir, "equivalence_tost.tex"))
message("Wrote equivalence_tost.csv and equivalence_tost.tex under ", out_dir)
