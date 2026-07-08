# re-specification-debt

R code and results for the paper "When Should Forecasting Models Be
Re-Specified? A Cost-Sensitive Trigger for Adaptive Model-Form Updating"
(arXiv:2606.06670).

The repository runs the full empirical evaluation on all 47,982 monthly M4
series and the simulation experiments reported in Section 8. It compares
model-maintenance policies for ETS forecasting models:

- full model-form updating at every review period
- fixed-frequency model-form updating
- parameter-only updating under a fixed model form
- capped adaptive updating, where the model form is re-specified early when
  a score-gap trigger fires and otherwise when it reaches a maximum age

The M4 data are downloaded by the setup functions and are not stored in the
repository. Origin-level records from the full run are large and are not
committed; the summary outputs that every table and figure in the paper
rests on are committed, so the analysis and verification steps run on a
fresh clone without re-running the experiments.

## Requirements

Base R plus `data.table`, `forecast`, and `ggplot2`:

```r
source(file = "scripts/install_dependencies.R")
```

Python 3 with matplotlib is needed only to regenerate Figure 3
(`scripts/gen_fig3.py`); the compiled figure PDFs are committed.

## Quick start

From the repository root:

```r
source(file = "scripts/install_dependencies.R")
source(file = "R/adaptive_update.R")

download_m4(data_dir = "data", overwrite = FALSE)
check_m4_loader("data")
check_ets_smoke(data_dir = "data", train_length = 36L, seed = 123L)

source(file = "scripts/run_smoke_m4.R")
```

## Verify the paper's numbers

Every number printed in the paper is re-derived from the committed summary
outputs by one script:

```
Rscript scripts/verify_paper_numbers.R
```

It runs about 190 checks (all Table 2 cells, the Section 8.4 inference,
every Table 3 cell including the sign-test columns, the horizon sweep,
both Table 4 blocks, the Figure 3 bins, the cost-weight winner grid, the
fallback disclosure, and the shock, timing, and held-form tables of
Appendix C) and prints PASS or FAIL per claim. On a fresh clone this runs without any experiment.

## Reproduce the experiments

The runs behind the paper, in order. The full benchmark is multi-day on a
single machine; run it in tmux and set the worker count with `--n-jobs`.

```
# 1. Full 47,982-series benchmark at horizon three (Table 2, Figure 1,
#    Sections 8.1 and 8.3), then summaries and the origin-level paired split.
Rscript scripts/run_m4_full_capped.R --n-jobs 8
Rscript scripts/summarize_streaming.R
Rscript scripts/paired_diagnostics.R

# 2. Horizon sweep on the fixed 4,000-series subsample (Figure 2, Section 8.4).
Rscript scripts/run_horizon_sweep.R --n-jobs 8

# 3. Series-level inference for Section 8.4 and Table 3.
Rscript scripts/series_level_inference.R

# 4. Triggered-origin bridge with cluster bootstrap intervals
#    (Figure 3 and the slope sentence in Section 8.5).
Rscript scripts/bridge_uncertainty.R

# 5. Shock battery behind the regime sentence in Section 8.3.
Rscript scripts/simulate_shock_series.R
Rscript scripts/run_shock_experiment.R --n-jobs 8

# 6. Timing isolation behind the jitter sentence in Section 8.4.
Rscript scripts/simulate_timing_isolation.R
Rscript scripts/run_timing_isolation.R --n-jobs 8

# 7. Drift regime and the frozen-form divergence in Table 4 and Section 8.5.
Rscript scripts/simulate_drift_regime.R
Rscript scripts/run_heldform_debt.R --n-jobs 8
Rscript scripts/analyze_heldform_divergence.R

# Optional: fit and MASE-denominator disclosure counts for Section 8.1.
Rscript scripts/disclosure_counts.R
```

## Committed outputs

The paper's tables and figures read from these files, committed at their
canonical paths (see `.gitignore` for the whitelist):

```
outputs/m4_full_capped/summary.csv                    Table 2, Figure 1
outputs/m4_full_capped/fallback_share.csv             Section 8.1 disclosure
outputs/m4_full_capped/paired_diagnostics.csv         Section 8.4
outputs/m4_full_capped/triggered_subset.csv           Section 8.4
outputs/m4_full_capped/spec_debt_bridge_summary.csv   Table 4, first block
outputs/horizon_sweep.csv                             Figure 2, Section 8.4
outputs/series_level_inference.csv                    Table 3, Section 8.4
outputs/bridge_bins_ci.csv                            Figure 3
outputs/bridge_regression.csv                         Section 8.5
outputs/m4_shock_experiment/shock_winner_by_cell.csv  Section 8.3, Appendix C.1
outputs/m4_shock_experiment/shock_matched_comparison.csv
outputs/m4_shock_experiment/shock_fire_timing.csv
outputs/m4_timing_isolation/timing_matched_by_jitter.csv  Appendix C.2
outputs/m4_timing_isolation/timing_winsum.csv
outputs/m4_timing_isolation/timing_firetime.csv
outputs/m4_drift_experiment/heldform_debt_divergence.csv  Table 4, Appendix C.3
```

`RESULTS_MANIFEST.md` maps every claim in the paper to its run, file, and
script.

## Citation

```bibtex
@misc{katz2026respecify,
  title  = {When Should Forecasting Models Be Re-Specified?
            A Cost-Sensitive Trigger for Adaptive Model-Form Updating},
  author = {Harrison Katz},
  year   = {2026},
  note   = {arXiv:2606.06670}
}
```
