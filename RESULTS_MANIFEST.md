# RESULTS MANIFEST: "When Should Forecasting Models Be Re-Specified?"

Maps every number, table, and figure in paper.tex (July 2 final) to the run,
output file, and script that produced it. Compiled from the session record
(June 22, 25, 29, 30, July 2) and verified against the output files.

Paths are relative to the project root:
~/helicon/adaptive_model_form_updating_R_clean/

---

## 1. Claim-to-source map

### Table 2 (tab:m4-frontier) and Figure 1
- Claims: all 9 policy rows; 0.87 percent parameter-only gap; fixed_f8 at
  0.06 percent and 16.7 percent compute; fixed cadences within a quarter of
  a percent; 3.4 percent loss at the lowest thresholds; 47,982 series;
  1,727,352 records per policy.
- Run: full M4 monthly benchmark, h=3.
- Scripts: scripts/run_m4_full_capped.R (experiment),
  scripts/summarize_streaming.R (summaries from the parted records).
- Files: outputs/m4_full_capped/summary.csv (Table 2, Fig 1),
  outputs/m4_full_capped/fallback_share.csv (the 216-276 fallback
  disclosure in 8.1).

### Section 8.4, first paragraph (benchmark comparison)
- Claims: 91.1 / 8.9 percent agreement split; per-series mean +0.0010,
  series t 4.9, bootstrap CI [+0.0006, +0.0014]; 54.8 percent of 12,459
  differing series favor the cadence; 0.714 vs 0.687 mean MASE at fired
  origins.
- Scripts: scripts/paired_diagnostics.R (agreement split, origin-level
  diagnostics), scripts/series_level_inference.R (series-level test, NEW),
  scripts/summarize_streaming.R (triggered subset).
- Files: outputs/m4_full_capped/paired_diagnostics.csv,
  outputs/m4_full_capped/triggered_subset.csv,
  outputs/series_level_inference.csv.

### Section 8.4, horizon sweep, Figure 2, Table 3
- Claims: parameter-only 1.015 / 1.015 / 1.016 / 1.013 / 1.019; matched gap
  +0.0002 at h3 then -0.0057 / -0.0066 / -0.0051 / -0.0038; Table 3
  series-level t, CI, and sign share at h6-h18.
- Scripts: scripts/run_horizon_sweep.R (runs),
  scripts/series_level_inference.R (Table 3 inference).
- Files: outputs/horizon_sweep.csv (Fig 2, prose values),
  outputs/m4_horizon_h06 ... h18 (per-horizon records),
  outputs/series_level_inference.csv (Table 3).

### Section 8.3, regime sentence (simulated)
- Claim: abrupt persistent shift favors full re-fitting even against a
  fast-firing trigger; outliers and transient bursts favor rarely-firing
  policies, restraint over detection.
- Scripts: scripts/simulate_shock_series.R, scripts/run_shock_experiment.R.
- Files: outputs/m4_shock_experiment/shock_winner_by_cell.csv (cell
  winners: level_shift to full_update; additive_outlier to pure adaptives
  at 1.2-2.1 respecs per series; transient_burst to parameter_only),
  shock_matched_comparison.csv, shock_fire_timing.csv, plus the run
  summary.csv (respecification counts that ground "rarely-firing").

### Section 8.4, timing-jitter sentence (simulated)
- Claim: matched gap slides from zero under scheduled change to a trigger
  advantage as change is jittered off schedule, crossover near half the
  update interval.
- Scripts: scripts/simulate_timing_isolation.R,
  scripts/run_timing_isolation.R.
- Files: outputs/m4_timing_isolation/timing_matched_by_jitter.csv (the
  table in Appendix C.2), timing_winsum.csv, timing_firetime.csv.

### Table 4 (tab:spec-debt-bridge) and Section 8.5 prose
- First block (Spearman 0.008 AICc, 0.075 BIC, range 0.003 to 0.16):
  outputs/m4_full_capped/spec_debt_bridge_summary.csv, produced by
  scripts/summarize_streaming.R.
- Second block (frozen-form drift cells: 0.34 score gap vs 0.03 IC debt,
  sign flips with noise): scripts/simulate_drift_regime.R,
  scripts/run_heldform_debt.R, scripts/analyze_heldform_divergence.R;
  file outputs/m4_drift_experiment/heldform_debt_divergence.csv.

### Figure 3 (binned trigger outcomes with intervals)
- Claims: score-gap bins monotone from +0.057 to -0.013 with only the
  largest-gap bin at break-even; IC-debt bins ragged; IC slope mildly
  positive (wrong-signed).
- Script: scripts/bridge_uncertainty.R (NEW).
- Files: outputs/bridge_bins_ci.csv (the plotted bins and intervals),
  outputs/bridge_regression.csv (the slope sentence in 8.5).
- Renderer: gen_fig3.py (bundled) writes
  figures/figure_spec_debt_bridge.pdf.

---

## 2. Repo upload list

Required for the paper as written (Appendix B names the starred ones):

    R/adaptive_update.R                    core implementation *
    scripts/download_m4.R
    scripts/install_dependencies.R
    scripts/check_setup.R
    scripts/run_m4_full_capped.R           full 48k benchmark
    scripts/summarize_streaming.R          summaries, bridge, triggered subset
    scripts/paired_diagnostics.R           agreement split
    scripts/run_horizon_sweep.R            h3-h18 subsample runs
    scripts/series_level_inference.R       Table 3 + 8.4 inference * (NEW)
    scripts/bridge_uncertainty.R           Figure 3 + slopes * (NEW)
    scripts/simulate_shock_series.R        regime sentence
    scripts/run_shock_experiment.R
    scripts/simulate_timing_isolation.R    jitter sentence
    scripts/run_timing_isolation.R
    scripts/simulate_drift_regime.R        frozen-form block
    scripts/run_heldform_debt.R
    scripts/analyze_heldform_divergence.R
    scripts/verify_paper_numbers.R         the verifier (NEW)

Plus the summary-level outputs so a reader can verify without the multi-day
rerun: summary.csv, fallback_share.csv, paired_diagnostics.csv,
triggered_subset.csv, spec_debt_bridge_summary.csv, horizon_sweep.csv,
series_level_inference.csv, bridge_bins_ci.csv, bridge_regression.csv,
shock_winner_by_cell.csv, shock_matched_comparison.csv,
shock_fire_timing.csv, timing_matched_by_jitter.csv, timing_winsum.csv,
timing_firetime.csv, heldform_debt_divergence.csv. Do NOT push by_policy/
or parts/ (tens of GB).

Not required by the paper (iterations and side experiments, safe to omit):
run_m4_100_capped.R, run_m4_100_fixed_grid.R, run_m4_500_capped.R,
run_score_gap_variant.R, run_window_sweep.R, run_monitor_sweep.R,
run_ic_boundary_sweep.R, simulate_ic_boundary.R, simulate_break_series.R,
run_break_experiment.R, decompose_break_cells.R, run_drift_experiment.R,
analyze_drift_debt.R, salvage_debt.R, diagnose_trigger.R, eda_winners.R,
segment_by_instability.R, summarize_existing.R, run_smoke_m4.R,
make_readable_figures.R. (Break and drift-grid runs support the regime
story redundantly; keep them if you want the fuller record.)

README: update the 500-series description to the full run, list the
required scripts above, and point summary outputs at the files listed.

---

## 3. Your to-do list

Before sending to Titus:
1. Copy the new figures/figure_spec_debt_bridge.pdf over your old one
   (Figures 1 and 2 are unchanged) and recompile paper.tex. Expect 29 pages.
2. Run: Rscript scripts/verify_paper_numbers.R from the project root.
   Every line should print PASS. Send me any FAIL.
3. Bibliography fixes in your server references.bib (bibtex is downcasing):
   - Makridakis 2020: "The {M4} competition"
   - Makridakis 2022b: "The {M5} competition: Background..."
   - Makridakis 2022a: protect "{M5}" for safety
   - Petropoulos 2025: "Wielding {O}ccam's razor"
   - Yao 2018: "average {B}ayesian predictive distributions"
   - Gardner 2006: "part {II}"
   - Yardley 2021 title: "utility and cost of the forecasts" (add "the")

When you choose (not blockers for Titus):
4. Push the repo per Section 2 and update the README.
5. Post the recompiled PDF to arXiv as v2 (v1 is the 500-series draft).
6. Optional 8.1 addition: run disclosure_counts.R Part 2 with m4_train_csv
   pointed at your real Monthly-train.csv for the zero-denominator count.

Before journal submission (remaining):
7. Landed in the July 3 revision, no action needed: Appendix C documents
   the shock, timing, and held-form designs with results tables, and
   Section 8.2 carries the trigger timeline with the selected-comparison
   disclosure, the exact ETS grid, and the edge-case rules.
8. Optional, your compute: a tau-by-cap sensitivity sweep at horizons 6 to
   18 if you want the strongest possible answer on threshold choice.
