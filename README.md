# Adaptive model-form updating: R code only

This stripped-down package contains only the R code and scripts needed to rerun the empirical illustration.
It does not include the manuscript, TeX source, generated results, or raw M4 data.

## Interactive R workflow

From the package root:

```r
source(file = "scripts/install_dependencies.R")
source(file = "R/adaptive_update.R")

download_m4(data_dir = "data", overwrite = FALSE)
check_m4_loader("data")
check_ets_smoke(data_dir = "data", train_length = 36L, seed = 123L)

source(file = "scripts/run_smoke_m4.R")
source(file = "scripts/run_m4_100_fixed_grid.R")
source(file = "scripts/run_m4_100_capped.R")
source(file = "scripts/run_m4_500_capped.R")
```

## New diagnostics added in this version

The capped-adaptive runs now write additional files that directly address the specification-debt / score-gap bridge:

```text
spec_debt_bridge.csv
spec_debt_bridge_summary.csv
triggered_subset.csv
spec_debt_bridge.png
triggered_series_effects.png
```

The main `records.csv` also includes additional columns:

```text
tau_cost_ratio
trigger_reason
triggered_by_score
triggered_by_cap
age_before_action
current_validation_loss
challenger_validation_loss
challenger_form
deployed_weight_aicc
spec_debt_aicc
best_ic_form_aicc
best_weight_aicc
deployed_weight_bic
spec_debt_bic
best_ic_form_bic
best_weight_bic
```

Interpretation:

- `spec_debt_aicc = -log(AICc weight of the deployed form)` at monitoring origins.
- `spec_debt_bic = -log(BIC weight of the deployed form)` at monitoring origins.
- `score_gap` is the rolling validation loss of the deployed form minus the validation loss of the best challenger.
- `tau_cost_ratio` is the empirical score-unit threshold corresponding to `c_R / (K c_S)` in the paper.
- `triggered_subset.csv` compares the main capped adaptive policy against `fixed_f8` on series with and without early score-triggered updates.

## Regenerating figures

After `outputs/m4_500_capped` exists:

```r
source(file = "scripts/make_readable_figures.R")
```

The standard output plots are still written to each output directory by `run_m4_experiment()`.
