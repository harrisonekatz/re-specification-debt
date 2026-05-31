# re-specification-debt

R code for experiments on adaptive model-form updating in forecasting systems.

The repository reproduces the M4 monthly empirical illustration used in the paper. It compares several model-maintenance policies for ETS forecasting models:

- full model-form updating at every review period
- fixed-frequency model-form updating
- parameter-only updating under a fixed model form
- capped adaptive updating, where the model form is updated early when a score-gap trigger fires and otherwise updated when it reaches a maximum age

The code is written for interactive R use. The M4 data are downloaded by the setup script and are not stored in the repository.

## Requirements

The scripts use base R plus:

- `data.table`
- `forecast`
- `ggplot2`

Install them from R with:

```r
source(file = "scripts/install_dependencies.R")
```

## Quick start

From the repository root:

```r
source(file = "scripts/install_dependencies.R")
source(file = "R/adaptive_update.R")

# Download and check the M4 monthly data.
download_m4(data_dir = "data", overwrite = FALSE)
check_m4_loader("data")
check_ets_smoke(data_dir = "data", train_length = 36L, seed = 123L)

# Small test run.
source(file = "scripts/run_smoke_m4.R")
```

If the smoke run completes, run the calibration and main experiments:

```r
source(file = "scripts/run_m4_100_fixed_grid.R")
source(file = "scripts/run_m4_100_capped.R")
source(file = "scripts/run_m4_500_capped.R")
```

The 500-series run is the main experiment. It can take several hours depending on hardware and the number of parallel workers configured in the script.

## Main outputs

Each run writes a folder under `outputs/`. The main run writes to:

```text
outputs/m4_500_capped/
```

Important files:

| File | Contents |
|---|---|
| `records.csv` | Origin-level forecasts, losses, actions, score gaps, and diagnostics |
| `summary.csv` | Policy-level accuracy, runtime, instability, and re-specification counts |
| `diagnostics.csv` | Model-form diversity and monitoring diagnostics by policy |
| `spec_debt_bridge.csv` | Monitoring-origin score gaps and IC-weight specification-debt diagnostics |
| `spec_debt_bridge_summary.csv` | Summary of the score-gap/specification-debt relationship |
| `triggered_subset.csv` | Comparison of the capped adaptive policy with `fixed_f8` on triggered and non-triggered series |
| `frontier.png` | Cost-accuracy frontier |
| `update_counts.png` | Mean model-form updates per series |
| `spec_debt_bridge.png` | Score-gap trigger versus IC-weight specification debt |
| `triggered_series_effects.png` | Loss differences on triggered versus non-triggered series |

Readable versions of the main figures can be regenerated with:

```r
source(file = "scripts/make_readable_figures.R")
```

## Diagnostics

The capped adaptive policies use a rolling validation score gap as the operational trigger. The score gap is:

```text
validation loss of deployed form - validation loss of best challenger
```

A positive score gap means the challenger did better on the monitoring window. The threshold column, `tau_cost_ratio`, is the score-unit trigger threshold used by the adaptive policy.

The code also computes closed-grid IC-weight diagnostics for the ETS candidate set:

```text
spec_debt_aicc = -log(AICc weight of the deployed form)
spec_debt_bic  = -log(BIC weight of the deployed form)
```

These diagnostics are computed on the full current training window at monitoring origins. Validation-split versions are retained in columns ending in `_validation`.

`fit_seconds` reports the operational runtime of the policy. The IC-weight diagnostics are audit quantities and their runtime is stored separately in `ic_diagnostic_seconds`.

## Repository layout

```text
R/
  adaptive_update.R          # core implementation

scripts/
  install_dependencies.R     # install required packages
  download_m4.R              # download M4 monthly data
  check_setup.R              # data and ETS smoke checks
  run_smoke_m4.R             # small end-to-end run
  run_m4_100_fixed_grid.R    # fixed-frequency calibration
  run_m4_100_capped.R        # capped adaptive calibration
  run_m4_500_capped.R        # main experiment
  run_m4_experiment.R        # configurable experiment runner
  summarize_existing.R       # recompute summaries from records.csv
  make_readable_figures.R    # regenerate manuscript-friendly figures
```

## Notes

- The experiments use the M4 monthly train and test files together, then apply the rolling-origin eligibility filter.
- The main evaluation uses 36 rolling-origin rounds, horizon 3, seasonal period 12, and a fixed training window of 36 observations.
- The implementation is an empirical illustration of adaptive model-form updating. It is not a full reproduction of all update scenarios in Spiliotis and Petropoulos (2024).
