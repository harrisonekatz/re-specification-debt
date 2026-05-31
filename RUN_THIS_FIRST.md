# Run this first

Use this package from inside interactive R. Start in the package root.

```r
source(file = "scripts/install_dependencies.R")
source(file = "R/adaptive_update.R")

download_m4(data_dir = "data", overwrite = FALSE)
check_m4_loader("data")
check_ets_smoke(data_dir = "data", train_length = 36L, seed = 123L)
```

Then run the main experiments:

```r
source(file = "scripts/run_smoke_m4.R")
source(file = "scripts/run_m4_100_fixed_grid.R")
source(file = "scripts/run_m4_100_capped.R")
source(file = "scripts/run_m4_500_capped.R")
```

The most important output directory is:

```text
outputs/m4_500_capped/
```

In addition to the previous `records.csv`, `summary.csv`, and `diagnostics.csv`, this version writes:

```text
spec_debt_bridge.csv
spec_debt_bridge_summary.csv
triggered_subset.csv
spec_debt_bridge.png
triggered_series_effects.png
```

These files let you show that the score-gap trigger is being checked against an information-criterion version of specification debt in the finite ETS grid, and that the adaptive exceptions can be compared directly against the closest fixed schedule.
