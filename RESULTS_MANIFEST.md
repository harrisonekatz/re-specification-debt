# Results manifest

Companion to "When Should Forecasting Models Be Re-Specified? Search Rate,
Detection, and Replacement Selection." This file describes **what the runs
were and which of them count**. It does not map individual claims to files;
`docs/PROVENANCE.md` does that, and it is generated rather than written, so it
cannot drift out of step with the code the way a hand-written map does.

Paths are relative to the repository root.

---

## Division of labour

| Question | Answer lives in |
|---|---|
| Which file produced this table? | `docs/PROVENANCE.md` (generated) |
| Do the committed numbers still reproduce the paper? | `scripts/verify_priority1_numbers.R`, `scripts/verify_paper_numbers.R` |
| Is every file the paper cites actually here? | `scripts/provenance_map.py` (exits nonzero if not) |
| Which runs happened, and which are dead? | this file |

Run all three from a fresh clone before any submission or release.

---

## Environment

Every run in this repository: single-socket Intel Xeon Platinum 8375C at
2.90 GHz, 32 physical cores, 64 hardware threads, 247 GB memory, Linux,
R 4.5.1, `forecast` 9.0.2, `data.table` 1.18.4.

The full-scale batteries ran 32 worker processes over series-level batches, one
per physical core. The timing stratum ran 8. That difference matters for the
wall-time columns and is stated in the paper.

---

## The horizon-eighteen layer

The analyses behind Table 2, Table 4, Figure 1, and the two break-even
constants come from three runs. They are not interchangeable.

| Directory | Produced | Holds |
|---|---|---|
| `outputs/m4_priority1/` | 2026-07-26 | **canonical** contrasts, policy summaries, reproduction gates, break-even constants, lead-level differences. 38,134 series, `n_jobs: 32` |
| `outputs/m4_priority1_trigg_v2/` | 2026-07-27 | configuration of record for the tracking policy. 38,134 series, `n_jobs: 32`, `trigg_warmup: 3` |
| `outputs/m4_priority1_timing_v2/` | 2026-07-27 | time and instability denominators only. 2,000-series length-stratified subsample, `n_jobs: 8`, single session |

**The one thing to know.** `outputs/m4_priority1/contrasts_priority1.csv` is
canonical for every policy including the tracking signal: the tracking rows
were spliced in from the 07-27 rerun through the `--splice-dir` path in
`priority1_inference.R`. The `run_config.txt` sitting beside those rows
describes only the original 07-26 launch and records `trigg_warmup: 1`. It does
not describe the tracking rows in that file. For the tracking policy's actual
configuration, at the three-error warmup the paper specifies, read
`outputs/m4_priority1_trigg_v2/run_config.txt`.

**Table 4 mixes two runs, by design.** Loss, searches, fired, and changes come
from the full 38,134-series battery. Relative time and relative instability come
from the timing stratum, run in one session on one node so every ratio shares
identical conditions. The table caption states this. The two break-even
constants, 0.012 and 0.031, are anchored on the stratum.

---

## The seeding layer

Section 4.5, Table 3, and Figure 2.

| Directory | Holds | Status |
|---|---|---|
| `outputs/m4_p1_resid_0.3/`, `_0.5/` | `run_config.txt` only; origin-level parts are regenerated, not committed | runner output, configuration of record |
| `outputs/m4_p1_seed_0.3/` | spliced inference at control limit 0.3 | **canonical**: Table 3 rate-matched block, Figure 2 tau-0.3 point |
| `outputs/m4_p1_seed_0.5b/` | spliced inference at control limit 0.5 | **canonical**: Table 3 selective block, Figure 2 tau-0.5 point |
| `outputs/m4_p1_seed_0.3b/`, `outputs/m4_p1_seed_0.5/` | identical point estimates, different bootstrap draw | superseded, not cited, not committed |
| `outputs/m4_p1_splice_0.3/`, `_0.5/` | staging parts | intermediate, not committed |

**The two blocks of Table 3 were printed from different inference passes**,
which is why the canonical directory is `_0.3` at one control limit and `_0.5b`
at the other. Each pair carries identical means and t statistics and differs
only in the percentile bootstrap intervals.

- tau 0.3: `m4_p1_seed_0.3` reproduces all five confidence bounds. `_0.3b`
  gives a horizon-nine lower bound of -6.95e-06 where the table prints a
  positive zero.
- tau 0.5: `m4_p1_seed_0.5b` reproduces horizon nine at [-0.0014, 0.0002].
  `m4_p1_seed_0.5` gives [-0.0013, 0.0001] there.

Two cells reproduced from neither pass and were corrected in the manuscript
rather than the data: the horizon-12 `series_t` in the tau-0.3 block is
0.851485, printed as 0.8 and corrected to 0.9; and the horizon-3 upper bound in
the tau-0.5 block is 5.6e-05 or 5.1e-05 depending on pass, printed as 0.0000
and corrected to 0.0001.

**The `run_config.txt` the inference stage writes into a seed directory is
inherited from the July base battery and describes neither seeding run.** It is
excluded from the commit for that reason. The configuration of record for each
control limit is in the matching `m4_p1_resid_*` directory.

`scripts/noise_baseline_tracking.py` establishes the pure-noise firing rates
quoted in Section 4.5 and re-derives them on demand with `--check`. Two
conventions it shares with `run_trigg_capped` in `R/priority1_policies.R`,
neither stated in the manuscript: the one-step error at an origin is folded in
at the following origin, so 36 rounds yield 35 usable errors; and when the age
cap and the signal would both act at the same origin, the event is attributed
to the cap. Total searches do not depend on the second convention. The split
between fired and cap-driven re-selections does.

`scripts/plot_seeding_line.py` produces `figures/figure_seeding_line.pdf`. It
hard-codes its five points from the seed policy summaries, so it runs
standalone.

---

## Other committed outputs

| Directory or file | Supplies |
|---|---|
| `outputs/m4_full_capped/summary.csv` | Table 1, the 47,982-series benchmark at h=3 |
| `outputs/m4_full_capped/equivalence_tost.csv` | Table S5 |
| `outputs/m4_tracking_signal/summary.csv` | Table S1 |
| `outputs/m4_tracking_signal_h03/` through `_h18/` | horizon boards for the monitors |
| `outputs/tracking_pairwise_inference.csv` | Table S2 |
| `outputs/series_level_inference.csv` | Table S3 |
| `outputs/horizon_sweep.csv` | Section 4.4 horizon sweep |
| `outputs/bridge_bins_ci.csv`, `bridge_regression.csv` | Table S6, Figure S2 |
| `outputs/m4_shock_experiment/` | Tables S8, S9 |
| `outputs/m4_timing_isolation/` | Table S10 |
| `outputs/m4_drift_experiment/` | Table S11 |

Origin-level record files are large and are regenerated by the runners rather
than committed. This applies to every `parts/` directory and to
`per_series_priority1.csv` and `per_series_leads.csv`.

---

## Verification

    Rscript scripts/verify_paper_numbers.R
    Rscript scripts/verify_priority1_numbers.R
    python3 scripts/provenance_map.py -o docs/PROVENANCE.md

The second re-derives every cell of Tables 2 and 4, Table S5, the three
reproduction gates, and both break-even constants from the committed summaries:
59 checks. The third checks that every file the paper cites is present and exits
nonzero if any is missing. Commit the `docs/PROVENANCE.md` it writes.

---


When a run is superseded, say so in the table above in the same commit that
supersedes it. Every confusion this file exists to prevent came from a
superseded artifact that was left in place with nothing marking it dead.
