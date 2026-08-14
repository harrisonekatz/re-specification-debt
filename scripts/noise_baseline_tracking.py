#!/usr/bin/env python3
"""
noise_baseline_tracking.py — the firing-rate accounting behind Section 4.5.

Runs the capped Trigg policy on pure iid Gaussian noise, with no
misspecification anywhere and no model fitting, to establish how much of the
monitor's observed firing on the real panel its own initialization supplies.

Two seedings:

  error   the published configuration. On the first error after a reset both
          smoothers take that error: E = e, M = |e|. This puts weights of
          0.64, 0.16, 0.20 on the first three errors in numerator and
          denominator alike, so three same-signed errors give |T| = 1
          whatever their magnitudes.

  resid   a neutral initialization. E starts at zero and M at an independent
          scale estimate, here E|e| for the noise distribution, standing in
          for the deployed form's fitting-period mean absolute residual.

Everything else matches the battery: alpha 0.2, three-error warmup, cap 8,
36 rolling origins, full state reset on any re-specification.

The REFERENCE block below is what the paper reports. --check re-derives those
figures and fails loudly if this implementation has drifted from them.

Usage:
    python3 scripts/noise_baseline_tracking.py --check
    python3 scripts/noise_baseline_tracking.py --n-series 200000 --seed 7
"""

import argparse
import numpy as np

ALPHA, WARMUP, CAP, N_ORIGINS = 0.2, 3, 8, 35

# Two conventions, recovered by matching this simulation to the figures in
# REFERENCE below and then CONFIRMED against R/priority1_policies.R:
#   N_ORIGINS is 35, not 36. run_trigg_capped folds the previous origin's error
#   only when idx >= 2L, so 36 rounds yield 35 usable errors.
#   When the cap and the signal would both act at the same origin, the event is
#   attributed to the cap: run_trigg_capped tests due_to_age before check_signal.
#   Total searches are unaffected; the split between fires and cap-driven
#   re-selections is not.

# What Section 4.5 states, and what this script has to reproduce.
REFERENCE = {
    "first_check_fire_prob": {0.6: 0.61, 0.5: 0.69, 0.4: 0.76},
    "searches_error_seeded": {0.6: 8.4, 0.5: 9.3, 0.4: 10.2},
    "fires_error_seeded": {0.6: 5.7},
    "searches_resid_seeded": {0.6: 5.35},
    "fires_resid_seeded": {0.6: 1.15},
    # real panel, for orientation: 8.99 searches, 6.65 fires, cap contributes 1.34
}
# For orientation only, not re-derived here: the real panel reports 8.99
# searches and 6.65 fires per series, and Table S1 gives 8.97, 9.74, 10.48
# searches across the three limits.


def run(limit, seeding, n_series, rng):
    """Mean searches and fires per series under pure noise."""
    e = rng.standard_normal((n_series, N_ORIGINS))
    m0 = np.sqrt(2.0 / np.pi)  # E|e| for a standard normal

    E = np.zeros(n_series)
    M = np.full(n_series, m0 if seeding == "resid" else 0.0)
    fresh = np.ones(n_series, dtype=bool)      # no error seen since last reset
    since = np.zeros(n_series, dtype=int)      # errors since last reset
    age = np.zeros(n_series, dtype=int)        # origins since last re-spec
    searches = np.ones(n_series)               # initialization counts as one
    fires = np.zeros(n_series)

    for t in range(N_ORIGINS):
        et = e[:, t]
        if seeding == "error":
            seed = fresh
            E = np.where(seed, et, ALPHA * et + (1 - ALPHA) * E)
            M = np.where(seed, np.abs(et), ALPHA * np.abs(et) + (1 - ALPHA) * M)
        else:
            E = ALPHA * et + (1 - ALPHA) * E
            M = ALPHA * np.abs(et) + (1 - ALPHA) * M
        fresh = np.zeros(n_series, dtype=bool)
        since += 1
        age += 1

        with np.errstate(divide="ignore", invalid="ignore"):
            T = np.where(M > 0, E / M, 0.0)
        signalled = (since >= WARMUP) & (np.abs(T) >= limit)
        capped = age >= CAP
        fired = signalled & ~capped

        searches += fired + capped
        fires += fired

        reset = fired | capped
        E = np.where(reset, 0.0, E)
        M = np.where(reset, m0 if seeding == "resid" else 0.0, M)
        fresh |= reset
        since = np.where(reset, 0, since)
        age = np.where(reset, 0, age)

    return searches.mean(), fires.mean()


def first_check_fire_prob(limit, n, rng):
    """P(|T| >= limit) at the first eligible check under the published seeding."""
    e = rng.standard_normal((n, 3))
    w = np.array([0.64, 0.16, 0.20])
    T = (e * w).sum(1) / (np.abs(e) * w).sum(1)
    return float((np.abs(T) >= limit).mean())


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--n-series", type=int, default=200_000)
    ap.add_argument("--seed", type=int, default=20260727)
    ap.add_argument("--check", action="store_true",
                    help="re-derive the figures reported in Section 4.5 and exit "
                         "nonzero if any has drifted")
    args = ap.parse_args()

    rng = np.random.default_rng(args.seed)
    fails = []

    print(f"Pure-noise baseline, {args.n_series:,} series, {N_ORIGINS} origins, "
          f"alpha {ALPHA}, warmup {WARMUP}, cap {CAP}\n")

    print("First eligible check, published seeding")
    for lim in (0.6, 0.5, 0.4):
        p = first_check_fire_prob(lim, args.n_series, rng)
        ref = REFERENCE["first_check_fire_prob"][lim]
        ok = abs(p - ref) < 0.01
        fails += [] if ok else [f"first-check p at limit {lim}: {p:.3f} vs {ref}"]
        print(f"  limit {lim}   P(fire) {p:.3f}   paper {ref}   {'ok' if ok else 'DRIFT'}")

    for seeding, key_s, key_f in (("error", "searches_error_seeded", "fires_error_seeded"),
                                  ("resid", "searches_resid_seeded", "fires_resid_seeded")):
        print(f"\nFull policy on noise, {seeding} seeding")
        for lim in (0.6, 0.5, 0.4):
            s, f = run(lim, seeding, args.n_series, rng)
            ref_s = REFERENCE[key_s].get(lim)
            ref_f = REFERENCE[key_f].get(lim)
            note = ""
            if ref_s is not None:
                ok = abs(s - ref_s) < 0.1
                fails += [] if ok else [f"{seeding} searches at {lim}: {s:.2f} vs {ref_s}"]
                note += f"  paper {ref_s}  {'ok' if ok else 'DRIFT'}"
            if ref_f is not None:
                okf = abs(f - ref_f) < 0.1
                fails += [] if okf else [f"{seeding} fires at {lim}: {f:.2f} vs {ref_f}"]
                note += f" | fires paper {ref_f} {'ok' if okf else 'DRIFT'}"
            print(f"  limit {lim}   searches {s:5.2f}   fires {f:5.2f}{note}")

    if args.check:
        print()
        if fails:
            print(f"{len(fails)} figure(s) drifted from Section 4.5:")
            for x in fails:
                print("   ", x)
            return 1
        print("Every figure reported in Section 4.5 reproduced.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
