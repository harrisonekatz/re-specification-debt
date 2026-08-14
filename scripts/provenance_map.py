#!/usr/bin/env python3
"""
provenance_map.py — where every reported number comes from.

Emits docs/PROVENANCE.md: one row per table, figure, and headline constant in
the manuscript, naming the directory and file it was computed from, with the
run configuration that produced it.

The point is that this is a GATE, not a document. Every source path below is
declared once and then checked. If a declared path is missing, the script
exits nonzero and says so, which is what should happen when the paper cites
outputs that were never committed.

Usage (repo root):
    python3 scripts/provenance_map.py                 # check local tree
    python3 scripts/provenance_map.py --remote        # check the public repo
    python3 scripts/provenance_map.py -o docs/PROVENANCE.md

Run it after verify_priority1_numbers.R passes. The verifier proves the
numbers; this proves you can find them.
"""

import argparse
import os
import re
import sys
import urllib.error
import urllib.request

RAW = "https://raw.githubusercontent.com/harrisonekatz/re-specification-debt/main"

# ---------------------------------------------------------------------------
# THE DECLARATION.
#
# object   : how the manuscript refers to it
# supplies : which numbers in that object this source provides
# paths    : files that must exist for the object to be reproducible
# config   : run_config.txt describing the run that produced them, if any
# note     : anything a replicating reader needs in order not to be misled
#
# Adding a table to the paper means adding a row here. If you don't, the row
# is absent from PROVENANCE.md and nobody finds out; that is the one failure
# mode this script cannot catch for you.
# ---------------------------------------------------------------------------

DECLARED = [
    dict(
        object="Table 1, cost-accuracy frontier at h=3",
        supplies="relative loss, time, instability, searches for 9 policies",
        paths=["outputs/m4_full_capped/summary.csv"],
        config=None,
        note="47,982-series benchmark run.",
    ),
    dict(
        object="Table 2, deciding tests",
        supplies="all 25 paired cells, CIs, trimmed means, win shares",
        paths=[
            "outputs/m4_priority1/contrasts_priority1.csv",
            "outputs/m4_priority1/table5_deciding_tests.tex",
        ],
        config="outputs/m4_priority1/run_config.txt",
        note="CANONICAL FOR EVERY POLICY. The tracking rows were spliced in from the "
             "policy-subset rerun, so the run_config.txt in this directory describes "
             "only the original launch. The tracking policy's own configuration is "
             "outputs/m4_priority1_trigg_v2/run_config.txt.",
    ),
    dict(
        object="Table 2, tracking-policy rows: configuration of record",
        supplies="trigg_warmup, trigg_limit, trigg_alpha for the spliced rows",
        paths=[],
        config="outputs/m4_priority1_trigg_v2/run_config.txt",
        note="Config only. The rerun's numbers live in the Table 2 contrasts file above.",
    ),
    dict(
        object="Table 3, seeding robustness",
        supplies="all 10 paired cells for the neutrally reseeded monitor",
        paths=[
            "outputs/m4_p1_seed_0.3/contrasts_priority1.csv",
            "outputs/m4_p1_seed_0.3/policy_summary_priority1.csv",
            "outputs/m4_p1_seed_0.5b/contrasts_priority1.csv",
            "outputs/m4_p1_seed_0.5b/policy_summary_priority1.csv",
        ],
        config="outputs/m4_p1_resid_0.3/run_config.txt",
        note="The two blocks of this table come from different inference passes: "
             "m4_p1_seed_0.3 for the rate-matched block, m4_p1_seed_0.5b for the "
             "selective block. Their siblings m4_p1_seed_0.3b and m4_p1_seed_0.5 carry "
             "identical point estimates under a different bootstrap draw and are NOT "
             "cited. m4_p1_resid_0.3 and _0.5 are the runner outputs, run_config.txt "
             "only. m4_p1_splice_0.3 and _0.5 hold staging parts.",
    ),
    dict(
        object="Table 4, horizon-18 cost frontier",
        supplies="MASE(18), searches, fired, changes",
        paths=[
            "outputs/m4_priority1/policy_summary_priority1.csv",
            "outputs/m4_priority1/table_cost_h18.tex",
        ],
        config="outputs/m4_priority1/run_config.txt",
        note="Loss and intervention columns, 38,134 series.",
    ),
    dict(
        object="Table 4, relative time and instability columns",
        supplies="Rel. time, Rel. instab.",
        paths=["outputs/m4_priority1_timing_v2/timing_summary.csv"],
        config="outputs/m4_priority1_timing_v2/run_config.txt",
        note="DIFFERENT RUN from the loss columns in the same table: 2,000-series "
             "stratum at n_jobs 8, single session. Disclosed in the caption.",
    ),
    dict(
        object="Break-even constants 0.012 and 0.031",
        supplies="alpha_star for both gates",
        paths=["outputs/m4_priority1/cost_breakeven.csv"],
        config=None,
        note="Anchored on the timing stratum, per the anchor column.",
    ),
    dict(
        object="Reproduction gates",
        supplies="the three PASS rows quoted in Section 4.5",
        paths=["outputs/m4_priority1/repro_gates.csv"],
        config=None,
        note=None,
    ),
    dict(
        object="Figure 1, lead profile",
        supplies="lead-level differences and bootstrap ribbons",
        paths=[
            "outputs/m4_priority1/leadwise_diffs.csv",
            "outputs/m4_priority1/leadwise_profile.png",
        ],
        config=None,
        note="Manuscript includes it as figures/figure_lead_profile.png.",
    ),
    dict(
        object="Figure 2, monitors against the clock line",
        supplies="loss against searches at h=18 for 5 configurations",
        paths=["scripts/plot_seeding_line.py", "scripts/noise_baseline_tracking.py"],
        config=None,
        note="Built from the policy summaries of the seeding runs declared for Table 3.",
    ),
    dict(
        object="Table S5, equivalence tests",
        supplies="11 cadences, TOST p, attained bounds",
        paths=["outputs/m4_full_capped/equivalence_tost.csv"],
        config=None,
        note=None,
    ),
    dict(
        object="Table S1, tracking-signal benchmark board",
        supplies="8 policies at h=3, 47,982 series",
        paths=["outputs/m4_tracking_signal/summary.csv"],
        config=None,
        note=None,
    ),
    dict(
        object="Table S2, paired comparisons by horizon",
        supplies="3 pairings across 5 horizons",
        paths=["outputs/tracking_pairwise_inference.csv"],
        config=None,
        note=None,
    ),
    dict(
        object="Table S3, series-level horizon tests",
        supplies="4 horizons, per-series t and sign tests",
        paths=["outputs/series_level_inference.csv"],
        config=None,
        note=None,
    ),
    dict(
        object="Table S6 and Figure S2, specification-debt diagnostics",
        supplies="Spearman correlations and the binned outcome",
        paths=["outputs/bridge_bins_ci.csv", "outputs/bridge_regression.csv"],
        config=None,
        note=None,
    ),
    dict(
        object="Tables S8 and S9, shock battery",
        supplies="winning policy by cell, matched comparisons",
        paths=["outputs/m4_shock_experiment/"],
        config=None,
        note=None,
    ),
    dict(
        object="Table S10, timing isolation",
        supplies="jitter cells",
        paths=["outputs/m4_timing_isolation/"],
        config=None,
        note=None,
    ),
    dict(
        object="Table S11, held-form signal validity",
        supplies="Spearman by drift cell",
        paths=["outputs/m4_drift_experiment/"],
        config=None,
        note=None,
    ),
]

CONFIG_FIELDS = ["timestamp", "eligible_series", "n_jobs", "trigg_warmup", "batch_size"]


# ---------------------------------------------------------------------------

class Tree:
    """Local directory or the public repo, behind one interface."""

    def __init__(self, remote: bool, root: str = "."):
        self.remote, self.root = remote, root

    def exists(self, path: str):
        """True, False, or None when remote mode cannot decide (bare directory)."""
        if not self.remote:
            p = os.path.join(self.root, path)
            return os.path.isdir(p) if path.endswith("/") else os.path.isfile(p)
        if path.endswith("/"):
            return None  # the contents API is rate limited; verify these locally
        req = urllib.request.Request(f"{RAW}/{path}", method="HEAD")
        try:
            with urllib.request.urlopen(req, timeout=25) as r:
                return r.status == 200
        except urllib.error.HTTPError:
            return False
        except Exception:
            return False

    def read(self, path: str):
        try:
            if not self.remote:
                with open(os.path.join(self.root, path), encoding="utf-8") as fh:
                    return fh.read()
            with urllib.request.urlopen(f"{RAW}/{path}", timeout=25) as r:
                return r.read().decode("utf-8")
        except Exception:
            return None


def parse_config(text):
    """Pull the fields worth showing out of a run_config.txt."""
    if not text:
        return {}
    out = {}
    for field in CONFIG_FIELDS:
        m = re.search(rf"{field}:\s*([^\s|]+)", text)
        if m:
            out[field] = m.group(1)
    return out


def main() -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument("--remote", action="store_true",
                    help="check the public GitHub repo instead of the local tree")
    ap.add_argument("--root", default=".")
    ap.add_argument("-o", "--out", default="docs/PROVENANCE.md")
    ap.add_argument("--no-write", action="store_true")
    args = ap.parse_args()

    tree = Tree(args.remote, args.root)
    where = "public repo" if args.remote else os.path.abspath(args.root)

    rows, missing = [], []
    for d in DECLARED:
        status = []
        for p in d["paths"]:
            ok = tree.exists(p)
            status.append((p, ok))
            if ok is False:
                missing.append((d["object"], p))
        cfg = {}
        if d["config"]:
            ok = tree.exists(d["config"])
            if ok is True:
                cfg = parse_config(tree.read(d["config"]))
            elif ok is False:
                missing.append((d["object"], d["config"]))
            status.append((d["config"], ok))
        rows.append((d, status, cfg))

    # ---- report to stdout -------------------------------------------------
    print(f"Provenance check against {where}\n")
    for d, status, cfg in rows:
        bad = [p for p, ok in status if ok is False]
        unk = [p for p, ok in status if ok is None]
        mark = "FAIL" if bad else ("????" if unk else ("OK  " if status else "----"))
        print(f"[{mark}] {d['object']}")
        for p, ok in status:
            sym = "  " if ok is True else ("??" if ok is False else " ~")
            print(f"        {sym} {p}")
        if cfg:
            print("         config: " + " | ".join(f"{k}={v}" for k, v in cfg.items()))
    print()

    if missing:
        print(f"{len(missing)} declared path(s) NOT FOUND:\n")
        for obj, p in missing:
            print(f"  {p}\n      needed by: {obj}")
        print()
    else:
        print("Every declared source resolves.\n")

    # ---- emit the document ------------------------------------------------
    if not args.no_write:
        lines = [
            "# Provenance of reported results",
            "",
            "Generated by `scripts/provenance_map.py`. Do not edit by hand.",
            "",
            "Each row names the files a reported object was computed from. A missing",
            "row means the object is not declared in the generator, not that it has no",
            "source. Run `verify_priority1_numbers.R` for the numbers themselves; this",
            "table is about where they live.",
            "",
            "| Object | Supplies | Source | Status |",
            "|---|---|---|---|",
        ]
        for d, status, cfg in rows:
            src = "<br>".join(f"`{p}`" for p, _ in status) or "(none declared)"
            bad = [p for p, ok in status if ok is False]
            unk = [p for p, ok in status if ok is None]
            st = "**MISSING**" if bad else ("unverified" if unk else
                                            ("present" if status else "n/a"))
            lines.append(f"| {d['object']} | {d['supplies']} | {src} | {st} |")
        lines += ["", "## Run configurations", ""]
        for d, status, cfg in rows:
            if cfg:
                lines.append(f"- **{d['object']}** — "
                             + ", ".join(f"`{k}: {v}`" for k, v in cfg.items()))
        lines += ["", "## Notes", ""]
        for d, *_ in rows:
            if d["note"]:
                lines.append(f"- **{d['object']}** — {d['note']}")
        lines.append("")

        os.makedirs(os.path.dirname(args.out) or ".", exist_ok=True)
        with open(args.out, "w", encoding="utf-8") as fh:
            fh.write("\n".join(lines))
        print(f"Wrote {args.out}")

    return 1 if missing else 0


if __name__ == "__main__":
    sys.exit(main())
