#!/usr/bin/env python3
"""Figure: tracking-signal configurations against the cost-accuracy line the
two fixed cadences trace at horizon eighteen.

Inputs are the committed full-scale policy summaries (horizon-18 battery,
38,134 eligible series): mean searches per series and mean per-series
horizon-18 MASE. Values are hard-coded from
outputs/m4_priority1_seedcheck_tau03/policy_summary_priority1.csv and
outputs/m4_priority1_seedcheck_tau05/policy_summary_priority1.csv; the two
clocks and the published monitor are identical across the two files.

Output: figures/figure_seeding_line.pdf
"""
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt

clocks = {
    "fixed_f8": (5.000, 1.185753),
    "fixed_f4": (9.000, 1.184112),
}
monitors = {
    "trigg_cap8_tau0.6\n(published seeding)": (8.992, 1.184673),
    "trigg_resid_cap8_tau0.3": (9.105, 1.184528),
    "trigg_resid_cap8_tau0.5": (6.859, 1.185123),
}

(x8, y8), (x4, y4) = clocks["fixed_f8"], clocks["fixed_f4"]
slope = (y4 - y8) / (x4 - x8)


def line(x):
    return y8 + slope * (x - x8)


fig, ax = plt.subplots(figsize=(5.6, 3.6))

xs = [4.6, 9.5]
ax.plot(xs, [line(v) for v in xs], linestyle="--", color="0.45", lw=1.0,
        zorder=1, label="line through the two clocks")

ax.scatter(*zip(*clocks.values()), marker="s", s=42, color="black", zorder=3,
           label="fixed cadence")
ax.scatter(*zip(*monitors.values()), marker="o", s=42, facecolors="white",
           edgecolors="black", linewidths=1.1, zorder=3,
           label="tracking signal")

ax.annotate("fixed_f8", clocks["fixed_f8"], textcoords="offset points",
            xytext=(6, 4), fontsize=8)
ax.annotate("fixed_f4", clocks["fixed_f4"], textcoords="offset points",
            xytext=(-6, -11), fontsize=8, ha="right")
ax.annotate("trigg_cap8_tau0.6\n(published seeding)",
            monitors["trigg_cap8_tau0.6\n(published seeding)"],
            textcoords="offset points", xytext=(-64, 8), fontsize=8)
ax.annotate("trigg_resid_cap8_tau0.3",
            monitors["trigg_resid_cap8_tau0.3"],
            textcoords="offset points", xytext=(-40, -14), fontsize=8)
ax.annotate("trigg_resid_cap8_tau0.5",
            monitors["trigg_resid_cap8_tau0.5"],
            textcoords="offset points", xytext=(7, 3), fontsize=8)

ax.set_xlabel("full-window searches per series")
ax.set_ylabel("MASE at horizon 18")
ax.set_xlim(4.4, 9.9)
ax.set_ylim(1.1838, 1.1861)
ax.yaxis.set_major_formatter(matplotlib.ticker.FormatStrFormatter("%.4f"))
ax.legend(frameon=False, fontsize=8, loc="upper right")
ax.set_title("Monitors against the clock line, 38,134 series", fontsize=10)
fig.tight_layout()
fig.savefig("figures/figure_seeding_line.pdf")
