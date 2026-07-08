import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib import rcParams
rcParams.update({"font.family":"serif","font.size":9,"axes.linewidth":0.7,
                 "xtick.direction":"in","ytick.direction":"in","pdf.fonttype":42})

# ---- values straight from the series-clustered bridge run ----
# left: triggering score gap
sg_x   = [0.8437,0.9510,1.1079,1.3846,3.0351]
sg_m   = [0.056899,0.038653,0.025839,0.022405,-0.012797]
sg_med = [0.038387,0.015332,0.020865,0.000069,0.000000]
sg_lo  = [0.037318,0.018367,0.004833,-0.006928,-0.037928]
sg_hi  = [0.075543,0.060472,0.046721,0.057558,0.011106]
# right: in-sample IC debt (-log deployed AICc weight)
ic_x   = [0.3447,0.7070,1.0123,3.4624,38.2760]
ic_m   = [0.003180,-0.006598,0.048544,0.016134,0.069739]
ic_med = [0.030238,0.017112,0.031993,0.000196,0.000000]
ic_lo  = [-0.029120,-0.029956,0.025956,-0.005390,0.052639]
ic_hi  = [0.039715,0.015472,0.069309,0.038932,0.087149]

def err(m,lo,hi): return [[a-b for a,b in zip(m,lo)],[c-a for c,a in zip(hi,m)]]

fig,(axL,axR)=plt.subplots(1,2,figsize=(7.0,3.05))
ylim=(-0.055,0.095)
for ax in (axL,axR):
    ax.axhline(0,color="0.6",lw=0.8,ls=(0,(4,3)),zorder=1)
    ax.set_ylim(*ylim); ax.tick_params(length=3)

axL.errorbar(sg_x,sg_m,yerr=err(sg_m,sg_lo,sg_hi),fmt="o",ms=5,color="black",
             ecolor="0.35",elinewidth=0.9,capsize=2.5,zorder=3,label="mean")
axL.plot(sg_x,sg_med,"o",ms=5,mfc="white",mec="black",mew=0.9,zorder=3,label="median")
axL.set_title("By triggering score gap",fontsize=9)
axL.set_xlabel("mean score gap in bin")
axL.set_ylabel("Adaptive minus fixed loss\n(lower means re-specifying helps)")
axL.annotate("largest-gap bin\nnears break-even",xy=(3.0351,-0.0128),xytext=(1.55,-0.045),
             fontsize=7.2,ha="left",va="center",
             arrowprops=dict(arrowstyle="->",lw=0.7,color="0.35"))
axL.legend(frameon=False,fontsize=7.5,loc="upper right",handletextpad=0.4)

axR.errorbar(ic_x,ic_m,yerr=err(ic_m,ic_lo,ic_hi),fmt="o",ms=5,color="black",
             ecolor="0.35",elinewidth=0.9,capsize=2.5,zorder=3)
axR.plot(ic_x,ic_med,"o",ms=5,mfc="white",mec="black",mew=0.9,zorder=3)
axR.set_xscale("log")
axR.set_title("By in-sample IC debt",fontsize=9)
axR.set_xlabel("mean IC debt in bin (log scale)")

fig.tight_layout(pad=0.6,w_pad=1.4)
fig.savefig("figures/figure_spec_debt_bridge.pdf",bbox_inches="tight")
print("wrote figures/figure_spec_debt_bridge.pdf")
