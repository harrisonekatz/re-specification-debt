# plot_leadwise.R
#
# Renders the lead-profile figure from leadwise_diffs.csv so the figure is
# reproducible from the manifest rather than an ad hoc snippet.
#
# Usage (repo root):
#   Rscript scripts/plot_leadwise.R --run-dir outputs/m4_priority1

suppressMessages(source("R/adaptive_update.R"))
suppressMessages({library(data.table); library(ggplot2)})

args <- parse_cli_args()
run_dir <- get_arg(args, "run_dir", default = "outputs/m4_priority1", type = "character")

lw <- fread(file.path(run_dir, "leadwise_diffs.csv"))
p <- ggplot(lw, aes(lead, mean_diff)) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_ribbon(aes(ymin = boot_lo, ymax = boot_hi), alpha = 0.2) +
  geom_line() +
  facet_wrap(~contrast) +
  labs(y = "mean per-series loss difference", x = "forecast lead") +
  theme_bw()
ggsave(file.path(run_dir, "leadwise_profile.png"), p, width = 9, height = 4, dpi = 200)
message("Wrote ", file.path(run_dir, "leadwise_profile.png"))
