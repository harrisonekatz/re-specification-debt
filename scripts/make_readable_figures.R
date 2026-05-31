# make_readable_figures.R
# Regenerate readable output figures from the 500-series capped output.
# Run from the package root after outputs/m4_500_capped exists.
#
# Interactive use:
#   source(file = "scripts/make_readable_figures.R")

library(data.table)
library(ggplot2)

first_existing <- function(paths) {
  for (p in paths) if (file.exists(p)) return(p)
  stop("None of these files exist: ", paste(paths, collapse = ", "))
}

summary_path <- first_existing(c(
  "outputs/m4_500_capped/summary.csv"
))
records_path <- first_existing(c(
  "outputs/m4_500_capped/records.csv"
))

out_dir <- if (grepl("^outputs/", summary_path)) {
  "outputs/m4_500_capped/readable_figures"
} else {
  "results/m4_500_capped/readable_figures_regenerated"
}

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
summary <- fread(summary_path)
records <- fread(records_path)

label_policy <- function(p) {
  out <- p
  out[p == "full_update"] <- "Full update"
  out[p == "parameter_only"] <- "Parameter only"
  is_fixed <- grepl("^fixed_f", p)
  out[is_fixed] <- paste0("Fixed f=", sub("fixed_f", "", p[is_fixed]))
  is_cap <- grepl("^adaptive_cap", p)
  if (any(is_cap)) {
    tmp <- sub("adaptive_cap", "", p[is_cap])
    cap <- sub("_tau.*", "", tmp)
    tau <- sub(".*_tau", "", tmp)
    out[is_cap] <- paste0("Cap ", cap, ", tau=", tau)
  }
  out
}

summary[, label := label_policy(policy)]
summary[, policy_type := fifelse(
  grepl("^adaptive", policy), "Capped adaptive",
  fifelse(grepl("^fixed", policy), "Fixed schedule",
          fifelse(policy == "full_update", "Full update", "Parameter only"))
)]

selected <- c(
  "full_update", "parameter_only", "fixed_f4", "fixed_f8", "fixed_f12",
  "adaptive_cap8_tau0.8", "adaptive_cap12_tau0.8"
)

frontier <- ggplot(summary, aes(relative_time, relative_loss, shape = policy_type, color = policy_type)) +
  geom_point(size = 2.4, alpha = 0.75) +
  geom_point(data = summary[policy %in% selected], size = 3.4, color = "black") +
  geom_text(data = summary[policy %in% selected], aes(label = label), hjust = -0.05, size = 3.1, show.legend = FALSE) +
  geom_hline(yintercept = 1, linetype = "dashed", linewidth = 0.4) +
  geom_vline(xintercept = 1, linetype = "dashed", linewidth = 0.4) +
  scale_x_log10(breaks = c(0.05, 0.1, 0.2, 0.3, 0.5, 1.0)) +
  coord_cartesian(xlim = c(0.045, 1.18), ylim = c(0.9965, 1.030), clip = "off") +
  labs(
    title = "Cost and accuracy frontier, M4 500-series illustration",
    subtitle = "Lower is better on both axes. Loss is relative to full updating.",
    x = "Relative computational time, log scale",
    y = "Relative forecast loss",
    color = NULL,
    shape = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "right",
    plot.margin = margin(10, 30, 10, 10)
  )

ggsave(file.path(out_dir, "figure_frontier_readable.png"), frontier, width = 8.6, height = 5.25, dpi = 300)
ggsave(file.path(out_dir, "figure_frontier_readable.pdf"), frontier, width = 8.6, height = 5.25)

update_policies <- c(
  "parameter_only", "fixed_f12", "adaptive_cap12_tau0.8", "fixed_f8",
  "adaptive_cap8_tau0.8", "fixed_f4", "fixed_f2", "full_update"
)
updates <- summary[policy %in% update_policies]
updates[, label := factor(label, levels = label_policy(update_policies))]

upd_plot <- ggplot(updates, aes(x = mean_respecifications_per_series, y = label, fill = policy_type)) +
  geom_col(width = 0.72, show.legend = FALSE) +
  geom_text(aes(label = sprintf("%.1f", mean_respecifications_per_series)), hjust = -0.15, size = 3.2) +
  coord_cartesian(xlim = c(0, 39), clip = "off") +
  labs(
    title = "Model-form update activity",
    subtitle = "Selected policies from the 500-series M4 run.",
    x = "Mean model-form re-specifications per series",
    y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"), plot.margin = margin(10, 30, 10, 10))

ggsave(file.path(out_dir, "figure_update_activity_readable.png"), upd_plot, width = 7.5, height = 4.7, dpi = 300)
ggsave(file.path(out_dir, "figure_update_activity_readable.pdf"), upd_plot, width = 7.5, height = 4.7)

policy_name <- "adaptive_cap8_tau0.8"
sub <- records[policy == policy_name]
candidate <- sub[!is.na(score_gap) & score_gap > 0.8, .N, by = series_id][order(-N)][1, series_id]
if (length(candidate) == 0 || is.na(candidate)) candidate <- sub[1, series_id]
if ("M539" %in% sub$series_id) candidate <- "M539"
s <- sub[series_id == candidate]
s[, respec_type := ""]
s[respecified == 1 & origin_number == 0, respec_type := "Initial"]
s[respecified == 1 & origin_number > 0 & !is.na(score_gap) & score_gap > threshold, respec_type := "Triggered by score gap"]
s[respecified == 1 & origin_number > 0 & respec_type == "", respec_type := "Cap reached"]
monitor <- s[!is.na(score_gap)]
cap_events <- s[respecified == 1 & respec_type == "Cap reached"]
trig_events <- s[respecified == 1 & respec_type == "Triggered by score gap"]

score_plot <- ggplot(monitor, aes(origin_number, score_gap)) +
  geom_vline(data = cap_events, aes(xintercept = origin_number), color = "gray55", alpha = 0.55, linewidth = 0.5) +
  geom_vline(data = trig_events, aes(xintercept = origin_number), color = "firebrick", alpha = 0.9, linewidth = 0.8) +
  geom_hline(yintercept = 0.8, linetype = "dashed", color = "firebrick", linewidth = 0.6) +
  geom_line(color = "darkgreen", linewidth = 0.8) +
  geom_point(color = "darkgreen", size = 2.2) +
  labs(
    title = paste0("Score-gap monitoring example, ", candidate, ", Cap 8, tau=0.8"),
    subtitle = "Gray vertical lines mark age-cap updates. Red marks a threshold-triggered update.",
    x = "Rolling-origin round",
    y = "Validation score gap"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

ggsave(file.path(out_dir, "figure_score_gap_readable.png"), score_plot, width = 8.3, height = 4.8, dpi = 300)
ggsave(file.path(out_dir, "figure_score_gap_readable.pdf"), score_plot, width = 8.3, height = 4.8)

fwrite(
  summary[policy %in% selected, .(
    policy, label, relative_loss, relative_time,
    relative_instability, mean_respecifications_per_series
  )],
  file.path(out_dir, "selected_policy_table.csv")
)

message("Wrote readable figures to ", out_dir)
