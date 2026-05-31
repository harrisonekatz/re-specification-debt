# Recompute summaries and plots from an existing records.csv without refitting models.
source("R/adaptive_update.R")
args <- parse_cli_args()
records_path <- get_arg(args, "records", default = "outputs/m4_100_capped/records.csv", type = "character")
out_dir <- get_arg(args, "out_dir", default = "outputs/resummarized", type = "character")
alpha <- get_arg(args, "alpha", default = 0.0, type = "numeric")
gamma <- get_arg(args, "gamma", default = 0.0, type = "numeric")
summary <- summarize_existing_records(records_path = records_path, out_dir = out_dir, alpha = alpha, gamma = gamma)
print(summary)
