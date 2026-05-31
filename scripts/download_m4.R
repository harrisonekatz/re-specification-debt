# Download M4 monthly train, M4 monthly test, and M4 info files.
source("R/adaptive_update.R")
args <- parse_cli_args()
data_dir <- get_arg(args, "data_dir", default = "data", type = "character")
overwrite <- get_arg(args, "overwrite", default = FALSE, type = "logical")
download_m4(data_dir = data_dir, overwrite = overwrite)
