# Check the M4 loader and ETS backend before running long experiments.
source("R/adaptive_update.R")
args <- parse_cli_args()
data_dir <- get_arg(args, "data_dir", default = "data", type = "character")
train_length <- get_arg(args, "train_length", default = 36L, type = "integer")
seed <- get_arg(args, "seed", default = 123L, type = "integer")

print(check_m4_loader(data_dir))
print(check_ets_smoke(data_dir = data_dir, train_length = train_length, seed = seed))
