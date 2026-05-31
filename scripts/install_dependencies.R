# Install runtime dependencies.
packages <- c("data.table", "forecast", "ggplot2")
missing <- packages[!vapply(packages, requireNamespace, logical(1L), quietly = TRUE)]
if (length(missing) > 0L) {
  install.packages(missing, repos = "https://cloud.r-project.org")
} else {
  message("All required packages are already installed.")
}
