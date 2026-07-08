# adaptive_update.R
# Consolidated R implementation for the adaptive model-form updating technical note.
# Source this file once. There are no patch files and no source order dependencies.

# -----------------------------
# General utilities
# -----------------------------

ensure_dir <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
  invisible(path)
}

as_bool <- function(x, default = FALSE) {
  if (is.null(x)) return(default)
  if (is.logical(x)) return(x)
  if (length(x) == 0L) return(default)
  val <- tolower(as.character(x[1L]))
  val %in% c("1", "true", "t", "yes", "y", "on")
}

parse_numeric_list <- function(x, default = numeric()) {
  if (is.null(x) || is.na(x) || !nzchar(as.character(x))) return(default)
  vals <- strsplit(as.character(x), ",", fixed = TRUE)[[1L]]
  vals <- trimws(vals)
  vals <- vals[nzchar(vals)]
  as.numeric(vals)
}

parse_integer_list <- function(x, default = integer()) {
  as.integer(parse_numeric_list(x, default = default))
}

parse_cli_args <- function(argv = commandArgs(trailingOnly = TRUE)) {
  out <- list()
  i <- 1L
  while (i <= length(argv)) {
    token <- argv[[i]]
    if (startsWith(token, "--")) {
      key <- sub("^--", "", token)
      key <- gsub("-", "_", key)
      if (i == length(argv) || startsWith(argv[[i + 1L]], "--")) {
        out[[key]] <- TRUE
        i <- i + 1L
      } else {
        out[[key]] <- argv[[i + 1L]]
        i <- i + 2L
      }
    } else {
      i <- i + 1L
    }
  }
  out
}

get_arg <- function(args, name, default = NULL, type = c("character", "integer", "numeric", "logical")) {
  type <- match.arg(type)
  if (!name %in% names(args)) return(default)
  val <- args[[name]]
  if (type == "integer") return(as.integer(val))
  if (type == "numeric") return(as.numeric(val))
  if (type == "logical") return(as_bool(val, default = default))
  as.character(val)
}

safe_mean <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  if (length(x) == 0L || all(!is.finite(x))) return(NA_real_)
  mean(x, na.rm = TRUE)
}

safe_max <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  if (length(x) == 0L || all(!is.finite(x))) return(NA_real_)
  max(x, na.rm = TRUE)
}

clean_numeric <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  x[is.finite(x)]
}

check_required_packages <- function() {
  required <- c("data.table", "forecast", "ggplot2")
  missing <- required[!vapply(required, requireNamespace, logical(1L), quietly = TRUE)]
  if (length(missing) > 0L) {
    stop(
      "Missing R package(s): ", paste(missing, collapse = ", "),
      ". Run Rscript scripts/install_dependencies.R first."
    )
  }
  invisible(TRUE)
}

# -----------------------------
# Data download and M4 loading
# -----------------------------

M4_URLS <- list(
  monthly_train = "https://raw.githubusercontent.com/Mcompetitions/M4-methods/master/Dataset/Train/Monthly-train.csv",
  monthly_test = "https://raw.githubusercontent.com/Mcompetitions/M4-methods/master/Dataset/Test/Monthly-test.csv",
  info = "https://raw.githubusercontent.com/Mcompetitions/M4-methods/master/Dataset/M4-info.csv"
)

download_url <- function(url, dest, overwrite = FALSE) {
  ensure_dir(dirname(dest))
  if (file.exists(dest) && !isTRUE(overwrite)) {
    message("exists: ", dest)
    return(invisible(dest))
  }
  message("downloading: ", url)
  utils::download.file(url, dest, mode = "wb", quiet = FALSE)
  message("wrote: ", dest)
  invisible(dest)
}

download_m4 <- function(data_dir = "data", overwrite = FALSE) {
  m4_dir <- file.path(data_dir, "m4")
  ensure_dir(m4_dir)
  download_url(M4_URLS$monthly_train, file.path(m4_dir, "monthly_train.csv"), overwrite = overwrite)
  download_url(M4_URLS$monthly_test, file.path(m4_dir, "monthly_test.csv"), overwrite = overwrite)
  download_url(M4_URLS$info, file.path(m4_dir, "info.csv"), overwrite = overwrite)
  invisible(m4_dir)
}

m4_strip_token <- function(x) {
  x <- sub("^\\ufeff", "", x)
  x <- trimws(as.character(x))
  x <- sub('^"', '', x)
  x <- sub('"$', '', x)
  x
}

m4_line_is_data <- function(line) {
  if (!nzchar(trimws(line))) return(FALSE)
  first <- strsplit(line, ",", fixed = TRUE)[[1L]][1L]
  first <- m4_strip_token(first)
  grepl("^M[0-9]+$", first)
}

m4_line_tokens <- function(line) {
  toks <- strsplit(line, ",", fixed = TRUE)[[1L]]
  m4_strip_token(toks)
}

find_m4_file <- function(data_dir = "data", filename) {
  candidates <- c(
    file.path(data_dir, "m4", filename),
    file.path(data_dir, "M4", filename),
    file.path(data_dir, "Dataset", "Train", filename),
    file.path(data_dir, "Dataset", "Test", filename),
    file.path(data_dir, "m4", "Dataset", "Train", filename),
    file.path(data_dir, "m4", "Dataset", "Test", filename)
  )
  found <- candidates[file.exists(candidates)]
  if (length(found) > 0L) return(found[[1L]])

  recursive <- list.files(
    data_dir,
    pattern = paste0("^", gsub("\\.", "\\\\.", filename), "$"),
    recursive = TRUE,
    full.names = TRUE
  )
  if (length(recursive) > 0L) return(recursive[[1L]])
  NULL
}

read_m4_named_series <- function(path, min_obs = 1L) {
  if (is.null(path) || !file.exists(path)) return(list())
  lines <- readLines(path, warn = FALSE)
  keep <- vapply(lines, m4_line_is_data, logical(1L))
  lines <- lines[keep]
  out <- list()
  min_obs <- max(1L, as.integer(min_obs))

  for (line in lines) {
    toks <- m4_line_tokens(line)
    if (length(toks) < 2L) next
    sid <- toks[[1L]]
    y <- suppressWarnings(as.numeric(toks[-1L]))
    y <- y[is.finite(y)]
    if (length(y) >= min_obs && nzchar(sid)) {
      out[[sid]] <- y
    }
  }
  out
}

load_m4_monthly <- function(
    data_dir = "data",
    n_series = 100L,
    seed = 123L,
    min_obs = 24L,
    include_test = TRUE) {

  train_path <- find_m4_file(data_dir, "monthly_train.csv")
  if (is.null(train_path)) train_path <- find_m4_file(data_dir, "Monthly-train.csv")
  if (is.null(train_path)) {
    stop("Could not find monthly_train.csv or Monthly-train.csv under ", data_dir,
         ". Run Rscript scripts/download_m4.R first.")
  }

  test_path <- NULL
  if (isTRUE(include_test)) {
    test_path <- find_m4_file(data_dir, "monthly_test.csv")
    if (is.null(test_path)) test_path <- find_m4_file(data_dir, "Monthly-test.csv")
  }

  train <- read_m4_named_series(train_path, min_obs = 1L)
  test <- read_m4_named_series(test_path, min_obs = 1L)
  min_obs <- max(1L, as.integer(min_obs))

  sids <- names(train)
  items <- vector("list", length(sids))
  used <- 0L

  for (sid in sids) {
    y <- train[[sid]]
    if (length(test) > 0L && sid %in% names(test)) {
      y <- c(y, test[[sid]])
    }
    y <- clean_numeric(y)
    if (length(y) >= min_obs) {
      used <- used + 1L
      items[[used]] <- list(
        series_id = sid,
        values = y,
        meta = list(
          dataset = "M4",
          frequency = "monthly",
          train_path = train_path,
          test_path = ifelse(is.null(test_path), "", test_path),
          include_test = isTRUE(include_test)
        )
      )
    }
  }

  if (used == 0L) return(list())
  items <- items[seq_len(used)]

  if (!is.null(n_series) && n_series > 0L && n_series < length(items)) {
    set.seed(seed)
    idx <- sort(sample(seq_along(items), n_series))
    items <- items[idx]
  }

  lens <- vapply(items, function(x) length(x$values), integer(1L))
  message(
    "Loaded ", length(items), " M4 monthly series from ", train_path,
    if (!is.null(test_path)) paste0(" + ", test_path) else "",
    " | min length=", min(lens),
    " | median length=", stats::median(lens),
    " | max length=", max(lens)
  )

  items
}

check_m4_loader <- function(data_dir = "data") {
  items <- load_m4_monthly(
    data_dir = data_dir,
    n_series = -1L,
    seed = 123L,
    min_obs = 1L,
    include_test = TRUE
  )
  lens <- vapply(items, function(x) length(x$values), integer(1L))
  data.frame(
    n_series = length(items),
    min_obs = min(lens),
    median_obs = stats::median(lens),
    max_obs = max(lens),
    n_ge_77 = sum(lens >= 77L),
    n_ge_98 = sum(lens >= 98L),
    stringsAsFactors = FALSE
  )
}

# -----------------------------
# Accuracy and stability metrics
# -----------------------------

seasonal_naive_scale <- function(y_train, seasonality, squared = FALSE) {
  y <- clean_numeric(y_train)
  if (length(y) <= 1L) return(1.0)
  s <- max(1L, as.integer(seasonality))
  if (length(y) > s) {
    diffs <- y[(s + 1L):length(y)] - y[1L:(length(y) - s)]
  } else {
    diffs <- diff(y)
  }
  if (length(diffs) == 0L || all(!is.finite(diffs))) return(1.0)
  scale <- if (isTRUE(squared)) mean(diffs^2, na.rm = TRUE) else mean(abs(diffs), na.rm = TRUE)
  if (!is.finite(scale) || scale <= 1e-12) {
    diffs1 <- diff(y)
    if (length(diffs1) == 0L) return(1.0)
    scale <- if (isTRUE(squared)) mean(diffs1^2, na.rm = TRUE) else mean(abs(diffs1), na.rm = TRUE)
  }
  if (is.finite(scale) && scale > 1e-12) scale else 1.0
}

mase <- function(y_train, y_true, y_pred, seasonality) {
  yt <- clean_numeric(y_true)
  yp <- clean_numeric(y_pred)
  m <- min(length(yt), length(yp))
  if (m == 0L) return(NA_real_)
  scale <- seasonal_naive_scale(y_train, seasonality, squared = FALSE)
  mean(abs(yt[seq_len(m)] - yp[seq_len(m)]), na.rm = TRUE) / scale
}

rmsse <- function(y_train, y_true, y_pred, seasonality) {
  yt <- clean_numeric(y_true)
  yp <- clean_numeric(y_pred)
  m <- min(length(yt), length(yp))
  if (m == 0L) return(NA_real_)
  scale <- seasonal_naive_scale(y_train, seasonality, squared = TRUE)
  sqrt(mean((yt[seq_len(m)] - yp[seq_len(m)])^2, na.rm = TRUE) / scale)
}

metric_value <- function(metric, y_train, y_true, y_pred, seasonality) {
  metric <- tolower(as.character(metric)[1L])
  if (metric == "rmsse") return(rmsse(y_train, y_true, y_pred, seasonality))
  mase(y_train, y_true, y_pred, seasonality)
}

smapc_between_forecasts <- function(prev, curr) {
  a <- clean_numeric(prev)
  b <- clean_numeric(curr)
  m <- min(length(a), length(b))
  if (m == 0L) return(NA_real_)
  denom <- abs(a[seq_len(m)]) + abs(b[seq_len(m)])
  keep <- denom > 1e-12
  if (!any(keep)) return(0.0)
  200.0 * mean(abs(a[seq_len(m)][keep] - b[seq_len(m)][keep]) / denom[keep], na.rm = TRUE)
}

# -----------------------------
# ETS candidate fitting with forecast::ets
# -----------------------------

seasonal_naive_forecast <- function(y, horizon, seasonality) {
  y <- clean_numeric(y)
  horizon <- as.integer(horizon)
  if (length(y) == 0L) return(rep(0, horizon))
  s <- max(1L, as.integer(seasonality))
  if (length(y) >= s) {
    base <- tail(y, s)
    return(rep(base, length.out = horizon))
  }
  rep(tail(y, 1L), horizon)
}

make_ts <- function(y, seasonality) {
  stats::ts(clean_numeric(y), frequency = max(1L, as.integer(seasonality)))
}

parse_ets_form <- function(form) {
  form <- as.character(form)[1L]
  if (grepl("^[AM]Ad[NAM]$", form)) {
    e <- substr(form, 1L, 1L)
    s <- substr(form, nchar(form), nchar(form))
    return(list(model = paste0(e, "A", s), damped = TRUE, code = form))
  }
  if (grepl("^[AM][NA][NAM]$", form)) {
    e <- substr(form, 1L, 1L)
    t <- substr(form, 2L, 2L)
    s <- substr(form, 3L, 3L)
    return(list(model = paste0(e, t, s), damped = FALSE, code = form))
  }
  NULL
}

ets_candidate_models <- function(y_train, allow_multiplicative = TRUE) {
  y <- clean_numeric(y_train)
  strictly_positive <- length(y) > 0L && all(y > 0)

  candidates <- c(
    "ANN", "AAN", "AAdN",
    "ANA", "AAA", "AAdA"
  )

  if (isTRUE(allow_multiplicative) && strictly_positive) {
    candidates <- c(
      candidates,
      "ANM", "AAM", "AAdM",
      "MNN", "MAN", "MAdN",
      "MNM", "MAM", "MAdM"
    )
  }

  unique(candidates)
}

fallback_es_outcome <- function(
    y_train,
    horizon,
    seasonality,
    clip_nonnegative = FALSE,
    fit_seconds = 0.0,
    error = "all ETS candidates failed") {

  pred <- seasonal_naive_forecast(y_train, horizon, seasonality)
  if (isTRUE(clip_nonnegative)) pred <- pmax(pred, 0)
  list(
    form = "SNAIVE",
    forecast = pred,
    aic = Inf,
    aicc = Inf,
    bic = Inf,
    fit_seconds = as.numeric(fit_seconds),
    status = "fallback",
    error = error
  )
}

extract_forecast_ets <- function(fit, horizon) {
  fc <- forecast::forecast(fit, h = as.integer(horizon))
  vals <- as.numeric(fc$mean)
  if (length(vals) < horizon) vals <- rep(vals, length.out = horizon)
  vals[seq_len(horizon)]
}

fit_es_internal <- function(
    y_train,
    model_code,
    horizon,
    seasonality,
    clip_nonnegative = FALSE) {

  check_required_packages()
  y <- clean_numeric(y_train)
  horizon <- as.integer(horizon)
  parsed <- parse_ets_form(model_code)
  start <- proc.time()[["elapsed"]]

  if (length(y) < 3L || is.null(parsed)) {
    return(fallback_es_outcome(
      y,
      horizon,
      seasonality,
      clip_nonnegative,
      fit_seconds = proc.time()[["elapsed"]] - start,
      error = "too few observations or invalid model code"
    ))
  }

  fit <- try(
    forecast::ets(
      y = make_ts(y, seasonality),
      model = parsed$model,
      damped = parsed$damped,
      restrict = TRUE,
      allow.multiplicative.trend = FALSE
    ),
    silent = TRUE
  )

  fit_seconds <- proc.time()[["elapsed"]] - start

  if (inherits(fit, "try-error")) {
    return(fallback_es_outcome(
      y,
      horizon,
      seasonality,
      clip_nonnegative,
      fit_seconds = fit_seconds,
      error = as.character(fit)[1L]
    ))
  }

  pred <- try(extract_forecast_ets(fit, horizon), silent = TRUE)
  if (inherits(pred, "try-error") || length(pred) == 0L || !any(is.finite(pred))) {
    return(fallback_es_outcome(
      y,
      horizon,
      seasonality,
      clip_nonnegative,
      fit_seconds = fit_seconds,
      error = "could not extract forecast"
    ))
  }

  pred <- as.numeric(pred)
  if (length(pred) < horizon) pred <- rep(pred, length.out = horizon)
  pred <- pred[seq_len(horizon)]
  if (isTRUE(clip_nonnegative)) pred <- pmax(pred, 0)

  aic <- suppressWarnings(as.numeric(fit$aic))
  aicc <- suppressWarnings(as.numeric(fit$aicc))
  bic <- suppressWarnings(as.numeric(fit$bic))
  if (!is.finite(aic)) aic <- Inf
  if (!is.finite(aicc)) aicc <- aic
  if (!is.finite(bic)) bic <- Inf

  list(
    form = as.character(model_code),
    forecast = pred,
    aic = aic,
    aicc = aicc,
    bic = bic,
    fit_seconds = fit_seconds,
    status = "ok",
    error = ""
  )
}

fit_auto_es <- function(
    y_train,
    horizon,
    seasonality,
    allow_multiplicative = TRUE,
    clip_nonnegative = FALSE) {

  candidates <- ets_candidate_models(y_train, allow_multiplicative = allow_multiplicative)
  if (length(candidates) == 0L) {
    return(fallback_es_outcome(y_train, horizon, seasonality, clip_nonnegative))
  }

  fits <- vector("list", length(candidates))
  total_seconds <- 0.0

  for (i in seq_along(candidates)) {
    fits[[i]] <- fit_es_internal(
      y_train = y_train,
      model_code = candidates[[i]],
      horizon = horizon,
      seasonality = seasonality,
      clip_nonnegative = clip_nonnegative
    )
    total_seconds <- total_seconds + as.numeric(fits[[i]]$fit_seconds)
    fits[[i]]$form <- candidates[[i]]
  }

  aiccs <- vapply(fits, function(z) {
    if (!is.null(z$aicc) && is.finite(z$aicc) && !identical(z$status, "fallback")) {
      as.numeric(z$aicc)
    } else {
      Inf
    }
  }, numeric(1L))

  if (!any(is.finite(aiccs))) {
    errors <- unique(vapply(fits, function(z) as.character(z$error), character(1L)))
    return(fallback_es_outcome(
      y_train,
      horizon,
      seasonality,
      clip_nonnegative,
      fit_seconds = total_seconds,
      error = paste(utils::head(errors, 3L), collapse = " | ")
    ))
  }

  best_idx <- which.min(aiccs)
  best <- fits[[best_idx]]
  best$form <- candidates[[best_idx]]
  best$fit_seconds <- total_seconds
  best$status <- "ok"
  best$error <- ""
  best
}

fit_form_es <- function(y_train, form, horizon, seasonality, clip_nonnegative = FALSE) {
  form <- as.character(form)[1L]
  if (!nzchar(form) || is.na(form) || identical(toupper(form), "SNAIVE")) {
    return(fallback_es_outcome(y_train, horizon, seasonality, clip_nonnegative))
  }
  if (form %in% c("ZZZ", "AZZ", "ZXZ", "XXX")) {
    return(fit_auto_es(
      y_train = y_train,
      horizon = horizon,
      seasonality = seasonality,
      allow_multiplicative = TRUE,
      clip_nonnegative = clip_nonnegative
    ))
  }
  outcome <- fit_es_internal(
    y_train = y_train,
    model_code = form,
    horizon = horizon,
    seasonality = seasonality,
    clip_nonnegative = clip_nonnegative
  )
  outcome$form <- form
  outcome
}


ic_weights <- function(ic_values) {
  vals <- suppressWarnings(as.numeric(ic_values))
  out <- rep(NA_real_, length(vals))
  good <- is.finite(vals)
  if (!any(good)) return(out)
  delta <- vals[good] - min(vals[good], na.rm = TRUE)
  rel <- exp(-0.5 * delta)
  denom <- sum(rel, na.rm = TRUE)
  if (!is.finite(denom) || denom <= 0) return(out)
  out[good] <- rel / denom
  out
}

safe_debt <- function(weight) {
  w <- suppressWarnings(as.numeric(weight))
  if (length(w) == 0L || !is.finite(w[[1L]])) return(NA_real_)
  -log(max(w[[1L]], .Machine$double.xmin))
}

ic_weight_summary <- function(forms, aiccs, bics, current_form) {
  forms <- as.character(forms)
  current_form <- as.character(current_form)[1L]
  aicc_w <- ic_weights(aiccs)
  bic_w <- ic_weights(bics)
  current_idx <- match(current_form, forms)

  current_aicc_w <- if (!is.na(current_idx)) aicc_w[[current_idx]] else NA_real_
  current_bic_w <- if (!is.na(current_idx)) bic_w[[current_idx]] else NA_real_
  best_aicc_idx <- if (any(is.finite(aicc_w))) which.max(aicc_w) else NA_integer_
  best_bic_idx <- if (any(is.finite(bic_w))) which.max(bic_w) else NA_integer_

  list(
    deployed_weight_aicc = current_aicc_w,
    spec_debt_aicc = safe_debt(current_aicc_w),
    best_ic_form_aicc = if (!is.na(best_aicc_idx)) forms[[best_aicc_idx]] else NA_character_,
    best_weight_aicc = if (!is.na(best_aicc_idx)) aicc_w[[best_aicc_idx]] else NA_real_,
    deployed_weight_bic = current_bic_w,
    spec_debt_bic = safe_debt(current_bic_w),
    best_ic_form_bic = if (!is.na(best_bic_idx)) forms[[best_bic_idx]] else NA_character_,
    best_weight_bic = if (!is.na(best_bic_idx)) bic_w[[best_bic_idx]] else NA_real_
  )
}

select_form_validation <- function(
    y_base,
    y_val,
    seasonality,
    metric = "mase",
    allow_multiplicative = TRUE,
    clip_nonnegative = FALSE,
    current_form = NULL) {

  val <- clean_numeric(y_val)
  empty <- list(
    form = "SNAIVE", loss = Inf, fit_seconds = 0.0,
    deployed_weight_aicc = NA_real_, spec_debt_aicc = NA_real_,
    best_ic_form_aicc = NA_character_, best_weight_aicc = NA_real_,
    deployed_weight_bic = NA_real_, spec_debt_bic = NA_real_,
    best_ic_form_bic = NA_character_, best_weight_bic = NA_real_
  )
  if (length(val) == 0L) return(empty)

  candidates <- ets_candidate_models(y_base, allow_multiplicative = allow_multiplicative)
  losses <- rep(Inf, length(candidates))
  aiccs <- rep(Inf, length(candidates))
  bics <- rep(Inf, length(candidates))
  total_seconds <- 0.0

  for (i in seq_along(candidates)) {
    outcome <- fit_form_es(
      y_train = y_base,
      form = candidates[[i]],
      horizon = length(val),
      seasonality = seasonality,
      clip_nonnegative = clip_nonnegative
    )
    total_seconds <- total_seconds + as.numeric(outcome$fit_seconds)
    if (!identical(outcome$status, "fallback")) {
      losses[[i]] <- metric_value(metric, y_base, val, outcome$forecast, seasonality)
      aiccs[[i]] <- if (!is.null(outcome$aicc)) as.numeric(outcome$aicc) else Inf
      bics[[i]] <- if (!is.null(outcome$bic)) as.numeric(outcome$bic) else Inf
    }
  }

  if (!any(is.finite(losses))) {
    empty$fit_seconds <- total_seconds
    return(empty)
  }

  best_idx <- which.min(losses)
  ic <- ic_weight_summary(candidates, aiccs, bics, current_form)
  c(list(
    form = candidates[[best_idx]],
    loss = losses[[best_idx]],
    fit_seconds = total_seconds
  ), ic)
}


check_ets_smoke <- function(data_dir = "data", train_length = 36L, seed = 123L) {
  check_required_packages()
  items <- load_m4_monthly(
    data_dir = data_dir,
    n_series = 5L,
    seed = seed,
    min_obs = train_length + 3L,
    include_test = TRUE
  )
  rows <- lapply(items, function(item) {
    y <- item$values
    train <- y[seq_len(train_length)]
    fit <- fit_auto_es(train, horizon = 3L, seasonality = 12L, allow_multiplicative = TRUE)
    snaive <- seasonal_naive_forecast(train, horizon = 3L, seasonality = 12L)
    data.frame(
      series_id = item$series_id,
      form = fit$form,
      status = fit$status,
      error = fit$error,
      aicc = fit$aicc,
      equals_snaive = isTRUE(all.equal(as.numeric(fit$forecast), as.numeric(snaive), tolerance = 1e-8)),
      forecast = paste(round(as.numeric(fit$forecast), 4), collapse = ","),
      stringsAsFactors = FALSE
    )
  })
  data.table::rbindlist(rows, fill = TRUE)
}

# -----------------------------
# Rolling-origin policies
# -----------------------------

new_rolling_config <- function(
    horizon,
    seasonality,
    train_length,
    n_rounds,
    fixed_window = TRUE,
    metric = "mase",
    allow_multiplicative = TRUE,
    clip_nonnegative = FALSE,
    monitor_window = 12L,
    monitor_every = 1L) {
  list(
    horizon = as.integer(horizon),
    seasonality = as.integer(seasonality),
    train_length = as.integer(train_length),
    n_rounds = as.integer(n_rounds),
    fixed_window = as.logical(fixed_window),
    metric = tolower(metric),
    allow_multiplicative = as.logical(allow_multiplicative),
    clip_nonnegative = as.logical(clip_nonnegative),
    monitor_window = as.integer(monitor_window),
    monitor_every = as.integer(monitor_every)
  )
}

make_origins <- function(y, train_length, horizon, n_rounds) {
  y <- clean_numeric(y)
  max_origin <- length(y) - as.integer(horizon) + 1L
  min_origin <- as.integer(train_length) + 1L
  if (max_origin < min_origin) return(integer())
  start <- max(min_origin, max_origin - as.integer(n_rounds) + 1L)
  seq.int(start, max_origin)
}

training_window <- function(y, origin, cfg) {
  y <- clean_numeric(y)
  if (isTRUE(cfg$fixed_window)) {
    start <- max(1L, as.integer(origin) - cfg$train_length)
    return(y[start:(origin - 1L)])
  }
  y[1L:(origin - 1L)]
}

make_record <- function(
    series_id,
    policy,
    origin_number,
    origin_index,
    form,
    y_true,
    y_pred,
    loss,
    fit_seconds,
    respecified,
    score_gap = NA_real_,
    threshold = NA_real_,
    tau_cost_ratio = NA_real_,
    trigger_reason = "none",
    triggered_by_score = FALSE,
    triggered_by_cap = FALSE,
    age_before_action = NA_integer_,
    current_validation_loss = NA_real_,
    challenger_validation_loss = NA_real_,
    challenger_form = NA_character_,
    deployed_weight_aicc = NA_real_,
    spec_debt_aicc = NA_real_,
    best_ic_form_aicc = NA_character_,
    best_weight_aicc = NA_real_,
    deployed_weight_bic = NA_real_,
    spec_debt_bic = NA_real_,
    best_ic_form_bic = NA_character_,
    best_weight_bic = NA_real_) {

  y_true <- as.numeric(y_true)
  y_pred <- as.numeric(y_pred)
  rec <- data.frame(
    series_id = as.character(series_id),
    policy = as.character(policy),
    origin_number = as.integer(origin_number),
    origin_index = as.integer(origin_index),
    form = as.character(form),
    loss = as.numeric(loss),
    fit_seconds = as.numeric(fit_seconds),
    respecified = as.integer(respecified),
    score_gap = as.numeric(score_gap),
    threshold = as.numeric(threshold),
    tau_cost_ratio = as.numeric(tau_cost_ratio),
    trigger_reason = as.character(trigger_reason),
    triggered_by_score = as.integer(triggered_by_score),
    triggered_by_cap = as.integer(triggered_by_cap),
    age_before_action = as.integer(age_before_action),
    current_validation_loss = as.numeric(current_validation_loss),
    challenger_validation_loss = as.numeric(challenger_validation_loss),
    challenger_form = as.character(challenger_form),
    deployed_weight_aicc = as.numeric(deployed_weight_aicc),
    spec_debt_aicc = as.numeric(spec_debt_aicc),
    best_ic_form_aicc = as.character(best_ic_form_aicc),
    best_weight_aicc = as.numeric(best_weight_aicc),
    deployed_weight_bic = as.numeric(deployed_weight_bic),
    spec_debt_bic = as.numeric(spec_debt_bic),
    best_ic_form_bic = as.character(best_ic_form_bic),
    best_weight_bic = as.numeric(best_weight_bic),
    stringsAsFactors = FALSE
  )
  if (length(y_true) > 0L) {
    for (i in seq_along(y_true)) rec[[paste0("y", i)]] <- y_true[[i]]
  }
  if (length(y_pred) > 0L) {
    for (i in seq_along(y_pred)) rec[[paste0("f", i)]] <- y_pred[[i]]
  }
  rec
}


run_full_update <- function(series_id, y, cfg) {
  requireNamespace("data.table", quietly = TRUE)
  y <- clean_numeric(y)
  origins <- make_origins(y, cfg$train_length, cfg$horizon, cfg$n_rounds)
  records <- vector("list", length(origins))
  if (length(origins) == 0L) return(data.frame())

  for (idx in seq_along(origins)) {
    origin <- origins[[idx]]
    train <- training_window(y, origin, cfg)
    truth <- y[origin:(origin + cfg$horizon - 1L)]
    outcome <- fit_auto_es(train, cfg$horizon, cfg$seasonality, cfg$allow_multiplicative, cfg$clip_nonnegative)
    loss <- metric_value(cfg$metric, train, truth, outcome$forecast, cfg$seasonality)
    records[[idx]] <- make_record(series_id, "full_update", idx - 1L, origin, outcome$form, truth, outcome$forecast, loss, outcome$fit_seconds, TRUE)
  }
  data.table::rbindlist(records, fill = TRUE)
}

run_parameter_only <- function(series_id, y, cfg) {
  requireNamespace("data.table", quietly = TRUE)
  y <- clean_numeric(y)
  origins <- make_origins(y, cfg$train_length, cfg$horizon, cfg$n_rounds)
  records <- vector("list", length(origins))
  if (length(origins) == 0L) return(data.frame())

  current_form <- NULL
  for (idx in seq_along(origins)) {
    origin <- origins[[idx]]
    train <- training_window(y, origin, cfg)
    truth <- y[origin:(origin + cfg$horizon - 1L)]
    respecified <- FALSE
    if (is.null(current_form)) {
      outcome <- fit_auto_es(train, cfg$horizon, cfg$seasonality, cfg$allow_multiplicative, cfg$clip_nonnegative)
      current_form <- outcome$form
      respecified <- TRUE
    } else {
      outcome <- fit_form_es(train, current_form, cfg$horizon, cfg$seasonality, cfg$clip_nonnegative)
    }
    loss <- metric_value(cfg$metric, train, truth, outcome$forecast, cfg$seasonality)
    records[[idx]] <- make_record(series_id, "parameter_only", idx - 1L, origin, current_form, truth, outcome$forecast, loss, outcome$fit_seconds, respecified)
  }
  data.table::rbindlist(records, fill = TRUE)
}

run_fixed_frequency <- function(series_id, y, cfg, frequency) {
  requireNamespace("data.table", quietly = TRUE)
  y <- clean_numeric(y)
  origins <- make_origins(y, cfg$train_length, cfg$horizon, cfg$n_rounds)
  records <- vector("list", length(origins))
  if (length(origins) == 0L) return(data.frame())

  current_form <- NULL
  frequency <- max(1L, as.integer(frequency))
  policy_name <- paste0("fixed_f", frequency)

  for (idx in seq_along(origins)) {
    origin <- origins[[idx]]
    train <- training_window(y, origin, cfg)
    truth <- y[origin:(origin + cfg$horizon - 1L)]
    respecified <- FALSE
    if (is.null(current_form) || ((idx - 1L) %% frequency == 0L)) {
      outcome <- fit_auto_es(train, cfg$horizon, cfg$seasonality, cfg$allow_multiplicative, cfg$clip_nonnegative)
      current_form <- outcome$form
      respecified <- TRUE
    } else {
      outcome <- fit_form_es(train, current_form, cfg$horizon, cfg$seasonality, cfg$clip_nonnegative)
    }
    loss <- metric_value(cfg$metric, train, truth, outcome$forecast, cfg$seasonality)
    records[[idx]] <- make_record(series_id, policy_name, idx - 1L, origin, current_form, truth, outcome$forecast, loss, outcome$fit_seconds, respecified)
  }
  data.table::rbindlist(records, fill = TRUE)
}

run_adaptive_score <- function(series_id, y, cfg, threshold) {
  requireNamespace("data.table", quietly = TRUE)
  y <- clean_numeric(y)
  origins <- make_origins(y, cfg$train_length, cfg$horizon, cfg$n_rounds)
  records <- vector("list", length(origins))
  if (length(origins) == 0L) return(data.frame())

  current_form <- NULL
  threshold <- as.numeric(threshold)
  policy_name <- paste0("adaptive_tau", format(threshold, trim = TRUE, scientific = FALSE))

  for (idx in seq_along(origins)) {
    origin <- origins[[idx]]
    train <- training_window(y, origin, cfg)
    truth <- y[origin:(origin + cfg$horizon - 1L)]
    respecified <- FALSE
    score_gap <- NA_real_
    monitor_seconds <- 0.0
    current_validation_loss <- NA_real_
    challenger_validation_loss <- NA_real_
    challenger_form <- NA_character_
    deployed_weight_aicc <- NA_real_
    spec_debt_aicc <- NA_real_
    best_ic_form_aicc <- NA_character_
    best_weight_aicc <- NA_real_
    deployed_weight_bic <- NA_real_
    spec_debt_bic <- NA_real_
    best_ic_form_bic <- NA_character_
    best_weight_bic <- NA_real_
    triggered_by_score <- FALSE
    trigger_reason <- "none"

    if (is.null(current_form)) {
      outcome <- fit_auto_es(train, cfg$horizon, cfg$seasonality, cfg$allow_multiplicative, cfg$clip_nonnegative)
      current_form <- outcome$form
      respecified <- TRUE
      trigger_reason <- "initial"
    } else {
      do_monitor <- cfg$monitor_window > 1L &&
        ((idx - 1L) %% max(1L, cfg$monitor_every) == 0L) &&
        length(train) > cfg$monitor_window + max(6L, cfg$seasonality)

      if (do_monitor) {
        mw <- cfg$monitor_window
        base <- train[seq_len(length(train) - mw)]
        val <- tail(train, mw)
        current_val <- fit_form_es(base, current_form, mw, cfg$seasonality, cfg$clip_nonnegative)
        current_loss <- metric_value(cfg$metric, base, val, current_val$forecast, cfg$seasonality)
        best_val <- select_form_validation(
          base, val, cfg$seasonality,
          metric = cfg$metric,
          allow_multiplicative = cfg$allow_multiplicative,
          clip_nonnegative = cfg$clip_nonnegative,
          current_form = current_form
        )
        monitor_seconds <- as.numeric(current_val$fit_seconds) + as.numeric(best_val$fit_seconds)
        current_validation_loss <- current_loss
        challenger_validation_loss <- best_val$loss
        challenger_form <- as.character(best_val$form)
        deployed_weight_aicc <- best_val$deployed_weight_aicc
        spec_debt_aicc <- best_val$spec_debt_aicc
        best_ic_form_aicc <- best_val$best_ic_form_aicc
        best_weight_aicc <- best_val$best_weight_aicc
        deployed_weight_bic <- best_val$deployed_weight_bic
        spec_debt_bic <- best_val$spec_debt_bic
        best_ic_form_bic <- best_val$best_ic_form_bic
        best_weight_bic <- best_val$best_weight_bic
        if (is.finite(current_loss) && is.finite(best_val$loss)) score_gap <- current_loss - best_val$loss
        if (is.finite(score_gap) && score_gap > threshold && !identical(as.character(best_val$form), as.character(current_form))) {
          current_form <- best_val$form
          respecified <- TRUE
          triggered_by_score <- TRUE
          trigger_reason <- "score"
        }
      }
      outcome <- fit_form_es(train, current_form, cfg$horizon, cfg$seasonality, cfg$clip_nonnegative)
      outcome$fit_seconds <- as.numeric(outcome$fit_seconds) + monitor_seconds
    }

    loss <- metric_value(cfg$metric, train, truth, outcome$forecast, cfg$seasonality)
    records[[idx]] <- make_record(
      series_id, policy_name, idx - 1L, origin, current_form, truth, outcome$forecast,
      loss, outcome$fit_seconds, respecified,
      score_gap = score_gap, threshold = threshold, tau_cost_ratio = threshold,
      trigger_reason = trigger_reason, triggered_by_score = triggered_by_score,
      triggered_by_cap = FALSE,
      current_validation_loss = current_validation_loss,
      challenger_validation_loss = challenger_validation_loss,
      challenger_form = challenger_form,
      deployed_weight_aicc = deployed_weight_aicc,
      spec_debt_aicc = spec_debt_aicc,
      best_ic_form_aicc = best_ic_form_aicc,
      best_weight_aicc = best_weight_aicc,
      deployed_weight_bic = deployed_weight_bic,
      spec_debt_bic = spec_debt_bic,
      best_ic_form_bic = best_ic_form_bic,
      best_weight_bic = best_weight_bic
    )
  }
  data.table::rbindlist(records, fill = TRUE)
}


run_adaptive_capped_score <- function(series_id, y, cfg, threshold, max_age) {
  requireNamespace("data.table", quietly = TRUE)
  y <- clean_numeric(y)
  origins <- make_origins(y, cfg$train_length, cfg$horizon, cfg$n_rounds)
  records <- vector("list", length(origins))
  if (length(origins) == 0L) return(data.frame())

  current_form <- NULL
  threshold <- as.numeric(threshold)
  max_age <- max(1L, as.integer(max_age))
  last_respec_idx <- NA_integer_
  policy_name <- paste0("adaptive_cap", max_age, "_tau", format(threshold, trim = TRUE, scientific = FALSE))

  for (idx in seq_along(origins)) {
    origin <- origins[[idx]]
    train <- training_window(y, origin, cfg)
    truth <- y[origin:(origin + cfg$horizon - 1L)]

    respecified <- FALSE
    score_gap <- NA_real_
    monitor_seconds <- 0.0
    current_validation_loss <- NA_real_
    challenger_validation_loss <- NA_real_
    challenger_form <- NA_character_
    deployed_weight_aicc <- NA_real_
    spec_debt_aicc <- NA_real_
    best_ic_form_aicc <- NA_character_
    best_weight_aicc <- NA_real_
    deployed_weight_bic <- NA_real_
    spec_debt_bic <- NA_real_
    best_ic_form_bic <- NA_character_
    best_weight_bic <- NA_real_
    triggered_by_score <- FALSE
    triggered_by_cap <- FALSE
    trigger_reason <- "none"
    age_before_action <- if (is.na(last_respec_idx)) NA_integer_ else as.integer(idx - last_respec_idx)

    due_to_age <- is.null(current_form) || is.na(last_respec_idx) || ((idx - last_respec_idx) >= max_age)

    if (due_to_age) {
      outcome <- fit_auto_es(train, cfg$horizon, cfg$seasonality, cfg$allow_multiplicative, cfg$clip_nonnegative)
      current_form <- outcome$form
      triggered_by_cap <- !is.na(last_respec_idx)
      trigger_reason <- ifelse(triggered_by_cap, "cap", "initial")
      last_respec_idx <- idx
      respecified <- TRUE
    } else {
      do_monitor <- cfg$monitor_window > 1L &&
        ((idx - 1L) %% max(1L, cfg$monitor_every) == 0L) &&
        length(train) > cfg$monitor_window + max(6L, cfg$seasonality)

      if (do_monitor) {
        mw <- cfg$monitor_window
        base <- train[seq_len(length(train) - mw)]
        val <- tail(train, mw)
        current_val <- fit_form_es(base, current_form, mw, cfg$seasonality, cfg$clip_nonnegative)
        current_loss <- metric_value(cfg$metric, base, val, current_val$forecast, cfg$seasonality)
        best_val <- select_form_validation(
          base, val, cfg$seasonality,
          metric = cfg$metric,
          allow_multiplicative = cfg$allow_multiplicative,
          clip_nonnegative = cfg$clip_nonnegative,
          current_form = current_form
        )
        monitor_seconds <- as.numeric(current_val$fit_seconds) + as.numeric(best_val$fit_seconds)
        current_validation_loss <- current_loss
        challenger_validation_loss <- best_val$loss
        challenger_form <- as.character(best_val$form)
        deployed_weight_aicc <- best_val$deployed_weight_aicc
        spec_debt_aicc <- best_val$spec_debt_aicc
        best_ic_form_aicc <- best_val$best_ic_form_aicc
        best_weight_aicc <- best_val$best_weight_aicc
        deployed_weight_bic <- best_val$deployed_weight_bic
        spec_debt_bic <- best_val$spec_debt_bic
        best_ic_form_bic <- best_val$best_ic_form_bic
        best_weight_bic <- best_val$best_weight_bic
        if (is.finite(current_loss) && is.finite(best_val$loss)) score_gap <- current_loss - best_val$loss
        if (is.finite(score_gap) && score_gap > threshold && !identical(as.character(best_val$form), as.character(current_form))) {
          current_form <- best_val$form
          last_respec_idx <- idx
          respecified <- TRUE
          triggered_by_score <- TRUE
          trigger_reason <- "score"
        }
      }
      outcome <- fit_form_es(train, current_form, cfg$horizon, cfg$seasonality, cfg$clip_nonnegative)
      outcome$fit_seconds <- as.numeric(outcome$fit_seconds) + monitor_seconds
    }

    loss <- metric_value(cfg$metric, train, truth, outcome$forecast, cfg$seasonality)
    records[[idx]] <- make_record(
      series_id, policy_name, idx - 1L, origin, current_form, truth, outcome$forecast,
      loss, outcome$fit_seconds, respecified,
      score_gap = score_gap, threshold = threshold, tau_cost_ratio = threshold,
      trigger_reason = trigger_reason,
      triggered_by_score = triggered_by_score,
      triggered_by_cap = triggered_by_cap,
      age_before_action = age_before_action,
      current_validation_loss = current_validation_loss,
      challenger_validation_loss = challenger_validation_loss,
      challenger_form = challenger_form,
      deployed_weight_aicc = deployed_weight_aicc,
      spec_debt_aicc = spec_debt_aicc,
      best_ic_form_aicc = best_ic_form_aicc,
      best_weight_aicc = best_weight_aicc,
      deployed_weight_bic = deployed_weight_bic,
      spec_debt_bic = spec_debt_bic,
      best_ic_form_bic = best_ic_form_bic,
      best_weight_bic = best_weight_bic
    )
  }

  data.table::rbindlist(records, fill = TRUE)
}


run_all_policies_for_series <- function(
    series_id,
    values,
    cfg,
    fixed_frequencies,
    adaptive_thresholds = numeric(),
    adaptive_caps = integer(),
    include_pure_adaptive = FALSE,
    include_capped_adaptive = TRUE) {

  requireNamespace("data.table", quietly = TRUE)
  y <- clean_numeric(values)
  if (length(y) < cfg$train_length + cfg$horizon) return(data.frame())

  blocks <- list(
    run_full_update(series_id, y, cfg),
    run_parameter_only(series_id, y, cfg)
  )

  for (f in fixed_frequencies) {
    if (as.integer(f) != 1L) {
      blocks[[length(blocks) + 1L]] <- run_fixed_frequency(series_id, y, cfg, f)
    }
  }

  if (isTRUE(include_pure_adaptive) && length(adaptive_thresholds) > 0L) {
    for (tau in adaptive_thresholds) {
      blocks[[length(blocks) + 1L]] <- run_adaptive_score(series_id, y, cfg, tau)
    }
  }

  if (isTRUE(include_capped_adaptive) && length(adaptive_thresholds) > 0L && length(adaptive_caps) > 0L) {
    for (cap in adaptive_caps) {
      for (tau in adaptive_thresholds) {
        blocks[[length(blocks) + 1L]] <- run_adaptive_capped_score(series_id, y, cfg, threshold = tau, max_age = cap)
      }
    }
  }

  data.table::rbindlist(blocks, fill = TRUE)
}

parallel_lapply <- function(items, fun, n_jobs = 1L) {
  n_jobs <- max(1L, as.integer(n_jobs))
  if (n_jobs <= 1L) return(lapply(items, fun))
  if (.Platform$OS.type == "unix") {
    return(parallel::mclapply(items, fun, mc.cores = n_jobs))
  }
  warning("Parallel execution with n_jobs > 1 is only enabled by default on Unix-like systems. Running serially.")
  lapply(items, fun)
}

safe_run_series <- function(item, cfg, fixed_frequencies, adaptive_thresholds, adaptive_caps,
                            include_pure_adaptive, include_capped_adaptive) {
  out <- try(
    run_all_policies_for_series(
      series_id = item$series_id,
      values = item$values,
      cfg = cfg,
      fixed_frequencies = fixed_frequencies,
      adaptive_thresholds = adaptive_thresholds,
      adaptive_caps = adaptive_caps,
      include_pure_adaptive = include_pure_adaptive,
      include_capped_adaptive = include_capped_adaptive
    ),
    silent = TRUE
  )
  if (inherits(out, "try-error")) {
    message("Series failed: ", item$series_id, " | ", as.character(out)[1L])
    return(data.frame())
  }
  out
}

# -----------------------------
# Summary, plotting, and diagnostics
# -----------------------------

compute_instability <- function(records) {
  requireNamespace("data.table", quietly = TRUE)
  dt <- data.table::as.data.table(records)
  fcols <- grep("^f[0-9]+$", names(dt), value = TRUE)
  if (length(fcols) == 0L) {
    return(dt[, .(instability = NA_real_), by = .(policy, series_id)])
  }

  data.table::setorder(dt, policy, series_id, origin_number)
  groups <- unique(dt[, .(policy, series_id)])
  rows <- vector("list", nrow(groups))

  for (i in seq_len(nrow(groups))) {
    g <- dt[policy == groups$policy[i] & series_id == groups$series_id[i]]
    prev <- NULL
    vals <- numeric()
    for (j in seq_len(nrow(g))) {
      origin <- as.integer(g$origin_index[j])
      curr <- list()
      for (k in seq_along(fcols)) {
        val <- as.numeric(g[[fcols[[k]]]][j])
        if (is.finite(val)) curr[[as.character(origin + k - 1L)]] <- val
      }
      if (!is.null(prev)) {
        common <- intersect(names(prev), names(curr))
        if (length(common) > 0L) {
          vals <- c(vals, smapc_between_forecasts(unlist(prev[common]), unlist(curr[common])))
        }
      }
      prev <- curr
    }
    rows[[i]] <- data.table::data.table(
      policy = groups$policy[i],
      series_id = groups$series_id[i],
      instability = safe_mean(vals)
    )
  }

  data.table::rbindlist(rows, fill = TRUE)
}

policy_sort_key <- function(policy) {
  policy <- as.character(policy)
  if (policy == "full_update") return(c(0, 1))
  if (policy == "parameter_only") return(c(1, 9999))
  if (startsWith(policy, "fixed_f")) {
    val <- suppressWarnings(as.numeric(sub("^fixed_f", "", policy)))
    if (!is.finite(val)) val <- 9999
    return(c(2, val))
  }
  if (startsWith(policy, "adaptive_cap")) {
    cap <- suppressWarnings(as.numeric(sub("^adaptive_cap([0-9]+).*", "\\1", policy)))
    tau <- suppressWarnings(as.numeric(sub("^adaptive_cap[0-9]+_tau", "", policy)))
    if (!is.finite(cap)) cap <- 9999
    if (!is.finite(tau)) tau <- 9999
    return(c(3 + cap / 1000, tau))
  }
  if (startsWith(policy, "adaptive_tau")) {
    val <- suppressWarnings(as.numeric(sub("^adaptive_tau", "", policy)))
    if (!is.finite(val)) val <- 9999
    return(c(4, val))
  }
  c(9, 9999)
}

summarize_records <- function(records, alpha = 0.0, gamma = 0.0) {
  requireNamespace("data.table", quietly = TRUE)
  dt <- data.table::as.data.table(records)

  base <- dt[, .(
    mean_loss = mean(loss, na.rm = TRUE),
    total_fit_seconds = sum(fit_seconds, na.rm = TRUE),
    mean_fit_seconds = mean(fit_seconds, na.rm = TRUE),
    respecifications = sum(respecified, na.rm = TRUE),
    n_records = .N,
    n_series = data.table::uniqueN(series_id)
  ), by = policy]

  inst <- compute_instability(dt)
  inst_summary <- inst[, .(mean_instability = mean(instability, na.rm = TRUE)), by = policy]
  out <- merge(base, inst_summary, by = "policy", all.x = TRUE)

  ref <- out[policy == "full_update"]
  if (nrow(ref) == 0L) ref <- out[1L]
  ref_loss <- as.numeric(ref$mean_loss[1L])
  ref_time <- as.numeric(ref$total_fit_seconds[1L])
  ref_inst <- as.numeric(ref$mean_instability[1L])

  out[, relative_loss := if (is.finite(ref_loss) && ref_loss > 0) mean_loss / ref_loss else NA_real_]
  out[, relative_time := if (is.finite(ref_time) && ref_time > 0) total_fit_seconds / ref_time else NA_real_]
  out[, relative_instability := if (is.finite(ref_inst) && ref_inst > 0) {
    mean_instability / ref_inst
  } else ifelse(is.finite(mean_instability) & abs(mean_instability) <= 1e-12, 1.0, NA_real_)]
  out[, mean_respecifications_per_series := respecifications / pmax(n_series, 1L)]

  loss_component <- out$relative_loss
  time_component <- if (as.numeric(alpha) == 0) rep(0.0, nrow(out)) else as.numeric(alpha) * out$relative_time
  instability_component <- if (as.numeric(gamma) == 0) rep(0.0, nrow(out)) else as.numeric(gamma) * out$relative_instability
  out[, total_cost_index := loss_component + time_component + instability_component]

  data.table::setorder(out, total_cost_index, relative_loss, relative_time)
  out
}

write_latex_table <- function(summary, path) {
  ensure_dir(dirname(path))
  dt <- data.table::copy(data.table::as.data.table(summary))
  dt[, sort_major := vapply(policy, function(p) policy_sort_key(p)[1L], numeric(1L))]
  dt[, sort_minor := vapply(policy, function(p) policy_sort_key(p)[2L], numeric(1L))]
  data.table::setorder(dt, sort_major, sort_minor)

  cols <- c("policy", "relative_loss", "relative_time", "relative_instability",
            "mean_respecifications_per_series", "total_cost_index")
  dt <- dt[, cols, with = FALSE]
  names(dt) <- c("Policy", "Rel. loss", "Rel. time", "Rel. instability", "Mean re-spec.", "Cost index")

  fmt <- function(x) ifelse(is.finite(x), sprintf("%.3f", x), "NA")
  lines <- c(
    "\\begin{tabular}{lrrrrr}",
    "\\toprule",
    "Policy & Rel. loss & Rel. time & Rel. instability & Mean re-spec. & Cost index \\\\",
    "\\midrule"
  )
  for (i in seq_len(nrow(dt))) {
    lines <- c(lines, paste0(
      dt$Policy[i], " & ", fmt(dt[["Rel. loss"]][i]), " & ", fmt(dt[["Rel. time"]][i]), " & ",
      fmt(dt[["Rel. instability"]][i]), " & ", fmt(dt[["Mean re-spec."]][i]), " & ",
      fmt(dt[["Cost index"]][i]), " \\\\")
    )
  }
  lines <- c(lines, "\\bottomrule", "\\end{tabular}")
  writeLines(lines, path)
  invisible(path)
}

plot_frontier <- function(summary, path) {
  requireNamespace("ggplot2", quietly = TRUE)
  ensure_dir(dirname(path))
  df <- as.data.frame(summary)
  df$policy_type <- ifelse(startsWith(df$policy, "adaptive_cap"), "capped adaptive",
                           ifelse(startsWith(df$policy, "adaptive_tau"), "pure adaptive",
                                  ifelse(startsWith(df$policy, "fixed"), "fixed", "other")))
  p <- ggplot2::ggplot(df, ggplot2::aes(x = relative_time, y = relative_loss, label = policy, shape = policy_type)) +
    ggplot2::geom_point(size = 2.5) +
    ggplot2::geom_text(size = 3, nudge_x = 0.02, nudge_y = 0.02, check_overlap = TRUE) +
    ggplot2::labs(x = "Relative computational time", y = "Relative forecast loss", title = "Cost accuracy frontier") +
    ggplot2::theme_minimal()
  ggplot2::ggsave(path, plot = p, width = 8, height = 5, dpi = 200)
  invisible(path)
}

plot_update_counts <- function(summary, path) {
  requireNamespace("ggplot2", quietly = TRUE)
  ensure_dir(dirname(path))
  dt <- data.table::copy(data.table::as.data.table(summary))
  dt[, sort_major := vapply(policy, function(p) policy_sort_key(p)[1L], numeric(1L))]
  dt[, sort_minor := vapply(policy, function(p) policy_sort_key(p)[2L], numeric(1L))]
  data.table::setorder(dt, sort_major, sort_minor)
  dt$policy <- factor(dt$policy, levels = dt$policy)
  p <- ggplot2::ggplot(as.data.frame(dt), ggplot2::aes(x = policy, y = mean_respecifications_per_series)) +
    ggplot2::geom_col() +
    ggplot2::labs(x = "Policy", y = "Mean re-specifications per series", title = "Model-form update activity") +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  ggplot2::ggsave(path, plot = p, width = 9, height = 5, dpi = 200)
  invisible(path)
}

plot_score_gap_example <- function(records, path, series_id = NULL, policy_prefix = "adaptive") {
  requireNamespace("ggplot2", quietly = TRUE)
  dt <- data.table::as.data.table(records)
  if (!("score_gap" %in% names(dt))) return(invisible(NULL))
  dt <- dt[startsWith(policy, policy_prefix) & is.finite(score_gap)]
  if (nrow(dt) == 0L) return(invisible(NULL))
  if (is.null(series_id)) {
    series_id <- dt[, .(max_gap = max(score_gap, na.rm = TRUE)), by = series_id][order(-max_gap)]$series_id[1L]
  }
  sid <- series_id
  g <- dt[series_id == sid]
  if (nrow(g) == 0L) return(invisible(NULL))
  p <- ggplot2::ggplot(as.data.frame(g), ggplot2::aes(x = origin_number, y = score_gap, color = policy)) +
    ggplot2::geom_line() +
    ggplot2::geom_point(size = 1.5) +
    ggplot2::labs(x = "Rolling-origin round", y = "Validation score gap", title = paste("Score-gap trajectory:", series_id)) +
    ggplot2::theme_minimal()
  ensure_dir(dirname(path))
  ggplot2::ggsave(path, plot = p, width = 8, height = 4, dpi = 200)
  invisible(path)
}

validate_records <- function(records) {
  dt <- data.table::as.data.table(records)
  needed <- c(
    "score_gap", "spec_debt_aicc", "spec_debt_bic", "deployed_weight_aicc",
    "deployed_weight_bic", "triggered_by_score", "triggered_by_cap", "tau_cost_ratio"
  )
  for (nm in needed) if (!(nm %in% names(dt))) dt[, (nm) := NA_real_]

  out <- dt[, .(
    n_forms = data.table::uniqueN(form),
    example_forms = paste(utils::head(sort(unique(form)), 12L), collapse = ", "),
    mean_loss = mean(loss, na.rm = TRUE),
    sd_loss = stats::sd(loss, na.rm = TRUE),
    n_monitored = sum(is.finite(score_gap) | is.finite(spec_debt_aicc) | is.finite(spec_debt_bic), na.rm = TRUE),
    mean_score_gap = safe_mean(score_gap),
    max_score_gap = safe_max(score_gap),
    mean_spec_debt_aicc = safe_mean(spec_debt_aicc),
    median_spec_debt_aicc = ifelse(all(!is.finite(spec_debt_aicc)), NA_real_, stats::median(spec_debt_aicc, na.rm = TRUE)),
    mean_deployed_weight_aicc = safe_mean(deployed_weight_aicc),
    mean_spec_debt_bic = safe_mean(spec_debt_bic),
    median_spec_debt_bic = ifelse(all(!is.finite(spec_debt_bic)), NA_real_, stats::median(spec_debt_bic, na.rm = TRUE)),
    mean_deployed_weight_bic = safe_mean(deployed_weight_bic),
    score_gap_debt_cor_aicc = if (sum(is.finite(score_gap) & is.finite(spec_debt_aicc)) >= 3L) {
      suppressWarnings(stats::cor(score_gap, spec_debt_aicc, use = "complete.obs", method = "spearman"))
    } else NA_real_,
    score_gap_debt_cor_bic = if (sum(is.finite(score_gap) & is.finite(spec_debt_bic)) >= 3L) {
      suppressWarnings(stats::cor(score_gap, spec_debt_bic, use = "complete.obs", method = "spearman"))
    } else NA_real_,
    score_triggers = sum(triggered_by_score == 1L, na.rm = TRUE),
    cap_triggers = sum(triggered_by_cap == 1L, na.rm = TRUE),
    mean_tau_cost_ratio = safe_mean(tau_cost_ratio)
  ), by = policy][order(policy)]
  out
}

check_policy_forecast_diversity <- function(records_path) {
  requireNamespace("data.table", quietly = TRUE)
  dt <- data.table::fread(records_path)
  validate_records(dt)
}

make_spec_debt_bridge <- function(records) {
  dt <- data.table::as.data.table(records)
  needed <- c(
    "score_gap", "threshold", "tau_cost_ratio", "triggered_by_score", "triggered_by_cap",
    "current_validation_loss", "challenger_validation_loss", "challenger_form",
    "deployed_weight_aicc", "spec_debt_aicc", "best_ic_form_aicc", "best_weight_aicc",
    "deployed_weight_bic", "spec_debt_bic", "best_ic_form_bic", "best_weight_bic"
  )
  for (nm in needed) if (!(nm %in% names(dt))) dt[, (nm) := NA]
  bridge <- dt[is.finite(score_gap) | is.finite(spec_debt_aicc) | is.finite(spec_debt_bic), .(
    series_id, policy, origin_number, origin_index, form, challenger_form,
    score_gap, threshold, tau_cost_ratio, triggered_by_score, triggered_by_cap,
    current_validation_loss, challenger_validation_loss,
    deployed_weight_aicc, spec_debt_aicc, best_ic_form_aicc, best_weight_aicc,
    deployed_weight_bic, spec_debt_bic, best_ic_form_bic, best_weight_bic
  )]
  bridge[]
}

summarize_spec_debt_bridge <- function(records) {
  bridge <- make_spec_debt_bridge(records)
  if (nrow(bridge) == 0L) return(data.table::data.table())
  bridge[, .(
    n_monitoring_points = .N,
    n_series = data.table::uniqueN(series_id),
    score_triggers = sum(triggered_by_score == 1L, na.rm = TRUE),
    mean_score_gap = safe_mean(score_gap),
    median_score_gap = ifelse(all(!is.finite(score_gap)), NA_real_, stats::median(score_gap, na.rm = TRUE)),
    mean_spec_debt_aicc = safe_mean(spec_debt_aicc),
    median_spec_debt_aicc = ifelse(all(!is.finite(spec_debt_aicc)), NA_real_, stats::median(spec_debt_aicc, na.rm = TRUE)),
    mean_deployed_weight_aicc = safe_mean(deployed_weight_aicc),
    mean_spec_debt_bic = safe_mean(spec_debt_bic),
    median_spec_debt_bic = ifelse(all(!is.finite(spec_debt_bic)), NA_real_, stats::median(spec_debt_bic, na.rm = TRUE)),
    mean_deployed_weight_bic = safe_mean(deployed_weight_bic),
    spearman_score_gap_debt_aicc = if (sum(is.finite(score_gap) & is.finite(spec_debt_aicc)) >= 3L) {
      suppressWarnings(stats::cor(score_gap, spec_debt_aicc, use = "complete.obs", method = "spearman"))
    } else NA_real_,
    spearman_score_gap_debt_bic = if (sum(is.finite(score_gap) & is.finite(spec_debt_bic)) >= 3L) {
      suppressWarnings(stats::cor(score_gap, spec_debt_bic, use = "complete.obs", method = "spearman"))
    } else NA_real_
  ), by = policy][order(policy)]
}

triggered_subset_analysis <- function(records,
                                      adaptive_policy = "adaptive_cap8_tau0.8",
                                      fixed_policy = "fixed_f8") {
  dt <- data.table::as.data.table(records)
  if (!("triggered_by_score" %in% names(dt))) return(data.table::data.table())
  if (!(adaptive_policy %in% dt$policy) || !(fixed_policy %in% dt$policy)) return(data.table::data.table())

  a <- dt[policy == adaptive_policy, .(
    series_id, origin_number, adaptive_loss = loss,
    adaptive_respecified = respecified,
    adaptive_triggered_by_score = triggered_by_score,
    adaptive_triggered_by_cap = triggered_by_cap,
    score_gap, spec_debt_aicc, spec_debt_bic
  )]
  f <- dt[policy == fixed_policy, .(series_id, origin_number, fixed_loss = loss)]
  joined <- merge(a, f, by = c("series_id", "origin_number"), all.x = TRUE)
  joined[, loss_diff_adaptive_minus_fixed := adaptive_loss - fixed_loss]
  triggered_series <- joined[adaptive_triggered_by_score == 1L, unique(series_id)]
  joined[, subset := ifelse(series_id %in% triggered_series, "series_with_score_trigger", "series_without_score_trigger")]

  by_series_status <- joined[, .(
    n_series = data.table::uniqueN(series_id),
    n_records = .N,
    n_score_trigger_records = sum(adaptive_triggered_by_score == 1L, na.rm = TRUE),
    mean_adaptive_loss = mean(adaptive_loss, na.rm = TRUE),
    mean_fixed_loss = mean(fixed_loss, na.rm = TRUE),
    mean_loss_diff_adaptive_minus_fixed = mean(loss_diff_adaptive_minus_fixed, na.rm = TRUE),
    median_loss_diff_adaptive_minus_fixed = stats::median(loss_diff_adaptive_minus_fixed, na.rm = TRUE),
    mean_score_gap = safe_mean(score_gap),
    mean_spec_debt_aicc = safe_mean(spec_debt_aicc),
    mean_spec_debt_bic = safe_mean(spec_debt_bic)
  ), by = subset]

  exact_trigger_records <- joined[adaptive_triggered_by_score == 1L, .(
    subset = "score_trigger_records",
    n_series = data.table::uniqueN(series_id),
    n_records = .N,
    n_score_trigger_records = .N,
    mean_adaptive_loss = mean(adaptive_loss, na.rm = TRUE),
    mean_fixed_loss = mean(fixed_loss, na.rm = TRUE),
    mean_loss_diff_adaptive_minus_fixed = mean(loss_diff_adaptive_minus_fixed, na.rm = TRUE),
    median_loss_diff_adaptive_minus_fixed = stats::median(loss_diff_adaptive_minus_fixed, na.rm = TRUE),
    mean_score_gap = safe_mean(score_gap),
    mean_spec_debt_aicc = safe_mean(spec_debt_aicc),
    mean_spec_debt_bic = safe_mean(spec_debt_bic)
  )]

  overall <- joined[, .(
    subset = "all_records",
    n_series = data.table::uniqueN(series_id),
    n_records = .N,
    n_score_trigger_records = sum(adaptive_triggered_by_score == 1L, na.rm = TRUE),
    mean_adaptive_loss = mean(adaptive_loss, na.rm = TRUE),
    mean_fixed_loss = mean(fixed_loss, na.rm = TRUE),
    mean_loss_diff_adaptive_minus_fixed = mean(loss_diff_adaptive_minus_fixed, na.rm = TRUE),
    median_loss_diff_adaptive_minus_fixed = stats::median(loss_diff_adaptive_minus_fixed, na.rm = TRUE),
    mean_score_gap = safe_mean(score_gap),
    mean_spec_debt_aicc = safe_mean(spec_debt_aicc),
    mean_spec_debt_bic = safe_mean(spec_debt_bic)
  )]

  data.table::rbindlist(list(overall, by_series_status, exact_trigger_records), fill = TRUE)
}

plot_spec_debt_bridge <- function(records, path, policy = "adaptive_cap8_tau0.8", max_points = 5000L) {
  requireNamespace("ggplot2", quietly = TRUE)
  dt <- make_spec_debt_bridge(records)
  if (nrow(dt) == 0L) return(invisible(NULL))
  pol <- policy
  if (!is.null(pol) && pol %in% dt$policy) dt <- dt[policy == pol]
  dt <- dt[is.finite(score_gap) & is.finite(spec_debt_aicc)]
  if (nrow(dt) == 0L) return(invisible(NULL))
  if (nrow(dt) > max_points) {
    set.seed(123L)
    dt <- dt[sample(seq_len(nrow(dt)), max_points)]
  }
  dt[, triggered := factor(ifelse(triggered_by_score == 1L, "Score-triggered", "Monitored only"),
                           levels = c("Monitored only", "Score-triggered"))]
  p <- ggplot2::ggplot(as.data.frame(dt), ggplot2::aes(x = spec_debt_aicc, y = score_gap, shape = triggered)) +
    ggplot2::geom_point(alpha = 0.55, size = 1.8) +
    ggplot2::geom_hline(ggplot2::aes(yintercept = threshold), linetype = "dashed", linewidth = 0.4, alpha = 0.65) +
    ggplot2::labs(
      x = "AICc-weight specification debt, -log weight of deployed form",
      y = "Validation score gap",
      title = "Score-gap surrogate versus closed-grid specification debt",
      subtitle = paste0("Policy: ", pol, ". Dashed line is the score-gap threshold."),
      shape = NULL
    ) +
    ggplot2::theme_minimal(base_size = 12)
  ensure_dir(dirname(path))
  ggplot2::ggsave(path, plot = p, width = 7.8, height = 4.8, dpi = 250)
  invisible(path)
}

plot_triggered_series_effects <- function(records, path,
                                          adaptive_policy = "adaptive_cap8_tau0.8",
                                          fixed_policy = "fixed_f8") {
  requireNamespace("ggplot2", quietly = TRUE)
  dt <- data.table::as.data.table(records)
  if (!(adaptive_policy %in% dt$policy) || !(fixed_policy %in% dt$policy)) return(invisible(NULL))
  a <- dt[policy == adaptive_policy, .(series_id, origin_number, adaptive_loss = loss, triggered_by_score)]
  f <- dt[policy == fixed_policy, .(series_id, origin_number, fixed_loss = loss)]
  ser <- merge(a, f, by = c("series_id", "origin_number"), all.x = TRUE)
  ser[, loss_diff_adaptive_minus_fixed := adaptive_loss - fixed_loss]
  triggered_series <- ser[triggered_by_score == 1L, unique(series_id)]
  ser[, group := ifelse(series_id %in% triggered_series, "At least one early score trigger", "No early score trigger")]
  p <- ggplot2::ggplot(as.data.frame(ser), ggplot2::aes(x = group, y = loss_diff_adaptive_minus_fixed)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
    ggplot2::geom_boxplot(outlier.alpha = 0.35) +
    ggplot2::labs(
      x = NULL,
      y = paste0(adaptive_policy, " loss minus ", fixed_policy, " loss"),
      title = "Where does the adaptive exception help?"
    ) +
    ggplot2::theme_minimal(base_size = 11)
  ensure_dir(dirname(path))
  ggplot2::ggsave(path, plot = p, width = 7.2, height = 4.6, dpi = 200)
  invisible(path)
}

write_experiment_outputs <- function(records, out_dir, alpha = 0.0, gamma = 0.0) {
  check_required_packages()
  requireNamespace("data.table", quietly = TRUE)
  ensure_dir(out_dir)

  records_path <- file.path(out_dir, "records.csv")
  summary_path <- file.path(out_dir, "summary.csv")
  diagnostics_path <- file.path(out_dir, "diagnostics.csv")
  spec_debt_bridge_path <- file.path(out_dir, "spec_debt_bridge.csv")
  spec_debt_bridge_summary_path <- file.path(out_dir, "spec_debt_bridge_summary.csv")
  triggered_subset_path <- file.path(out_dir, "triggered_subset.csv")

  data.table::fwrite(records, records_path)
  summary <- summarize_records(records, alpha = alpha, gamma = gamma)
  diagnostics <- validate_records(records)
  spec_debt_bridge <- make_spec_debt_bridge(records)
  spec_debt_bridge_summary <- summarize_spec_debt_bridge(records)
  triggered_subset <- triggered_subset_analysis(records)

  data.table::fwrite(summary, summary_path)
  data.table::fwrite(diagnostics, diagnostics_path)
  data.table::fwrite(spec_debt_bridge, spec_debt_bridge_path)
  data.table::fwrite(spec_debt_bridge_summary, spec_debt_bridge_summary_path)
  data.table::fwrite(triggered_subset, triggered_subset_path)
  write_latex_table(summary, file.path(out_dir, "summary_table.tex"))
  plot_frontier(summary, file.path(out_dir, "frontier.png"))
  plot_update_counts(summary, file.path(out_dir, "update_counts.png"))
  plot_score_gap_example(records, file.path(out_dir, "score_gap_example.png"))
  plot_spec_debt_bridge(records, file.path(out_dir, "spec_debt_bridge.png"))
  plot_triggered_series_effects(records, file.path(out_dir, "triggered_series_effects.png"))

  message("Wrote ", records_path)
  message("Wrote ", summary_path)
  message("Wrote ", diagnostics_path)
  message("Wrote ", spec_debt_bridge_path)
  message("Wrote ", spec_debt_bridge_summary_path)
  message("Wrote ", triggered_subset_path)
  print(as.data.frame(summary))

  invisible(summary)
}

summarize_existing_records <- function(records_path, out_dir, alpha = 0.0, gamma = 0.0) {
  check_required_packages()
  requireNamespace("data.table", quietly = TRUE)
  records <- data.table::fread(records_path)
  write_experiment_outputs(records, out_dir, alpha = alpha, gamma = gamma)
}

run_m4_experiment <- function(
    data_dir = "data",
    out_dir = "outputs/m4",
    n_series = 100L,
    seed = 123L,
    n_rounds = 36L,
    horizon = 3L,
    seasonality = 12L,
    train_length = 36L,
    fixed_frequencies = 2L:12L,
    adaptive_thresholds = c(0.03, 0.05, 0.10, 0.20, 0.40, 0.80),
    adaptive_caps = c(8L, 12L),
    include_pure_adaptive = FALSE,
    include_capped_adaptive = TRUE,
    monitor_window = 12L,
    monitor_every = 6L,
    n_jobs = 1L,
    alpha = 0.0,
    gamma = 0.0,
    allow_multiplicative = TRUE,
    batch_size = NULL,
    resume = TRUE) {

  check_required_packages()
  requireNamespace("data.table", quietly = TRUE)

  min_obs_needed <- as.integer(train_length) + as.integer(n_rounds) + as.integer(horizon) - 1L
  items <- load_m4_monthly(
    data_dir = data_dir,
    n_series = -1L,
    seed = seed,
    min_obs = min_obs_needed,
    include_test = TRUE
  )

  if (!is.null(n_series) && n_series > 0L && n_series < length(items)) {
    set.seed(seed)
    idx <- sort(sample(seq_along(items), n_series))
    items <- items[idx]
  }

  if (length(items) == 0L) stop("No M4 items loaded. Check data and configuration.")

  cfg <- new_rolling_config(
    horizon = horizon,
    seasonality = seasonality,
    train_length = train_length,
    n_rounds = n_rounds,
    fixed_window = TRUE,
    metric = "mase",
    allow_multiplicative = allow_multiplicative,
    clip_nonnegative = FALSE,
    monitor_window = monitor_window,
    monitor_every = monitor_every
  )

  message("Running M4 experiment on ", length(items), " series.")
  message("Policies: full_update, parameter_only, fixed_f,",
          if (isTRUE(include_pure_adaptive)) " pure adaptive," else "",
          if (isTRUE(include_capped_adaptive)) " capped adaptive" else "")

  run_one <- function(item) {
    safe_run_series(
      item = item,
      cfg = cfg,
      fixed_frequencies = fixed_frequencies,
      adaptive_thresholds = adaptive_thresholds,
      adaptive_caps = adaptive_caps,
      include_pure_adaptive = include_pure_adaptive,
      include_capped_adaptive = include_capped_adaptive
    )
  }

  # Default path: run every series in a single pass, exactly as before.
  if (is.null(batch_size)) {
    blocks <- parallel_lapply(items, run_one, n_jobs = n_jobs)
    records <- data.table::rbindlist(blocks, fill = TRUE)
    if (nrow(records) == 0L) stop("No records produced. Check series lengths and configuration.")
    return(write_experiment_outputs(records, out_dir, alpha = alpha, gamma = gamma))
  }

  # Checkpointed path for very large runs (e.g. all M4 monthly series).
  # Series are independent and each per-series computation is deterministic,
  # so splitting the SAME item list into contiguous batches and concatenating
  # the per-batch records reproduces the single-pass result exactly. mclapply
  # preserves input order, and the summaries are group-by aggregations that do
  # not depend on row order. Batching only bounds peak memory during the run
  # and lets an interrupted run resume. It does not change which series are
  # run, the seeds, the protocol, or any reported number.
  batch_size <- max(1L, as.integer(batch_size))
  ensure_dir(out_dir)
  parts_dir <- file.path(out_dir, "parts")
  ensure_dir(parts_dir)

  n_items <- length(items)
  starts <- seq.int(1L, n_items, by = batch_size)
  n_batches <- length(starts)
  message("Checkpointed run: ", n_items, " series in ", n_batches,
          " batch(es) of up to ", batch_size, ". Parts in ", parts_dir)

  for (b in seq_len(n_batches)) {
    lo <- starts[[b]]
    hi <- min(lo + batch_size - 1L, n_items)
    part_path <- file.path(parts_dir, sprintf("records_part_%05d.csv", b))
    if (isTRUE(resume) && file.exists(part_path)) {
      message("  batch ", b, "/", n_batches, " (series ", lo, "-", hi, "): exists, skipping")
      next
    }
    t_batch <- Sys.time()
    blocks <- parallel_lapply(items[lo:hi], run_one, n_jobs = n_jobs)
    batch_records <- data.table::rbindlist(blocks, fill = TRUE)
    # Write atomically so an interrupted write never leaves a partial part
    # that a later resume would mistake for a finished batch.
    tmp_path <- paste0(part_path, ".tmp")
    data.table::fwrite(batch_records, tmp_path)
    file.rename(tmp_path, part_path)
    message("  batch ", b, "/", n_batches, " (series ", lo, "-", hi, "): ",
            nrow(batch_records), " rows, ",
            round(as.numeric(difftime(Sys.time(), t_batch, units = "mins")), 1), " min")
    rm(blocks, batch_records)
    gc(verbose = FALSE)
  }

  part_files <- sort(list.files(parts_dir, pattern = "^records_part_[0-9]+\\.csv$", full.names = TRUE))
  if (length(part_files) == 0L) stop("No record parts were written.")
  records <- data.table::rbindlist(lapply(part_files, data.table::fread), fill = TRUE)
  if (nrow(records) == 0L) stop("No records produced. Check series lengths and configuration.")
  write_experiment_outputs(records, out_dir, alpha = alpha, gamma = gamma)
}

run_m4_fixed_grid_calibration <- function(
    data_dir = "data",
    out_dir = "outputs/m4_100_fixed_grid",
    n_series = 100L,
    seed = 123L,
    n_jobs = 1L) {
  run_m4_experiment(
    data_dir = data_dir,
    out_dir = out_dir,
    n_series = n_series,
    seed = seed,
    n_rounds = 36L,
    horizon = 3L,
    seasonality = 12L,
    train_length = 36L,
    fixed_frequencies = 2L:12L,
    adaptive_thresholds = numeric(),
    adaptive_caps = integer(),
    include_pure_adaptive = FALSE,
    include_capped_adaptive = FALSE,
    monitor_window = 12L,
    monitor_every = 6L,
    n_jobs = n_jobs,
    alpha = 0.0,
    gamma = 0.0,
    allow_multiplicative = TRUE
  )
}

run_m4_capped_calibration <- function(
    data_dir = "data",
    out_dir = "outputs/m4_100_capped",
    n_series = 100L,
    seed = 123L,
    n_jobs = 1L) {
  run_m4_experiment(
    data_dir = data_dir,
    out_dir = out_dir,
    n_series = n_series,
    seed = seed,
    n_rounds = 36L,
    horizon = 3L,
    seasonality = 12L,
    train_length = 36L,
    fixed_frequencies = 2L:12L,
    adaptive_thresholds = c(0.03, 0.05, 0.10, 0.20, 0.40, 0.80),
    adaptive_caps = c(8L, 12L),
    include_pure_adaptive = FALSE,
    include_capped_adaptive = TRUE,
    monitor_window = 12L,
    monitor_every = 6L,
    n_jobs = n_jobs,
    alpha = 0.0,
    gamma = 0.0,
    allow_multiplicative = TRUE
  )
}

message("Loaded adaptive_update.R. Use check_m4_loader(), check_ets_smoke(), run_m4_experiment(). Outputs include IC-weight specification-debt diagnostics.")
