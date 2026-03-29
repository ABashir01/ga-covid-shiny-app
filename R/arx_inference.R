.arx_source_file <- tryCatch(
  normalizePath(sys.frame(1)$ofile, winslash = "/", mustWork = FALSE),
  error = function(...) ""
)

project_root_dir <- function() {
  candidates <- c(
    if (nzchar(.arx_source_file)) dirname(dirname(.arx_source_file)) else "",
    getwd(),
    file.path(getwd(), "..")
  )

  for (candidate in candidates) {
    if (!nzchar(candidate)) {
      next
    }

    resolved <- normalizePath(candidate, winslash = "/", mustWork = FALSE)
    model_candidate <- file.path(resolved, "arx_model", "output", "covid_prediction_model.rds")
    if (file.exists(model_candidate)) {
      return(resolved)
    }
  }

  normalizePath(getwd(), winslash = "/", mustWork = FALSE)
}

resolve_project_path <- function(...) {
  file.path(project_root_dir(), ...)
}

model_artifact_path <- function() {
  resolve_project_path("arx_model", "output", "covid_prediction_model.rds")
}

model_metrics_path <- function() {
  resolve_project_path("arx_model", "analysis_output", "model_accuracy_metrics.csv")
}

prediction_history_path <- function() {
  resolve_project_path("arx_model", "analysis_output", "2025_predictions.csv")
}

live_prediction_history_path <- function() {
  resolve_project_path("arx_model", "analysis_output", "live_predictions.csv")
}

coerce_numeric <- function(value, field_name) {
  numeric_value <- suppressWarnings(as.numeric(value))
  if (length(numeric_value) != 1 || is.na(numeric_value)) {
    stop(sprintf("`%s` must be a single numeric value.", field_name), call. = FALSE)
  }
  numeric_value
}

coerce_date <- function(value, field_name) {
  date_value <- as.Date(value)
  if (length(date_value) != 1 || is.na(date_value)) {
    stop(sprintf("`%s` must be an ISO date like YYYY-MM-DD.", field_name), call. = FALSE)
  }
  date_value
}

compute_std_dev <- function(values) {
  mean_value <- mean(values)
  sqrt(mean((values - mean_value) ^ 2))
}

build_feature_row <- function(week,
                              weekly_rate_current,
                              wastewater_t,
                              wastewater_t_minus_1,
                              wastewater_t_minus_2,
                              wastewater_t_minus_3) {
  week_number <- as.numeric(format(week, "%U"))
  wastewater_change_2w <- wastewater_t - wastewater_t_minus_2
  wastewater_seasonal <- sin(2 * pi * week_number / 52)
  wastewater_cos_seasonal <- cos(2 * pi * week_number / 52)

  wastewater_vol_2w <- compute_std_dev(c(wastewater_t, wastewater_t_minus_1))
  wastewater_vol_3w <- compute_std_dev(c(wastewater_t, wastewater_t_minus_1, wastewater_t_minus_2))
  wastewater_vol_4w <- compute_std_dev(c(wastewater_t, wastewater_t_minus_1, wastewater_t_minus_2, wastewater_t_minus_3))

  data.frame(
    WeeklyRate = weekly_rate_current,
    wastewater_current = wastewater_t,
    wastewater_change_2w = wastewater_change_2w,
    wastewater_seasonal = wastewater_seasonal,
    wastewater_cos_seasonal = wastewater_cos_seasonal,
    wastewater_vol_2w = wastewater_vol_2w,
    wastewater_vol_3w = wastewater_vol_3w,
    wastewater_vol_4w = wastewater_vol_4w
  )
}

normalize_payload <- function(payload) {
  required_fields <- c(
    "week",
    "weekly_rate_current",
    "wastewater_t",
    "wastewater_t_minus_1",
    "wastewater_t_minus_2",
    "wastewater_t_minus_3"
  )

  missing_fields <- setdiff(required_fields, names(payload))
  if (length(missing_fields) > 0) {
    stop(
      sprintf(
        "Missing required fields: %s",
        paste(missing_fields, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  list(
    week = coerce_date(payload$week, "week"),
    weekly_rate_current = coerce_numeric(payload$weekly_rate_current, "weekly_rate_current"),
    wastewater_t = coerce_numeric(payload$wastewater_t, "wastewater_t"),
    wastewater_t_minus_1 = coerce_numeric(payload$wastewater_t_minus_1, "wastewater_t_minus_1"),
    wastewater_t_minus_2 = coerce_numeric(payload$wastewater_t_minus_2, "wastewater_t_minus_2"),
    wastewater_t_minus_3 = coerce_numeric(payload$wastewater_t_minus_3, "wastewater_t_minus_3")
  )
}

.arx_model_cache <- NULL

get_arx_model <- function(path = model_artifact_path()) {
  if (!file.exists(path)) {
    stop(sprintf("Model file not found at `%s`.", path), call. = FALSE)
  }

  if (is.null(.arx_model_cache)) {
    .arx_model_cache <<- readRDS(path)
  }

  .arx_model_cache
}

predict_two_week_hospitalization <- function(payload, model = get_arx_model()) {
  parsed <- normalize_payload(payload)
  feature_row <- build_feature_row(
    week = parsed$week,
    weekly_rate_current = parsed$weekly_rate_current,
    wastewater_t = parsed$wastewater_t,
    wastewater_t_minus_1 = parsed$wastewater_t_minus_1,
    wastewater_t_minus_2 = parsed$wastewater_t_minus_2,
    wastewater_t_minus_3 = parsed$wastewater_t_minus_3
  )

  predicted_rate <- as.numeric(predict(model, newdata = feature_row))

  list(
    prediction = predicted_rate,
    feature_row = feature_row,
    week = as.character(parsed$week)
  )
}

get_model_metadata <- function(path = model_artifact_path()) {
  model <- get_arx_model(path)
  file_info <- file.info(path)
  terms <- attr(stats::terms(model), "term.labels")

  list(
    model_type = class(model)[1],
    model_path = path,
    model_size_bytes = as.integer(file_info$size),
    model_last_modified = format(as.POSIXct(file_info$mtime), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    predictor_columns = terms
  )
}

load_model_metrics <- function(path = model_metrics_path()) {
  if (!file.exists(path)) {
    return(data.frame())
  }

  read.csv(path, stringsAsFactors = FALSE)
}

load_prediction_history <- function(path = prediction_history_path()) {
  if (!file.exists(path)) {
    return(data.frame())
  }

  history <- read.csv(path, stringsAsFactors = FALSE)
  if ("week" %in% names(history)) {
    history$week <- as.Date(history$week)
  }
  history
}

load_live_prediction_history <- function(path = live_prediction_history_path()) {
  if (!file.exists(path)) {
    return(data.frame())
  }

  history <- read.csv(path, stringsAsFactors = FALSE)
  date_columns <- c("predictor_week", "target_week")
  for (col in date_columns) {
    if (col %in% names(history)) {
      history[[col]] <- as.Date(history[[col]])
    }
  }
  history
}
