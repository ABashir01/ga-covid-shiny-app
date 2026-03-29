if (!exists("build_feature_row", mode = "function")) {
  pipeline_file <- tryCatch(
    normalizePath(sys.frame(1)$ofile, winslash = "/", mustWork = FALSE),
    error = function(...) ""
  )

  candidates <- c(
    file.path(getwd(), "R", "arx_inference.R"),
    if (nzchar(pipeline_file)) file.path(dirname(pipeline_file), "arx_inference.R") else ""
  )

  loaded <- FALSE
  for (path in candidates) {
    if (!nzchar(path)) {
      next
    }
    resolved <- normalizePath(path, winslash = "/", mustWork = FALSE)
    if (file.exists(resolved)) {
      source(resolved)
      loaded <- TRUE
      break
    }
  }

  if (!loaded) {
    stop("Could not locate R/arx_inference.R for live pipeline.", call. = FALSE)
  }
}

wastewater_source_url <- function() {
  "https://www.cdc.gov/wcms/vizdata/NCEZID_DIDRI/SC2/nwsssc2stateactivitylevelDL.csv"
}

hospitalization_source_url <- function() {
  "https://data.cdc.gov/api/views/6jg4-xsqq/rows.csv?accessType=DOWNLOAD"
}

raw_latest_hospitalization_path <- function() {
  resolve_project_path("data", "hospitalization_rates_latest.csv")
}

raw_latest_wastewater_path <- function() {
  resolve_project_path("data", "wastewater_trends_latest.csv")
}

pick_column <- function(data, candidates, field_name) {
  matched <- intersect(candidates, names(data))
  if (length(matched) == 0) {
    stop(
      sprintf(
        "Could not find `%s`. Expected one of: %s",
        field_name,
        paste(candidates, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  matched[1]
}

normalize_hospitalization <- function(data) {
  state_col <- pick_column(data, c("State", "state"), "state")
  week_col <- pick_column(data, c("_WeekendDate", "Week ending date", "week_ending_date"), "week ending date")
  age_col <- pick_column(data, c("AgeCategory_Legend", "Age Category", "agecat_label"), "age category")
  sex_col <- pick_column(data, c("Sex_Label", "Sex", "sex_label"), "sex")
  race_col <- pick_column(data, c("Race_Label", "Race", "race_label"), "race")
  rate_type_col <- pick_column(data, c("Type", "Rate Type", "rate_type"), "rate type")
  weekly_rate_col <- pick_column(data, c("WeeklyRate", "Weekly Rate", "weekly_rate"), "weekly rate")

  normalized <- data.frame(
    state = data[[state_col]],
    week = as.Date(data[[week_col]]),
    age_category = data[[age_col]],
    sex_label = data[[sex_col]],
    race_label = data[[race_col]],
    rate_type = data[[rate_type_col]],
    weekly_rate = suppressWarnings(as.numeric(data[[weekly_rate_col]])),
    stringsAsFactors = FALSE
  )

  normalized <- normalized[
    normalized$state %in% c("Georgia", "GA") &
      normalized$age_category == "All" &
      normalized$sex_label == "All" &
      normalized$race_label == "All" &
      normalized$rate_type %in% c("Crude Rate", "Observed") &
      !is.na(normalized$week) &
      !is.na(normalized$weekly_rate),
  ]

  if (nrow(normalized) == 0) {
    stop("No Georgia hospitalization rows remained after filtering.", call. = FALSE)
  }

  agg <- aggregate(weekly_rate ~ week, data = normalized, FUN = mean)
  agg <- agg[order(agg$week), ]
  rownames(agg) <- NULL
  agg
}

normalize_wastewater <- function(data) {
  state_col <- pick_column(data, c("State/Territory", "state_territory"), "state/territory")
  week_col <- pick_column(data, c("Week_Ending_Date", "Week Ending Date", "week_ending_date"), "week ending date")
  wval_col <- pick_column(data, c("State/Territory_WVAL", "state_territory_wval"), "state wastewater value")

  period_col <- NULL
  if ("Data_Collection_Period" %in% names(data)) {
    period_col <- "Data_Collection_Period"
  } else if ("data_collection_period" %in% names(data)) {
    period_col <- "data_collection_period"
  }

  normalized <- data.frame(
    state = data[[state_col]],
    week = as.Date(data[[week_col]]),
    period = if (!is.null(period_col)) data[[period_col]] else NA_character_,
    wastewater_value = suppressWarnings(as.numeric(data[[wval_col]])),
    stringsAsFactors = FALSE
  )

  normalized <- normalized[
    normalized$state == "Georgia" &
      !is.na(normalized$week) &
      !is.na(normalized$wastewater_value),
  ]

  if (!is.null(period_col)) {
    all_results <- normalized[normalized$period == "All Results", ]
    if (nrow(all_results) > 0) {
      normalized <- all_results
    }
  }

  if (nrow(normalized) == 0) {
    stop("No Georgia wastewater rows remained after filtering.", call. = FALSE)
  }

  agg <- aggregate(wastewater_value ~ week, data = normalized, FUN = mean)
  agg <- agg[order(agg$week), ]
  rownames(agg) <- NULL
  agg
}

download_latest_source <- function(url, destination_path) {
  data <- read.csv(url, stringsAsFactors = FALSE, check.names = FALSE)
  write.csv(data, destination_path, row.names = FALSE)
  data
}

load_local_source <- function(path) {
  if (!file.exists(path)) {
    stop(sprintf("Local source file missing: `%s`.", path), call. = FALSE)
  }
  read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
}

join_georgia_series <- function(hospitalization, wastewater) {
  merged <- merge(hospitalization, wastewater, by = "week", all = FALSE)
  merged <- merged[order(merged$week), ]
  rownames(merged) <- NULL
  merged
}

build_live_prediction_history <- function(merged_data, model = get_arx_model()) {
  if (nrow(merged_data) < 4) {
    stop("At least 4 joined weekly rows are required to compute volatility features.", call. = FALSE)
  }

  rows <- vector("list", max(0, nrow(merged_data) - 3))
  out_idx <- 1

  for (i in 4:nrow(merged_data)) {
    feature_row <- build_feature_row(
      week = merged_data$week[i],
      weekly_rate_current = merged_data$weekly_rate[i],
      wastewater_t = merged_data$wastewater_value[i],
      wastewater_t_minus_1 = merged_data$wastewater_value[i - 1],
      wastewater_t_minus_2 = merged_data$wastewater_value[i - 2],
      wastewater_t_minus_3 = merged_data$wastewater_value[i - 3]
    )

    predicted <- as.numeric(predict(model, newdata = feature_row))

    if (i + 2 <= nrow(merged_data)) {
      target_week <- merged_data$week[i + 2]
      actual_rate <- merged_data$weekly_rate[i + 2]
    } else {
      target_week <- merged_data$week[i] + 14
      actual_rate <- NA_real_
    }

    error <- if (is.na(actual_rate)) NA_real_ else actual_rate - predicted
    abs_error <- if (is.na(actual_rate)) NA_real_ else abs(error)

    rows[[out_idx]] <- data.frame(
      predictor_week = merged_data$week[i],
      target_week = target_week,
      weekly_rate_current = merged_data$weekly_rate[i],
      wastewater_t = merged_data$wastewater_value[i],
      wastewater_t_minus_1 = merged_data$wastewater_value[i - 1],
      wastewater_t_minus_2 = merged_data$wastewater_value[i - 2],
      wastewater_t_minus_3 = merged_data$wastewater_value[i - 3],
      predicted_rate = predicted,
      actual_rate = actual_rate,
      error = error,
      abs_error = abs_error,
      stringsAsFactors = FALSE
    )

    out_idx <- out_idx + 1
  }

  history <- do.call(rbind, rows)
  history <- history[order(history$target_week), ]
  history$is_future <- is.na(history$actual_rate)
  history$is_latest_forecast <- FALSE
  history$is_latest_forecast[which.max(history$target_week)] <- TRUE
  history$generated_at_utc <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  rownames(history) <- NULL
  history
}

refresh_georgia_live_forecast <- function(use_remote = TRUE, save_output = TRUE) {
  if (use_remote) {
    hospitalization_raw <- download_latest_source(
      hospitalization_source_url(),
      raw_latest_hospitalization_path()
    )
    wastewater_raw <- download_latest_source(
      wastewater_source_url(),
      raw_latest_wastewater_path()
    )
  } else {
    hospitalization_raw <- load_local_source(resolve_project_path("data", "hospitalization_rates.csv"))
    wastewater_raw <- load_local_source(resolve_project_path("data", "wastewater_trends.csv"))
  }

  hospitalization <- normalize_hospitalization(hospitalization_raw)
  wastewater <- normalize_wastewater(wastewater_raw)
  merged <- join_georgia_series(hospitalization, wastewater)
  history <- build_live_prediction_history(merged)

  if (save_output) {
    write.csv(history, live_prediction_history_path(), row.names = FALSE)
  }

  latest_row <- history[which.max(history$target_week), , drop = FALSE]

  list(
    history = history,
    latest = latest_row,
    latest_observed_week = as.character(max(merged$week)),
    source = if (use_remote) "remote" else "local"
  )
}
