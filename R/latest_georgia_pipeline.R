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
  paste0(
    "https://data.cdc.gov/resource/atcp-73re.csv",
    "?%24limit=500000",
    "&state_territory=Georgia",
    "&pathogen_target=SARS-CoV-2"
  )
}

hospitalization_source_url <- function() {
  paste0(
    "https://data.cdc.gov/resource/6jg4-xsqq.csv",
    "?%24limit=500000",
    "&state=GA",
    "&date_type=Week%20Ending%20Date",
    "&agecat_label=All",
    "&race_label=All",
    "&sex_label=All",
    "&data_type=Weekly%20Rate",
    "&estimate_type=Rate%20per%20100%2C000",
    "&rate_type=Observed"
  )
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
        "Could not find `%s`. Expected one of: %s. Available columns: %s",
        field_name,
        paste(candidates, collapse = ", "),
        paste(names(data), collapse = ", ")
      ),
      call. = FALSE
    )
  }
  matched[1]
}

optional_column <- function(data, candidates) {
  matched <- intersect(candidates, names(data))
  if (length(matched) == 0) {
    return(NULL)
  }
  matched[1]
}

column_or_na <- function(data, column) {
  if (is.null(column)) {
    return(rep(NA_character_, nrow(data)))
  }
  data[[column]]
}

normalize_hospitalization <- function(data) {
  state_col <- pick_column(data, c("State", "state"), "state")
  week_col <- pick_column(data, c("_WeekendDate", "Week ending date", "week_ending_date", "Date", "date"), "week ending date")
  age_col <- pick_column(data, c("AgeCategory_Legend", "Age Category", "agecat_label"), "age category")
  sex_col <- pick_column(data, c("Sex_Label", "Sex", "sex_label"), "sex")
  race_col <- pick_column(data, c("Race_Label", "Race", "race_label"), "race")
  rate_type_col <- pick_column(data, c("Type", "Rate Type", "rate_type"), "rate type")
  weekly_rate_col <- pick_column(data, c("WeeklyRate", "Weekly Rate", "weekly_rate", "Estimate", "estimate"), "weekly rate")
  data_type_col <- optional_column(data, c("Data Type", "data_type"))
  estimate_type_col <- optional_column(data, c("Estimate Type", "estimate_type"))

  normalized <- data.frame(
    state = data[[state_col]],
    week = as.Date(data[[week_col]]),
    age_category = data[[age_col]],
    sex_label = data[[sex_col]],
    race_label = data[[race_col]],
    data_type = column_or_na(data, data_type_col),
    estimate_type = column_or_na(data, estimate_type_col),
    rate_type = data[[rate_type_col]],
    weekly_rate = suppressWarnings(as.numeric(data[[weekly_rate_col]])),
    stringsAsFactors = FALSE
  )

  normalized <- normalized[
    normalized$state %in% c("Georgia", "GA") &
      normalized$age_category == "All" &
      normalized$sex_label == "All" &
      normalized$race_label == "All" &
      (is.na(normalized$data_type) | normalized$data_type == "Weekly Rate") &
      (is.na(normalized$estimate_type) | normalized$estimate_type == "Rate per 100,000") &
      normalized$rate_type %in% c("Crude Rate", "Observed") &
      !is.na(normalized$week) &
      !is.na(normalized$weekly_rate),
  ]

  if (nrow(normalized) == 0) {
    stop(
      sprintf(
        "No Georgia hospitalization rows remained after filtering. Raw rows: %s. Columns: %s",
        nrow(data),
        paste(names(data), collapse = ", ")
      ),
      call. = FALSE
    )
  }

  agg <- aggregate(weekly_rate ~ week, data = normalized, FUN = mean)
  agg <- agg[order(agg$week), ]
  rownames(agg) <- NULL
  agg
}

normalize_wastewater <- function(data) {
  state_col <- pick_column(data, c("State/Territory", "state_territory"), "state/territory")
  week_col <- pick_column(data, c("Week_Ending_Date", "Week Ending Date", "week_ending_date", "week_end"), "week ending date")
  wval_col <- pick_column(data, c("State/Territory_WVAL", "state_territory_wval", "site_wval", "Site_WVAL"), "wastewater value")

  period_col <- NULL
  if ("Data_Collection_Period" %in% names(data)) {
    period_col <- "Data_Collection_Period"
  } else if ("data_collection_period" %in% names(data)) {
    period_col <- "data_collection_period"
  }
  pathogen_col <- optional_column(data, c("pathogen_target", "Pathogen_Target", "Pathogen Target"))

  normalized <- data.frame(
    state = data[[state_col]],
    week = as.Date(data[[week_col]]),
    period = if (!is.null(period_col)) data[[period_col]] else NA_character_,
    pathogen = column_or_na(data, pathogen_col),
    wastewater_value = suppressWarnings(as.numeric(data[[wval_col]])),
    stringsAsFactors = FALSE
  )

  normalized <- normalized[
    normalized$state == "Georgia" &
      !is.na(normalized$week) &
      !is.na(normalized$wastewater_value),
  ]

  if (!is.null(pathogen_col)) {
    normalized <- normalized[normalized$pathogen == "SARS-CoV-2", ]
  }

  if (!is.null(period_col)) {
    all_results <- normalized[normalized$period == "All Results", ]
    if (nrow(all_results) > 0) {
      normalized <- all_results
    }
  }

  if (nrow(normalized) == 0) {
    stop(
      sprintf(
        "No Georgia wastewater rows remained after filtering. Raw rows: %s. Columns: %s",
        nrow(data),
        paste(names(data), collapse = ", ")
      ),
      call. = FALSE
    )
  }

  agg <- aggregate(wastewater_value ~ week, data = normalized, FUN = function(values) {
    median(values, na.rm = TRUE)
  })
  agg <- agg[order(agg$week), ]
  rownames(agg) <- NULL
  agg
}

download_latest_source <- function(url, destination_path = NULL, save_raw = TRUE, source_name = "source") {
  data <- tryCatch(
    read.csv(url, stringsAsFactors = FALSE, check.names = FALSE),
    error = function(err) {
      stop(
        sprintf("Could not download %s from `%s`: %s", source_name, url, err$message),
        call. = FALSE
      )
    }
  )

  if (save_raw && !is.null(destination_path)) {
    write.csv(data, destination_path, row.names = FALSE)
  }

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

series_diagnostics <- function(data, label, week_col = "week") {
  weeks <- if (week_col %in% names(data)) data[[week_col]] else as.Date(character())
  list(
    source = label,
    rows = nrow(data),
    first_week = if (length(weeks) == 0 || all(is.na(weeks))) NULL else as.character(min(weeks, na.rm = TRUE)),
    latest_week = if (length(weeks) == 0 || all(is.na(weeks))) NULL else as.character(max(weeks, na.rm = TRUE))
  )
}

raw_diagnostics <- function(data, label) {
  list(
    source = label,
    rows = nrow(data),
    columns = names(data)
  )
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

refresh_georgia_live_forecast <- function(use_remote = TRUE, save_output = TRUE, save_raw_sources = save_output) {
  if (use_remote) {
    hospitalization_raw <- download_latest_source(
      hospitalization_source_url(),
      raw_latest_hospitalization_path(),
      save_raw = save_raw_sources,
      source_name = "CDC COVID-NET hospitalization data"
    )
    wastewater_raw <- download_latest_source(
      wastewater_source_url(),
      raw_latest_wastewater_path(),
      save_raw = save_raw_sources,
      source_name = "CDC wastewater data"
    )
  } else {
    hospitalization_raw <- load_local_source(resolve_project_path("data", "hospitalization_rates.csv"))
    wastewater_raw <- load_local_source(resolve_project_path("data", "wastewater_trends.csv"))
  }

  hospitalization <- normalize_hospitalization(hospitalization_raw)
  wastewater <- normalize_wastewater(wastewater_raw)
  merged <- join_georgia_series(hospitalization, wastewater)

  diagnostics <- list(
    raw_hospitalization = raw_diagnostics(hospitalization_raw, "hospitalization_raw"),
    raw_wastewater = raw_diagnostics(wastewater_raw, "wastewater_raw"),
    hospitalization = series_diagnostics(hospitalization, "hospitalization"),
    wastewater = series_diagnostics(wastewater, "wastewater"),
    joined = series_diagnostics(merged, "joined")
  )

  if (nrow(merged) < 4) {
    stop(
      sprintf(
        "Only %s joined Georgia weekly rows were available after filtering; at least 4 are required. Hospitalization rows: %s, wastewater rows: %s.",
        nrow(merged),
        nrow(hospitalization),
        nrow(wastewater)
      ),
      call. = FALSE
    )
  }

  history <- build_live_prediction_history(merged)

  if (save_output) {
    write.csv(history, live_prediction_history_path(), row.names = FALSE)
  }

  latest_row <- history[which.max(history$target_week), , drop = FALSE]

  list(
    history = history,
    latest = latest_row,
    latest_observed_week = as.character(max(merged$week)),
    source = if (use_remote) "remote" else "local",
    diagnostics = diagnostics
  )
}
