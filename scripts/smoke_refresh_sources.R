source("R/latest_georgia_pipeline.R")

args <- commandArgs(trailingOnly = TRUE)
use_remote <- !("--local-only" %in% args)

result <- refresh_georgia_live_forecast(
  use_remote = use_remote,
  save_output = FALSE,
  save_raw_sources = FALSE
)

latest <- result$latest
cat(sprintf("Data source: %s\n", result$source))
cat(sprintf(
  "Rows: raw_hospitalization=%s, raw_wastewater=%s, hospitalization=%s, wastewater=%s, joined=%s\n",
  result$diagnostics$raw_hospitalization$rows,
  result$diagnostics$raw_wastewater$rows,
  result$diagnostics$hospitalization$rows,
  result$diagnostics$wastewater$rows,
  result$diagnostics$joined$rows
))
cat(sprintf(
  "Latest source weeks: hospitalization=%s, wastewater=%s, joined=%s\n",
  result$diagnostics$hospitalization$latest_week,
  result$diagnostics$wastewater$latest_week,
  result$diagnostics$joined$latest_week
))
cat(sprintf("Latest observed input week: %s\n", result$latest_observed_week))
cat(sprintf("Latest forecast target week: %s\n", as.character(latest$target_week[1])))
cat(sprintf("Latest predicted rate: %.4f\n", as.numeric(latest$predicted_rate[1])))
cat("Smoke check passed without writing refresh output.\n")
