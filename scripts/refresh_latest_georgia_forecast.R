source("R/latest_georgia_pipeline.R")

args <- commandArgs(trailingOnly = TRUE)
use_remote <- !("--local-only" %in% args)

result <- refresh_georgia_live_forecast(use_remote = use_remote, save_output = TRUE)

latest <- result$latest
cat(sprintf("Data source: %s\n", result$source))
cat(sprintf("Latest observed input week: %s\n", result$latest_observed_week))
cat(sprintf("Latest forecast target week: %s\n", as.character(latest$target_week[1])))
cat(sprintf("Latest predicted rate: %.4f\n", as.numeric(latest$predicted_rate[1])))
cat(sprintf("Saved history: %s\n", live_prediction_history_path()))
