# Georgia COVID Forecast Shiny App

Production-focused Shiny app package containing only runtime assets:

- pp.R
- R/arx_inference.R
- R/latest_georgia_pipeline.R
- scripts/refresh_latest_georgia_forecast.R
- ARX model artifact + prediction history CSVs

## Local Run

`ash
R -e "shiny::runApp('app.R', host='127.0.0.1', port=3838)"
`

## Refresh Latest Forecast Data

`ash
Rscript scripts/refresh_latest_georgia_forecast.R
`

## Deployment

This repo is prepared for Posit Connect Cloud / shinyapps.io deployment.
Generate manifest.json with sconnect::writeManifest() before publishing if needed.