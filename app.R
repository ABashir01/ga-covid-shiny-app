project_library <- file.path(getwd(), ".r-lib")
if (dir.exists(project_library)) {
  .libPaths(c(project_library, .libPaths()))
}

library(shiny)
library(ggplot2)

source("R/arx_inference.R")
source("R/latest_georgia_pipeline.R")

metrics_df <- load_model_metrics()
rate_unit_label <- "weekly hospitalizations per 100,000 people"

format_metric <- function(metric_name) {
  idx <- which(metrics_df$Metric == metric_name)
  if (length(idx) == 0) return("N/A")
  format(round(as.numeric(metrics_df$Value[idx[1]]), 3), nsmall = 3)
}

fmt_date <- function(value) {
  if (is.null(value) || length(value) == 0 || is.na(value)) return("N/A")
  format(as.Date(value), "%b %d, %Y")
}

fmt_rate <- function(value) {
  if (is.null(value) || length(value) == 0 || is.na(value)) return("N/A")
  sprintf("%.2f", as.numeric(value))
}

signal_label <- function(rate_value) {
  if (is.null(rate_value) || is.na(rate_value)) return("Unknown")
  if (rate_value >= 5) return("Elevated")
  if (rate_value >= 2) return("Watch")
  "Low"
}

signal_class <- function(rate_value) {
  label <- signal_label(rate_value)
  if (label == "Elevated") return("risk-elevated")
  if (label == "Watch") return("risk-watch")
  if (label == "Low") return("risk-low")
  "risk-unknown"
}

fallback_history_from_legacy <- function() {
  old_history <- load_prediction_history()
  if (nrow(old_history) == 0) return(data.frame())

  converted <- data.frame(
    predictor_week = as.Date(old_history$week),
    target_week = as.Date(old_history$week) + 14,
    weekly_rate_current = NA_real_,
    wastewater_t = old_history$wastewater_current,
    wastewater_t_minus_1 = NA_real_,
    wastewater_t_minus_2 = NA_real_,
    wastewater_t_minus_3 = NA_real_,
    predicted_rate = old_history$predicted_rate,
    actual_rate = old_history$actual_rate,
    error = old_history$error,
    abs_error = old_history$abs_error,
    is_future = is.na(old_history$actual_rate),
    is_latest_forecast = FALSE,
    generated_at_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    stringsAsFactors = FALSE
  )
  converted$is_latest_forecast[which.max(converted$target_week)] <- TRUE
  converted
}

initialize_history <- function() {
  live <- load_live_prediction_history()
  if (nrow(live) > 0) {
    return(list(history = live, refreshed_at = max(live$generated_at_utc)))
  }

  legacy <- fallback_history_from_legacy()
  if (nrow(legacy) > 0) {
    return(list(history = legacy, refreshed_at = max(legacy$generated_at_utc)))
  }

  list(history = data.frame(), refreshed_at = NA_character_)
}

latest_forecast_row <- function(history_df) {
  if (nrow(history_df) == 0) return(NULL)
  history_df[which.max(history_df$target_week), , drop = FALSE]
}

latest_actual_row <- function(history_df) {
  if (nrow(history_df) == 0) return(NULL)
  actual_rows <- history_df[!is.na(history_df$actual_rate), , drop = FALSE]
  if (nrow(actual_rows) == 0) return(NULL)
  actual_rows[which.max(actual_rows$target_week), , drop = FALSE]
}

initial <- initialize_history()
initial_latest <- latest_forecast_row(initial$history)

ui <- fluidPage(
  tags$head(
    tags$link(rel = "icon", href = "data:,"),
    tags$style(
      HTML(
        "
        :root {
          --bg-top: #f7f9fc;
          --bg-bottom: #eef3f8;
          --ink: #163247;
          --ink-soft: #5a7082;
          --frame: #ffffff;
          --frame-border: #d5dee8;
          --accent: #0f6ea3;
          --accent-2: #1f8cc7;
          --space-1: 4px;
          --space-2: 8px;
          --space-3: 12px;
          --space-4: 16px;
          --space-5: 20px;
          --space-6: 24px;
        }
        html, body {
          min-height: 100%;
        }
        body {
          margin: 0;
          min-height: 100vh;
          font-family: 'Segoe UI', 'Segoe UI Variable Text', 'Trebuchet MS', sans-serif;
          background:
            radial-gradient(circle at 10% 0%, #ffffff 0%, transparent 38%),
            linear-gradient(180deg, var(--bg-top) 0%, var(--bg-bottom) 100%);
          background-repeat: no-repeat;
          background-attachment: fixed;
          color: var(--ink);
          line-height: 1.4;
        }
        .container-fluid {
          min-height: 100vh;
          background: transparent;
        }
        .shell {
          max-width: 1240px;
          margin: 0 auto;
          padding: var(--space-5) var(--space-4) var(--space-6) var(--space-4);
        }
        .topbar {
          display: flex;
          justify-content: space-between;
          align-items: baseline;
          gap: var(--space-3);
          margin-bottom: var(--space-4);
        }
        .title {
          font-size: 1.3rem;
          font-weight: 750;
          color: #173a53;
          letter-spacing: 0.01em;
        }
        .subtitle {
          font-size: 0.82rem;
          color: var(--ink-soft);
        }
        .frame {
          background: var(--frame);
          border: 1px solid var(--frame-border);
          border-radius: 12px;
          box-shadow: 0 3px 12px rgba(18, 47, 69, 0.08);
          padding: var(--space-4);
          margin-bottom: var(--space-3);
        }
        .signal-wall {
          display: grid;
          grid-template-columns: 1.1fr 1fr 1fr;
          gap: var(--space-3);
          align-items: stretch;
          background: #f5f9fd;
          border-color: #c7d9e9;
          position: relative;
        }
        .signal-tile {
          border-radius: 10px;
          background: #ffffff;
          border: 1px solid #d4e0eb;
          box-shadow: none;
          padding: var(--space-3) var(--space-3) var(--space-4) var(--space-3);
          display: grid;
          grid-template-rows: auto 1fr auto;
          min-height: 210px;
          position: relative;
          z-index: 1;
        }
        .tile-label {
          text-transform: uppercase;
          letter-spacing: 0.06em;
          font-size: 0.78rem;
          color: #617b8f;
          font-weight: 700;
          align-self: start;
        }
        .tile-number {
          font-size: clamp(4rem, 8vw, 7rem);
          font-weight: 800;
          line-height: 0.95;
          color: #145b86;
          text-align: center;
          margin: 0;
          text-shadow: none;
          align-self: center;
        }
        .tile-sub {
          text-align: center;
          font-size: 0.78rem;
          color: #5f7487;
          font-weight: 500;
          align-self: end;
        }
        .week-line {
          text-align: center;
          margin: 0;
          font-size: clamp(2rem, 3.6vw, 3rem);
          font-weight: 800;
          color: #0f4f79;
          line-height: 1.1;
          align-self: center;
        }
        .risk-wrap {
          text-align: center;
          margin-top: 0;
          align-self: center;
        }
        .risk-pill {
          display: inline-block;
          border-radius: 999px;
          padding: 10px 18px;
          font-size: clamp(1.25rem, 2.4vw, 2rem);
          font-weight: 800;
          border: 1px solid transparent;
          min-width: 160px;
        }
        .risk-low { background: #dff8ea; border-color: #6bcd93; color: #0e6b39; }
        .risk-watch { background: #fff1d1; border-color: #e9bd61; color: #8a5406; }
        .risk-elevated { background: #ffe0e0; border-color: #e98989; color: #981919; }
        .risk-unknown { background: #edf2f7; border-color: #cfdbe5; color: #43596a; }
        .status-line {
          margin-top: var(--space-1);
          font-size: 0.8rem;
          color: #415e73;
          text-align: center;
          grid-column: 1 / -1;
          white-space: normal;
          word-break: break-word;
          position: relative;
          z-index: 1;
        }
        .status-line a {
          color: #0f6ea3;
          font-weight: 600;
          text-decoration: underline;
        }
        .panel-title {
          font-size: 0.84rem;
          font-weight: 800;
          margin-bottom: var(--space-2);
          color: #24475f;
          text-transform: uppercase;
          letter-spacing: 0.07em;
        }
        .panel-note {
          font-size: 0.78rem;
          color: #60788c;
          margin-bottom: var(--space-2);
        }
        .timeline-frame {
          background: #ffffff;
          height: 520px;
          display: flex;
          flex-direction: column;
        }
        .timeline-frame .shiny-plot-output {
          flex: 1;
        }
        .context-item {
          border-top: 1px dashed #d6e3ee;
          padding-top: 8px;
          margin-top: 8px;
        }
        .context-key {
          font-size: 0.68rem;
          color: #68839a;
          text-transform: uppercase;
          letter-spacing: 0.08em;
        }
        .context-val {
          font-family: Consolas, 'Lucida Console', monospace;
          font-size: 0.9rem;
          color: #1a4159;
          margin-top: 2px;
          white-space: normal;
          word-break: break-word;
        }
        .scenario-frame {
          height: 520px;
          display: grid;
          grid-template-rows: auto 1fr auto;
          row-gap: var(--space-3);
        }
        .scenario-head {
          display: flex;
          flex-direction: column;
          gap: 2px;
          margin-bottom: 0;
        }
        .scenario-middle {
          display: flex;
          align-items: center;
        }
        .scenario-form {
          width: 100%;
          display: grid;
          grid-template-columns: 1fr 1fr;
          gap: var(--space-3) var(--space-3);
          align-items: start;
        }
        .scenario-tail {
          align-self: end;
        }
        .scenario-form .shiny-input-container {
          width: 100% !important;
          max-width: 100%;
          min-width: 0;
        }
        .scenario-frame .form-group {
          margin-bottom: 8px;
        }
        .scenario-frame .control-label {
          font-size: 0.7rem;
          line-height: 1.15;
          letter-spacing: 0.02em;
          margin-bottom: var(--space-1);
          color: #4a6578;
        }
        .scenario-frame .form-control {
          width: 100% !important;
          max-width: 100%;
          min-width: 0;
          height: 32px;
          padding: 5px 8px;
          font-size: 0.86rem;
        }
        .scenario-frame .input-group {
          width: 100%;
        }
        .scenario-frame .input-daterange,
        .scenario-frame .input-group.date {
          width: 100%;
        }
        .scenario-frame .input-group-btn .btn {
          height: 32px;
          padding: 5px 8px;
        }
        .scenario-actions {
          margin-top: 0;
          display: flex;
          align-items: center;
          gap: 0;
          width: 100%;
        }
        .scenario-actions .btn,
        .scenario-actions .btn-primary {
          width: 100%;
          display: block;
          border-radius: 10px;
        }
        .scenario-result {
          margin-top: var(--space-2);
          border: 1px solid #0f6ea3;
          background: #0f6ea3;
          border-radius: 10px;
          padding: var(--space-3);
          min-height: 56px;
          max-height: 110px;
          overflow-y: auto;
          display: flex;
          align-items: center;
        }
        summary {
          cursor: pointer;
          font-weight: 700;
          color: #1b4e6d;
          margin-bottom: 8px;
        }
        .unit-note {
          font-size: 0.76rem;
          line-height: 1.25;
          color: #4f6d82;
          margin: 0;
        }
        .scenario-frame .panel-title {
          margin: 0;
        }
        .scenario-frame .panel-note {
          margin: 0;
          line-height: 1.25;
          font-size: 0.78rem;
        }
        .btn-primary {
          background: var(--accent);
          border-color: var(--accent);
          color: #ffffff;
          font-weight: 700;
        }
        #prediction_text {
          white-space: pre-wrap;
          overflow-wrap: break-word;
          color: #ffffff;
          margin: 0;
          font-size: 1.05rem;
          line-height: 1.3;
          font-weight: 700;
          width: 100%;
        }
        .live-context-frame {
          background: #f5f9fd;
          border-color: #c7d9e9;
        }
        .live-context-frame .panel-title {
          text-align: left;
          margin-bottom: 10px;
          padding-left: 2px;
        }
        .live-context-frame .context-grid {
          display: grid;
          grid-template-columns: repeat(4, minmax(0, 1fr));
          gap: 12px;
        }
        .live-context-frame .context-item {
          margin: 0;
          padding: 10px 12px;
          border-top: none;
          border: 1px solid #cfe1ef;
          border-radius: 10px;
          background: #ffffff;
          box-shadow: none;
          display: grid;
          grid-template-rows: auto 1fr;
          min-height: 100px;
          text-align: left;
          align-items: stretch;
        }
        .live-context-frame .context-key {
          font-size: 0.68rem;
          margin-bottom: var(--space-2);
          text-align: left;
          width: 100%;
          align-self: start;
          justify-self: start;
        }
        .live-context-frame .context-val {
          font-size: 1.14rem;
          margin: 0;
          line-height: 1.25;
          max-width: 100%;
          text-align: center;
          width: 100%;
          display: flex;
          align-items: center;
          justify-content: center;
        }
        @media (max-width: 900px) {
          .signal-wall {
            grid-template-columns: 1fr;
          }
          .signal-tile {
            min-height: 150px;
          }
          .tile-number {
            font-size: clamp(3.4rem, 17vw, 5rem);
          }
          .week-line {
            font-size: clamp(1.8rem, 9vw, 2.4rem);
          }
          .risk-pill {
            font-size: clamp(1.1rem, 8vw, 1.6rem);
          }
          .timeline-frame,
          .scenario-frame {
            height: auto;
          }
          .scenario-form {
            grid-template-columns: 1fr;
          }
          .scenario-middle {
            display: block;
          }
          .live-context-frame .context-grid {
            grid-template-columns: 1fr;
          }
        }
        "
      )
    )
  ),
  div(
    class = "shell",
    div(
      class = "topbar",
      div(class = "title", "Georgia COVID Hospitalization Two-Week Forecast"),
    ),
    fluidRow(
      column(
        width = 12,
        div(
          class = "frame signal-wall",
          div(
            class = "signal-tile",
            div(class = "tile-label", "Predicted Hospitalization Rate"),
            div(class = "tile-number", textOutput("spotlight_rate", inline = TRUE)),
            div(class = "tile-sub", "per 100,000 people")
          ),
          div(
            class = "signal-tile",
            div(class = "tile-label", "Predicted Risk Level"),
            div(class = "risk-wrap", uiOutput("spotlight_risk")),
            div(class = "tile-sub", "Thresholds: Low <2 | Watch 2-4.99 | Elevated >=5")
          ),
          div(
            class = "signal-tile",
            div(class = "tile-label", "Forecast Week"),
            div(class = "week-line", textOutput("spotlight_week", inline = TRUE)),
            div(class = "tile-sub", "Two-week-ahead target")
          ),
          div(class = "status-line", uiOutput("spotlight_status"))
        )
      ),
    ),
    fluidRow(
      column(
        width = 8,
        div(
          class = "frame timeline-frame",
          div(class = "panel-title", "Forecast Timeline"),
          div(class = "panel-note", "Observed line, model forecast line, and the latest forecast point."),
          plotOutput("history_plot", height = "430px")
        )
      ),
      column(
        width = 4,
        div(
          class = "frame scenario-frame",
          div(
            class = "scenario-head",
            div(class = "panel-title", "Scenario Lab"),
            div(class = "panel-note", "Optional what-if forecast. Does not alter the live timeline."),
            div(class = "unit-note", "Hospitalization input uses per 100k. Wastewater inputs use CDC WVAL (unitless index).")
          ),
          div(
            class = "scenario-middle",
            div(
              class = "scenario-form",
              div(
                class = "scenario-field",
                dateInput("week", "Current Week Ending Date", value = Sys.Date(), format = "yyyy-mm-dd"),
              ),
              div(
                class = "scenario-field",
                numericInput("weekly_rate_current", "Current Weekly Hospitalization Rate (per 100k)", value = 3.5, step = 0.1),
              ),
              div(
                class = "scenario-field",
                numericInput("wastewater_t", "Wastewater Level (this week)", value = 2.8, step = 0.1),
              ),
              div(
                class = "scenario-field",
                numericInput("wastewater_t_minus_1", "Wastewater Level (1 week ago)", value = 2.6, step = 0.1),
              ),
              div(
                class = "scenario-field",
                numericInput("wastewater_t_minus_2", "Wastewater Level (2 weeks ago)", value = 2.4, step = 0.1),
              ),
              div(
                class = "scenario-field",
                numericInput("wastewater_t_minus_3", "Wastewater Level (3 weeks ago)", value = 2.3, step = 0.1),
              )
            )
          ),
          div(
            class = "scenario-tail",
            div(
              class = "scenario-actions",
              actionButton("predict_btn", "Run Scenario", class = "btn-primary")
            ),
          div(
            class = "scenario-result",
            textOutput("prediction_text")
          )
        )
      )
      )
    ),
    fluidRow(
      column(
        width = 12,
        div(
          class = "frame live-context-frame",
          div(class = "panel-title", "Live Context"),
          div(
            class = "context-grid",
            div(class = "context-item", div(class = "context-key", "Latest Known Actual"), div(class = "context-val", textOutput("context_actual", inline = TRUE))),
            div(class = "context-item", div(class = "context-key", "Latest Observed Input Week"), div(class = "context-val", textOutput("context_input_week", inline = TRUE))),
            div(class = "context-item", div(class = "context-key", "Last Cache Refresh"), div(class = "context-val", textOutput("context_refresh", inline = TRUE))),
            div(class = "context-item", div(class = "context-key", "Model Quality (Tested on 2025 Holdout)"), div(class = "context-val", textOutput("context_quality", inline = TRUE)))
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  rv <- reactiveValues(
    history_df = initial$history,
    refreshed_at = initial$refreshed_at
  )

  update_numeric_if_valid <- function(input_id, value) {
    if (!is.na(value) && is.finite(value)) {
      updateNumericInput(session, input_id, value = value)
    }
  }

  if (!is.null(initial_latest)) {
    updateDateInput(session, "week", value = as.Date(initial_latest$predictor_week[1]))
    update_numeric_if_valid("weekly_rate_current", as.numeric(initial_latest$weekly_rate_current[1]))
    update_numeric_if_valid("wastewater_t", as.numeric(initial_latest$wastewater_t[1]))
    update_numeric_if_valid("wastewater_t_minus_1", as.numeric(initial_latest$wastewater_t_minus_1[1]))
    update_numeric_if_valid("wastewater_t_minus_2", as.numeric(initial_latest$wastewater_t_minus_2[1]))
    update_numeric_if_valid("wastewater_t_minus_3", as.numeric(initial_latest$wastewater_t_minus_3[1]))
  }

  latest_forecast <- reactive({
    latest_forecast_row(rv$history_df)
  })

  latest_actual <- reactive({
    latest_actual_row(rv$history_df)
  })

  output$spotlight_week <- renderText({
    row <- latest_forecast()
    if (is.null(row)) return("N/A")
    fmt_date(row$target_week[1])
  })

  output$spotlight_rate <- renderText({
    row <- latest_forecast()
    if (is.null(row)) return("N/A")
    fmt_rate(row$predicted_rate[1])
  })

  output$spotlight_risk <- renderUI({
    row <- latest_forecast()
    if (is.null(row)) return(tags$span(class = "risk-pill risk-unknown", "Unknown"))
    score <- as.numeric(row$predicted_rate[1])
    tags$span(class = paste("risk-pill", signal_class(score)), signal_label(score))
  })

  output$spotlight_status <- renderUI({
    tags$span(
      "Data sources: ",
      tags$a(
        href = "https://www.cdc.gov/nwss/rv/COVID19-statetrend.html",
        target = "_blank",
        rel = "noopener noreferrer",
        "CDC NWSS Respiratory Virus State Trends"
      ),
      " | ",
      tags$a(
        href = hospitalization_source_url(),
        target = "_blank",
        rel = "noopener noreferrer",
        "CDC Hospitalization Data Feed"
      ),
      " | ",
      tags$a(
        href = wastewater_source_url(),
        target = "_blank",
        rel = "noopener noreferrer",
        "CDC NWSS Wastewater Data Feed"
      )
    )
  })

  output$context_actual <- renderText({
    row <- latest_actual()
    if (is.null(row)) return("Pending publication")
    paste0(fmt_rate(row$actual_rate[1]), " (", fmt_date(row$target_week[1]), ")")
  })

  output$context_input_week <- renderText({
    row <- latest_forecast()
    if (is.null(row)) return("N/A")
    fmt_date(row$predictor_week[1])
  })

  output$context_refresh <- renderText({
    if (is.na(rv$refreshed_at) || !nzchar(rv$refreshed_at)) return("Not refreshed yet")
    ts <- suppressWarnings(as.POSIXct(rv$refreshed_at, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))
    if (is.na(ts)) return(substr(rv$refreshed_at, 1, 10))
    format(ts, "%b %d, %Y %H:%M UTC")
  })

  output$context_quality <- renderText({
    paste(
      "R^2 =", format_metric("R_squared_2025"),
      "| RMSE =", format_metric("RMSE_2025"),
      "| MAE =", format_metric("MAE_2025")
    )
  })

  output$history_plot <- renderPlot({
    validate(need(nrow(rv$history_df) > 0, "No timeline data available."))

    plot_df <- rv$history_df[order(rv$history_df$target_week), ]
    plot_df$target_week <- as.Date(plot_df$target_week)
    actual_df <- plot_df[!is.na(plot_df$actual_rate), , drop = FALSE]
    latest_df <- plot_df[plot_df$is_latest_forecast, , drop = FALSE]

    ggplot(plot_df, aes(x = target_week)) +
      geom_line(aes(y = predicted_rate, color = "Forecast"), linewidth = 1.1, linetype = "22") +
      geom_line(data = actual_df, aes(y = actual_rate, color = "Observed"), linewidth = 1.1) +
      geom_point(data = latest_df, aes(y = predicted_rate), color = "#ff4d73", size = 3.5) +
      scale_color_manual(values = c("Observed" = "#1d87de", "Forecast" = "#ff9e2c")) +
      labs(
        x = "Forecast Target Week",
        y = "Weekly Hospitalization Rate (per 100k)",
        color = ""
      ) +
      theme_minimal(base_size = 13) +
      theme(
        legend.position = "top",
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "#d6e3ef"),
        panel.background = element_rect(fill = "#ffffff", color = NA),
        plot.background = element_rect(fill = "#ffffff", color = NA),
        text = element_text(color = "#1a3d59"),
        axis.text = element_text(color = "#36576e"),
        axis.title = element_text(color = "#1a4763", face = "bold"),
        legend.text = element_text(color = "#244b66"),
        legend.background = element_rect(fill = "#ffffff", color = NA)
      )
  })

  prediction_result <- eventReactive(input$predict_btn, {
    payload <- list(
      week = as.character(input$week),
      weekly_rate_current = input$weekly_rate_current,
      wastewater_t = input$wastewater_t,
      wastewater_t_minus_1 = input$wastewater_t_minus_1,
      wastewater_t_minus_2 = input$wastewater_t_minus_2,
      wastewater_t_minus_3 = input$wastewater_t_minus_3
    )

    tryCatch(
      predict_two_week_hospitalization(payload),
      error = function(err) list(error = err$message)
    )
  })

  output$prediction_text <- renderText({
    result <- prediction_result()
    if (is.null(result)) return("Enter inputs and click Run Scenario.")
    if (!is.null(result$error)) return(paste("Prediction error:", result$error))
    input_week <- as.Date(result$week)
    target_week <- input_week + 14
    paste(
      "Forecast for week ending on", fmt_date(target_week), ":",
      fmt_rate(result$prediction), "per 100k people"
    )
  })
}

shinyApp(ui, server)
