#' @file ts_server.R
#' @title Time Series Module Server Logic
#'
#' Server-side rendering for the Time Series dashboard panel. This module 
#' displays pre-aggregated monthly KPIs, a scrollable summary table, 
#' an interactive plot, and conditional data export options.
#'
#' Built to support modular Shiny layouts with reactive filters, 
#' theming support, and cache-aware data inputs.
#'
#' Relies on shared helpers from \code{ts_helpers.R}, \code{stylers.R}, and 
#' \code{server_utils.R}.
#'
#' @keywords internal module server dashboard time-series chinook

#' Mount Server Logic for Time Series Panel
#'
#' Registers outputs for KPI cards, Plotly line chart, data table,
#' and conditional CSV download based on user filters.
#'
#' @param id Module ID string.
#' @param con Active DBI connection to DuckDB.
#' @param events_shared Reactive expression of filtered event records.
#' @param kpis_shared Reactive expression with shared KPI values.
#' @param metric Reactive metric info (name, label).
#' @param date_range Reactive character vector (length 2).
#' @param styles Reactive style list for light/dark themes.
#'
#' @return Adds outputs to the module environment.
#' @export
ts_server <- function(
    id, 
    con, 
    events_shared, 
    kpis_shared, 
    metric, 
    date_range, 
    styles
) {
  shiny::moduleServer(id, function(input, output, session) {
    
    ts_df <- shiny::reactive({
      # Ensure reactivity when upstream events table updates
      invisible(events_shared())
      
      get_ts_monthly_summary(
        con = con,
        date_range = as.character(date_range())
      )
      
    })
    
    ts_kpis <- shiny::reactive({
      # Ensure reactivity when upstream events table updates
      invisible(events_shared())
      
      format_ts_kpi_display(kpis_shared(), as.character(date_range()))
    })
    
    # --- KPI Card Outputs ---
    # 1. Revenue Box
    output$revenue_box <- shiny::renderUI({
      safe_kpi_card(
        kpis = ts_kpis(),
        body_fn = function() {
          k <- ts_kpis()
          list(
            safe_kpi_entry("Total", k$total_rev, "Total revenue."),
            safe_kpi_entry(
              "Avg / Month", k$avg_rev, "Average revenue per month."
            )
          )
        },
        title = "Revenue",
        icon = bsicons::bs_icon("currency-dollar"),
        tooltip = "Gross revenue, in US dollars.",
        styles = styles()
      )
    })
    
    # 2. Purchases Box
    output$purchase_box <- shiny::renderUI({
      safe_kpi_card(
        kpis = ts_kpis(),
        body_fn = function() {
          k <- ts_kpis()
          list(
            safe_kpi_entry(
              "Purchases",
              k$total_purchases,
              "Total number of unique purchases."
            ),
            safe_kpi_entry(
              "Tracks Sold",
              k$total_tracks,
              "Total number of tracks sold (unit sales volume)."
            ),
            safe_kpi_entry(
              "Avg $ / Purchase",
              k$avg_per_purchase,
              "Average revenue per purchase."
            )
          )
        },
        title = "Purchases",
        icon = bsicons::bs_icon("receipt"),
        tooltip = "Purchase patterns.",
        styles = styles()
      )
    })
    
    # 3. Customers Box
    output$customer_box <- shiny::renderUI({
      safe_kpi_card(
        kpis = ts_kpis(),
        body_fn = function() {
          k <- ts_kpis()
          list(
            safe_kpi_entry("Total", k$total_customers, "Total unique customers."),
            safe_kpi_entry(
              "First-Time", k$first_time_pct, "(%)Number of first time customers."
            )
          )
        },
        title = "Customers",
        icon = bsicons::bs_icon("people-fill"),
        tooltip = "Customers that made a purchase.",
        styles = styles()
      )
    })
    
    # --- Plot Output: Interactive Time Series Line Plot ---
    output$ts_plot <- plotly::renderPlotly({
      # Ensure reactivity when upstream events table updates
      invisible(events_shared)
      invisible(kpis_shared)
      
      df <- ts_df()
      
      # Fallback: No data
      if (nrow(df) == 0 || all(is.na(df))) {
        return(
          plotly::plotly_empty(type = "scatter", mode = "markers") %>%
            plotly::layout(title = "No data available for selected filters.")
        )
      }
      
      ts_plotter(df = df, metric = metric(), styles = styles())
    })
    
    # --- Scrollable Data Table with Metrics ---
    output$table <- DT::renderDataTable({
      df <- ts_df()
      validate(
        shiny::need(nrow(df) > 0, "No data available for selected filters.")
      )
      render_scrollable_table(df)
    })
    
    output$download_ui <- shiny::renderUI({
      render_conditional_download_button("download_csv", ts_df())
    })
    
    # --- CSV Download for Raw Transaction Records ---
    output$download_csv <- shiny::downloadHandler(
      filename = function() paste0("chinook_ts_", Sys.Date(), ".csv"),
      content = function(file) write.csv(ts_df(), file, row.names = FALSE)
    )
    
    # Only Render the Download Button if Data Exists
    output$download_ui <- shiny::renderUI({
      render_conditional_download_button("download_csv", ts_df())
    })
  })
}
