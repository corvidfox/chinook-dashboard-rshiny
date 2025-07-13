#' @file retention_server.R
#' @title Customer Retention Module Server Logic
#'
#' Server-side rendering for the Customer Retention dashboard panel. This
#' module displays pre-aggregated KPIs, a scrollable summary tables,
#' interactive retention decay and cohort retention heatmap plots, and
#' conditional data export options.
#'
#' Built to support modular Shiny layouts with reactive filters,
#' theming support, and cache-aware data inputs.
#'
#' Relies on shared helpers from \code{retention_helpers.R}, \code{stylers.R},
#' and \code{server_utils.R}.
#'
#' @keywords internal module server dashboard customer-retention chinook

#' Mount Server Logic for Customer Retention Panel
#'
#' Registers outputs for KPI cards, Plotly line and heatmap charts,
#' data tables, and conditional CSV downloads based on user filters.
#'
#' @param id Module ID string.
#' @param con Active DBI connection to DuckDB.
#' @param events_shared Reactive expression of filtered event records.
#' @param kpis_shared Reactive expression with shared KPI values.
#' @param metric Reactive metric info (name, label).
#' @param date_range Reactive character vector (length 2).
#' @param styles Reactive style list for light/dark themes.
#' @param max_offset Optional integer max offset (default = inferred from
#'   range)
#'
#' @return Adds outputs to the module environment.
#' @export
retention_server <- function(id,
                             con,
                             events_shared,
                             cohort_df_shared,
                             kpis_shared,
                             date_range,
                             styles,
                             max_offset = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    decay_df <- shiny::reactive({
      # Ensure reactivity when upstream events table updates
      invisible(events_shared())
      memo_get_retention_decay_data(
        con = con,
        tbl = "filtered_invoices",
        date_range = as.character(date_range()),
        max_offset = NULL
      )
    }) %>%
      shiny::bindCache(events_shared(), date_range())
    
    retention_kpis <- shiny::reactive({
      # Ensure reactivity when upstream events table updates
      invisible(events_shared())
      
      kpis_shared()$retention_kpis
      
    })
    
    # --- KPI Card Outputs ---
    # 1. Customer Lifespan Box
    output$lifespan_box <- renderUI({
      safe_kpi_card(
        kpis = retention_kpis(),
        body_fn = function() {
          k <- retention_kpis()
          list(
            build_kpi(
              "Total Customers (% New)",
              glue::glue("{k$num_cust} ({k$pct_new})"),
              "Total customers active in subset (% of that are new)."
            ),
            build_kpi(
              "Avg Lifespan (Full, Months)",
              k$avg_life_mo_tot,
              "Average customer lifespan across full history, in months."
            ),
            build_kpi(
              "Avg Lifespan (Subset, Months)",
              k$avg_life_mo_win,
              "Average time active in the current date range, in months."
            )
          )
        },
        title = "Customer Overview",
        icon = bsicons::bs_icon("people-fill"),
        tooltip = paste0("Customer count and average engagement lifespan."),
        styles = styles()
      )
    })
    
    # 2. Repeat Behavior Box
    output$repeat_behavior_box <- renderUI({
      safe_kpi_card(
        kpis = retention_kpis(),
        body_fn = function() {
          k <- retention_kpis()
          list(
            build_kpi(
              "Lifetime",
              glue::glue("{k$ret_n_any} ({k$ret_rate_any})"),
              paste0("Number of and % of repeat customers (lifetime repeat).")
            ),
            build_kpi(
              "Returning",
              glue::glue("{k$ret_n_return} ({k$ret_rate_return})"),
              "Number and % of returning repeat customers in the subset."
            ),
            build_kpi(
              "New → Repeat Rate",
              glue::glue("{k$ret_n_conv} ({k$ret_rate_conv})"),
              paste0(
                "Number and % of new customers in the subset that ",
                "became repeating customers."
              )
            ),
            build_kpi(
              "1st→2nd Gap (Lifetime, Days)",
              k$med_gap_life,
              paste0(
                "Median days between first and second purchase",
                " in customer lifetime."
              )
            )
          )
        },
        title = "Repeat Customer Behavior",
        icon = bsicons::bs_icon("arrow-repeat"),
        tooltip = "Conversion from first-time to returning customers.",
        styles = styles()
      )
    })
    
    # 3. Engagement Tempo Box
    output$tempo_box <- renderUI({
      safe_kpi_card(
        kpis = retention_kpis(),
        body_fn = function() {
          k <- retention_kpis()
          list(
            build_kpi(
              "Avg Gap (Subset, Days)",
              k$avg_gap_window,
              "Average days between purchases in subset."
            ),
            build_kpi(
              "Avg Gap (Bounded, Days)",
              k$avg_gap_bound,
              paste0(
                "Average days between purchases, ",
                "including the most recent purchase before and/or",
                " after the subset date range."
              )
            ),
            build_kpi(
              "Top Cohort (3mo)",
              glue::glue(
                "{k$top_cohort_month_3} ({k$top_cohort_retention_3})"
              ),
              tooltip = "Best 3-month retention cohort, and retention rate."
            ),
            build_kpi(
              "Top Cohort (6mo)",
              glue::glue(
                "{k$top_cohort_month_6} ({k$top_cohort_retention_6})"
              ),
              tooltip = "Best 6-month retention cohort, and retention rate."
            ),
            build_kpi(
              "Top Cohort (9mo)",
              glue::glue(
                "{k$top_cohort_month_9} ({k$top_cohort_retention_9})"
              ),
              tooltip = "Best 9-month retention cohort, and retention rate."
            )
          )
        },
        title = "Purchase Tempo",
        icon = bsicons::bs_icon("graph-up"),
        tooltip = "Average purchase spacing and cohort rentention highlights.",
        styles = styles()
      )
    })
    
    # --- Plot Output: Interactive Retention Curve (Line) Plot ---
    output$decay_curve_plot <- plotly::renderPlotly({
      # Ensure reactivity when upstream events table updates
      invisible(events_shared)
      invisible(kpis_shared)
      
      df <- decay_df()
      
      # Fallback: No data
      if (nrow(df) == 0 || all(is.na(df))) {
        return(
          plotly::plotly_empty(type = "scatter", mode = "markers") %>%
            plotly::layout(title = "No data available for selected filters.")
        )
      }
      
      decay_plotter(df = df, styles = styles())
    }) %>%
      shiny::bindCache(events_shared(), date_range(), styles(), metric())
    
    # --- Plot Output: Cohort Retention Heatmap Plot ---
    output$cohort_heatmap_plot <- plotly::renderPlotly({
      # Ensure reactivity when upstream events table updates
      invisible(events_shared)
      invisible(kpis_shared)
      
      df <- cohort_df_shared()
      
      # Fallback: No data
      if (nrow(df) == 0 || all(is.na(df))) {
        return(
          plotly::plotly_empty(type = "scatter", mode = "markers") %>%
            plotly::layout(title = "No data available for selected filters.")
        )
      }
      
      cohort_heatmap_plotter(df = df, styles = styles())
    }) %>%
      shiny::bindCache(events_shared(), date_range(), styles(), metric())
    
    # --- Scrollable Data Table with Metrics (Retention Decay Curve) ---
    output$decay_table <- DT::renderDataTable({
      df <- decay_df()
      shiny::validate(
        shiny::need(nrow(df) > 0, "No data available for selected filters.")
        )
      render_scrollable_table(df)
    })
    
    # --- CSV Download for Raw Transaction Records ---
    output$decay_download_csv <- shiny::downloadHandler(
      filename = function()
        paste0("chinook_decay_", Sys.Date(), ".csv"),
      content = function(file)
        write.csv(decay_df(), file, row.names = FALSE)
    )
    
    # Only Render the Download Button if Data Exists
    output$decay_download_ui <- shiny::renderUI({
      render_conditional_download_button(ns, "decay_download_csv", decay_df())
    })
    
    # --- Scrollable Data Table with Metrics (Cohort Heatmap) ---
    output$cohort_table <- DT::renderDataTable({
      df <- cohort_df_shared()
      shiny::validate(
        shiny::need(nrow(df) > 0, "No data available for selected filters.")
        )
      render_scrollable_table(df)
    })
    
    # --- CSV Download for Raw Transaction Records ---
    output$cohort_download_csv <- shiny::downloadHandler(
      filename = function()
        paste0("chinook_cohort_", Sys.Date(), ".csv"),
      content = function(file)
        write.csv(cohort_df_shared(), file, row.names = FALSE)
    )
    
    # Only Render the Download Button if Data Exists
    output$cohort_download_ui <- shiny::renderUI({
      render_conditional_download_button(
        ns, "cohort_download_csv", cohort_df_shared()
        )
    })
    
  })
}
