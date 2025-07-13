#' @file geo_server.R
#' @title Geographic Distribution Module Server Logic
#'
#' Server-side rendering for the Geographic Distribution dashboard panel. This 
#' module displays pre-aggregated country-level KPIs, a scrollable summary 
#' table, an interactive plot, and conditional data export options.
#'
#' Built to support modular Shiny layouts with reactive filters, 
#' theming support, and cache-aware data inputs.
#'
#' Relies on shared helpers from \code{geo_helpers.R}, \code{stylers.R}, and 
#' \code{server_utils.R}.
#'
#' @keywords internal module server dashboard geographic-distribution chinook

#' Mount Server Logic for Geographic Distribution Panel
#'
#' Registers outputs for KPI cards, Plotly choropleth chart, data table,
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
geo_server <- function(
    id, 
    con, 
    events_shared, 
    kpis_shared, 
    metric, 
    date_range, 
    styles
) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    geo_yearly <- shiny::reactive({
      # Ensure reactivity when upstream events table updates
      invisible(events_shared())
      
      get_geo_metrics(
        con = con,
        date_range = as.character(date_range()),
        mode = "yearly"
      )
    }) %>%
      shiny::bindCache(events_shared(), date_range())
    
    geo_agg <- shiny::reactive({
      # Ensure reactivity when upstream events table updates
      invisible(events_shared())
      
      get_geo_metrics(
        con = con,
        date_range = as.character(date_range()),
        mode = "aggregate"
      )
    })
    
    geo_kpis <- shiny::reactive({
      # Ensure reactivity when upstream events table updates
      invisible(events_shared())
      
      kpis_shared()$topn$topn_billingcountry
      #format_geo_kpi_display(kpis_shared())
    })
    
    # --- KPI Card Outputs ---
    # 1. Top 5 Countries by Metric
    output$top5_countries_box <- shiny::renderUI({
      safe_kpi_card(
        kpis = geo_kpis(),
        body_fn = function() {
          k <- geo_kpis()[[metric()$var_name]]
          metric_label <- metric()$label 
          
          list(
            build_kpi(
              label = metric_label %||% "Top Countries",  
              value = build_kpi_list_html(
                labels = k$group_val,
                values = k[[glue::glue("{metric()$var_name}_fmt")]],
                ordered = TRUE
              ),
              tooltip = "Top countries by the selected metric."
            ),
            build_kpi(
              label = "Total Countries", 
              value = geo_kpis()$num_vals, 
              tooltip = "Number of countries with available data."
            )
          )
        }, 
        title = "Top Countries",
        icon = bsicons::bs_icon("trophy-fill"),
        tooltip = glue::glue("Top {nrow(geo_kpis()[[metric()$var_name]])} countries, by the chosen metric."),
        styles = styles()
      )
    })
    
    # 2. Revenue Share Box
    output$revenue_share_box <- shiny::renderUI({
      safe_kpi_card(
        kpis = geo_kpis(),
        body_fn = function() {
          k <- geo_kpis()[[metric()$var_name]]
          
          list(
            build_kpi(
              label = "Revenue (% Share)",  
              value = build_kpi_list_html(
                labels = k$revenue_fmt,
                values = k$revenue_share_fmt,
                ordered = TRUE
              ),
              tooltip = "Revenue (% of total revenue)."
            )
          )
        },
        title = "Revenue Share",
        icon = bsicons::bs_icon("pie-chart-fill"),
        tooltip = "Revenue and percentage of total revenue (USD$).",
        styles = styles()
      )
    })
    
    # 3. Customer Box
    output$customer_box <- shiny::renderUI({
      safe_kpi_card(
        kpis = geo_kpis(),
        body_fn = function() {
          k <- geo_kpis()[[metric()$var_name]]
          
          list(
            build_kpi(
              label = "Customers",  
              value = build_kpi_list_html(
                labels = k$num_customers_fmt,
                values = k$avg_revenue_per_cust_fmt,
                ordered = TRUE
              ),
              tooltip = paste0(
                "Total number of customers ",
                "(average revenue per customer, USD$)."
              )
            )
          )
        },
        title = "Customers (N, Avg$)",
        icon = bsicons::bs_icon("people-fill"),
        tooltip = paste0(
          "Total number of customers and average",
          " revenue per customer (USD$)."
        ),
        styles = styles()
      )
    })
    
    # --- Plot Output: Interactive Choropleth (Year Animated) Plot ---
    output$geo_plot <- plotly::renderPlotly({
      
      # Ensure reactivity when upstream events table updates
      invisible(events_shared)
      invisible(kpis_shared)
      
      yearly_df <- geo_yearly()
      agg_df <- geo_agg()
      
      # Fallback: No data
      if (nrow(yearly_df) == 0 || all(is.na(yearly_df))) {
        return(
          plotly::plotly_empty(type = "scatter", mode = "markers") %>%
            plotly::layout(title = "No data available for selected filters.")
        )
      }

      df <- dplyr::bind_rows(agg_df, yearly_df)
      geo_plotter(df = df, metric = metric(), styles = styles())
    }) %>%
      shiny::bindCache(events_shared(), date_range(), styles(), metric())
    
    # --- Scrollable Data Table with Metrics ---
    output$table <- DT::renderDataTable({
      df <- geo_yearly()
      shiny::validate(
        shiny::need(nrow(df) > 0, "No data available for selected filters.")
      )
      render_scrollable_table(df)
    })

    # --- CSV Download for Raw Transaction Records ---
    output$download_csv <- shiny::downloadHandler(
      filename = function() paste0("chinook_{ns}_", Sys.Date(), ".csv"),
      content = function(file) write.csv(geo_yearly(), file, row.names = FALSE)
    )
    
    # Only Render the Download Button if Data Exists
    output$download_ui <- shiny::renderUI({
      render_conditional_download_button(ns, "download_csv", geo_yearly())
    })
  })
}