#' @file group_server.R
#' @title Group Performance Module Server Logic
#'
#' Server-side rendering for the Group Performance dashboard panels. This 
#' module displays pre-aggregated Group-level KPIs, a scrollable summary 
#' table, an interactive plot, and conditional data export options.
#'
#' Built to support modular Shiny layouts with reactive filters, 
#' theming support, and cache-aware data inputs.
#'
#' Relies on shared helpers from \code{group_helpers.R}, \code{stylers.R}, and 
#' \code{server_utils.R}.
#'
#' @keywords internal module server dashboard group-performance chinook

#' Mount Server Logic for Group Performance Panels
#'
#' Registers outputs for KPI cards, Plotly stacked bar chart, data table,
#' and conditional CSV download based on user filters.
#'
#' @param id Module ID string.
#' @param con Active DBI connection to DuckDB.
#' @param events_shared Reactive expression of filtered event records.
#' @param kpis_shared Reactive expression with shared KPI values.
#' @param metric Reactive metric info (name, label).
#' @param date_range Reactive character vector (length 2).
#' @param styles Reactive style list for light/dark themes.
#' @param group_var Name of the grouping variable (e.g. `"Genre"`)
#' @param group_label Label of the group for UI naming and card headings.
#' @param max_n Maximum number of group categories to show in the plot. 
#'
#' @return Adds outputs to the module environment.
#' @export
group_server <- function(
    id, 
    con, 
    events_shared, 
    kpis_shared, 
    metric, 
    date_range, 
    styles,
    group_var,
    group_label,
    max_n = 15
) {
  shiny::moduleServer(id, function(input, output, session) {
    
    group_df <- shiny::reactive({
      # Ensure reactivity when upstream events table updates
      invisible(events_shared())
      
      get_group_yearly_summary(
        con = con,
        date_range = as.character(date_range()),
        group_var = group_var
      )
    })
    
    group_kpis <- shiny::reactive({
      # Ensure reactivity when upstream events table updates
      invisible(events_shared())
      
      format_group_kpi_display(
        kpis_shared = kpis_shared(),
        group_var = group_var
        )
    })
    
    # --- KPI Card Outputs ---
    # 1. Top 5 Groups by Metric
    output$top_box <- shiny::renderUI({
      safe_kpi_card(
        kpis = group_kpis(),
        body_fn = function() {
          k <- group_kpis()[[metric()$var_name]]
          metric_label <- metric()$label

          list(
            build_kpi(
              label = metric_label %||% glue::glue("Top {group_label}"),
              value = build_kpi_list_html(
                labels = k$group_val,
                values = k[[glue::glue("{metric()$var_name}_fmt")]],
                ordered = TRUE
              ),
              tooltip = glue::glue("Top {group_label} by the selected metric.")
            ),
            build_kpi(
              label = glue::glue("Total {group_label}"),
              value = group_kpis()$num_group_cats,
              tooltip = glue::glue("Number of {group_label} with available data.")
            )
          )
        },
        title = glue::glue("Top {group_label}"),
        icon = bsicons::bs_icon("trophy-fill"),
        tooltip = glue::glue("Top {nrow(group_kpis()[[metric()$var_name]])} {group_label}, by the chosen metric."),
        styles = styles()
      )
    })
    
    # 2. Revenue Box
    output$revenue_box <- shiny::renderUI({
      safe_kpi_card(
        kpis = group_kpis(),
        body_fn = function() {
          k <- group_kpis()[[metric()$var_name]]
          
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
    
    # 3. Catalog Box
    output$catalog_box <- shiny::renderUI({
      safe_kpi_card(
        kpis = group_kpis(),
        body_fn = function() {
          k <- group_kpis()[[metric()$var_name]]
          # Body Items for KPI
          list(
            build_kpi(
              label = "Tracks (% of Catalog Sold)",  
              value = build_kpi_list_html(
                labels = k$catalog_size_fmt,
                values = k$pct_catalog_sold_fmt,
                ordered = TRUE
              ),
              tooltip = paste0(
                "Total tracks in catalog ",
                "(% of catalog sold)."
              )
            )
          )
        },
        title = "Catalog",
        icon = bsicons::bs_icon("disc-fill"),
        tooltip = paste0(
          "Total tracks and percentage of catalog sold."
        ),
        styles = styles()
      )
    })
    
    # --- Plot Output: Interactive Stacked Bar Plot ---
    output$group_plot <- plotly::renderPlotly({

      # Ensure reactivity when upstream events table updates
      invisible(events_shared)
      invisible(kpis_shared)
      
      df <- group_df()
      
      # Fallback: No data
      if (nrow(df) == 0 || all(is.na(df))) {
        return(
          plotly::plotly_empty(type = "scatter", mode = "markers") %>%
            plotly::layout(title = "No data available for selected filters.")
        )
      }
      
      group_plotter(df = df, metric = metric(), styles = styles(), group_var = group_var, group_label = group_label, max_n = max_n)      
    })
    
    # --- Scrollable Data Table with Metrics ---
    output$table <- DT::renderDataTable({
      df <- group_df()
      validate(
        shiny::need(nrow(df) > 0, "No data available for selected filters.")
      )
      render_scrollable_table(df)
    })
    
    output$download_ui <- shiny::renderUI({
      render_conditional_download_button("download_csv", group_df())
    })
    
    # --- CSV Download for Raw Transaction Records ---
    output$download_csv <-shiny::downloadHandler(
      filename = function() glue::glue("chinook_{group_var}_{Sys.Date()}.csv"),
      content = function(file) write.csv(group_df(), file, row.names = FALSE)
    )
    
    # Only Render the Download Button if Data Exists
    output$download_ui <- shiny::renderUI({
      render_conditional_download_button("download_csv", group_df())
    })
  })
}