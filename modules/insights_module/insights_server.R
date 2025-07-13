#' @file insights_server.R
#' @title Insights Module Server Logic
#'
#' Server-side rendering for the Insights dashboard panel. This module 
#' displays pre-aggregated Group-level KPIs comparing data-set-wide values
#' to subsets from filters.
#'
#' Built to support modular Shiny layouts with reactive filters,
#' theming support, and cache-aware data inputs.
#'
#' Relies on shared helpers from \code{insights_helpers.R}, \code{stylers.R}, 
#' and \code{server_utils.R}.
#'
#' @keywords internal module server dashboard insights chinook

#' Mount Server Logic for Key Insights Panels
#'
#' Registers outputs for KPI cards for Insights panel
#'
#' @param id Module ID string.
#' @param kpis_static Reactive expression with static KPIs over the entire 
#'   data set.
#' @param kpis_subset Reactive expression with shared KPI values for subsetted 
#'   data.
#' @param styles Reactive style list for light/dark themes.
#'
#' @return Adds outputs to the module environment.
#' @export

insights_server <- function(id, kpis_static, kpis_subset, styles) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns
    
    # --- KPI Card Outputs ---
    # 1. Revenue Tab  
    revenue_category <- list(
      get_overall = function() kpis_static(),
      get_subset  = function() kpis_subset(),
      icon        = bsicons::bs_icon("cash"),
      bullets     = list(
        list(
          type     = "value",
          label    = "Total Revenue",
          value_fn = function(k) format_kpi_value(k$revenue_kpis$total_revenue, "dollar"),
          tooltip  = "Aggregate across entire date range"
        ),
        list(
          type     = "list",
          label    = "Top 3 Markets",
          list_fn  = function(k) {
            tbl <- k$topn$topn_billingcountry$revenue
            tbl <- dplyr::slice_head(tbl, n = 3)
            labels <- tbl$group_val
            values <- tbl$revenue_fmt
            build_kpi_list_html(labels, values, ordered = TRUE)
          },
          tooltip = "Markets with highest revenue contribution"
        ),
        list(
          type = "value",
          label    = "Top Genre",
          value_fn = function(k) {
            top <- k$topn$topn_genre$revenue[1, ]
            glue::glue("{top$group_val} ({top$revenue_fmt})")
          },
          tooltip = "Genre with highest total revenue"
        ),
        list(
          type = "value",
          label    = "Top Artist",
          value_fn = function(k) {
            top <- k$topn$topn_artist$revenue[1, ]
            glue::glue("{top$group_val} ({top$revenue_fmt})")
          },
          tooltip = "Artist with highest total revenue"
        )
      )
    )
    # 2. Purchases Tab
    purchase_category <- list(
      get_overall = function() kpis_static(),
      get_subset  = function() kpis_subset(),
      icon        = bsicons::bs_icon("bag-fill"),
      bullets     = list(
        list(
          type     = "value",
          label    = "Total Orders",
          value_fn = function(k) {
            format_kpi_value(k$purchase_kpis$total_orders, "number")
          },
          tooltip  = "Number of purchases (invoices) in range"
        ),
        list(
          type     = "value",
          label    = "Avg Revenue per Order",
          value_fn = function(k) {
            format_kpi_value(k$purchase_kpis$avg_rev_per_order, "dollar")
          },
          tooltip  = "Average income per invoice"
        ),
        list(
          type     = "value",
          label    = "Total Tracks Sold",
          value_fn = function(k) {
            format_kpi_value(k$purchase_kpis$total_tracks, "number")
          },
          tooltip  = "Number of individual tracks purchased"
        ),
        list(
          type     = "value",
          label    = "Avg Tracks per Order",
          value_fn = function(k) {
            format_kpi_value(k$purchase_kpis$avg_tracks_per_order, "float")
          },
          tooltip  = "Average track count per invoice"
        )
      )
    )
    
    
    # 3. Customer Behavior Tab
    customer_category <- list(
      get_overall = function() kpis_static(),
      get_subset  = function() kpis_subset(),
      icon        = bsicons::bs_icon("people-fill"),
      bullets     = list(
        list(
          type     = "value",
          label    = "Total Customers",
          value_fn = function(k) {
            format_kpi_value(k$customer_kpis$total_customers, "number")
          },
          tooltip  = "Number of customers active in date range"
        ),
        list(
          type     = "value",
          label    = "% New Customers",
          value_fn = function(k) {
            format_kpi_value(k$customer_kpis$pct_new_customers, "percent")
          },
          tooltip  = "Customers with first purchase in date range"
        ),
        list(
          type     = "value",
          label    = "Revenue per Customer",
          value_fn = function(k) {
            total <- k$revenue_kpis$total_revenue
            n     <- k$customer_kpis$total_customers
            if (is.null(total) || is.null(n) || n == 0) return("—")
            format_kpi_value(total / n, "dollar")
          },
          tooltip  = "Average revenue per active customer"
        ),
        list(
          type     = "value",
          label    = "Top 3-Month Retention",
          value_fn = function(k) {
            cohort <- k$retention_kpis$top_cohort_month_3
            rate   <- k$retention_kpis$top_cohort_retention_3
            glue::glue("{cohort} ({rate})")
          },
          tooltip  = "Best-performing 3-month cohort"
        ),
        list(
          type     = "value",
          label    = "Top 6-Month Retention",
          value_fn = function(k) {
            cohort <- k$retention_kpis$top_cohort_month_6
            rate   <- k$retention_kpis$top_cohort_retention_6
            glue::glue("{cohort} ({rate})")
          },
          tooltip  = "Best-performing 6-month cohort"
        )
      )
    )
    
    # --- Render KPI Cards ---
    output$revenue_cards <- shiny::renderUI({
      generate_insights_kpi_cards_ui(
        cfg    = revenue_category,
        styles = styles()
      )
    })
    
    output$customer_cards <- shiny::renderUI({
      generate_insights_kpi_cards_ui(
        cfg    = customer_category,
        styles = styles()
      )
    })
    
    output$purchase_cards <- shiny::renderUI({
      generate_insights_kpi_cards_ui(
        cfg    = purchase_category,
        styles = styles()
      )
    })
    
    # --- Narrative Panels ---
    
    
    # --- "How this Works" Panel ---

  })
}
