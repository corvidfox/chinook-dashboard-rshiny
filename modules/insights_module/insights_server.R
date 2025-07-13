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
    
    kpi_categories <- list(
      Revenue = revenue_category
    )
    
    output$revenue_cards <- shiny::renderUI({
      generate_insights_kpi_cards_ui(
        cfg    = revenue_category,
        styles = styles()
      )
    })
    
    

  })
}
