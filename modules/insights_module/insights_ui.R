insights_ui <- function(id) {
  ns <- NS(id)
  div(
    # Enables smooth transitions during theme changes
    class = "theme-transition",
    shiny::tagList(
      h4("KPI Highlights"),
      # Tabset: KPI Comparisons
      bslib::navset_tab(
        id = "nav",
        bslib::nav_panel(
          "Revenue", 
          br(),
          shiny::uiOutput(ns("revenue_cards"))
        ),
        bslib::nav_panel(
          "Purchase Patterns", 
          br(),
          shiny::uiOutput(ns("purchase_cards"))
        ),
        bslib::nav_panel(
          "Customer Behavior", 
          br(),
          shiny::uiOutput(ns("customer_cards"))
        )
      )
      
      
      
      
    ) 
  )
}

