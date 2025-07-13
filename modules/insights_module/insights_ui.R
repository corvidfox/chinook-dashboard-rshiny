insights_ui <- function(id) {
  ns <- NS(id)
  div(
    # Enables smooth transitions during theme changes
    class = "theme-transition",
    shiny::tagList(
      br(),
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
      ),
      br(),
      bslib::navset_tab(
        id = "nav",
        bslib::nav_panel(
          "Executive Summary", 
          br(),
          shiny::includeMarkdown("www/markdown/executive_summary.md")
        ),
        bslib::nav_panel(
          "Strategic Opportunities", 
          br(),
          shiny::includeMarkdown("www/markdown/strategic_opportunities.md")
        ),
        bslib::nav_panel(
          "Next Steps", 
          br(),
          shiny::includeMarkdown("www/markdown/next_steps.md")
        )
      )
    ) 
  )
}

