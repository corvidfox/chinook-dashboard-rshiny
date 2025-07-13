#' @file insights_ui.R
#' @title Insights Module UI Definition
#'
#' Defines the user interface layout for the Insights dashboard panel. 
#' Displays comparative KPI summary cards and markdown narratives.
#'
#' Supports namespace-safe modular integration with reactive outputs.
#'
#' @keywords internal module UI dashboard insights chinook

#' Insights Module UI Layout
#'
#' Constructs the UI for the Insights panels, including:
#' \itemize{
#'   \item{KPI overview comparison tabs (revenue, purchases, customers)}
#'   \item{Narrative summary tabs}
#'   \item{Accordion with technical detail narrative}
#' }
#'
#' @param id Module ID used for namespacing outputs.
#'
#' @return A div container of UI elements for the panel.
#' @export
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
      ),
      br(),
      bslib::accordion(
        id = "about_data",
        bslib::accordion_panel(
          "Technical Notes",
          shiny::includeMarkdown("www/markdown/under_the_hood.md"),
          icon = bsicons::bs_icon("tools")
        ), 
        open = FALSE
      )
    ) 
  )
}

