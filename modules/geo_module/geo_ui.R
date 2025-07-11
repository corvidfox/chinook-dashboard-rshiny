#' @file geo_ui.R
#' @title Geographic Distribution Module UI Definition
#'
#' Defines the user interface layout for the Geographic Distribution dashboard 
#' panel. Displays a themed KPI summary row, an interactive Plotly chart, and
#' a scrollable data table with optional download button.
#'
#' Leverages shared UI components from \code{ui_utils.R} and supports 
#' namespace-safe modular integration with reactive outputs.
#'
#' @keywords internal module UI dashboard geographic-distribution chinook

#' Geographic Distribution Module UI Layout
#'
#' Constructs the UI for the Geographic Distribution panel, including:
#' \itemize{
#'   \item{KPI overview row (revenue, purchases, customers)}
#'   \item{Interactive animated choropleth plot}
#'   \item{Downloadable scrollable data table in a card container}
#' }
#'
#' @param id Module ID used for namespacing outputs.
#'
#' @return A div container of UI elements for the panel.
#' @export
geo_ui <- function(id) {
  ns <- NS(id)
  div(
    # Enables smooth transitions during theme changes
    class = "theme-transition",
    shiny::tagList(
      br(),
      # KPI Overview (responsive row of 3 cards)
      bslib::layout_column_wrap(
        width = 1 / 3,
        min_width = 200,
        gap = "1rem",
        fill = TRUE,
        shiny::uiOutput(ns("top5_countries_box")),
        shiny::uiOutput(ns("revenue_share_box")),
        shiny::uiOutput(ns("customer_box"))
      ),
      br(),
      # Interactive choropleth plot
      plotly::plotlyOutput(ns("geo_plot")),
      br(),
      # Data table and conditional download button in card wrapper
      render_data_card(ns = ns)
    )
  )
}
