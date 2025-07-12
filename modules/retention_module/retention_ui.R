#' @file retention_ui.R
#' @title Customer Retention Module UI Definition
#'
#' Defines the user interface layout for the Customer Retention dashboard 
#' panel. Displays a themed KPI summary row and tabs for interactive 
#' Plotly charts and scrollable data table with optional download buttons for
#' two views of customer retention metrics.
#'
#' Leverages shared UI components from \code{ui_utils.R} and supports 
#' namespace-safe modular integration with reactive outputs.
#'
#' @keywords internal module UI dashboard customer-retention chinook

#' Customer Retention Module UI Layout
#'
#' Constructs the UI for the Customer Retention panel, including:
#' \itemize{
#'   \item{KPI overview row (revenue, purchases, customers)}
#'   \item{Interactive retention decay curve plot}
#'   \item{Interactive cohort retention heatmap plot}
#'   \item{Downloadable scrollable data tables in card containers}
#' }
#'
#' @param id Module ID used for namespacing outputs.
#'
#' @return A div container of UI elements for the panel.
#' @export
retention_ui <- function(id) {
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
        shiny::uiOutput(ns("lifespan_box")),
        shiny::uiOutput(ns("repeat_behavior_box")),
        shiny::uiOutput(ns("tempo_box"))
      ),
      
      br(),
      # Tabset: Customer Decay Curve & Cohort Retention Heatmap
      bslib::navset_tab(
        id = "nav",
        bslib::nav_panel(
          "Retention Decay Curve", 
          br(),
          plotly::plotlyOutput(ns("decay_curve_plot")),
          br(),
          render_data_card(
            ns = ns, 
            table_id = "decay_table", 
            download_ui_id = "decay_download_ui"
            ),
          br()
        ),
        bslib::nav_panel(
          "Cohort Retention Heatmap",
          br(),
          plotly::plotlyOutput(ns("cohort_heatmap_plot")),
          br(),
          render_data_card(
            ns = ns,
            table_id = "cohort_table",
            download_ui_id = "cohort_download_ui"
            ),
          br()
          )
        )
      )
    )
}

