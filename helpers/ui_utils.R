#' @file ui_utils.R
#' @title Shared UI Helpers for Modular Dashboard Components
#'
#' Provides reusable UI layout elements to streamline construction of
#' Shiny module interfaces. These helpers support standardized design
#' patterns including card-based layouts and output containers.
#'
#' Currently includes:
#' \itemize{
#'   \item{\code{\link{render_data_card}}: Wraps a scrollable data table
#'     and download button UI in a full-width card component.}
#'   \item{\code{\link{spinner_styled}}: Wraps a UI expression with dynamic 
#'     styles.}
#' }
#'
#' Additional helpers may be added over time to support chart containers,
#' KPI rows, or responsive multi-column panels.
#'
#' Designed for use across multiple \code{ui.R} module definitions with
#' \code{bslib::card()} layout and namespaced outputs.
#'
#' @keywords internal UI utilities modular layout components

#' Render Standard Data Card
#'
#' Wraps a scrollable data table and download button in a card layout.
#'
#' @param ns Namespace function from module.
#' @param title Card title (default: "Data Table").
#' @param table_id Output ID for the data table.
#' @param download_ui_id Output ID for the download button UI.
#'
#' @return bslib::card UI element.
#' @export
render_data_card <- function(
    ns, title = "Data Table", table_id = "table", download_ui_id = "download_ui"
) {
  bslib::card(
    full_screen = FALSE,
    bslib::card_header(title),
    bslib::card_body(DT::dataTableOutput(ns(table_id))),
    div(
      style = "text-align: center;",
      shiny::uiOutput(ns(download_ui_id))
    )
  )
}

# helpers/ui_utils.R

#' Wrap a UI output in a themed spinner
#'
#' @param ui_expr A Shiny UI expression (e.g. plotlyOutput("id"), 
#'   DT::dataTableOutput("id"), etc.)
#' @param styles A list returned by your generate_styles(), containing 
#'   spinner$type, color, color_background, size, and caption.
#' @return A UI tag that shows a spinner while ui_expr is loading
spinner_styled <- function(ui_expr, styles) {
  shinycssloaders::withSpinner(
    ui_expr,
    type  = styles$spinner$type,
    color = styles$spinner$color,
    color.background = styles$spinner$color_background,
    size  = styles$spinner$size,
    caption = styles$spinner$caption
  )
}
