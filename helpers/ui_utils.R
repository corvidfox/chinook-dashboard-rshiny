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
