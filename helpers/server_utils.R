#' @file server_utils.R
#' @title Shared Server-Side Utilities for Module Output Rendering
#'
#' Provides reusable components for rendering scrollable data tables
#' and conditional download buttons across Shiny module servers.
#'
#' These helpers streamline UI management and reduce duplication for
#' common patterns, including:
#' \itemize{
#'   \item{\code{\link{render_scrollable_table}}: Creates a styled,
#'     paginated, scrollable data table with fallback handling.}
#'   \item{\code{\link{render_conditional_download_button}}: Dynamically
#'     shows a CSV download button only if data is available.}
#'   \item{\code{\link{log_msg}}: Conditionally creates log messages.}
#' }
#'
#' Designed for integration within modular dashboard panels that rely on
#' reactive data sources, customizable layout styling, and filter-aware output
#' behavior.
#'
#' @section Dependencies:
#' Requires \code{DT}, \code{shiny}, and optionally \code{glue} for logging.
#'
#' @keywords internal utilities server rendering reactive output

#' Render Scrollable Data Table
#'
#' Returns a styled datatable if data is available.
#'
#' @param df A data.frame to render.
#'
#' @return A DT::datatable or tagList.
#' @export
render_scrollable_table <- function(df) {
  if (nrow(df) == 0 || all(is.na(df))) {
    return(shiny::tagList(shiny::p(fallback)))
  }
  
  log_msg(glue::glue("[MODULE] Rendering scrollable data table."))
  
  DT::datatable(
    df,
    extensions = "Scroller",
    options = list(
      pageLength = 10,
      scrollY = 300,
      scrollX = TRUE,
      scrollCollapse = TRUE,
      dom = "tp"
    ),
    rownames = FALSE,
    class = "compact stripe hover"
  )
}

#' Render Conditional Download Button UI
#'
#' Returns a Shiny download button only if data has non-zero rows.
#'
#' @param output_id ID for the download button.
#' @param df A data.frame to check for availability.
#' @param label Button label (default: "Download CSV").
#' @param class CSS class for styling.
#'
#' @return downloadButton or NULL.
#' @export
render_conditional_download_button <- function(
    ns, output_id, df, label = "Download CSV", class = "download-btn"
) {
  if (nrow(df) == 0 || all(is.na(df))) return(NULL)
  
  shiny::downloadButton(outputId = ns(output_id), label = label, class = class)
}

#' Conditional Logging
#'
#' Allows log messages if `enable_logging` exists and is set to TRUE. Allows
#' an optional, additional condition to creating the message.
#'
#' @param msg Log message.
#' @param cond A separate conditional. Must evaluate to TRUE or FALSE. 
#'   Default is TRUE.
#'
#' @return Prints `msg` in the log, conditionally.
#' @export
log_msg <- function(msg, cond = TRUE) {
  if (exists("enable_logging", inherits = TRUE) && enable_logging && cond) {
    message(msg)
  }
}

