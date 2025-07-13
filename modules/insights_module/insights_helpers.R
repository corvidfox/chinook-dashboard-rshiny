#' @file insights_helpers.R
#' @title Helper Functions for Insights Panel
#'
#' Provides UI builder functions for constructing formatted KPI cards in the
#' Insights panel. These functions support consistent layout and theme styling
#' for overall vs. filtered metrics across categories.
#'
#' Includes:
#' - \code{\link{make_insights_kpi_card}}: Builds one KPI card with bullets.
#' - \code{\link{generate_insights_kpi_cards_ui}}: Renders two cards side-by-side.
#'
#' Designed for use in \code{insights_server.R} and compatible with
#' formatting helpers from \code{kpi_cards_utils.R}.
#'
#' @keywords internal

#' Build a single KPI card for the Insights panel
#'
#' Constructs a themed KPI card with a list of bullet points representing
#' either summary values or formatted HTML lists.
#'
#' @param kpi_values Named list containing KPI values for display.
#' @param cfg Configuration list with:
#'   \describe{
#'     \item{icon}{Shiny tag for card icon.}
#'     \item{bullets}{List of bullet specs, each with:}
#'       \itemize{
#'         \item{\code{type}: Either "value" or "list".}
#'         \item{\code{label}: Text label for the bullet.}
#'         \item{\code{value_fn}/\code{list_fn}: Function to compute value.}
#'         \item{\code{tooltip}: Tooltip description.}
#'       }
#'   }
#' @param label Title for the KPI card.
#' @param styles Theme styles from \code{styles()}.
#'
#' @return A \code{safe_kpi_card()} UI component.
#' @export
make_insights_kpi_card <- function(kpi_values, cfg, label, styles) {
  # If KPI values are invalid or missing, return fallback card
  if (!is.list(kpi_values) || length(kpi_values) == 0) {
    return(
      safe_kpi_card(
        kpis    = list(),
        title   = label,
        icon    = cfg$icon,
        tooltip = paste0(label, " metrics (no data)"),
        styles  = styles,
        body_fn = function() {
          list(
            safe_kpi_entry(
              label   = "No data available",
              value   = "",
              tooltip = paste0("No metrics found for ", label)
            )
          )
        }
      )
    )
  }
  
  bullets <- purrr::map(cfg$bullets, function(b) {
    val <- switch(b$type,
                  value = b$value_fn(kpi_values),
                  list  = b$list_fn(kpi_values),
                  stop("Unknown bullet type: ", b$type)
    )
    
    safe_kpi_entry(
      label   = b$label,
      value   = val,
      tooltip = b$tooltip
    )
  })
  
  safe_kpi_card(
    kpis    = kpi_values,
    title   = label,
    icon    = cfg$icon,
    tooltip = paste0(label, " metrics"),
    styles  = styles,
    body_fn = function() bullets
  )
}

#' Render two KPI cards side-by-side for a given category
#'
#' Applies \code{make_insights_kpi_card()} to both the full dataset and
#' filtered subset for comparison, styled within a responsive layout.
#'
#' @param cfg A single category config list containing:
#'   \describe{
#'     \item{\code{get_overall}}{Function returning full KPI values.}
#'     \item{\code{get_subset}}{Function returning filtered KPI values.}
#'     \item{\code{icon}, \code{bullets}}{As defined in \code{make_insights_kpi_card}.}
#'   }
#' @param styles A list of theme styles from \code{styles()}.
#'
#' @return A \code{bslib::layout_column_wrap()} containing both cards.
#' @export
generate_insights_kpi_cards_ui <- function(cfg, styles) {
  stopifnot(is.list(cfg), is.list(styles))
  
  all_kpis    <- cfg$get_overall()
  subset_kpis <- cfg$get_subset()
  
  bslib::layout_column_wrap(
    width     = 1 / 2,
    min_width = 200,
    gap       = "1rem",
    fill      = TRUE,
    make_insights_kpi_card(all_kpis,    cfg, "All Data",     styles),
    make_insights_kpi_card(subset_kpis, cfg, "Filtered View", styles)
  )
}
