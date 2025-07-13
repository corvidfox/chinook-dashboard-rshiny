#' @file insights_helpers.R
#' @title Key Insights Module Helpers
#'
#' Shared utilities for formatting and visualizing insights panel metrics and
#' other UI elements.
#'
#' This file includes:
#' \itemize{
#'   \item{\code{\link{make_insights_kpi_card}}: Generates a single insights 
#'     panel KPI card.}
#'   \item{\code{\link{generate_insights_kpi_cards_ui}}: Generates the 
#'   side-by-side KPI cards for a given category.}
#' }
#'
#' These helpers are reactive-safe, modular in design, and compatible 
#' with dynamic filters, light/dark themes, and flexible metric overlays.
#'
#' Recommended for use in server-side logic within \code{retention_server.R}, 
#' and optionally paired with formatting utilities from \code{kpi_utils.R}.
#'
#' @keywords internal helper insights module dashboard reactive

#' Generate a single insights KPI card
#'
#' @param kpi_values Named list of KPI values for display.
#' @param cfg A list with elements:
#'   * icon: shiny tag for the card icon
#'   * bullets: list of bullet defs, each a list with
#'       - title: character
#'       - value_fn: function(kpi_values) -> character
#'       - subtitle: character
#' @param label Character; title for this KPI card.
#' @param styles A styles() list for theming.
#' @return A Shiny UI tag containing a styled KPI card.
#' @import shiny purrr
#' @export
make_insights_kpi_card <- function(kpi_values, cfg, label, styles) {
  stopifnot(is.list(kpi_values), is.list(cfg))
  
  # build bullet elements
  bullets <- purrr::map(cfg$bullets, function(b) {
    val <- switch(b$type,
                  value = b$value_fn(kpi_values),
                  list  = b$list_fn(kpi_values),
                  stop("Unknown bullet type: ", b$type)
    )
    
    build_kpi(
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

#' Generate the insights KPI cards UI
#'
#' This will render two cards per category (Overall vs Filtered).
#'
#' @param kpi_categories Named list of category configs. Each element must
#'   include `get_overall()`, `get_subset()`, `icon`, and `bullets`.
#' @param styles A styles() list for theming.
#' @return A shiny.tagList of KPI card layouts.
#' @import shiny purrr bslib
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