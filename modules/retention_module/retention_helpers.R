#' @file retention_helpers.R
#' @title Customer Retention Module Helpers
#'
#' Shared utilities for querying, formatting, and visualizing monthly 
#' performance KPIs in the Customer Retention panel of the Chinook dashboard.
#'
#' This file includes:
#' \itemize{
#'   \item{\code{\link{get_ts_monthly_summary}}: SQL query for monthly KPI data.}
#'   \item{\code{\link{format_ts_kpi_display}}: Prettifies shared KPIs for UI cards.}
#'   \item{\code{\link{ts_plotter}}: Builds styled Plotly time-series plots.}
#' }
#'
#' These helpers are reactive-safe, modular in design, and compatible 
#' with dynamic filters, light/dark themes, and flexible metric overlays.
#'
#' Recommended for use in server-side logic within \code{retention_server.R}, 
#' and optionally paired with formatting utilities from \code{kpi_utils.R}.
#' 
#' @note All functions assume prior setup of a DuckDB temporary table named
#' \code{filtered_invoices}. That table should be pre-filtered for genre,
#' artist, and country before calling the time series helpers.
#'
#' @keywords internal helper customer-retention module dashboard reactive SQL