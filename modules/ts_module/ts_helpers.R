#' @file ts_helpers.R
#' @title Time Series Module Helpers
#'
#' Shared utilities for querying, formatting, and visualizing monthly 
#' performance KPIs in the Time Series panel of the Chinook dashboard.
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
#' Recommended for use in server-side logic within \code{ts_server.R}, 
#' and optionally paired with formatting utilities from \code{kpi_utils.R}.
#' 
#' @note All functions assume prior setup of a DuckDB temporary table named
#' \code{filtered_invoices}. That table should be pre-filtered for genre,
#' artist, and country before calling the time series helpers.
#'
#' @keywords internal helper time-series module dashboard reactive SQL

#' Get Monthly Time Series Summary
#'
#' Queries the filtered_invoices temp table in DuckDB and returns
#' monthly aggregates for KPIs, including revenue, purchases,
#' tracks sold, customer counts, and first-time customer flag.
#' Data is pre-grouped by month to reduce processing in R.
#'
#' @param con DBI connection to DuckDB (validated).
#' @param date_range Character vector of length 2 (YYYY-MM-DD).
#'
#' @return A data.frame with one row per month.
#' @export
get_ts_monthly_summary <- function(con, date_range) {
  stopifnot(DBI::dbIsValid(con))
  if (!is.character(date_range) || length(date_range) != 2) {
    stop("`date_range` must be a character vector of length 2.")
  }
  
  if (exists("enable_logging", inherits = TRUE) && enable_logging) { 
    message("[SQL] get_ts_monthly_summary(): querying pre-aggregated KPIs.")
  }
  
  query <- glue::glue_sql("
    WITH first_invoices AS (
      SELECT 
        CustomerId,
        MIN(dt) AS first_invoice_date
      FROM filtered_invoices
      GROUP BY CustomerId
    ),
    invoice_expanded AS (
      SELECT 
        fi.CustomerId,
        fi.InvoiceId,
        fi.dt AS invoice_date,
        STRFTIME('%Y-%m', fi.dt) AS month,
        i.BillingCountry,
        i.BillingState,
        il.Quantity,
        il.UnitPrice,
        CASE 
          WHEN STRFTIME('%Y-%m', fi.dt) = 
               STRFTIME('%Y-%m', fi2.first_invoice_date) THEN 1
          ELSE 0
        END AS first_time_flag
      FROM filtered_invoices fi
      JOIN Invoice i ON fi.InvoiceId = i.InvoiceId
      JOIN InvoiceLine il ON i.InvoiceId = il.InvoiceId
      JOIN first_invoices fi2 ON fi.CustomerId = fi2.CustomerId
      WHERE DATE(fi.dt) BETWEEN DATE({date_range[1]}) 
        AND DATE({date_range[2]})
    )
    SELECT 
      month,
      COUNT(DISTINCT InvoiceId) AS num_purchases,
      COUNT(DISTINCT CustomerId) AS num_customers,
      SUM(Quantity) AS tracks_sold,
      SUM(UnitPrice * Quantity) AS revenue,
      SUM(first_time_flag) AS first_time_customers
    FROM invoice_expanded
    GROUP BY month
    ORDER BY month;
  ", .con = con)
  
  DBI::dbGetQuery(con, query)
}

#' Format Time Series KPI Display Values
#'
#' Extracts relevant KPIs from the kpis_shared object and formats
#' them for visual display in Shiny cards or tooltips. All numerical
#' fields are prettified using `format_kpi_value()`. Only calculates average
#' revenue per month, otherwise formats existing metrics.
#'
#' @param kpis_shared Named list containing precomputed KPI objects.
#' @param date_range Character vector of length 2 (YYYY-MM-DD).
#'
#' @return Named list of formatted KPI values (all character strings).
#' @export
format_ts_kpi_display <- function(kpis_shared, date_range) {
  if (exists("enable_logging", inherits = TRUE) && enable_logging) { 
    message("[KPI] format_ts_kpi_display(): formatting KPIs from kpis_shared.")
  }
  
  # Defensive checks
  if (!is.character(date_range) || length(date_range) != 2) {
    stop("`date_range` must be a character vector of length 2.")
  }
  
  if(is.null(kpis_shared)){
    if (exists("enable_logging", inherits = TRUE) && enable_logging) { 
      message("[KPI] format_ts_kpi_display(): No shared KPIs.")
    }
    return(NULL)
  }
  
  if (!is.list(kpis_shared)) {
    warning("Invalid `kpis_shared`. Must be a named list.")
    return(NULL)
  }
  
  num_months <- lubridate::interval(
    lubridate::ymd(date_range[1]),
    lubridate::ymd(date_range[2])
  ) %/% months(1) + 1
  
  kpi_list <- list(
    total_rev         = format_kpi_value(
      kpis_shared$revenue_kpis$total_revenue, "dollar"
    ),
    avg_rev           = format_kpi_value(
      (kpis_shared$revenue_kpis$total_revenue / num_months), 
      "dollar"
    ),
    total_purchases   = format_kpi_value(
      kpis_shared$purchase_kpis$total_orders, "number"
    ),
    total_tracks      = format_kpi_value(
      kpis_shared$purchase_kpis$total_tracks, "number"
    ),
    avg_per_purchase  = format_kpi_value(
      kpis_shared$purchase_kpis$avg_rev_per_order, "dollar"
    ),
    total_customers   = format_kpi_value(
      kpis_shared$customer_kpis$total_customers, "number"
    ),
    first_time_pct    = format_kpi_value(
      kpis_shared$customer_kpis$pct_new_customers, "percent"
    )
  )
  
  return(kpi_list)
}

#' Create Time Series Plot with Metric Overlay
#'
#' Generates an interactive Plotly line and scatter plot showing a selected
#' KPI metric over time, using pre-aggregated monthly data. Visual style
#' dynamically adapts to light/dark theme options.
#'
#' Tooltips include expanded monthly KPIs including revenue, purchases,
#' tracks sold, customer counts, and derived metrics.
#'
#' @param df A data.frame of monthly KPI summary data.
#' @param metric A named list with metric details:
#' \describe{
#'   \item{var_name}{Column name for y-axis metric.}
#'   \item{label}{Display label for y-axis metric.}
#' }
#' @param styles Named list of plot style elements (colors, sizes).
#'
#' @return A Plotly HTML widget (interactive time series plot).
#' @export
ts_plotter <- function(df, metric, styles) {
  # Defensive checks
  stopifnot(is.data.frame(df), is.list(metric), is.list(styles))
  
  y_var <- metric$var_name
  y_label <- metric$label
  s <- styles
  
  # Fallback: Empty plot if no data
  if (nrow(df) == 0 || all(is.na(df[[y_var]]))) {
    return(
      plotly::plotly_empty(type = "scatter", mode = "markers") %>%
        plotly::layout(title = "No data available for selected filters")
    )
  }
  
  if (exists("enable_logging", inherits = TRUE) && enable_logging) {
    message("[TS] ts_plotter(): building time-series plot.")
  }
  
  # Plot construction
  p <- suppressWarnings(
    ggplot2::ggplot(
      df,
      ggplot2::aes(x = lubridate::ym(month), y = .data[[y_var]])
    ) +
      ggplot2::geom_line(
        color = s$line_color, size = s$line_size
      ) +
      ggplot2::geom_point(
        ggplot2::aes(
          text = paste0(
            "Month: ", month, "<br>",
            "Revenue: ", format_kpi_value(revenue, "dollar"), "<br>",
            "Purchases: ", format_kpi_value(num_purchases, "number"), "<br>",
            "Tracks Sold: ", format_kpi_value(tracks_sold, "number"), "<br>",
            "Customers: ", format_kpi_value(num_customers, "number"), "<br>",
            "First-Time: ",
            format_kpi_value(first_time_customers, "number"), "<br>",
            "Rev / Cust: ", format_kpi_value(
              revenue / num_customers, "dollar"
            )
          )
        ),
        color = s$point_color, size = s$point_size
      ) +
      ggplot2::scale_y_continuous(
        labels = if (y_var == "revenue") scales::dollar else scales::comma
      ) +
      ggplot2::labs(
        title = paste(y_label, "by Month"),
        x = "Time",
        y = y_label
      )
  )
  
  # Apply ggplot2 and plotly styling
  styled_plot <- p |>
    style_ggplot2(styles = s) |>
    plotly::ggplotly(tooltip = "text") |>
    style_plotly(styles = s)
  
  return(styled_plot)
}

# Memoised version of get_ts_monthly_summary() to avoid recomputation across 
# identical filter sets
memo_get_ts_monthly_summary <- 
  memoise::memoise(get_ts_monthly_summary, cache = shared_cache)