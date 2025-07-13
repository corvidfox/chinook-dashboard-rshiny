#' @file geo_helpers.R
#' @title Geographic Distribution Module Helpers
#'
#' Shared utilities for querying, formatting, and visualizing country-level 
#' performance KPIs in the Geographic Distribution panel of the Chinook 
#' dashboard.
#'
#' This file includes:
#' \itemize{
#'   \item{\code{\link{get_geo_metrics}}: SQL query for country-level KPI data.}
#'   \item{\code{\link{geo_plotter}}: Builds styled Plotly choropleth plots.}
#' }
#'
#' These helpers are reactive-safe, modular in design, and compatible 
#' with dynamic filters, light/dark themes, and flexible metric overlays.
#'
#' Recommended for use in server-side logic within \code{geo_server.R}, 
#' and optionally paired with formatting utilities from \code{kpi_utils.R}.
#' 
#' @note All functions assume prior setup of a DuckDB temporary table named
#' \code{filtered_invoices}. That table should be pre-filtered for genre,
#' artist, and country before calling the module helpers.
#'
#' @keywords internal helper geographic-distribution module dashboard reactive SQL

#' Get Country-Year Geographic Summary
#'
#' Queries the filtered_invoices temp table in DuckDB and returns
#' year-country aggregates for KPIs, including revenue, purchases,
#' tracks sold, customer counts, and first-time customer flag.
#' Data is pre-grouped by year or in aggregate to reduce processing in R.
#'
#' @param con DBI connection to DuckDB (validated).
#' @param date_range Character vector of length 2 (YYYY-MM-DD).
#' @param mode Either 'yearly' or 'aggregate'
#'
#' @return A data.frame with one row per year and country.
#' @export
get_geo_metrics <- function(con, date_range, mode = "yearly") {
  mode <- match.arg(mode, choices = c("yearly", "aggregate"))
  
  stopifnot(DBI::dbIsValid(con))
  if (!is.character(date_range) || length(date_range) != 2) {
    stop("`date_range` must be a character vector of length 2.")
  }
  
  log_msg("[SQL] get_geo_metrics(): querying pre-aggregated KPIs.")
  
  group_by_clause <- if (mode == "yearly") {
    glue::glue_sql("STRFTIME('%Y', fd.dt) AS year,")
  } else {
    glue::glue_sql("'All' AS year,")
  }
  
  group_fields <- if (mode == "yearly") {
    glue::glue_sql("STRFTIME('%Y', fd.dt), fd.country")
  } else {
    glue::glue_sql("fd.country")
  }
  
  query <- glue::glue_sql("
    WITH filtered_data AS (
      SELECT
        fi.CustomerId,
        fi.dt,
        i.BillingCountry AS country,
        fi.InvoiceId
      FROM filtered_invoices fi
      JOIN Invoice i ON fi.InvoiceId = i.InvoiceId
      WHERE fi.dt BETWEEN DATE({date_range[1]}) AND DATE({date_range[2]})
    ),
    first_purchases AS (
      SELECT CustomerId, MIN(dt) AS first_date
      FROM filtered_data
      GROUP BY CustomerId
    ),
    detailed AS (
      SELECT
        fd.CustomerId,
        fd.country,
        fd.dt,
        il.Quantity,
        il.UnitPrice
      FROM filtered_data fd
      JOIN InvoiceLine il ON fd.InvoiceId = il.InvoiceId
    )
    SELECT
      {group_by_clause}
      fd.country AS country,
      COUNT(DISTINCT fd.CustomerId) AS num_customers,
      COUNT(DISTINCT fd.InvoiceId) AS num_purchases,
      SUM(il.Quantity) AS tracks_sold,
      SUM(il.UnitPrice * il.Quantity) AS revenue,
      COUNT(DISTINCT CASE
        WHEN fd.dt = fp.first_date THEN fd.CustomerId
        ELSE NULL
      END) AS first_time_customers
    FROM filtered_data fd
    JOIN InvoiceLine il ON fd.InvoiceId = il.InvoiceId
    JOIN first_purchases fp ON fd.CustomerId = fp.CustomerId
    GROUP BY {group_fields}
    ORDER BY {group_fields}
  ", .con = con)
  
  result <- DBI::dbGetQuery(con, query)
  
  # Calculate months in each year, based on date range (regardless of activity)
  num_months <- data.frame(
    year = seq(lubridate::year(date_range[1]), lubridate::year(date_range[2]))
  ) |>
    dplyr::mutate(
      year = as.character(year),
      num_months = dplyr::case_when(
        year == lubridate::year(date_range[1]) ~ 
          13 - lubridate::month(date_range[1]),
        year == lubridate::year(date_range[2]) ~ 
          lubridate::month(date_range[2]),
        TRUE ~ 12
      )
    )
  
  dplyr::left_join(result, num_months, by = 'year') |>
    dplyr::relocate('num_months', .after = 'year')
}

#' Create Animated Yearly Choropleth Plot with Metric Overlay
#'
#' Generates an interactive Plotly choropleth plot showing the country-level
#' geographic distribution of a selected KPI metric each year and in 
#' aggregate, using pre-aggregated country-level data. Visual style
#' dynamically adapts to light/dark theme options.
#'
#' Tooltips include expanded country-year KPIs including revenue, purchases,
#' tracks sold, customer counts, and derived metrics.
#'
#' @param df A data.frame of country-level KPI summary data.
#' @param metric A named list with metric details:
#' \describe{
#'   \item{var_name}{Column name for y-axis metric.}
#'   \item{label}{Display label for y-axis metric.}
#' }
#' @param styles Named list of plot style elements (colors, sizes).
#'
#' @return A Plotly HTML widget (interactive choropleth plot).
#' @export
geo_plotter <- function(df, metric, styles) {
  # Defensive checks
  stopifnot(is.data.frame(df), is.list(metric), is.list(styles))
  
  y_var <- metric$var_name
  y_label <- metric$label
  s <- styles
  
  # Add ISO3 and remove values missing the value (should be none)
  df <- df |>
    dplyr::mutate(iso_alpha = standardize_country_to_iso3(country)) |>
    dplyr::filter(!is.na(iso_alpha))
  
  # Fallback: Empty plot if no data
  if (nrow(df) == 0 || all(is.na(df[[y_var]]))) {
    return(
      plotly::plotly_empty(type = "scatter", mode = "markers") %>%
        plotly::layout(title = "No data available for selected filters")
    )
  }
  
  log_msg("[GEO] geo_plotter(): building choropleth plot.")
  
  # Make 'year' into an ordered factor, with 'All' last in animation
  year_vals <- c(sort(unique(df$year[df$year != "All"])), "All")
  df <- df |>
    dplyr::mutate(year = factor(year, levels = year_vals))
  
  # Hover Text KPIs
  df$hover <- paste0(
    "Country: ", df$country, "<br>",
    "Year: ", df$year, "<br>",
    "Revenue: ", format_kpi_value(df$revenue, "dollar"), "<br>",
    "Purchases: ", format_kpi_value(df$num_purchases, "number"), "<br>",
    "Tracks Sold: ", format_kpi_value(df$tracks_sold, "number"), "<br>",
    "Unique Customers: ", format_kpi_value(df$num_customers, "number"), "<br>",
    "First Time Customers: ", 
    format_kpi_value(df$first_time_customers, "number"), "<br>",
    "Revenue per Customer: ", 
    format_kpi_value(df$revenue / df$num_customers, "dollar")
  )
  
  # Format Legend
  fmt_prefix <- switch(
    metric$var_name,
    "revenue" = "$",
    "num_customers" = "",
    "tracks_sold" = "",
    "first_time_customers" = "",
    "num_purchases" = "",
    ""
  )
  
  fmt_tick <- switch(
    metric$var_name,
    "revenue" = ".2f",
    "num_customers" = ",d",
    "tracks_sold" = ",d",
    "first_time_customers" = ",d",
    "num_purchases" = ",d",
    ".2f"
  )
  
  # Plot construction
  plotly::plot_ly(
    data = df,
    type = "choropleth",
    locations = ~iso_alpha,
    z = df[[y_var]],
    # Set minimum and maximum Z values, for consistent scale
    zmin = min(df[[y_var]]),
    zmax = max(df[[y_var]]),
    # Hover Text
    text = df$hover,
    hoverinfo = "text",
    colorscale = s$fill_palette$colorscale,
    reversescale = s$fill_palette$reversescale,
    showscale = TRUE,
    ## Label legend
    colorbar = list(
      title = y_label,
      tickprefix = fmt_prefix,
      tickformat = fmt_tick
    ),
    # Give national boundaries an outline
    marker = list(
      line = list(color = s$geo_border_color, width = s$geo_border_width)
    ),
    frame = ~year
  ) %>%
    style_plotly(styles = s, type = "geo") %>%
    plotly::layout(
      title = list(
        text = paste(y_label, "by Country (Animated by Year)")
      )
    ) %>%
    plotly::animation_opts(frame = 1000, transition = 0, redraw = TRUE)
}
