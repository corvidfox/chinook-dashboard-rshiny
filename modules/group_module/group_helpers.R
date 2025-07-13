#' @file group_helpers.R
#' @title Group Performance Module Helpers
#'
#' Shared utilities for querying, formatting, and visualizing genre-level 
#' performance KPIs in the Group Performance panels of the Chinook 
#' dashboard.
#'
#' This file includes:
#' \itemize{
#'   \item{\code{\link{get_group_yearly_summary}}: SQL query for genre-level 
#'     KPI data.}
#'   \item{\code{\link{group_plotter}}: Builds styled Plotly stacked bar plots.}
#' }
#'
#' These helpers are reactive-safe, modular in design, and compatible 
#' with dynamic filters, light/dark themes, and flexible metric overlays.
#'
#' Recommended for use in server-side logic within \code{group_server.R}, 
#' and optionally paired with formatting utilities from \code{kpi_utils.R}.
#' 
#' @note All functions assume prior setup of a DuckDB temporary table named
#' \code{filtered_invoices}. That table should be pre-filtered for genre,
#' artist, and country before calling the module helpers.
#'
#' @keywords internal helper group-performance module dashboard reactive SQL

#' @title Query Yearly KPIs by Artist or Genre
#' @description
#' Retrieves yearly performance KPIs for either artist or genre,
#' using pre-filtered invoice data in `filtered_invoices` and
#' catalog reference tables (`artist_catalog`, `genre_catalog`).
#' Returns one row per year per group value.
#'
#' KPIs include:
#' - Revenue
#' - Number of purchases
#' - Tracks sold
#' - Unique tracks sold
#' - Number of customers
#' - First-time customers
#' - Number of countries
#' - Catalog size
#' - Number of tracks sold for the first time in selected period
#'
#' @param con A DBI connection object to DuckDB.
#' @param group_var Either "Genre" or "Artist".
#' @param date_range A character vector of length 2: start and end date.
#'
#' @return A data.frame with columns: year, group_val, and KPI fields.
#' @export
get_group_yearly_summary <- function(con, group_var = "Genre", date_range) {
  stopifnot(DBI::dbIsValid(con))
  group_var <- rlang::arg_match(group_var, c("Genre", "Artist"))
  
  if (!is.character(date_range) || length(date_range) != 2) {
    stop("`date_range` must be a character vector of length 2.")
  }
  
  log_msg("[SQL] get_group_yearly_summary(): querying data for {group_var}")
  
  group_fields <- switch(group_var,
                         Genre = list(
                           join_clause = "
        JOIN Track t ON il.TrackId = t.TrackId
        JOIN Genre g ON g.GenreId = t.GenreId
        LEFT JOIN genre_catalog gc ON gc.genre = g.Name
      ",
                           group_expr = "g.Name",
                           catalog_expr = "gc.num_tracks"
                         ),
                         Artist = list(
                           join_clause = "
        JOIN Track t ON il.TrackId = t.TrackId
        JOIN Album al ON al.AlbumId = t.AlbumId
        JOIN Artist ar ON ar.ArtistId = al.ArtistId
        LEFT JOIN artist_catalog ac ON ac.artist = ar.Name
      ",
                           group_expr = "ar.Name",
                           catalog_expr = "ac.num_tracks"
                         )
  )
  
  query <- glue::glue_sql("
    WITH base AS (
      SELECT
        fi.CustomerId,
        fi.dt,
        i.BillingCountry AS country,
        fi.InvoiceId,
        il.TrackId,
        il.Quantity,
        il.UnitPrice,
        {DBI::SQL(group_fields$group_expr)} AS group_val,
        {DBI::SQL(group_fields$catalog_expr)} AS catalog_size
      FROM filtered_invoices fi
      JOIN Invoice i ON fi.InvoiceId = i.InvoiceId
      JOIN InvoiceLine il ON fi.InvoiceId = il.InvoiceId
      {DBI::SQL(group_fields$join_clause)}
      WHERE fi.dt BETWEEN DATE({date_range[1]}) AND DATE({date_range[2]})
    ),
    first_purchases AS (
      SELECT CustomerId, MIN(dt) AS first_date
      FROM base
      GROUP BY CustomerId
    ),
    first_track_sales AS (
      SELECT TrackId, MIN(dt) AS first_sold_date
      FROM base
      GROUP BY TrackId
    )
    SELECT
      STRFTIME('%Y', b.dt) AS year,
      b.group_val,
      COUNT(DISTINCT b.CustomerId) AS num_customers,
      COUNT(DISTINCT b.InvoiceId) AS num_purchases,
      COUNT(DISTINCT b.country) AS num_countries,
      SUM(b.Quantity) AS tracks_sold,
      SUM(b.UnitPrice * b.Quantity) AS revenue,
      COUNT(DISTINCT CASE
        WHEN STRFTIME('%Y', b.dt) = STRFTIME('%Y', fp.first_date)
        THEN b.CustomerId END) AS first_time_customers,
      COUNT(DISTINCT b.TrackId) AS unique_tracks_sold,
      COUNT(DISTINCT CASE
        WHEN STRFTIME('%Y', b.dt) = STRFTIME('%Y', fts.first_sold_date)
        THEN b.TrackId END) AS first_tracks_sold,
      ANY_VALUE(b.catalog_size) AS catalog_size
    FROM base b
    LEFT JOIN first_purchases fp ON b.CustomerId = fp.CustomerId
    LEFT JOIN first_track_sales fts ON b.TrackId = fts.TrackId
    GROUP BY year, b.group_val
    ORDER BY year, b.group_val
  ", .con = con)
  
  DBI::dbGetQuery(con, query) |>
    dplyr::rename_at("group_val", ~tolower(group_var))
}

#' @title Stacked Bar Chart for Group Performance Metrics
#' @description
#' Generates a stacked bar chart of yearly performance metrics 
#' for grouped entities (e.g., Genre or Artist). Highlights the top N groups 
#' based on aggregate metric values, supports hover formatting, and applies 
#' theme-aware styling via ggplot2 and Plotly.
#'
#' @param df A data.frame with yearly metrics by group.
#' @param metric Named list with `var_name` and `label`.
#' @param group_var Character string: `"Genre"` or `"Artist"`.
#' @param group_label Display label for axis and tooltip.
#' @param styles Theme-aware style list.
#' @param max_n Integer: number of top groups to display.
#'
#' @return A Plotly stacked bar chart object.
#' @export
group_plotter <- function(df, metric, group_var, group_label, styles, max_n){
  # Defensive checks
  stopifnot(is.data.frame(df), is.list(metric), is.list(styles))
  
  y_var <- metric$var_name
  y_label <- metric$label
  s <- styles
  
  # Fall back if there are no rows to plot
  if (nrow(df) == 0 || all(is.na(df[[y_var]]))) {
    return(
      plotly::plotly_empty(type = "scatter", mode = "markers") %>% 
        plotly::layout(title = "No data available for selected filters")
    )
  }
  
  log_msg(glue::glue(
    "[GROUP] group_plotter(): building stacked bar plot for {group_var}"
    ))
  
  # Format year as factor so it orders correctly (earliest year on bottom)
  df$year <- factor(
    df$year, 
    levels = sort(unique(df$year), decreasing = TRUE)
  )
  
  # Dynamically sort group by aggregate of selected metric
  df <- df |>
    dplyr::rename(group_var = !!rlang::sym(tolower(group_var))) |>
    dplyr::group_by(group_var) |>
    dplyr::mutate(total_metric = sum(.data[[y_var]], na.rm = TRUE)) |>
    dplyr::ungroup() |>
    dplyr::mutate(group_var = reorder(group_var, -total_metric))
  
  # Restrict to top `max_n` values of the group by total metric (for space)
  top_groups <- df %>%
    dplyr::distinct(group_var) %>%
    dplyr::slice_head(n = min(nrow(.), max_n)) %>%
    dplyr::pull(group_var)
  
  df <- df %>%
    dplyr::filter(group_var %in% top_groups)
  
  # Add hover text
  df$hover_text <- paste0(
    group_var, ": ", df$group_var, "<br>",
    "Year: ", df$year, "<br>",
    "Total Revenue (USD$): ", format_kpi_value(df$revenue, "dollar"), "<br>",
    "Purchases: ", format_kpi_value(df$num_purchases, "number"), "<br>",
    "Tracks Sold: ", format_kpi_value(df$tracks_sold, "number"), "<br>",
    "Tracks in Catalog: ", format_kpi_value(df$catalog_size, "number"), "<br>",
    "Revenue per Track in Catalog: ", 
    format_kpi_value(df$revenue / df$catalog_size, "dollar"), "<br>",
    "Percentage of Catalog Sold: ", 
    format_kpi_value(
      df$unique_tracks_sold / df$catalog_size, "percent"
    )
  )
  
  base_title <- paste(
    group_label, "Performance:", y_label, "- Top", length(top_groups)
    )
  wrapped_title <- if (nchar(base_title) > 40) {
    stringr::str_wrap(base_title, width = 30)
  } else {
    base_title
  }
  
  
  # Make Stacked Bar Plot - Suppress "Text" Warning
  p <- suppressWarnings(ggplot2::ggplot(
    df, 
    ggplot2::aes(
      x = group_var, y = .data[[y_var]], fill = year, text = hover_text
    )) +
      # Make stacked bar plots for aggregate view
      ggplot2::geom_bar(stat = "identity", position = "stack") +
      # Format y-axis for legibility
      ggplot2::scale_y_continuous(
        labels = if (y_var == "revenue") scales::dollar else scales::comma
      ) +
      ggplot2::scale_fill_viridis_d(
        option = "D",
        direction = if (s$fill_palette$reversescale) 1 else -1,
        end = 0.9
      ) +
      # Labels
      ggplot2::labs(
        title = wrapped_title,
        x = group_label,
        y = y_label,
        fill = "Year"
      )
  ) 
  
  p <- p %>%
    style_ggplot2(styles = s) +
    ggplot2::theme(
      legend.position = "bottom",
      # Angle x-axis labels for legibility
      axis.text.x = ggplot2::element_text(
        angle = 45, vjust = 0.5, hjust = 1, size = 8
      )
    )
  
  plotly::ggplotly(p, tooltip = "text") %>%
    style_plotly(
      styles = s,
      axes_labs = list(x = group_label, y = y_label, legend = "Year")
    ) %>%
    # Override hover mode of styler back to default
    plotly::layout(hovermode = 'closest')
}

# Memoised version of get_group_yearly_summary() to avoid recomputation across 
# identical filter sets
memo_get_group_yearly_summary <- 
  memoise::memoise(get_group_yearly_summary, cache = shared_cache)
