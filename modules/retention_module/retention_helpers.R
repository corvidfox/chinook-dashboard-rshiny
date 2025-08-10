#' @file retention_helpers.R
#' @title Customer Retention Module Helpers
#'
#' Shared utilities for querying, formatting, and visualizing customer 
#' retention KPIs in the Customer Retention panel of the Chinook dashboard.
#'
#' This file includes:
#' \itemize{
#'   \item{\code{\link{get_retention_decay_data}}: SQL query for retention 
#'     decay data.}
#'   \item{\code{\link{decay_plotter}}: Builds styled Plotly retention 
#'     decay-curve plots.}
#'   \item{\code{\link{cohort_heatmap_plotter}}: Builds styled Plotly cohort
#'     retention heatmap plots.}
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
#' artist, and country before calling the customer retention helpers.
#'
#' @keywords internal helper customer-retention module dashboard reactive SQL
#' 

#' @title Get Global Retention Decay Curve Data
#' @description
#' Computes monthly customer retention across cohort offsets using 
#' filtered subset activity. Base cohort is defined from full Invoice data.
#'
#' @param con DBI connection to DuckDB
#' @param tbl Invoice table name (e.g., "filtered_invoices")
#' @param date_range A character vector of length 2: start and end date.
#' @param max_offset Optional integer max offset (default = inferred from 
#'   range)
#'
#' @return A data.frame with one row per month_offset.
#' @export
get_retention_decay_data <- function(
    con, tbl, date_range = NULL, max_offset = NULL
) {
  stopifnot(DBI::dbIsValid(con), is.character(tbl), length(tbl) == 1)
  
  if (!is.character(date_range) || length(date_range) != 2) {
    stop("`date_range` must be a character vector of length 2.")
  }
  
  log_msg("[SQL] get_retention_decay_data(): querying pre-aggregated data.")
  
  if (is.null(max_offset)) {
    date_bounds <- DBI::dbGetQuery(con, glue::glue_sql("
        SELECT 
          MIN(i.InvoiceDate) AS min_date,
          MAX(i.InvoiceDate) AS max_date
        FROM {`tbl`} fi
        JOIN Invoice i ON fi.InvoiceId = i.InvoiceId
      ", .con = con))
    
    max_offset <- lubridate::interval(
      date_bounds$min_date, date_bounds$max_date
    ) %/% months(1)
    
  }
  
  offset_clause <- if (!is.null(max_offset)) {
    glue::glue_sql(
      "AND DATE_DIFF('month', c.cohort_start, i.InvoiceDate) <= {max_offset}",
      .con = con
    )
  } else {
    glue::glue_sql("", .con = con)
  }
  
  # Use correct date column based on table
  date_column <- if (tbl == "Invoice") "InvoiceDate" else "dt"
  
  query <- glue::glue_sql("
    WITH cohorts AS (
      SELECT
        CustomerId,
        DATE_TRUNC('month', MIN(InvoiceDate)) AS cohort_start
      FROM Invoice
      GROUP BY CustomerId
    ),
    activity AS (
      SELECT
        fi.CustomerId,
        i.InvoiceDate,
        DATE_TRUNC('month', c.cohort_start) AS cohort_month,
        DATE_TRUNC('month', i.InvoiceDate) AS activity_month,
        DATE_DIFF('month', c.cohort_start, i.InvoiceDate) AS month_offset
      FROM {`tbl`} fi
      JOIN Invoice i ON i.InvoiceId = fi.InvoiceId
      JOIN cohorts c ON c.CustomerId = fi.CustomerId
      WHERE DATE_DIFF('month', c.cohort_start, i.InvoiceDate) >= 0
        AND fi.{DBI::SQL(date_column)} BETWEEN DATE({date_range[1]}) AND DATE({date_range[2]})
      {offset_clause}
    ),
    retention AS (
      SELECT 
        month_offset,
        COUNT(DISTINCT CustomerId) AS num_retained
      FROM activity
      GROUP BY month_offset
    ),
    cohort_size AS (
      SELECT COUNT(DISTINCT CustomerId) AS num_customers FROM activity
    )
    SELECT
      r.month_offset,
      r.num_retained,
      cs.num_customers,
      r.num_retained * 1.0 / cs.num_customers AS retention_rate
    FROM retention r, cohort_size cs
    WHERE r.month_offset > 0
    ORDER BY r.month_offset
  ", .con = con)
  
  DBI::dbGetQuery(con, query)
}

#' Create Retention Decay Curve Plot
#'
#' Generates an interactive Plotly line and scatter plot showing customer 
#' retention decay by month offset, using pre-aggregated data. Visual style
#' dynamically adapts to light/dark theme options.
#'
#' Tooltips include month offset and retention rate.
#'
#' @param df A data.frame of retention decay summary data.
#' @param styles Named list of plot style elements (colors, sizes).
#'
#' @return A Plotly HTML widget (interactive retention decay curve plot).
#' @export
decay_plotter <- function(df, styles) {
  # Defensive checks
  stopifnot(is.data.frame(df), is.list(styles))
  
  s <- styles
  
  # Fallback: Empty plot if no data
  if (
    nrow(df) == 0 || all(is.na(df$month_offset)) || 
    all(is.na(df$retention_rate))
  ) {
    return(
      plotly::plotly_empty(type = "scatter", mode = "markers") %>%
        plotly::layout(title = "No data available for selected filters")
    )
  }
  
  log_msg("[RETENTION] decay_plotter(): building retention-decay plot.")
  
  # Plot construction
  p <- suppressWarnings(
    ggplot2::ggplot(
      df,
      ggplot2::aes(x = month_offset, y = retention_rate)
    ) +
      ggplot2::geom_line(
        color = s$line_color, size = s$line_size
      ) +
      ggplot2::geom_point(
        ggplot2::aes(
          text = paste0(
            "Month Offset: ", format_kpi_value(month_offset, "number"), 
            "<br> Retention: ", 
            format_kpi_value(retention_rate, "percent"), "<br>"
          )
        ),
        color = s$point_color, size = s$point_size
      ) +
      ggplot2::scale_y_continuous(
        labels = scales::percent
      ) +
      ggplot2::scale_x_continuous(
        labels = scales::comma
      ) +
      ggplot2::labs(
        title = "Customer Retention Decay",
        x = "Months Since First Purchase",
        y = "Retention (%)"
      )
  )
  
  # Apply ggplot2 and plotly styling
  styled_plot <- p |>
    style_ggplot2(styles = s) |>
    plotly::ggplotly(tooltip = "text") |>
    style_plotly(styles = s)
  
  return(styled_plot)
}

#' Create Cohort Retention Heatmap Plot
#'
#' Generates an interactive Plotly heatmap showing customer cohort retention 
#' rate by month offset, using pre-aggregated data. Visual style dynamically 
#' adapts to light/dark theme options.
#'
#' Tooltips include cohort, month offset, cohort size, and retention rate.
#'
#' @param df A data.frame of cohort retention summary data.
#' @param styles Named list of plot style elements (colors, sizes).
#'
#' @return A Plotly HTML widget (interactive retention decay curve plot).
#' @export
cohort_heatmap_plotter <- function(df, styles) {
  # Defensive checks
  stopifnot(is.data.frame(df), is.list(styles))
  
  s <- styles
  
  # Fallback: Empty plot if no data
  if (
    nrow(df) == 0 || all(is.na(df$cohort_month)) || 
    all(is.na(df$month_offset)) || all(is.na(df$retention_pct)) || 
    all(is.na(df$cohort_size))
  ) {
    return(
      plotly::plotly_empty(type = "scatter", mode = "markers") %>%
        plotly::layout(title = "No data available for selected filters")
    )
  }
  
  log_msg(paste0(c(
    "[RETENTION] cohort_heatmap_plotter(): ",
    "building cohort retention heatmap plot."
    )))
  
  # Ensure all Cohort-Months are represented
  df <- tidyr::complete(
    df,
    cohort_month = seq.Date(
      min(df$cohort_month, na.rm = TRUE),
      max(df$cohort_month, na.rm = TRUE),
      by = "month"
    ),
    month_offset = seq(
      min(df$month_offset, na.rm = TRUE),
      max(df$month_offset, na.rm = TRUE),
      by = 1
    )
  ) |>  
    dplyr::mutate(
      hover_text = dplyr::if_else(
        !is.na(retention_pct),
        paste0(
          "Cohort: ", format(cohort_month, "%b %Y"), "\n",
          "Month Offset: ", format_kpi_value(month_offset, "number"), "\n",
          "Cohort Size: ", format_kpi_value(cohort_size, "number"), "\n",
          "Retention: ", format_kpi_value(retention_pct, "percent")
        ),
        paste0(
          "Cohort: ", format(cohort_month, "%b %Y"), "\n",
          "Month Offset: ", format_kpi_value(month_offset, "number"), "\n",
          "No data"
        )
      )
    )
  
  # Plot construction
  p <- suppressWarnings(
    ggplot2::ggplot(
      df,
      ggplot2::aes(
        x = month_offset, 
        y = cohort_month,
        fill = retention_pct,
        text = hover_text
      )
    ) +
      ggplot2::geom_tile(color = s$ggplot_bg, size = 0.2) +
      ggplot2::scale_fill_viridis_c(
        na.value = s$plotly_hover_bg,
        option = "D",
        direction = if (s$fill_palette$reversescale) 1 else -1,
        labels = scales::percent
      ) +
      scale_x_continuous(breaks = pretty(df$month_offset)) +
      scale_y_date(date_breaks = "1 month", date_labels = "%b %Y") +
      ggplot2::labs(
        title = "Customer Retention by Cohort",
        x = "Months Since First Purchase",
        y = "Cohort Month (First Purchase)",
        fill = "Retention (%)"
      )
  )
  
  # Apply ggplot2 and plotly styling
  p <- p %>%
    style_ggplot2(styles = s) +
    ggplot2::theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid = element_blank()
    )
  
  plotly::ggplotly(p, tooltip = "text") %>%
    style_plotly(styles = s)
  
}
