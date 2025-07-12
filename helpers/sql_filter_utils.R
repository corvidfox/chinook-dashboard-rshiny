#' @file sql_filter_utils.R
#' @title SQL Filter Utilities for Reactive Querying
#'
#' @description
#' Contains foundational SQL clause generators used across KPI functions.
#' These helpers format WHERE and JOIN conditions based on Shiny UI inputs.
#' Designed for safe interpolation via `glue::glue_sql()` and optimized
#' to support modular, reactive pipelines across filtered data subsets.
#'
#' @keywords internal

#' Build SQL WHERE Clause from App Filters
#'
#' Constructs a parameter-safe SQL WHERE clause using the current filter 
#' values from the Shiny app UI. Filters include date range, billing 
#' country, genre, and artist, and can be partially or fully applied. 
#' This function uses `glue_sql()` to safely escape and interpolate 
#' values, preventing SQL injection and ensuring DuckDB compatibility.
#'
#' @param date_range A Date vector of length 2 specifying the start and 
#'   end of the selected time window.
#' @param country Optional character vector of billing countries selected 
#'   in the UI.
#' @param genre Optional character vector of genres.
#' @param artist Optional character vector of artist names.
#' @param .con A valid DBI connection object (e.g., DuckDB connection).
#'
#' @return A `DBI::SQL` object with a complete WHERE clause, or an empty 
#'   string if no filters are applied.
#' @keywords internal
form_where_clause <- function(
    date_range = NULL, 
    country    = NULL, 
    genre      = NULL, 
    artist     = NULL,
    .con       = NULL
) {
  stopifnot(!is.null(.con), DBI::dbIsValid(.con))
  
  if (exists("enable_logging", inherits = TRUE) && enable_logging) {
    message("[SQL] Running form_where_clause() with active filters")
  }
  
  clauses <- list()
  
  # Date filter: round to month boundaries
  if (!is.null(date_range)) {
    start_date <- lubridate::floor_date(as.Date(date_range[1]), "month")
    end_date   <- lubridate::ceiling_date(as.Date(date_range[2]), "month") - 
      lubridate::days(1)
    
    clauses$date <- glue::glue_sql(
      "DATE(i.InvoiceDate) BETWEEN DATE({start_date}) AND DATE({end_date})",
      .con = .con
    )
  }
  
  # Country filter
  if (length(country) > 0) {
    clauses$country <- glue::glue_sql(
      "i.BillingCountry IN ({country*})", .con = .con
    )
  }
  
  # Genre filter
  if (length(genre) > 0) {
    clauses$genre <- glue::glue_sql(
      "g.Name IN ({genre*})", .con = .con
    )
  }
  
  # Artist filter
  if (length(artist) > 0) {
    clauses$artist <- glue::glue_sql(
      "ar.Name IN ({artist*})", .con = .con
    )
  }
  
  # Combine all clauses with AND
  if (length(clauses) > 0) {
    where_clause <- DBI::SQL(
      paste("WHERE", paste(clauses, collapse = " AND "))
    )
  } else {
    where_clause <- DBI::SQL("")
  }
  
  return(where_clause)
}

#' Join Invoice Table with Optional Month-Bound Date Filtering
#'
#' Constructs a parameterized SQL clause `JOIN` clause joining the `e` 
#' table to the `Invoice` table, with an optional date filter on
#' `InvoiceDate`. The date range is sanitized, sorted, and rounded to month
#' boundaries for consistency. This is typically used within KPI SQL queries
#' that operate on filtered invoice IDs.
#'
#' @param date_range A length-2 Date vector specifying the start and end 
#'   of the selected time window.
#' @param con A valid DBI database connection (e.g., DuckDB).
#'
#' @return A `DBI::SQL` object representing the JOIN clause with or without 
#'   the date range condition.
#' @keywords internal
apply_date_filter <- function(date_range = NULL, .con = NULL) {
  stopifnot(!is.null(.con), DBI::dbIsValid(.con))
  
  if (!is.null(date_range)) {
    # Sort and round to month boundaries
    start_date <- lubridate::floor_date(min(as.Date(date_range)), "month")
    end_date   <- lubridate::ceiling_date(max(as.Date(date_range)), "month") - 
      lubridate::days(1)
    
    date_clause <- glue::glue_sql(
      "DATE(i.InvoiceDate) BETWEEN DATE({start_date}) AND DATE({end_date})",
      .con = con
    )
    
    return(glue::glue_sql(
      "JOIN Invoice i ON i.InvoiceId = e.InvoiceId AND {date_clause}",
      .con = con
    ))
  }
  
  return(glue::glue_sql(
    "JOIN Invoice i ON i.InvoiceId = e.InvoiceId",
    .con = con
  ))
}
