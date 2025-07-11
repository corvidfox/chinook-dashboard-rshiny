#' @file subset_kpis.R
#' @title KPI Extraction Helpers for Filtered Invoice Subsets
#'
#' @description
#' Provides modular functions for computing key performance indicators
#' based on filtered invoice events. Includes revenue, customer behavior,
#' and purchase metrics. These helpers support reactive pipelines and
#' scale effectively for large DuckDB datasets.
#'
#' @keywords internal

#' Retrieve Filtered Invoice Events for a Subset of Customers
#'
#' Executes a SQL query to extract customer-level invoice events from
#' the Chinook database, with optional filters for genre, artist, and
#' country. The function returns a distinct set of customer–invoice–
#' date rows, sorted and de-duplicated to serve as a base table for
#' downstream module queries.
#'
#' @param con A valid DBI connection to a DuckDB instance.
#' @param where_clause A `DBI::SQL` object created by 
#'   `form_where_clause()`, containing zero or more WHERE filters.
#'
#' @return A data frame with columns: `CustomerId`, `dt` (date), and 
#'   `InvoiceId`. If no matches, returns an empty data frame.
#'
#' @examples
#' clause <- form_where_clause(country = "USA", .con = con)
#' df <- get_events_shared(con, clause)
#'
#' @seealso [form_where_clause()]
#' @keywords internal
get_events_shared <- function(con, where_clause) {
  stopifnot(DBI::dbIsValid(con))
  
  if (!inherits(where_clause, "SQL")) {
    stop("`where_clause` must be a DBI::SQL object.")
  }
  
  if (exists("enable_logging", inherits = TRUE) && enable_logging) {
    message("[SQL] Running get_events_shared() with active filters")
  }
  
  query <- glue::glue_sql("
    SELECT
      i.CustomerId,
      DATE(i.InvoiceDate) AS dt,
      i.InvoiceId
    FROM Invoice i
    JOIN InvoiceLine il ON i.InvoiceId = il.InvoiceId
    JOIN Track t ON il.TrackId = t.TrackId
    JOIN Album al ON t.AlbumId = al.AlbumId
    JOIN Artist ar ON al.ArtistId = ar.ArtistId
    JOIN Genre g ON t.GenreId = g.GenreId
    {where_clause}
  ", .con = con)
  
  DBI::dbGetQuery(con, query) |>
    dplyr::distinct(CustomerId, InvoiceId, dt, .keep_all = TRUE) |>
    dplyr::arrange(CustomerId, dt)
}

#' Compute Total Revenue KPI
#'
#' Retrieves the total revenue by summing unit prices and quantities
#' from the `InvoiceLine` table, joined through a filtered Invoice set.
#'
#' @param con A valid DBI connection object.
#' @param tbl A character string of the temporary events table name.
#' @param date_range Optional Date vector (length 2) to constrain the KPI 
#'   to a specific range.
#'
#' @return A named list with `total_revenue`, or `NULL` if no results.
#' @keywords internal
get_revenue_kpis <- function(con, tbl, date_range = NULL) {
  stopifnot(DBI::dbIsValid(con))
  
  join_clause <- apply_date_filter(date_range, con)
  
  query <- glue::glue_sql("
    SELECT SUM(il.UnitPrice * il.Quantity) AS total_revenue
    FROM {`tbl`} e
    {join_clause}
    JOIN InvoiceLine il ON il.InvoiceId = e.InvoiceId
  ", .con = con)
  
  result <- DBI::dbGetQuery(con, query)
  
  if (nrow(result) == 0 || is.na(result$total_revenue)) return(NULL)
  
  list(total_revenue = result$total_revenue)
}

#' Compute Customer KPIs (Total and First-Time)
#'
#' Returns the number of distinct customers and the share of new 
#' customers within the date range based on first purchase date.
#'
#' @param con A DBI connection.
#' @param tbl A character string of the filtered invoice events table.
#' @param date_range Optional Date vector (length 2). Defaults to full 
#'   global range if `NULL`.
#'
#' @return A named list of KPIs, or `NULL` if query returns no rows.
#' @keywords internal
get_customer_kpis <- function(con, tbl, date_range = NULL) {
  stopifnot(DBI::dbIsValid(con))
  
  if (is.null(date_range)) {
    if (
      exists("min_date", inherits = TRUE) && 
      exists("min_date", inherits = TRUE)
    ) {
      date_range <- c(min_date, max_date)
    }
  }
  
  query <- glue::glue_sql("
    WITH first_dates AS (
      SELECT e.CustomerId, MIN(DATE(i.InvoiceDate)) AS first_purchase
      FROM {`tbl`} e
      JOIN Invoice i ON i.InvoiceId = e.InvoiceId
      GROUP BY e.CustomerId
    ),
    current_customers AS (
      SELECT DISTINCT e.CustomerId, DATE(i.InvoiceDate) AS dt
      FROM {`tbl`} e
      JOIN Invoice i ON i.InvoiceId = e.InvoiceId
    )
    SELECT
      COUNT(DISTINCT current_customers.CustomerId) AS total_customers,
      COUNT(DISTINCT CASE
        WHEN first_dates.first_purchase BETWEEN 
          DATE({date_range[1]}) AND DATE({date_range[2]})
        THEN current_customers.CustomerId
      END) AS new_customers
    FROM current_customers
    JOIN first_dates 
      ON first_dates.CustomerId = current_customers.CustomerId
  ", .con = con)
  
  result <- DBI::dbGetQuery(con, query)
  
  if (nrow(result) == 0 || is.na(result$total_customers)) return(NULL)
  
  list(
    total_customers = result$total_customers,
    pct_new_customers = ifelse(
      result$total_customers == 0, 0,
      result$new_customers / result$total_customers
    )
  )
}

#' Compute Purchase Metrics from Invoices
#'
#' Computes aggregated metrics at the invoice level: total orders, 
#' average revenue per order, total tracks sold, and tracks per order.
#'
#' @param con DBI connection.
#' @param tbl Temporary table of filtered invoices (`filtered_invoices`).
#' @param date_range Optional filter window as Date vector (length 2).
#'
#' @return Named list of KPIs, or `NULL` if query returns no rows.
#' @keywords internal
get_purchase_kpis <- function(con, tbl, date_range = NULL) {
  stopifnot(DBI::dbIsValid(con))
  
  join_clause <- apply_date_filter(date_range, con)
  
  query <- glue::glue_sql("
    WITH invoice_summary AS (
      SELECT
        e.InvoiceId,
        SUM(il.Quantity) AS tracks_per_order,
        SUM(il.UnitPrice * il.Quantity) AS revenue
      FROM {`tbl`} e
      {join_clause}
      JOIN InvoiceLine il ON il.InvoiceId = e.InvoiceId
      GROUP BY e.InvoiceId
    )
    SELECT
      COUNT(*) AS total_orders,
      SUM(revenue) / COUNT(*) AS avg_rev_per_order,
      SUM(tracks_per_order) AS total_tracks,
      AVG(tracks_per_order) AS avg_tracks_per_order
    FROM invoice_summary
  ", .con = con)
  
  result <- DBI::dbGetQuery(con, query)
  
  if (nrow(result) == 0 || is.na(result$total_orders)) return(NULL)
  
  list(
    total_orders = result$total_orders,
    avg_rev_per_order = result$avg_rev_per_order,
    total_tracks = result$total_tracks,
    avg_tracks_per_order = result$avg_tracks_per_order
  )
}
