#' @file metadata_utils.R
#' @title Metadata Helpers for Filters, Catalog, and Static KPI Display
#'
#' @description
#' Utility functions for retrieving and summarizing metadata from the
#' Chinook DuckDB dataset. These functions support UI filter options,
#' catalog construction, and sidebar summaries. 
#' Includes:
#' \itemize{
#'   \item{\code{\link{get_filter_meta()}}: Global filter values and date 
#'     range.}
#'   \item{\code{\link{make_static_summary_table()}}: Formats KPIs into a 
#'     summary datatable.}
#'   \item{\code{\link{create_catalog_tables()}}: Constructs artist/genre 
#'     temp catalog tables.}
#'   \item{\code{\link{check_catalog_tables()}}: Validates performance of 
#'     `create_catalog_tables()`.}
#' }
#' 
#' These helpers are optimized for large datasets and support scalable,
#' template-driven Shiny dashboards.
#'
#' @keywords internal

#' Retrieve Filter Values for Global UI Selectors
#'
#' Pulls metadata from the database to populate selectize filters and
#' sets date range limits. Includes genre, artist, country, and invoice dates.
#'
#' @param con A valid DBI connection object.
#'
#' @return A named list with keys: `date_range`, `genre_choices`,
#'   `artist_choices`, and `country_choices`.
#' @keywords internal
get_filter_meta <- function(con) {
  stopifnot(!is.null(con), DBI::dbIsValid(con))
  
  log_msg("[DATA META] Fetching filter metadata from DuckDB.")
  
  list(
    date_range = DBI::dbGetQuery(
      con,
      "SELECT MIN(InvoiceDate) AS min_date,
              MAX(InvoiceDate) AS max_date
       FROM Invoice"
    ),
    genre_choices = DBI::dbGetQuery(
      con, "SELECT DISTINCT Name FROM Genre"
    )$Name |> sort(),
    artist_choices = DBI::dbGetQuery(
      con, "SELECT DISTINCT Name FROM Artist"
    )$Name |> sort(),
    country_choices = DBI::dbGetQuery(
      con, "SELECT DISTINCT BillingCountry FROM Invoice"
    )$BillingCountry |> sort()
  )
}

#' Format Sidebar Summary Table from KPI and Filter Metadata
#'
#' Combines `get_shared_kpis()` and `get_filter_meta()` to render a
#' static datatable with high-level metrics for display in sidebar UI.
#' Handles null fallbacks and formats values with helper functions.
#'
#' @param kpis Output from `get_shared_kpis()` containing shared KPIs.
#' @param filter_meta Output from `get_filter_meta()`.
#'
#' @return A compact `DT::datatable()` widget with KPI values.
#' @keywords internal
make_static_summary_table <- function(kpis, filter_meta) {
  log_msg("[DATA META] Generating static summary DT table.")
  
  if (is.null(kpis)) {
    return(DT::datatable(
      data.frame(
        Metric = "Summary",
        Value = "No data available"
      ),
      options = list(dom = "t", paging = FALSE),
      rownames = FALSE,
      class = "compact stripe hover"
    ))
  }
  
  min_date <- as.Date(filter_meta$date_range$min_date)
  max_date <- as.Date(filter_meta$date_range$max_date)
  
  range_str <- paste(
    format(min_date, "%b %Y"), "–", format(max_date, "%b %Y")
  )
  
  summary_df <- data.frame(
    Metric = c(
      "Date Range", "Number of Purchases", "Number of Customers",
      "Tracks Sold", "Revenue (USD$)", "Number of Genres",
      "Number of Artists", "Number of Countries"
    ),
    Value = c(
      range_str,
      format_kpi_value(
        kpis$purchase_kpis$total_orders, type = "number"
      ),
      format_kpi_value(
        kpis$customer_kpis$total_customers, type = "number"
      ),
      format_kpi_value(
        kpis$purchase_kpis$total_tracks, type = "number"
      ),
      format_kpi_value(
        kpis$revenue_kpis$total_revenue, type = "dollar"
      ),
      format_kpi_value(
        length(filter_meta$genre_choices), type = "number"
      ),
      format_kpi_value(
        length(filter_meta$artist_choices), type = "number"
      ),
      format_kpi_value(
        length(filter_meta$country_choices), type = "number"
      )
    ),
    stringsAsFactors = FALSE
  )
  
  DT::datatable(
    summary_df,
    options = list(dom = "t", paging = FALSE),
    rownames = FALSE,
    class = "compact stripe hover"
  )
}

#' Create Temp Catalog Tables for Artist and Genre
#'
#' Constructs `genre_catalog` and `artist_catalog` tables containing
#' total number of distinct tracks per group. These are used for
#' catalog coverage KPIs (e.g. percent of genre tracks sold).
#'
#' @param con A valid DuckDB connection.
#'
#' @return Invisible NULL. Catalog tables created with `CREATE TEMP TABLE`.
#' @keywords internal
create_catalog_tables <- function(con) {
  stopifnot(!is.null(con), DBI::dbIsValid(con))
  
  log_msg("[DATA META] Creating catalog tables in DuckDB.")
  
  # Drop temp tables if they already exist
  DBI::dbExecute(con, "DROP TABLE IF EXISTS genre_catalog")
  DBI::dbExecute(con, "DROP TABLE IF EXISTS artist_catalog")
  
  DBI::dbExecute(con, "
    CREATE TEMP TABLE genre_catalog AS
    SELECT g.Name AS genre, COUNT(DISTINCT t.TrackId) AS num_tracks
    FROM Track t
    JOIN Genre g ON t.GenreId = g.GenreId
    GROUP BY genre
  ")
  
  DBI::dbExecute(con, "
    CREATE TEMP TABLE artist_catalog AS
    SELECT ar.Name AS artist, COUNT(DISTINCT t.TrackId) AS num_tracks
    FROM Track t
    JOIN Album al ON t.AlbumId = al.AlbumId
    JOIN Artist ar ON al.ArtistId = ar.ArtistId
    GROUP BY artist
  ")
  
  invisible(NULL)
}

#' Validate Presence of Temp Catalog Tables
#'
#' Checks whether the expected catalog tables (`genre_catalog` and
#' `artist_catalog`) exist in the DuckDB connection. Designed for
#' post-setup verification after calling `create_catalog_tables()`.
#'
#' @param con A valid DuckDB connection object.
#'
#' @return Logical TRUE if both tables exist, otherwise FALSE.
#' @keywords internal
check_catalog_tables <- function(con) {
  stopifnot(!is.null(con), DBI::dbIsValid(con))
  
  tables <- DBI::dbListTables(con)
  expected <- c("genre_catalog", "artist_catalog")
  missing <- setdiff(expected, tables)
  
  log_msg(
    cond = length(missing) > 0,
    msg = paste0(
      "[DATA META] ❌ Missing catalog tables: ", 
      paste(missing, collapse = ", ")
    )
  )
  log_msg(
    cond = length(missing) == 0,
    msg = "[DATA META] ✅ Catalog tables present.")
}
