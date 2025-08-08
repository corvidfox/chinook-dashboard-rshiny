#' @file sql_kpi_catalog_utils.R
#' @title Catalog KPI Utilities for Enrichment and Coverage Metrics
#'
#' @description
#' Contains SQL helpers that calculate catalog-level engagement metrics
#' (e.g. percent of catalog sold) for genre and artist dimensions. These
#' functions enrich top-N KPI tables with sales coverage, and summarize
#' overall diversity in the filtered subset (number of genres, artists,
#' countries). Built for DuckDB and used downstream in visual modules.
#'
#' @keywords internal

#' Query percent of catalog sold for Genre or Artist
#'
#' @param con DBI connection to DuckDB
#' @param tbl Filtered invoice table name
#' @param group_var Either "Genre" or "Artist"
#' @param date_range Character vector of length 2.
#'
#' @return A data.frame with group_val, catalog_size, and pct_catalog_sold
query_catalog_sales <- function(con, tbl, group_var = "Genre", date_range) {
  stopifnot(!is.null(con), DBI::dbIsValid(con))
  
  group_var <- rlang::arg_match(group_var, c("Genre", "Artist"))
  
  date_clause <- apply_date_filter(date_range = date_range, .con = con)
  
  join_clause <- switch(
    group_var,
    Genre = "
      JOIN Track t ON il.TrackId = t.TrackId
      JOIN Genre g ON g.GenreId = t.GenreId
      LEFT JOIN genre_catalog gc ON gc.genre = g.Name
    ",
    Artist = "
      JOIN Track t ON il.TrackId = t.TrackId
      JOIN Album al ON al.AlbumId = t.AlbumId
      JOIN Artist ar ON ar.ArtistId = al.ArtistId
      LEFT JOIN artist_catalog ac ON ac.artist = ar.Name
    "
  )
  
  group_field <- switch(group_var,
                        Genre = "g.Name",
                        Artist = "ar.Name"
  )
  
  catalog_field <- switch(group_var,
                          Genre = "gc.num_tracks",
                          Artist = "ac.num_tracks"
  )
  
  query <- glue::glue_sql("
    SELECT {DBI::SQL(group_field)} AS group_val,
           COUNT(DISTINCT il.TrackId) AS unique_tracks_sold,
           MAX({DBI::SQL(catalog_field)}) AS catalog_size,
           COUNT(DISTINCT il.TrackId) * 1.0 /
             MAX({DBI::SQL(catalog_field)}) AS pct_catalog_sold
    FROM {`tbl`} e
    {date_clause}
    JOIN InvoiceLine il ON il.InvoiceId = e.InvoiceId
    {DBI::SQL(join_clause)}
    GROUP BY group_val
  ", .con = con)
  
  DBI::dbGetQuery(con, query)
}

#' Join catalog sales KPIs onto TopN summary table
#'
#' @param con DBI connection
#' @param tbl filtered invoice temp table
#' @param topn_df TopN augmented data.frame (genre or artist)
#' @param group_var either 'Genre' or 'Artist'
#' @param date_range Character vector of length 2.
enrich_catalog_kpis <- function(con, tbl, topn_df, group_var, date_range) {
  catalog_sales <- query_catalog_sales(con, tbl, group_var, date_range)
  top_groups <- topn_df$group_val
  catalog_sales_subset <- dplyr::filter(catalog_sales, group_val %in% top_groups)
  dplyr::left_join(topn_df, catalog_sales_subset, by = "group_val")
}