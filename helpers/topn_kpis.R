#' @file topn_kpis.R
#' @title Top-N KPI Aggregation and Enrichment Functions
#'
#' @description
#' Provides SQL wrappers and transformation logic for calculating ranked
#' top-N KPIs by groupings (country, genre, artist) and metrics (revenue,
#' customers, purchases, etc.). Outputs enriched tables with derived fields
#' and optional catalog coverage KPIs. Used for dashboard summaries, card
#' components, and plot data across modules.
#'
#' @keywords internal

#' Return Top N Entities for KPI Metric
#'
#' Produces a ranked top-N set of countries, genres, or artists based 
#' on the selected KPI: revenue, customers, first-time customers, etc.
#'
#' @param con A DBI connection to the DuckDB database.
#' @param tbl Filtered event table (e.g., `filtered_invoices`).
#' @param metric_var One of `"revenue"`, `"num_customers"`, 
#'   `"first_time_customers"`, `"num_purchases"`, `"tracks_sold"`.
#' @param group_var One of `"BillingCountry"`, `"Genre"`, or `"Artist"`.
#' @param n Integer: number of top entities to return.
#' @param date_range Optional Date vector of length 2 to constrain the time window.
#'
#' @return A data frame with `group_val` and `value`, ordered descending.
#'   May return an empty data frame if no data matched the filters.
#' @keywords internal
get_topn_kpis <- function(
    con,
    tbl,
    metric_var = "revenue",
    group_var = "BillingCountry",
    n = 5,
    date_range = NULL
) {
  stopifnot(DBI::dbIsValid(con))
  if (n <= 0) stop("`n` must be greater than 0.")
  
  # Match inputs against allowed options
  metric_var <- rlang::arg_match(
    metric_var,
    c("revenue", "num_customers", "first_time_customers",
      "num_purchases", "tracks_sold")
  )
  
  group_var <- rlang::arg_match(
    group_var,
    c("BillingCountry", "Genre", "Artist")
  )
  
  # Ensure date_range is available when needed
  if (is.null(date_range)) {
    date_range <- c(min_date, max_date)
  }
  
  # Safely interpolate filtering join to Invoice + date
  join_clause <- apply_date_filter(date_range, con)
  
  # Map group_var to SQL field and extra join logic
  group_expr <- switch(
    group_var,
    BillingCountry = "i.BillingCountry",
    Genre = "g.Name",
    Artist = "ar.Name"
  )
  
  join_extra <- switch(
    group_var,
    BillingCountry = "",
    Genre = paste(
      "JOIN Track t ON t.TrackId = il.TrackId",
      "JOIN Genre g ON g.GenreId = t.GenreId"
    ),
    Artist = paste(
      "JOIN Track t ON t.TrackId = il.TrackId",
      "JOIN Album al ON al.AlbumId = t.AlbumId",
      "JOIN Artist ar ON ar.ArtistId = al.ArtistId"
    )
  )
  
  # Special case: First-time customers need additional logic
  if (metric_var == "first_time_customers") {
    start_date <- as.Date(min(date_range))
    end_date   <- as.Date(max(date_range))
    
    query <- glue::glue_sql("
      WITH first_dates AS (
        SELECT CustomerId, MIN(DATE(InvoiceDate)) AS first_purchase
        FROM Invoice
        GROUP BY CustomerId
      ),
      subset_invoices AS (
        SELECT e.InvoiceId, e.CustomerId
        FROM {`tbl`} e
        JOIN Invoice i ON i.InvoiceId = e.InvoiceId
        WHERE DATE(i.InvoiceDate) BETWEEN DATE({start_date})
                                     AND DATE({end_date})
      ),
      invoice_lines AS (
        SELECT si.CustomerId, {DBI::SQL(group_expr)} AS group_val
        FROM subset_invoices si
        JOIN Invoice i ON i.InvoiceId = si.InvoiceId
        JOIN InvoiceLine il ON il.InvoiceId = si.InvoiceId
        {DBI::SQL(join_extra)}
        JOIN first_dates fd ON fd.CustomerId = si.CustomerId
        WHERE fd.first_purchase BETWEEN DATE({start_date})
                                   AND DATE({end_date})
        GROUP BY si.CustomerId, group_val
      )
      SELECT group_val,
             COUNT(DISTINCT CustomerId) AS value
      FROM invoice_lines
      GROUP BY group_val
      ORDER BY value DESC
      LIMIT {n}
    ", .con = con)
    
  } else {
    # Define metric expression based on selected KPI
    metric_expr <- switch(
      metric_var,
      revenue = "SUM(il.UnitPrice * il.Quantity)",
      num_purchases = "COUNT(DISTINCT e.InvoiceId)",
      tracks_sold = "SUM(il.Quantity)",
      num_customers = "COUNT(DISTINCT e.CustomerId)"
    )
    
    query <- glue::glue_sql("
      SELECT {DBI::SQL(group_expr)} AS group_val,
             {DBI::SQL(metric_expr)} AS value
      FROM {`tbl`} e
      {join_clause}
      JOIN InvoiceLine il ON il.InvoiceId = e.InvoiceId
      {DBI::SQL(join_extra)}
      GROUP BY group_val
      ORDER BY value DESC
      LIMIT {n}
    ", .con = con)
  }
  
  result <- DBI::dbGetQuery(con, query)
  return(result)
}

#' Create a wide-format KPI summary from top-N list
#'
#' Joins all metrics, calculates derived KPIs, and sorts by target metric.
#'
#' @param topn_group Named list of metrics for one grouping (e.g., Genre)
#' @param metric Target metric for sorting
#' @param total_revenue Optional total revenue value
#'
#' @return A data.frame with wide-format KPIs and derived columns
build_topn_table <- function(topn_group, metric, total_revenue = NULL) {
  
  if (exists("enable_logging", inherits = TRUE) && enable_logging) {
    missing_metrics <- setdiff(
      names(topn_group)[names(topn_group) != "group"],
      names(metric_tables)
    )
    if (length(missing_metrics) > 0) {
      message("[TOPN] Missing metrics for: ", paste(missing_metrics, collapse = ", "))
    }
  }
  
  metric_tables <- topn_group[names(topn_group) != "group"]
  
  # Remove NULL entries before renaming
  metric_tables <- purrr::compact(metric_tables)
  
  renamed <- purrr::imap(metric_tables, function(tbl, name) {
    dplyr::rename(tbl, !!name := value)
  })
  
  if (length(renamed) == 0) return(tibble::tibble(group_val = character()))
  
  df <- purrr::reduce(renamed, dplyr::full_join, by = "group_val")
  
  df <- dplyr::mutate(df,
                      revenue_share           = revenue / total_revenue,
                      revenue_per_cust        = revenue / num_customers,
                      revenue_per_purchase    = revenue / num_purchases,
                      revenue_per_track       = revenue / tracks_sold,
                      avg_tracks_per_purchase = tracks_sold / num_purchases,
                      pct_first_time_cust     = first_time_customers / num_customers
  )
  
  df_sorted <- dplyr::arrange(df, dplyr::desc(.data[[metric]]))
  return(df_sorted)
}

#' Apply KPI formatting to enriched TopN summary table
#'
#' @param df A TopN KPI data.frame containing raw values.
#'
#' @return A data.frame with additional `*_fmt` columns formatted for display.
#' @keywords internal
format_topn_table_values <- function(df) {
  excluded <- c("group_val")
  metric_cols <- setdiff(names(df), excluded)
  
  formatted <- purrr::map_dfc(metric_cols, function(col) {
    type <- dplyr::case_when(
      grepl("percent|pct", col) ~ "percent",
      grepl("share", col)       ~ "percent",
      grepl("revenue", col)     ~ "dollar",
      grepl("tracks|purchases|customers|orders|catalog", col) ~ "number",
      TRUE ~ "float"
    )
    
    val <- df[[col]]
    fmt <- format_kpi_value(val, type = type)
    
    # Return a single column with "_fmt" suffix
    tibble::tibble(!!paste0(col, "_fmt") := fmt)
  })
  
  dplyr::bind_cols(df, formatted)
}


#' Aggregate All Shared KPI Metrics for Filtered Subset
#'
#' Computes revenue, customer, purchase, and Top-N KPIs across selected
#' filters and ranking dimensions. Returns both raw and enriched formats
#' for module use. Catalog coverage is joined for Genre and Artist.
#'
#' @param con DBI connection to DuckDB.
#' @param tbl Character name of temp table (filtered invoices).
#' @param date_range Date vector of length 2 (optional).
#' @param top_n Integer for number of top values per group.
#'
#' @return Named list of KPI components. Returns NULL if any base pull fails.
#' @keywords internal
get_shared_kpis <- function(con, tbl, date_range = NULL, top_n = 5) {
  stopifnot(DBI::dbIsValid(con), is.character(tbl), length(tbl) == 1)
  
  if (exists("enable_logging", inherits = TRUE) && enable_logging) {
    message("[KPI] ▶️ Starting shared KPI aggregation")
  }
  
  revenue_kpis <- get_revenue_kpis(con, tbl, date_range)
  if (is.null(revenue_kpis)) return(NULL)
  if (exists("enable_logging", inherits = TRUE) && enable_logging) {
    message(glue::glue("   ✔ Retrieved Revenue KPIs"))
  }
  
  customer_kpis <- get_customer_kpis(con, tbl, date_range)
  if (is.null(customer_kpis)) return(NULL)
  if (exists("enable_logging", inherits = TRUE) && enable_logging) {
    message(glue::glue("   ✔ Retrieved Customer KPIs"))
  }
  
  purchase_kpis <- get_purchase_kpis(con, tbl, date_range)
  if (is.null(purchase_kpis)) return(NULL)
  if (exists("enable_logging", inherits = TRUE) && enable_logging) {
    message(glue::glue("   ✔ Retrieved Purchase KPIs"))
  }
  
  metrics <- c(
    "revenue", "num_customers", "first_time_customers",
    "num_purchases", "tracks_sold"
  )
  groupings <- c("BillingCountry", "Genre", "Artist")
  
  # Top-N KPIS: Step 1: Fetch Raw Top-N Tables
  topn_raw <- lapply(groupings, function(group) {
    metric_tables <- lapply(metrics, function(metric) {
      result <- get_topn_kpis(
        con, tbl,
        metric_var = metric,
        group_var = group,
        n = top_n,
        date_range = date_range
      )
      if (exists("enable_logging", inherits = TRUE) && enable_logging) {
        message(glue::glue("   ✔ Retrieved {metric} for {group}"))
      }
      return(result)
    })
    names(metric_tables) <- metrics
    metric_tables$group <- group
    return(metric_tables)
  })
  names(topn_raw) <- paste0("topn_", tolower(groupings))
  
  # Top-N KPIS: Step 2: Build enriched tables sorted by metric 
  topn_by_metric <- purrr::map(topn_raw, function(group_tbl) {
    group_name <- group_tbl$group
    purrr::map(metrics, function(metric) {
      df <- build_topn_table(
        topn_group = group_tbl,
        metric = metric,
        total_revenue = revenue_kpis$total_revenue
      )
      # Filter out NA values in the sorting metric
      df <- df |> dplyr::filter(!is.na(.data[[metric]]))
      
      if (group_name %in% c("Genre", "Artist")) {
        df <- enrich_catalog_kpis(con, tbl, df, group_name)
      }
      df <- format_topn_table_values(df)
      return(df)
    }) |> rlang::set_names(metrics)
  })
  if (exists("enable_logging", inherits = TRUE) && enable_logging) {
    message(glue::glue("   ✔ Processed Full Top-N KPIs"))
  }
  
  # Top-N KPIS: Step 3: Metadata counts
  metadata_kpis <- get_subset_metadata_kpis(con, tbl)
  if (exists("enable_logging", inherits = TRUE) && enable_logging) {
    message(glue::glue("   ✔ Retrieved Metadata KPIs"))
  }
  
  # Top-N KPIS: Return all blocks
  return(list(
    revenue_kpis    = revenue_kpis,
    customer_kpis   = customer_kpis,
    purchase_kpis   = purchase_kpis,
    topn            = topn_by_metric,
    metadata_kpis   = metadata_kpis
  ))
}
