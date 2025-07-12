#' @file sql_kpi_utils.R
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
  
  log_msg("[SQL] Running get_events_shared() with active filters")
  
  query <- glue::glue_sql(
    "
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
  ",
    .con = con
  )
  
  DBI::dbGetQuery(con, query) |>
    dplyr::distinct(CustomerId, InvoiceId, dt, .keep_all = TRUE) |>
    dplyr::arrange(CustomerId, dt)
}

#' Count unique genres, artists, and countries in subset
#'
#' @param con DBI connection
#' @param tbl Name of filtered invoice temp table
#'
#' @return Named list of counts for genres, artists, countries
get_subset_metadata_kpis <- function(con, tbl) {
  stopifnot(!is.null(con), DBI::dbIsValid(con))
  
  query <- glue::glue_sql(
    "
    SELECT
      COUNT(DISTINCT g.Name) AS num_genre,
      COUNT(DISTINCT ar.Name) AS num_artist,
      COUNT(DISTINCT i.BillingCountry) AS num_billingcountry
    FROM {`tbl`} e
    JOIN Invoice i ON i.InvoiceId = e.InvoiceId
    JOIN InvoiceLine il ON il.InvoiceId = e.InvoiceId
    JOIN Track t ON il.TrackId = t.TrackId
    JOIN Genre g ON t.GenreId = g.GenreId
    JOIN Album al ON t.AlbumId = al.AlbumId
    JOIN Artist ar ON al.ArtistId = ar.ArtistId
  ",
    .con = con
  )
  
  DBI::dbGetQuery(con, query) |> as.list()
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
  
  query <- glue::glue_sql(
    "
    SELECT SUM(il.UnitPrice * il.Quantity) AS total_revenue
    FROM {`tbl`} e
    {join_clause}
    JOIN InvoiceLine il ON il.InvoiceId = e.InvoiceId
  ",
    .con = con
  )
  
  result <- DBI::dbGetQuery(con, query)
  
  if (nrow(result) == 0 || is.na(result$total_revenue))
    return(NULL)
  
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
    if (exists("min_date", inherits = TRUE) &&
        exists("min_date", inherits = TRUE)) {
      date_range <- c(min_date, max_date)
    }
  }
  
  query <- glue::glue_sql(
    "
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
  ",
    .con = con
  )
  
  result <- DBI::dbGetQuery(con, query)
  
  if (nrow(result) == 0 ||
      is.na(result$total_customers))
    return(NULL)
  
  list(
    total_customers = result$total_customers,
    pct_new_customers = ifelse(
      result$total_customers == 0,
      0,
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
  
  query <- glue::glue_sql(
    "
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
  ",
    .con = con
  )
  
  result <- DBI::dbGetQuery(con, query)
  
  if (nrow(result) == 0 || is.na(result$total_orders))
    return(NULL)
  
  list(
    total_orders = result$total_orders,
    avg_rev_per_order = result$avg_rev_per_order,
    total_tracks = result$total_tracks,
    avg_tracks_per_order = result$avg_tracks_per_order
  )
}

#' @title Get Group-Level KPI Summary
#' @description
#' Computes full-period KPIs for grouped entities (Genre, Artist, Country)
#' using the specified invoice table. Aggregates are pushed to DuckDB for
#' performance. Catalog size is joined for Genre and Artist.
#'
#' @param con DBI connection to DuckDB.
#' @param tbl Invoice table name (e.g., "filtered_invoices").
#' @param group_var One of "Genre", "Artist", or "BillingCountry".
#' @param date_range Optional character vector (YYYY-MM-DD x 2).
#'
#' @return A data.frame with one row per group and KPI metrics.
#' @export
get_group_kpis_full <- function(con, tbl, group_var, date_range = NULL) {
  stopifnot(DBI::dbIsValid(con))
  group_var <- rlang::arg_match(
    group_var, c("Genre", "Artist", "BillingCountry")
    )
  
  log_msg(glue::glue(
      "[SQL] get_group_kpis_full(): querying full KPIs by {group_var}"
    ))
  
  # Define joins and expressions for group_var
  group_fields <- switch(
    group_var,
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
    ),
    BillingCountry = list(
      join_clause = "",
      group_expr = "i.BillingCountry",
      catalog_expr = "NULL"
    )
  )
  
  # Construct parameterized date filter and invoice join
  invoice_join <- apply_date_filter(date_range = date_range, .con = con)
  
  query <- glue::glue_sql(
    "
    WITH base AS (
      SELECT
        e.CustomerId,
        i.InvoiceDate AS invoice_date,
        i.InvoiceId,
        il.TrackId,
        il.Quantity,
        il.UnitPrice,
        {DBI::SQL(group_fields$group_expr)} AS group_val,
      FROM {`tbl`} e
      {invoice_join}
      JOIN InvoiceLine il ON i.InvoiceId = il.InvoiceId
      {DBI::SQL(group_fields$join_clause)}
    ),
    first_purchases AS (
      SELECT CustomerId, MIN(DATE(invoice_date)) AS first_purchase
      FROM base
      GROUP BY CustomerId
    )
    SELECT
      b.group_val,
      COUNT(DISTINCT b.CustomerId) AS num_customers,
      COUNT(DISTINCT b.InvoiceId) AS num_purchases,
      SUM(b.Quantity) AS tracks_sold,
      SUM(b.UnitPrice * b.Quantity) AS revenue,
      COUNT(DISTINCT CASE
        WHEN b.invoice_date = fp.first_purchase THEN b.CustomerId
        ELSE NULL
      END) AS first_time_customers
    FROM base b
    LEFT JOIN first_purchases fp ON b.CustomerId = fp.CustomerId
    GROUP BY b.group_val
    ORDER BY b.group_val
  ",
    .con = con
  )
  
  DBI::dbGetQuery(con, query)
}

#' @title Get Cohort Retention Heatmap Data
#' @description
#' Returns cohort retention percentages by month offset, using
#' filtered subset activity and full-data cohort definitions.
#'
#' @param con DBI connection to DuckDB
#' @param tbl Invoice table name (e.g., "filtered_invoices")
#' @param date_range Character vector of length 2 (YYYY-MM-DD)
#' @param max_offset Optional integer for max month offset
#'
#' @return A data.frame with cohort retention rows
#' @export
get_retention_cohort_data <- function(con,
                                      tbl,
                                      date_range = NULL,
                                      max_offset = NULL) {
  stopifnot(DBI::dbIsValid(con), is.character(tbl), length(tbl) == 1)
  
  if (!is.character(date_range) || length(date_range) != 2) {
    stop("`date_range` must be a character vector of length 2.")
  }
  
  log_msg("[SQL] get_retention_cohort_data(): querying cohort heatmap data.")
  
  if (is.null(max_offset)) {
    date_bounds <- DBI::dbGetQuery(
      con,
      glue::glue_sql(
        "
      SELECT MIN(i.InvoiceDate) AS min_date,
             MAX(i.InvoiceDate) AS max_date
      FROM {`tbl`} fi
      JOIN Invoice i ON fi.InvoiceId = i.InvoiceId
    ",
        .con = con
      )
    )
    
    max_offset <- lubridate::interval(
      date_bounds$min_date, date_bounds$max_date
      ) %/% months(1)
  }
  
  offset_clause <- glue::glue_sql("
    AND DATE_DIFF('month', c.cohort_month, i.InvoiceDate) <= {max_offset}
  ",
                                  .con = con)
  
  query <- glue::glue_sql(
    "
    WITH cohort_dates AS (
      SELECT
        CustomerId,
        DATE_TRUNC('month', MIN(InvoiceDate)) AS cohort_month
      FROM Invoice
      GROUP BY CustomerId
    ),
    cohort_sizes AS (
      SELECT
        cohort_month,
        COUNT(*) AS cohort_size
      FROM cohort_dates
      GROUP BY cohort_month
    ),
    activity AS (
      SELECT
        fi.CustomerId,
        c.cohort_month,
        DATE_TRUNC('month', i.InvoiceDate) AS activity_month,
        DATE_DIFF('month', c.cohort_month, i.InvoiceDate) AS month_offset
      FROM {`tbl`} fi
      JOIN Invoice i ON fi.InvoiceId = i.InvoiceId
      JOIN cohort_dates c ON fi.CustomerId = c.CustomerId
      WHERE DATE_DIFF('month', c.cohort_month, i.InvoiceDate) >= 0
        AND fi.dt BETWEEN DATE({date_range[1]}) AND DATE({date_range[2]})
        {offset_clause}
    ),
    activity_counts AS (
      SELECT
        cohort_month,
        month_offset,
        COUNT(DISTINCT CustomerId) AS num_active_customers
      FROM activity
      GROUP BY cohort_month, month_offset
    )
    SELECT
      ac.cohort_month,
      ac.month_offset,
      ac.num_active_customers,
      cs.cohort_size,
      ac.num_active_customers * 1.0 / cs.cohort_size AS retention_pct
    FROM activity_counts ac
    JOIN cohort_sizes cs ON ac.cohort_month = cs.cohort_month
    WHERE ac.month_offset > 0
    ORDER BY ac.cohort_month, ac.month_offset
  ",
    .con = con
  )
  
  DBI::dbGetQuery(con, query)
}

#' @title Compute Customer Retention KPIs
#' @description
#' Calculates retention KPIs including customer counts, repeat behavior,
#' lifespan metrics, purchase gaps, and top cohort performance across a 
#' filtered subset. Designed for server-level use with data already narrowed 
#' in SQL.
#'
#' @param con A valid DBI connection to a DuckDB instance.
#' @param tbl A character string of the temporary events table name.
#' @param date_range Character vector of length 2 (YYYY-MM-DD)
#' @param cohort_df Data.frame of cohort retention statistics, from
#'   `get_retention_cohort_data()`
#' @param offsets Integer vector of month offsets to assess top cohort 
#'   retention
#'
#' @return A named list of raw KPI values
#' @export

get_retention_kpis <- function(con,
                               date_range,
                               cohort_df,
                               tbl     = "filtered_invoices",
                               offsets = c(3, 6, 9)) {
  stopifnot(!is.null(con), DBI::dbIsValid(con))
  
  log_msg("[SQL] get_retention_kpis(): querying customer retention KPIs.")
  
  if (is.null(date_range)) {
    date_bounds <- DBI::dbGetQuery(
      con,
      glue::glue_sql(
        "
      SELECT MIN(i.InvoiceDate) AS min_date,
             MAX(i.InvoiceDate) AS max_date
      FROM {`tbl`} e
      JOIN Invoice i ON e.InvoiceId = i.InvoiceId
    ",
        .con = con
      )
    )
    
    date_range <- as.character(c(date_bounds$min_date, date_bounds$max_date))
  }

  start_date <- as.Date(min(date_range))
  end_date   <- as.Date(max(date_range))
  
  # Pull one row per customer with all bounds & counts
  query <- glue::glue_sql(
    "
    WITH all_events AS (
      SELECT
        e.CustomerId,
        e.InvoiceId,
        DATE(i.InvoiceDate) as dt,
        ROW_NUMBER() OVER (
          PARTITION BY e.CustomerId
          ORDER BY DATE(i.InvoiceDate), e.InvoiceId
        ) AS rn,
        COUNT(*) OVER (
          PARTITION BY e.CustomerId
        ) AS total_purchases
      FROM {`tbl`} e
      JOIN Invoice i ON i.InvoiceId = e.InvoiceId
    ),

    windowed_events AS (
      SELECT
        CustomerId,
        dt,
        ROW_NUMBER() OVER (
          PARTITION BY CustomerId
          ORDER BY dt, InvoiceId
        ) AS win_rn,
        COUNT(*) OVER (
          PARTITION BY CustomerId
        ) AS num_in_window
      FROM all_events
      WHERE dt BETWEEN {start_date} AND {end_date}
    ),

    bounds_all AS (
      SELECT
        CustomerId,
        MIN(dt) FILTER (WHERE rn = 1) AS first_date,
        MIN(dt) FILTER (WHERE rn = 2) AS second_date,
        MAX(dt) AS last_date,
        total_purchases,
        MAX(dt) FILTER (WHERE dt < {start_date}) AS last_before_window,
        MIN(dt) FILTER (WHERE dt > {end_date}) AS first_after_window
      FROM all_events
      GROUP BY CustomerId, total_purchases
    ),

    bounds_window AS (
      SELECT
        CustomerId,
        MAX(num_in_window) AS num_in_window,
        MAX(CASE WHEN win_rn = 1 THEN dt END) AS first_in_window,
        MAX(CASE WHEN win_rn = 2 THEN dt END) AS second_in_window,
        MAX(dt) AS last_in_window
      FROM windowed_events
      GROUP BY CustomerId
    )

    SELECT
      a.CustomerId,
      a.first_date,
      a.second_date,
      a.last_date,
      a.total_purchases,
      w.first_in_window,
      w.second_in_window,
      w.last_in_window,
      w.num_in_window,
      a.last_before_window,
      a.first_after_window
    FROM bounds_all a
    JOIN bounds_window w USING (CustomerId)
    WHERE w.num_in_window > 0;
    ",
    .con = con
  )
  
  cust_df <- DBI::dbGetQuery(con, query)
  
  if (nrow(cust_df) == 0)
    return(NULL)
  
  log_msg(glue::glue("Loaded {nrow(cust_df)} customers with in-window data"))
  
  # Derived metrics in R
  cust_df <- cust_df |>
    dplyr::mutate(
      cust_new     = num_in_window > 0 & (is.na(last_before_window)),
      repeat_any   = total_purchases > 1,
      repeat_ret   = !cust_new & num_in_window > 0,
      repeat_conv  = 
        (cust_new & num_in_window > 1) | (cust_new & is.na(first_after_window)),
      repeat_window = num_in_window > 1,
      lifespan_mo_total = dplyr::if_else(
        total_purchases > 1,
        lubridate::interval(first_date, last_date) /
          months(1),
        NA_real_
      ),
      lifespan_mo_window = dplyr::case_when(
        !is.na(last_before_window) & !is.na(first_after_window) ~
          lubridate::interval(start_date, end_date) / months(1),
        is.na(last_before_window) ~
          lubridate::interval(first_in_window, end_date) / months(1),
        is.na(first_after_window) ~
          lubridate::interval(start_date, last_in_window) / months(1),
        num_in_window == 1 ~ NA_real_,
        TRUE ~ lubridate::interval(first_in_window, last_in_window) / months(1)
      ),
      # Gaps in days, only when valid
      gap_life    = dplyr::if_else(total_purchases > 1, as.numeric(
        difftime(second_date, first_date, units = "days")
      ), NA_real_),
      gap_window  = dplyr::if_else(!is.na(second_in_window), as.numeric(
        difftime(second_in_window, first_in_window, units = "days")
      ), NA_real_),
      gap_winback   = dplyr::if_else(
        !is.na(last_before_window) & !is.na(first_in_window),
        as.numeric(
          difftime(first_in_window, last_before_window, units = "days")
        ),
        NA_real_
      ),
      gap_retention = dplyr::if_else(
        !is.na(last_in_window) & !is.na(first_after_window),
        as.numeric(difftime(
          first_after_window, last_in_window, units = "days"
        )),
        NA_real_
      ),
      avg_gap_life   = dplyr::if_else(
        total_purchases > 1,
        as.numeric(difftime(last_date, first_date, units = "days")) / 
          (total_purchases - 1),
        NA_real_
      ),
      avg_gap_window = dplyr::if_else(
        num_in_window > 1,
        as.numeric(difftime(
          last_in_window, first_in_window, units = "days"
        )) / (num_in_window - 1),
        NA_real_
      ),
      avg_gap_bound  = dplyr::case_when(
        is.na(last_before_window) & is.na(first_after_window) ~
          avg_gap_window,!is.na(last_before_window) &
          !is.na(first_after_window) ~
          as.numeric(
            difftime(first_after_window, last_before_window, units = "days")
          ) /
          (num_in_window + 1),
        is.na(last_before_window) ~ as.numeric(
          difftime(first_after_window, first_in_window, units = "days")
        ) / num_in_window,
        is.na(first_after_window) ~ as.numeric(difftime(
          last_in_window, last_before_window, units = "days"
        )) / num_in_window,
        TRUE ~ avg_gap_window
      )
    )
  
  # Summarize
  n_total   <- nrow(cust_df)
  n_new     <- sum(cust_df$cust_new, na.rm = TRUE)
  n_repeat  <- sum(cust_df$repeat_any, na.rm = TRUE)
  n_ret     <- sum(cust_df$repeat_ret, na.rm = TRUE)
  n_conv    <- sum(cust_df$repeat_conv, na.rm = TRUE)
  n_rep_w   <- sum(cust_df$repeat_window, na.rm = TRUE)
  
  raw_kpis <- list(
    num_cust         = n_total,
    num_new          = n_new,
    pct_new          = n_new / n_total,
    ret_n_any    = n_repeat,
    ret_rate_any      = n_repeat / n_total,
    ret_n_return = n_ret,
    ret_rate_return  = n_ret / (n_total - n_new),
    ret_n_conv = n_conv,
    ret_rate_conv    = n_conv / n_new,
    ret_n_window = n_rep_w,
    ret_rate_window = n_rep_w / n_total,
    avg_life_mo_tot  = mean(cust_df$lifespan_mo_total, na.rm = TRUE),
    avg_life_mo_win  = mean(cust_df$lifespan_mo_window, na.rm = TRUE),
    med_gap_life     = stats::median(cust_df$gap_life, na.rm = TRUE),
    med_gap_window = stats::median(cust_df$gap_window, na.rm = TRUE),
    med_gap_winback = stats::median(cust_df$gap_winback, na.rm = TRUE),
    med_gap_ret      = stats::median(cust_df$gap_retention, na.rm = TRUE),
    avg_gap_life     = mean(cust_df$avg_gap_life, na.rm = TRUE),
    avg_gap_window      = mean(cust_df$avg_gap_window, na.rm = TRUE),
    avg_gap_bound    = mean(cust_df$avg_gap_bound, na.rm = TRUE)
  )
  
  # Format with centralized helper
  kpis <- purrr::imap(raw_kpis, function(val, key) {
    type <- dplyr::case_when(
      grepl("pct|rate", key)      ~ "percent",
      grepl("life|avg|gap|med", key) ~ "float",
      TRUE                          ~ "number"
    )
    format_kpi_value(val, type = type)
  })
  
  # Top Cohort by Offset
  top_cohorts <- purrr::map_dfc(offsets, function(offset) {
    filtered <- cohort_df |>
      dplyr::filter(month_offset == offset, !is.na(retention_pct)) |>
      dplyr::arrange(dplyr::desc(retention_pct)) |>
      dplyr::slice_head(n = 1)
    
    cohort_fmt <- if (nrow(filtered) == 1) {
      format(as.Date(filtered$cohort_month), "%b %Y")
    } else {
      NA_character_
    }
    
    retention_val <- if (nrow(filtered) == 1) {
      format_kpi_value(filtered$retention_pct, "percent")
    } else {
      NA_real_
    }
    
    setNames(list(cohort_fmt, retention_val), c(
      paste0("top_cohort_month_", offset),
      paste0("top_cohort_retention_", offset)
    ))
  })
  
  kpis <- append(kpis, top_cohorts)
  
  log_msg("[SQL] get_retention_kpis(): Retention KPI aggregation complete.")
  
  return(kpis)
  
}

# Memoised version of get_retention_cohort_data() to avoid recomputation 
# across identical filter sets
memo_get_retention_cohort_data <- 
  memoise::memoise(get_retention_cohort_data, cache = shared_cache)

# Memoised version of get_events_shared() to avoid recomputation 
# across identical filter sets
memo_get_events_shared <- 
  memoise::memoise(get_events_shared, cache = shared_cache)
