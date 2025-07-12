#' @file topn_kpis.R
#' @title Top-N KPI Table Extraction Utilities
#' @description
#' This file contains a set of helper functions for extracting and formatting
#' top-N KPI tables for grouped dashboard panels (e.g., Genre, Artist).
#' It slices top groups by selected metric while preserving all KPI columns,
#' and returns nested structures for easy UI card access.

#' Build Full KPI Table for a Group
#'
#' @param con A DBI connection object
#' @param group_var Character string: "Genre", "Artist", etc.
#' @param tbl A character string of the temporary events table name.
#' @param date_range Character vector of length 2
#' @return A data.frame with KPI metrics for each group value
#' @export
topn_kpis_build_full_table <- function(con, group_var, tbl, date_range = NULL) {
  stopifnot(DBI::dbIsValid(con))
  group_var <- rlang::arg_match(
    group_var, c("Genre", "Artist", "BillingCountry")
  )
  
  log_msg(glue::glue("[topn_kpis] Building full KPI table for {group_var}"))
  
  df <- get_group_kpis_full(
    con = con,
    tbl = tbl,
    group_var = group_var,
    date_range = date_range
  )
  
  return(df)
}

#' Slice Top-N Groups by Selected Metric
#'
#' @param df KPI table of all groups
#' @param metric_var Metric column to sort by
#' @param n Integer: number of top groups to return
#' @return A data.frame of top-N rows, preserving all KPI columns
#' @export
topn_kpis_slice_topn <- function(df, metric_var, n = 5) {
  df |>
    dplyr::filter(!is.na(.data[[metric_var]])) |>
    dplyr::arrange(dplyr::desc(.data[[metric_var]])) |>
    dplyr::slice_head(n = n)
}

#' Generate Top-N KPI Tables for All Metrics
#'
#' @param df_full Full KPI table for a group (Genre, Artist)
#' @param metrics Named list with each item containing `var_name`
#' @param n Number of top rows to extract per metric
#' @return A named list of top-N tables keyed by metric
#' @export
topn_kpis_generate <- function(df_full, metrics, n = 5) {
  log_msg(glue::glue("[topn_kpis] Slicing top {n} groups by metrics"))
  
  tables <- purrr::map(
    metrics,
    ~ topn_kpis_slice_topn(df_full, .x$var_name, n)
  )
  
  names(tables) <- purrr::map_chr(metrics, "var_name")
  return(tables)
}

#' Format KPI Table Display Values
#'
#' @param df A top-N KPI table
#' @param group_var Character: "Genre", "Artist"
#' @param total_rev Numeric: total revenue of the subset
#' 
#' @return A data.frame with formatted display values
#' @export
topn_kpis_format_display <- function(df, group_var, total_rev = NULL) {
  
  revenue_share_calc <- if (
    is.null(total_rev) || is.na(total_rev) || !is.numeric(total_rev)
  ) {
    df$revenue / sum(df$revenue, na.rm = TRUE)
  } else {
    df$revenue / total_rev
  }
  
  # Calculate aggregation KPIs
  df <- df |>
    dplyr::mutate(
      avg_revenue_per_cust = revenue / num_customers,
      avg_revenue_per_purchase = revenue / num_purchases,
      avg_tracks_per_purchase = tracks_sold / num_purchases,
      revenue_share = revenue_share_calc
    ) 
  
  # Format col names (dynamic, flexible for different group-based columns)
  excluded <- c("group_val")
  metric_cols <- setdiff(names(df), excluded)
  
  formatted <- purrr::map_dfc(metric_cols, function(col) {
    type <- dplyr::case_when(
      grepl("percent|pct", col) ~ "percent",
      grepl("share", col)       ~ "percent",
      grepl("revenue", col)     ~ "dollar",
      grepl("avg_tracks_", col) ~ "float",
      grepl("tracks|purchases|customers|orders|catalog", col) ~ "number",
      TRUE ~ "float"
    )
    
    val <- df[[col]]
    fmt <- format_kpi_value(val, type = type)
    
    # Return a single column with "_fmt" suffix
    tibble::tibble(!!paste0(col, "_fmt") := fmt)
  })
  
  df <- dplyr::bind_cols(df, formatted)
  
  if (tolower(group_var) == "billingcountry"){
    df <- df |>
      dplyr::mutate(group_val = format_kpi_value(group_val, type = "country"))
  }
  
  return(df)
}
