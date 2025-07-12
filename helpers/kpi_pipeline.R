#' @file kpi_pipeline.R
#' @title Shared KPI Aggregation and Metric Utilities
#' @description
#' This file defines helper functions for computing shared KPIs and
#' slicing top-N group metrics across the dashboard. It provides:
#' - A wrapper for building metric definitions from UI filters
#' - A pipeline function for retrieving core KPIs and per-group rankings
#' - Logging support for tracing data aggregation stages
#'
#' The metric definitions are compatible with module filtering logic,
#' and enriched tables support display-friendly formatting and metadata
#' for genre, artist, and country-level summaries.
#'
#' All computations are delegated to DuckDB using parameterized queries.

#' @title Build Metric Definitions from UI Choice Vector
#' @description
#' Converts the global `metric_choices` vector into a list of definitions
#' with `var_name` and `label` fields. Used for dynamic top-N slicing and
#' labeling throughout KPI pipelines.
#'
#' @param metric_choices Named character vector from `global.R`
#'   (e.g., `"Revenue (USD$)" = "revenue"`)
#'
#' @param append Optional list of additional metrics not included in UI
#'   (e.g., derived values like `"revenue_share"`)
#'
#' @return A list of metric definitions, each with `var_name` and `label`.
#' @export
get_metric_definitions <- function(metric_choices, append = NULL) {
  ui_metrics <- purrr::map(names(metric_choices),
                           ~ list(var_name = metric_choices[[.x]], label = .x))
  
  if (!is.null(append)) {
    full_metrics <- c(ui_metrics, append)
  } else {
    full_metrics <- ui_metrics
  }
  
  return(full_metrics)
}

#' @title Aggregate Shared KPI Metrics for Dashboard Use
#' @description
#' Computes aggregate KPIs, top-N group slices, and customer retention KPIs
#' across selected filters. Returns nested KPI blocks for genre, artist, and
#' countries for Top-N tables.
#'
#' @param con DBI connection to DuckDB.
#' @param tbl Name of temp view with filtered invoices.
#' @param metrics Output of `get_metric_definitions()` for metrics for top-N
#'   group-by-metric slices.
#' @param date_range Character vector of length 2.
#' @param top_n Integer: max top group values to retain.
#' @param cohort_df Data.frame of cohort retention statistics, from
#'   `get_retention_cohort_data()`
#' @param offsets Integer vector of month offsets to assess top cohort
#'   retention
#'
#' @return List containing KPI blocks and top-N nested tables.
#' @keywords internal
get_shared_kpis <- function(con,
                            tbl,
                            metrics,
                            date_range = NULL,
                            top_n = 5,
                            cohort_df,
                            offsets = c(3, 6, 9)) {
  stopifnot(DBI::dbIsValid(con), is.character(tbl), length(tbl) == 1)
  
  log_msg("[KPI] ▶ Starting shared KPI aggregation")
  
  revenue_kpis <- get_revenue_kpis(con, tbl, date_range)
  if (is.null(revenue_kpis))
    return(NULL)
  
  customer_kpis <- get_customer_kpis(con, tbl, date_range)
  if (is.null(customer_kpis))
    return(NULL)
  
  purchase_kpis <- get_purchase_kpis(con, tbl, date_range)
  if (is.null(purchase_kpis))
    return(NULL)
  
  metadata_kpis <- get_subset_metadata_kpis(con, tbl)
  
  log_msg("   ✔ Retrieved core KPI blocks")
  
  groupings <- c("Genre", "Artist", "BillingCountry")
  topn_by_group <- purrr::map(groupings, function(group) {
    log_msg(glue::glue("   ⏳ Generating top-N tables for {group}"))
    
    # Build full KPI table
    full_kpis <- topn_kpis_build_full_table(
      con = con,
      tbl = tbl,
      group_var = group,
      date_range = date_range
    )
    
    # Enrich with catalog coverage for Genre/Artist only
    if (group %in% c("Genre", "Artist")) {
      full_kpis <- enrich_catalog_kpis(con, tbl, full_kpis, group)
    }
    
    # Slice top-N tables for each metric & format for display
    topn_tables <- purrr::map(metrics, function(metric_def) {
      trimmed <- topn_kpis_slice_topn(df = full_kpis,
                                      metric_var = metric_def$var_name,
                                      n = top_n)
      topn_kpis_format_display(
        df = trimmed,
        total_rev = revenue_kpis$total_revenue,
        group_var = group
      )
    })
    
    names(topn_tables) <- purrr::map_chr(metrics, "var_name")
    
    # Add meta-data "Total possible values"
    topn_tables$num_vals <- format_kpi_value(
      metadata_kpis[[glue::glue("num_{tolower(group)}")]], 
      "number"
      )
    
    return(topn_tables)
  })
  names(topn_by_group) <- paste0("topn_", tolower(groupings))
  
  log_msg("   ✔ Retrieved Top-N KPI blocks")
  
  retention_kpis <- get_retention_kpis(
    con = con,
    date_range = date_range,
    cohort_df = cohort_df,
    tbl = tbl,
    offsets = offsets
  )
  
  log_msg("   ✔ Shared KPI pipeline complete")
  
  return(
    list(
      revenue_kpis    = revenue_kpis,
      customer_kpis   = customer_kpis,
      purchase_kpis   = purchase_kpis,
      topn            = topn_by_group,
      metadata_kpis   = metadata_kpis,
      retention_kpis  = retention_kpis
    )
  )
}

# Memoised version of get_shared_kpis() to avoid recomputation across 
# identical filter sets
memo_get_shared_kpis <- memoise::memoise(get_shared_kpis, cache = shared_cache)