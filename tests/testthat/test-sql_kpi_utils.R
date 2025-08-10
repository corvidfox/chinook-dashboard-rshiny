# tests/testthat/test-sql_kpi_utils.R
# Setup DuckDB connection and load Chinook data

# -------------------------------
# Tests for get_events_shared()
# -------------------------------

test_that("get_events_shared returns expected columns", {
  clause <- form_where_clause(country = "USA", .con = con)
  df <- get_events_shared(con, clause)
  expect_s3_class(df, "data.frame")
  expect_true(all(c("CustomerId", "InvoiceId", "dt") %in% names(df)))
})

# -------------------------------
# Tests for get_subset_metadata_kpis()
# -------------------------------

test_that("get_subset_metadata_kpis returns named list of counts", {
  result <- get_subset_metadata_kpis(con, tbl = "Invoice", date_range = c("2010-01-01", "2010-03-31"))
  expect_type(result, "list")
  expect_named(result, c("num_genre", "num_artist", "num_billingcountry"))
})

# -------------------------------
# Tests for get_revenue_kpis()
# -------------------------------

test_that("get_revenue_kpis returns total revenue", {
  result <- get_revenue_kpis(con, tbl = "Invoice", date_range = c("2010-01-01", "2010-03-31"))
  expect_type(result, "list")
  expect_named(result, "total_revenue")
  expect_true(is.numeric(result$total_revenue))
})

# -------------------------------
# Tests for get_customer_kpis()
# -------------------------------

test_that("get_customer_kpis returns customer metrics", {
  result <- get_customer_kpis(con, tbl = "Invoice", date_range = c("2010-01-01", "2010-03-31"))
  expect_type(result, "list")
  expect_named(result, c("total_customers", "pct_new_customers"))
})

# -------------------------------
# Tests for get_purchase_kpis()
# -------------------------------

test_that("get_purchase_kpis returns purchase metrics", {
  result <- get_purchase_kpis(con, tbl = "Invoice", date_range = c("2010-01-01", "2010-03-31"))
  expect_type(result, "list")
  expect_named(result, c("total_orders", "avg_rev_per_order", "total_tracks", "avg_tracks_per_order"))
})

# -------------------------------
# Tests for get_group_kpis_full()
# -------------------------------

test_that("get_group_kpis_full returns grouped KPI table", {
  df <- get_group_kpis_full(con, tbl = "Invoice", group_var = "Genre", date_range = c("2010-01-01", "2010-03-31"))
  expect_s3_class(df, "data.frame")
  expect_true("group_val" %in% names(df))
})

# -------------------------------
# Tests for get_retention_cohort_data()
# -------------------------------

test_that("get_retention_cohort_data returns cohort heatmap data", {
  df <- get_retention_cohort_data(con, tbl = "Invoice", date_range = c("2010-01-01", "2010-03-31"))
  expect_s3_class(df, "data.frame")
  expect_true(all(c("cohort_month", "month_offset", "retention_pct") %in% names(df)))
})

# -------------------------------
# Tests for get_retention_kpis()
# -------------------------------

test_that("get_retention_kpis returns formatted retention KPIs", {
  cohort_df <- get_retention_cohort_data(con, tbl = "Invoice", date_range = c("2010-01-01", "2010-03-31"))
  result <- get_retention_kpis(con, date_range = c("2010-01-01", "2010-03-31"), cohort_df = cohort_df, tbl = "Invoice")
  expect_type(result, "list")
  expect_true("ret_rate_any" %in% names(result))
})
