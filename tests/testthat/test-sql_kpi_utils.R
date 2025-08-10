# tests/testthat/test-sql_kpi_utils.R

# -------------------------------
# Tests for get_events_shared()
# -------------------------------

test_that("get_events_shared returns expected columns", {
  clause <- form_where_clause(country = "USA", .con = con)
  df <- get_events_shared(con, clause)
  expect_s3_class(df, "data.frame")
  expect_true(all(c("CustomerId", "InvoiceId", "dt") %in% names(df)))
})

test_that("get_events_shared returns correct USA subset", {
  clause <- form_where_clause(country = "USA", .con = con)
  df <- get_events_shared(con, clause)
  
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 91)
  expect_equal(length(unique(df$CustomerId)), 13)
  expect_equal(length(unique(df$InvoiceId)), 91)
  expect_equal(length(unique(df$dt)), 80)
  expect_equal(min(df$dt), as.Date("2009-01-11"))
  expect_equal(max(df$dt), as.Date("2013-12-05"))
})

test_that("get_events_shared returns empty data frame for nonexistent country", {
  clause <- form_where_clause(country = "Uzbekistan", .con = con)
  df <- get_events_shared(con, clause)
  
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 0)
})

# -------------------------------
# Tests for get_subset_metadata_kpis()
# -------------------------------

test_that("get_subset_metadata_kpis returns named list of counts", {
  result <- get_subset_metadata_kpis(con, tbl = "Invoice", date_range = c("2010-01-01", "2010-03-31"))
  expect_type(result, "list")
  expect_named(result, c("num_genre", "num_artist", "num_billingcountry"))
})

test_that("get_subset_metadata_kpis returns correct full-range counts", {
  result <- get_subset_metadata_kpis(con, tbl = "Invoice", date_range = c("2009-01-01", "2013-12-31"))
  expect_equal(result$num_genre, 24)
  expect_equal(result$num_artist, 165)
  expect_equal(result$num_billingcountry, 24)
})

test_that("get_subset_metadata_kpis returns correct 2010 counts", {
  result <- get_subset_metadata_kpis(con, tbl = "Invoice", date_range = c("2010-01-01", "2010-12-31"))
  expect_equal(result$num_genre, 24)
  expect_equal(result$num_artist, 113)
  expect_equal(result$num_billingcountry, 20)
})

test_that("get_subset_metadata_kpis returns zeros for empty date range", {
  result <- get_subset_metadata_kpis(con, tbl = "Invoice", date_range = c("2014-01-01", "2014-01-01"))
  expect_equal(result$num_genre, 0)
  expect_equal(result$num_artist, 0)
  expect_equal(result$num_billingcountry, 0)
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

test_that("get_revenue_kpis returns correct full-range revenue", {
  result <- get_revenue_kpis(con, tbl = "Invoice", date_range = c("2009-01-01", "2013-12-31"))
  expect_equal(result$total_revenue, 2328.6, tolerance = 1e-2)
})

test_that("get_revenue_kpis returns correct 2010 revenue", {
  result <- get_revenue_kpis(con, tbl = "Invoice", date_range = c("2010-01-01", "2010-12-31"))
  expect_equal(result$total_revenue, 481.45, tolerance = 1e-2)
})

test_that("get_revenue_kpis returns NULL for empty date range", {
  result <- get_revenue_kpis(con, tbl = "Invoice", date_range = c("2014-01-01", "2014-01-01"))
  expect_null(result)
})

# -------------------------------
# Tests for get_customer_kpis()
# -------------------------------

test_that("get_customer_kpis returns customer metrics", {
  result <- get_customer_kpis(con, tbl = "Invoice", date_range = c("2010-01-01", "2010-03-31"))
  expect_type(result, "list")
  expect_named(result, c("total_customers", "pct_new_customers"))
})

test_that("get_customer_kpis returns correct full-range customer metrics", {
  result <- get_customer_kpis(con, tbl = "Invoice", date_range = c("2009-01-01", "2013-12-31"))
  expect_equal(result$total_customers, 59)
  expect_equal(result$pct_new_customers, 1, tolerance = 1e-6)
})

test_that("get_customer_kpis returns correct 2010 customer metrics", {
  result <- get_customer_kpis(con, tbl = "Invoice", date_range = c("2010-01-01", "2010-12-31"))
  expect_equal(result$total_customers, 46)
  expect_equal(result$pct_new_customers, 0.2826087, tolerance = 1e-6)
})

test_that("get_customer_kpis returns zeros for empty date range", {
  result <- get_customer_kpis(con, tbl = "Invoice", date_range = c("2014-01-01", "2014-01-01"))
  expect_equal(result$total_customers, 0)
  expect_equal(result$pct_new_customers, 0)
})

# -------------------------------
# Tests for get_purchase_kpis()
# -------------------------------

test_that("get_purchase_kpis returns purchase metrics", {
  result <- get_purchase_kpis(con, tbl = "Invoice", date_range = c("2010-01-01", "2010-03-31"))
  expect_type(result, "list")
  expect_named(result, c("total_orders", "avg_rev_per_order", "total_tracks", "avg_tracks_per_order"))
})

test_that("get_purchase_kpis returns correct full-range purchase metrics", {
  result <- get_purchase_kpis(con, tbl = "Invoice", date_range = c("2009-01-01", "2013-12-31"))
  expect_equal(result$total_orders, 412)
  expect_equal(result$avg_rev_per_order, 5.651942, tolerance = 1e-6)
  expect_equal(result$total_tracks, 2240)
  expect_equal(result$avg_tracks_per_order, 5.436893, tolerance = 1e-6)
})

test_that("get_purchase_kpis returns correct 2010 purchase metrics", {
  result <- get_purchase_kpis(con, tbl = "Invoice", date_range = c("2010-01-01", "2010-12-31"))
  expect_equal(result$total_orders, 83)
  expect_equal(result$avg_rev_per_order, 5.800602, tolerance = 1e-6)
  expect_equal(result$total_tracks, 455)
  expect_equal(result$avg_tracks_per_order, 5.481928, tolerance = 1e-6)
})

test_that("get_purchase_kpis returns NA metrics for empty date range", {
  result <- get_purchase_kpis(con, tbl = "Invoice", date_range = c("2014-01-01", "2014-01-01"))
  expect_equal(result$total_orders, 0)
  expect_true(is.na(result$avg_rev_per_order))
  expect_true(is.na(result$total_tracks))
  expect_true(is.na(result$avg_tracks_per_order))
})

# -------------------------------
# Tests for get_group_kpis_full()
# -------------------------------

test_that("get_group_kpis_full returns grouped KPI table", {
  df <- get_group_kpis_full(con, tbl = "Invoice", group_var = "Genre", date_range = c("2010-01-01", "2010-03-31"))
  expect_s3_class(df, "data.frame")
  expect_true("group_val" %in% names(df))
})

test_that("get_group_kpis_full returns correct full-range genre metrics", {
  df <- get_group_kpis_full(con, tbl = "Invoice", group_var = "Genre", date_range = c("2009-01-01", "2013-12-31"))
  
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 24)
  expect_equal(df$num_customers[df$group_val == "Rock"], 59)
  expect_equal(df$revenue[df$group_val == "Rock"], 826.65, tolerance = 1e-2)
  expect_equal(df$first_time_customers[df$group_val == "Rock"], 27)
})

test_that("get_group_kpis_full returns correct 2010 genre metrics", {
  df <- get_group_kpis_full(con, tbl = "Invoice", group_var = "Genre", date_range = c("2010-01-01", "2010-12-31"))
  
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 24)
  expect_equal(df$num_customers[df$group_val == "Rock"], 33)
  expect_equal(df$revenue[df$group_val == "Rock"], 155.43, tolerance = 1e-2)
  expect_equal(df$first_time_customers[df$group_val == "Rock"], 20)
})

test_that("get_group_kpis_full returns empty data frame for empty date range", {
  df <- get_group_kpis_full(con, tbl = "Invoice", group_var = "Genre", date_range = c("2014-01-01", "2014-01-01"))
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 0)
})

# -------------------------------
# Tests for get_retention_cohort_data()
# -------------------------------

test_that("get_retention_cohort_data returns cohort heatmap data", {
  df <- get_retention_cohort_data(con, tbl = "Invoice", date_range = c("2010-01-01", "2010-03-31"))
  expect_s3_class(df, "data.frame")
  expect_true(all(c("cohort_month", "month_offset", "retention_pct") %in% names(df)))
})

test_that("get_retention_cohort_data returns correct full-range cohort metrics", {
  df <- get_retention_cohort_data(con, tbl = "Invoice", date_range = c("2009-01-01", "2013-12-31"))
  
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 304)
  expect_equal(names(df), c("cohort_month", "month_offset", "num_active_customers", "cohort_size", "retention_pct"))
  
  # Check first cohort
  first <- df[df$cohort_month == "2009-01-01" & df$month_offset == 1, ]
  expect_equal(first$num_active_customers, 1)
  expect_equal(first$cohort_size, 6)
  expect_equal(first$retention_pct, 1/6, tolerance = 1e-6)
  
  # Check last cohort
  last <- df[df$cohort_month == "2010-07-01" & df$month_offset == 41, ]
  expect_equal(last$num_active_customers, 1)
  expect_equal(last$cohort_size, 1)
  expect_equal(last$retention_pct, 1, tolerance = 1e-6)
})

test_that("get_retention_cohort_data returns correct 2010 cohort metrics", {
  df <- get_retention_cohort_data(con, tbl = "Invoice", date_range = c("2010-01-01", "2010-12-31"))
  
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 65)
  expect_equal(names(df), c("cohort_month", "month_offset", "num_active_customers", "cohort_size", "retention_pct"))
  
  # Check first row
  first <- df[1, ]
  expect_equal(first$cohort_month, as.Date("2009-01-01"))
  expect_equal(first$month_offset, 18)
  expect_equal(first$num_active_customers, 1)
  expect_equal(first$cohort_size, 6)
  expect_equal(first$retention_pct, 1/6, tolerance = 1e-6)
  
  # Check last row
  last <- df[nrow(df), ]
  expect_equal(last$cohort_month, as.Date("2010-07-01"))
  expect_equal(last$month_offset, 3)
  expect_equal(last$num_active_customers, 1)
  expect_equal(last$cohort_size, 1)
  expect_equal(last$retention_pct, 1.0, tolerance = 1e-6)
})

test_that("get_retention_cohort_data returns empty data frame for empty date range", {
  df <- get_retention_cohort_data(con, tbl = "Invoice", date_range = c("2014-01-01", "2014-01-01"))
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 0)
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

test_that("get_retention_kpis returns correct full-range retention metrics", {
  cohort_df <- get_retention_cohort_data(con, tbl = "Invoice", date_range = c("2009-01-01", "2013-12-31"))
  result <- get_retention_kpis(con, date_range = c("2009-01-01", "2013-12-31"), cohort_df = cohort_df, tbl = "Invoice")
  
  expect_equal(result$num_cust, "59")
  expect_equal(result$num_new, "59")
  expect_equal(result$pct_new, "100.00%")
  expect_equal(result$ret_n_any, "59")
  expect_equal(result$ret_rate_any, "100.00%")
  expect_equal(result$ret_n_return, "0")
  expect_equal(result$ret_rate_return, "NA")
  expect_equal(result$ret_n_conv, "59")
  expect_equal(result$ret_rate_conv, "100.00%")
  expect_equal(result$ret_n_window, "59")
  expect_equal(result$ret_rate_window, "100.00%")
  expect_equal(result$avg_life_mo_tot, "46.03")
  expect_equal(result$avg_life_mo_win, "46.03")
  expect_equal(result$med_gap_life, "94.00")
  expect_equal(result$med_gap_window, "94.00")
  expect_equal(result$med_gap_winback, "NA")
  expect_equal(result$med_gap_ret, "NA")
  expect_equal(result$avg_gap_life, "234.61")
  expect_equal(result$avg_gap_window, "234.61")
  expect_equal(result$avg_gap_bound, "234.61")
  expect_equal(result$top_cohort_month_3, "Jul 2010")
  expect_equal(result$top_cohort_retention_3, "100.00%")
  expect_equal(result$top_cohort_month_6, "Jul 2010")
  expect_equal(result$top_cohort_retention_6, "100.00%")
  expect_equal(result$top_cohort_month_9, "Sep 2009")
  expect_equal(result$top_cohort_retention_9, "50.00%")
})

test_that("get_retention_kpis returns correct 2010 retention metrics", {
  cohort_df <- get_retention_cohort_data(con, tbl = "Invoice", date_range = c("2010-01-01", "2010-12-31"))
  result <- get_retention_kpis(con, date_range = c("2010-01-01", "2010-12-31"), cohort_df = cohort_df, tbl = "Invoice")
  
  expect_equal(result$num_cust, "46")
  expect_equal(result$num_new, "13")
  expect_equal(result$pct_new, "28.26%")
  expect_equal(result$ret_n_any, "46")
  expect_equal(result$ret_rate_any, "100.00%")
  expect_equal(result$ret_n_return, "33")
  expect_equal(result$ret_rate_return, "100.00%")
  expect_equal(result$ret_n_conv, "13")
  expect_equal(result$ret_rate_conv, "100.00%")
  expect_equal(result$ret_n_window, "27")
  expect_equal(result$ret_rate_window, "58.70%")
  expect_equal(result$avg_life_mo_tot, "44.28")
  expect_equal(result$avg_life_mo_win, "10.22")
  expect_equal(result$med_gap_life, "94.00")
  expect_equal(result$med_gap_window, "94.00")
  expect_equal(result$med_gap_winback, "243.00")
  expect_equal(result$med_gap_ret, "391.50")
  expect_equal(result$avg_gap_life, "225.97")
  expect_equal(result$avg_gap_window, "107.17")
  expect_equal(result$avg_gap_bound, "285.14")
  expect_equal(result$top_cohort_month_3, "Jul 2010")
  expect_equal(result$top_cohort_retention_3, "100.00%")
  expect_equal(result$top_cohort_month_6, "Sep 2009")
  expect_equal(result$top_cohort_retention_6, "50.00%")
  expect_equal(result$top_cohort_month_9, "Sep 2009")
  expect_equal(result$top_cohort_retention_9, "50.00%")
})

test_that("get_retention_kpis returns NULL for empty cohort data", {
  cohort_df <- get_retention_cohort_data(con, tbl = "Invoice", date_range = c("2014-01-01", "2014-01-01"))
  result <- get_retention_kpis(con, date_range = c("2014-01-01", "2014-04-01"), cohort_df = cohort_df, tbl = "Invoice")
  expect_null(result)
})
