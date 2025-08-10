# tests/testthat/test-ts_helpers.R

# -------------------------------
# Tests for get_ts_monthly_summary()
# -------------------------------

test_that("get_ts_monthly_summary returns expected monthly aggregates", {
  df <- get_ts_monthly_summary(con, c("2009-01-01", "2010-12-31"))
  expect_s3_class(df, "data.frame")
  expect_true(all(c("month", "num_purchases", "num_customers", "tracks_sold", "revenue", "first_time_customers") %in% names(df)))
  expect_true(nrow(df) > 0)
})

test_that("get_ts_monthly_summary returns correct full-range summary", {
  df <- get_ts_monthly_summary(con, c("2009-01-01", "2013-12-31"))
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 60)
  expect_equal(names(df), c("month", "num_purchases", "num_customers", "tracks_sold", "revenue", "first_time_customers"))
  
  # Check first row
  expect_equal(df[1, "month"], "2009-01")
  expect_equal(df[1, "num_purchases"], 6)
  expect_equal(df[1, "num_customers"], 6)
  expect_equal(df[1, "tracks_sold"], 36)
  expect_equal(df[1, "revenue"], 35.64)
  expect_equal(df[1, "first_time_customers"], 6)
  
  # Check last row
  expect_equal(df[60, "month"], "2013-12")
  expect_equal(df[60, "num_purchases"], 7)
  expect_equal(df[60, "revenue"], 38.62)
  expect_equal(df[60, "first_time_customers"], 0)
})

test_that("get_ts_monthly_summary returns correct 2010 subset", {
  df <- get_ts_monthly_summary(con, c("2010-01-01", "2010-12-31"))
  expect_equal(nrow(df), 12)
  expect_equal(df[1, "month"], "2010-01")
  expect_equal(df[1, "revenue"], 52.62)
  expect_equal(df[9, "month"], "2010-09")
  expect_equal(df[9, "num_purchases"], 6)
  expect_equal(df[9, "tracks_sold"], 37)
  expect_equal(df[9, "first_time_customers"], 0)
})

test_that("get_ts_monthly_summary returns empty data frame for null subset", {
  df <- get_ts_monthly_summary(con, c("2014-01-01", "2014-12-31"))
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 0)
})

# -------------------------------
# Tests for format_ts_kpi_display()
# -------------------------------
test_that("format_ts_kpi_display returns formatted KPI values", {
  kpis <- list(
    revenue_kpis = list(total_revenue = 1000),
    purchase_kpis = list(total_orders = 50, total_tracks = 200, avg_rev_per_order = 20),
    customer_kpis = list(total_customers = 40, pct_new_customers = 0.25)
  )
  result <- format_ts_kpi_display(kpis, c("2020-01-01", "2020-03-01"))
  expect_type(result, "list")
  expect_equal(names(result), c("total_rev", "avg_rev", "total_purchases", "total_tracks", "avg_per_purchase", "total_customers", "first_time_pct"))
  expect_true(all(sapply(result, is.character)))
})

# -------------------------------
# Tests for ts_plotter()
# -------------------------------

test_that("ts_plotter returns plotly object for valid data", {
  df <- data.frame(
    month = c("2020-01", "2020-02"),
    revenue = c(100, 200),
    num_purchases = c(10, 20),
    tracks_sold = c(50, 60),
    num_customers = c(5, 10),
    first_time_customers = c(2, 3)
  )
  metric <- list(var_name = "revenue", label = "Revenue")
  styles <- list(line_color = "blue", line_size = 1, point_color = "red", point_size = 2, font_size = 12)
  
  plot <- ts_plotter(df, metric, styles)
  expect_s3_class(plot, "plotly")
})

test_that("ts_plotter returns empty plot for missing data", {
  df <- data.frame(month = character(), revenue = numeric())
  metric <- list(var_name = "revenue", label = "Revenue")
  styles <- list(line_color = "blue", line_size = 1, point_color = "red", point_size = 2)
  
  plot <- ts_plotter(df, metric, styles)
  expect_s3_class(plot, "plotly")
})
