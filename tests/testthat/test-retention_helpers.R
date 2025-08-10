# tests/testthat/test-retention_helpers.R

# -------------------------------
# Tests for get_retention_decay_data()
# -------------------------------

test_that("get_retention_decay_data returns correct decay data for full range", {
  df <- get_retention_decay_data(con, tbl = "Invoice", date_range = c("2009-01-01", "2013-12-31"))
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 36)
  expect_equal(names(df), c("month_offset", "num_retained", "num_customers", "retention_rate"))
  expect_equal(df[1, "month_offset"], 1)
  expect_equal(df[1, "num_retained"], 18)
  expect_equal(df[1, "retention_rate"], 0.30508475)
  expect_equal(df[36, "month_offset"], 59)
})

test_that("get_retention_decay_data returns correct decay data for 2010", {
  df <- get_retention_decay_data(con, tbl = "Invoice", date_range = c("2010-01-01", "2010-12-31"))
  expect_equal(nrow(df), 10)
  expect_equal(df[1, "month_offset"], 1)
  expect_equal(df[1, "num_retained"], 7)
  expect_equal(df[1, "retention_rate"], 0.15217391)
})

test_that("get_retention_decay_data returns empty data frame for null subset", {
  df <- get_retention_decay_data(con, tbl = "Invoice", date_range = c("2014-01-01", "2014-01-01"))
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 0)
})

# -------------------------------
# Tests for decay_plotter()
# -------------------------------

test_that("decay_plotter returns plotly object for valid data", {
  df <- data.frame(
    month_offset = c(1, 3, 6),
    retention_rate = c(0.3, 0.4, 0.25)
  )
  styles <- list(
    line_color = "blue",
    line_size = 1,
    point_color = "red",
    point_size = 2,
    font_size = 12
  )
  
  plot <- decay_plotter(df, styles)
  expect_s3_class(plot, "plotly")
})

test_that("decay_plotter returns empty plot for empty data", {
  df <- data.frame(
    month_offset = numeric(),
    retention_rate = numeric()
  )
  styles <- list(
    line_color = "blue",
    line_size = 1,
    point_color = "red",
    point_size = 2,
    font_size = 12
  )
  
  plot <- decay_plotter(df, styles)
  expect_s3_class(plot, "plotly")
})

# -------------------------------
# Tests for cohort_heatmap_plotter()
# -------------------------------

test_that("cohort_heatmap_plotter returns plotly object for valid cohort data", {
  df <- data.frame(
    cohort = c("2009-01", "2009-01", "2009-02", "2009-02"),
    month_offset = c(1, 2, 1, 2),
    retention_rate = c(0.3, 0.2, 0.4, 0.25)
  )
  styles <- list(
    font_size = 12,
    fill_palette = list(reversescale = FALSE)
  )
  
  plot <- cohort_heatmap_plotter(df, styles)
  expect_s3_class(plot, "plotly")
})

test_that("cohort_heatmap_plotter returns empty plot for empty data", {
  df <- data.frame(
    cohort = character(),
    month_offset = numeric(),
    retention_rate = numeric()
  )
  styles <- list(
    font_size = 12,
    fill_palette = list(reversescale = FALSE)
  )
  
  plot <- cohort_heatmap_plotter(df, styles)
  expect_s3_class(plot, "plotly")
})
