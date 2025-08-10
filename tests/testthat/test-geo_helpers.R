# tests/testthat/test-geo_helpers.R

# -------------------------------
# Tests for get_geo_metrics()
# -------------------------------

test_that("get_geo_metrics returns correct yearly summary for full range", {
  df <- get_geo_metrics(con, date_range = c("2009-01-01", "2013-12-31"), mode = "yearly")
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 101)
  expect_equal(names(df), c("year", "num_months", "country", "num_customers", "num_purchases",
                            "tracks_sold", "revenue", "first_time_customers"))
  
  # Check first row
  expect_equal(df[1, "year"], "2009")
  expect_equal(df[1, "country"], "Australia")
  expect_equal(df[1, "num_customers"], 1)
  expect_equal(df[1, "revenue"], 11.88)
  
  # Check last row
  expect_equal(df[101, "year"], "2013")
  expect_equal(df[101, "country"], "United Kingdom")
  expect_equal(df[101, "num_customers"], 3)
  expect_equal(df[101, "revenue"], 28.71)
})

test_that("get_geo_metrics returns correct yearly summary for 2010", {
  df <- get_geo_metrics(con, date_range = c("2010-01-01", "2010-12-31"), mode = "yearly")
  expect_equal(nrow(df), 20)
  expect_true(all(df$year == "2010"))
  expect_equal(df[1, "country"], "Argentina")
  expect_equal(df[1, "revenue"], 11.88)
  expect_equal(df[20, "country"], "United Kingdom")
  expect_equal(df[20, "revenue"], 30.69)
})

test_that("get_geo_metrics returns correct aggregate summary for full range", {
  df <- get_geo_metrics(con, date_range = c("2009-01-01", "2013-12-31"), mode = "aggregate")
  expect_equal(nrow(df), 24)
  expect_true(all(df$year == "All"))
  expect_true(all(is.na(df$num_months)))
  expect_equal(df[1, "country"], "Argentina")
  expect_equal(df[1, "revenue"], 37.62)
  expect_equal(df[24, "country"], "United Kingdom")
  expect_equal(df[24, "revenue"], 112.86)
})

test_that("get_geo_metrics returns correct aggregate summary for 2010", {
  df <- get_geo_metrics(con, date_range = c("2010-01-01", "2010-12-31"), mode = "aggregate")
  expect_equal(nrow(df), 20)
  expect_true(all(df$year == "All"))
  expect_equal(df[1, "country"], "Argentina")
  expect_equal(df[1, "revenue"], 11.88)
  expect_equal(df[20, "country"], "United Kingdom")
  expect_equal(df[20, "revenue"], 30.69)
})

test_that("get_geo_metrics returns empty data frame for null subset", {
  df <- get_geo_metrics(con, date_range = c("2014-01-01", "2014-01-01"), mode = "aggregate")
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 0)
})

# -------------------------------
# Tests for geo_plotter()
# -------------------------------

test_that("geo_plotter returns plotly object for valid data", {
  df <- data.frame(
    year = rep("All", 3),
    country = c("USA", "Canada", "Brazil"),
    revenue = c(100, 80, 60),
    num_customers = c(10, 8, 6),
    num_purchases = c(20, 15, 12),
    tracks_sold = c(200, 150, 120),
    first_time_customers = c(5, 4, 3)
  )
  metric <- list(var_name = "revenue", label = "Revenue")
  styles <- list(
    font_size = 12,
    color_scale = "Blues",
    border_color = "gray"
  )
  
  plot <- geo_plotter(df, metric, styles)
  expect_s3_class(plot, "plotly")
})

test_that("geo_plotter returns empty plot for empty data", {
  df <- data.frame(
    country = character(),
    revenue = numeric(),
    num_customers = numeric(),
    num_purchases = numeric(),
    tracks_sold = numeric(),
    first_time_customers = numeric()
  )
  metric <- list(var_name = "revenue", label = "Revenue")
  styles <- list(
    font_size = 12,
    color_scale = "Blues",
    border_color = "gray"
  )
  
  plot <- geo_plotter(df, metric, styles)
  expect_s3_class(plot, "plotly")
})
