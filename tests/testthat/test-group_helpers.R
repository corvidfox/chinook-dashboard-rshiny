# tests/testthat/test-group_helpers.R

# -------------------------------
# Tests for get_group_yearly_summary()
# -------------------------------

test_that("get_group_yearly_summary returns correct Genre summary for full range", {
  df <- get_group_yearly_summary(con, group_var = "Genre", date_range = c("2009-01-01", "2013-12-31"))
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 104)
  expect_equal(names(df), c("year", "genre", "num_customers", "num_purchases", "num_countries",
                            "tracks_sold", "revenue", "first_time_customers",
                            "unique_tracks_sold", "first_tracks_sold", "catalog_size"))
  expect_equal(df[1, "genre"], "Alternative & Punk")
  expect_equal(df[104, "genre"], "World")
})

test_that("get_group_yearly_summary returns correct Genre summary for 2010", {
  df <- get_group_yearly_summary(con, group_var = "Genre", date_range = c("2010-01-01", "2010-12-31"))
  expect_equal(nrow(df), 24)
  expect_true(all(df$year == "2010"))
  expect_equal(df[1, "genre"], "Alternative")
  expect_equal(df[24, "genre"], "World")
})

test_that("get_group_yearly_summary returns correct Artist summary for full range", {
  df <- get_group_yearly_summary(con, group_var = "Artist", date_range = c("2009-01-01", "2013-12-31"))
  expect_equal(nrow(df), 553)
  expect_equal(df[1, "artist"], "AC/DC")
  expect_equal(df[553, "artist"], "Zeca Pagodinho")
})

test_that("get_group_yearly_summary returns correct Artist summary for 2010", {
  df <- get_group_yearly_summary(con, group_var = "Artist", date_range = c("2010-01-01", "2010-12-31"))
  expect_equal(nrow(df), 113)
  expect_true(all(df$year == "2010"))
  expect_equal(df[1, "artist"], "AC/DC")
  expect_equal(df[113, "artist"], "Zeca Pagodinho")
})

test_that("get_group_yearly_summary returns empty data frame for null subset", {
  df_genre <- get_group_yearly_summary(con, group_var = "Genre", date_range = c("2014-01-01", "2014-01-01"))
  df_artist <- get_group_yearly_summary(con, group_var = "Artist", date_range = c("2014-01-01", "2014-01-01"))
  expect_equal(nrow(df_genre), 0)
  expect_equal(nrow(df_artist), 0)
})

# -------------------------------
# Tests for group_plotter()
# -------------------------------

test_that("group_plotter returns plotly object for valid Genre data", {
  df <- data.frame(
    year = rep("2010", 3),
    genre = c("Rock", "Jazz", "Pop"),
    revenue = c(100, 80, 60),
    num_purchases = c(20, 15, 12),
    tracks_sold = c(200, 150, 120),
    catalog_size = c(300, 250, 200),
    unique_tracks_sold = c(150, 120, 100)
  )
  metric <- list(var_name = "revenue", label = "Revenue")
  styles <- list(
    font_size = 12,
    fill_palette = list(reversescale = FALSE)
  )
  
  plot <- group_plotter(df, metric, group_var = "Genre", group_label = "Genre", styles = styles, max_n = 3)
  expect_s3_class(plot, "plotly")
})

test_that("group_plotter returns empty plot for empty data", {
  df <- data.frame(
    year = character(),
    genre = character(),
    revenue = numeric(),
    num_purchases = numeric(),
    tracks_sold = numeric(),
    catalog_size = numeric(),
    unique_tracks_sold = numeric()
  )
  metric <- list(var_name = "revenue", label = "Revenue")
  styles <- list(
    font_size = 12,
    fill_palette = list(reversescale = FALSE)
  )
  
  plot <- group_plotter(df, metric, group_var = "Genre", group_label = "Genre", styles = styles, max_n = 3)
  expect_s3_class(plot, "plotly")
})
