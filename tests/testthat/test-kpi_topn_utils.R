# tests/testthat/test-kpi_topn_utils.R

# -------------------------------
# Tests for topn_kpis_build_full_table()
# -------------------------------

test_that("topn_kpis_build_full_table returns a data.frame for Genre", {
  df <- topn_kpis_build_full_table(
    con = con,
    group_var = "Genre",
    tbl = "Invoice",
    date_range = c("2010-01-01", "2010-03-31")
  )
  expect_s3_class(df, "data.frame")
  expect_true("group_val" %in% names(df))
})

test_that("topn_kpis_build_full_table fails with invalid group_var", {
  expect_error(
    topn_kpis_build_full_table(con, group_var = "Album", tbl = "Invoice"),
    "must be one of"
  )
})

# -------------------------------
# Tests for topn_kpis_slice_topn()
# -------------------------------

test_that("topn_kpis_slice_topn returns top-N rows sorted by metric", {
  df <- data.frame(
    group_val = c("A", "B", "C"),
    revenue = c(100, 300, 200),
    num_customers = c(10, 20, 15)
  )
  top <- topn_kpis_slice_topn(df, metric_var = "revenue", n = 2)
  expect_equal(nrow(top), 2)
  expect_equal(top$group_val, c("B", "C"))
})

# -------------------------------
# Tests for topn_kpis_generate()
# -------------------------------

test_that("topn_kpis_generate returns named list of top-N tables", {
  df <- data.frame(
    group_val = c("A", "B", "C"),
    revenue = c(100, 300, 200),
    num_customers = c(10, 20, 15),
    num_purchases = c(5, 10, 8),
    tracks_sold = c(50, 100, 80)
  )
  metrics <- list(
    revenue = list(var_name = "revenue"),
    customers = list(var_name = "num_customers")
  )
  result <- topn_kpis_generate(df, metrics, n = 2)
  expect_type(result, "list")
  expect_named(result, c("revenue", "num_customers"))
  expect_equal(nrow(result$revenue), 2)
})

# -------------------------------
# Tests for topn_kpis_format_display()
# -------------------------------

test_that("topn_kpis_format_display returns formatted columns", {
  df <- data.frame(
    group_val = c("Rock", "Jazz"),
    revenue = c(1000, 500),
    num_customers = c(100, 50),
    num_purchases = c(200, 100),
    tracks_sold = c(1000, 500)
  )
  formatted <- topn_kpis_format_display(df, group_var = "Genre", total_rev = 1500)
  expect_s3_class(formatted, "data.frame")
  expect_true("avg_revenue_per_cust_fmt" %in% names(formatted))
  expect_true("revenue_share_fmt" %in% names(formatted))
})

test_that("topn_kpis_format_display handles BillingCountry formatting", {
  df <- data.frame(
    group_val = c("USA", "Canada"),
    revenue = c(1000, 500),
    num_customers = c(100, 50),
    num_purchases = c(200, 100),
    tracks_sold = c(1000, 500)
  )
  formatted <- topn_kpis_format_display(df, group_var = "BillingCountry", total_rev = 1500)
  expect_true(all(grepl("🇺🇸|🇨🇦", formatted$group_val)))
})
