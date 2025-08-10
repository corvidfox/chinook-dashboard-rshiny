# tests/testthat/test-kpi_pipeline.R

# -------------------------------
# Tests for get_metric_definitions()
# -------------------------------

test_that("get_metric_definitions returns correct structure", {
  choices <- c("Revenue (USD$)" = "revenue", "Tracks Sold" = "tracks_sold")
  defs <- get_metric_definitions(choices)
  
  expect_type(defs, "list")
  expect_equal(length(defs), 2)
  expect_equal(defs[[1]]$var_name, "revenue")
  expect_equal(defs[[1]]$label, "Revenue (USD$)")
})

# -------------------------------
# Tests for get_shared_kpis()
# -------------------------------

test_that("get_shared_kpis returns NULL if revenue_kpis is NULL", {
  result <- get_shared_kpis(
    con = con,
    tbl = "Invoice",
    metrics = get_metric_definitions(c("Revenue" = "revenue")),
    date_range = c("2014-01-01", "2014-01-01"),
    cohort_df = data.frame(),
    top_n = 5
  )
  expect_null(result)
})

