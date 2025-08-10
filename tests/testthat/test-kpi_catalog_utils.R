# tests/testthat/test-kpi_catalog_utils.R

# ---- Ensure Catalog Tables Exist ----
if (!check_catalog_tables(con)) {
  create_catalog_tables(con)
}

# -------------------------------
# Tests for query_catalog_sales()
# -------------------------------

test_that("query_catalog_sales returns expected columns for Genre", {
  result <- query_catalog_sales(
    con = con,
    tbl = "Invoice",
    group_var = "Genre",
    date_range = c("2010-01-01", "2010-03-31")
  )
  
  expect_s3_class(result, "data.frame")
  expect_true(all(c("group_val", "unique_tracks_sold", "catalog_size", "pct_catalog_sold") %in% names(result)))
})

test_that("query_catalog_sales returns expected columns for Artist", {
  result <- query_catalog_sales(
    con = con,
    tbl = "Invoice",
    group_var = "Artist",
    date_range = c("2010-01-01", "2010-03-31")
  )
  
  expect_s3_class(result, "data.frame")
  expect_true(all(c("group_val", "unique_tracks_sold", "catalog_size", "pct_catalog_sold") %in% names(result)))
})

test_that("query_catalog_sales fails with invalid group_var", {
  expect_error(
    query_catalog_sales(con, tbl = "Invoice", group_var = "Album", date_range = c("2010-01-01", "2010-03-31")),
    "must be one of"
  )
})

# -------------------------------
# Tests for enrich_catalog_kpis()
# -------------------------------

test_that("enrich_catalog_kpis joins catalog metrics onto TopN table", {
  topn_df <- data.frame(
    group_val = c("Rock", "Jazz"),
    total_revenue = c(1000, 500),
    stringsAsFactors = FALSE
  )
  
  enriched <- enrich_catalog_kpis(
    con = con,
    tbl = "Invoice",
    topn_df = topn_df,
    group_var = "Genre",
    date_range = c("2010-01-01", "2010-03-31")
  )
  
  expect_s3_class(enriched, "data.frame")
  expect_true("pct_catalog_sold" %in% names(enriched))
  expect_equal(nrow(enriched), nrow(topn_df))
})