# tests/testthat/test-metadata_utils.R

# -------------------------------
# Tests for get_filter_meta()
# -------------------------------

test_that("get_filter_meta returns expected keys and types", {
  meta <- get_filter_meta(con)
  expect_type(meta, "list")
  expect_true(all(c("date_range", "genre_choices", "artist_choices", "country_choices") %in% names(meta)))
  expect_s3_class(meta$date_range, "data.frame")
  expect_type(meta$genre_choices, "character")
  expect_type(meta$artist_choices, "character")
  expect_type(meta$country_choices, "character")
})

# -------------------------------
# Tests for make_static_summary_table()
# -------------------------------

test_that("make_static_summary_table returns datatable with valid input", {
  kpis <- list(
    purchase_kpis = list(total_orders = 100, total_tracks = 500),
    customer_kpis = list(total_customers = 50),
    revenue_kpis = list(total_revenue = 12345.67)
  )
  meta <- get_filter_meta(con)
  dt <- make_static_summary_table(kpis, meta)
  expect_s3_class(dt, "datatables")
})

test_that("make_static_summary_table handles NULL kpis gracefully", {
  dt <- make_static_summary_table(NULL, list())
  expect_s3_class(dt, "datatables")
})

# -------------------------------
# Tests for create_catalog_tables() and check_catalog_tables()
# -------------------------------

test_that("create_catalog_tables creates expected temp tables", {
  create_catalog_tables(con)
  tables <- DBI::dbListTables(con)
  expect_true("genre_catalog" %in% tables)
  expect_true("artist_catalog" %in% tables)
})

test_that("check_catalog_tables returns TRUE when tables exist", {
  create_catalog_tables(con)
  expect_true(check_catalog_tables(con))
})

test_that("check_catalog_tables returns FALSE when tables are missing", {
  DBI::dbExecute(con, "DROP TABLE IF EXISTS genre_catalog")
  DBI::dbExecute(con, "DROP TABLE IF EXISTS artist_catalog")
  expect_false(check_catalog_tables(con))
})
