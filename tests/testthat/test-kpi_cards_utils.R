# tests/testthat/test-kpi_cards_utils.R
# -------------------------------
# Tests for build_kpi()
# -------------------------------

test_that("build_kpi returns correct structure", {
  kpi <- build_kpi("Revenue", "$125K", "Total income in USD")
  expect_type(kpi, "list")
  expect_equal(kpi$label, "Revenue")
  expect_equal(kpi$value, "$125K")
  expect_equal(kpi$tooltip, "Total income in USD")
})

test_that("build_kpi handles missing tooltip", {
  kpi <- build_kpi("Users", "1,200")
  expect_null(kpi$tooltip)
})

# -------------------------------
# Tests for safe_kpi_entry()
# -------------------------------

test_that("safe_kpi_entry returns fallback for NULL, NA, or empty", {
  expect_equal(safe_kpi_entry("Revenue", NULL)$value, "No data available")
  expect_equal(safe_kpi_entry("Revenue", NA)$value, "No data available")
  expect_equal(safe_kpi_entry("Revenue", "")$value, "No data available")
})

test_that("safe_kpi_entry returns original value when present", {
  expect_equal(safe_kpi_entry("Revenue", "$100K")$value, "$100K")
})

# -------------------------------
# Tests for build_kpi_list_html()
# -------------------------------

test_that("build_kpi_list_html returns correct HTML for ordered list", {
  html <- build_kpi_list_html(c("USA", "UK"), c("$100K", "$85K"), ordered = TRUE)
  expect_match(html, "<ol")
  expect_match(html, "<li><strong>USA</strong> \\(\\$100K\\)</li>")
})

test_that("build_kpi_list_html returns correct HTML for unordered list", {
  html <- build_kpi_list_html(c("USA", "UK"), c("$100K", "$85K"), ordered = FALSE)
  expect_match(html, "<ul")
})

# -------------------------------
# Tests for format_kpi_value()
# -------------------------------

test_that("format_kpi_value formats dollar values", {
  expect_equal(format_kpi_value(1500, "dollar"), "$1,500.00")
})

test_that("format_kpi_value formats percent values", {
  expect_equal(format_kpi_value(0.42, "percent"), "42.00%")
})

test_that("format_kpi_value formats float and number", {
  expect_equal(format_kpi_value(1500.5, "float"), "1,500.50")
  expect_equal(format_kpi_value(1500, "number"), "1,500")
})

test_that("format_kpi_value formats country codes", {
  expect_match(format_kpi_value("USA", "country"), "🇺🇸")
})

test_that("format_kpi_value returns 'NA' for NA input", {
  expect_equal(format_kpi_value(NA, "dollar"), "NA")
})
