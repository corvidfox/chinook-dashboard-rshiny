# tests/testthat/test-sql_filter_utils.R

# -------------------------------
# Tests for form_where_clause()
# -------------------------------

test_that("form_where_clause returns empty SQL when no filters are applied", {
  sql <- form_where_clause(.con = con)
  expect_true(inherits(sql, "SQL"))
  expect_equal(as.character(sql), "")
})

test_that("form_where_clause returns correct SQL for date filter only", {
  date_range <- as.Date(c("2010-01-01", "2010-03-31"))
  sql <- form_where_clause(date_range = date_range, .con = con)
  expect_match(as.character(sql), "WHERE DATE\\(i.InvoiceDate\\) BETWEEN DATE\\(")
})

test_that("form_where_clause returns SQL with multiple filters", {
  sql <- form_where_clause(
    date_range = as.Date(c("2010-01-01", "2010-03-31")),
    country = c("USA", "Canada"),
    genre = c("Rock"),
    artist = c("AC/DC"),
    .con = con
  )
  sql_text <- as.character(sql)
  expect_match(sql_text, "WHERE")
  expect_match(sql_text, "i.BillingCountry IN")
  expect_match(sql_text, "g.Name IN")
  expect_match(sql_text, "ar.Name IN")
})

# -------------------------------
# Tests for apply_date_filter()
# -------------------------------

test_that("apply_date_filter returns JOIN clause without date filter", {
  sql <- apply_date_filter(.con = con)
  expect_true(inherits(sql, "SQL"))
  expect_match(as.character(sql), "JOIN Invoice i ON i.InvoiceId = e.InvoiceId$")
})

test_that("apply_date_filter returns JOIN clause with date filter", {
  date_range <- as.Date(c("2010-01-01", "2010-03-31"))
  sql <- apply_date_filter(date_range = date_range, .con = con)
  expect_match(as.character(sql), "JOIN Invoice i ON i.InvoiceId = e.InvoiceId AND DATE\\(i.InvoiceDate\\)")
})

# -------------------------------
# Tests for malformed inputs
# -------------------------------

test_that("form_where_clause handles empty vectors safely", {
  sql <- form_where_clause(
    country = character(0),
    genre = character(0),
    artist = character(0),
    .con = con
  )
  expect_true(inherits(sql, "SQL"))
  expect_equal(as.character(sql), "")
})

test_that("form_where_clause handles NULL filters safely", {
  sql <- form_where_clause(
    date_range = NULL,
    country = NULL,
    genre = NULL,
    artist = NULL,
    .con = con
  )
  expect_true(inherits(sql, "SQL"))
  expect_equal(as.character(sql), "")
})

# -------------------------------
# Tests for SQL injection attempts
# -------------------------------

test_that("form_where_clause escapes potentially malicious input", {
  malicious_country <- "'; DROP TABLE Invoice; --"
  sql <- form_where_clause(
    country = malicious_country,
    .con = con
  )
  sql_text <- as.character(sql)
  expect_true(inherits(sql, "SQL"))
  expect_match(sql_text, "i.BillingCountry IN")
  expect_match(sql_text, "'[^']*DROP TABLE[^']*'")
})

test_that("form_where_clause escapes quotes and special characters", {
  weird_artist <- "O'Reilly"
  sql <- form_where_clause(
    artist = weird_artist,
    .con = con
  )
  sql_text <- as.character(sql)
  expect_true(inherits(sql, "SQL"))
  expect_match(sql_text, "ar.Name IN")
  expect_false(grepl("O'Reilly", sql_text, fixed = TRUE))  # Should be escaped
})

