# tests/testthat/test-style_utils.R

# -------------------------------
# Tests for standardize_country_to_iso3()
# -------------------------------

test_that("standardize_country_to_iso3 returns correct ISO3 codes", {
  # Common country names
  expect_equal(standardize_country_to_iso3("United States"), "USA")
  expect_equal(standardize_country_to_iso3("United Kingdom"), "GBR")
  
  # Already in ISO3 format
  expect_equal(standardize_country_to_iso3("USA"), "USA")
  
  # Edge case: ambiguous or non-existent country
  expect_true(is.na(standardize_country_to_iso3("Atlantis")))
  
  # Abbreviations
  expect_equal(standardize_country_to_iso3("UK"), "GBR")
})

# -------------------------------
# Tests for flagify_country()
# -------------------------------

test_that("flagify_country returns correct emoji for ISO3 codes", {
  expect_equal(flagify_country("USA"), "🇺🇸")
  expect_equal(flagify_country("GBR"), "🇬🇧")
})

test_that("flagify_country returns emoji with label when requested", {
  expect_match(flagify_country("USA", label = TRUE), "United States")
  expect_match(flagify_country("GBR", label = TRUE), "United Kingdom")
})

test_that("flagify_country returns ISO3 label when label_type is 'iso3'", {
  expect_match(flagify_country("USA", label = TRUE, label_type = "iso3"), "USA")
  expect_match(flagify_country("GBR", label = TRUE, label_type = "iso3"), "GBR")
})

# -------------------------------
# Tests for generate_styles()
# -------------------------------

test_that("generate_styles returns expected structure for light mode", {
  styles <- generate_styles("light")
  
  normalize_hex <- function(hex) {
    rgb_vals <- grDevices::col2rgb(hex)
    rgb(rgb_vals[1], rgb_vals[2], rgb_vals[3], maxColorValue = 255)
  }
  
  expect_type(styles, "list")
  expect_true(all(
    c("line_color", "background_color", "font_size") %in% names(styles)
    ))
  expect_equal(normalize_hex(styles$background_color), "#FFFFFF")
  expect_equal(styles$font_size, 14)
})

test_that("generate_styles returns expected structure for dark mode", {
  styles <- generate_styles("dark")
  
  normalize_hex <- function(hex) {
    rgb_vals <- grDevices::col2rgb(hex)
    rgb(rgb_vals[1], rgb_vals[2], rgb_vals[3], maxColorValue = 255)
  }
  
  expect_type(styles, "list")
  expect_equal(normalize_hex(styles$background_color), "#222222")
  expect_equal(styles$font_size, 14)
})

test_that("generate_styles throws error for invalid mode", {
  expect_error(generate_styles("neon"), "Invalid theme mode")
})
