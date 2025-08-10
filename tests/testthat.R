# tests/testthat.R

# -----------------------------------------------
# Master Test Runner for Chinook Dashboard App
# -----------------------------------------------
# This script sets up and runs all unit tests for
# helper functions and modules in a non-package
# Shiny app structure.
#
# To run all tests, simply source this file from
# the project root:
#   source("tests/testthat.R")
#
# Since the app depends on a DuckDB connection 
# established in `global.R`, which also imports
# all helper files, it is the single import.
# -----------------------------------------------

# Load testthat package
library(testthat)

# Source `global.R` to establish the DuckDB connection
source("global.R")

# Set up required temporary tables
create_catalog_tables(con)

DBI::dbWriteTable(
  con = con,
  name = "filtered_invoices",
  value = get_events_shared(con, DBI::SQL("")),
  overwrite = TRUE,
  temporary = TRUE
)

# -----------------------------------------------
# Run all test files in the tests/testthat directory
# -----------------------------------------------
# This will automatically discover and run all test-*.R files

test_dir("tests/testthat")

# -----------------------------------------------
# Ensure safe disconnect from DB
# -----------------------------------------------
# Check for a common CI/CD environment variable
is_ci <- Sys.getenv("CI") == "true"

# If not in CI/CD environment and connection still exists, disconnect safely
if (is_ci && exists("con", inherits = TRUE) && DBI::dbIsValid(con)) {
  try({
    DBI::dbDisconnect(con, shutdown = TRUE)
    message("✅ DuckDB connection closed in CI/CD.")
  }, silent = TRUE)
}

