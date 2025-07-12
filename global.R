#' @file global.R
#' @title Global Initialization for Chinook BI Dashboard
#'
#' Loads required packages, helper scripts, and modules. Establishes a 
#' persistent connection to the DuckDB instance of the Chinook dataset. 
#' Initializes global variables and constants used throughout the app 
#' (e.g., filter choices).
#'
#' This script is sourced once at app startup before UI and server 
#' environments are created.
#'
#' @seealso ui.R, server.R

# ---- Load Required Libraries ----
# TO DO: Alphabetize
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(bslib)
library(bsicons)
library(bsplus)
library(rlang)

library(DBI)
library(duckdb)

library(dplyr)
library(tidyr)
library(glue)
library(lubridate)
library(scales)
library(countrycode)

library(plotly)
library(ggplot2)

# ---- Load and Validate Helper Functions from /helpers ----
helper_files <- list.files("helpers", full.names = TRUE)

for (f in helper_files) {
  tryCatch({
    source(f, local = TRUE)
    message(sprintf("✅ Loaded: %s", f))
  }, error = function(e) {
    message(sprintf("❌ Error in %s:\n  %s", f, e$message))
  })
}

# ---- Load and Validate UI and Server Modules from /modules ----
module_files <- list.files(
  path = "modules",
  pattern = "\\.R$",
  full.names = TRUE,
  recursive = TRUE
)

for (f in module_files) {
  tryCatch({
    source(f, local = TRUE)
    message(sprintf("✅ Loaded: %s", f))
  }, error = function(e) {
    message(sprintf("❌ Error in %s:\n  %s", f, e$message))
  })
}

# ---- Logging ---
enable_logging <- Sys.getenv("SHINY_ENV") != "production"

# ---- Connect to Chinook DuckDB Database ----
# Uses embedded DuckDB; requires relative path resolution
con <- DBI::dbConnect(duckdb::duckdb(), dbdir = "data/chinook.duckdb", read_only = TRUE)

# Close DB connection when app stops
shiny::onStop(function() {
  try({
    DBI::dbDisconnect(con, shutdown = TRUE)
    message("Database connection closed.")
  })
})

# ---- Prepare Global Filter Values ----
## Pull from DB Meta Values
filter_meta <- get_filter_meta(con)
min_date <- as.Date(filter_meta$date_range$min_date)
max_date <- as.Date(filter_meta$date_range$max_date)

genre_choices   <- filter_meta$genre_choices
artist_choices  <- filter_meta$artist_choices
country_choices <- filter_meta$country_choices

## Metric Choices
metric_choices <- c(
  "Revenue (USD$)" = "revenue",
  "Number of Customers" = "num_customers",
  "First-Time Customers" = "first_time_customers",
  "Number of Purchases" = "num_purchases",
  "Tracks Sold" = "tracks_sold"
)
