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
# Base & Shiny Ecosystem
library(bslib)
library(bsicons)
library(bsplus)
library(cachem)
library(memoise)
library(rlang)
library(shiny)
library(shinyWidgets)
library(shinydashboard)

# Data Manipulation
library(countrycode)
library(dplyr)
library(glue)
library(lubridate)
library(scales)
library(tibble)
library(tidyr)
library(purrr)

# Visualizations
library(markdown)
library(ggplot2)
library(plotly)

# Database
library(DBI)
library(duckdb)

# ---- Logging ----
enable_logging <- Sys.getenv("SHINY_ENV") != "production"

log_msg <- function(msg, cond = TRUE) {
  if (exists("enable_logging", inherits = TRUE) && enable_logging && cond) {
    message(msg)
  }
}

# ---- Cache with Size Cap ----
options(shiny.reactlog = enable_logging)
# 50 MB
shared_cache <- cachem::cache_mem(max_size = 50 * 1024^2)
shiny::shinyOptions(cache = shared_cache)

# ---- Connect to Chinook DuckDB Database ----
# Uses embedded DuckDB; requires relative path resolution
con <- DBI::dbConnect(
  duckdb::duckdb(), dbdir = "data/chinook.duckdb", read_only = TRUE
  )

if (exists("con") && DBI::dbIsValid(con)) {
  log_msg("✅ Successfully connected to duckDb.")
}

# Close DB connection when app stops
shiny::onStop(function() {
  try({
    DBI::dbDisconnect(con, shutdown = TRUE)
    log_msg("Database connection closed.")
  })
})

# ---- Load and Validate Helper Functions from /helpers ----
helper_files <- list.files("helpers", full.names = TRUE)

for (f in helper_files) {
  tryCatch({
    source(f, local = TRUE)
    log_msg(sprintf("✅ Loaded: %s", f))
  }, error = function(e) {
    log_msg(sprintf("❌ Error in %s:\n  %s", f, e$message))
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
    log_msg(sprintf("✅ Loaded: %s", f))
  }, error = function(e) {
    log_msg(sprintf("❌ Error in %s:\n  %s", f, e$message))
  })
}

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

# ---- Pull Last Commit (Update Date) ----
last_commit <- tryCatch(
  readRDS("data/last_commit.rds"),
  error = function(e) NA
)
