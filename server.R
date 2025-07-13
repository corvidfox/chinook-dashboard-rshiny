#' @file server.R
#' @title Server Logic for Chinook BI Dashboard
#'
#' Contains server-side logic for theming, filter handling, dataset summary 
#' rendering, and module initialization. Bridges UI inputs with dynamic 
#' outputs using reactivity.
#'
#' @section Responsibilities:
#' - Compute and render static sidebar summary
#' - Dynamically inject themed CSS via `generate_css()`
#' - Handle theme toggling and color reactivity
#' - Clear user-selected filters
#' - Bundle filters and apply them to modular panels (e.g. `ts_server()`)
#'
#' @seealso \code{\link{generate_styles}}, \code{\link{generate_css}}, 
#' \code{\link{ts_server}}
function(input, output, session) {

  ## ---- Theming: Light/Dark Toggle ----
  theme_mode <- shiny::reactive(input$theme_switcher)
  theme_debounced <- shiny::debounce(theme_mode, 300)
  
  shiny::observe({
    if (isTRUE(theme_debounced())) {
      session$setCurrentTheme(theme_dark)
    } else {
      session$setCurrentTheme(theme_light)
    }
  })

  ## ---- Inject Theme-Aware CSS ----
  output$dynamic_styles <- shiny::renderUI({
    styles <- if (theme_debounced() == "dark") theme_dark else theme_light

    tags$style(shiny::HTML(generate_css(styles)))
  })

  ## ---- Generate Style Parameters for Plots/Cards ----
  styles <- shiny::reactive({
    mode <- theme_debounced()
    generate_styles(mode)
  })

  ## ---- Clear Filters ----
  shiny::observeEvent(input$clear_filters, {
    shinyWidgets::updateAirDateInput(
      session,
      inputId = "date_range",
      value = c(min_date, max_date)
    )
    shiny::updateSelectizeInput(
      session, "genre", selected = NULL, server = TRUE
    )
    shiny::updateSelectizeInput(
      session, "artist", selected = NULL, server = TRUE
    )
    shiny::updateSelectizeInput(
      session, "country", selected = NULL, server = TRUE
    )
    shiny::updateSelectInput(session, "metric", selected = "revenue")
  })
  
  ## ---- Reactive Filters ----
  
  # Sanitize Date Range (Whole Months)
  date_range_clean <- shiny::reactive({
    req(input$date_range)

    start_date <- lubridate::floor_date(as.Date(min(input$date_range)), "month")
    end_date   <- lubridate::ceiling_date(
      as.Date(max(input$date_range)),
      "month"
      ) - lubridate::days(1)

    c(start_date, end_date)
  })

  # Dynamic SQL WHERE clause, based on active filters
  where_clause_reactive <- shiny::reactive({
    form_where_clause(
      country = input$country,
      genre = input$genre,
      artist = input$artist,
      .con = con
    )
  })

  # Pull Shared Events Table (CACHED)
  events_shared <- shiny::reactive({
    get_events_shared(con = con, where_clause_reactive())
  }) %>%
    shiny::bindCache(where_clause_reactive())

  # Push Shared Events Table to Temp DuckDB for Memory Management
  shiny::observeEvent(events_shared(), {
    DBI::dbWriteTable(
      con = con,
      name = "filtered_invoices",
      value = events_shared(),
      overwrite = TRUE,
      temporary = TRUE
    )
  })

  create_catalog_tables(con)
  check_catalog_tables(con)

  # Pull Cohort Retention Events Table
  cohort_df_shared <- shiny::reactive({
    # Ensure reactivity to "filtered_invoices" changes
    invisible(where_clause_reactive())

    get_retention_cohort_data(
      con = con,
      tbl = "filtered_invoices",
      date_range = as.character(date_range_clean()),
      max_offset = NULL
    )
  }) %>%
    shiny::bindCache(events_shared(), date_range_clean())

  # Parse Metrics & Labels for KPI Pipeline
  metrics_parsed <- get_metric_definitions(metric_choices)

  # Pull Shared KPIs (CACHED)
  kpis_shared <- shiny::reactive({
    cohort_df_shared <- cohort_df_shared()
    get_shared_kpis(
      con = con,
      tbl = "filtered_invoices",
      metrics = metrics_parsed,
      date_range = date_range_clean(),
      top_n = 5,
      cohort_df = cohort_df_shared,
      offsets = c(3, 6, 9)
      )
  }) %>%
    shiny::bindCache(events_shared(), date_range_clean())

  ## ---- Static Values: Full-Set KPIs and Meta Summary for Sidebar ----
  kpis_full <- shiny::reactive({
    cohort_df_shared <- cohort_df_shared()
    get_shared_kpis(
      con,
      tbl = "Invoice",
      metrics = metrics_parsed,
      date_range = NULL,
      top_n = 5,
      cohort_df = cohort_df_shared,
      offsets = c(3, 6, 9)
    )
  }) |>
    shiny::bindCache("static_kpis")

  output$static_summary <- DT::renderDataTable({
    make_static_summary_table(kpis_full(), filter_meta)
  })

  create_catalog_tables(con)
  check_catalog_tables(con)

  ## ---- Reactives for Additional Filters, For Modules ----

  metric <- shiny::reactive({
    list(
      var_name = input$metric,
      label = names(metric_choices)[metric_choices == input$metric]
      )
  })

  ## ---- Module Mounting ----
  ts_server(
    "ts", con = con, events_shared = events_shared, kpis_shared = kpis_shared,
    metric = metric, date_range = date_range_clean, styles = styles
    )
  geo_server(
    "geo", con = con, events_shared = events_shared,
    kpis_shared = kpis_shared, metric = metric, date_range = date_range_clean,
    styles = styles
    )
  group_server(
    "genre", con = con, events_shared = events_shared,
    kpis_shared = kpis_shared, metric = metric, date_range = date_range_clean,
    styles = styles, group_var = "Genre", group_label = "Genres", max_n = 15
    )
  group_server(
    "artist", con = con, events_shared = events_shared,
    kpis_shared = kpis_shared, metric = metric, date_range = date_range_clean,
    styles = styles, group_var = "Artist", group_label = "Artists", max_n = 15
    )
  retention_server(
    "retention", con = con, events_shared = events_shared,
    cohort_df_shared = cohort_df_shared, kpis_shared = kpis_shared,
    date_range = date_range_clean, styles = styles, max_offset = NULL
  )
  insights_server("insights", kpis_static = kpis_full, kpis_subset = kpis_shared, styles = styles)
}
