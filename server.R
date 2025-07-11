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
  
  ## ---- Static Values: Full-Set KPIs and Meta Summary for Sidebar ----
  kpis_full <- shiny::reactive({
    get_shared_kpis(con, tbl = "Invoice", date_range = NULL)
  }) |> 
    shiny::bindCache("static_kpis")
  
  output$static_summary <- DT::renderDataTable({
    make_static_summary_table(kpis_full(), filter_meta)
  })
  
  create_catalog_tables(con)
  check_catalog_tables(con)
  
  ## ---- Theming: Light/Dark Toggle ----
  shiny::observe({
    if (isTRUE(input$theme_switcher)) {
      session$setCurrentTheme(theme_dark)
    } else {
      session$setCurrentTheme(theme_light)
    }
  })
  
  ## ---- Inject Theme-Aware CSS ----
  output$dynamic_styles <- shiny::renderUI({
    styles <- if (input$theme_switcher == "dark") theme_dark else theme_light
    
    tags$style(shiny::HTML(generate_css(styles)))
  })
  
  ## ---- Generate Style Parameters for Plots/Cards ----
  styles <- shiny::reactive({
    mode <- input$theme_switcher
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
  date_range <- shiny::reactive({
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
  
  # Pull Shared KPIs (CACHED)
  kpis_shared <- shiny::reactive({
    get_shared_kpis(
      con = con, 
      tbl = "filtered_invoices", 
      date_range = input$date_range
      )
  }) %>%
    shiny::bindCache(where_clause_reactive())
  
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
    metric = metric, date_range = date_range, styles = styles
    )
  geo_server(
    "geo", con = con, events_shared = events_shared, 
    kpis_shared = kpis_shared, metric = metric, date_range = date_range, 
    styles = styles
    )
  group_server(
    "genre", con = con, events_shared = events_shared, 
    kpis_shared = kpis_shared, metric = metric, date_range = date_range, 
    styles = styles, group_var = "Genre", group_label = "Genres", max_n = 15
    )
  group_server(
    "artist", con = con, events_shared = events_shared, 
    kpis_shared = kpis_shared, metric = metric, date_range = date_range, 
    styles = styles, group_var = "Artist", group_label = "Artists", max_n = 15
    )
  #retention_server("retention", con = con, filters = filter_inputs, styles = styles)
  #insights_server("insights", styles = styles)
}
