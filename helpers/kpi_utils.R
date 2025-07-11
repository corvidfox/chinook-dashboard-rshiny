#' @file kpi_utils.R
#' @title KPI Card UI Builders and Layout Helpers
#'
#' @description
#' This module contains helper functions for constructing responsive,
#' Bootstrap-themed KPI cards used across Shiny dashboard modules.
#' It supports light/dark theming, tooltip-augmented entries,
#' conditional rendering for missing KPI states, and dynamic layout
#' formatting for multi-line card content.
#'
#' Functions include:
#'
#' - `build_kpi()`: Creates a single KPI entry (label, value, tooltip)
#' - `render_kpi_card()`: Renders a complete Bootstrap-style KPI card
#' - `safe_kpi_card()`: Wraps card rendering with fallback logic
#' - `build_kpi_list_html()`: Generates HTML lists from KPI label/value pairs
#' - `safe_kpi_entry()`: Formats a single KPI line with fallback protection
#' - `format_kpi_value()`: Formats raw numeric KPI values into consistent 
#'   currency, percent, number, or country-label strings for display
#'
#' These are used across modules to render formatted KPI metrics with
#' semantic structure and consistent styling.
#'
#' @seealso [format_kpi_value()], [generate_styles()], [bsplus::bs_embed_tooltip()]
#' @keywords internal

#' Build a KPI Entry for Card Display
#'
#' Returns a named list representing a single KPI item. Each entry includes
#' a label (metric name), a formatted value, and an optional tooltip string.
#'
#' @param label A character string describing the metric.
#' @param value A character or numeric value to display.
#' @param tooltip Optional text for hover tooltip.
#'
#' @return Named list with elements: `label`, `value`, and optional `tooltip`.
#' @examples
#' build_kpi("Revenue", "$125K", "Total income in USD")
build_kpi <- function(label, value, tooltip = NULL) {
  stopifnot(is.character(label), !is.null(value))
  
  if (exists("enable_logging", inherits = TRUE) && enable_logging) {
    message(glue::glue("[KPI] Building KPI entry: {label}"))
  }
  
  list(label = label, value = value, tooltip = tooltip)
}

#' Render a Thematic KPI Card
#'
#' Constructs a Bootstrap-themed UI card showing multiple KPI metrics.
#' Includes optional icon, tooltip, and dynamic style support.
#'
#' @param title String shown as card header.
#' @param kpi_list List of KPI entries (built with `build_kpi()`).
#' @param icon Optional icon or HTML tag prefix.
#' @param tooltip Optional tooltip for card header.
#' @param styles Optional list from `generate_styles()` for color theming.
#' @param height CSS string controlling card height (default: `"130px"`).
#'
#' @return A Shiny UI card object.
render_kpi_card <- function(
    title,
    kpi_list,
    icon = NULL,
    tooltip = NULL,
    styles = NULL,
    height = "130px"
) {
  if (exists("enable_logging", inherits = TRUE) && enable_logging) {
    message(glue::glue("[KPI] Rendering KPI card: {title}"))
  }
  
  stopifnot(is.character(title), is.list(kpi_list) || is.null(kpi_list))
  
  if (is.null(kpi_list) || length(kpi_list) == 0) {
    message("[KPI] No KPI data available. Rendering fallback.")
    kpi_list <- list(list(label = "Status", value = "No data available"))
  }
  
  # Style resolution
  bg_color        <- styles$kpi_bg        %||% "var(--bs-secondary-bg)"
  text_color      <- styles$text_color    %||% "#000"
  secondary_color <- styles$secondary_color %||% "#666"
  
  # Header block
  card_header <- if (!is.null(tooltip)) {
    bsplus::bs_embed_tooltip(
      bslib::card_header(HTML(paste0(icon, " ", title))),
      title     = tooltip,
      placement = "top",
      trigger   = "hover"
    )
  } else {
    bslib::card_header(HTML(paste0(icon, " ", title)))
  }
  
  # KPI entry display elements
  body_items <- lapply(kpi_list, function(kpi) {
    content <- div(
      HTML(paste0("<strong>", kpi$label, ":</strong> ", kpi$value)),
      style = paste0(
        "font-size: 1em; margin-bottom: 0.4em; color:", text_color
      )
    )
    if (!is.null(kpi$tooltip)) {
      bsplus::bs_embed_tooltip(
        content,
        title     = kpi$tooltip,
        placement = "top",
        trigger   = "hover"
      )
    } else {
      content
    }
  })
  
  # Final card output
  bslib::card(
    full_screen = FALSE,
    style = paste0(
      "min-height:", height, ";",
      " flex: 1;",
      " display: flex;",
      " flex-direction: column;",
      " justify-content: center;",
      " background-color:", bg_color, ";",
      " border-radius: 6px;"
    ),
    card_header,
    bslib::card_body(tagList(body_items))
  )
}

#' Conditionally Render a KPI Card with Fallback
#'
#' Safely renders a KPI card by checking if the KPI data is available.
#' If KPIs are NULL (e.g. due to no query result), displays a fallback
#' message instead. This is commonly used in dashboard modules where
#' filtering may return zero results.
#'
#' @param kpis A list of KPIs (may be NULL).
#' @param body_fn Function that returns a list of KPI entries via `build_kpi()`.
#' @param title Title shown on the card.
#' @param icon Optional icon HTML.
#' @param tooltip Optional tooltip string.
#' @param styles Optional style list from `generate_styles()`.
#'
#' @return A KPI card UI object.
#' @keywords internal
safe_kpi_card <- function(
    kpis, body_fn, title, icon = NULL, tooltip = NULL, styles = NULL
) {
  if (exists("enable_logging", inherits = TRUE) && enable_logging) {
    message(glue::glue("[KPI] Building safe card for: {title}"))
  }
  
  if (is.null(kpis)) {
    return(render_kpi_card(
      kpi_list = NULL,
      title    = title,
      icon     = icon,
      tooltip  = tooltip,
      styles   = styles
    ))
  }
  
  kpi_list <- body_fn()
  
  render_kpi_card(
    kpi_list = kpi_list,
    title    = title,
    icon     = icon,
    tooltip  = tooltip,
    styles   = styles
  )
}

#' Generate HTML List for KPI Card Display
#'
#' Converts KPI label/value pairs into a formatted HTML string suitable
#' for use inside `build_kpi()` values. Supports ordered (`<ol>`) or
#' unordered (`<ul>`) lists.
#'
#' @param labels Character vector of label strings.
#' @param values Character vector of value strings. Must be same length.
#' @param ordered Logical. If TRUE, creates `<ol>`; else `<ul>`.
#'
#' @return A character string containing HTML-formatted list.
#' @examples
#' build_kpi_list_html(c("USA", "UK"), c("$100K", "$85K"))
#'
#' @keywords internal
build_kpi_list_html <- function(labels, values, ordered = TRUE) {
  stopifnot(length(labels) == length(values))
  tag <- if (ordered) "ol" else "ul"
  
  entries <- paste0(
    "<li><strong>", labels, "</strong> (", values, ")</li>"
  )
  
  html <- glue::glue(
    "<{tag} style='margin: 0; padding-left: 1rem;'>\n{paste(entries, collapse = '\n')}\n</{tag}>"
  )
  
  return(html)
}

#' Build Single KPI Entry with Fallback for Missing Values
#'
#' Creates a KPI line that defaults to `"No data available"` when the
#' value is NULL, NA, or an empty string. Ensures cards stay readable
#' and consistent in cases of missing input.
#'
#' @param label A string describing the KPI metric.
#' @param value A string or numeric value (can be NULL).
#' @param tooltip Optional tooltip to include.
#'
#' @return A KPI list item for `render_kpi_card()`.
#' @examples
#' safe_kpi_entry("Revenue", NULL, "Total USD income")
#'
#' @keywords internal
safe_kpi_entry <- function(label, value, tooltip = NULL) {
  val <- if (is.null(value) || value == "" || is.na(value)) {
    "No data available"
  } else {
    value
  }
  
  build_kpi(label = label, value = val, tooltip = tooltip)
}

#' Format KPI Value for Display
#'
#' Formats numeric KPI values (e.g., dollars, percents, floats) or 
#' country identifiers for consistent display across UI components. 
#' `"country"` formatting uses Unicode flag emoji and optional labels.
#'
#' Type `"number"` assumes whole values and trims decimals. If a 
#' numeric input has non-zero decimal precision, `"float"` can 
#' be used to display it explicitly.
#'
#' @param value A numeric value or country name/code.
#' @param type One of `"dollar"`, `"percent"`, `"number"`, `"float"`, 
#'   `"country"`.
#' @param accuracy Numeric. Controls rounding (default: 0.01).
#' @param prefix Optional. Symbol prefix for `"dollar"` type 
#'   (default: `"$"`).
#' @param label Logical. Only for `"country"` type. Show label? 
#'   Default: TRUE.
#' @param label_type One of `"name"` or `"iso3"` (for `"country"` only).
#'
#' @return A formatted string for display.
#'
#' @examples
#' format_kpi_value(1500, "dollar")         # "$1,500.00"
#' format_kpi_value(0.42, "percent")        # "42.00%"
#' format_kpi_value(1500, "number")         # "1,500"
#' format_kpi_value(1500.5, "float")        # "1,500.50"
#' format_kpi_value("USA", "country")       # "🇺🇸 United States"
#'
#' @export
format_kpi_value <- function(
    value,
    type = "number",
    accuracy = 0.01,
    prefix = "$",
    label = TRUE,
    label_type = "name"
) {
  type <- rlang::arg_match(
    type, c("dollar", "percent", "number", "float", "country")
  )
  
  # Return blank for NA or non-numeric input
  if (type != "country" && (!is.numeric(value) || all(is.na(value)))) {
    return(rep("", length(value)))
  }
  
  # Format only non-NA values
  formatted <- rep("", length(value))
  non_na_idx <- which(!is.na(value))
  
  switch(type,
         percent = {
           formatted[non_na_idx] <- scales::percent(
             value[non_na_idx], accuracy = accuracy, trim = TRUE
           )
         },
         dollar = {
           formatted[non_na_idx] <- scales::dollar(
             value[non_na_idx], accuracy = accuracy, 
             prefix = prefix, trim = TRUE
           )
         },
         float = {
           formatted[non_na_idx] <- scales::comma(
             value[non_na_idx], accuracy = accuracy, trim = TRUE
           )
         },
         number = {
           formatted[non_na_idx] <- scales::comma(
             value[non_na_idx],
             accuracy = ifelse(value[non_na_idx] %% 1 == 0, 1, accuracy),
             trim = TRUE
           )
         },
         country = {
           formatted <- flagify_country(
             value, label = label, label_type = label_type
           )
         }
  )
  
  return(formatted)
}
