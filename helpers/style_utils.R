#' @file style_utils.R
#' @title Thematic and Plot Styling Utilities for UI and KPI Formatting
#'
#' @description
#' This module centralizes theming, plot styling, and visual formatting 
#' functions for the Shiny app. It is designed to support Bootstrap 5 themes 
#' via `{bslib}`, responsive visual coherence across light/dark modes, and 
#' consistent presentation of KPIs, country data, and interactive charts.
#'
#' Key components include:
#'
#' - `theme_light` / `theme_dark`: Bootstrap theme declarations for global UI
#' - `generate_styles()`: Extracts style parameters (colors, fonts, etc.) from 
#'   the active theme mode for reuse across modules
#' - `generate_css()`: Produces inline CSS from Bootstrap theme variables for 
#'   dynamic styling of tabs, buttons, and layout containers
#' - `style_ggplot2()`: Applies standardized background, font, and axis styling 
#'   to ggplot2 charts
#' - `style_plotly()`: Applies theme-consistent layout, hover, and legend 
#'   styling to plotly charts, including geographic maps
#' - `standardize_country_to_iso3()`: Converts country names or codes to ISO 
#'   alpha-3 format for mapping or joins
#' - `flagify_country()`: Converts country identifiers to Unicode flag emojis, 
#'   with optional label formatting
#' - `format_kpi_value()`: Formats raw numeric KPI values into consistent 
#'   currency, percent, number, or country-label strings for display
#'
#' These utilities are used throughout server modules and UI renderers to 
#' ensure clarity, performance, accessibility, and cross-module style 
#' consistency.
#'
#' @keywords internal

#' Light Mode Bootstrap Theme
#'
#' Defines a `bslib` Bootstrap 5 theme suitable for light mode styling
#' across the Shiny UI. Uses the Flatly bootswatch variant and Inter font.
#'
#' @format A `bslib::bs_theme` object.
#' @seealso [bslib::bs_theme()]
theme_light <- bslib::bs_theme(
  version = 5,
  bootswatch = "flatly",
  base_font = bslib::font_google("Inter")
)

#' Dark Mode Bootstrap Theme
#'
#' Defines a `bslib` Bootstrap 5 theme suitable for dark mode styling
#' across the Shiny UI. Uses the Darkly bootswatch variant and Inter font.
#'
#' @format A `bslib::bs_theme` object.
#' @seealso [bslib::bs_theme()]
theme_dark <- bslib::bs_theme(
  version = 5,
  bootswatch = "darkly",
  base_font = bslib::font_google("Inter")
)

#' Generate Style Parameters Based on Theme Mode
#'
#' Produces a named list of styling values based on the current theme mode 
#' (`light` or `dark`). Includes color settings for ggplot2, plotly, KPI cards, 
#' and plotly maps. Intended to be passed into module-specific styling functions.
#'
#' Automatically adjusts the plotly colorscale depending on theme mode to support 
#' visual contrast and colorblind-safe design.
#'
#' @param theme_mode A string, either `"light"` or `"dark"` (default: `"light"`)
#'
#' @return A named list of theme-based style parameters
#' @keywords internal
generate_styles <- function(theme_mode = "light") {
  if (exists("enable_logging", inherits = TRUE) && enable_logging) {
    message(glue::glue("[STYLER] Generating styles for {theme_mode} mode"))
  }
  
  current_theme <- if (identical(theme_mode, "dark")) theme_dark else theme_light
  
  theme_colors <- bslib::bs_get_variables(
    current_theme,
    varnames = c(
      "primary", "secondary", "body-color", 
      "body-bg", "secondary-bg"
    )
  )
  
  # Safe fallbacks
  primary_color  <- theme_colors[["primary"]]      %||% "#1f77b4"
  secondary_color<- theme_colors[["secondary"]]    %||% "#ff7f0e"
  text_color     <- theme_colors[["body-color"]]   %||% "#212529"
  bg_color       <- theme_colors[["body-bg"]]      %||% "#f8f9fa"
  sec_bg_color   <- theme_colors[["secondary-bg"]] %||% "#e9ecef"
  
  # Theme-based color scale (Same for now)
  colorscale_name <- if (identical(theme_mode, "dark")) "Viridis" else "Viridis"
  
  list(
    line_color        = primary_color,
    point_color       = secondary_color,
    text_color        = text_color,
    font_size         = 14,
    line_size         = 0.75,
    point_size        = 1,
    ggplot_bg         = "transparent",
    plotly_bg         = "rgba(0,0,0,0)",
    plotly_hover_bg   = bg_color,
    geo_border_color  = secondary_color,
    geo_frame_color   = text_color,
    geo_country_fill  = secondary_color,
    geo_border_width  = 0.5,
    fill_palette      = list(
      colorscale  = colorscale_name,
      reversescale= TRUE,
      colorbar    = list(
        tickfont   = list(color = text_color),
        titlefont  = list(color = text_color)
      )
    ),
    kpi_bg            = sec_bg_color,
    secondary_color   = secondary_color
  )
}

#' Generate Theme-Sensitive CSS from Bootstrap Variables
#'
#' Creates a CSS stylesheet as a character string using Bootstrap 5 theme 
#' variables (via `bslib::bs_get_variables()`). This enables consistent styling 
#' for sticky headers, navigation tabs, and hover effects that respond to theme 
#' changes (light/dark mode).
#'
#' Use this function inside `renderUI()` via `tags$style(HTML(...))` to apply 
#' dynamic styles inline. Static styles should be defined separately in 
#' `www/styles.css` and loaded via `tags$link()`.
#'
#' @param theme A `bslib::bs_theme` object (e.g. `theme_light`, `theme_dark`)
#'
#' @return A character string of theme-bound CSS rules
#' @keywords internal
generate_css <- function(theme) {
  if (exists("enable_logging", inherits = TRUE) && enable_logging) {
    message("[STYLER] Generating dynamic CSS.")
  }
  
  theme_vars <- bslib::bs_get_variables(
    theme,
    varnames = c(
      "primary", "secondary-bg", "body-bg", "body-color",
      "border-color", "primary-bg-subtle"
    )
  )
  
  glue::glue("
    /* Nav Tab Styling */
    .nav-tabs .nav-link {{
      color: {theme_vars[['body-color']]};
      background-color: {theme_vars[['body-bg']]};
      border: 1px solid {theme_vars[['border-color']]};
      margin-right: 4px;
    }}

    .nav-tabs .nav-link.active {{
      background-color: {theme_vars[['primary']]};
      color: {theme_vars[['body-bg']]};
      border-color: {theme_vars[['primary']]};
    }}

    /* Active Pagination Button */
    .dataTables_wrapper .paginate_button.current {{
      background-color: {theme_vars[['primary']]} !important;
      color: {theme_vars[['body-bg']]} !important;
    }}

    /* Download Button Hover */
    .download-btn:hover {{
      background-color: {theme_vars[['primary-bg-subtle']]} !important;
      color: {theme_vars[['body-color']]} !important;
    }}
  ")
}

#' Apply Theme-Based Styling to ggplot2 Object
#'
#' Formats a ggplot object using background fill, font size, and text color
#' pulled from a theme style list (typically created via `generate_styles()`).
#' Optionally allows manual axis title override for cases where plot labels
#' are lost during conversion.
#'
#' @param ggplot_plot A `ggplot2` object to be styled.
#' @param styles A named list with elements: `font_size`, `ggplot_bg`, 
#'   `text_color`, etc.
#' @param axes_labs Optional named list with overrides for `x`, `y`, or `title`
#'
#' @return A styled `ggplot2` object.
#' @keywords internal
style_ggplot2 <- function(ggplot_plot, styles, axes_labs = NULL) {
  stopifnot(inherits(ggplot_plot, "ggplot"))
  
  if (exists("enable_logging", inherits = TRUE) && enable_logging) {
    message("[STYLER] Styling ggplot2")
  }
  
  s <- styles
  
  plot <- ggplot_plot +
    ggplot2::theme_minimal(base_size = s$font_size) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = s$ggplot_bg, color = NA),
      panel.background = ggplot2::element_rect(fill = s$ggplot_bg, color = NA),
      legend.background = ggplot2::element_rect(fill = s$ggplot_bg, color = NA),
      axis.text = ggplot2::element_text(color = s$text_color),
      axis.title = ggplot2::element_text(color = s$text_color),
      legend.text = ggplot2::element_text(color = s$text_color),
      legend.title = ggplot2::element_text(color = s$text_color),
      title = ggplot2::element_text(color = s$text_color)
    )
  
  # Optional axis/plot title override
  if (!is.null(axes_labs)) {
    if (!is.null(axes_labs$x)) {
      plot <- plot + ggplot2::labs(x = axes_labs$x)
    }
    if (!is.null(axes_labs$y)) {
      plot <- plot + ggplot2::labs(y = axes_labs$y)
    }
    if (!is.null(axes_labs$title)) {
      plot <- plot + ggplot2::labs(title = axes_labs$title)
    }
  }
  
  return(plot)
}

#' Apply Theme-Based Styling to Plotly Plot
#'
#' Applies layout styling (font, background, hover label) to plotly plots.
#' Supports `type = "geo"` for choropleth or map plots, or `"default"` for
#' generic charts. Optionally supports axis or legend label overrides.
#'
#' @param plotly_plot A plotly object.
#' @param styles A named style list from `generate_styles()`.
#' @param type Character. `"geo"` or `"default"` (default: `"default"`).
#' @param axes_labs Optional named list: `x`, `y`, `legend`.
#'
#' @return A styled `plotly` object.
#' @keywords internal
style_plotly <- function(plotly_plot, styles, type = "default", axes_labs = NULL) {
  stopifnot(inherits(plotly_plot, "plotly"))
  
  if (exists("enable_logging", inherits = TRUE) && enable_logging) {
    message("[STYLER] Styling plotly")
  }
  
  s <- styles
  is_geo <- identical(type, "geo")
  
  layout_opts <- list(
    paper_bgcolor = s$plotly_bg,
    plot_bgcolor  = s$plotly_bg,
    font          = list(color = s$text_color, size = s$font_size),
    legend        = list(bgcolor = s$plotly_bg, font = list(color = s$text_color)),
    hoverlabel    = list(
      bgcolor     = s$plotly_hover_bg,
      font        = list(color = s$text_color),
      bordercolor = s$text_color,
      align       = "left"
    ),
    margin        = list(t = 40)
  )
  
  if (is_geo) {
    layout_opts$geo <- list(
      showframe     = TRUE,
      showcoastlines= TRUE,
      showland      = TRUE,
      projection    = list(type = "natural earth"),
      bgcolor       = s$plotly_bg,
      framecolor    = s$geo_frame_color,
      landcolor     = s$geo_country_fill
    )
    
    layout_opts$annotations <- list(list(
      text       = "<b>No Data Available for Location<b>",
      x          = 0.01,
      y          = -0.1,
      xref       = "paper",
      yref       = "paper",
      showarrow  = FALSE,
      font       = list(size = s$font_size - 1, color = s$text_color),
      bgcolor    = s$geo_country_fill,
      bordercolor= s$geo_frame_color,
      borderwidth= 1,
      opacity    = 0.8
    ))
  } else {
    layout_opts$xaxis <- list(
      tickfont   = list(color = s$text_color),
      titlefont  = list(color = s$text_color)
    )
    layout_opts$yaxis <- list(
      tickfont   = list(color = s$text_color),
      titlefont  = list(color = s$text_color)
    )
    layout_opts$hovermode <- "x unified"
  }
  
  # Manual axis and legend labels
  if (!is.null(axes_labs)) {
    if (!is.null(axes_labs$x)) {
      layout_opts$xaxis$title <- list(
        text = axes_labs$x,
        font = list(color = s$text_color)
      )
    }
    if (!is.null(axes_labs$y)) {
      layout_opts$yaxis$title <- list(
        text = axes_labs$y,
        font = list(color = s$text_color)
      )
    }
    if (!is.null(axes_labs$legend)) {
      layout_opts$legend$title <- list(
        text = axes_labs$legend,
        font = list(color = s$text_color)
      )
    }
  }
  
  return(do.call(plotly::layout, c(list(p = plotly_plot), layout_opts)))
}

#' Standardize Country Input to ISO Alpha-3 Code
#'
#' Accepts country names, ISO Alpha-2, or Alpha-3 codes and returns
#' standardized ISO3 country codes for use in choropleths or join keys.
#' Handles edge cases like "UK" vs "United Kingdom" and applies 
#' fallback matching and uppercase normalization.
#'
#' @param input A character vector of country identifiers.
#'
#' @return A character vector of ISO Alpha-3 country codes. Invalid entries 
#'   will return NA.
#'
#' @examples
#' standardize_country_to_iso3("United States")  # "USA"
#' standardize_country_to_iso3("UK")             # "GBR"
#' standardize_country_to_iso3("USA")            # "USA"
#'
#' @export
standardize_country_to_iso3 <- function(input) {
  if (!is.character(input)) stop("Input must be a character vector.")
  
  iso3 <- countrycode::countrycode(
    input,
    origin = "country.name",
    destination = "iso3c",
    warn = FALSE
  )
  
  # Try fallback: ISO2 to ISO3
  failed <- is.na(iso3)
  if (any(failed)) {
    iso3[failed] <- countrycode::countrycode(
      input[failed],
      origin = "iso2c",
      destination = "iso3c",
      warn = FALSE
    )
  }
  
  # Normalize uppercase (for any surviving lowercase ISO3)
  iso3 <- toupper(iso3)
  
  return(iso3)
}

#' Convert Country Identifier to Unicode Flag Emoji or Labeled Display
#'
#' Converts country names, ISO Alpha-2, or Alpha-3 codes to a Unicode flag 
#' emoji. Optionally appends a label, either the full country name or its 
#' ISO Alpha-3 code.
#'
#' @param input A character vector of country names, ISO2, or ISO3 codes.
#' @param label Logical. If TRUE, appends a text label. Default: FALSE.
#' @param label_type One of `"name"` or `"iso3"` (default: `"name"`).
#'
#' @return A character vector of emojis or emoji + label strings.
#'
#' @examples
#' flagify_country("USA")                     # "🇺🇸"
#' flagify_country("UK", label = TRUE)        # "🇬🇧 United Kingdom"
#' flagify_country("UK", label_type = "iso3") # "🇬🇧 GBR"
#'
#' @export
flagify_country <- function(input, label = FALSE, label_type = "name") {
  if (!is.character(input)) stop("Input must be a character vector.")
  label_type <- rlang::arg_match(label_type, c("name", "iso3"))
  
  # Resolve ISO2
  iso2 <- countrycode::countrycode(input, "country.name", "iso2c", warn = FALSE)
  
  # Fallback: iso3 → iso2
  failed <- is.na(iso2)
  if (any(failed)) {
    iso2[failed] <- countrycode::countrycode(
      input[failed], "iso3c", "iso2c", warn = FALSE
    )
  }
  
  # Fallback: try input as iso2
  still_failed <- is.na(iso2)
  iso2[still_failed] <- toupper(input[still_failed])
  
  valid <- grepl("^[A-Z]{2}$", iso2)
  
  # Convert ISO2 to emoji
  flagify <- function(code) {
    paste0(
      intToUtf8(127397 + utf8ToInt(substr(code, 1, 1))),
      intToUtf8(127397 + utf8ToInt(substr(code, 2, 2)))
    )
  }
  
  flags <- ifelse(valid, vapply(iso2, flagify, character(1)), "")
  
  # Resolve label text
  label_text <- switch(
    label_type,
    name = countrycode::countrycode(iso2, "iso2c", "country.name", warn = FALSE),
    iso3 = countrycode::countrycode(iso2, "iso2c", "iso3c", warn = FALSE)
  )
  
  # Final result
  result <- if (label) {
    ifelse(flags != "", paste(flags, label_text), "")
  } else {
    flags
  }
  
  return(result)
}
