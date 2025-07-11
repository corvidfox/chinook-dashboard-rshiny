insights_ui <- function(id) {
  ns <- NS(id)
  tagList(
    bslib::layout_column_wrap(
      width = 1/3,
      fill = TRUE,
      fillable = TRUE,
      height = "auto",
      gap = "1rem",
      card_height = "200px",
      render_kpi_card(
        title = "Customer Overview",
        icon = bsicons::bs_icon("people"),
        tooltip = "Customer count and average engagement lifespan",
        kpi_list = list(
          build_kpi("Total Customers", textOutput(ns("cust_count"))),
          build_kpi("% New", textOutput(ns("cust_pct_new"))),
          build_kpi("Avg Lifespan (Full)", textOutput(ns("life_full"))),
          build_kpi("Avg Lifespan (Subset)", textOutput(ns("life_subset")))
        )
      ),
      render_kpi_card(
        title = "Repeat Behavior",
        icon = bsicons::bs_icon("arrow-repeat"),
        tooltip = "Insights on repeat customer dynamics",
        kpi_list = list(
          build_kpi("Repeat Rate (All)", textOutput(ns("repeat_total"))),
          build_kpi("Returning Repeat", textOutput(ns("repeat_ret"))),
          build_kpi("New → Repeat", textOutput(ns("repeat_new"))),
          build_kpi("1st→2nd Gap", textOutput(ns("gap_overall")))
        )
      ),
      render_kpi_card(
        title = "Purchase Tempo & Cohorts",
        icon = bsicons::bs_icon("clock-history"),
        tooltip = "Engagement pacing and top retention cohorts",
        kpi_list = list(
          build_kpi("Avg Gap (Window)", textOutput(ns("gap_window"))),
          build_kpi("Avg Gap (Bounded)", textOutput(ns("gap_bounded"))),
          build_kpi("Top 3-Mo Cohort", textOutput(ns("cohort_3"))),
          build_kpi("Top 6-Mo Cohort", textOutput(ns("cohort_6"))),
          build_kpi("Top 9-Mo Cohort", textOutput(ns("cohort_9")))
        )
      )
    ),
    tags$hr(),
    bslib::card(
      full_screen = FALSE,
      class = "mt-4",
      style = "background-color: var(--bs-secondary-bg);",
      bslib::card_header("Key Takeaways"),
      bslib::card_body(
        tags$ul(
          tags$li("🇺🇸 USA and 🇩🇪 Germany are revenue powerhouses."),
          tags$li("🎧 Comedy and Sci-Fi offer strong ROI per track."),
          tags$li("🔁 Retention drops after 1 month; Month 5 is key."),
          tags$li("🌎 Chile and Austria outperform size-wise."),
          tags$li("💡 Consider mid-cycle loyalty nudges and bundle promos.")
        )
      )
    )
  )
}
