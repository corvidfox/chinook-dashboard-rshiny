insights_server <- function(id, kpi_data, styles) {
  moduleServer(id, function(input, output, session) {
    # All outputs use pre-computed KPI values passed in
    output$cust_count     <- renderText(kpi_data()$num_customers)
    output$cust_pct_new   <- renderText(kpi_data()$pct_new_customers)
    output$life_full      <- renderText(kpi_data()$avg_lifespan_months_total)
    output$life_subset    <- renderText(kpi_data()$avg_lifespan_months_window)
    
    output$repeat_total   <- renderText(kpi_data()$repeat_rate_total)
    output$repeat_ret     <- renderText(kpi_data()$repeat_rate_returning)
    output$repeat_new     <- renderText(kpi_data()$repeat_rate_new)
    output$gap_overall    <- renderText(kpi_data()$median_gap_overall)
    
    output$gap_window     <- renderText(kpi_data()$avg_gap_window)
    output$gap_bounded    <- renderText(kpi_data()$avg_gap_bounded)
    
    output$cohort_3 <- renderText(paste(
      kpi_data()$top_cohort_month_3,
      "(", kpi_data()$top_cohort_retention_3, ")"
    ))
    
    output$cohort_6 <- renderText(paste(
      kpi_data()$top_cohort_month_6,
      "(", kpi_data()$top_cohort_retention_6, ")"
    ))
    
    output$cohort_9 <- renderText(paste(
      kpi_data()$top_cohort_month_9,
      "(", kpi_data()$top_cohort_retention_9, ")"
    ))
  })
}
