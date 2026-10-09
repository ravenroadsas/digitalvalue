#' Portfolio overview server
#' @param id Module id.
#' @param state Shared application state.
#' @export
mod_overview_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    cfg <- state$cfg

    output$kpis <- shiny::renderUI({
      k <- portfolio_kpis(state$portfolio(), state$history())
      tone_pct <- function(x, good = 90, warn = 70) {
        if (is.na(x)) "neutral" else if (x >= good) "good" else if (x >= warn) "warn" else "bad"
      }
      htmltools::div(class = "dv-kpis",
        kpi_tile("Initiatives", k$n_total,
                 sprintf("%d in assessment \u00b7 %d rejected", k$n_assessment, k$n_rejected)),
        kpi_tile("Pipeline value", fmt_num(k$pipeline_value_mm_usd, 1, " mm$"), "in assessment", "info"),
        kpi_tile("Committed value", fmt_num(k$committed_value_mm_usd, 1, " mm$"),
                 sprintf("%d prioritized \u00b7 %d executing", k$n_prioritized, k$n_execution), "good"),
        kpi_tile("Value / cost", fmt_num(k$value_to_cost, 1, "x"),
                 sprintf("cost %s mm$", fmt_num(k$committed_cost_mm_usd, 1))),
        kpi_tile("Realised value", fmt_num(k$realized_value_mm_usd, 2, " mm$"),
                 sprintf("planned %s mm$ (audited)", fmt_num(k$planned_audited_mm_usd, 2))),
        kpi_tile("Realisation rate", fmt_num(k$realization_rate_pct, 0, "%"),
                 "actual / planned, audited", tone_pct(k$realization_rate_pct)),
        kpi_tile("Gate compliance", sprintf("%s / %s", fmt_num(k$gate2_compliance_pct, 0, "%"),
                                            fmt_num(k$gate3_compliance_pct, 0, "%")),
                 "Phase II / Phase III done", tone_pct(min(k$gate2_compliance_pct, k$gate3_compliance_pct, na.rm = TRUE))),
        kpi_tile("Decision lead time", fmt_num(k$median_lead_time_days, 0, " d"),
                 "median, registration \u2192 decision"),
        kpi_tile("Open alerts", k$n_alerts, "pending process actions",
                 if (k$n_alerts > 0) "warn" else "good")
      )
    })

    output$scatter <- echarts4r::renderEcharts4r({
      echart_from_option(prioritization_option(state$portfolio(), cfg, input$y_axis %||% "score",
                                               highlight = state$selected()))
    })
    output$status <- echarts4r::renderEcharts4r({
      echart_from_option(status_option(state$portfolio()))
    })

    alerts <- shiny::reactive(assessment_alerts(state$portfolio()))
    output$alerts <- DT::renderDT({
      a <- alerts()
      dt_compact(a[, c("id", "name", "alert", "severity")], page_length = 8,
                 columnDefs = list(list(visible = FALSE, targets = 3))) |>
        DT::formatStyle("severity", target = "row",
                        backgroundColor = DT::styleEqual(
                          c("danger", "warning", "info", "success"),
                          c("#fcf1f0", "#fdf8ec", "#f4f8fd", "#f1f9f4")))
    })
    shiny::observeEvent(input$alerts_rows_selected, {
      a <- alerts()[input$alerts_rows_selected, ]
      state$selected(a$id)
      tab <- switch(a$severity, warning = "phase2", danger = "phase3", info = "audit", "phase1")
      state$goto(tab)
    })

    table_data <- shiny::reactive({
      pf <- state$portfolio()
      data.frame(ID = pf$id, Initiative = pf$name, Unit = pf$business_unit, Status = pf$status,
                 `RICE` = round(pf$score, 2), Rank = pf$rice_rank, Effort = pf$effort,
                 `Value mm$` = round(pf$planned_value_mm_usd, 2),
                 `Cost mm$` = round(pf$planned_cost_mm_usd, 2),
                 `Actual mm$` = round(pf$audited_value_mm_usd, 2), check.names = FALSE)
    })
    output$table <- DT::renderDT({
      d <- table_data()
      cols <- status_colors()
      dt_compact(d, page_length = 12) |>
        DT::formatStyle("Status", color = "#fff", fontWeight = "600",
                        backgroundColor = DT::styleEqual(names(cols), unname(cols)))
    })
    shiny::observeEvent(input$table_rows_selected, {
      state$selected(table_data()$ID[input$table_rows_selected])
    })
  })
}
