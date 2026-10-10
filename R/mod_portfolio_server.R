#' Portfolio tab server
#' @param id Module id.
#' @param state Shared application state.
#' @export
mod_portfolio_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    cfg <- state$cfg
    estimates <- shiny::reactive(portfolio_estimates(state$portfolio(), state$models()$rice_to_review, cfg))

    output$kpis <- shiny::renderUI({
      k <- portfolio_kpis(state$portfolio(), state$history())
      htmltools::div(class = "dv-kpis",
        kpi_tile("Initiatives", k$n_total,
                 sprintf("%d in appraisal \u00b7 %d rejected", k$n_assessment, k$n_rejected)),
        kpi_tile("Pipeline value", fmt_num(k$pipeline_value_mm_usd, 1, " mm$"), "in appraisal"),
        kpi_tile("Committed value", fmt_num(k$committed_value_mm_usd, 1, " mm$"),
                 sprintf("%d prioritized \u00b7 %d executing", k$n_prioritized, k$n_execution), "strong"),
        kpi_tile("Value / cost", fmt_num(k$value_to_cost, 1, "x"),
                 sprintf("cost %s mm$", fmt_num(k$committed_cost_mm_usd, 1))),
        kpi_tile("Realised value", fmt_num(k$realized_value_mm_usd, 2, " mm$"),
                 sprintf("ex-ante %s mm$ (audited)", fmt_num(k$planned_audited_mm_usd, 2))),
        kpi_tile("Realisation rate", fmt_num(k$realization_rate_pct, 0, "%"), "audited / ex-ante"),
        kpi_tile("Adoption", fmt_num(k$mean_adoption_pct, 0, "%"), "mean, audited initiatives"),
        kpi_tile("Gate compliance", sprintf("%s / %s", fmt_num(k$gate2_compliance_pct, 0, "%"),
                                            fmt_num(k$gate3_compliance_pct, 0, "%")),
                 "4M valuation / expert review done"),
        kpi_tile("Decision lead time", fmt_num(k$median_lead_time_days, 0, " d"),
                 "median, registration \u2192 decision"),
        kpi_tile("Open alerts", k$n_alerts, "pending process actions"))
    })

    output$scatter <- echarts4r::renderEcharts4r({
      echart_from_option(prioritization_option(state$portfolio(), cfg, input$y_axis %||% "score",
                                               highlight = state$selected(),
                                               estimate = estimates()$predicted))
    })
    output$status <- echarts4r::renderEcharts4r(echart_from_option(status_option(state$portfolio())))

    alerts <- shiny::reactive(assessment_alerts(state$portfolio()))
    output$alerts <- DT::renderDT({
      a <- alerts()
      dt_compact(data.frame(ID = a$id, Initiative = a$name, Alert = a$alert, Priority = a$severity),
                 page_length = 8, dom = "tp") |>
        DT::formatStyle("Priority", target = "row", fontWeight = DT::styleEqual("high", "600"))
    })
    shiny::observeEvent(input$alerts_rows_selected, {
      a <- alerts()[input$alerts_rows_selected, ]
      state$open(a$id, a$stage)
    })

    table_data <- shiny::reactive({
      pf <- state$portfolio(); est <- estimates()
      data.frame(ID = pf$id, Initiative = pf$name, Unit = pf$business_unit, Status = pf$status,
                 RICE = round(pf$score, 2), Rank = pf$rice_rank, Effort = pf$effort,
                 `Est. mm$` = round(est$predicted, 2),
                 `Ex-ante mm$` = ifelse(pf$planned_value_mm_usd > 0, round(pf$planned_value_mm_usd, 2), NA),
                 `Cost mm$` = round(pf$planned_cost_mm_usd, 2),
                 `Audited mm$` = round(pf$audited_value_mm_usd, 2),
                 `Adoption %` = pf$adoption_pct, check.names = FALSE)
    })
    output$table <- DT::renderDT({
      cols <- status_colors()
      text <- ifelse(cols %in% c(mono$c500, mono$c600, mono$c700, mono$c800, mono$c900), "#ffffff", mono$c900)
      dt_compact(table_data(), page_length = 12, dom = "ftp") |>
        DT::formatStyle("Status", fontWeight = "600",
                        backgroundColor = DT::styleEqual(names(cols), unname(cols)),
                        color = DT::styleEqual(names(cols), unname(text)))
    })
    shiny::observeEvent(input$table_rows_selected, {
      state$open(table_data()$ID[input$table_rows_selected])
    })
  })
}
