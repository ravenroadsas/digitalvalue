#' Value audit server
#' @param id Module id.
#' @param state Shared application state.
#' @export
mod_audit_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    cfg <- state$cfg
    shiny::updateSelectInput(session, "category", choices = names(reserve_values(cfg)))

    editable <- shiny::reactive({
      r <- state$row()
      !is.null(r) && state$can("audit") && r$status == "Closed"
    })
    shiny::observe(shinyjs::toggle("form", condition = editable()))

    lines <- shiny::reactive({ state$version(); db_get_m4_lines(state$con, state$selected() %||% "") })
    reviews <- shiny::reactive({ state$version(); db_get_reviews(state$con, state$selected() %||% "") })
    audits <- shiny::reactive({ state$version(); db_get_audits(state$con, state$selected() %||% "") })
    lifecycle <- shiny::reactive({
      rv <- reviews(); au <- audits()
      m4_lifecycle(lines(), if (nrow(rv)) rv[1, ] else NULL, if (nrow(au)) au[1, ] else NULL, cfg)
    })

    output$status <- shiny::renderUI({
      r <- state$row()
      if (is.null(r)) return(callout(type = "info", "Select an initiative."))
      switch(r$status,
        "Prioritized" = callout(type = "info", title = "Prioritized",
                                "Start execution from the decision bar; the audit opens after closure."),
        "In execution" = callout(type = "info", title = "In execution",
                                 "Close execution from the decision bar to record the value audit."),
        "Closed" = callout(type = "medium", title = "Value audit pending",
          if (state$can("audit")) "Record the actual 4M figures and adoption."
          else "The value audit is recorded by superusers."),
        "Audited" = callout(type = "low", title = "Audited", "Realised value recorded."),
        callout(type = "info", title = r$status, "Realisation starts once the initiative is prioritized."))
    })

    output$tiles <- shiny::renderUI({
      r <- state$row()
      if (is.null(r)) return(NULL)
      rec <- state$models()$review_to_audit
      est <- predict_realised(rec, r$planned_value_mm_usd, r$confidence, r$effort, cfg)
      lab <- interval_labels(rec$level %||% 0.8)
      ac <- r$audited_value_mm_usd
      rr <- realization_pct(ac, r$planned_value_mm_usd)
      htmltools::div(class = "dv-kpis",
        kpi_tile("Ex-ante value", fmt_num(r$planned_value_mm_usd, 2, " mm$"),
                 if (isTRUE(r$review_decision == "Approve")) "expert review" else "4M estimate"),
        kpi_tile("Anticipated realised", fmt_num(est$predicted, 2, " mm$"),
                 if (is.na(est$predicted)) "no published model"
                 else sprintf("%s\u2013%s %s \u2013 %s", lab[1], lab[2], fmt_num(est$low, 2), fmt_num(est$high, 2))),
        kpi_tile("Audited value", fmt_num(ac, 2, " mm$"), "actual 4M", "strong"),
        kpi_tile("Realisation", fmt_num(rr, 0, "%"), "audited / ex-ante"),
        kpi_tile("Adoption", fmt_num(r$adoption_pct, 0, "%"), "of intended users"))
    })

    # pre-fill with the latest audit, else with the ex-ante figures
    prefill <- shiny::reactive(list(state$selected(), nrow(audits()), nrow(reviews())))
    shiny::observeEvent(prefill(), {
      au <- audits(); rv <- reviews()
      if (nrow(au)) {
        update_m4_inputs(session, list(P = au$p_bopd[1], R = au$r_mmbbl[1], M = au$m_mm_usd[1],
                                       T = au$t_khours[1], category = au$r_category[1]))
        shiny::updateSliderInput(session, "adoption", value = au$adoption_pct[1])
      } else if (nrow(rv)) {
        update_m4_inputs(session, list(P = rv$p_bopd[1], R = rv$r_mmbbl[1], M = rv$m_mm_usd[1],
                                       T = rv$t_khours[1], category = rv$r_category[1]))
      } else {
        update_m4_inputs(session, m4_from_lines(lines()))
      }
    })

    values <- shiny::reactive(list(P = input$P, R = input$R, M = input$M, T = input$T,
                                   category = input$category))
    output$preview <- shiny::renderUI({
      r <- state$row()
      if (is.null(r)) return(NULL)
      v <- m4_value(values(), cfg)$total
      htmltools::div(class = "dv-estimate",
        htmltools::div(class = "dv-estimate-label", "Audited value"),
        htmltools::div(class = "dv-estimate-value", sprintf("%s mm USD", fmt_num(v, 2)),
          htmltools::span(sprintf("%s of the ex-ante value",
                                  fmt_num(realization_pct(v, r$planned_value_mm_usd), 0, "%")))))
    })
    output$locked <- shiny::renderUI({
      if (editable()) return(NULL)
      htmltools::div(class = "dv-muted", "The audit form opens for superusers once execution is closed.")
    })

    output$chart <- echarts4r::renderEcharts4r(echart_from_option(lifecycle_option(lifecycle())))
    output$table <- DT::renderDT({
      lc <- lifecycle()
      dt_compact(data.frame(Metric = lc$label, Unit = lc$unit, `4M estimate` = round(lc$estimate, 3),
                            Review = round(lc$review, 3), Audited = round(lc$actual, 3),
                            `Real. %` = round(lc$realization_pct, 0), check.names = FALSE),
                 dom = "t", selection = "none", ordering = FALSE)
    })
    output$history <- DT::renderDT({
      a <- audits()
      dt_compact(data.frame(Date = substr(a$audited_at, 1, 16),
                            `Ex-ante mm$` = round(a$expected_value_mm_usd, 2),
                            `Audited mm$` = round(a$actual_value_mm_usd, 2),
                            `Real. %` = round(a$realization_pct, 0), `Adoption %` = a$adoption_pct,
                            Auditor = a$auditor, Conclusion = a$comment, check.names = FALSE),
                 dom = "t", selection = "none")
    })

    shiny::observeEvent(input$save, {
      r <- state$row()
      if (is.null(r) || !editable()) return()
      v <- values()
      db_add_audit(state$con, r$id, v, input$adoption, m4_value(v, cfg)$total, r$planned_value_mm_usd,
                   if (nzchar(input$comment)) input$comment else NA, state$user$user)
      db_set_status(state$con, r$id, "Audited", state$user$user, "Value audit recorded")
      state$refresh()
      shiny::showNotification(paste(r$id, "audited"), type = "message")
    })
  })
}
