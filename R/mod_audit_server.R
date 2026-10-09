#' Execution and audit server
#' @param id Module id.
#' @param state Shared application state.
#' @export
mod_audit_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    editable <- shiny::reactive({
      r <- state$row()
      !is.null(r) && r$status %in% c("In execution", "Closed") && state$user$is_planning
    })
    actual <- mod_prmt_editor_server("editor", state, "actual", enabled = editable)
    plan <- shiny::reactive({
      state$version()
      id <- state$selected()
      db_get_prmt_lines(state$con, if (is.null(id)) "" else id, "plan")
    })
    pva <- shiny::reactive(plan_vs_actual(plan(), actual()))

    queue <- shiny::reactive({
      pf <- state$portfolio()
      pf <- pf[pf$status %in% c("Prioritized", "In execution", "Closed", "Audited"), , drop = FALSE]
      data.frame(ID = pf$id, Initiative = pf$name, Status = pf$status,
                 `Planned mm$` = round(pf$planned_value_mm_usd, 2),
                 `Actual mm$` = round(pf$actual_value_mm_usd, 2), check.names = FALSE)
    })
    output$queue <- DT::renderDT({
      cols <- status_colors()
      dt_compact(queue(), page_length = 8) |>
        DT::formatStyle("Status", color = "#fff", fontWeight = "600",
                        backgroundColor = DT::styleEqual(names(cols), unname(cols)))
    })
    shiny::observeEvent(input$queue_rows_selected, {
      state$selected(queue()$ID[input$queue_rows_selected])
    })

    output$gate <- shiny::renderUI({
      r <- state$row()
      if (is.null(r)) return(NULL)
      if (!state$user$is_planning)
        return(callout(type = "warning", "Only the planning group can audit realised value."))
      switch(r$status,
        "In execution" = callout(type = "info", title = paste(r$id, "in execution"),
                                 "Actual metrics can be recorded; close the initiative to complete the audit."),
        "Closed" = callout(type = "warning", title = paste(r$id, "\u00b7 audit pending"),
                           "Record the actual P/R/M/T with the same formulas used in Phase II."),
        "Audited" = callout(type = "success", title = paste(r$id, "audited"), "Audit completed."),
        callout(type = "info", title = paste(r$id, "\u00b7", r$status),
                "Audit is available once the initiative is in execution or closed."))
    })

    output$tiles <- shiny::renderUI({
      d <- pva()
      r <- state$row()
      # planned = planning-validated value when available, else Phase II total
      pl <- if (is.null(r)) sum(d$planned_mm_usd) else r$planned_value_mm_usd
      ac <- sum(d$actual_mm_usd)
      rr <- realization_pct(ac, pl)
      htmltools::div(class = "dv-kpis",
        kpi_tile("Planned value", fmt_num(pl, 2, " mm$"), "validated / Phase II", "info"),
        kpi_tile("Actual value", fmt_num(ac, 2, " mm$"), "materialised", "good"),
        kpi_tile("Realisation", fmt_num(rr, 0, "%"), "actual / planned",
                 if (is.na(rr)) "neutral" else if (rr >= 90) "good" else if (rr >= 70) "warn" else "bad"),
        kpi_tile("Gap", fmt_num(ac - pl, 2, " mm$"), "actual \u2212 planned"))
    })

    output$pva <- echarts4r::renderEcharts4r(echart_from_option(plan_actual_option(pva())))
    output$pva_table <- DT::renderDT({
      d <- pva()
      dt_compact(data.frame(Metric = d$label, Unit = d$unit, Planned = round(d$planned, 3),
                            Actual = round(d$actual, 3), `Planned mm$` = round(d$planned_mm_usd, 3),
                            `Actual mm$` = round(d$actual_mm_usd, 3),
                            `Real. %` = round(d$realization_pct, 0), check.names = FALSE),
                 dom = "t", selection = "none", ordering = FALSE)
    })

    shiny::observeEvent(input$complete, {
      r <- state$row()
      if (is.null(r) || r$status != "Closed" || !state$user$is_planning) {
        shiny::showNotification("Only closed initiatives can be audited (planning group)", type = "warning")
        return()
      }
      if (!nrow(actual())) {
        shiny::showNotification("Record at least one actual metric first", type = "warning"); return()
      }
      d <- pva()
      db_add_audit(state$con, r$id, r$planned_value_mm_usd, sum(d$actual_mm_usd),
                   if (nzchar(input$comment)) input$comment else NA, state$user$user)
      db_set_status(state$con, r$id, "Audited", state$user$user, "Audit completed")
      state$refresh()
      shiny::showNotification(paste(r$id, "audited"), type = "message")
    })

    output$audits <- DT::renderDT({
      state$version()
      a <- db_get_audits(state$con)
      a <- a[a$initiative_id %in% (state$selected() %||% ""), ]
      dt_compact(data.frame(Date = substr(a$audited_at, 1, 16), `Planned mm$` = round(a$planned_value_mm_usd, 2),
                            `Actual mm$` = round(a$actual_value_mm_usd, 2),
                            `Real. %` = round(a$realization_pct, 0), Auditor = a$auditor,
                            Conclusion = a$comment, check.names = FALSE),
                 dom = "t", selection = "none")
    })
  })
}
