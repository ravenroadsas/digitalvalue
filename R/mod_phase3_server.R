#' Phase III (planning review) server
#' @param id Module id.
#' @param state Shared application state.
#' @export
mod_phase3_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    cfg <- state$cfg

    queue <- shiny::reactive({
      pf <- state$portfolio()
      pf <- pf[pf$req_phase3, , drop = FALSE]
      pf <- pf[order(pf$status != "Phase III", -pf$planned_value_mm_usd), , drop = FALSE]
      data.frame(ID = pf$id, Initiative = pf$name, Status = pf$status,
                 `Value mm$` = round(pf$planned_value_mm_usd, 2),
                 `Cost mm$` = round(pf$planned_cost_mm_usd, 2),
                 Review = pf$review_decision, check.names = FALSE)
    })
    output$queue <- DT::renderDT({
      cols <- status_colors()
      dt_compact(queue(), page_length = 12) |>
        DT::formatStyle("Status", color = "#fff", fontWeight = "600",
                        backgroundColor = DT::styleEqual(names(cols), unname(cols)))
    })
    shiny::observeEvent(input$queue_rows_selected, {
      state$selected(queue()$ID[input$queue_rows_selected])
    })

    output$title <- shiny::renderText({
      r <- state$row()
      if (is.null(r)) "Dossier" else paste("Dossier \u00b7", r$id, r$name)
    })

    output$dossier <- shiny::renderUI({
      r <- state$row()
      if (is.null(r)) return(NULL)
      gr <- gate_reasons(r, cfg)
      lines <- db_get_prmt_lines(state$con, r$id, "plan")
      htmltools::tagList(
        htmltools::p(r$description),
        if (r$req_phase3) callout(type = "danger", title = "Phase III required",
                                  htmltools::tags$ul(lapply(gr$phase3, htmltools::tags$li)))
        else callout(type = "info", title = "Phase III not required",
                     "Below thresholds; a planning review can still be recorded."),
        htmltools::div(class = "dv-kpis",
          kpi_tile("RICE", fmt_num(r$score, 2), paste(r$impact, r$confidence, r$effort, sep = " \u00b7 ")),
          kpi_tile("Value", fmt_num(r$plan_value_mm_usd, 2, " mm$"), "Phase II", "good"),
          kpi_tile("Cost", fmt_num(r$cost_mm_usd, 2, " mm$"), "estimated", "warn")),
        htmltools::div(class = "dv-box-header", "PRMT calculations"),
        if (!nrow(lines)) htmltools::div(class = "dv-muted", "No valuation recorded.")
        else lapply(seq_len(nrow(lines)), function(i) htmltools::tagList(
          htmltools::div(class = "dv-formula", paste0("[", lines$metric[i], "] ", lines$formula_text[i])),
          if (!is.na(lines$comment[i])) htmltools::div(class = "dv-muted", style = "margin:-4px 0 6px",
                                                       lines$comment[i])))
      )
    })

    # pre-fill with the Phase II figures; refreshed when the valuation changes
    prefill <- shiny::reactive({
      r <- state$row()
      if (is.null(r)) NULL else list(r$id, r$plan_value_mm_usd, r$cost_mm_usd)
    })
    shiny::observeEvent(prefill(), {
      r <- state$row()
      shiny::updateNumericInput(session, "validated_value", value = round(r$plan_value_mm_usd, 3))
      shiny::updateNumericInput(session, "validated_cost", value = r$cost_mm_usd)
    })

    output$access <- shiny::renderUI({
      if (!state$user$is_planning)
        callout(type = "warning", "Only members of the planning group can record evaluations.")
    })

    shiny::observeEvent(input$save, {
      r <- state$row()
      if (is.null(r)) return()
      if (!state$user$is_planning) {
        shiny::showNotification("Planning group membership required", type = "error"); return()
      }
      if (!r$status %in% status_assessment) {
        shiny::showNotification("Evaluation is only possible before the prioritisation decision",
                                type = "warning"); return()
      }
      db_add_review(state$con, r$id, input$decision, input$validated_value, input$validated_cost,
                    if (nzchar(input$comment)) input$comment else NA, state$user$user)
      shiny::updateTextAreaInput(session, "comment", value = "")
      state$refresh()
      shiny::showNotification(sprintf("%s: %s recorded \u2013 status %s", r$id, input$decision,
                                      state$row()$status), type = "message")
    })

    output$history <- DT::renderDT({
      state$version()
      id <- state$selected()
      h <- if (is.null(id)) db_get_reviews(state$con, "") else db_get_reviews(state$con, id)
      dt_compact(data.frame(Date = substr(h$reviewed_at, 1, 16), Decision = h$decision,
                            `Value mm$` = h$validated_value_mm_usd, `Cost mm$` = h$validated_cost_mm_usd,
                            Reviewer = h$reviewer, Notes = h$comment, check.names = FALSE),
                 page_length = 5, selection = "none")
    })
  })
}
