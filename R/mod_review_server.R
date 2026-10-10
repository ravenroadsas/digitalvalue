#' Expert review server
#' @param id Module id.
#' @param state Shared application state.
#' @export
mod_review_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    cfg <- state$cfg
    shiny::updateSelectInput(session, "category", choices = names(reserve_values(cfg)))

    editable <- shiny::reactive({
      r <- state$row()
      !is.null(r) && state$can("review") && r$status %in% status_assessment
    })
    shiny::observe(shinyjs::toggle("form", condition = editable()))

    lines <- shiny::reactive({ state$version(); db_get_m4_lines(state$con, state$selected() %||% "") })
    reviews <- shiny::reactive({ state$version(); db_get_reviews(state$con, state$selected() %||% "") })

    output$tag <- shiny::renderUI({
      r <- state$row()
      if (is.null(r)) return(NULL)
      pending <- isTRUE(r$pending_step == "Expert review") ||
        (r$req_review && !isTRUE(r$review_decision == "Approve") && r$status == "Recorded")
      htmltools::span(class = paste("dv-tag", if (pending) "dv-tag-gate"),
                      if (pending) "required \u00b7 pending" else if (r$req_review) "required" else "not required")
    })
    output$gate <- shiny::renderUI({
      r <- state$row()
      if (is.null(r)) return(NULL)
      htmltools::tagList(
        if (!state$can("review")) callout(type = "info", "Expert reviews are recorded by superusers."))
    })

    # pre-fill with the latest review, else with the 4MC estimate
    prefill <- shiny::reactive(list(state$selected(), nrow(lines()), nrow(reviews())))
    shiny::observeEvent(prefill(), {
      rv <- reviews(); r <- state$row()
      if (is.null(r)) return()
      if (nrow(rv)) {
        update_m4_inputs(session, list(P = rv$p_bopd[1], R = rv$r_mmbbl[1], M = rv$m_mm_usd[1],
                                       T = rv$t_khours[1], C = rv$cost_mm_usd[1],
                                       category = rv$r_category[1]))
      } else {
        v <- m4_from_lines(lines())
        v$C <- r$planned_cost_mm_usd  # 4MC cost estimate, else registration cost
        update_m4_inputs(session, v)
      }
    })

    values <- shiny::reactive(list(P = input$P, R = input$R, M = input$M, T = input$T,
                                   C = input$C, category = input$category))
    output$preview <- shiny::renderUI({
      r <- state$row()
      if (is.null(r)) return(NULL)
      v <- m4_value(values(), cfg)
      rec <- state$models()$review_to_audit
      est <- predict_realised(rec, v$total, r$confidence, r$effort, cfg)
      lab <- interval_labels(rec$level %||% 0.8)
      htmltools::div(class = "dv-estimate",
        htmltools::div(class = "dv-estimate-label", "Ex-ante value of this evaluation"),
        htmltools::div(class = "dv-estimate-value", sprintf("%s mm USD", fmt_num(v$total, 2)),
          htmltools::span(sprintf("cost %s mm USD \u00b7 value/cost %s", fmt_num(v$cost, 2),
                                  fmt_num(v$value_to_cost, 1, "x")))),
        if (!is.na(est$predicted)) htmltools::div(class = "dv-muted", sprintf(
          "Anticipated realised value: %s mm USD (%s\u2013%s %s \u2013 %s), from %d audited initiatives.",
          fmt_num(est$predicted, 2), lab[1], lab[2], fmt_num(est$low, 2), fmt_num(est$high, 2), rec$n)))
    })

    output$compare <- DT::renderDT({
      rv <- reviews()
      lc <- m4_lifecycle(lines(), if (nrow(rv)) rv[1, ] else NULL, NULL, cfg)
      dt_compact(data.frame(Metric = lc$label, Unit = lc$unit, `4MC estimate` = round(lc$estimate, 3),
                            `Expert review` = round(lc$review, 3),
                            `Estimate mm$` = round(lc$estimate_mm_usd, 3),
                            `Review mm$` = round(lc$review_mm_usd, 3), check.names = FALSE),
                 dom = "t", selection = "none", ordering = FALSE)
    })
    output$history <- DT::renderDT({
      h <- reviews()
      dt_compact(data.frame(Date = substr(h$reviewed_at, 1, 16), Decision = h$decision,
                            `Value mm$` = round(h$value_mm_usd, 2), `Cost mm$` = h$cost_mm_usd,
                            Reviewer = h$reviewer, Notes = h$comment, check.names = FALSE),
                 page_length = 4, dom = "tp", selection = "none")
    })

    shiny::observeEvent(input$save, {
      r <- state$row()
      if (is.null(r) || !editable()) return()
      v <- values()
      db_add_review(state$con, r$id, input$decision, v, m4_value(v, cfg)$total, input$C,
                    if (nzchar(input$comment)) input$comment else NA, state$user$user)
      shiny::updateTextAreaInput(session, "comment", value = "")
      state$refresh()
      shiny::showNotification(sprintf("%s: %s recorded \u2013 status %s", r$id, input$decision,
                                      state$row()$status), type = "message")
    })
  })
}
