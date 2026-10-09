#' Phase I (RICE) server
#' @param id Module id.
#' @param state Shared application state.
#' @export
mod_phase1_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    cfg <- state$cfg
    shiny::updateSelectInput(session, "impact", choices = rice_levels(cfg, "impact"), selected = "M")
    shiny::updateRadioButtons(session, "confidence", choices = rice_levels(cfg, "confidence"),
                              selected = "High", inline = TRUE)
    shiny::updateSelectInput(session, "effort", choices = rice_levels(cfg, "effort"), selected = "M")

    output$title <- shiny::renderText({
      r <- state$row()
      if (is.null(r)) "RICE inputs" else paste("RICE inputs \u00b7", r$id, r$name)
    })

    # load stored RICE when the active initiative changes
    shiny::observeEvent(state$selected(), {
      r <- state$row()
      if (is.null(r) || is.na(r$score)) {
        shiny::updateTextAreaInput(session, "rationale", value = "")
        return()
      }
      rc <- db_get_rice(state$con)
      rc <- rc[rc$initiative_id == r$id, ]
      shiny::updateNumericInput(session, "users", value = rc$users)
      shiny::updateSelectInput(session, "impact", selected = rc$impact)
      shiny::updateRadioButtons(session, "confidence", selected = rc$confidence)
      shiny::updateSelectInput(session, "effort", selected = rc$effort)
      shiny::updateTextAreaInput(session, "rationale", value = if (is.na(rc$rationale)) "" else rc$rationale)
    })

    assessment <- shiny::reactive({
      shiny::req(input$impact, input$confidence, input$effort)
      tryCatch(rice_assess(input$users, input$impact, input$confidence, input$effort, cfg,
                           input$rationale), error = function(e) NULL)
    })

    output$preview <- shiny::renderUI({
      a <- assessment()
      if (is.null(a)) return(callout("Enter a valid number of users.", type = "danger"))
      req2 <- requires_phase2(a$effort, a$score, cfg)
      m <- state$value_model()
      est <- predict_value(m, a$users, a$impact, a$confidence, a$effort, cfg)
      lab <- interval_labels(m$level)
      r <- state$row()
      htmltools::tagList(
        htmltools::div(class = "dv-formula", sprintf(
          "log10(%s) \u00d7 %s \u00d7 %s / %s = %.2f \u00d7 %s \u00d7 %s / %s = %.2f",
          format(a$users, big.mark = ","), a$impact, a$confidence, a$effort, a$reach_value,
          rice_weight(a$impact, "impact", cfg), rice_weight(a$confidence, "confidence", cfg),
          rice_weight(a$effort, "effort", cfg), a$score)),
        if (req2) callout(type = "warning", title = "Phase II required",
          sprintf("Effort \u2265 %s or score \u2265 %s: a monetary (PRMT) valuation will be required.",
                  cfg$params$gate2_effort_min, cfg$params$gate2_score_min))
        else callout(type = "success", title = "Phase I sufficient",
                     "Below Phase II thresholds: can be prioritised on RICE alone."),
        if (is.na(est$median)) callout(type = "info", title = "Anticipated value", m$message)
        else callout(type = "info", title = "Anticipated value (value model)",
          htmltools::div(class = "dv-kpi-value", style = "font-size:15px",
            sprintf("%s mm USD", fmt_num(est$median, 2)),
            htmltools::span(class = "dv-muted", style = "font-size:11px; font-weight:400",
              sprintf(" %s\u2013%s: %s \u2013 %s mm USD", lab[1], lab[2],
                      fmt_num(est$low, 2), fmt_num(est$high, 2)))),
          htmltools::div(class = "dv-muted", sprintf(
            "%s model on %d valued initiatives \u00b7 R\u00b2 %s \u00b7 correlation %s.",
            if (m$type == "simple") "Simple" else "Multivariable", m$n,
            fmt_num(m$r2, 2), fmt_num(m$cor_pearson %||% NA, 2))),
          if (est$high >= cfg$params$gate3_value_min_mm_usd)
            htmltools::div(sprintf("Upper end \u2265 %s mm USD: Phase III planning review is likely.",
                                   cfg$params$gate3_value_min_mm_usd)),
          if (!is.null(r) && isTRUE(r$plan_value_mm_usd > 0))
            htmltools::div(sprintf("Phase II valuation recorded: %s mm USD (%s the range).",
                                   fmt_num(r$planned_value_mm_usd, 2),
                                   if (r$planned_value_mm_usd < est$low) "below"
                                   else if (r$planned_value_mm_usd > est$high) "above" else "within")))
      )
    })

    shiny::observeEvent(input$save, {
      r <- state$row(); a <- assessment()
      if (is.null(r) || is.null(a)) return()
      db_save_rice(state$con, r$id, a, state$user$user)
      state$refresh()
      r2 <- state$row()
      nxt <- switch(r2$status, "Phase II" = "phase2", "Phase III" = "phase3", NULL)
      shiny::showNotification(sprintf("%s scored %.2f \u2013 status %s", r$id, a$score, r2$status),
                              type = "message")
      if (!is.null(nxt)) state$goto(nxt)
    })

    output$scales <- DT::renderDT({
      s <- cfg$scales
      dt_compact(data.frame(Metric = s$dimension, Level = s$level, Weight = s$value,
                            Meaning = s$description), page_length = 15, dom = "t",
                 ordering = FALSE, selection = "none")
    })

    output$scatter <- echarts4r::renderEcharts4r({
      pf <- state$portfolio()
      echart_from_option(prioritization_option(pf, cfg, input$y_axis %||% "score",
                                               highlight = state$selected(),
                                               estimate = portfolio_estimates(pf, state$value_model(), cfg)$median))
    })

    rank_tbl <- shiny::reactive({
      pf <- state$portfolio()
      est <- portfolio_estimates(pf, state$value_model(), cfg)
      keep <- !is.na(pf$score)
      pf <- pf[keep, ]; est <- est[keep, , drop = FALSE]
      o <- order(pf$rice_rank)
      pf <- pf[o, ]; est <- est[o, , drop = FALSE]
      data.frame(Rank = pf$rice_rank, ID = pf$id, Initiative = pf$name,
                 Users = pf$users, I = pf$impact, C = pf$confidence, E = pf$effort,
                 Score = round(pf$score, 2),
                 `Est. mm$` = round(est$median, 2),
                 `Est. range` = ifelse(is.na(est$median), "",
                                       sprintf("%.2f\u2013%.2f", est$low, est$high)),
                 `Valued mm$` = ifelse(pf$planned_value_mm_usd > 0, round(pf$planned_value_mm_usd, 2), NA),
                 Status = pf$status, check.names = FALSE)
    })
    output$ranking <- DT::renderDT({
      cols <- status_colors()
      dt_compact(rank_tbl(), page_length = 10) |>
        DT::formatStyle("Status", color = "#fff", fontWeight = "600",
                        backgroundColor = DT::styleEqual(names(cols), unname(cols)))
    })
    shiny::observeEvent(input$ranking_rows_selected, {
      state$selected(rank_tbl()$ID[input$ranking_rows_selected])
    })

    output$alerts <- shiny::renderUI({
      a <- assessment_alerts(state$portfolio())
      if (!nrow(a)) return(callout("No pending gate actions.", type = "success"))
      lapply(split(a, a$severity), function(g)
        callout(type = g$severity[1], title = g$alert[1],
                htmltools::tags$ul(lapply(seq_len(nrow(g)), function(i)
                  htmltools::tags$li(paste(g$id[i], "\u00b7", g$name[i]))))))
    })
  })
}
