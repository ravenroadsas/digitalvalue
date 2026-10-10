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
                 sprintf("%d recorded \u00b7 %d evaluated \u00b7 %d delivered \u00b7 %d audited",
                         k$n_recorded, k$n_evaluated, k$n_delivered, k$n_audited)),
        kpi_tile("Pipeline value", fmt_num(k$pipeline_value_mm_usd, 1, " mm$"), "recorded, evaluation pending"),
        kpi_tile("Evaluated value", fmt_num(k$evaluated_value_mm_usd, 1, " mm$"),
                 sprintf("%d evaluated, not yet delivered", k$n_evaluated), "strong"),
        kpi_tile("Delivered value", fmt_num(k$delivered_value_mm_usd, 1, " mm$"),
                 sprintf("%d awaiting value audit", k$n_delivered)),
        kpi_tile("Value / cost", fmt_num(k$value_to_cost, 1, "x"),
                 sprintf("evaluated + delivered \u00b7 cost %s mm$", fmt_num(k$evaluated_cost_mm_usd, 1))),
        kpi_tile("Realised value", fmt_num(k$realized_value_mm_usd, 2, " mm$"),
                 sprintf("ex-ante %s mm$ (audited)", fmt_num(k$planned_audited_mm_usd, 2))),
        kpi_tile("Realisation rate", fmt_num(k$realization_rate_pct, 0, "%"), "audited / ex-ante"),
        kpi_tile("Adoption", fmt_num(k$mean_adoption_pct, 0, "%"), "mean, audited initiatives"),
        kpi_tile("Gate compliance", sprintf("%s / %s", fmt_num(k$gate2_compliance_pct, 0, "%"),
                                            fmt_num(k$gate3_compliance_pct, 0, "%")),
                 "4MC validated / expert review approved"),
        kpi_tile("Evaluation lead time", fmt_num(k$median_lead_time_days, 0, " d"),
                 "median, registration \u2192 evaluated"),
        kpi_tile("Open alerts", k$n_alerts, "pending process actions"))
    })

    # validations waiting for the current user
    output$mine <- shiny::renderUI({
      state$version()
      v <- db_get_validations(state$con, validator = state$user$user)
      if (!nrow(v)) return(NULL)
      pf <- state$portfolio()
      callout(type = "gate", title = sprintf("4MC validations waiting for you \u00b7 %d", nrow(v)),
        htmltools::tags$ul(lapply(seq_len(nrow(v)), function(i)
          htmltools::tags$li(htmltools::span(class = "dv-gap-id", v$initiative_id[i]), " \u00b7 ",
                             pf$name[match(v$initiative_id[i], pf$id)],
                             sprintf(" (sent by %s on %s)", user_label(v$requested_by[i], state$users()),
                                     substr(v$requested_at[i], 1, 10))))),
        htmltools::div(class = "dv-muted", "Open the initiative: the validation form is in the 4MC card."))
    })

    # highlighted initiative (search box or click in the register)
    highlight <- shiny::reactiveVal(NULL)
    shiny::observe({
      pf <- state$portfolio()
      shiny::updateSelectizeInput(session, "highlight",
        choices = stats::setNames(pf$id, paste(pf$id, "\u00b7", pf$name)),
        selected = shiny::isolate(highlight()) %||% character(0))
    })
    shiny::observeEvent(input$highlight, {
      if (nzchar(input$highlight) && !identical(input$highlight, highlight())) highlight(input$highlight)
    })
    shiny::observeEvent(highlight(), ignoreNULL = FALSE, {
      if (!identical(input$highlight, highlight() %||% ""))
        shiny::updateSelectizeInput(session, "highlight", selected = highlight() %||% character(0))
    })
    shiny::observeEvent(input$clear, highlight(NULL))
    shiny::observeEvent(input$open, if (!is.null(highlight())) state$open(highlight(), "evaluation"))
    output$highlight_info <- shiny::renderUI({
      id <- highlight()
      pf <- state$portfolio()
      if (is.null(id) || !id %in% pf$id) return(NULL)
      r <- pf[pf$id == id, ]
      htmltools::tagList(status_badge(r$status),
        htmltools::span(class = "dv-headline-meta", sprintf("RICE %s \u00b7 ex-ante %s mm$ \u00b7 effort %s",
          fmt_num(r$score, 2), fmt_num(r$planned_value_mm_usd, 2), r$effort)))
    })

    output$scatter <- echarts4r::renderEcharts4r({
      echart_from_option(prioritization_option(state$portfolio(), cfg, input$y_axis %||% "rice_value",
                                               highlight = highlight(),
                                               estimate = estimates()$predicted))
    })
    output$status <- echarts4r::renderEcharts4r(echart_from_option(status_option(state$portfolio(), highlight())))

    gaps <- shiny::reactive(evaluation_gaps(state$portfolio()))
    output$gaps <- shiny::renderUI({
      g <- gaps()
      box_for <- function(col, title, what, value = "missing", suffix = "missing") {
        sel <- g[[col]] == value
        if (!any(sel)) return(NULL)
        callout(type = "gate", title = sprintf("%s %s \u00b7 %d", title, suffix, sum(sel)),
          htmltools::div(what),
          htmltools::tags$ul(lapply(which(sel), function(i)
            htmltools::tags$li(htmltools::span(class = "dv-gap-id", g$id[i]), " \u00b7 ", g$name[i]))))
      }
      boxes <- list(
        box_for("valuation", "4MC valuation", "Above the valuation gate, no 4MC valuation recorded:"),
        box_for("validation", "4MC validation", "4MC required but not validated (not sent or changes requested):"),
        box_for("validation", "4MC validation", "Sent to a validator, decision pending:", "pending", "pending"),
        box_for("review", "Expert review", "Above the review gate, no approved expert review:"),
        box_for("audit", "Value audit", "Delivered, realised value not audited:"))
      boxes <- Filter(Negate(is.null), boxes)
      if (!length(boxes)) return(callout(type = "low", "All required evaluations are recorded."))
      htmltools::div(class = "dv-gaps", boxes)
    })

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
      pf <- state$portfolio(); est <- estimates(); g <- gaps()
      data.frame(ID = pf$id, Initiative = pf$name, Unit = pf$business_unit, Status = pf$status,
                 `4MC` = g$valuation, Validation = g$validation, Review = g$review, Audit = g$audit,
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
                        color = DT::styleEqual(names(cols), unname(text))) |>
        DT::formatStyle(c("4MC", "Validation", "Review", "Audit"),
                        backgroundColor = DT::styleEqual(c("missing", "pending"), c("#FFE7B0", "#FFF4DC")),
                        color = DT::styleEqual(c("missing", "pending", "done", "n/a"),
                                               c("#7A4A00", "#7A4A00", mono$c900, mono$c300)),
                        fontWeight = DT::styleEqual("missing", "600"))
    })
    shiny::observeEvent(input$table_rows_selected, {
      highlight(table_data()$ID[input$table_rows_selected])
    })
  })
}
