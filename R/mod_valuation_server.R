#' 4M valuation server
#' @param id Module id.
#' @param state Shared application state.
#' @export
mod_valuation_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    cfg <- state$cfg
    methods <- m4_methods(cfg)

    editable <- shiny::reactive({
      r <- state$row()
      !is.null(r) && state$can("valuate") && r$status %in% status_assessment
    })
    shiny::observe(shinyjs::toggle("calculator", condition = editable()))
    shiny::observe(shinyjs::toggle("delete", condition = editable()))

    output$tag <- shiny::renderUI({
      r <- state$row()
      if (is.null(r)) return(NULL)
      pending <- r$req_valuation && !r$has_valuation
      htmltools::span(class = paste("dv-tag", if (pending) "dv-tag-gate"),
                      if (pending) "required \u00b7 pending" else if (r$req_valuation) "required" else "optional")
    })

    output$gate <- shiny::renderUI({
      r <- state$row()
      if (is.null(r)) return(callout(type = "info", "Register the initiative first."))
      gr <- gate_reasons(r, cfg)
      htmltools::tagList(
        # requirement of the next stage, driven by the 4M value and the cost
        if (r$req_review) callout(type = "gate", title = "Next stage required \u00b7 expert review",
                                  paste(gr$review, collapse = " \u00b7 "))
        else callout(type = "low", sprintf(
          "Below the review gate (value < %s mm USD and cost < %s mm USD) \u2013 expert review not required.",
          cfg$params$gate3_value_min_mm_usd, cfg$params$gate3_cost_min_mm_usd)),
        if (!state$can("valuate")) callout(type = "info", "4M valuation is recorded by superusers.")
        else if (!editable()) callout(type = "info", "Valuation is locked after the decision."))
    })

    shiny::observeEvent(input$metric, {
      shiny::updateSelectInput(session, "method", choices = m4_method_choices(input$metric, cfg))
    })
    output$params <- shiny::renderUI({
      shiny::req(input$method %in% names(methods))
      m <- methods[[input$method]]
      inputs <- lapply(m$params, function(p) {
        lbl <- if (nzchar(p$unit)) sprintf("%s (%s)", p$label, p$unit) else p$label
        if (p$type == "select") shiny::selectInput(ns(paste0("p_", p$name)), lbl, p$choices, p$default,
                                                   selectize = FALSE)
        else shiny::numericInput(ns(paste0("p_", p$name)), lbl, p$default)
      })
      do.call(bslib::layout_columns, c(list(col_widths = rep(6, length(inputs))), inputs))
    })
    calc <- shiny::reactive({
      shiny::req(input$method %in% names(methods))
      m <- methods[[input$method]]
      p <- lapply(m$params, function(s) input[[paste0("p_", s$name)]])
      names(p) <- vapply(m$params, `[[`, "", "name")
      tryCatch(m4_calculate(input$method, p, cfg), error = function(e) e)
    })
    output$calc_preview <- shiny::renderUI({
      c <- calc()
      if (inherits(c, "error")) return(callout(conditionMessage(c), type = "high"))
      htmltools::div(class = "dv-formula", c$formula_text)
    })

    lines <- shiny::reactive({
      state$version()
      db_get_m4_lines(state$con, state$selected() %||% "")
    })
    summary <- shiny::reactive(m4_summary(lines()))

    output$tiles <- shiny::renderUI({
      s <- summary()
      tile <- function(k, digits) {
        i <- s$metric == k
        kpi_tile(paste(k, "\u00b7", s$label[i]), fmt_num(s$value[i], digits, paste0(" ", s$unit[i])),
                 sprintf("%s mm USD \u00b7 %d line(s)", fmt_num(s$value_mm_usd[i], 2), s$n_lines[i]))
      }
      htmltools::div(class = "dv-kpis",
        tile("P", 0), tile("R", 3), tile("M", 2), tile("T", 1),
        kpi_tile("4M total", fmt_num(sum(s$value_mm_usd), 2, " mm USD"), "P, M, T annual \u00b7 R one-off",
                 "strong"))
    })

    output$lines <- DT::renderDT({
      l <- lines()
      dt_compact(data.frame(Metric = l$metric, Value = round(l$result_value, 3), Unit = l$result_unit,
                            `mm USD` = round(l$value_mm_usd, 3), Formula = l$formula_text,
                            Comment = l$comment, Parameters = l$params_json, By = l$created_by,
                            check.names = FALSE),
                 page_length = 6, dom = "tp", columnDefs = list(list(visible = FALSE, targets = 6)))
    })

    output$params_note <- shiny::renderUI({
      p <- cfg$params
      sprintf("Conversion: P \u00d7 %s d \u00d7 %s USD/bbl \u00b7 R \u00d7 value per bbl of the category \u00b7 T \u00d7 %s USD/h (salary \u00d7 %s).",
              p$days_per_year, p$netback_usd_bbl, fmt_num(time_value_usd_hour(cfg), 0),
              p$time_productivity_coef)
    })

    shiny::observeEvent(input$add, {
      c <- calc(); id <- state$selected()
      if (is.null(id) || inherits(c, "error") || !editable()) return()
      db_add_m4_line(state$con, id, c, if (nzchar(input$comment %||% "")) input$comment else NA,
                     state$user$user)
      shiny::updateTextAreaInput(session, "comment", value = "")
      state$refresh()
      shiny::showNotification(sprintf("%s: %s line added \u2013 status %s", id, c$metric,
                                      state$row()$status), type = "message")
    })
    shiny::observeEvent(input$delete, {
      i <- input$lines_rows_selected
      if (is.null(i) || !editable()) return()
      db_delete_m4_line(state$con, lines()$line_id[i])
      state$refresh()
    })
  })
}
