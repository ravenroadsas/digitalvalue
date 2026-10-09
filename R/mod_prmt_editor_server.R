#' PRMT calculation editor server
#' @param id Module id.
#' @param state Shared application state.
#' @param stage `"plan"` or `"actual"`.
#' @param enabled Reactive returning `TRUE` when editing is allowed.
#' @return Reactive with the stored lines of the active initiative.
#' @export
mod_prmt_editor_server <- function(id, state, stage = "plan", enabled = shiny::reactive(TRUE)) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    cfg <- state$cfg
    methods <- prmt_methods(cfg)

    shiny::observeEvent(input$metric, {
      shiny::updateSelectInput(session, "method", choices = prmt_method_choices(input$metric, cfg))
    })

    output$params <- shiny::renderUI({
      shiny::req(input$method %in% names(methods))
      m <- methods[[input$method]]
      inputs <- lapply(m$params, function(p) {
        lbl <- if (nzchar(p$unit)) sprintf("%s (%s)", p$label, p$unit) else p$label
        if (p$type == "select") shiny::selectInput(ns(paste0("p_", p$name)), lbl, p$choices, p$default)
        else shiny::numericInput(ns(paste0("p_", p$name)), lbl, p$default)
      })
      do.call(bslib::layout_columns, c(list(col_widths = rep(6, length(inputs))), inputs))
    })

    calc <- shiny::reactive({
      shiny::req(input$method %in% names(methods))
      m <- methods[[input$method]]
      p <- lapply(m$params, function(s) input[[paste0("p_", s$name)]])
      names(p) <- vapply(m$params, `[[`, "", "name")
      tryCatch(prmt_calculate(input$method, p, cfg), error = function(e) e)
    })

    output$preview <- shiny::renderUI({
      c <- calc()
      if (inherits(c, "error")) return(callout(conditionMessage(c), type = "danger"))
      htmltools::div(class = "dv-formula", c$formula_text)
    })

    lines <- shiny::reactive({
      state$version()
      id <- state$selected()
      if (is.null(id)) return(db_get_prmt_lines(state$con, "", stage))
      db_get_prmt_lines(state$con, id, stage)
    })

    shiny::observeEvent(input$add, {
      c <- calc(); id <- state$selected()
      if (is.null(id) || inherits(c, "error")) return()
      if (!isTRUE(enabled())) {
        shiny::showNotification("Editing is not allowed at this stage", type = "warning"); return()
      }
      db_add_prmt_line(state$con, id, c, stage,
                       if (nzchar(input$comment %||% "")) input$comment else NA, state$user$user)
      shiny::updateTextAreaInput(session, "comment", value = "")
      state$refresh()
      shiny::showNotification(sprintf("%s: %s line added (%s)", id, c$metric, stage), type = "message")
    })

    output$lines <- DT::renderDT({
      l <- lines()
      dt_compact(data.frame(Metric = l$metric, Method = l$method,
                            Value = round(l$result_value, 3), Unit = l$result_unit,
                            `mm USD` = round(l$value_mm_usd, 3), Formula = l$formula_text,
                            Comment = l$comment, Parameters = l$params_json,
                            By = l$created_by, check.names = FALSE),
                 page_length = 8, columnDefs = list(list(visible = FALSE, targets = 7)))
    })

    shiny::observeEvent(input$delete, {
      i <- input$lines_rows_selected
      if (is.null(i) || !isTRUE(enabled())) return()
      db_delete_prmt_line(state$con, lines()$line_id[i])
      state$refresh()
    })

    lines
  })
}
