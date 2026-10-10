#' 4MC valuation server
#' @param id Module id.
#' @param state Shared application state.
#' @export
mod_valuation_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    cfg <- state$cfg
    methods <- m4_methods(cfg)

    # the owner records the 4MC until it is sent for validation; superusers always can
    editable <- shiny::reactive({
      r <- state$row()
      if (is.null(r) || !r$status %in% status_assessment || !state$can("valuate", r)) return(FALSE)
      state$user$role == "superuser" || !r$validation_status %in% c("pending", "validated")
    })
    shiny::observe({
      shinyjs::toggle("calculator", condition = editable())
      shinyjs::toggleClass("calc_grid", "dv-calc-hidden", condition = !editable())
    })
    shiny::observe(shinyjs::toggle("delete", condition = editable()))

    output$tag <- shiny::renderUI({
      r <- state$row()
      if (is.null(r)) return(NULL)
      pending <- r$req_valuation && !r$validated
      htmltools::span(class = paste("dv-tag", if (pending) "dv-tag-gate"),
                      if (pending) "required \u00b7 pending" else if (r$req_valuation) "required" else "optional")
    })

    output$gate <- shiny::renderUI({
      r <- state$row()
      if (is.null(r)) return(callout(type = "info", "Register the initiative first."))
      gr <- gate_reasons(r, cfg)
      htmltools::tagList(
        # requirement of the next stage, driven by the 4MC value and the cost
        if (r$req_review) callout(type = "gate", title = "Next stage required \u00b7 expert review",
                                  paste(gr$review, collapse = " \u00b7 "))
        else callout(type = "low", sprintf(
          "Below the review gate (value < %s mm USD and cost < %s mm USD) \u2013 expert review not required.",
          cfg$params$gate3_value_min_mm_usd, cfg$params$gate3_cost_min_mm_usd)),
        if (!state$can("valuate", r)) callout(type = "info",
          "The 4MC valuation is recorded by the initiative owner or a superuser.")
        else if (!editable()) callout(type = "info", if (r$status %in% status_decision)
          "Valuation is locked after delivery." else
          "Valuation is locked while it is under validation or once validated (a superuser can still edit it)."))
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
      value <- sum(s$value_mm_usd[s$metric %in% m4_value_metrics])
      cost <- s$value[s$metric == "C"]
      htmltools::div(class = "dv-kpis",
        tile("P", 0), tile("R", 3), tile("M", 2), tile("T", 1),
        kpi_tile("C \u00b7 Cost", fmt_num(cost, 2, " mm USD"),
                 sprintf("%d line(s) \u00b7 value/cost %s", s$n_lines[s$metric == "C"],
                         fmt_num(if (cost > 0 && value > 0) value / cost else NA, 1, "x"))),
        kpi_tile("4MC value", fmt_num(value, 2, " mm USD"), "P, M, T annual \u00b7 R one-off", "strong"))
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
      sprintf("Conversion: P \u00d7 %s d \u00d7 %s USD/bbl \u00b7 R \u00d7 value per bbl of the category \u00b7 T \u00d7 %s USD/h (salary \u00d7 %s) \u00b7 C = cost in mm USD, kept separate from value.",
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
    # validation workflow ------------------------------------------------------
    validations <- shiny::reactive({ state$version(); db_get_validations(state$con, state$selected() %||% "") })

    output$validation <- shiny::renderUI({
      r <- state$row()
      if (is.null(r)) return(NULL)
      vs <- if (is.na(r$validation_status)) "" else r$validation_status
      v <- validations()
      last <- if (nrow(v)) v[v$status != "superseded", , drop = FALSE][1, ] else NULL
      who <- function(u) user_label(u, state$users())
      can_send <- state$can("valuate", r) && r$has_valuation && r$status %in% status_assessment &&
        (vs %in% c("", "changes_requested") || state$user$role == "superuser")
      send_form <- if (can_send) htmltools::tagList(
        shiny::selectizeInput(ns("validator"), "Send to validator (Posit Connect user)",
          choices = user_choices(state$users()[state$users()$username != state$user$user, ]),
          selected = character(0), width = "100%", options = list(placeholder = "Search users\u2026")),
        shiny::textInput(ns("request_comment"), NULL, placeholder = "Note to the validator (optional)",
                         width = "100%"),
        shiny::actionButton(ns("send"), if (vs == "pending") "Re-assign validation" else "Send to validator",
                            class = "btn-primary btn-sm"))
      status_box <- switch(vs,
        pending = callout(type = "gate", title = "Validation pending",
          sprintf("Sent to %s by %s on %s.", who(r$validator), who(last$requested_by),
                  substr(last$requested_at, 1, 10)),
          if (!is.na(last$request_comment)) htmltools::div(class = "dv-muted", last$request_comment)),
        validated = callout(type = "low", title = "Validated",
          sprintf("By %s on %s.", who(last$decided_by), substr(last$decided_at, 1, 10)),
          if (!is.na(last$decision_comment)) htmltools::div(class = "dv-muted", last$decision_comment)),
        changes_requested = callout(type = "gate", title = "Changes requested",
          sprintf("By %s on %s.", who(last$decided_by), substr(last$decided_at, 1, 10)),
          if (!is.na(last$decision_comment)) htmltools::div(last$decision_comment),
          htmltools::div(class = "dv-muted", "Update the calculation lines and send them again.")),
        if (!r$has_valuation) callout(type = if (r$req_valuation) "gate" else "low",
          if (r$req_valuation) "Record at least one value line, then send the 4MC to a validator."
          else "Optional: a 4MC valuation is not required for this initiative.")
        else callout(type = if (r$req_valuation) "gate" else "low", title = "Not yet sent",
          "Send the recorded 4MC figures to a validator."))
      decide_form <- if (vs == "pending" && state$can("validate", r)) htmltools::div(class = "dv-validate",
        htmltools::div(class = "dv-section", "Your validation"),
        shiny::textAreaInput(ns("decision_comment"), NULL, rows = 2, width = "100%",
                             placeholder = "Comment (required when requesting changes)"),
        htmltools::div(class = "dv-actions",
          shiny::actionButton(ns("validate"), "Validate 4MC", class = "btn-primary btn-sm"),
          shiny::actionButton(ns("request_changes"), "Request changes", class = "btn-outline-secondary btn-sm")))
      htmltools::tagList(status_box, decide_form, send_form)
    })

    output$validations <- DT::renderDT({
      v <- validations()
      u <- state$users()
      dt_compact(data.frame(Sent = substr(v$requested_at, 1, 10), Validator = user_label(v$validator, u),
                            Status = gsub("_", " ", v$status), Decided = substr(v$decided_at, 1, 10),
                            Comment = ifelse(is.na(v$decision_comment), v$request_comment, v$decision_comment),
                            check.names = FALSE), page_length = 4, dom = "tp", selection = "none")
    })

    shiny::observeEvent(input$send, {
      r <- state$row()
      if (is.null(r) || !state$can("valuate", r) || !r$has_valuation) return()
      if (!nzchar(input$validator %||% "")) {
        return(shiny::showNotification("Choose a validator", type = "error"))
      }
      db_request_validation(state$con, r$id, input$validator, state$user$user,
                            if (nzchar(input$request_comment %||% "")) input$request_comment else NA)
      state$refresh()
      shiny::showNotification(sprintf("%s: 4MC sent to %s for validation", r$id,
                                      user_label(input$validator, state$users())), type = "message")
    })
    decide <- function(decision) {
      r <- state$row()
      if (is.null(r) || !isTRUE(r$validation_status == "pending") || !state$can("validate", r)) return()
      cm <- input$decision_comment %||% ""
      if (decision == "changes_requested" && !nzchar(trimws(cm))) {
        return(shiny::showNotification("Explain which changes are needed", type = "error"))
      }
      db_decide_validation(state$con, r$validation_id, decision, state$user$user,
                           if (nzchar(cm)) cm else NA)
      state$refresh()
      shiny::showNotification(sprintf("%s: 4MC %s \u2013 status %s", r$id,
                                      if (decision == "validated") "validated" else "returned for changes",
                                      state$row()$status), type = "message")
    }
    shiny::observeEvent(input$validate, decide("validated"))
    shiny::observeEvent(input$request_changes, decide("changes_requested"))

    shiny::observeEvent(input$delete, {
      i <- input$lines_rows_selected
      if (is.null(i) || !editable()) return()
      db_delete_m4_line(state$con, lines()$line_id[i])
      state$refresh()
    })
  })
}
