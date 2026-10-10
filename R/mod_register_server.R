#' Registration & RICE server
#'
#' Everyone can register an initiative with its RICE. Once saved the record is
#' locked; only superusers can edit it.
#' @param id Module id.
#' @param state Shared application state.
#' @param new_mode Function returning `TRUE` while registering a new initiative.
#' @export
mod_register_server <- function(id, state, new_mode = function() FALSE) {
  shiny::moduleServer(id, function(input, output, session) {
    cfg <- state$cfg
    fields <- c("name", "description", "owner", "business_unit", "cost_mm_usd", "start_date",
                "end_date", "users", "impact", "confidence", "effort", "rationale")
    shiny::updateSelectInput(session, "impact", choices = rice_levels(cfg, "impact"), selected = "M")
    shiny::updateSelectInput(session, "confidence", choices = rice_levels(cfg, "confidence"), selected = "Low")
    shiny::updateSelectInput(session, "effort", choices = rice_levels(cfg, "effort"), selected = "M")

    is_new <- shiny::reactive(is.null(state$selected()))
    editable <- shiny::reactive(is_new() || state$can("edit_registration"))

    output$title <- shiny::renderText({
      r <- state$row()
      if (is.null(r)) "1 \u00b7 Registration & RICE \u2013 new initiative"
      else paste("1 \u00b7 Registration & RICE \u00b7", r$id, r$name)
    })
    output$lock <- shiny::renderUI({
      if (is_new()) return(htmltools::span(class = "dv-tag", "open to everyone"))
      if (editable()) htmltools::span(class = "dv-tag", "superuser edit")
      else htmltools::span(class = "dv-tag dv-tag-dark", "locked after registration")
    })

    fill <- function(i = NULL, rc = NULL) {
      g <- function(x, k) if (is.null(x) || !nrow(x) || is.na(x[[k]])) "" else as.character(x[[k]])
      shiny::updateTextInput(session, "name", value = g(i, "name"))
      shiny::updateTextAreaInput(session, "description", value = g(i, "description"))
      shiny::updateTextInput(session, "owner", value = if (is.null(i)) state$user$user else g(i, "owner"))
      shiny::updateTextInput(session, "business_unit", value = g(i, "business_unit"))
      shiny::updateNumericInput(session, "cost_mm_usd", value = if (is.null(i)) NA else i$cost_mm_usd)
      if (nzchar(g(i, "start_date"))) shiny::updateDateInput(session, "start_date", value = g(i, "start_date"))
      if (nzchar(g(i, "end_date"))) shiny::updateDateInput(session, "end_date", value = g(i, "end_date"))
      has_rc <- !is.null(rc) && nrow(rc) > 0
      shiny::updateNumericInput(session, "users", value = if (has_rc) rc$users else 100)
      shiny::updateSelectInput(session, "impact", selected = if (has_rc) rc$impact else "M")
      shiny::updateSelectInput(session, "confidence", selected = if (has_rc) rc$confidence else "Low")
      shiny::updateSelectInput(session, "effort", selected = if (has_rc) rc$effort else "M")
      shiny::updateTextAreaInput(session, "rationale", value = g(rc, "rationale"))
    }
    shiny::observeEvent(state$selected(), ignoreNULL = FALSE, {
      id <- state$selected()
      if (is.null(id)) return(fill())
      rc <- db_get_rice(state$con)
      fill(db_get_initiatives(state$con, id), rc[rc$initiative_id == id, , drop = FALSE])
    })

    shiny::observe({
      ed <- editable()
      for (f in fields) shinyjs::toggleState(f, condition = ed)
      shinyjs::toggle("save", condition = is_new())
      shinyjs::toggle("update", condition = !is_new() && state$can("edit_registration"))
    })

    assessment <- shiny::reactive({
      shiny::req(input$impact, input$confidence, input$effort)
      tryCatch(rice_assess(input$users, input$impact, input$confidence, input$effort, cfg,
                           input$rationale), error = function(e) NULL)
    })

    output$preview <- shiny::renderUI({
      a <- assessment()
      if (is.null(a)) return(callout("Enter a valid number of users.", type = "high"))
      rec <- state$models()$rice_to_review
      est <- predict_value(rec, a$users, a$impact, a$confidence, a$effort, cfg)
      lab <- interval_labels(rec$level %||% 0.8)
      need_val <- requires_valuation(a$effort, a$score, cfg)
      htmltools::tagList(
        callout(type = "info", title = "RICE score",
          htmltools::div(class = "dv-score", fmt_num(a$score, 2))),
        htmltools::div(class = "dv-estimate",
          htmltools::div(class = "dv-estimate-label", "Anticipated ex-ante value"),
          if (is.na(est$predicted)) htmltools::div(class = "dv-muted", "No calibrated model published yet.")
          else htmltools::tagList(
            htmltools::div(class = "dv-estimate-value", sprintf("%s mm USD", fmt_num(est$predicted, 2)),
              htmltools::span(sprintf("%s\u2013%s  %s \u2013 %s", lab[1], lab[2],
                                      fmt_num(est$low, 2), fmt_num(est$high, 2)))),
            htmltools::div(class = "dv-muted", sprintf("Model %s \u00b7 n = %d \u00b7 R\u00b2 %s \u00b7 published %s",
              rec$type, rec$n, fmt_num(rec$r2, 2), substr(rec$created_at %||% "", 1, 10))))),
        callout(type = if (need_val) "medium" else "low",
          title = if (need_val) "4M valuation will be required" else "RICE is sufficient for a decision",
          if (need_val) sprintf("Effort \u2265 %s or score \u2265 %s.", cfg$params$gate2_effort_min,
                                cfg$params$gate2_score_min)
          else "Below the valuation gate; a 4M valuation is optional.")
      )
    })

    form <- function() {
      d <- function(x) if (length(x) && !is.na(x)) format(x) else NA
      list(name = input$name, description = input$description, owner = input$owner,
           business_unit = input$business_unit, cost_mm_usd = input$cost_mm_usd,
           start_date = d(input$start_date), end_date = d(input$end_date))
    }

    shiny::observeEvent(input$save, {
      if (!is_new() || !state$can("register")) return()
      a <- assessment()
      if (!nzchar(trimws(input$name))) return(shiny::showNotification("Name is required", type = "error"))
      if (is.null(a)) return(shiny::showNotification("Complete the RICE inputs", type = "error"))
      id <- db_register_initiative(state$con, form(), a, state$user$user)
      state$refresh()
      state$selected(id)
      shiny::showNotification(sprintf("%s registered \u2013 RICE %.2f, status %s", id, a$score,
                                      state$row()$status), type = "message")
    })

    shiny::observeEvent(input$update, {
      id <- state$selected()
      a <- assessment()
      if (is.null(id) || is.null(a) || !state$can("edit_registration")) return()
      if (!nzchar(trimws(input$name))) return(shiny::showNotification("Name is required", type = "error"))
      db_update_initiative(state$con, id, form())
      db_save_rice(state$con, id, a, state$user$user)
      state$refresh()
      shiny::showNotification(paste(id, "updated"), type = "message")
    })
  })
}
