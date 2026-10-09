#' Sidebar server
#' @param id Module id.
#' @param state Shared application state (see [app_server()]).
#' @export
mod_sidebar_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    cfg <- state$cfg

    shiny::observe({
      pf <- state$portfolio()
      ch <- stats::setNames(pf$id, paste(pf$id, "\u00b7", pf$name))
      sel <- shiny::isolate(state$selected())
      if (is.null(sel) || !sel %in% pf$id) sel <- if (length(ch)) ch[[1]] else NULL
      shiny::updateSelectizeInput(session, "initiative", choices = ch, selected = sel)
      if (!identical(sel, shiny::isolate(state$selected()))) state$selected(sel)
    })
    shiny::observeEvent(input$initiative, {
      if (nzchar(input$initiative) && !identical(input$initiative, state$selected()))
        state$selected(input$initiative)
    })
    shiny::observeEvent(state$selected(), {
      if (!identical(input$initiative, state$selected()))
        shiny::updateSelectizeInput(session, "initiative", selected = state$selected())
    })

    output$summary <- shiny::renderUI({
      r <- state$row()
      if (is.null(r)) return(callout("Register an initiative to start.", type = "info"))
      htmltools::tagList(
        htmltools::div(class = "dv-sidebar-title", r$name),
        htmltools::div(status_badge(r$status), " ",
                       htmltools::span(class = "dv-sidebar-meta",
                                       paste(r$business_unit %||% "", "\u00b7", r$owner %||% ""))),
        htmltools::div(class = "dv-sidebar-meta", style = "margin-top:4px",
          sprintf("RICE %s \u00b7 Value %s mm USD \u00b7 Cost %s mm USD",
                  fmt_num(r$score, 2), fmt_num(r$planned_value_mm_usd, 2),
                  fmt_num(r$planned_cost_mm_usd, 2)))
      )
    })

    output$stepper <- shiny::renderUI({
      r <- state$row()
      if (is.null(r)) return(NULL)
      steps <- stage_steps(r)
      htmltools::tags$ul(class = "dv-steps", lapply(seq_len(nrow(steps)), function(i)
        htmltools::tags$li(class = steps$state[i], steps$label[i],
                           htmltools::span(class = "dv-muted", steps$note[i]))))
    })

    action_labels <- c(prioritize = "Prioritize", reject = "Reject", start = "Start execution",
                       deprioritize = "Back to ready", close = "Close", reopen = "Re-open")
    action_class <- c(prioritize = "btn-success", reject = "btn-outline-danger",
                      start = "btn-primary", deprioritize = "btn-outline-secondary",
                      close = "btn-secondary", reopen = "btn-outline-secondary")

    output$workflow <- shiny::renderUI({
      r <- state$row()
      if (is.null(r)) return(NULL)
      acts <- allowed_actions(r$status)
      if (!length(acts)) {
        msg <- if (r$status == "Closed") "Closed \u2013 record actual value in Execution & audit."
               else "No workflow action available."
        return(htmltools::div(class = "dv-muted", msg))
      }
      htmltools::tagList(
        if (r$status %in% status_assessment && r$status != "Ready")
          htmltools::div(class = "dv-muted", style = "margin-bottom:4px",
                         "Complete the pending assessment to enable prioritisation."),
        shiny::textInput(ns("comment"), NULL, placeholder = "Decision comment (optional)"),
        htmltools::div(class = "dv-actions", lapply(names(acts), function(a)
          shiny::actionButton(ns(paste0("act_", a)), action_labels[[a]],
                              class = paste("btn-sm", action_class[[a]]))))
      )
    })

    lapply(names(action_labels), function(a) {
      shiny::observeEvent(input[[paste0("act_", a)]], {
        r <- state$row()
        target <- allowed_actions(r$status)[a]
        if (is.na(target)) return()
        db_set_status(state$con, r$id, unname(target), state$user$user,
                      if (nzchar(input$comment %||% "")) input$comment else NA)
        state$refresh()
        shiny::showNotification(sprintf("%s \u2192 %s", r$id, target), type = "message")
      }, ignoreInit = TRUE)
    })

    output$user <- shiny::renderText({
      paste0("User: ", state$user$user, if (state$user$is_planning) " (planning)" else "",
             " \u00b7 config: ", cfg$source)
    })
  })
}

#' Process stage checklist for one initiative
#' @param r One portfolio row (see [compute_portfolio()]).
#' @return Data frame `label`, `state` (done/current/todo/na), `note`.
#' @export
stage_steps <- function(r) {
  st <- r$status
  post <- c("Prioritized", "In execution", "Closed", "Audited")
  reviewed <- !is.na(r$review_decision) && r$review_decision %in% c("Approve", "Reject")
  s <- data.frame(
    label = c("Registered", "Phase I \u00b7 RICE", "Phase II \u00b7 PRMT",
              "Phase III \u00b7 Planning", "Prioritized", "Execution", "Audit"),
    state = c(
      "done",
      if (r$has_rice) "done" else "current",
      if (!r$req_phase2 && !r$has_valuation) "na" else if (r$has_valuation) "done" else "current",
      if (!r$req_phase3) "na" else if (reviewed) "done" else "current",
      if (st %in% post) "done" else if (st == "Ready") "current" else "todo",
      if (st %in% c("Closed", "Audited")) "done" else if (st == "In execution") "current" else "todo",
      if (st == "Audited") "done" else if (st == "Closed") "current" else "todo"),
    note = c("",
             if (r$has_rice) sprintf(" score %.2f", r$score) else " pending",
             if (r$has_valuation) sprintf(" %.2f mm USD", r$plan_value_mm_usd)
             else if (r$req_phase2) " required" else " not required",
             if (!r$req_phase3) " not required" else if (reviewed) paste0(" ", r$review_decision) else " required",
             "", "", ""),
    stringsAsFactors = FALSE)
  if (st == "Rejected") s$state[s$state %in% c("current", "todo")] <- "na"
  # only the first pending step is "current"
  cur <- which(s$state == "current")
  if (length(cur) > 1) s$state[cur[-1]] <- "todo"
  s
}
