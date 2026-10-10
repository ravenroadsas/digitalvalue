#' Initiative tab server
#' @param id Module id.
#' @param state Shared application state (see [app_server()]).
#' @export
mod_initiative_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # selection <-> shared state; NULL selection = registering a new initiative
    shiny::observe({
      pf <- state$portfolio()
      ch <- stats::setNames(pf$id, paste(pf$id, "\u00b7", pf$name))
      sel <- shiny::isolate(state$selected())
      first <- shiny::isolate(input$select) %||% ""
      if (is.null(sel) && !nzchar(first) && length(ch) && !isTRUE(session$userData$new_mode)) {
        sel <- ch[[length(ch)]]
        state$selected(sel)
      }
      shiny::updateSelectizeInput(session, "select", choices = ch, selected = sel %||% character(0))
    })
    shiny::observeEvent(input$select, {
      if (nzchar(input$select) && !identical(input$select, state$selected())) {
        session$userData$new_mode <- FALSE
        state$selected(input$select)
      }
    })
    shiny::observeEvent(state$selected(), ignoreNULL = FALSE, {
      sel <- state$selected()
      if (!is.null(sel)) session$userData$new_mode <- FALSE
      if (!identical(input$select, sel %||% ""))
        shiny::updateSelectizeInput(session, "select", selected = sel %||% character(0))
    })
    shiny::observeEvent(input$new, {
      session$userData$new_mode <- TRUE
      state$selected(NULL)
      state$stage("appraisal")
    })
    shiny::observeEvent(state$stage(), bslib::nav_select("stage", state$stage()))
    shiny::observeEvent(input$stage, if (!identical(input$stage, state$stage())) state$stage(input$stage))

    output$headline <- shiny::renderUI({
      r <- state$row()
      if (is.null(r)) return(htmltools::span(class = "dv-muted", "New initiative \u2013 fill in registration and RICE below."))
      htmltools::tagList(
        status_badge(r$status),
        htmltools::span(class = "dv-headline-meta",
          sprintf("%s \u00b7 %s \u00b7 registered by %s on %s", r$business_unit %||% "\u2013",
                  r$owner %||% "\u2013", r$created_by, substr(r$created_at, 1, 10))))
    })

    output$stepper <- shiny::renderUI({
      r <- state$row()
      if (is.null(r)) return(NULL)
      st <- lifecycle_steps(r)
      phase <- function(ph) htmltools::div(class = "dv-phase",
        htmltools::div(class = "dv-phase-label", if (ph == "Appraisal") "Appraisal \u00b7 pre-execution"
                                                  else "Realisation \u00b7 post-execution"),
        htmltools::tags$ol(class = "dv-steps", lapply(which(st$phase == ph), function(i)
          htmltools::tags$li(class = st$state[i],
            htmltools::span(class = "dv-step-n", st$step[i]),
            htmltools::span(class = "dv-step-label", st$label[i]),
            htmltools::span(class = "dv-step-note", st$note[i])))))
      htmltools::div(class = "dv-stepper", phase("Appraisal"), phase("Realisation"))
    })

    action_labels <- c(prioritize = "Prioritize", reject = "Reject", start = "Start execution",
                       deprioritize = "Back to ready", close = "Close execution", reopen = "Re-open")
    output$workflow <- shiny::renderUI({
      r <- state$row()
      if (is.null(r)) return(NULL)
      acts <- allowed_actions(r$status)
      if (!state$can("decide") || !length(acts)) return(NULL)
      htmltools::div(class = "dv-decision",
        htmltools::span(class = "dv-decision-label", "Decision"),
        shiny::textInput(ns("decision_comment"), NULL, placeholder = "Comment (optional)", width = "320px"),
        lapply(names(acts), function(a)
          shiny::actionButton(ns(paste0("act_", a)), action_labels[[a]],
                              class = paste("btn-sm", if (a %in% c("prioritize", "start", "close"))
                                "btn-primary" else "btn-outline-secondary"))),
        if (r$status %in% status_assessment && r$status != "Ready")
          htmltools::span(class = "dv-muted", "Complete the pending appraisal steps to enable prioritisation."))
    })
    lapply(names(action_labels), function(a) {
      shiny::observeEvent(input[[paste0("act_", a)]], {
        r <- state$row()
        target <- allowed_actions(r$status)[a]
        if (is.na(target) || !state$can("decide")) return()
        db_set_status(state$con, r$id, unname(target), state$user$user,
                      if (nzchar(input$decision_comment %||% "")) input$decision_comment else NA)
        state$refresh()
        if (target %in% c("In execution", "Closed")) state$stage("realisation")
        shiny::showNotification(sprintf("%s \u2192 %s", r$id, target), type = "message")
      }, ignoreInit = TRUE)
    })

    mod_register_server("register", state, new_mode = function() isTRUE(session$userData$new_mode))
    mod_valuation_server("valuation", state)
    mod_review_server("review", state)
    mod_audit_server("audit", state)
  })
}
