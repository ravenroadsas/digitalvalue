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
      state$stage("evaluation")
    })
    shiny::observeEvent(state$stage(), bslib::nav_select("stage", state$stage()))
    shiny::observeEvent(input$stage, if (!identical(input$stage, state$stage())) state$stage(input$stage))

    output$headline <- shiny::renderUI({
      r <- state$row()
      if (is.null(r)) return(htmltools::span(class = "dv-muted", "New initiative \u2013 fill in registration and RICE below."))
      htmltools::tagList(
        status_badge(r$status),
        if (!is.na(r$pending_step))
          htmltools::span(class = "dv-tag dv-tag-gate", paste("next:", r$pending_step)),
        htmltools::span(class = "dv-headline-meta",
          sprintf("%s \u00b7 owner %s \u00b7 registered by %s on %s", r$business_unit %||% "\u2013",
                  user_label(r$owner %||% "\u2013", state$users()), r$created_by,
                  substr(r$created_at, 1, 10))))
    })

    output$stepper <- shiny::renderUI({
      r <- state$row()
      if (is.null(r)) return(NULL)
      st <- lifecycle_steps(r)
      phase <- function(ph) htmltools::div(class = "dv-phase",
        htmltools::div(class = "dv-phase-label", if (ph == "Evaluation") "Evaluation \u00b7 before delivery"
                                                  else "Realisation \u00b7 after delivery"),
        htmltools::tags$ol(class = "dv-steps", lapply(which(st$phase == ph), function(i)
          htmltools::tags$li(class = st$state[i],
            htmltools::span(class = "dv-step-n", st$step[i]),
            htmltools::span(class = "dv-step-label", st$label[i]),
            htmltools::span(class = "dv-step-note", st$note[i])))))
      htmltools::div(class = "dv-stepper", phase("Evaluation"), phase("Realisation"))
    })

    # delivery: the owner or a superuser marks an evaluated initiative delivered
    output$workflow <- shiny::renderUI({
      r <- state$row()
      if (is.null(r)) return(NULL)
      if (r$status == "Evaluated" && state$can("deliver", r)) {
        return(htmltools::div(class = "dv-decision",
          htmltools::span(class = "dv-decision-label", "Delivery"),
          shiny::textInput(ns("decision_comment"), NULL, placeholder = "Comment (optional)", width = "320px"),
          shiny::actionButton(ns("act_deliver"), "Mark as delivered", class = "btn-sm btn-primary"),
          htmltools::span(class = "dv-muted", "Opens the value audit.")))
      }
      if (r$status == "Delivered" && state$can("undeliver", r)) {
        return(htmltools::div(class = "dv-decision",
          htmltools::span(class = "dv-decision-label", "Delivery"),
          shiny::textInput(ns("decision_comment"), NULL, placeholder = "Comment (optional)", width = "320px"),
          shiny::actionButton(ns("act_undeliver"), "Revert delivery", class = "btn-sm btn-outline-secondary")))
      }
      NULL
    })
    for (a in c("deliver", "undeliver")) local({
      act <- a
      shiny::observeEvent(input[[paste0("act_", act)]], {
        r <- state$row()
        target <- allowed_actions(r$status)[act]
        if (is.na(target) || !state$can(act, r)) return()
        db_set_status(state$con, r$id, unname(target), state$user$user,
                      if (nzchar(input$decision_comment %||% "")) input$decision_comment else NA)
        state$refresh()
        if (target == "Delivered") state$stage("realisation")
        shiny::showNotification(sprintf("%s \u2192 %s", r$id, target), type = "message")
      }, ignoreInit = TRUE)
    })

    mod_register_server("register", state, new_mode = function() isTRUE(session$userData$new_mode))
    mod_valuation_server("valuation", state)
    mod_review_server("review", state)
    mod_audit_server("audit", state)
  })
}
