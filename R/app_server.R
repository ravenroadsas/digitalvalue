#' Application server
#' @param input,output,session Shiny objects.
#' @export
app_server <- function(input, output, session) {
  con <- .dv$con
  cfg <- .dv$cfg
  usr <- app_user(session)

  version <- shiny::reactiveVal(0)
  selected <- shiny::reactiveVal(NULL)
  stage <- shiny::reactiveVal("evaluation")

  # changes made by other users (other sessions) are picked up every few seconds
  shared <- shiny::reactivePoll(5000, session, checkFunc = function() db_change_stamp(con),
                                valueFunc = function() Sys.time())
  shiny::observeEvent(shared(), version(shiny::isolate(version()) + 1), ignoreInit = TRUE)

  portfolio <- shiny::reactive({
    version()
    compute_portfolio(db_portfolio(con), cfg)
  })
  history <- shiny::reactive({
    version()
    db_get_status_history(con)
  })
  # published (active) value models; they change only when a superuser publishes
  models <- shiny::reactive({
    version()
    stats::setNames(lapply(names(value_model_kinds), function(k) db_active_value_model(con, k)),
                    names(value_model_kinds))
  })

  state <- list(
    con = con, cfg = cfg, user = usr,
    can = function(action, row = NULL) can(usr, action, row),
    # Posit Connect user directory (owners, validators), cached for an hour
    users = shiny::reactive(user_directory()),
    portfolio = portfolio, history = history, models = models,
    selected = selected, stage = stage, version = version,
    # call after every write: re-derives statuses and refreshes all views
    refresh = function() {
      sync_status(con, cfg, usr$user)
      version(shiny::isolate(version()) + 1)
    },
    # open an initiative in the Initiative tab, optionally on a lifecycle view
    open = function(id, view = NULL) {
      selected(id)
      if (!is.null(view)) stage(view)
      bslib::nav_select("nav", "initiative", session = session)
    },
    row = shiny::reactive({
      id <- selected()
      pf <- portfolio()
      if (is.null(id) || !id %in% pf$id) NULL else pf[pf$id == id, , drop = FALSE]
    })
  )

  output$whoami <- shiny::renderUI({
    htmltools::span(class = "dv-whoami", usr$user,
                    htmltools::span(class = paste("dv-role", usr$role), usr$role))
  })

  mod_initiative_server("initiative", state)
  mod_portfolio_server("portfolio", state)
  mod_methodology_server("methodology", state)
  if (can(usr, "admin")) {
    mod_models_server("models", state)
    mod_process_server("process", state)
    mod_parameters_server("parameters", state)
  } else {
    bslib::nav_remove("nav", "admin", session = session)
  }

  # Process mining: raw activity batches buffered client-side
  shiny::observeEvent(input$dv_raw_events, {
    # logging must never break the user session
    tryCatch({
      ev <- normalize_raw_batch(input$dv_raw_events, session$token, usr$user,
                                shiny::isolate(selected()))
      db_log_events(con, ev)
    }, error = function(e) warning("Activity logging failed: ", conditionMessage(e), call. = FALSE))
  })
}
