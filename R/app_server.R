#' Application server
#' @param input,output,session Shiny objects.
#' @export
app_server <- function(input, output, session) {
  con <- .dv$con
  cfg <- .dv$cfg
  usr <- app_user(session)

  version <- shiny::reactiveVal(0)
  selected <- shiny::reactiveVal(NULL)

  portfolio <- shiny::reactive({
    version()
    compute_portfolio(db_portfolio(con), cfg)
  })
  history <- shiny::reactive({
    version()
    db_get_status_history(con)
  })

  state <- list(
    con = con, cfg = cfg, user = usr,
    portfolio = portfolio, history = history, selected = selected, version = version,
    # call after every write: re-derives statuses and refreshes all views
    refresh = function() {
      sync_status(con, cfg, usr$user)
      version(shiny::isolate(version()) + 1)
    },
    goto = function(tab) bslib::nav_select("nav", tab, session = session),
    row = shiny::reactive({
      id <- selected()
      pf <- portfolio()
      if (is.null(id) || !id %in% pf$id) NULL else pf[pf$id == id, , drop = FALSE]
    })
  )

  mod_sidebar_server("sidebar", state)
  mod_overview_server("overview", state)
  mod_register_server("register", state)
  mod_phase1_server("phase1", state)
  mod_phase2_server("phase2", state)
  mod_phase3_server("phase3", state)
  mod_audit_server("audit", state)
  mod_process_server("process", state)
  mod_parameters_server("parameters", state)

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
