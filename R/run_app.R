#' Run the Digital Value Shiny application
#'
#' Opens the database (`DV_DB_PATH`, `DV_DB_DRIVER`), creates the schema,
#' seeds demo data when the database is empty and `DV_SEED_DEMO` is not
#' `"false"`, takes the daily backup (`DV_BACKUP_DIR`) and loads the
#' configuration (pins or CSV). The database is released when the process
#' has no open session, so that another process (e.g. a new version of the
#' app) can open a DuckDB file; it is reopened by the next session.
#' @param ... Passed to [shiny::shinyApp()] `options`.
#' @return A shiny app object.
#' @export
run_app <- function(...) {
  shiny::addResourcePath("dv-www", system.file("app", "www", package = "digitalvalue"))
  shiny::shinyApp(
    ui = app_ui,
    server = app_server,
    onStart = function() {
      cfg <- load_config()
      con <- db_connect()
      db_init(con)
      if (!identical(tolower(Sys.getenv("DV_SEED_DEMO", "true")), "false")) seed_demo_data(con, cfg)
      # first calibration when data allow and nothing is published yet
      ensure_value_models(con, cfg, "system")
      db_backup(con)
      .dv$con <- con
      .dv$cfg <- cfg
      .dv$roles_at <- Sys.time()
      .dv$sessions <- 0L
      # an idle process does not keep the database file: the first session reopens it
      db_release()
      shiny::onStop(db_release)
    },
    options = list(...)
  )
}

# Process-wide resources shared by all sessions (one DB connection per R process).
.dv <- new.env(parent = emptyenv())

#' Acquire the process-wide database connection for a session
#'
#' Reopens the connection if it was released, counts the session and takes
#' the daily backup when due.
#' @return A DBI connection.
#' @export
db_acquire <- function() {
  if (is.null(.dv$con)) .dv$con <- db_connect()
  .dv$sessions <- (.dv$sessions %||% 0L) + 1L
  db_backup(.dv$con)
  .dv$con
}

#' Release the database connection when no session uses it
#' @param force Release even if sessions are open (process shutdown).
#' @export
db_release <- function(force = TRUE) {
  .dv$sessions <- max(0L, (.dv$sessions %||% 1L) - if (force) 0L else 1L)
  if (!is.null(.dv$con) && (force || .dv$sessions == 0L)) {
    try(db_disconnect(.dv$con), silent = TRUE)
    .dv$con <- NULL
  }
  invisible(TRUE)
}
