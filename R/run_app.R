#' Run the Digital Value Shiny application
#'
#' Opens the database (`DV_DB_PATH`, `DV_DB_DRIVER`), creates the schema,
#' seeds demo data when the database is empty and `DV_SEED_DEMO` is not
#' `"false"`, and loads the configuration (pins or CSV).
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
      .dv$con <- con
      .dv$cfg <- cfg
      shiny::onStop(function() {
        db_disconnect(con)
        .dv$con <- NULL
      })
    },
    options = list(...)
  )
}

# Process-wide resources shared by all sessions (one DB connection per R process).
.dv <- new.env(parent = emptyenv())
