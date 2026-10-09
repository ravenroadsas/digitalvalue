test_cfg <- function() load_config("csv", system.file("config", package = "digitalvalue"))

test_driver <- function() {
  if (requireNamespace("duckdb", quietly = TRUE)) "duckdb" else "sqlite"
}

# fresh in-memory database with schema
test_con <- function(env = parent.frame()) {
  testthat::skip_if_not(requireNamespace("duckdb", quietly = TRUE) ||
                          requireNamespace("RSQLite", quietly = TRUE), "no DBI driver")
  con <- db_connect(":memory:", test_driver())
  db_init(con)
  withr::defer(try(db_disconnect(con), silent = TRUE), envir = env)
  con
}
