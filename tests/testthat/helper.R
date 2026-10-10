test_cfg <- function() load_config("csv", system.file("config", package = "digitalvalue"))

# DuckDB is the application store; SQLite is used only if DuckDB is unavailable
test_driver <- function() {
  d <- Sys.getenv("DV_TEST_DRIVER")
  if (nzchar(d)) return(d)
  if (requireNamespace("duckdb", quietly = TRUE)) "duckdb" else "sqlite"
}

# fresh in-memory database with schema
test_con <- function(env = parent.frame()) {
  con <- db_connect(":memory:", test_driver())
  db_init(con)
  withr::defer(try(db_disconnect(con), silent = TRUE), envir = env)
  con
}

# register an initiative with RICE in one call
reg <- function(con, cfg, name = "X", users = 1000, impact = "L", confidence = "High",
                effort = "M", cost = 0.5, user = "u1") {
  db_register_initiative(con, list(name = name, cost_mm_usd = cost),
                         rice_assess(users, impact, confidence, effort, cfg), user)
}
