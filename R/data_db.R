# -----------------------------------------------------------------------------
# Data access layer
#
# ALL database interaction of the app lives in this file. The rest of the code
# only calls the db_* functions below, so migrating from DuckDB (development)
# to an enterprise database (SQL Server, PostgreSQL, Oracle via odbc) only
# requires changes here:
#   * db_connect(): add the driver branch (e.g. odbc::odbc() + DSN from env).
#   * SQL is kept ANSI (CTEs, window functions, CASE) and uses `?` positional
#     parameters; PostgreSQL drivers expect `$1` -> see sql_params().
#   * Timestamps are stored as ISO-8601 UTC text for portability.
# -----------------------------------------------------------------------------

#' Default database driver
#'
#' `DV_DB_DRIVER` selects the driver (`duckdb`, `sqlite`). When unset DuckDB is
#' used if installed, SQLite otherwise.
#' @return A driver name.
#' @export
db_driver <- function() {
  d <- Sys.getenv("DV_DB_DRIVER")
  if (nzchar(d)) return(d)
  if (requireNamespace("duckdb", quietly = TRUE)) "duckdb" else "sqlite"
}

#' Connect to the application database
#'
#' @param path Database file (`DV_DB_PATH`), `":memory:"` for an in-memory DB.
#' @param driver Driver name, see [db_driver()].
#' @return A DBI connection.
#' @export
db_connect <- function(path = Sys.getenv("DV_DB_PATH", "digitalvalue.duckdb"),
                       driver = db_driver()) {
  switch(driver,
    duckdb = DBI::dbConnect(duckdb::duckdb(), dbdir = path),
    sqlite = DBI::dbConnect(RSQLite::SQLite(), path),
    stop("Unsupported DV_DB_DRIVER: ", driver)
  )
}

#' Disconnect (and shut down an embedded DuckDB instance)
#' @param con A DBI connection.
#' @export
db_disconnect <- function(con) {
  if (inherits(con, "duckdb_connection")) {
    DBI::dbDisconnect(con, shutdown = TRUE)
  } else {
    DBI::dbDisconnect(con)
  }
  invisible(TRUE)
}

# Schema ----------------------------------------------------------------------

db_schema <- c(
  initiatives = "CREATE TABLE IF NOT EXISTS initiatives (
    id VARCHAR PRIMARY KEY, name VARCHAR NOT NULL, description VARCHAR,
    owner VARCHAR, business_unit VARCHAR, cost_mm_usd DOUBLE,
    start_date VARCHAR, end_date VARCHAR, status VARCHAR NOT NULL,
    created_by VARCHAR, created_at VARCHAR, updated_at VARCHAR)",
  rice = "CREATE TABLE IF NOT EXISTS rice (
    initiative_id VARCHAR PRIMARY KEY, users DOUBLE, impact VARCHAR,
    confidence VARCHAR, effort VARCHAR, reach_value DOUBLE, score DOUBLE,
    rationale VARCHAR, scored_by VARCHAR, scored_at VARCHAR)",
  prmt_lines = "CREATE TABLE IF NOT EXISTS prmt_lines (
    line_id VARCHAR PRIMARY KEY, initiative_id VARCHAR NOT NULL,
    stage VARCHAR NOT NULL, metric VARCHAR NOT NULL, method VARCHAR NOT NULL,
    params_json VARCHAR, result_value DOUBLE, result_unit VARCHAR,
    value_mm_usd DOUBLE, formula_text VARCHAR, comment VARCHAR,
    created_by VARCHAR, created_at VARCHAR)",
  planning_reviews = "CREATE TABLE IF NOT EXISTS planning_reviews (
    review_id VARCHAR PRIMARY KEY, initiative_id VARCHAR NOT NULL,
    decision VARCHAR NOT NULL, validated_value_mm_usd DOUBLE,
    validated_cost_mm_usd DOUBLE, comment VARCHAR, reviewer VARCHAR,
    reviewed_at VARCHAR)",
  audits = "CREATE TABLE IF NOT EXISTS audits (
    audit_id VARCHAR PRIMARY KEY, initiative_id VARCHAR NOT NULL,
    planned_value_mm_usd DOUBLE, actual_value_mm_usd DOUBLE,
    realization_pct DOUBLE, comment VARCHAR, auditor VARCHAR,
    audited_at VARCHAR)",
  status_history = "CREATE TABLE IF NOT EXISTS status_history (
    initiative_id VARCHAR NOT NULL, from_status VARCHAR, to_status VARCHAR,
    changed_by VARCHAR, changed_at VARCHAR, comment VARCHAR)",
  event_log = "CREATE TABLE IF NOT EXISTS event_log (
    ts VARCHAR, session_id VARCHAR, user_name VARCHAR, initiative_id VARCHAR,
    source VARCHAR, raw_name VARCHAR, raw_value VARCHAR)"
)

#' Create the schema if it does not exist
#' @param con A DBI connection.
#' @return Invisibly, the table names.
#' @export
db_init <- function(con) {
  for (sql in db_schema) DBI::dbExecute(con, sql)
  invisible(names(db_schema))
}

# Helpers ---------------------------------------------------------------------

#' Current UTC timestamp as ISO text
#' @param time A POSIXct.
#' @return Character.
#' @export
now_utc <- function(time = Sys.time()) {
  format(time, "%Y-%m-%d %H:%M:%S", tz = "UTC")
}

new_uid <- function(prefix) {
  paste0(prefix, "-", format(Sys.time(), "%y%m%d%H%M%S", tz = "UTC"),
         "-", paste(sample(c(0:9, letters[1:6]), 6, TRUE), collapse = ""))
}

# Converts `?` placeholders for drivers that need numbered ones.
sql_params <- function(con, sql) {
  if (inherits(con, "PqConnection")) {
    i <- 0
    while (grepl("?", sql, fixed = TRUE)) {
      i <- i + 1
      sql <- sub("?", paste0("$", i), sql, fixed = TRUE)
    }
  }
  sql
}

db_exec <- function(con, sql, params = list()) {
  DBI::dbExecute(con, sql_params(con, sql), params = unname(params))
}

db_query <- function(con, sql, params = NULL) {
  if (is.null(params)) DBI::dbGetQuery(con, sql)
  else DBI::dbGetQuery(con, sql_params(con, sql), params = unname(params))
}

na_chr <- function(x) if (is.null(x) || length(x) == 0) NA_character_ else as.character(x)
na_num <- function(x) if (is.null(x) || length(x) == 0) NA_real_ else as.numeric(x)

# Initiatives -----------------------------------------------------------------

#' Next human-readable initiative id (DV-0001, DV-0002, ...)
#' @param con A DBI connection.
#' @return Character id.
#' @export
db_next_initiative_id <- function(con) {
  ids <- db_query(con, "SELECT id FROM initiatives")$id
  n <- suppressWarnings(as.integer(sub("^DV-", "", ids)))
  sprintf("DV-%04d", max(c(0L, n), na.rm = TRUE) + 1L)
}

#' Create an initiative
#'
#' @param con A DBI connection.
#' @param name,description,owner,business_unit Text fields.
#' @param cost_mm_usd Estimated implementation cost (mm USD).
#' @param start_date,end_date Planned dates (`Date` or text).
#' @param user User creating the record.
#' @param status Initial status.
#' @param time Creation time (for seeding historical data).
#' @return The new initiative id.
#' @export
db_add_initiative <- function(con, name, description = NA, owner = NA,
                              business_unit = NA, cost_mm_usd = NA,
                              start_date = NA, end_date = NA, user = "unknown",
                              status = "Phase I", time = Sys.time()) {
  if (!nzchar(trimws(na_chr(name)) %||% "")) stop("Initiative name is required")
  id <- db_next_initiative_id(con)
  ts <- now_utc(time)
  DBI::dbAppendTable(con, "initiatives", data.frame(
    id = id, name = trimws(name), description = na_chr(description),
    owner = na_chr(owner), business_unit = na_chr(business_unit),
    cost_mm_usd = na_num(cost_mm_usd), start_date = na_chr(start_date),
    end_date = na_chr(end_date), status = status, created_by = user,
    created_at = ts, updated_at = ts, stringsAsFactors = FALSE))
  db_add_status_history(con, id, NA, status, user, "Initiative registered", time)
  id
}

#' Update editable fields of an initiative
#' @param con A DBI connection.
#' @param id Initiative id.
#' @param fields Named list of columns to update.
#' @return Number of rows updated.
#' @export
db_update_initiative <- function(con, id, fields) {
  allowed <- c("name", "description", "owner", "business_unit", "cost_mm_usd",
               "start_date", "end_date")
  fields <- fields[intersect(names(fields), allowed)]
  if (!length(fields)) return(0L)
  fields$updated_at <- now_utc()
  set <- paste(paste0(names(fields), " = ?"), collapse = ", ")
  vals <- lapply(fields, function(v) if (is.null(v) || length(v) == 0) NA else v)
  db_exec(con, paste0("UPDATE initiatives SET ", set, " WHERE id = ?"),
          c(vals, list(id)))
}

#' Read initiatives
#' @param con A DBI connection.
#' @param id Optional id filter.
#' @return Data frame.
#' @export
db_get_initiatives <- function(con, id = NULL) {
  if (is.null(id)) db_query(con, "SELECT * FROM initiatives ORDER BY id")
  else db_query(con, "SELECT * FROM initiatives WHERE id = ?", list(id))
}

#' Change the status of an initiative and record it in the history
#' @param con A DBI connection.
#' @param id Initiative id.
#' @param status New status.
#' @param user User.
#' @param comment Optional comment.
#' @param time Change time.
#' @return `TRUE` if the status changed.
#' @export
db_set_status <- function(con, id, status, user = "unknown", comment = NA,
                          time = Sys.time()) {
  old <- db_query(con, "SELECT status FROM initiatives WHERE id = ?", list(id))$status
  if (!length(old)) stop("Unknown initiative: ", id)
  if (identical(old, status)) return(FALSE)
  db_exec(con, "UPDATE initiatives SET status = ?, updated_at = ? WHERE id = ?",
          list(status, now_utc(time), id))
  db_add_status_history(con, id, old, status, user, comment, time)
  TRUE
}

db_add_status_history <- function(con, id, from, to, user, comment = NA,
                                  time = Sys.time()) {
  DBI::dbAppendTable(con, "status_history", data.frame(
    initiative_id = id, from_status = na_chr(from), to_status = to,
    changed_by = user, changed_at = now_utc(time), comment = na_chr(comment),
    stringsAsFactors = FALSE))
}

#' Status history
#' @param con A DBI connection.
#' @return Data frame.
#' @export
db_get_status_history <- function(con) {
  db_query(con, "SELECT * FROM status_history ORDER BY changed_at")
}

# Phase I - RICE --------------------------------------------------------------

#' Save (upsert) the RICE assessment of an initiative
#' @param con A DBI connection.
#' @param id Initiative id.
#' @param rice List with `users`, `impact`, `confidence`, `effort`,
#'   `reach_value`, `score`, `rationale`.
#' @param user User.
#' @param time Scoring time.
#' @export
db_save_rice <- function(con, id, rice, user = "unknown", time = Sys.time()) {
  DBI::dbWithTransaction(con, {
    db_exec(con, "DELETE FROM rice WHERE initiative_id = ?", list(id))
    DBI::dbAppendTable(con, "rice", data.frame(
      initiative_id = id, users = na_num(rice$users), impact = na_chr(rice$impact),
      confidence = na_chr(rice$confidence), effort = na_chr(rice$effort),
      reach_value = na_num(rice$reach_value), score = na_num(rice$score),
      rationale = na_chr(rice$rationale), scored_by = user,
      scored_at = now_utc(time), stringsAsFactors = FALSE))
  })
  invisible(TRUE)
}

#' Read RICE assessments
#' @param con A DBI connection.
#' @return Data frame.
#' @export
db_get_rice <- function(con) db_query(con, "SELECT * FROM rice")

# Phase II / audit - PRMT calculation lines ------------------------------------

#' Store a PRMT calculation line
#'
#' Each line keeps the method, the full parameter set (JSON), the
#' human-readable formula and a free comment, so that the valuation can be
#' re-traced and re-computed.
#'
#' @param con A DBI connection.
#' @param id Initiative id.
#' @param calc Result of [prmt_calculate()].
#' @param stage `"plan"` (Phase II) or `"actual"` (audit).
#' @param comment Free comment.
#' @param user User.
#' @param time Creation time.
#' @return The line id.
#' @export
db_add_prmt_line <- function(con, id, calc, stage = c("plan", "actual"),
                             comment = NA, user = "unknown", time = Sys.time()) {
  stage <- match.arg(stage)
  line_id <- new_uid("L")
  DBI::dbAppendTable(con, "prmt_lines", data.frame(
    line_id = line_id, initiative_id = id, stage = stage, metric = calc$metric,
    method = calc$method, params_json = jsonlite::toJSON(calc$params, auto_unbox = TRUE),
    result_value = calc$value, result_unit = calc$unit,
    value_mm_usd = calc$value_mm_usd, formula_text = calc$formula_text,
    comment = na_chr(comment), created_by = user, created_at = now_utc(time),
    stringsAsFactors = FALSE))
  line_id
}

#' Delete a PRMT calculation line
#' @param con A DBI connection.
#' @param line_id Line id.
#' @export
db_delete_prmt_line <- function(con, line_id) {
  db_exec(con, "DELETE FROM prmt_lines WHERE line_id = ?", list(line_id))
}

#' Read PRMT calculation lines
#' @param con A DBI connection.
#' @param id Optional initiative id.
#' @param stage Optional stage filter.
#' @return Data frame.
#' @export
db_get_prmt_lines <- function(con, id = NULL, stage = NULL) {
  sql <- "SELECT * FROM prmt_lines WHERE 1 = 1"
  p <- list()
  if (!is.null(id)) { sql <- paste(sql, "AND initiative_id = ?"); p <- c(p, id) }
  if (!is.null(stage)) { sql <- paste(sql, "AND stage = ?"); p <- c(p, stage) }
  db_query(con, paste(sql, "ORDER BY created_at, line_id"), if (length(p)) p)
}

# Phase III - planning reviews --------------------------------------------------

#' Record a planning (Phase III) review
#' @param con A DBI connection.
#' @param id Initiative id.
#' @param decision `"Approve"`, `"Rework"` or `"Reject"`.
#' @param validated_value_mm_usd,validated_cost_mm_usd Planning figures.
#' @param comment Comment.
#' @param user Reviewer.
#' @param time Review time.
#' @return The review id.
#' @export
db_add_review <- function(con, id, decision, validated_value_mm_usd = NA,
                          validated_cost_mm_usd = NA, comment = NA,
                          user = "unknown", time = Sys.time()) {
  decision <- match.arg(decision, review_decisions)
  rid <- new_uid("R")
  DBI::dbAppendTable(con, "planning_reviews", data.frame(
    review_id = rid, initiative_id = id, decision = decision,
    validated_value_mm_usd = na_num(validated_value_mm_usd),
    validated_cost_mm_usd = na_num(validated_cost_mm_usd),
    comment = na_chr(comment), reviewer = user, reviewed_at = now_utc(time),
    stringsAsFactors = FALSE))
  rid
}

#' Read planning reviews
#' @param con A DBI connection.
#' @param id Optional initiative id.
#' @return Data frame.
#' @export
db_get_reviews <- function(con, id = NULL) {
  if (is.null(id)) db_query(con, "SELECT * FROM planning_reviews ORDER BY reviewed_at DESC")
  else db_query(con, "SELECT * FROM planning_reviews WHERE initiative_id = ? ORDER BY reviewed_at DESC", list(id))
}

# Audit -------------------------------------------------------------------------

#' Record a post-closure audit
#' @param con A DBI connection.
#' @param id Initiative id.
#' @param planned,actual Planned and actual value (mm USD).
#' @param comment Comment.
#' @param user Auditor.
#' @param time Audit time.
#' @return The audit id.
#' @export
db_add_audit <- function(con, id, planned, actual, comment = NA,
                         user = "unknown", time = Sys.time()) {
  aid <- new_uid("A")
  DBI::dbAppendTable(con, "audits", data.frame(
    audit_id = aid, initiative_id = id, planned_value_mm_usd = na_num(planned),
    actual_value_mm_usd = na_num(actual),
    realization_pct = realization_pct(actual, planned),
    comment = na_chr(comment), auditor = user, audited_at = now_utc(time),
    stringsAsFactors = FALSE))
  aid
}

#' Read audits
#' @param con A DBI connection.
#' @return Data frame.
#' @export
db_get_audits <- function(con) db_query(con, "SELECT * FROM audits ORDER BY audited_at DESC")

# Portfolio view (aggregation pushed to the database) ---------------------------

#' Portfolio: one row per initiative with RICE, PRMT totals, review and audit
#'
#' Aggregations run in the database engine (columnar in DuckDB) instead of in
#' the Shiny R process.
#' @param con A DBI connection.
#' @return Data frame.
#' @export
db_portfolio <- function(con) {
  agg <- function(stage, prefix) sprintf(
    "SELECT initiative_id,
       SUM(CASE WHEN metric = 'P' THEN result_value ELSE 0 END) AS %1$s_p,
       SUM(CASE WHEN metric = 'R' THEN result_value ELSE 0 END) AS %1$s_r,
       SUM(CASE WHEN metric = 'M' THEN result_value ELSE 0 END) AS %1$s_m,
       SUM(CASE WHEN metric = 'T' THEN result_value ELSE 0 END) AS %1$s_t,
       SUM(value_mm_usd) AS %1$s_value_mm_usd,
       COUNT(*) AS %1$s_lines
     FROM prmt_lines WHERE stage = '%2$s' GROUP BY initiative_id", prefix, stage)
  sql <- paste0(
    "WITH pl AS (", agg("plan", "plan"), "),
     ac AS (", agg("actual", "actual"), "),
     rv AS (SELECT * FROM (
        SELECT r.*, ROW_NUMBER() OVER (PARTITION BY initiative_id
                                       ORDER BY reviewed_at DESC, review_id DESC) AS rn
        FROM planning_reviews r) x WHERE rn = 1),
     au AS (SELECT * FROM (
        SELECT a.*, ROW_NUMBER() OVER (PARTITION BY initiative_id
                                       ORDER BY audited_at DESC, audit_id DESC) AS rn
        FROM audits a) y WHERE rn = 1)
     SELECT i.*, rc.users, rc.impact, rc.confidence, rc.effort, rc.reach_value,
       rc.score, rc.scored_at,
       COALESCE(pl.plan_p, 0) AS plan_p, COALESCE(pl.plan_r, 0) AS plan_r,
       COALESCE(pl.plan_m, 0) AS plan_m, COALESCE(pl.plan_t, 0) AS plan_t,
       COALESCE(pl.plan_value_mm_usd, 0) AS plan_value_mm_usd,
       COALESCE(pl.plan_lines, 0) AS plan_lines,
       COALESCE(ac.actual_p, 0) AS actual_p, COALESCE(ac.actual_r, 0) AS actual_r,
       COALESCE(ac.actual_m, 0) AS actual_m, COALESCE(ac.actual_t, 0) AS actual_t,
       COALESCE(ac.actual_value_mm_usd, 0) AS actual_value_mm_usd,
       COALESCE(ac.actual_lines, 0) AS actual_lines,
       rv.decision AS review_decision, rv.validated_value_mm_usd,
       rv.validated_cost_mm_usd, rv.reviewer, rv.reviewed_at,
       au.actual_value_mm_usd AS audited_value_mm_usd, au.audited_at
     FROM initiatives i
     LEFT JOIN rice rc ON rc.initiative_id = i.id
     LEFT JOIN pl ON pl.initiative_id = i.id
     LEFT JOIN ac ON ac.initiative_id = i.id
     LEFT JOIN rv ON rv.initiative_id = i.id
     LEFT JOIN au ON au.initiative_id = i.id
     ORDER BY i.id")
  out <- db_query(con, sql)
  # BIGINT counts may come back as integer64 depending on the driver
  for (k in c("plan_lines", "actual_lines")) out[[k]] <- as.numeric(out[[k]])
  out
}

# Process-mining event log -------------------------------------------------------

#' Append raw user-activity events
#' @param con A DBI connection.
#' @param events Data frame with columns `ts`, `session_id`, `user_name`,
#'   `initiative_id`, `source`, `raw_name`, `raw_value`.
#' @return Number of rows written.
#' @export
db_log_events <- function(con, events) {
  if (is.null(events) || !nrow(events)) return(0L)
  cols <- c("ts", "session_id", "user_name", "initiative_id", "source",
            "raw_name", "raw_value")
  events <- events[, cols, drop = FALSE]
  events[] <- lapply(events, as.character)
  DBI::dbAppendTable(con, "event_log", events)
}

#' Read raw user-activity events
#' @param con A DBI connection.
#' @param since Optional ISO timestamp lower bound.
#' @return Data frame.
#' @export
db_get_events <- function(con, since = NULL) {
  if (is.null(since)) db_query(con, "SELECT * FROM event_log ORDER BY ts")
  else db_query(con, "SELECT * FROM event_log WHERE ts >= ? ORDER BY ts", list(since))
}
