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
#' Data are stored in DuckDB. `DV_DB_DRIVER` can select another driver
#' (`sqlite` is supported for lightweight testing only).
#' @return A driver name.
#' @export
db_driver <- function() {
  d <- Sys.getenv("DV_DB_DRIVER")
  if (nzchar(d)) d else "duckdb"
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
  m4_lines = "CREATE TABLE IF NOT EXISTS m4_lines (
    line_id VARCHAR PRIMARY KEY, initiative_id VARCHAR NOT NULL,
    metric VARCHAR NOT NULL, method VARCHAR NOT NULL,
    params_json VARCHAR, result_value DOUBLE, result_unit VARCHAR,
    value_mm_usd DOUBLE, formula_text VARCHAR, comment VARCHAR,
    created_by VARCHAR, created_at VARCHAR)",
  reviews = "CREATE TABLE IF NOT EXISTS reviews (
    review_id VARCHAR PRIMARY KEY, initiative_id VARCHAR NOT NULL,
    decision VARCHAR NOT NULL, p_bopd DOUBLE, r_mmbbl DOUBLE, r_category VARCHAR,
    m_mm_usd DOUBLE, t_khours DOUBLE, value_mm_usd DOUBLE, cost_mm_usd DOUBLE,
    comment VARCHAR, reviewer VARCHAR, reviewed_at VARCHAR)",
  audits = "CREATE TABLE IF NOT EXISTS audits (
    audit_id VARCHAR PRIMARY KEY, initiative_id VARCHAR NOT NULL,
    p_bopd DOUBLE, r_mmbbl DOUBLE, r_category VARCHAR, m_mm_usd DOUBLE,
    t_khours DOUBLE, adoption_pct DOUBLE, actual_value_mm_usd DOUBLE,
    expected_value_mm_usd DOUBLE, realization_pct DOUBLE, comment VARCHAR,
    auditor VARCHAR, audited_at VARCHAR)",
  value_models = "CREATE TABLE IF NOT EXISTS value_models (
    model_id VARCHAR PRIMARY KEY, kind VARCHAR NOT NULL, active INTEGER NOT NULL,
    type VARCHAR, n INTEGER, r2 DOUBLE, adj_r2 DOUBLE, sigma DOUBLE,
    df_residual INTEGER, level DOUBLE, terms_json VARCHAR, coef_json VARCHAR,
    vcov_json VARCHAR, comment VARCHAR, created_by VARCHAR, created_at VARCHAR)",
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
                              status = "Registered", time = Sys.time()) {
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

# RICE --------------------------------------------------------------

#' Save (upsert) the RICE assessment of an initiative
#' @param con A DBI connection.
#' @param id Initiative id.
#' @param rice List with `users`, `impact`, `confidence`, `effort`,
#'   `reach_value`, `score`, `rationale`.
#' @param user User.
#' @param time Scoring time.
#' @export
db_save_rice <- function(con, id, rice, user = "unknown", time = Sys.time()) {
  DBI::dbWithTransaction(con, write_rice(con, id, rice, user, time))
  invisible(TRUE)
}

write_rice <- function(con, id, rice, user, time) {
  db_exec(con, "DELETE FROM rice WHERE initiative_id = ?", list(id))
  DBI::dbAppendTable(con, "rice", data.frame(
    initiative_id = id, users = na_num(rice$users), impact = na_chr(rice$impact),
    confidence = na_chr(rice$confidence), effort = na_chr(rice$effort),
    reach_value = na_num(rice$reach_value), score = na_num(rice$score),
    rationale = na_chr(rice$rationale), scored_by = user,
    scored_at = now_utc(time), stringsAsFactors = FALSE))
}

#' Register an initiative together with its RICE assessment
#'
#' Registration and RICE are recorded at once, in a single transaction.
#' @param con A DBI connection.
#' @param fields Named list: `name`, `description`, `owner`, `business_unit`,
#'   `cost_mm_usd`, `start_date`, `end_date`.
#' @param rice Output of [rice_assess()].
#' @param user User registering the initiative.
#' @param time Registration time.
#' @return The new initiative id.
#' @export
db_register_initiative <- function(con, fields, rice, user = "unknown", time = Sys.time()) {
  g <- function(k) fields[[k]] %||% NA
  id <- NULL
  DBI::dbWithTransaction(con, {
    id <- db_add_initiative(con, g("name"), g("description"), g("owner"), g("business_unit"),
                            g("cost_mm_usd"), g("start_date"), g("end_date"), user = user,
                            time = time)
    write_rice(con, id, rice, user, time)
  })
  id
}

#' Read RICE assessments
#' @param con A DBI connection.
#' @return Data frame.
#' @export
db_get_rice <- function(con) db_query(con, "SELECT * FROM rice")

# 4M valuation - calculation lines ---------------------------------------------

#' Store a 4M calculation line
#'
#' Each line keeps the method, the full parameter set (JSON), the
#' human-readable formula and a free comment, so that the valuation can be
#' re-traced and re-computed.
#'
#' @param con A DBI connection.
#' @param id Initiative id.
#' @param calc Result of [m4_calculate()].
#' @param comment Free comment.
#' @param user User.
#' @param time Creation time.
#' @return The line id.
#' @export
db_add_m4_line <- function(con, id, calc, comment = NA, user = "unknown", time = Sys.time()) {
  line_id <- new_uid("L")
  DBI::dbAppendTable(con, "m4_lines", data.frame(
    line_id = line_id, initiative_id = id, metric = calc$metric,
    method = calc$method, params_json = as.character(jsonlite::toJSON(calc$params, auto_unbox = TRUE)),
    result_value = calc$value, result_unit = calc$unit,
    value_mm_usd = calc$value_mm_usd, formula_text = calc$formula_text,
    comment = na_chr(comment), created_by = user, created_at = now_utc(time),
    stringsAsFactors = FALSE))
  line_id
}

#' Delete a 4M calculation line
#' @param con A DBI connection.
#' @param line_id Line id.
#' @export
db_delete_m4_line <- function(con, line_id) {
  db_exec(con, "DELETE FROM m4_lines WHERE line_id = ?", list(line_id))
}

#' Read 4M calculation lines
#' @param con A DBI connection.
#' @param id Optional initiative id.
#' @return Data frame.
#' @export
db_get_m4_lines <- function(con, id = NULL) {
  if (is.null(id)) db_query(con, "SELECT * FROM m4_lines ORDER BY created_at, line_id")
  else db_query(con, "SELECT * FROM m4_lines WHERE initiative_id = ? ORDER BY created_at, line_id",
                list(id))
}

# Expert review (manual evaluation) ---------------------------------------------

m4_frame <- function(v) {
  data.frame(p_bopd = na_num(v$P), r_mmbbl = na_num(v$R), r_category = na_chr(v$category),
             m_mm_usd = na_num(v$M), t_khours = na_num(v$T), stringsAsFactors = FALSE)
}

#' Record an expert review (manual evaluation) with its own 4M figures
#' @param con A DBI connection.
#' @param id Initiative id.
#' @param decision `"Approve"`, `"Rework"` or `"Reject"`.
#' @param values Named list `P`, `R`, `category`, `M`, `T` (native units).
#' @param value_mm_usd Monetary equivalent of `values` (see [m4_value()]).
#' @param cost_mm_usd Validated cost.
#' @param comment Comment.
#' @param user Reviewer.
#' @param time Review time.
#' @return The review id.
#' @export
db_add_review <- function(con, id, decision, values = list(), value_mm_usd = NA,
                          cost_mm_usd = NA, comment = NA, user = "unknown", time = Sys.time()) {
  decision <- match.arg(decision, review_decisions)
  rid <- new_uid("R")
  DBI::dbAppendTable(con, "reviews", cbind(
    data.frame(review_id = rid, initiative_id = id, decision = decision, stringsAsFactors = FALSE),
    m4_frame(values),
    data.frame(value_mm_usd = na_num(value_mm_usd), cost_mm_usd = na_num(cost_mm_usd),
               comment = na_chr(comment), reviewer = user, reviewed_at = now_utc(time),
               stringsAsFactors = FALSE)))
  rid
}

#' Read expert reviews
#' @param con A DBI connection.
#' @param id Optional initiative id.
#' @return Data frame.
#' @export
db_get_reviews <- function(con, id = NULL) {
  if (is.null(id)) db_query(con, "SELECT * FROM reviews ORDER BY reviewed_at DESC")
  else db_query(con, "SELECT * FROM reviews WHERE initiative_id = ? ORDER BY reviewed_at DESC", list(id))
}

# Value audit (post execution) ----------------------------------------------------

#' Record a post-execution value audit: actual 4M figures plus adoption
#' @param con A DBI connection.
#' @param id Initiative id.
#' @param values Named list `P`, `R`, `category`, `M`, `T` (actual, native units).
#' @param adoption_pct Share of the intended users actually using the solution.
#' @param actual_value_mm_usd Monetary equivalent of `values`.
#' @param expected_value_mm_usd Ex-ante value the audit is compared with.
#' @param comment Comment.
#' @param user Auditor.
#' @param time Audit time.
#' @return The audit id.
#' @export
db_add_audit <- function(con, id, values = list(), adoption_pct = NA, actual_value_mm_usd = NA,
                         expected_value_mm_usd = NA, comment = NA, user = "unknown",
                         time = Sys.time()) {
  aid <- new_uid("A")
  DBI::dbAppendTable(con, "audits", cbind(
    data.frame(audit_id = aid, initiative_id = id, stringsAsFactors = FALSE),
    m4_frame(values),
    data.frame(adoption_pct = na_num(adoption_pct), actual_value_mm_usd = na_num(actual_value_mm_usd),
               expected_value_mm_usd = na_num(expected_value_mm_usd),
               realization_pct = realization_pct(na_num(actual_value_mm_usd), na_num(expected_value_mm_usd)),
               comment = na_chr(comment), auditor = user, audited_at = now_utc(time),
               stringsAsFactors = FALSE)))
  aid
}

#' Read value audits
#' @param con A DBI connection.
#' @param id Optional initiative id.
#' @return Data frame.
#' @export
db_get_audits <- function(con, id = NULL) {
  if (is.null(id)) db_query(con, "SELECT * FROM audits ORDER BY audited_at DESC")
  else db_query(con, "SELECT * FROM audits WHERE initiative_id = ? ORDER BY audited_at DESC", list(id))
}

# Calibrated value models ----------------------------------------------------------

#' Publish a calibrated value model (new active version)
#'
#' Previous versions of the same kind are kept but deactivated.
#' @param con A DBI connection.
#' @param record Output of [model_record()].
#' @param comment Calibration comment.
#' @param user Superuser publishing the model.
#' @param time Publication time.
#' @return The model id.
#' @export
db_publish_value_model <- function(con, record, comment = NA, user = "unknown", time = Sys.time()) {
  mid <- new_uid("M")
  DBI::dbWithTransaction(con, {
    db_exec(con, "UPDATE value_models SET active = 0 WHERE kind = ?", list(record$kind))
    DBI::dbAppendTable(con, "value_models", data.frame(
      model_id = mid, kind = record$kind, active = 1L, type = record$type,
      n = as.integer(record$n), r2 = record$r2, adj_r2 = record$adj_r2, sigma = record$sigma,
      df_residual = as.integer(record$df_residual), level = record$level,
      terms_json = as.character(jsonlite::toJSON(record$terms)),
      coef_json = as.character(jsonlite::toJSON(as.list(record$coef), auto_unbox = TRUE, digits = NA)),
      vcov_json = as.character(jsonlite::toJSON(unname(record$vcov), digits = NA)),
      comment = na_chr(comment), created_by = user, created_at = now_utc(time),
      stringsAsFactors = FALSE))
  })
  mid
}

#' Re-activate a previously published model version (rollback)
#' @param con A DBI connection.
#' @param model_id Model id.
#' @export
db_activate_value_model <- function(con, model_id) {
  kind <- db_query(con, "SELECT kind FROM value_models WHERE model_id = ?", list(model_id))$kind
  if (!length(kind)) stop("Unknown model: ", model_id)
  DBI::dbWithTransaction(con, {
    db_exec(con, "UPDATE value_models SET active = 0 WHERE kind = ?", list(kind))
    db_exec(con, "UPDATE value_models SET active = 1 WHERE model_id = ?", list(model_id))
  })
  invisible(TRUE)
}

#' Read published value models
#' @param con A DBI connection.
#' @param kind Optional model kind.
#' @return Data frame (one row per version, newest first).
#' @export
db_get_value_models <- function(con, kind = NULL) {
  if (is.null(kind)) db_query(con, "SELECT * FROM value_models ORDER BY created_at DESC, model_id DESC")
  else db_query(con, "SELECT * FROM value_models WHERE kind = ? ORDER BY created_at DESC, model_id DESC",
                list(kind))
}

#' Active value model of a kind, as a model record
#' @param con A DBI connection.
#' @param kind Model kind (see [value_model_kinds]).
#' @return A model record (see [model_record()]) or `NULL`.
#' @export
db_active_value_model <- function(con, kind) {
  row <- db_query(con, "SELECT * FROM value_models WHERE kind = ? AND active = 1", list(kind))
  if (!nrow(row)) return(NULL)
  record_from_row(row[1, ])
}

# Portfolio view (aggregation pushed to the database) ---------------------------

#' Portfolio: one row per initiative with RICE, 4M totals, review and audit
#'
#' Aggregations run in the database engine (columnar in DuckDB) instead of in
#' the Shiny R process.
#' @param con A DBI connection.
#' @return Data frame.
#' @export
db_portfolio <- function(con) {
  sql <- "WITH pl AS (
       SELECT initiative_id,
         SUM(CASE WHEN metric = 'P' THEN result_value ELSE 0 END) AS plan_p,
         SUM(CASE WHEN metric = 'R' THEN result_value ELSE 0 END) AS plan_r,
         SUM(CASE WHEN metric = 'M' THEN result_value ELSE 0 END) AS plan_m,
         SUM(CASE WHEN metric = 'T' THEN result_value ELSE 0 END) AS plan_t,
         SUM(value_mm_usd) AS plan_value_mm_usd, COUNT(*) AS plan_lines
       FROM m4_lines GROUP BY initiative_id),
     rv AS (SELECT * FROM (
        SELECT r.*, ROW_NUMBER() OVER (PARTITION BY initiative_id
                                       ORDER BY reviewed_at DESC, review_id DESC) AS rn
        FROM reviews r) x WHERE rn = 1),
     au AS (SELECT * FROM (
        SELECT a.*, ROW_NUMBER() OVER (PARTITION BY initiative_id
                                       ORDER BY audited_at DESC, audit_id DESC) AS rn
        FROM audits a) y WHERE rn = 1)
     SELECT i.*, rc.users, rc.impact, rc.confidence, rc.effort, rc.reach_value,
       rc.score, rc.scored_by, rc.scored_at,
       COALESCE(pl.plan_p, 0) AS plan_p, COALESCE(pl.plan_r, 0) AS plan_r,
       COALESCE(pl.plan_m, 0) AS plan_m, COALESCE(pl.plan_t, 0) AS plan_t,
       COALESCE(pl.plan_value_mm_usd, 0) AS plan_value_mm_usd,
       COALESCE(pl.plan_lines, 0) AS plan_lines,
       rv.decision AS review_decision, rv.p_bopd AS review_p, rv.r_mmbbl AS review_r,
       rv.m_mm_usd AS review_m, rv.t_khours AS review_t,
       rv.value_mm_usd AS review_value_mm_usd, rv.cost_mm_usd AS review_cost_mm_usd,
       rv.reviewer, rv.reviewed_at,
       au.p_bopd AS actual_p, au.r_mmbbl AS actual_r, au.m_mm_usd AS actual_m,
       au.t_khours AS actual_t, au.adoption_pct,
       au.actual_value_mm_usd AS audited_value_mm_usd, au.audited_at
     FROM initiatives i
     LEFT JOIN rice rc ON rc.initiative_id = i.id
     LEFT JOIN pl ON pl.initiative_id = i.id
     LEFT JOIN rv ON rv.initiative_id = i.id
     LEFT JOIN au ON au.initiative_id = i.id
     ORDER BY i.id"
  out <- db_query(con, sql)
  # BIGINT counts may come back as integer64 depending on the driver
  out$plan_lines <- as.numeric(out$plan_lines)
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
