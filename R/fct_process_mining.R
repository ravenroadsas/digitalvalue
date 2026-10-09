# Process mining ---------------------------------------------------------------
# Level 0  raw events      : every Shiny input change / navigation, buffered in
#                            the browser (www/process_logger.js) and written in
#                            batches to `event_log`.
# Level 1  mapped events   : raw input names are mapped to human-readable
#                            activities and process phases (event_map.csv).
# Level 2  activity log    : consecutive mapped events of the same activity,
#                            initiative and session are collapsed into one
#                            activity instance (start / complete).
# Level 3  business log    : status transitions (status_history), the
#                            authoritative case-level process.
# Levels 2 + 3 export as a standard event log (case_id, activity, timestamp,
# resource) for bupaR / pm4py / Celonis.

#' Map raw events to activities and process phases
#' @param events Raw events (see [db_get_events()]).
#' @param event_map Data frame `pattern`, `activity`, `process_phase`; the
#'   first matching regex wins. Navigation events are matched as `nav:<tab>`.
#' @return `events` with columns `key`, `activity`, `process_phase` (NA if unmapped).
#' @export
map_raw_events <- function(events, event_map) {
  key <- ifelse(events$raw_name == "nav", paste0("nav:", events$raw_value), events$raw_name)
  activity <- rep(NA_character_, length(key))
  phase <- rep(NA_character_, length(key))
  for (i in seq_len(nrow(event_map))) {
    hit <- is.na(activity) & grepl(event_map$pattern[i], key, perl = TRUE)
    activity[hit] <- event_map$activity[i]
    phase[hit] <- event_map$process_phase[i]
  }
  events$key <- key
  events$activity <- activity
  events$process_phase <- phase
  events
}

#' Collapse mapped events into activity instances
#'
#' @param events Raw events.
#' @param event_map Event map.
#' @param gap_minutes A new activity instance starts after this idle gap.
#' @return Data frame `case_id`, `activity`, `process_phase`, `resource`,
#'   `session_id`, `start`, `complete`, `n_events`.
#' @export
build_activity_log <- function(events, event_map, gap_minutes = 10) {
  empty <- data.frame(case_id = character(0), activity = character(0),
                      process_phase = character(0), resource = character(0),
                      session_id = character(0), start = character(0),
                      complete = character(0), n_events = integer(0))
  if (is.null(events) || !nrow(events)) return(empty)
  ev <- map_raw_events(events, event_map)
  ev <- ev[!is.na(ev$activity), , drop = FALSE]
  if (!nrow(ev)) return(empty)
  ev$initiative_id[is.na(ev$initiative_id) | ev$initiative_id == ""] <- "(none)"
  ev <- ev[order(ev$session_id, ev$ts), , drop = FALSE]
  t <- as.POSIXct(ev$ts, tz = "UTC")
  n <- nrow(ev)
  prev_same <- c(FALSE,
    ev$session_id[-1] == ev$session_id[-n] &
    ev$activity[-1] == ev$activity[-n] &
    ev$initiative_id[-1] == ev$initiative_id[-n] &
    as.numeric(difftime(t[-1], t[-n], units = "mins")) <= gap_minutes)
  grp <- cumsum(!prev_same)
  idx <- split(seq_len(n), grp)
  data.frame(
    case_id = vapply(idx, function(i) ev$initiative_id[i[1]], ""),
    activity = vapply(idx, function(i) ev$activity[i[1]], ""),
    process_phase = vapply(idx, function(i) ev$process_phase[i[1]], ""),
    resource = vapply(idx, function(i) ev$user_name[i[1]], ""),
    session_id = vapply(idx, function(i) ev$session_id[i[1]], ""),
    start = vapply(idx, function(i) ev$ts[i[1]], ""),
    complete = vapply(idx, function(i) ev$ts[i[length(i)]], ""),
    n_events = vapply(idx, length, integer(1)),
    stringsAsFactors = FALSE, row.names = NULL)
}

#' Business (case-level) event log from status transitions
#' @param history Status history.
#' @return Data frame `case_id`, `activity`, `process_phase`, `resource`,
#'   `timestamp`, `comment`.
#' @export
build_business_log <- function(history) {
  if (is.null(history) || !nrow(history)) {
    return(data.frame(case_id = character(0), activity = character(0),
                      process_phase = character(0), resource = character(0),
                      timestamp = character(0), comment = character(0)))
  }
  data.frame(case_id = history$initiative_id,
             activity = ifelse(is.na(history$from_status), "Register initiative",
                               paste("Enter", history$to_status)),
             process_phase = "Lifecycle",
             resource = history$changed_by,
             timestamp = history$changed_at,
             comment = history$comment,
             stringsAsFactors = FALSE)
}

#' Standard event log for process-mining tools
#'
#' Combines the activity log (complete timestamps) and the business log.
#' @param activity_log Output of [build_activity_log()].
#' @param business_log Output of [build_business_log()].
#' @return Data frame `case_id`, `activity`, `process_phase`, `resource`,
#'   `timestamp`, `lifecycle`, `level`.
#' @export
export_event_log <- function(activity_log, business_log) {
  a <- if (nrow(activity_log)) rbind(
    data.frame(case_id = activity_log$case_id, activity = activity_log$activity,
               process_phase = activity_log$process_phase, resource = activity_log$resource,
               timestamp = activity_log$start, lifecycle = "start", level = "user activity"),
    data.frame(case_id = activity_log$case_id, activity = activity_log$activity,
               process_phase = activity_log$process_phase, resource = activity_log$resource,
               timestamp = activity_log$complete, lifecycle = "complete", level = "user activity"))
  b <- if (nrow(business_log)) data.frame(
    case_id = business_log$case_id, activity = business_log$activity,
    process_phase = business_log$process_phase, resource = business_log$resource,
    timestamp = business_log$timestamp, lifecycle = "complete", level = "business")
  out <- rbind(a, b)
  if (is.null(out)) {
    return(data.frame(case_id = character(0), activity = character(0),
                      process_phase = character(0), resource = character(0),
                      timestamp = character(0), lifecycle = character(0),
                      level = character(0)))
  }
  out <- out[out$case_id != "(none)", , drop = FALSE]
  out[order(out$case_id, out$timestamp), , drop = FALSE]
}

#' Time spent per process phase (from the activity log)
#' @param activity_log Output of [build_activity_log()].
#' @return Data frame `process_phase`, `instances`, `minutes`, `users`.
#' @export
phase_effort <- function(activity_log) {
  if (!nrow(activity_log)) {
    return(data.frame(process_phase = character(0), instances = integer(0),
                      minutes = numeric(0), users = integer(0)))
  }
  dur <- as.numeric(difftime(as.POSIXct(activity_log$complete, tz = "UTC"),
                             as.POSIXct(activity_log$start, tz = "UTC"), units = "mins"))
  ph <- sort(unique(activity_log$process_phase))
  data.frame(
    process_phase = ph,
    instances = vapply(ph, function(p) sum(activity_log$process_phase == p), integer(1)),
    minutes = vapply(ph, function(p) sum(dur[activity_log$process_phase == p]), numeric(1)),
    users = vapply(ph, function(p) length(unique(activity_log$resource[activity_log$process_phase == p])), integer(1)),
    stringsAsFactors = FALSE, row.names = NULL)
}

#' Normalise a batch of raw events received from the browser
#' @param batch JSON string (array of `{ts, name, value}` objects) sent by
#'   `www/process_logger.js`, or an equivalent data frame.
#' @param session_id,user_name,initiative_id Context added server-side.
#' @return Data frame ready for [db_log_events()], or `NULL` when empty.
#' @export
normalize_raw_batch <- function(batch, session_id, user_name, initiative_id = NA) {
  if (is.null(batch) || !length(batch)) return(NULL)
  if (is.character(batch)) batch <- jsonlite::fromJSON(batch, simplifyVector = TRUE)
  if (!is.data.frame(batch) || !nrow(batch)) return(NULL)
  col <- function(k) {
    if (!k %in% names(batch)) return(rep(NA_character_, nrow(batch)))
    v <- as.character(batch[[k]])
    substr(v, 1, 120)
  }
  # browser ISO timestamps -> "YYYY-mm-dd HH:MM:SS" UTC
  ts <- sub("T", " ", substr(col("ts"), 1, 19), fixed = TRUE)
  data.frame(ts = ts, session_id = session_id, user_name = user_name,
             initiative_id = if (is.null(initiative_id)) NA_character_ else initiative_id,
             source = "input", raw_name = col("name"), raw_value = col("value"),
             stringsAsFactors = FALSE, row.names = NULL)
}
