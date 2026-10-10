#' Realisation percentage (actual / planned)
#' @param actual,planned Values.
#' @return Percentage, `NA` when planned is zero or missing.
#' @export
realization_pct <- function(actual, planned) {
  ifelse(is.na(planned) | planned == 0, NA_real_, 100 * actual / planned)
}

safe_ratio <- function(num, den) if (is.na(den) || den == 0) NA_real_ else num / den

#' Decision lead time per initiative (days)
#'
#' Days from registration to the first prioritisation or rejection decision.
#' @param history Status history (see [db_get_status_history()]).
#' @return Data frame `initiative_id`, `lead_time_days`.
#' @export
decision_lead_times <- function(history) {
  if (is.null(history) || !nrow(history)) {
    return(data.frame(initiative_id = character(0), lead_time_days = numeric(0)))
  }
  ts <- as.POSIXct(history$changed_at, tz = "UTC")
  ids <- unique(history$initiative_id)
  lt <- vapply(ids, function(i) {
    sel <- history$initiative_id == i
    start <- min(ts[sel])
    dec <- ts[sel & history$to_status %in% c("Prioritized", "Rejected")]
    if (!length(dec)) NA_real_ else as.numeric(difftime(min(dec), start, units = "days"))
  }, numeric(1))
  out <- data.frame(initiative_id = ids, lead_time_days = unname(lt), stringsAsFactors = FALSE)
  out[!is.na(out$lead_time_days), , drop = FALSE]
}

#' Portfolio KPIs
#'
#' Besides portfolio size and value, the KPIs measure the value added by the
#' assessment process itself: gate compliance (are the required evaluations
#' done?), decision lead time (how fast does the process decide?) and
#' realisation rate (how accurate were the valuations?).
#'
#' @param pf Output of [compute_portfolio()].
#' @param history Status history.
#' @return Named list of KPIs.
#' @export
portfolio_kpis <- function(pf, history = NULL) {
  st <- pf$status
  committed <- st %in% c("Prioritized", "In execution")
  audited <- st == "Audited"
  lt <- decision_lead_times(history)
  req2 <- pf$req_valuation
  req3 <- pf$req_review
  reviewed <- !is.na(pf$review_decision) & pf$review_decision %in% c("Approve", "Reject")
  list(
    n_total = nrow(pf),
    n_assessment = sum(st %in% status_assessment),
    n_prioritized = sum(st == "Prioritized"),
    n_execution = sum(st == "In execution"),
    n_closed = sum(st %in% c("Closed", "Audited")),
    n_rejected = sum(st == "Rejected"),
    pipeline_value_mm_usd = sum(pf$planned_value_mm_usd[st %in% status_assessment], na.rm = TRUE),
    committed_value_mm_usd = sum(pf$planned_value_mm_usd[committed], na.rm = TRUE),
    committed_cost_mm_usd = sum(pf$planned_cost_mm_usd[committed], na.rm = TRUE),
    value_to_cost = safe_ratio(sum(pf$planned_value_mm_usd[committed], na.rm = TRUE),
                               sum(pf$planned_cost_mm_usd[committed], na.rm = TRUE)),
    planned_audited_mm_usd = sum(pf$planned_value_mm_usd[audited], na.rm = TRUE),
    realized_value_mm_usd = sum(pf$audited_value_mm_usd[audited], na.rm = TRUE),
    realization_rate_pct = 100 * safe_ratio(sum(pf$audited_value_mm_usd[audited], na.rm = TRUE),
                                            sum(pf$planned_value_mm_usd[audited], na.rm = TRUE)),
    gate2_compliance_pct = 100 * safe_ratio(sum(req2 & pf$has_valuation), sum(req2)),
    gate3_compliance_pct = 100 * safe_ratio(sum(req3 & reviewed), sum(req3)),
    median_lead_time_days = if (nrow(lt)) stats::median(lt$lead_time_days) else NA_real_,
    mean_adoption_pct = if (any(audited & !is.na(pf$adoption_pct)))
      mean(pf$adoption_pct[audited], na.rm = TRUE) else NA_real_,
    n_alerts = nrow(assessment_alerts(pf))
  )
}

#' Number of initiatives per status (all statuses, in process order)
#' @param pf Portfolio data frame.
#' @return Data frame `status`, `n`, `value_mm_usd`.
#' @export
status_summary <- function(pf) {
  data.frame(
    status = status_all,
    n = vapply(status_all, function(s) sum(pf$status == s), integer(1)),
    value_mm_usd = vapply(status_all, function(s)
      sum(pf$planned_value_mm_usd[pf$status == s], na.rm = TRUE), numeric(1)),
    stringsAsFactors = FALSE, row.names = NULL)
}

#' 4M figures of one initiative across the lifecycle
#'
#' Compares, per metric, the 4M estimate (calculation lines), the expert
#' review and the post-execution audit, in native units and mm USD.
#' @param lines 4M calculation lines of the initiative.
#' @param review Latest review row (or `NULL`).
#' @param audit Latest audit row (or `NULL`).
#' @param cfg Configuration list.
#' @return Data frame per metric: `metric`, `label`, `unit`, `estimate`,
#'   `review`, `actual`, `estimate_mm_usd`, `review_mm_usd`, `actual_mm_usd`,
#'   `realization_pct` (actual vs review if any, else vs estimate).
#' @export
m4_lifecycle <- function(lines, review = NULL, audit = NULL, cfg) {
  e <- m4_summary(lines)
  vals <- function(row) {
    if (is.null(row) || !nrow(row)) return(NULL)
    list(P = row$p_bopd, R = row$r_mmbbl, category = row$r_category, M = row$m_mm_usd,
         T = row$t_khours)
  }
  stage <- function(row) {
    v <- vals(row)
    if (is.null(v)) return(list(native = rep(NA_real_, 4), mm = rep(NA_real_, 4)))
    native <- vapply(c("P", "R", "M", "T"), function(k) as.numeric(v[[k]] %||% NA), numeric(1))
    list(native = unname(native), mm = unname(m4_value(v, cfg)$by_metric))
  }
  r <- stage(review); a <- stage(audit)
  ref <- if (all(is.na(r$mm))) e$value_mm_usd else r$mm
  data.frame(metric = e$metric, label = e$label, unit = e$unit,
             estimate = e$value, review = r$native, actual = a$native,
             estimate_mm_usd = e$value_mm_usd, review_mm_usd = r$mm, actual_mm_usd = a$mm,
             realization_pct = realization_pct(a$mm, ref), stringsAsFactors = FALSE)
}
