#' Realisation percentage (actual / planned)
#' @param actual,planned Values.
#' @return Percentage, `NA` when planned is zero or missing.
#' @export
realization_pct <- function(actual, planned) {
  ifelse(is.na(planned) | planned == 0, NA_real_, 100 * actual / planned)
}

safe_ratio <- function(num, den) if (is.na(den) || den == 0) NA_real_ else num / den

#' Evaluation lead time per initiative (days)
#'
#' Days from registration until the initiative first became `Evaluated`
#' (all required evaluations complete).
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
    dec <- ts[sel & history$to_status %in% "Evaluated"]
    if (!length(dec)) NA_real_ else as.numeric(difftime(min(dec), start, units = "days"))
  }, numeric(1))
  out <- data.frame(initiative_id = ids, lead_time_days = unname(lt), stringsAsFactors = FALSE)
  out[!is.na(out$lead_time_days), , drop = FALSE]
}

#' Portfolio KPIs
#'
#' Besides portfolio size and value, the KPIs measure the value added by the
#' evaluation process itself: gate compliance (are the required evaluations
#' done?), evaluation lead time (how fast are initiatives evaluated?) and
#' realisation rate (how accurate were the valuations?).
#'
#' @param pf Output of [compute_portfolio()].
#' @param history Status history.
#' @return Named list of KPIs.
#' @export
portfolio_kpis <- function(pf, history = NULL) {
  st <- pf$status
  evaluated <- st == "Evaluated"
  delivered <- st == "Delivered"
  audited <- st == "Audited"
  lt <- decision_lead_times(history)
  req2 <- pf$req_valuation
  req3 <- pf$req_review
  approved <- !is.na(pf$review_decision) & pf$review_decision == "Approve"
  ev <- evaluated | delivered
  list(
    n_total = nrow(pf),
    n_recorded = sum(st == "Recorded"),
    n_evaluated = sum(evaluated),
    n_delivered = sum(delivered),
    n_audited = sum(audited),
    pipeline_value_mm_usd = sum(pf$planned_value_mm_usd[st == "Recorded"], na.rm = TRUE),
    evaluated_value_mm_usd = sum(pf$planned_value_mm_usd[evaluated], na.rm = TRUE),
    delivered_value_mm_usd = sum(pf$planned_value_mm_usd[delivered], na.rm = TRUE),
    value_to_cost = safe_ratio(sum(pf$planned_value_mm_usd[ev], na.rm = TRUE),
                               sum(pf$planned_cost_mm_usd[ev], na.rm = TRUE)),
    evaluated_cost_mm_usd = sum(pf$planned_cost_mm_usd[ev], na.rm = TRUE),
    planned_audited_mm_usd = sum(pf$planned_value_mm_usd[audited], na.rm = TRUE),
    realized_value_mm_usd = sum(pf$audited_value_mm_usd[audited], na.rm = TRUE),
    realization_rate_pct = 100 * safe_ratio(sum(pf$audited_value_mm_usd[audited], na.rm = TRUE),
                                            sum(pf$planned_value_mm_usd[audited], na.rm = TRUE)),
    gate2_compliance_pct = 100 * safe_ratio(sum(req2 & pf$has_valuation & pf$validated), sum(req2)),
    gate3_compliance_pct = 100 * safe_ratio(sum(req3 & approved), sum(req3)),
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

#' 4MC figures of one initiative across the lifecycle
#'
#' Compares, per metric (P, R, M, T and cost C), the 4MC estimate (calculation
#' lines), the expert review and the post-execution audit, in native units and
#' mm USD. For C the realisation is actual cost / planned cost.
#' @param lines 4MC calculation lines of the initiative.
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
    # reviews store cost as cost_mm_usd, audits as c_mm_usd
    list(P = row$p_bopd, R = row$r_mmbbl, category = row$r_category, M = row$m_mm_usd,
         T = row$t_khours, C = row$cost_mm_usd %||% row$c_mm_usd)
  }
  stage <- function(row) {
    v <- vals(row)
    if (is.null(v)) return(list(native = rep(NA_real_, 5), mm = rep(NA_real_, 5)))
    native <- vapply(c("P", "R", "M", "T", "C"), function(k) as.numeric(v[[k]] %||% NA), numeric(1))
    list(native = unname(native), mm = unname(m4_value(v, cfg)$by_metric))
  }
  r <- stage(review); a <- stage(audit)
  ref <- if (all(is.na(r$mm))) e$value_mm_usd else r$mm
  data.frame(metric = e$metric, label = e$label, unit = e$unit,
             estimate = e$value, review = r$native, actual = a$native,
             estimate_mm_usd = e$value_mm_usd, review_mm_usd = r$mm, actual_mm_usd = a$mm,
             realization_pct = realization_pct(a$mm, ref), stringsAsFactors = FALSE)
}
