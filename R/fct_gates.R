# Lifecycle stages, evaluation gates and workflow ------------------------------
#
# Appraisal (pre-execution)
#   1 Registration & RICE  - everyone; recorded at once
#   2 4MC valuation          - required above the valuation gate (effort / RICE)
#   3 Expert review         - required above the review gate (value / cost)
#   4 Decision              - prioritise / reject
# Realisation (post-execution)
#   5 Execution             - start / close
#   6 Value audit           - actual 4MC + adoption

#' Initiative statuses
#'
#' Appraisal statuses are derived from the data (what is still pending);
#' decision statuses are set explicitly by a workflow action.
#' @export
status_assessment <- c("Registered", "4MC valuation", "Expert review", "Ready")

#' @rdname status_assessment
#' @export
status_decision <- c("Prioritized", "In execution", "Closed", "Audited", "Rejected")

#' @rdname status_assessment
#' @export
status_all <- c(status_assessment, status_decision)

#' Expert review decisions
#' @export
review_decisions <- c("Approve", "Rework", "Reject")

#' Is a 4MC valuation required (valuation gate)?
#'
#' Required when effort is at least `gate2_effort_min` or the RICE score is at
#' least `gate2_score_min`.
#' @param effort Effort level.
#' @param score RICE score.
#' @param cfg Configuration list.
#' @return Logical vector (`FALSE` when not scored).
#' @export
requires_valuation <- function(effort, score, cfg) {
  p <- cfg$params
  eff <- rice_ordinal(effort, "effort", cfg)
  thr <- rice_ordinal(p$gate2_effort_min, "effort", cfg)
  out <- (!is.na(eff) & eff >= thr) | (!is.na(score) & score >= p$gate2_score_min)
  out & !is.na(score)
}

#' Is an expert review required (review gate)?
#' @param value_mm_usd Monetary value (mm USD).
#' @param cost_mm_usd Implementation cost (mm USD).
#' @param cfg Configuration list.
#' @return Logical vector.
#' @export
requires_review <- function(value_mm_usd, cost_mm_usd, cfg) {
  p <- cfg$params
  (!is.na(value_mm_usd) & value_mm_usd >= p$gate3_value_min_mm_usd) |
    (!is.na(cost_mm_usd) & cost_mm_usd >= p$gate3_cost_min_mm_usd)
}

#' Human-readable reasons why a gate is triggered
#' @param row One portfolio row, see [compute_portfolio()].
#' @param cfg Configuration list.
#' @return List with `valuation` and `review` character vectors.
#' @export
gate_reasons <- function(row, cfg) {
  p <- cfg$params
  rv <- character(0); rr <- character(0)
  if (!is.na(row$score)) {
    if (rice_ordinal(row$effort, "effort", cfg) >= rice_ordinal(p$gate2_effort_min, "effort", cfg))
      rv <- c(rv, sprintf("Effort %s \u2265 %s", row$effort, p$gate2_effort_min))
    if (row$score >= p$gate2_score_min)
      rv <- c(rv, sprintf("RICE score %.2f \u2265 %s", row$score, p$gate2_score_min))
  }
  if (!is.na(row$planned_value_mm_usd) && row$planned_value_mm_usd >= p$gate3_value_min_mm_usd)
    rr <- c(rr, sprintf("Value %.2f mm USD \u2265 %s", row$planned_value_mm_usd, p$gate3_value_min_mm_usd))
  if (!is.na(row$planned_cost_mm_usd) && row$planned_cost_mm_usd >= p$gate3_cost_min_mm_usd)
    rr <- c(rr, sprintf("Cost %.2f mm USD \u2265 %s", row$planned_cost_mm_usd, p$gate3_cost_min_mm_usd))
  list(valuation = rv, review = rr)
}

#' Derive the appraisal status from the available data
#'
#' Decision statuses (Prioritized, In execution, ...) are kept; otherwise the
#' status is the first pending appraisal step, or "Ready" when complete.
#' @param current Current status.
#' @param has_rice Logical, RICE recorded.
#' @param req_val Logical, 4MC valuation required.
#' @param has_valuation Logical, at least one 4MC line.
#' @param req_rev Logical, expert review required.
#' @param review_decision Latest review decision (or `NA`).
#' @return Character vector of statuses.
#' @export
derive_status <- function(current, has_rice, req_val, has_valuation, req_rev, review_decision) {
  review_decision[is.na(review_decision)] <- ""
  out <- ifelse(!has_rice, "Registered",
         ifelse(req_val & !has_valuation, "4MC valuation",
         ifelse(req_rev & review_decision == "Reject", "Rejected",
         ifelse(req_rev & review_decision != "Approve", "Expert review", "Ready"))))
  keep <- current %in% status_decision
  out[keep] <- current[keep]
  out
}

#' Enrich the portfolio with gates, readiness and derived status
#'
#' The ex-ante value (`planned_value_mm_usd`) is the expert-review value when
#' the review approved the initiative, otherwise the 4MC estimate (P, R, M, T).
#' The ex-ante cost (`planned_cost_mm_usd`) is the approved review cost, else
#' the 4MC cost estimate (metric C), else the cost entered at registration.
#' @param portfolio Output of [db_portfolio()].
#' @param cfg Configuration list.
#' @return Data frame with extra columns `planned_value_mm_usd`,
#'   `planned_cost_mm_usd`, `has_rice`, `has_valuation`, `req_valuation`,
#'   `req_review`, `derived_status`, `rice_rank`.
#' @export
compute_portfolio <- function(portfolio, cfg) {
  df <- portfolio
  approved <- !is.na(df$review_decision) & df$review_decision == "Approve"
  df$planned_value_mm_usd <- ifelse(approved & !is.na(df$review_value_mm_usd),
                                    df$review_value_mm_usd, df$plan_value_mm_usd)
  # cost: approved review (C) > 4MC cost estimate (C lines) > cost entered at registration
  df$planned_cost_mm_usd <- ifelse(approved & !is.na(df$review_cost_mm_usd), df$review_cost_mm_usd,
                            ifelse(!is.na(df$plan_c) & df$plan_c > 0, df$plan_c, df$cost_mm_usd))
  df$has_rice <- !is.na(df$score)
  df$has_valuation <- df$plan_lines > 0
  df$req_valuation <- requires_valuation(df$effort, df$score, cfg)
  df$req_review <- df$has_rice & requires_review(df$planned_value_mm_usd, df$planned_cost_mm_usd, cfg)
  df$derived_status <- derive_status(df$status, df$has_rice, df$req_valuation,
                                     df$has_valuation, df$req_review, df$review_decision)
  df$rice_rank <- rank(-df$score, ties.method = "min", na.last = "keep")
  df
}

#' Workflow actions allowed from a status
#' @param status Current status.
#' @return Named character vector: action id -> target status.
#' @export
allowed_actions <- function(status) {
  switch(status,
    "Ready"        = c(prioritize = "Prioritized", reject = "Rejected"),
    "Prioritized"  = c(start = "In execution", deprioritize = "Ready", reject = "Rejected"),
    "In execution" = c(close = "Closed"),
    "Closed"       = character(0),
    "Audited"      = character(0),
    "Rejected"     = c(reopen = "Registered"),
    c(reject = "Rejected")
  )
}

#' Lifecycle step checklist for one initiative
#' @param r One portfolio row (see [compute_portfolio()]).
#' @return Data frame `step`, `label`, `phase`, `state` (done/current/todo/na), `note`.
#' @export
lifecycle_steps <- function(r) {
  st <- r$status
  post <- c("Prioritized", "In execution", "Closed", "Audited")
  reviewed <- !is.na(r$review_decision) && r$review_decision %in% c("Approve", "Reject")
  s <- data.frame(
    step = 1:6,
    label = c("Registration & RICE", "4MC valuation", "Expert review", "Decision",
              "Execution", "Value audit"),
    phase = c(rep("Appraisal", 4), rep("Realisation", 2)),
    state = c(
      if (r$has_rice) "done" else "current",
      if (!r$req_valuation && !r$has_valuation) "na" else if (r$has_valuation) "done" else "current",
      if (!r$req_review) "na" else if (reviewed) "done" else "current",
      if (st %in% post) "done" else if (st == "Ready") "current" else "todo",
      if (st %in% c("Closed", "Audited")) "done" else if (st == "In execution") "current" else "todo",
      if (st == "Audited") "done" else if (st == "Closed") "current" else "todo"),
    note = c(
      if (r$has_rice) sprintf("score %.2f", r$score) else "pending",
      if (r$has_valuation) sprintf("%.2f mm USD", r$plan_value_mm_usd)
      else if (r$req_valuation) "required" else "optional",
      if (!r$req_review) "not required" else if (reviewed) r$review_decision else "required",
      if (st %in% post) "prioritized" else if (st == "Rejected") "rejected" else "",
      "",
      if (st == "Audited" && !is.na(r$adoption_pct)) sprintf("adoption %.0f%%", r$adoption_pct) else ""),
    stringsAsFactors = FALSE)
  if (st == "Rejected") s$state[s$state %in% c("current", "todo")] <- "na"
  cur <- which(s$state == "current")
  if (length(cur) > 1) s$state[cur[-1]] <- "todo"
  s
}

#' Alerts: initiatives passing evaluation criteria with pending actions
#' @param pf Output of [compute_portfolio()].
#' @return Data frame `id`, `name`, `severity`, `alert`, `stage` (lifecycle
#'   view to open: `"appraisal"` or `"realisation"`).
#' @export
assessment_alerts <- function(pf) {
  empty <- data.frame(id = character(0), name = character(0), severity = character(0),
                      alert = character(0), stage = character(0))
  if (is.null(pf) || !nrow(pf)) return(empty)
  st <- pf$derived_status
  mk <- function(status, severity, alert, stage) {
    sel <- st == status
    data.frame(id = pf$id[sel], name = pf$name[sel], severity = rep(severity, sum(sel)),
               alert = rep(alert, sum(sel)), stage = rep(stage, sum(sel)),
               stringsAsFactors = FALSE)
  }
  rows <- list(
    mk("Expert review", "high", "Above review gate \u2013 expert review required", "appraisal"),
    mk("4MC valuation", "medium", "Above valuation gate \u2013 4MC valuation pending", "appraisal"),
    mk("Closed", "medium", "Closed \u2013 value audit pending", "realisation"),
    mk("Ready", "low", "Appraisal complete \u2013 ready for decision", "appraisal")
  )
  out <- do.call(rbind, rows)
  if (is.null(out) || !nrow(out)) return(empty)
  out[order(match(out$severity, c("high", "medium", "low")), out$id), , drop = FALSE]
}

#' Synchronise stored statuses with the derived appraisal status
#'
#' Called after any appraisal record is saved (registration, 4MC, review) so
#' that the stored status always reflects what is pending.
#' @param con A DBI connection.
#' @param cfg Configuration list.
#' @param user User triggering the change.
#' @return Ids whose status changed.
#' @export
sync_status <- function(con, cfg, user = "system") {
  pf <- compute_portfolio(db_portfolio(con), cfg)
  chg <- pf[pf$derived_status != pf$status, , drop = FALSE]
  for (i in seq_len(nrow(chg))) {
    db_set_status(con, chg$id[i], chg$derived_status[i], user, "Automatic (appraisal gates)")
  }
  chg$id
}

#' Evaluation coverage per initiative: done, missing or not applicable
#'
#' An evaluation is *missing* when the gates require it and it has not been
#' recorded: a 4MC valuation above the valuation gate, an expert review above
#' the review gate (a "Rework" decision is still pending), a value audit after
#' closure. Rejected initiatives have nothing missing.
#' @param pf Output of [compute_portfolio()].
#' @return Data frame `id`, `name`, `valuation`, `review`, `audit` with values
#'   `"done"`, `"missing"` or `"n/a"`, and `n_missing`.
#' @export
evaluation_gaps <- function(pf) {
  live <- pf$status != "Rejected"
  reviewed <- !is.na(pf$review_decision) & pf$review_decision %in% c("Approve", "Reject")
  state <- function(done, required) ifelse(done, "done", ifelse(required & live, "missing", "n/a"))
  out <- data.frame(
    id = pf$id, name = pf$name,
    valuation = state(pf$has_valuation, pf$req_valuation),
    review = state(reviewed, pf$req_review),
    audit = state(pf$status == "Audited", pf$status == "Closed"),
    stringsAsFactors = FALSE)
  out$n_missing <- rowSums(out[, c("valuation", "review", "audit")] == "missing")
  out
}
