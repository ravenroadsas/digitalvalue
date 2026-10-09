# Process stages, evaluation gates and workflow -------------------------------

#' Initiative statuses
#'
#' Assessment statuses are derived from the data (what is still pending);
#' decision statuses are set explicitly by a workflow action.
#' @export
status_assessment <- c("Phase I", "Phase II", "Phase III", "Ready")

#' @rdname status_assessment
#' @export
status_decision <- c("Prioritized", "In execution", "Closed", "Audited", "Rejected")

#' @rdname status_assessment
#' @export
status_all <- c(status_assessment, status_decision)

#' Planning review decisions
#' @export
review_decisions <- c("Approve", "Rework", "Reject")

#' Does an initiative require Phase II (monetary valuation)?
#'
#' Required when effort is at least `gate2_effort_min` or the RICE score is at
#' least `gate2_score_min`.
#' @param effort Effort level.
#' @param score RICE score.
#' @param cfg Configuration list.
#' @return Logical vector (`FALSE` when not yet scored).
#' @export
requires_phase2 <- function(effort, score, cfg) {
  p <- cfg$params
  eff <- rice_ordinal(effort, "effort", cfg)
  thr <- rice_ordinal(p$gate2_effort_min, "effort", cfg)
  out <- (!is.na(eff) & eff >= thr) | (!is.na(score) & score >= p$gate2_score_min)
  out & !is.na(score)
}

#' Does an initiative require Phase III (planning review)?
#' @param value_mm_usd Monetary value (mm USD).
#' @param cost_mm_usd Implementation cost (mm USD).
#' @param cfg Configuration list.
#' @return Logical vector.
#' @export
requires_phase3 <- function(value_mm_usd, cost_mm_usd, cfg) {
  p <- cfg$params
  (!is.na(value_mm_usd) & value_mm_usd >= p$gate3_value_min_mm_usd) |
    (!is.na(cost_mm_usd) & cost_mm_usd >= p$gate3_cost_min_mm_usd)
}

#' Human-readable reasons why a gate is triggered
#' @param row One portfolio row (list or 1-row data frame), see [compute_portfolio()].
#' @param cfg Configuration list.
#' @return List with `phase2` and `phase3` character vectors.
#' @export
gate_reasons <- function(row, cfg) {
  p <- cfg$params
  r2 <- character(0); r3 <- character(0)
  if (!is.na(row$score)) {
    if (rice_ordinal(row$effort, "effort", cfg) >= rice_ordinal(p$gate2_effort_min, "effort", cfg))
      r2 <- c(r2, sprintf("Effort %s \u2265 %s", row$effort, p$gate2_effort_min))
    if (row$score >= p$gate2_score_min)
      r2 <- c(r2, sprintf("RICE score %.2f \u2265 %s", row$score, p$gate2_score_min))
  }
  if (!is.na(row$planned_value_mm_usd) && row$planned_value_mm_usd >= p$gate3_value_min_mm_usd)
    r3 <- c(r3, sprintf("Value %.2f mm USD \u2265 %s", row$planned_value_mm_usd, p$gate3_value_min_mm_usd))
  if (!is.na(row$planned_cost_mm_usd) && row$planned_cost_mm_usd >= p$gate3_cost_min_mm_usd)
    r3 <- c(r3, sprintf("Cost %.2f mm USD \u2265 %s", row$planned_cost_mm_usd, p$gate3_cost_min_mm_usd))
  list(phase2 = r2, phase3 = r3)
}

#' Derive the assessment status from the available data
#'
#' Decision statuses (Prioritized, In execution, ...) are kept; otherwise the
#' status is the first pending assessment phase, or "Ready" when complete.
#' @param current Current status.
#' @param has_rice Logical, RICE scored.
#' @param req2 Logical, Phase II required.
#' @param has_valuation Logical, at least one plan PRMT line.
#' @param req3 Logical, Phase III required.
#' @param review_decision Latest review decision (or `NA`).
#' @return Character vector of statuses.
#' @export
derive_status <- function(current, has_rice, req2, has_valuation, req3, review_decision) {
  review_decision[is.na(review_decision)] <- ""
  out <- ifelse(!has_rice, "Phase I",
         ifelse(req2 & !has_valuation, "Phase II",
         ifelse(req3 & review_decision == "Reject", "Rejected",
         ifelse(req3 & review_decision != "Approve", "Phase III", "Ready"))))
  keep <- current %in% status_decision
  out[keep] <- current[keep]
  out
}

#' Enrich the portfolio with gates, readiness and derived status
#' @param portfolio Output of [db_portfolio()].
#' @param cfg Configuration list.
#' @return Data frame with extra columns `planned_value_mm_usd`,
#'   `planned_cost_mm_usd`, `req_phase2`, `req_phase3`, `has_rice`,
#'   `has_valuation`, `derived_status`.
#' @export
compute_portfolio <- function(portfolio, cfg) {
  df <- portfolio
  approved <- !is.na(df$review_decision) & df$review_decision == "Approve"
  df$planned_value_mm_usd <- ifelse(approved & !is.na(df$validated_value_mm_usd),
                                    df$validated_value_mm_usd, df$plan_value_mm_usd)
  df$planned_cost_mm_usd <- ifelse(approved & !is.na(df$validated_cost_mm_usd),
                                   df$validated_cost_mm_usd, df$cost_mm_usd)
  df$has_rice <- !is.na(df$score)
  df$has_valuation <- df$plan_lines > 0
  df$req_phase2 <- requires_phase2(df$effort, df$score, cfg)
  df$req_phase3 <- df$has_rice & requires_phase3(df$planned_value_mm_usd, df$planned_cost_mm_usd, cfg)
  df$derived_status <- derive_status(df$status, df$has_rice, df$req_phase2,
                                     df$has_valuation, df$req_phase3, df$review_decision)
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
    "Rejected"     = c(reopen = "Phase I"),
    c(reject = "Rejected")
  )
}

#' Alerts: initiatives passing evaluation criteria with pending actions
#' @param pf Output of [compute_portfolio()].
#' @return Data frame `id`, `name`, `severity`, `alert`.
#' @export
assessment_alerts <- function(pf) {
  empty <- data.frame(id = character(0), name = character(0),
                      severity = character(0), alert = character(0))
  if (is.null(pf) || !nrow(pf)) return(empty)
  st <- pf$derived_status
  mk <- function(status, severity, alert) {
    sel <- st == status
    data.frame(id = pf$id[sel], name = pf$name[sel], severity = rep(severity, sum(sel)),
               alert = rep(alert, sum(sel)), stringsAsFactors = FALSE)
  }
  rows <- list(
    mk("Phase II", "warning", "Passes Phase II gate \u2013 monetary (PRMT) valuation pending"),
    mk("Phase III", "danger", "Passes Phase III gate \u2013 planning review required"),
    mk("Ready", "success", "Assessment complete \u2013 ready for prioritisation"),
    mk("Closed", "info", "Closed \u2013 audit of materialised value pending")
  )
  out <- do.call(rbind, rows)
  if (is.null(out) || !nrow(out)) return(empty)
  out[order(match(out$severity, c("danger", "warning", "info", "success")), out$id), , drop = FALSE]
}

#' Synchronise stored statuses with the derived assessment status
#'
#' Called after any assessment is saved (RICE, PRMT, review) so that the
#' stored status always reflects what is pending.
#' @param con A DBI connection.
#' @param cfg Configuration list.
#' @param user User triggering the change.
#' @return Ids whose status changed.
#' @export
sync_status <- function(con, cfg, user = "system") {
  pf <- compute_portfolio(db_portfolio(con), cfg)
  chg <- pf[pf$derived_status != pf$status, , drop = FALSE]
  for (i in seq_len(nrow(chg))) {
    db_set_status(con, chg$id[i], chg$derived_status[i], user, "Automatic (assessment gates)")
  }
  chg$id
}
