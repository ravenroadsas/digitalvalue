# Lifecycle, evaluation gates and workflow -------------------------------------
#
# Statuses: Recorded -> Evaluated -> Delivered -> Audited
#
# Evaluation (pre-delivery)
#   1 Registration & RICE  - everyone; recorded at once, then locked
#   2 4MC valuation        - required above the valuation gate (effort / RICE);
#                            recorded by the owner (or a superuser)
#   3 4MC validation       - the owner sends the 4MC to a validator (Connect user)
#   4 Expert review        - required above the review gate (value / cost); superuser
#   -> "Evaluated" once every required step is complete
# Realisation (post-delivery)
#   5 Delivery             - the owner or a superuser marks the initiative delivered
#   6 Value audit          - actual 4MC + adoption; superuser -> "Audited"

#' Initiative statuses
#'
#' `Recorded` and `Evaluated` are derived from the data (are the required
#' evaluations complete?); `Delivered` and `Audited` are set by an action.
#' @export
status_assessment <- c("Recorded", "Evaluated")

#' @rdname status_assessment
#' @export
status_decision <- c("Delivered", "Audited")

#' @rdname status_assessment
#' @export
status_all <- c(status_assessment, status_decision)

#' Expert review decisions
#' @export
review_decisions <- c("Approve", "Rework")

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

#' Derive the status from the available data
#'
#' `Delivered` and `Audited` are kept; otherwise the initiative is `Evaluated`
#' when every required evaluation is complete, `Recorded` otherwise.
#' @param current Current status.
#' @param has_rice Logical, RICE recorded.
#' @param req_val Logical, 4MC valuation required.
#' @param has_valuation Logical, at least one 4MC value line.
#' @param validated Logical, latest 4MC validation is "validated".
#' @param req_rev Logical, expert review required.
#' @param review_decision Latest review decision (or `NA`).
#' @return Character vector of statuses.
#' @export
derive_status <- function(current, has_rice, req_val, has_valuation, validated, req_rev,
                          review_decision) {
  review_decision[is.na(review_decision)] <- ""
  complete <- has_rice & (!req_val | (has_valuation & validated)) & (!req_rev | review_decision == "Approve")
  out <- ifelse(complete, "Evaluated", "Recorded")
  keep <- current %in% status_decision
  out[keep] <- current[keep]
  out
}

#' Enrich the portfolio with gates, pending steps and derived status
#'
#' The ex-ante value (`planned_value_mm_usd`) is the expert-review value when
#' the review approved the initiative, otherwise the 4MC estimate (P, R, M, T).
#' The ex-ante cost (`planned_cost_mm_usd`) is the approved review cost, else
#' the 4MC cost estimate (metric C), else a legacy registration cost.
#' @param portfolio Output of [db_portfolio()].
#' @param cfg Configuration list.
#' @return Data frame with extra columns `planned_value_mm_usd`,
#'   `planned_cost_mm_usd`, `has_rice`, `has_valuation`, `validated`,
#'   `req_valuation`, `req_review`, `pending_step`, `derived_status`, `rice_rank`.
#' @export
compute_portfolio <- function(portfolio, cfg) {
  df <- portfolio
  approved <- !is.na(df$review_decision) & df$review_decision == "Approve"
  df$planned_value_mm_usd <- ifelse(approved & !is.na(df$review_value_mm_usd),
                                    df$review_value_mm_usd, df$plan_value_mm_usd)
  df$planned_cost_mm_usd <- ifelse(approved & !is.na(df$review_cost_mm_usd), df$review_cost_mm_usd,
                            ifelse(!is.na(df$plan_c) & df$plan_c > 0, df$plan_c, df$cost_mm_usd))
  df$has_rice <- !is.na(df$score)
  df$has_valuation <- df$plan_lines > 0
  df$validated <- !is.na(df$validation_status) & df$validation_status == "validated"
  df$req_valuation <- requires_valuation(df$effort, df$score, cfg)
  df$req_review <- df$has_rice & requires_review(df$planned_value_mm_usd, df$planned_cost_mm_usd, cfg)
  vs <- ifelse(is.na(df$validation_status), "", df$validation_status)
  df$pending_step <- ifelse(!df$has_rice, "Registration & RICE",
                     ifelse(df$req_valuation & !df$has_valuation, "4MC valuation",
                     ifelse(df$req_valuation & !df$validated,
                            ifelse(vs == "pending", "4MC validation", "Send 4MC to validator"),
                     ifelse(df$req_review & !approved, "Expert review", NA_character_))))
  df$derived_status <- derive_status(df$status, df$has_rice, df$req_valuation, df$has_valuation,
                                     df$validated, df$req_review, df$review_decision)
  df$pending_step[df$derived_status %in% status_decision] <- NA_character_
  df$rice_rank <- rank(-df$score, ties.method = "min", na.last = "keep")
  df
}

#' Workflow actions allowed from a status
#' @param status Current status.
#' @return Named character vector: action id -> target status.
#' @export
allowed_actions <- function(status) {
  switch(status,
    "Evaluated" = c(deliver = "Delivered"),
    "Delivered" = c(undeliver = "Evaluated"),
    character(0)
  )
}

#' Lifecycle step checklist for one initiative
#' @param r One portfolio row (see [compute_portfolio()]).
#' @return Data frame `step`, `label`, `phase`, `state` (done/current/todo/na), `note`.
#' @export
lifecycle_steps <- function(r) {
  st <- r$status
  vs <- if (is.na(r$validation_status)) "" else r$validation_status
  reviewed <- !is.na(r$review_decision) && r$review_decision == "Approve"
  val_needed <- r$req_valuation || r$has_valuation
  s <- data.frame(
    step = 1:6,
    label = c("Registration & RICE", "4MC valuation", "4MC validation", "Expert review",
              "Delivery", "Value audit"),
    phase = c(rep("Evaluation", 4), rep("Realisation", 2)),
    state = c(
      if (r$has_rice) "done" else "current",
      if (!val_needed) "na" else if (r$has_valuation) "done" else "current",
      if (!val_needed) "na" else if (r$validated) "done" else if (r$has_valuation) "current" else "todo",
      if (!r$req_review) "na" else if (reviewed) "done" else "current",
      if (st %in% status_decision) "done" else if (st == "Evaluated") "current" else "todo",
      if (st == "Audited") "done" else if (st == "Delivered") "current" else "todo"),
    note = c(
      if (r$has_rice) sprintf("score %.2f", r$score) else "pending",
      if (r$has_valuation) sprintf("%.2f mm USD", r$plan_value_mm_usd)
      else if (r$req_valuation) "required" else "optional",
      switch(vs, validated = paste("by", r$validator), pending = paste("with", r$validator),
             changes_requested = "changes requested", if (val_needed) "not sent" else ""),
      if (!r$req_review) "not required" else if (reviewed) "approved"
      else if (!is.na(r$review_decision)) "rework" else "required",
      if (st %in% status_decision) "delivered" else "",
      if (st == "Audited" && !is.na(r$adoption_pct)) sprintf("adoption %.0f%%", r$adoption_pct) else ""),
    stringsAsFactors = FALSE)
  cur <- which(s$state == "current")
  if (length(cur) > 1) s$state[cur[-1]] <- "todo"
  s
}

#' Alerts: pending actions on the initiatives
#' @param pf Output of [compute_portfolio()].
#' @return Data frame `id`, `name`, `severity`, `alert`, `stage` (lifecycle
#'   view to open: `"evaluation"` or `"realisation"`).
#' @export
assessment_alerts <- function(pf) {
  empty <- data.frame(id = character(0), name = character(0), severity = character(0),
                      alert = character(0), stage = character(0))
  if (is.null(pf) || !nrow(pf)) return(empty)
  ps <- ifelse(is.na(pf$pending_step), "", pf$pending_step)
  st <- pf$derived_status
  who <- ifelse(is.na(pf$validator), "", pf$validator)
  mk <- function(sel, severity, alert, stage) {
    data.frame(id = pf$id[sel], name = pf$name[sel], severity = rep(severity, sum(sel)),
               alert = if (length(alert) == 1) rep(alert, sum(sel)) else alert[sel],
               stage = rep(stage, sum(sel)), stringsAsFactors = FALSE)
  }
  rows <- list(
    mk(ps == "Expert review", "high", "Above review gate \u2013 expert review required", "evaluation"),
    mk(ps == "4MC valuation", "medium", "Above valuation gate \u2013 4MC valuation pending", "evaluation"),
    mk(ps == "Send 4MC to validator", "medium", "4MC recorded \u2013 send it to a validator", "evaluation"),
    mk(ps == "4MC validation", "medium", paste("4MC validation pending with", who), "evaluation"),
    mk(st == "Delivered", "medium", "Delivered \u2013 value audit pending", "realisation"),
    mk(st == "Evaluated", "low", "Evaluated \u2013 ready to deliver", "realisation")
  )
  out <- do.call(rbind, rows)
  if (is.null(out) || !nrow(out)) return(empty)
  out[order(match(out$severity, c("high", "medium", "low")), out$id), , drop = FALSE]
}

#' Synchronise stored statuses with the derived status
#'
#' Called after any evaluation is saved so that the stored status always
#' reflects whether the required evaluations are complete.
#' @param con A DBI connection.
#' @param cfg Configuration list.
#' @param user User triggering the change.
#' @return Ids whose status changed.
#' @export
sync_status <- function(con, cfg, user = "system") {
  pf <- compute_portfolio(db_portfolio(con), cfg)
  chg <- pf[pf$derived_status != pf$status, , drop = FALSE]
  for (i in seq_len(nrow(chg))) {
    db_set_status(con, chg$id[i], chg$derived_status[i], user, "Automatic (evaluation gates)")
  }
  chg$id
}

#' Evaluation coverage per initiative: done, missing or not applicable
#'
#' An evaluation is *missing* when the gates require it and it has not been
#' completed: a 4MC valuation or its validation above the valuation gate, an
#' approved expert review above the review gate, a value audit after delivery.
#' @param pf Output of [compute_portfolio()].
#' @return Data frame `id`, `name`, `valuation`, `validation`, `review`,
#'   `audit` with values `"done"`, `"missing"`, `"pending"` (validation sent,
#'   awaiting the validator) or `"n/a"`, and `n_missing`.
#' @export
evaluation_gaps <- function(pf) {
  approved <- !is.na(pf$review_decision) & pf$review_decision == "Approve"
  state <- function(done, required) ifelse(done, "done", ifelse(required, "missing", "n/a"))
  pend <- !is.na(pf$validation_status) & pf$validation_status == "pending"
  out <- data.frame(
    id = pf$id, name = pf$name,
    valuation = state(pf$has_valuation, pf$req_valuation),
    validation = ifelse(pf$validated, "done",
                 ifelse(pend, "pending", ifelse(pf$req_valuation, "missing", "n/a"))),
    review = state(approved, pf$req_review),
    audit = state(pf$status == "Audited", pf$status == "Delivered"),
    stringsAsFactors = FALSE)
  out$n_missing <- rowSums(as.matrix(out[, c("valuation", "validation", "review", "audit")]) == "missing")
  out
}
