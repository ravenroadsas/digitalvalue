# 4MC valuation: four value metrics used across the lifecycle (4MC estimate,
# expert review and post-execution value audit)
#   P - Production : average yearly incremental BOPD
#   R - Reserves   : MMbbl, by category (1P, 2P, 3P, contingent)
#   M - Monetary   : mm USD per year
#   T - Time saved : khours per year
# Each metric can be obtained with different calculation methods. A method has
# a parameter specification (drives the UI), a formula (returns the metric and
# a human-readable trace) and is monetised with the configuration parameters.

#' 4MC metric definitions
#'
#' Four value metrics (P, R, M, T) plus the cost metric C. Cost is tracked
#' alongside value but never added to it: value totals use P, R, M, T only.
#' @return Data frame with `metric`, `label`, `unit`, `kind` (`"value"`/`"cost"`).
#' @export
m4_metrics <- function() {
  data.frame(metric = c("P", "R", "M", "T", "C"),
             label = c("Production", "Reserves", "Monetary", "Time saved", "Cost"),
             unit = c("BOPD", "MMbbl", "mm USD", "khours", "mm USD"),
             kind = c(rep("value", 4), "cost"),
             stringsAsFactors = FALSE)
}

#' Codes of the value metrics (excluding cost)
#' @export
m4_value_metrics <- c("P", "R", "M", "T")

num_param <- function(name, label, default, unit = "") {
  list(name = name, label = label, default = default, unit = unit, type = "numeric")
}

fmt <- function(x, digits = 2) formatC(x, format = "f", digits = digits, big.mark = ",",
                                       drop0trailing = TRUE)
x_ <- " \u00d7 "
minus_ <- " \u2212 "

#' Registry of 4MC calculation methods
#'
#' @param cfg Configuration list (used for the reserve categories).
#' @return Named list of methods. Each method has `id`, `metric`, `label`,
#'   `params` (list of parameter specs) and `fun(p)` returning
#'   `list(value, formula_text)`.
#' @export
m4_methods <- function(cfg) {
  cats <- names(reserve_values(cfg))
  cat_param <- list(name = "category", label = "Reserve category", default = cats[1],
                    unit = "", type = "select", choices = cats)
  m <- list(
    p_direct = list(
      metric = "P", label = "Direct estimate of incremental BOPD",
      params = list(num_param("bopd", "Incremental production", 100, "BOPD")),
      fun = function(p) list(value = p$bopd,
                             formula_text = paste0(fmt(p$bopd), " BOPD (direct estimate)"))),
    p_jobs_success = list(
      metric = "P", label = "Jobs \u00d7 rate \u00d7 success-rate uplift",
      params = list(num_param("n_jobs", "Number of jobs (e.g. workovers)", 10, "jobs/yr"),
                    num_param("bopd_per_job", "Production per successful job", 30, "BOPD"),
                    num_param("success_before", "Success rate before", 60, "%"),
                    num_param("success_after", "Success rate after", 70, "%")),
      fun = function(p) {
        v <- p$n_jobs * p$bopd_per_job * (p$success_after - p$success_before) / 100
        list(value = v, formula_text = paste0(
          fmt(p$n_jobs), " jobs", x_, fmt(p$bopd_per_job), " BOPD", x_, "(",
          fmt(p$success_after), "%", minus_, fmt(p$success_before), "%) = ",
          fmt(v), " BOPD"))
      }),
    p_uptime = list(
      metric = "P", label = "Uptime / efficiency gain on base production",
      params = list(num_param("base_bopd", "Base production", 5000, "BOPD"),
                    num_param("uptime_before", "Uptime before", 92, "%"),
                    num_param("uptime_after", "Uptime after", 94, "%")),
      fun = function(p) {
        v <- p$base_bopd * (p$uptime_after - p$uptime_before) / 100
        list(value = v, formula_text = paste0(
          fmt(p$base_bopd), " BOPD", x_, "(", fmt(p$uptime_after), "%", minus_,
          fmt(p$uptime_before), "%) = ", fmt(v), " BOPD"))
      }),
    p_decline = list(
      metric = "P", label = "Decline mitigation (yearly average)",
      params = list(num_param("base_bopd", "Base production", 5000, "BOPD"),
                    num_param("decline_before", "Annual decline before", 15, "%"),
                    num_param("decline_after", "Annual decline after", 12, "%")),
      fun = function(p) {
        v <- p$base_bopd * (p$decline_before - p$decline_after) / 100 / 2
        list(value = v, formula_text = paste0(
          fmt(p$base_bopd), " BOPD", x_, "(", fmt(p$decline_before), "%", minus_,
          fmt(p$decline_after), "%) / 2 (yearly average) = ", fmt(v), " BOPD"))
      }),
    r_direct = list(
      metric = "R", label = "Direct estimate of reserves added",
      params = list(num_param("mmbbl", "Reserves added", 1, "MMbbl"), cat_param),
      fun = function(p) list(value = p$mmbbl, formula_text = paste0(
        fmt(p$mmbbl, 3), " MMbbl ", p$category, " (direct estimate)"))),
    r_recovery = list(
      metric = "R", label = "Recovery factor uplift on OOIP",
      params = list(num_param("ooip_mmbbl", "Original oil in place", 200, "MMbbl"),
                    num_param("rf_before", "Recovery factor before", 25, "%"),
                    num_param("rf_after", "Recovery factor after", 25.5, "%"),
                    cat_param),
      fun = function(p) {
        v <- p$ooip_mmbbl * (p$rf_after - p$rf_before) / 100
        list(value = v, formula_text = paste0(
          fmt(p$ooip_mmbbl), " MMbbl OOIP", x_, "(", fmt(p$rf_after), "%", minus_,
          fmt(p$rf_before), "%) = ", fmt(v, 3), " MMbbl ", p$category))
      }),
    m_direct = list(
      metric = "M", label = "Direct estimate (mm USD / yr)",
      params = list(num_param("mm_usd", "Annual monetary impact", 1, "mm USD")),
      fun = function(p) list(value = p$mm_usd, formula_text = paste0(
        fmt(p$mm_usd), " mm USD/yr (direct estimate)"))),
    m_cost_saving = list(
      metric = "M", label = "Events \u00d7 saving per event",
      params = list(num_param("events_per_year", "Events per year", 50, "#/yr"),
                    num_param("saving_kusd", "Saving per event", 20, "k USD")),
      fun = function(p) {
        v <- p$events_per_year * p$saving_kusd / 1000
        list(value = v, formula_text = paste0(
          fmt(p$events_per_year), " events", x_, fmt(p$saving_kusd), " kUSD / 1000 = ",
          fmt(v, 3), " mm USD/yr"))
      }),
    m_risk_avoided = list(
      metric = "M", label = "Avoided cost \u00d7 probability",
      params = list(num_param("cost_mm_usd", "Cost of the event avoided", 5, "mm USD"),
                    num_param("prob_before", "Annual probability before", 10, "%"),
                    num_param("prob_after", "Annual probability after", 4, "%")),
      fun = function(p) {
        v <- p$cost_mm_usd * (p$prob_before - p$prob_after) / 100
        list(value = v, formula_text = paste0(
          fmt(p$cost_mm_usd), " mm USD", x_, "(", fmt(p$prob_before), "%", minus_,
          fmt(p$prob_after), "%) = ", fmt(v, 3), " mm USD/yr"))
      }),
    t_direct = list(
      metric = "T", label = "Direct estimate (khours / yr)",
      params = list(num_param("khours", "Hours saved per year", 5, "khours")),
      fun = function(p) list(value = p$khours, formula_text = paste0(
        fmt(p$khours), " khours/yr (direct estimate)"))),
    t_users = list(
      metric = "T", label = "Users \u00d7 hours per week \u00d7 weeks",
      params = list(num_param("n_users", "Users", 100, "users"),
                    num_param("hours_week", "Hours saved per user per week", 2, "h/wk"),
                    num_param("weeks_year", "Working weeks per year", 46, "wk")),
      fun = function(p) {
        v <- p$n_users * p$hours_week * p$weeks_year / 1000
        list(value = v, formula_text = paste0(
          fmt(p$n_users), " users", x_, fmt(p$hours_week), " h/wk", x_,
          fmt(p$weeks_year), " wk / 1000 = ", fmt(v), " khours/yr"))
      }),
    t_tasks = list(
      metric = "T", label = "Tasks \u00d7 minutes saved per task",
      params = list(num_param("tasks_year", "Tasks per year", 20000, "#/yr"),
                    num_param("minutes_saved", "Minutes saved per task", 15, "min")),
      fun = function(p) {
        v <- p$tasks_year * p$minutes_saved / 60 / 1000
        list(value = v, formula_text = paste0(
          fmt(p$tasks_year), " tasks", x_, fmt(p$minutes_saved), " min / 60 / 1000 = ",
          fmt(v), " khours/yr"))
      })
,
    c_direct = list(
      metric = "C", label = "Capex + opex over the evaluation period",
      params = list(num_param("capex_mm_usd", "Capex (one-off)", 0.5, "mm USD"),
                    num_param("opex_mm_usd_yr", "Opex (run cost)", 0.1, "mm USD/yr"),
                    num_param("years", "Years of opex counted", 1, "yr")),
      fun = function(p) {
        v <- p$capex_mm_usd + p$opex_mm_usd_yr * p$years
        list(value = v, formula_text = paste0(
          fmt(p$capex_mm_usd, 3), " capex + ", fmt(p$opex_mm_usd_yr, 3), " opex", x_,
          fmt(p$years), " yr = ", fmt(v, 3), " mm USD"))
      }),
    c_effort = list(
      metric = "C", label = "Effort (person-months) \u00d7 rate + licences",
      params = list(num_param("person_months", "Effort", 12, "person-months"),
                    num_param("rate_kusd_pm", "Cost per person-month", 15, "k USD"),
                    num_param("licences_kusd", "Licences / services", 50, "k USD")),
      fun = function(p) {
        v <- (p$person_months * p$rate_kusd_pm + p$licences_kusd) / 1000
        list(value = v, formula_text = paste0(
          "(", fmt(p$person_months), " pm", x_, fmt(p$rate_kusd_pm), " kUSD + ",
          fmt(p$licences_kusd), " kUSD) / 1000 = ", fmt(v, 3), " mm USD"))
      })
  )
  for (k in names(m)) m[[k]]$id <- k
  m
}

#' Methods available for a metric
#' @param metric One of `"P"`, `"R"`, `"M"`, `"T"`.
#' @param cfg Configuration list.
#' @return Named character vector (label -> id) for select inputs.
#' @export
m4_method_choices <- function(metric, cfg) {
  m <- Filter(function(x) x$metric == metric, m4_methods(cfg))
  stats::setNames(names(m), vapply(m, `[[`, "", "label"))
}

#' Hourly value of saved time
#'
#' Average salary per hour x productivity coefficient (value generated when
#' the saved time is redeployed to productive tasks).
#' @param cfg Configuration list.
#' @return USD per hour.
#' @export
time_value_usd_hour <- function(cfg) {
  p <- cfg$params
  p$avg_salary_usd_year / p$work_hours_year * p$time_productivity_coef
}

#' Monetise a 4MC metric (mm USD)
#'
#' * P: BOPD x days/yr x netback (annual)
#' * R: MMbbl x value per barrel of the category (one-off)
#' * M: as is (annual)
#' * T: khours x 1000 x salary/h x productivity coefficient (annual)
#' * C: cost, as is (mm USD; never added to the value total)
#'
#' @param metric Metric code.
#' @param value Metric value in its native unit.
#' @param cfg Configuration list.
#' @param category Reserve category (metric R only).
#' @return mm USD.
#' @export
m4_monetize <- function(metric, value, cfg, category = NULL) {
  p <- cfg$params
  switch(metric,
    P = value * p$days_per_year * p$netback_usd_bbl / 1e6,
    R = {
      rv <- reserve_values(cfg)
      if (is.null(category) || !category %in% names(rv)) stop("Unknown reserve category: ", category)
      value * rv[[category]]
    },
    M = value,
    T = value * 1000 * time_value_usd_hour(cfg) / 1e6,
    C = value,
    stop("Unknown 4MC metric: ", metric)
  )
}

#' Run a 4MC calculation
#'
#' @param method Method id (see [m4_methods()]).
#' @param params Named list of parameters; missing ones take the defaults.
#' @param cfg Configuration list.
#' @return List with `method`, `metric`, `params`, `value`, `unit`,
#'   `value_mm_usd`, `formula_text`.
#' @export
m4_calculate <- function(method, params, cfg) {
  methods <- m4_methods(cfg)
  if (!method %in% names(methods)) stop("Unknown 4MC method: ", method)
  m <- methods[[method]]
  p <- list()
  for (spec in m$params) {
    v <- params[[spec$name]]
    if (is.null(v) || length(v) == 0 || (length(v) == 1 && is.na(v))) v <- spec$default
    if (spec$type == "numeric") {
      v <- suppressWarnings(as.numeric(v))
      if (is.na(v)) stop("Parameter '", spec$label, "' must be numeric")
    }
    p[[spec$name]] <- v
  }
  res <- m$fun(p)
  unit <- m4_metrics()$unit[m4_metrics()$metric == m$metric]
  mm <- m4_monetize(m$metric, res$value, cfg, p$category)
  list(method = method, metric = m$metric, params = p, value = res$value,
       unit = unit, value_mm_usd = mm,
       formula_text = if (m$metric == "C") res$formula_text   # already in mm USD
                      else paste0(res$formula_text, " \u2192 ", fmt(mm, 3), " mm USD"))
}

#' Summarise 4MC lines by metric
#' @param lines Data frame of 4MC lines (see [db_get_m4_lines()]).
#' @return Data frame with one row per metric: `metric`, `label`, `unit`,
#'   `value`, `value_mm_usd`, `n_lines`.
#' @export
m4_summary <- function(lines) {
  out <- m4_metrics()
  if (is.null(lines) || !nrow(lines)) {
    out$value <- 0; out$value_mm_usd <- 0; out$n_lines <- 0L
    return(out)
  }
  out$value <- vapply(out$metric, function(k) sum(lines$result_value[lines$metric == k], na.rm = TRUE), 0)
  out$value_mm_usd <- vapply(out$metric, function(k) sum(lines$value_mm_usd[lines$metric == k], na.rm = TRUE), 0)
  out$n_lines <- vapply(out$metric, function(k) sum(lines$metric == k), 0L)
  out
}

#' Monetary value of a set of 4MC figures (expert review, audit)
#'
#' @param values Named list `P` (BOPD), `R` (MMbbl), `category` (reserve
#'   category), `M` (mm USD), `T` (khours), `C` (cost, mm USD). Missing
#'   figures count as zero.
#' @param cfg Configuration list.
#' @return List with `by_metric` (named mm USD vector, P/R/M/T/C), `total`
#'   (value: P + R + M + T), `cost` (C) and `value_to_cost`.
#' @export
m4_value <- function(values, cfg) {
  g <- function(k) {
    v <- suppressWarnings(as.numeric(values[[k]] %||% 0))
    if (!length(v) || is.na(v)) 0 else v
  }
  cat <- values$category %||% names(reserve_values(cfg))[1]
  if (is.na(cat)) cat <- names(reserve_values(cfg))[1]
  mm <- c(P = m4_monetize("P", g("P"), cfg), R = m4_monetize("R", g("R"), cfg, cat),
          M = m4_monetize("M", g("M"), cfg), T = m4_monetize("T", g("T"), cfg), C = g("C"))
  total <- sum(mm[m4_value_metrics])
  list(by_metric = mm, total = total, cost = mm[["C"]],
       value_to_cost = if (mm[["C"]] > 0) total / mm[["C"]] else NA_real_)
}
