#' Seed demonstration data
#'
#' Populates an empty database with a realistic portfolio covering every stage
#' of the process (assessment, prioritised, in execution, audited, rejected).
#' @param con A DBI connection (schema initialised).
#' @param cfg Configuration list.
#' @return Invisibly, the ids created.
#' @export
seed_demo_data <- function(con, cfg) {
  if (nrow(db_get_initiatives(con))) return(invisible(character(0)))
  t0 <- Sys.time() - 200 * 86400
  at <- function(days) t0 + days * 86400
  demo <- list(
    list(name = "Workover candidate selection with ML", bu = "Production", owner = "Ana Ruiz",
         desc = "Ranks workover candidates using production history and logs to increase job success rate.",
         cost = 1.1, rice = list(400, "XL", "High", "M", "Two pilots in field A."),
         plan = list(list("p_jobs_success", list(n_jobs = 10, bopd_per_job = 30, success_before = 60, success_after = 70),
                          "10 workovers/yr, each 30 BOPD; ML lifts success from 60% to 70%.")),
         review = "Approve", final = "In execution"),
    list(name = "Digital twin - gas compression", bu = "Facilities", owner = "Luis Mendez",
         desc = "Real-time model of the compression train to cut unplanned downtime.",
         cost = 2.4, rice = list(150, "XL", "High", "XL", "Vendor benchmark data."),
         plan = list(list("p_uptime", list(base_bopd = 18000, uptime_before = 93, uptime_after = 95),
                          "Uptime of compression-constrained production improves by 2 pts.")),
         review = "Approve", final = "Prioritized"),
    list(name = "Automated daily production report", bu = "Production", owner = "Marta Gil",
         desc = "Replaces manual spreadsheet consolidation of daily production.",
         cost = 0.1, rice = list(250, "M", "Certain", "S", "Manual process timed: 25 min/day/engineer."),
         plan = list(list("t_users", list(n_users = 60, hours_week = 2, weeks_year = 46),
                          "60 engineers save 2 h/week.")),
         review = NULL, final = "Audited",
         actual_factor = 0.83, adoption = 92),
    list(name = "Waterflood optimisation advisor", bu = "Reservoir", owner = "Jorge Paz",
         desc = "Optimises injection allocation with capacitance-resistance models.",
         cost = 1.5, rice = list(80, "XL", "Low", "L", "Literature shows 0.5-1% RF uplift."),
         plan = list(list("r_recovery", list(ooip_mmbbl = 300, rf_before = 28, rf_after = 28.5, category = "2P"),
                          "0.5% RF uplift on the main waterflood."),
                     list("p_decline", list(base_bopd = 9000, decline_before = 14, decline_after = 12),
                          "Decline flattening in the first year.")),
         review = NULL, final = NULL),
    list(name = "Drilling NPT analytics", bu = "Drilling", owner = "Sofia Lara",
         desc = "Identifies non-productive-time patterns across rigs.",
         cost = 0.6, rice = list(120, "L", "High", "M", "NPT currently 18%."),
         plan = list(list("m_cost_saving", list(events_per_year = 40, saving_kusd = 35),
                          "40 NPT events/yr avoided, 35 kUSD each.")),
         review = NULL, final = "Closed"),
    list(name = "Chatbot for HSE procedures", bu = "HSE", owner = "Pedro Ortiz",
         desc = "Conversational search over HSE procedures.",
         cost = 0.15, rice = list(3000, "S", "High", "S", "Survey of 200 field staff."),
         plan = NULL, review = NULL, final = "Prioritized"),
    list(name = "Corrosion risk prediction", bu = "Integrity", owner = "Elena Rios",
         desc = "Predicts pipeline corrosion hot spots to prevent failures.",
         cost = 1.2, rice = list(60, "L", "Low", "L", "Inspection data incomplete."),
         plan = list(list("m_risk_avoided", list(cost_mm_usd = 12, prob_before = 8, prob_after = 5),
                          "Major leak cost 12 mm USD; probability 8% -> 5%.")),
         review = NULL, final = NULL),
    list(name = "Seismic interpretation copilot", bu = "Exploration", owner = "Diego Vega",
         desc = "AI-assisted horizon and fault picking.",
         cost = 3, rice = list(40, "XL", "Moonshot", "XL", "Early-stage technology."),
         plan = list(list("r_direct", list(mmbbl = 2, category = "contingent"),
                          "Faster screening of leads; contingent resources.")),
         review = "Reject", final = NULL),
    list(name = "Inventory optimisation (warehouse)", bu = "Supply chain", owner = "Laura Soto",
         desc = "Reduces spare-parts stock with demand forecasting.",
         cost = 0.4, rice = list(50, "M", "High", "L", NULL),
         plan = NULL, review = NULL, final = NULL),
    list(name = "Lab results digitalisation", bu = "Production", owner = "Ines Mora",
         desc = "Lab results flow directly into the production database.",
         cost = 0.05, rice = list(90, "S", "Certain", "XS", "Simple integration, vendor API available."),
         plan = NULL, review = NULL, final = NULL),
    list(name = "Gas lift optimisation", bu = "Production", owner = "Historical",
         desc = "Completed initiative (history used to calibrate the value model).",
         cost = 0.5, rice = list(300, "L", "High", "M", NULL),
         plan = list(list("p_uptime", list(base_bopd = 8000, uptime_before = 93, uptime_after = 94), "Historical valuation.")),
         review = NULL, final = "Audited", actual_factor = 0.9, adoption = 85),
    list(name = "Pump-off controller analytics", bu = "Production", owner = "Historical",
         desc = "Completed initiative (history used to calibrate the value model).",
         cost = 0.5, rice = list(150, "M", "High", "M", NULL),
         plan = list(list("p_direct", list(bopd = 25), "Historical valuation.")),
         review = NULL, final = "Audited", actual_factor = 0.8, adoption = 70),
    list(name = "Electronic permit to work", bu = "HSE", owner = "Historical",
         desc = "Completed initiative (history used to calibrate the value model).",
         cost = 0.5, rice = list(1500, "S", "Certain", "M", NULL),
         plan = list(list("t_users", list(n_users = 300, hours_week = 0.5, weeks_year = 46), "Historical valuation.")),
         review = NULL, final = "Audited", actual_factor = 0.95, adoption = 95),
    list(name = "Predictive maintenance - rotating equipment", bu = "Maintenance", owner = "Historical",
         desc = "Completed initiative (history used to calibrate the value model).",
         cost = 0.5, rice = list(200, "XL", "Low", "L", NULL),
         plan = list(list("m_risk_avoided", list(cost_mm_usd = 20, prob_before = 10, prob_after = 5), "Historical valuation.")),
         review = NULL, final = "Audited", actual_factor = 0.6, adoption = 55),
    list(name = "Reservoir surveillance dashboard", bu = "Reservoir", owner = "Historical",
         desc = "Completed initiative (history used to calibrate the value model).",
         cost = 0.5, rice = list(120, "L", "High", "S", NULL),
         plan = list(list("p_decline", list(base_bopd = 6000, decline_before = 12, decline_after = 11), "Historical valuation.")),
         review = NULL, final = "Audited", actual_factor = 1.1, adoption = 100),
    list(name = "Procurement spend analytics", bu = "Supply chain", owner = "Historical",
         desc = "Completed initiative (history used to calibrate the value model).",
         cost = 0.5, rice = list(80, "M", "High", "M", NULL),
         plan = list(list("m_cost_saving", list(events_per_year = 30, saving_kusd = 15), "Historical valuation.")),
         review = NULL, final = "Audited", actual_factor = 0.85, adoption = 80),
    list(name = "Flare monitoring with cameras", bu = "HSE", owner = "Historical",
         desc = "Completed initiative (history used to calibrate the value model).",
         cost = 0.5, rice = list(60, "S", "Certain", "S", NULL),
         plan = list(list("m_direct", list(mm_usd = 0.12), "Historical valuation.")),
         review = NULL, final = "Audited", actual_factor = 1, adoption = 90),
    list(name = "Integrated asset model", bu = "Reservoir", owner = "Historical",
         desc = "Completed initiative (history used to calibrate the value model).",
         cost = 0.5, rice = list(50, "XL", "Low", "XL", NULL),
         plan = list(list("r_recovery", list(ooip_mmbbl = 150, rf_before = 30, rf_after = 30.4, category = "2P"), "Historical valuation.")),
         review = NULL, final = "Audited", actual_factor = 0.7, adoption = 60),
    list(name = "Mobile field data capture", bu = "Operations", owner = "Raul Cano",
         desc = "Replaces paper field tickets with a mobile app.",
         cost = 0.3, rice = list(600, "M", "Low", "S", "Pilot requested by two field teams."),
         plan = NULL, review = NULL, final = NULL)
  )
  ids <- character(0)
  scaled_m4 <- function(id, factor) {
    v <- m4_from_lines(db_get_m4_lines(con, id))
    for (k in c("P", "R", "M", "T")) v[[k]] <- v[[k]] * factor
    v
  }
  for (k in seq_along(demo)) {
    d <- demo[[k]]
    day <- 3 * k
    r <- d$rice
    id <- db_register_initiative(
      con, list(name = d$name, description = d$desc, owner = d$owner, business_unit = d$bu,
                cost_mm_usd = d$cost),
      rice_assess(r[[1]], r[[2]], r[[3]], r[[4]], cfg, r[[5]] %||% NA),
      user = if (identical(d$owner, "Historical")) "demo" else d$owner, time = at(day))
    ids <- c(ids, id)
    for (l in d$plan) {
      db_add_m4_line(con, id, m4_calculate(l[[1]], l[[2]], cfg), comment = l[[3]],
                     user = "superuser", time = at(day + 10))
    }
    sync_status_one(con, cfg, id, at(day + 10))
    if (!is.null(d$review)) {
      v <- scaled_m4(id, 0.9)
      cost <- db_get_initiatives(con, id)$cost_mm_usd
      db_add_review(con, id, d$review, v, m4_value(v, cfg)$total, cost,
                    comment = if (d$review == "Approve") "Value haircut 10% for execution risk."
                              else "Technology not mature; revisit next year.",
                    user = "superuser", time = at(day + 18))
      sync_status_one(con, cfg, id, at(day + 18))
    }
    path <- switch(d$final %||% "",
      "Prioritized" = "Prioritized",
      "In execution" = c("Prioritized", "In execution"),
      "Closed" = c("Prioritized", "In execution", "Closed"),
      "Audited" = c("Prioritized", "In execution", "Closed"),
      character(0))
    for (j in seq_along(path)) {
      db_set_status(con, id, path[j], "superuser", time = at(day + 20 + 15 * j))
    }
    if (identical(d$final, "Audited")) {
      v <- scaled_m4(id, d[["actual_factor"]])
      pf <- compute_portfolio(db_portfolio(con), cfg)
      expected <- pf$planned_value_mm_usd[pf$id == id]
      db_add_audit(con, id, v, d[["adoption"]], m4_value(v, cfg)$total, expected,
                   "Measured 6 months after go-live.", user = "superuser", time = at(day + 81))
      db_set_status(con, id, "Audited", "superuser", time = at(day + 81))
    }
  }
  ensure_value_models(con, cfg, "system")
  invisible(ids)
}

sync_status_one <- function(con, cfg, id, time) {
  pf <- compute_portfolio(db_portfolio(con), cfg)
  row <- pf[pf$id == id, ]
  if (row$derived_status != row$status) {
    db_set_status(con, id, row$derived_status, "system", "Automatic (appraisal gates)", time)
  }
}
