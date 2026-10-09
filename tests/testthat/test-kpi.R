test_that("realisation percentage", {
  expect_equal(realization_pct(c(8, 1, 1), c(10, 0, NA)), c(80, NA, NA))
})

test_that("decision lead times", {
  h <- data.frame(initiative_id = c("A", "A", "B"),
                  to_status = c("Phase I", "Prioritized", "Phase I"),
                  changed_at = c("2026-01-01 00:00:00", "2026-01-11 00:00:00", "2026-01-01 00:00:00"))
  lt <- decision_lead_times(h)
  expect_equal(lt$initiative_id, "A")
  expect_equal(lt$lead_time_days, 10)
  expect_equal(nrow(decision_lead_times(NULL)), 0)
})

test_that("portfolio KPIs on demo data", {
  cfg <- test_cfg()
  con <- test_con()
  seed_demo_data(con, cfg)
  pf <- compute_portfolio(db_portfolio(con), cfg)
  k <- portfolio_kpis(pf, db_get_status_history(con))
  expect_equal(k$n_total, nrow(pf))
  expect_equal(k$n_prioritized + k$n_execution + k$n_closed + k$n_rejected + k$n_assessment, nrow(pf))
  expect_gt(k$realized_value_mm_usd, 0)
  expect_true(k$realization_rate_pct > 0 && k$realization_rate_pct < 100)
  expect_true(k$gate2_compliance_pct <= 100)
  expect_gt(k$median_lead_time_days, 0)
  s <- status_summary(pf)
  expect_equal(sum(s$n), nrow(pf))
  expect_equal(s$status, status_all)
})

test_that("plan vs actual", {
  plan <- data.frame(metric = "P", result_value = 10, value_mm_usd = 1)
  act <- data.frame(metric = "P", result_value = 8, value_mm_usd = 0.8)
  d <- plan_vs_actual(plan, act)
  expect_equal(d$realization_pct[d$metric == "P"], 80)
  expect_true(is.na(d$realization_pct[d$metric == "M"]))
})
