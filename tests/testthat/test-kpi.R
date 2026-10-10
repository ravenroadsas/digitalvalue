test_that("realisation percentage", {
  expect_equal(realization_pct(c(8, 1, 1), c(10, 0, NA)), c(80, NA, NA))
})

test_that("decision lead times", {
  h <- data.frame(initiative_id = c("A", "A", "B"),
                  to_status = c("Registered", "Prioritized", "Registered"),
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

test_that("4M lifecycle compares estimate, review and audit", {
  cfg <- test_cfg()
  lines <- data.frame(metric = "P", result_value = 10, value_mm_usd = m4_monetize("P", 10, cfg))
  rv <- data.frame(p_bopd = 8, r_mmbbl = 0, r_category = "2P", m_mm_usd = 0.5, t_khours = 0)
  au <- data.frame(p_bopd = 6, r_mmbbl = 0, r_category = "2P", m_mm_usd = 0.25, t_khours = 0)
  lc <- m4_lifecycle(lines, rv, au, cfg)
  expect_equal(lc$estimate[lc$metric == "P"], 10)
  expect_equal(lc$review[lc$metric == "P"], 8)
  expect_equal(lc$actual[lc$metric == "M"], 0.25)
  expect_equal(lc$realization_pct[lc$metric == "P"], 75)
  expect_equal(lc$realization_pct[lc$metric == "M"], 50)
  only <- m4_lifecycle(lines, NULL, NULL, cfg)
  expect_true(all(is.na(only$review)))
})

test_that("mean adoption KPI over audited initiatives", {
  cfg <- test_cfg()
  con <- test_con()
  seed_demo_data(con, cfg)
  pf <- compute_portfolio(db_portfolio(con), cfg)
  k <- portfolio_kpis(pf, db_get_status_history(con))
  expect_equal(k$mean_adoption_pct, mean(pf$adoption_pct[pf$status == "Audited"]))
})
