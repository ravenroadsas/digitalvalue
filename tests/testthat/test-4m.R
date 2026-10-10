test_that("workover example: 10 jobs x 30 BOPD x (70% - 60%)", {
  cfg <- test_cfg()
  r <- m4_calculate("p_jobs_success",
                      list(n_jobs = 10, bopd_per_job = 30, success_before = 60, success_after = 70), cfg)
  expect_equal(r$metric, "P")
  expect_equal(r$value, 30)
  expect_equal(r$unit, "BOPD")
  expect_equal(r$value_mm_usd, 30 * 365 * 35 / 1e6)
  expect_match(r$formula_text, "10 jobs")
  expect_equal(r$params$success_after, 70)
})

test_that("missing parameters take defaults and non-numeric fails", {
  cfg <- test_cfg()
  r <- m4_calculate("t_users", list(n_users = 10), cfg)
  expect_equal(r$params$hours_week, 2)
  expect_equal(r$value, 10 * 2 * 46 / 1000)
  expect_error(m4_calculate("t_users", list(n_users = "abc"), cfg), "numeric")
  expect_error(m4_calculate("nope", list(), cfg), "Unknown 4M method")
})

test_that("monetisation per metric", {
  cfg <- test_cfg()
  expect_equal(m4_monetize("P", 100, cfg), 100 * 365 * 35 / 1e6)
  expect_equal(m4_monetize("R", 2, cfg, "1P"), 20)
  expect_equal(m4_monetize("M", 1.5, cfg), 1.5)
  expect_equal(time_value_usd_hour(cfg), 95000 / 1800 * 3)
  expect_equal(m4_monetize("T", 1, cfg), 1000 * 95000 / 1800 * 3 / 1e6)
  expect_error(m4_monetize("R", 1, cfg, "9P"), "category")
  expect_error(m4_monetize("X", 1, cfg), "Unknown 4M metric")
})

test_that("every registered method computes with its defaults", {
  cfg <- test_cfg()
  for (m in names(m4_methods(cfg))) {
    r <- m4_calculate(m, list(), cfg)
    expect_true(is.finite(r$value_mm_usd), info = m)
    expect_true(r$metric %in% m4_metrics()$metric, info = m)
  }
  ch <- m4_method_choices("R", cfg)
  expect_setequal(unname(ch), c("r_direct", "r_recovery"))
})

test_that("reserves recovery-factor method uses the category value", {
  cfg <- test_cfg()
  r <- m4_calculate("r_recovery", list(ooip_mmbbl = 100, rf_before = 20, rf_after = 21,
                                         category = "3P"), cfg)
  expect_equal(r$value, 1)
  expect_equal(r$value_mm_usd, 3)
})

test_that("m4_summary aggregates by metric", {
  lines <- data.frame(metric = c("P", "P", "T"), result_value = c(10, 5, 2),
                      value_mm_usd = c(0.1, 0.05, 0.3))
  s <- m4_summary(lines)
  expect_equal(s$value[s$metric == "P"], 15)
  expect_equal(s$n_lines[s$metric == "T"], 1L)
  expect_equal(sum(m4_summary(NULL)$value_mm_usd), 0)
})

test_that("direct 4M figures are monetised like the calculation lines", {
  cfg <- test_cfg()
  v <- m4_value(list(P = 100, R = 2, category = "1P", M = 0.5, T = 1), cfg)
  expect_equal(v$by_metric[["P"]], m4_monetize("P", 100, cfg))
  expect_equal(v$by_metric[["R"]], 20)
  expect_equal(v$by_metric[["M"]], 0.5)
  expect_equal(v$total, sum(v$by_metric))
  expect_equal(m4_value(list(), cfg)$total, 0)
  expect_equal(m4_value(list(P = NA, R = 1, category = NA), cfg)$by_metric[["R"]],
               reserve_values(cfg)[[1]])
})

test_that("4M figures implied by calculation lines keep the reserve category", {
  cfg <- test_cfg()
  con <- test_con()
  id <- reg(con, cfg)
  db_add_m4_line(con, id, m4_calculate("r_direct", list(mmbbl = 2, category = "3P"), cfg))
  db_add_m4_line(con, id, m4_calculate("p_direct", list(bopd = 40), cfg))
  v <- m4_from_lines(db_get_m4_lines(con, id))
  expect_equal(v$R, 2)
  expect_equal(v$P, 40)
  expect_equal(v$category, "3P")
  expect_equal(m4_from_lines(db_get_m4_lines(con, "none"))$category, "2P")
})
