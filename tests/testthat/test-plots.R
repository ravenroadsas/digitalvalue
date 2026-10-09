test_that("status groups and colours highlight prioritized and executing", {
  expect_equal(plot_group(c("Prioritized", "In execution", "Phase II", "Ready", "Audited", "Rejected")),
               c("Prioritized", "In execution", "In assessment", "Ready for decision",
                 "Closed / audited", "Rejected"))
  expect_true(all(status_all %in% names(status_colors())))
  expect_false(group_colors()[["Prioritized"]] == group_colors()[["In execution"]])
})

test_that("prioritisation data and option", {
  cfg <- test_cfg()
  con <- test_con()
  seed_demo_data(con, cfg)
  pf <- compute_portfolio(db_portfolio(con), cfg)
  d <- prioritization_data(pf, cfg, "score")
  expect_equal(nrow(d), sum(!is.na(pf$score)))
  expect_true(all(abs(d$x - rice_ordinal(d$effort, "effort", cfg)) <= 0.25))
  dv <- prioritization_data(pf, cfg, "value")
  expect_equal(dv$y, pf$planned_value_mm_usd[!is.na(pf$score)])
  o <- prioritization_option(pf, cfg, "score", highlight = d$id[1])
  expect_equal(vapply(o$series, `[[`, "", "name"), names(group_colors()))
  n_points <- sum(vapply(o$series, function(s) length(s$data), 0L))
  expect_equal(n_points, nrow(d))
  expect_equal(sum(vapply(o$series, function(s) !is.null(s$markLine), TRUE)), 1)
  expect_equal(nrow(prioritization_data(pf[0, ], cfg)), 0)
  expect_type(prioritization_option(pf[0, ], cfg), "list")
})

test_that("other chart options are well formed", {
  cfg <- test_cfg()
  con <- test_con()
  seed_demo_data(con, cfg)
  pf <- compute_portfolio(db_portfolio(con), cfg)
  so <- status_option(pf)
  expect_equal(so$xAxis$data, status_all)
  pva <- plan_vs_actual(data.frame(metric = "P", result_value = 1, value_mm_usd = 1), NULL)
  expect_equal(plan_actual_option(pva)$series[[1]]$data[1], 1)
  bo <- prmt_breakdown_option(prmt_summary(data.frame(metric = "T", result_value = 1, value_mm_usd = 0.2)))
  expect_length(bo$series[[1]]$data, 1)
  pe <- data.frame(process_phase = "A", instances = 1L, minutes = 2, users = 1L)
  expect_equal(phase_effort_option(pe)$yAxis$data, "A")
})

test_that("echarts widget renders from an option", {
  skip_if_not_installed("echarts4r")
  w <- echart_from_option(list(series = list()))
  expect_s3_class(w, "htmlwidget")
})
