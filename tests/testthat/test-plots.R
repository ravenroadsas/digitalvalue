test_that("status groups and colours highlight prioritized and executing", {
  expect_equal(plot_group(c("Prioritized", "In execution", "4M valuation", "Ready", "Audited", "Rejected")),
               c("Prioritized", "In execution", "In appraisal", "Ready for decision",
                 "Closed / audited", "Rejected"))
  expect_true(all(status_all %in% names(status_colors())))
  gs <- group_style()
  expect_false(gs$color[gs$group == "Prioritized"] == gs$color[gs$group == "In execution"])
  expect_false(gs$symbol[gs$group == "Prioritized"] == gs$symbol[gs$group == "In execution"])
  expect_true(all(unlist(mono) == toupper(unlist(mono))))
})

test_that("prioritisation data and option", {
  cfg <- test_cfg()
  con <- test_con()
  seed_demo_data(con, cfg)
  pf <- compute_portfolio(db_portfolio(con), cfg)
  d <- prioritization_data(pf, cfg, "rice_value")
  expect_equal(nrow(d), sum(!is.na(pf$score)))
  sc <- pf[!is.na(pf$score), ]
  # y = reach x impact x confidence, i.e. RICE score x effort weight
  expect_equal(d$y, sc$score * rice_weight(sc$effort, "effort", cfg))
  expect_error(prioritization_data(pf, cfg, "score"))
  expect_true(all(abs(d$x - rice_ordinal(d$effort, "effort", cfg)) <= 0.25))
  dv <- prioritization_data(pf, cfg, "value")
  expect_equal(dv$y, pf$planned_value_mm_usd[!is.na(pf$score)])
  o <- prioritization_option(pf, cfg, "rice_value", highlight = d$id[1])
  nms <- vapply(o$series, `[[`, "", "name")
  expect_equal(nms[seq_along(group_style()$group)], group_style()$group)
  gate <- o$series[[length(o$series)]]
  expect_match(gate$name, "Valuation gate \\(score")
  # staircase: threshold x effort weight at every effort level
  steps <- vapply(gate$data, `[`, 0, 2)
  expect_equal(unique(steps), cfg$params$gate2_score_min * rice_weight(rice_levels(cfg, "effort"), "effort", cfg))
  expect_gte(o$yAxis$max, max(d$y))
  n_points <- sum(vapply(o$series[seq_along(group_style()$group)], function(s) length(s$data), 0L))
  expect_equal(n_points, nrow(d))
  ov <- prioritization_option(pf, cfg, "value")
  expect_equal(vapply(ov$series, `[[`, "", "name"), group_style()$group)
  expect_null(ov$yAxis$max)
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
  lc <- m4_lifecycle(data.frame(metric = "P", result_value = 1, value_mm_usd = 1), NULL, NULL, cfg)
  lo <- lifecycle_option(lc)
  expect_equal(lo$series[[1]]$data[1], 1)
  expect_equal(lo$series[[3]]$data, rep(0, 5))
  bo <- m4_breakdown_option(m4_summary(data.frame(metric = "T", result_value = 1, value_mm_usd = 0.2)))
  expect_length(bo$series[[1]]$data, 1)
  pe <- data.frame(process_phase = "A", instances = 1L, minutes = 2, users = 1L)
  expect_equal(phase_effort_option(pe)$yAxis$data, "A")
})

test_that("echarts widget renders from an option", {
  skip_if_not_installed("echarts4r")
  w <- echart_from_option(list(series = list()))
  expect_s3_class(w, "htmlwidget")
})
