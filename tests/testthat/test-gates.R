test_that("valuation gate on effort or score", {
  cfg <- test_cfg()
  expect_equal(requires_valuation(c("L", "S", "S", NA), c(0.1, 5, 0.1, NA), cfg),
               c(TRUE, TRUE, FALSE, FALSE))
})

test_that("review gate on value or cost", {
  cfg <- test_cfg()
  expect_equal(requires_review(c(6, 1, 1, NA), c(0, 2, 0.1, NA), cfg), c(TRUE, TRUE, FALSE, FALSE))
})

test_that("derive_status: Evaluated only when every required evaluation is complete", {
  st <- derive_status(
    current = c("Recorded", "Recorded", "Recorded", "Recorded", "Recorded", "Recorded", "Delivered"),
    has_rice = c(FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE),
    req_val = c(FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE),
    has_valuation = c(FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE),
    validated = c(FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, FALSE),
    req_rev = c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE),
    review_decision = c(NA, NA, NA, NA, "Rework", "Approve", NA))
  expect_equal(st, c("Recorded", "Evaluated", "Recorded", "Evaluated", "Recorded", "Evaluated", "Delivered"))
  expect_equal(status_all, c("Recorded", "Evaluated", "Delivered", "Audited"))
})

test_that("only delivery actions remain in the workflow", {
  expect_equal(allowed_actions("Evaluated"), c(deliver = "Delivered"))
  expect_equal(allowed_actions("Delivered"), c(undeliver = "Evaluated"))
  expect_length(allowed_actions("Recorded"), 0)
  expect_length(allowed_actions("Audited"), 0)
})

test_that("portfolio enrichment, pending steps, alerts and reasons", {
  cfg <- test_cfg()
  con <- test_con()
  seed_demo_data(con, cfg)
  pf <- compute_portfolio(db_portfolio(con), cfg)
  expect_true(all(pf$derived_status == pf$status))
  expect_setequal(unique(pf$status), status_all)
  expect_setequal(stats::na.omit(unique(pf$pending_step)), c("4MC valuation", "4MC validation", "Expert review"))
  expect_true(all(is.na(pf$pending_step[pf$status %in% c("Evaluated", "Delivered", "Audited")])))
  appr <- pf[!is.na(pf$review_decision) & pf$review_decision == "Approve", ][1, ]
  expect_equal(appr$planned_value_mm_usd, appr$review_value_mm_usd)
  a <- assessment_alerts(pf)
  expect_equal(a$severity[1], "high")
  expect_true(any(grepl("validation pending with dval", a$alert)))
  expect_setequal(unique(a$stage), c("evaluation", "realisation"))
  r <- pf[pf$pending_step %in% "Expert review", ][1, ]
  expect_true(length(gate_reasons(r, cfg)$review) > 0)
  expect_equal(nrow(assessment_alerts(pf[0, ])), 0)
})

test_that("lifecycle steps follow evaluation and realisation", {
  cfg <- test_cfg()
  con <- test_con()
  seed_demo_data(con, cfg)
  pf <- compute_portfolio(db_portfolio(con), cfg)
  s <- lifecycle_steps(pf[pf$pending_step %in% "4MC valuation", ][1, ])
  expect_equal(sum(s$state == "current"), 1)
  expect_equal(s$state[s$label == "4MC valuation"], "current")
  expect_equal(unique(s$phase), c("Evaluation", "Realisation"))
  s1 <- lifecycle_steps(pf[pf$pending_step %in% "4MC validation", ][1, ])
  expect_equal(s1$state[s1$label == "4MC validation"], "current")
  expect_match(s1$note[s1$label == "4MC validation"], "with dval")
  s2 <- lifecycle_steps(pf[pf$status == "Audited", ][1, ])
  expect_equal(s2$state[s2$label == "Value audit"], "done")
  expect_match(s2$note[6], "adoption")
})

test_that("evaluation gaps flag required-but-missing evaluations", {
  cfg <- test_cfg()
  con <- test_con()
  seed_demo_data(con, cfg)
  pf <- compute_portfolio(db_portfolio(con), cfg)
  g <- evaluation_gaps(pf)
  ps <- stats::setNames(pf$pending_step, pf$id)
  st <- stats::setNames(pf$status, pf$id)
  expect_true(all(g$valuation[ps[g$id] %in% "4MC valuation"] == "missing"))
  expect_true(all(g$validation[ps[g$id] %in% "4MC validation"] == "pending"))
  expect_true(all(g$review[ps[g$id] %in% "Expert review"] == "missing"))
  expect_true(all(g$audit[st[g$id] == "Delivered"] == "missing"))
  expect_true(all(g$audit[st[g$id] == "Audited"] == "done"))
  expect_true(all(g$n_missing[st[g$id] %in% c("Evaluated", "Audited")] == 0))
  expect_setequal(names(g), c("id", "name", "valuation", "validation", "review", "audit", "n_missing"))
})
