test_that("valuation gate on effort or score", {
  cfg <- test_cfg()
  expect_equal(requires_valuation(c("L", "S", "S", NA), c(0.1, 5, 0.1, NA), cfg),
               c(TRUE, TRUE, FALSE, FALSE))
})

test_that("review gate on value or cost", {
  cfg <- test_cfg()
  expect_equal(requires_review(c(6, 1, 1, NA), c(0, 2, 0.1, NA), cfg), c(TRUE, TRUE, FALSE, FALSE))
})

test_that("derive_status walks the appraisal steps and keeps decisions", {
  st <- derive_status(
    current = c("Registered", "Registered", "4M valuation", "Expert review", "Expert review",
                "Expert review", "In execution"),
    has_rice = c(FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE),
    req_val = c(FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE),
    has_valuation = c(FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE),
    req_rev = c(FALSE, FALSE, TRUE, TRUE, TRUE, FALSE, TRUE),
    review_decision = c(NA, NA, NA, "Approve", "Reject", NA, NA))
  expect_equal(st, c("Registered", "4M valuation", "Expert review", "Ready", "Rejected", "Ready",
                     "In execution"))
})

test_that("allowed workflow actions", {
  expect_equal(unname(allowed_actions("Ready")), c("Prioritized", "Rejected"))
  expect_equal(unname(allowed_actions("In execution")), "Closed")
  expect_length(allowed_actions("Audited"), 0)
  expect_equal(unname(allowed_actions("4M valuation")), "Rejected")
  expect_equal(unname(allowed_actions("Rejected")), "Registered")
})

test_that("portfolio enrichment, ex-ante value, alerts and reasons", {
  cfg <- test_cfg()
  con <- test_con()
  seed_demo_data(con, cfg)
  pf <- compute_portfolio(db_portfolio(con), cfg)
  expect_true(all(pf$derived_status == pf$status))
  expect_true(all(c("4M valuation", "Expert review", "Ready", "Prioritized", "In execution",
                    "Closed", "Audited", "Rejected") %in% pf$status))
  appr <- pf[!is.na(pf$review_decision) & pf$review_decision == "Approve", ][1, ]
  expect_equal(appr$planned_value_mm_usd, appr$review_value_mm_usd)
  a <- assessment_alerts(pf)
  expect_equal(a$severity[1], "high")
  expect_setequal(unique(a$stage), c("appraisal", "realisation"))
  r <- pf[pf$status == "Expert review", ][1, ]
  expect_true(length(gate_reasons(r, cfg)$review) > 0)
  expect_equal(nrow(assessment_alerts(pf[0, ])), 0)
})

test_that("lifecycle steps mark the first pending step as current", {
  cfg <- test_cfg()
  con <- test_con()
  seed_demo_data(con, cfg)
  pf <- compute_portfolio(db_portfolio(con), cfg)
  s <- lifecycle_steps(pf[pf$status == "4M valuation", ][1, ])
  expect_equal(sum(s$state == "current"), 1)
  expect_equal(s$state[s$label == "4M valuation"], "current")
  expect_equal(unique(s$phase), c("Appraisal", "Realisation"))
  s2 <- lifecycle_steps(pf[pf$status == "Audited", ][1, ])
  expect_equal(s2$state[s2$label == "Value audit"], "done")
  expect_match(s2$note[6], "adoption")
})
