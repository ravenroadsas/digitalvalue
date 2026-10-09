test_that("Phase II gate on effort or score", {
  cfg <- test_cfg()
  expect_equal(requires_phase2(c("L", "S", "S", NA), c(0.1, 5, 0.1, NA), cfg),
               c(TRUE, TRUE, FALSE, FALSE))
})

test_that("Phase III gate on value or cost", {
  cfg <- test_cfg()
  expect_equal(requires_phase3(c(6, 1, 1, NA), c(0, 2, 0.1, NA), cfg),
               c(TRUE, TRUE, FALSE, FALSE))
})

test_that("derive_status walks the assessment phases and keeps decisions", {
  st <- derive_status(
    current = c("Phase I", "Phase I", "Phase II", "Phase III", "Phase III", "Phase III", "In execution"),
    has_rice = c(FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE),
    req2 = c(FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE),
    has_valuation = c(FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE),
    req3 = c(FALSE, FALSE, TRUE, TRUE, TRUE, FALSE, TRUE),
    review_decision = c(NA, NA, NA, "Approve", "Reject", NA, NA))
  expect_equal(st, c("Phase I", "Phase II", "Phase III", "Ready", "Rejected", "Ready", "In execution"))
})

test_that("allowed workflow actions", {
  expect_equal(unname(allowed_actions("Ready")), c("Prioritized", "Rejected"))
  expect_equal(unname(allowed_actions("In execution")), "Closed")
  expect_length(allowed_actions("Audited"), 0)
  expect_equal(unname(allowed_actions("Phase II")), "Rejected")
})

test_that("portfolio enrichment, alerts and reasons", {
  cfg <- test_cfg()
  con <- test_con()
  seed_demo_data(con, cfg)
  pf <- compute_portfolio(db_portfolio(con), cfg)
  expect_true(all(pf$derived_status == pf$status))
  expect_true(all(c("Phase I", "Phase II", "Phase III", "Ready", "Prioritized", "In execution",
                    "Closed", "Audited", "Rejected") %in% pf$status))
  a <- assessment_alerts(pf)
  expect_true(all(c("warning", "danger", "info", "success") %in% a$severity))
  expect_equal(a$severity[1], "danger")
  r <- pf[pf$status == "Phase III", ][1, ]
  expect_true(length(gate_reasons(r, cfg)$phase3) > 0)
  expect_equal(nrow(assessment_alerts(pf[0, ])), 0)
})

test_that("stage_steps marks the first pending step as current", {
  cfg <- test_cfg()
  con <- test_con()
  seed_demo_data(con, cfg)
  pf <- compute_portfolio(db_portfolio(con), cfg)
  s <- stage_steps(pf[pf$status == "Phase II", ][1, ])
  expect_equal(sum(s$state == "current"), 1)
  expect_equal(s$state[s$label == "Phase II · PRMT"], "current")
  s2 <- stage_steps(pf[pf$status == "Audited", ][1, ])
  expect_equal(s2$state[nrow(s2)], "done")
})
