test_that("formatting helpers", {
  expect_equal(fmt_num(1234.567, 1, " x"), "1,234.6 x")
  expect_equal(fmt_num(NA), "–")
  expect_s3_class(kpi_tile("a", 1, "b", "good"), "shiny.tag")
  expect_match(as.character(status_badge("Prioritized")), "#1E9E5A")
  expect_match(as.character(callout("x", title = "t", type = "danger")), "dv-callout-danger")
})

test_that("user identification honours Posit Connect session and planning group", {
  s <- list(user = "ana", groups = c("planning", "eng"))
  old <- Sys.getenv("DV_PLANNING_GROUP")
  on.exit(Sys.setenv(DV_PLANNING_GROUP = old))
  Sys.setenv(DV_PLANNING_GROUP = "planning")
  expect_true(app_user(s)$is_planning)
  expect_equal(app_user(s)$user, "ana")
  expect_false(app_user(list(user = "bob", groups = "eng"))$is_planning)
  Sys.setenv(DV_PLANNING_GROUP = "")
  expect_true(app_user(list(user = "bob"))$is_planning)
})

test_that("demo seeding is idempotent", {
  cfg <- test_cfg()
  con <- test_con()
  ids <- seed_demo_data(con, cfg)
  expect_gt(length(ids), 5)
  expect_length(seed_demo_data(con, cfg), 0)
})
