test_that("formatting helpers", {
  expect_equal(fmt_num(1234.567, 1, " x"), "1,234.6 x")
  expect_equal(fmt_num(NA), "–")
  expect_s3_class(kpi_tile("a", 1, "b", "good"), "shiny.tag")
  expect_match(as.character(status_badge("Prioritized")), mono$c700)
  expect_match(as.character(callout("x", title = "t", type = "high")), "dv-callout-high")
})

test_that("two user levels from Posit Connect identity", {
  withr::local_envvar(DV_SUPERUSER_GROUP = "dv-admins", DV_SUPERUSERS = "carla, dan", DV_DEV_ROLE = "")
  expect_equal(user_role("ana", c("dv-admins", "eng")), "superuser")
  expect_equal(user_role("dan"), "superuser")
  expect_equal(user_role("bob", "eng"), "contributor")
  expect_equal(app_user(list(user = "bob", groups = "eng"))$role, "contributor")
  withr::local_envvar(DV_SUPERUSER_GROUP = "", DV_SUPERUSERS = "")
  expect_equal(user_role("bob"), "superuser")
  withr::local_envvar(DV_DEV_ROLE = "contributor")
  expect_equal(user_role("bob"), "contributor")
})

test_that("contributors can only register; everything else needs a superuser", {
  m <- permission_matrix()
  expect_equal(m$action[m$contributor], "register")
  expect_true(all(m$superuser))
  expect_true(can("contributor", "register"))
  expect_false(can("contributor", "edit_registration"))
  expect_false(can(list(role = "contributor"), "audit"))
  expect_true(can("superuser", "calibrate"))
  expect_error(can("superuser", "fly"), "Unknown action")
})

test_that("demo seeding is idempotent", {
  cfg <- test_cfg()
  con <- test_con()
  ids <- seed_demo_data(con, cfg)
  expect_gt(length(ids), 5)
  expect_setequal(db_get_value_models(con)$kind, names(value_model_kinds))
  expect_length(seed_demo_data(con, cfg), 0)
})
