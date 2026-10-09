test_that("CSV configuration loads and is typed", {
  cfg <- test_cfg()
  expect_equal(cfg$source, "csv")
  expect_type(cfg$params$netback_usd_bbl, "double")
  expect_equal(cfg$params$gate2_effort_min, "L")
  expect_setequal(unique(cfg$scales$dimension), c("impact", "confidence", "effort"))
  expect_true(all(c("pattern", "activity", "process_phase") %in% names(cfg$event_map)))
})

test_that("build_config validates required parameters", {
  cfg <- test_cfg()
  bad <- cfg$parameters[cfg$parameters$key != "netback_usd_bbl", ]
  expect_error(build_config(bad, cfg$scales, cfg$event_map), "netback_usd_bbl")
})

test_that("reserve values are extracted by category", {
  rv <- reserve_values(test_cfg())
  expect_named(rv, c("1P", "2P", "3P", "contingent"))
  expect_equal(rv[["2P"]], 6)
})

test_that("auto source falls back to CSV without a pins board", {
  old <- Sys.getenv("DV_PINS_BOARD")
  Sys.unsetenv("DV_PINS_BOARD")
  on.exit(if (nzchar(old)) Sys.setenv(DV_PINS_BOARD = old))
  expect_equal(load_config()$source, "csv")
})
