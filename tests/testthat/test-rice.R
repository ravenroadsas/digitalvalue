test_that("reach uses log10 of users and floors at 1 user", {
  expect_equal(reach_value(c(10, 1000, 0, 1)), c(1, 3, 0, 0))
})

test_that("RICE score = log10(users) x impact x confidence / effort", {
  cfg <- test_cfg()
  # log10(1000)=3, L=2, High=0.8, M=2
  expect_equal(rice_score(1000, "L", "High", "M", cfg), 3 * 2 * 0.8 / 2)
  expect_true(is.na(rice_score(1000, "ZZ", "High", "M", cfg)))
})

test_that("levels and ordinals follow the weights table", {
  cfg <- test_cfg()
  expect_equal(rice_levels(cfg, "confidence"), c("Moonshot", "Low", "High", "Certain"))
  expect_equal(rice_levels(cfg, "effort"), c("XS", "S", "M", "L", "XL"))
  expect_equal(rice_ordinal(c("XS", "XL"), "effort", cfg), c(1L, 5L))
  expect_equal(rice_weight("Moonshot", "confidence", cfg), 0.2)
})

test_that("rice_assess validates inputs", {
  cfg <- test_cfg()
  a <- rice_assess(100, "M", "Low", "S", cfg, "why")
  expect_equal(a$score, 2 * 1 * 0.5 / 1)
  expect_equal(a$rationale, "why")
  expect_error(rice_assess(-1, "M", "Low", "S", cfg), "non-negative")
  expect_error(rice_assess(10, "M", "Sure", "S", cfg), "confidence")
})
