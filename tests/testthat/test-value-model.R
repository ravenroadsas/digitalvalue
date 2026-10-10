# synthetic portfolio with known multiplicative relationships
synthetic_pf <- function(n, cfg, noise = 0.2, seed = 1, audited = TRUE) {
  set.seed(seed)
  imp <- sample(rice_levels(cfg, "impact"), n, TRUE)
  con <- sample(rice_levels(cfg, "confidence"), n, TRUE)
  eff <- sample(rice_levels(cfg, "effort"), n, TRUE)
  users <- round(10^stats::runif(n, 1, 4))
  lw <- function(l, d) log(rice_weight(l, d, cfg))
  value <- exp(-2 + 0.8 * log10(users) + lw(imp, "impact") + 0.5 * lw(con, "confidence") +
                 0.7 * lw(eff, "effort") + stats::rnorm(n, 0, noise))
  actual <- exp(log(value) * 0.9 + 0.3 * lw(con, "confidence") + stats::rnorm(n, 0, noise))
  data.frame(id = sprintf("DV-%04d", seq_len(n)), name = "x",
             status = if (audited) "Audited" else "Ready",
             users = users, impact = imp, confidence = con, effort = eff,
             score = rice_score(users, imp, con, eff, cfg), reach_value = reach_value(users),
             planned_value_mm_usd = value, audited_value_mm_usd = actual, adoption_pct = 80,
             stringsAsFactors = FALSE)
}

test_that("features follow the RICE weights", {
  cfg <- test_cfg()
  f <- value_model_features(1000, "L", "High", "M", cfg, expected = 2)
  expect_equal(f$reach_value, 3)
  expect_equal(f$rice_value, 3 * 2 * 0.8)
  expect_equal(f$log_effort, log(2))
  expect_equal(f$log_expected, log(2))
  expect_true(is.na(value_model_features(10, "M", "Low", "S", cfg)$log_expected))
})

test_that("training data per model kind", {
  cfg <- test_cfg()
  pf <- synthetic_pf(6, cfg)
  pf$planned_value_mm_usd[1] <- 0
  pf$score[2] <- NA
  pf$status[3] <- "Prioritized"
  d1 <- value_model_data(pf, cfg, "rice_to_review")
  expect_equal(nrow(d1), 4)
  expect_equal(d1$y, d1$expected)
  d2 <- value_model_data(pf, cfg, "review_to_audit")
  expect_equal(nrow(d2), 3)
  expect_equal(d2$y, pf$audited_value_mm_usd[4:6])
  expect_error(value_model_data(pf, cfg, "nope"), "Unknown")
})

test_that("RICE -> ex-ante model recovers the elasticities", {
  cfg <- test_cfg()
  m <- fit_value_model(value_model_data(synthetic_pf(80, cfg, noise = 0.1), cfg), cfg)
  expect_true(m$ok)
  expect_equal(m$type, "multivariable")
  cf <- stats::setNames(m$record$coef, names(m$record$coef))
  expect_equal(unname(cf["reach_value"]), 0.8, tolerance = 0.1)
  expect_equal(unname(cf["log_impact"]), 1, tolerance = 0.1)
  expect_equal(unname(cf["log_effort"]), 0.7, tolerance = 0.1)
  expect_gt(m$record$r2, 0.9)
})

test_that("ex-ante -> audited model recovers its elasticities", {
  cfg <- test_cfg()
  pf <- synthetic_pf(80, cfg, noise = 0.05)
  m <- fit_value_model(value_model_data(pf, cfg, "review_to_audit"), cfg, "review_to_audit")
  expect_equal(m$type, "multivariable")
  expect_equal(unname(m$record$coef["log_expected"]), 0.9, tolerance = 0.05)
  expect_equal(unname(m$record$coef["log_confidence"]), 0.3, tolerance = 0.1)
  p <- predict_realised(m$record, c(1, 10), "High", "M", cfg)
  expect_gt(p$predicted[2], p$predicted[1])
})

test_that("stored-record prediction equals predict.lm prediction interval", {
  cfg <- test_cfg()
  d <- value_model_data(synthetic_pf(30, cfg, noise = 0.4), cfg)
  m <- fit_value_model(d, cfg)
  nd <- value_model_features(c(500, 20), c("L", "S"), c("High", "Low"), c("M", "XS"), cfg)
  ref <- exp(stats::predict(m$fit, nd, interval = "prediction", level = 0.8))
  got <- predict_record(m$record, nd)
  expect_equal(got$predicted, unname(ref[, "fit"]), tolerance = 1e-10)
  expect_equal(got$low, unname(ref[, "lwr"]), tolerance = 1e-10)
  expect_equal(got$high, unname(ref[, "upr"]), tolerance = 1e-10)
  # and after a database roundtrip
  con <- test_con()
  db_publish_value_model(con, m$record, "t", "u")
  back <- db_active_value_model(con, "rice_to_review")
  expect_equal(predict_record(back, nd), got, tolerance = 1e-9)
})

test_that("range widens with the level; unknown inputs give NA", {
  cfg <- test_cfg()
  d <- value_model_data(synthetic_pf(40, cfg, noise = 0.4), cfg)
  m <- fit_value_model(d, cfg)
  p <- predict_value(m$record, 1000, "L", "High", "M", cfg)
  cfg95 <- cfg; cfg95$params$value_model_interval <- 0.95
  p95 <- predict_value(fit_value_model(d, cfg95)$record, 1000, "L", "High", "M", cfg95)
  expect_gt(p95$high - p95$low, p$high - p$low)
  expect_true(p$low < p$predicted && p$predicted < p$high)
  expect_true(is.na(predict_value(m$record, 100, "ZZ", "High", "M", cfg)$predicted))
  expect_true(is.na(predict_value(NULL, 100, "M", "High", "M", cfg)$predicted))
  expect_equal(interval_labels(0.8), c("P10", "P90"))
  expect_gt(range_factor(m$record), 1)
})

test_that("simple model with few data, none with very few, constant predictors dropped", {
  cfg <- test_cfg()
  m <- fit_value_model(value_model_data(synthetic_pf(5, cfg), cfg), cfg)
  expect_equal(m$type, "simple")
  expect_equal(m$record$terms, "log_rice_value")
  m0 <- fit_value_model(value_model_data(synthetic_pf(3, cfg), cfg), cfg)
  expect_false(m0$ok)
  expect_match(m0$message, "at least 4")
  pf <- synthetic_pf(20, cfg)
  pf$confidence <- "High"
  m2 <- fit_value_model(value_model_data(pf, cfg), cfg)
  expect_false("log_confidence" %in% m2$record$terms)
  cc <- record_coefficients(m2$record)
  expect_false(anyNA(cc$estimate))
  expect_equal(nrow(record_coefficients(NULL)), 0)
})

test_that("assess_record reports coverage on new data", {
  cfg <- test_cfg()
  d <- value_model_data(synthetic_pf(40, cfg), cfg)
  m <- fit_value_model(d, cfg)
  a <- assess_record(m$record, d)
  expect_true(a$coverage > 0.5 && a$coverage <= 1)
  expect_true(is.na(assess_record(NULL, d)$coverage))
})

test_that("demo data publish both calibrated models at seeding", {
  cfg <- test_cfg()
  con <- test_con()
  seed_demo_data(con, cfg)
  r1 <- db_active_value_model(con, "rice_to_review")
  r2 <- db_active_value_model(con, "review_to_audit")
  expect_equal(r1$type, "multivariable")
  expect_false(is.null(r2))
  expect_length(ensure_value_models(con, cfg), 0)
  pf <- compute_portfolio(db_portfolio(con), cfg)
  est <- portfolio_estimates(pf, r1, cfg)
  expect_equal(is.na(est$predicted), is.na(pf$score))
})

test_that("value model chart options", {
  cfg <- test_cfg()
  m <- fit_value_model(value_model_data(synthetic_pf(20, cfg), cfg), cfg)
  o <- value_correlation_option(m)
  expect_equal(o$xAxis$type, "log")
  expect_length(o$series, 4)
  expect_length(o$series[[1]]$data, 20)
  f <- value_fit_option(m$data, "rice_to_review")
  expect_equal(length(f$series[[1]]$data) + length(f$series[[2]]$data), 20)
  m0 <- fit_value_model(value_model_data(synthetic_pf(2, cfg), cfg), cfg)
  expect_length(value_correlation_option(m0)$series, 1)
})

test_that("scatter can plot estimated value for unvalued initiatives", {
  cfg <- test_cfg()
  pf <- synthetic_pf(4, cfg)
  pf$planned_value_mm_usd[2] <- 0
  d <- prioritization_data(pf, cfg, "estimate", estimate = c(9, 7, 9, 9))
  expect_equal(d$estimated, c(FALSE, TRUE, FALSE, FALSE))
  expect_equal(d$y[2], 7)
})
