# synthetic portfolio with a known multiplicative relationship
synthetic_pf <- function(n, cfg, noise = 0.2, seed = 1) {
  set.seed(seed)
  imp <- sample(rice_levels(cfg, "impact"), n, TRUE)
  con <- sample(rice_levels(cfg, "confidence"), n, TRUE)
  eff <- sample(rice_levels(cfg, "effort"), n, TRUE)
  users <- round(10^stats::runif(n, 1, 4))
  value <- exp(-2 + 0.8 * log10(users) + 1 * log(rice_weight(imp, "impact", cfg)) +
                 0.5 * log(rice_weight(con, "confidence", cfg)) +
                 0.7 * log(rice_weight(eff, "effort", cfg)) + stats::rnorm(n, 0, noise))
  data.frame(id = sprintf("DV-%04d", seq_len(n)), name = "x", status = "Audited",
             users = users, impact = imp, confidence = con, effort = eff,
             score = rice_score(users, imp, con, eff, cfg), reach_value = reach_value(users),
             planned_value_mm_usd = value,
             stringsAsFactors = FALSE)
}

test_that("features follow the RICE weights", {
  cfg <- test_cfg()
  f <- value_model_features(1000, "L", "High", "M", cfg)
  expect_equal(f$reach_value, 3)
  expect_equal(f$rice_value, 3 * 2 * 0.8)
  expect_equal(f$log_effort, log(2))
})

test_that("training data keeps only scored and valued initiatives", {
  cfg <- test_cfg()
  pf <- synthetic_pf(6, cfg)
  pf$planned_value_mm_usd[1] <- 0
  pf$score[2] <- NA
  d <- value_model_data(pf, cfg)
  expect_equal(nrow(d), 4)
  expect_equal(d$log_value, log(d$value_mm_usd))
})

test_that("multivariable model recovers the elasticities", {
  cfg <- test_cfg()
  m <- fit_value_model(value_model_data(synthetic_pf(80, cfg, noise = 0.1), cfg), cfg)
  expect_true(m$ok)
  expect_equal(m$type, "multivariable")
  cf <- stats::setNames(m$coefficients$estimate, m$coefficients$term)
  expect_equal(unname(cf["reach_value"]), 0.8, tolerance = 0.1)
  expect_equal(unname(cf["log_impact"]), 1, tolerance = 0.1)
  expect_equal(unname(cf["log_effort"]), 0.7, tolerance = 0.1)
  expect_gt(m$r2, 0.9)
  expect_gt(mean(m$data$inside), 0.6)
})

test_that("prediction gives an ordered range that widens with the level", {
  cfg <- test_cfg()
  d <- value_model_data(synthetic_pf(40, cfg, noise = 0.4), cfg)
  m <- fit_value_model(d, cfg)
  p <- predict_value(m, c(1000, 50), c("L", "S"), c("High", "Low"), c("M", "S"), cfg)
  expect_true(all(p$low < p$median & p$median < p$high))
  expect_gt(p$median[1], p$median[2])
  cfg95 <- cfg; cfg95$params$value_model_interval <- 0.95
  p95 <- predict_value(fit_value_model(d, cfg95), 1000, "L", "High", "M", cfg95)
  expect_gt(p95$high - p95$low, p$high[1] - p$low[1])
  expect_equal(interval_labels(0.8), c("P10", "P90"))
  expect_true(is.na(predict_value(m, 100, "ZZ", "High", "M", cfg)$median))
})

test_that("falls back to a simple model with few data and to none with very few", {
  cfg <- test_cfg()
  m <- fit_value_model(value_model_data(synthetic_pf(5, cfg), cfg), cfg)
  expect_equal(m$type, "simple")
  expect_equal(m$terms, "log_rice_value")
  expect_false(is.na(predict_value(m, 100, "M", "High", "M", cfg)$median))
  m0 <- fit_value_model(value_model_data(synthetic_pf(3, cfg), cfg), cfg)
  expect_false(m0$ok)
  expect_match(m0$message, "at least 4")
  expect_true(all(is.na(predict_value(m0, 100, "M", "High", "M", cfg))))
})

test_that("predictors without variation are dropped", {
  cfg <- test_cfg()
  pf <- synthetic_pf(20, cfg)
  pf$confidence <- "High"
  m <- fit_value_model(value_model_data(pf, cfg), cfg)
  expect_false("log_confidence" %in% m$terms)
  expect_false(anyNA(m$coefficients$estimate))
})

test_that("demo portfolio supports a multivariable model and estimates", {
  cfg <- test_cfg()
  con <- test_con()
  seed_demo_data(con, cfg)
  pf <- compute_portfolio(db_portfolio(con), cfg)
  m <- fit_value_model(value_model_data(pf, cfg), cfg)
  expect_equal(m$type, "multivariable")
  est <- portfolio_estimates(pf, m, cfg)
  expect_equal(nrow(est), nrow(pf))
  expect_equal(is.na(est$median), is.na(pf$score))
})

test_that("value model chart options", {
  cfg <- test_cfg()
  m <- fit_value_model(value_model_data(synthetic_pf(20, cfg), cfg), cfg)
  o <- value_correlation_option(m)
  expect_equal(o$xAxis$type, "log")
  expect_length(o$series, 4)
  expect_length(o$series[[1]]$data, 20)
  f <- value_fit_option(m)
  expect_length(f$series[[1]]$data, 20)
  m0 <- fit_value_model(value_model_data(synthetic_pf(2, cfg), cfg), cfg)
  expect_length(value_correlation_option(m0)$series, 1)
  expect_length(value_fit_option(m0)$series[[1]]$data, 0)
})

test_that("scatter can plot estimated value for unvalued initiatives", {
  cfg <- test_cfg()
  pf <- synthetic_pf(4, cfg)
  pf$planned_value_mm_usd[2] <- 0
  d <- prioritization_data(pf, cfg, "estimate", estimate = c(9, 7, 9, 9))
  expect_equal(d$estimated, c(FALSE, TRUE, FALSE, FALSE))
  expect_equal(d$y[2], 7)
  o <- prioritization_option(pf, cfg, "estimate", estimate = c(9, 7, 9, 9))
  pts <- unlist(lapply(o$series, `[[`, "data"), recursive = FALSE)
  expect_equal(sum(vapply(pts, function(p) isTRUE(p$estimated), TRUE)), 1)
})
