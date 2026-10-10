# Calibrated value models ----------------------------------------------------------
# Two log-linear regressions chain the lifecycle:
#
#   rice_to_review  : RICE inputs  -> ex-ante value (expert review, else 4MC)
#     log(value)  = b0 + b1 log10(users) + b2 log(impact) + b3 log(confidence)
#                      + b4 log(effort)
#   review_to_audit : ex-ante value -> audited (realised) value
#     log(actual) = b0 + b1 log(ex-ante value) + b2 log(confidence) + b3 log(effort)
#
# A superuser fits a *candidate* on the current data, compares it with the
# active version and publishes it (Admin > Value models). Published versions
# store coefficients, covariance matrix and residual sigma, so predictions and
# prediction intervals are computed from the stored record without refitting.
# With fewer than `value_model_min_n` observations a simple one-predictor
# model is used; with fewer than 4 no model can be fitted.

#' Value model kinds
#' @export
value_model_kinds <- c(rice_to_review = "RICE \u2192 ex-ante value",
                       review_to_audit = "Ex-ante value \u2192 realised value")

model_spec <- function(kind) {
  switch(kind,
    rice_to_review = list(
      full = c(reach_value = "Reach (log10 users)", log_impact = "log Impact",
               log_confidence = "log Confidence", log_effort = "log Effort"),
      simple = c(log_rice_value = "log RICE value (R \u00d7 I \u00d7 C)"),
      x_label = "RICE value (log10 users \u00d7 impact \u00d7 confidence)",
      y_label = "Ex-ante value (mm USD)"),
    review_to_audit = list(
      full = c(log_expected = "log Ex-ante value", log_confidence = "log Confidence",
               log_effort = "log Effort"),
      simple = c(log_expected = "log Ex-ante value"),
      x_label = "Ex-ante value (mm USD)",
      y_label = "Audited value (mm USD)"),
    stop("Unknown model kind: ", kind))
}

#' Model features from RICE inputs (and ex-ante value)
#' @param users,impact,confidence,effort RICE inputs.
#' @param cfg Configuration list.
#' @param expected Optional ex-ante value (mm USD).
#' @return Data frame of features.
#' @export
value_model_features <- function(users, impact, confidence, effort, cfg, expected = NA_real_) {
  iw <- rice_weight(impact, "impact", cfg)
  cw <- rice_weight(confidence, "confidence", cfg)
  ew <- rice_weight(effort, "effort", cfg)
  rv <- reach_value(users)
  expected <- as.numeric(expected)
  data.frame(users = as.numeric(users), reach_value = rv, impact = impact,
             confidence = confidence, effort = effort, rice_value = rv * iw * cw,
             log_impact = log(iw), log_confidence = log(cw), log_effort = log(ew),
             log_rice_value = log(pmax(rv * iw * cw, 1e-6)),
             expected = expected,
             log_expected = ifelse(!is.na(expected) & expected > 0, log(pmax(expected, 1e-9)), NA_real_),
             stringsAsFactors = FALSE)
}

#' Training data for a value model
#' @param pf Output of [compute_portfolio()].
#' @param cfg Configuration list.
#' @param kind Model kind.
#' @return Data frame with features, `x` (chart axis), `y` (target, mm USD) and
#'   `log_y`.
#' @export
value_model_data <- function(pf, cfg, kind = "rice_to_review") {
  ok_val <- !is.na(pf$planned_value_mm_usd) & pf$planned_value_mm_usd > 0
  d <- switch(kind,
    rice_to_review = pf[!is.na(pf$score) & ok_val, , drop = FALSE],
    review_to_audit = pf[!is.na(pf$score) & ok_val & pf$status == "Audited" &
                           !is.na(pf$audited_value_mm_usd) & pf$audited_value_mm_usd > 0, , drop = FALSE],
    stop("Unknown model kind: ", kind))
  f <- value_model_features(d$users, d$impact, d$confidence, d$effort, cfg, d$planned_value_mm_usd)
  out <- cbind(data.frame(id = d$id, name = d$name, status = d$status, stringsAsFactors = FALSE), f)
  if (kind == "rice_to_review") {
    out$x <- out$rice_value
    out$y <- d$planned_value_mm_usd
  } else {
    out$x <- d$planned_value_mm_usd
    out$y <- d$audited_value_mm_usd
    out$adoption_pct <- d$adoption_pct
  }
  out$log_y <- log(out$y)
  out
}

#' Fit a candidate value model
#' @param data Output of [value_model_data()].
#' @param cfg Configuration list.
#' @param kind Model kind.
#' @return A list with `ok`, `kind`, `type`, `n`, `message`, `data` (with
#'   fitted values), `fit` (lm), `record` (see [model_record()]),
#'   `cor_pearson`, `cor_spearman`.
#' @export
fit_value_model <- function(data, cfg, kind = "rice_to_review") {
  p <- cfg$params
  min_n <- as.integer(p$value_model_min_n %||% 8)
  level <- as.numeric(p$value_model_interval %||% 0.8)
  spec <- model_spec(kind)
  n <- nrow(data)
  out <- list(ok = FALSE, kind = kind, type = "none", n = n, level = level, min_n = min_n,
              data = data, cor_pearson = NA_real_, cor_spearman = NA_real_)
  if (n >= 3) {
    out$cor_pearson <- suppressWarnings(stats::cor(log(pmax(data$x, 1e-9)), data$log_y))
    out$cor_spearman <- suppressWarnings(stats::cor(data$x, data$y, method = "spearman"))
  }
  if (n < 4) {
    out$message <- sprintf("Not enough observations (%d); at least 4 are needed.", n)
    return(out)
  }
  varies <- function(v) length(unique(round(v, 9))) > 1
  if (n >= min_n) {
    terms <- names(spec$full)[vapply(names(spec$full), function(k) varies(data[[k]]), logical(1))]
    terms <- terms[seq_len(min(length(terms), n - 3))]
    type <- "multivariable"
  } else {
    terms <- names(spec$simple)[vapply(names(spec$simple), function(k) varies(data[[k]]), logical(1))]
    type <- "simple"
  }
  if (!length(terms)) {
    out$message <- "Predictors do not vary across the observations."
    return(out)
  }
  fit <- stats::lm(stats::as.formula(paste("log_y ~", paste(terms, collapse = " + "))), data = data)
  out$ok <- TRUE
  out$type <- type
  out$fit <- fit
  out$record <- model_record(fit, kind, type, level)
  out$data <- assess_record(out$record, data)$data
  out$message <- sprintf("%s model on %d observations%s.",
                         if (type == "simple") "Simple" else "Multivariable", n,
                         if (type == "simple") sprintf(" (multivariable from %d)", min_n) else "")
  out
}

#' Serialisable record of a fitted model
#' @param fit An `lm` fit.
#' @param kind Model kind.
#' @param type `"multivariable"` or `"simple"`.
#' @param level Prediction interval level.
#' @return List `kind`, `type`, `terms`, `coef`, `vcov`, `sigma`,
#'   `df_residual`, `level`, `n`, `r2`, `adj_r2`.
#' @export
model_record <- function(fit, kind, type, level) {
  s <- summary(fit)
  list(kind = kind, type = type, terms = attr(stats::terms(fit), "term.labels"),
       coef = stats::coef(fit), vcov = stats::vcov(fit), sigma = s$sigma,
       df_residual = fit$df.residual, level = level, n = length(fit$residuals),
       r2 = s$r.squared, adj_r2 = s$adj.r.squared)
}

#' Rebuild a model record from a stored row (see [db_active_value_model()])
#' @param row One row of the `value_models` table.
#' @return A model record.
#' @export
record_from_row <- function(row) {
  coef <- unlist(jsonlite::fromJSON(row$coef_json))
  v <- jsonlite::fromJSON(row$vcov_json)
  v <- matrix(as.numeric(v), nrow = length(coef), dimnames = list(names(coef), names(coef)))
  list(kind = row$kind, type = row$type, terms = as.character(jsonlite::fromJSON(row$terms_json)),
       coef = coef, vcov = v, sigma = row$sigma, df_residual = as.integer(row$df_residual),
       level = row$level, n = as.integer(row$n), r2 = row$r2, adj_r2 = row$adj_r2,
       model_id = row$model_id, created_at = row$created_at, created_by = row$created_by)
}

#' Predict from a model record (median and prediction interval, mm USD)
#'
#' Identical to `predict.lm(interval = "prediction")` back-transformed from
#' the log scale, computed from the stored coefficients and covariance.
#' @param record A model record (or `NULL`).
#' @param newdata Data frame with the model terms.
#' @return Data frame `predicted`, `low`, `high`.
#' @export
predict_record <- function(record, newdata) {
  n <- nrow(newdata)
  out <- data.frame(predicted = rep(NA_real_, n), low = NA_real_, high = NA_real_)
  if (is.null(record) || !n) return(out)
  if (!all(record$terms %in% names(newdata))) return(out)
  X <- cbind(1, as.matrix(newdata[, record$terms, drop = FALSE]))
  ok <- stats::complete.cases(X)
  if (!any(ok)) return(out)
  X <- X[ok, , drop = FALSE]
  b <- record$coef[c("(Intercept)", record$terms)]
  V <- record$vcov[names(b), names(b), drop = FALSE]
  fit <- as.vector(X %*% b)
  se <- sqrt(rowSums((X %*% V) * X) + record$sigma^2)
  q <- stats::qt(1 - (1 - record$level) / 2, record$df_residual)
  out$predicted[ok] <- exp(fit)
  out$low[ok] <- exp(fit - q * se)
  out$high[ok] <- exp(fit + q * se)
  out
}

#' Anticipated ex-ante value from RICE inputs
#' @param record Active `rice_to_review` record.
#' @param users,impact,confidence,effort RICE inputs.
#' @param cfg Configuration list.
#' @return Data frame `predicted`, `low`, `high` (mm USD).
#' @export
predict_value <- function(record, users, impact, confidence, effort, cfg) {
  predict_record(record, value_model_features(users, impact, confidence, effort, cfg))
}

#' Anticipated realised value from the ex-ante value
#' @param record Active `review_to_audit` record.
#' @param expected Ex-ante value (mm USD).
#' @param confidence,effort RICE levels.
#' @param cfg Configuration list.
#' @return Data frame `predicted`, `low`, `high` (mm USD).
#' @export
predict_realised <- function(record, expected, confidence, effort, cfg) {
  predict_record(record, value_model_features(NA, NA, confidence, effort, cfg, expected))
}

#' Evaluate a model record on data: predictions and interval coverage
#' @param record A model record (or `NULL`).
#' @param data Output of [value_model_data()].
#' @return List `data` (with `predicted`, `low`, `high`, `inside`), `coverage`
#'   (share inside the interval), `rmse_log`.
#' @export
assess_record <- function(record, data) {
  pr <- predict_record(record, data)
  d <- cbind(data[, setdiff(names(data), c("predicted", "low", "high", "inside")), drop = FALSE], pr)
  d$inside <- d$y >= d$low & d$y <= d$high
  ok <- !is.na(d$predicted)
  list(data = d, coverage = if (any(ok)) mean(d$inside[ok]) else NA_real_,
       rmse_log = if (any(ok)) sqrt(mean((log(d$predicted[ok]) - d$log_y[ok])^2)) else NA_real_)
}

#' Coefficient table of a record (with standard errors and p-values)
#' @param record A model record.
#' @return Data frame `term`, `label`, `estimate`, `std_error`, `p_value`.
#' @export
record_coefficients <- function(record) {
  if (is.null(record)) {
    return(data.frame(term = character(0), label = character(0), estimate = numeric(0),
                      std_error = numeric(0), p_value = numeric(0)))
  }
  spec <- model_spec(record$kind)
  lbl <- c(`(Intercept)` = "Intercept", spec$full, spec$simple)
  se <- sqrt(diag(record$vcov))[names(record$coef)]
  tval <- record$coef / se
  data.frame(term = names(record$coef), label = unname(lbl[names(record$coef)]),
             estimate = unname(record$coef), std_error = unname(se),
             p_value = unname(2 * stats::pt(-abs(tval), record$df_residual)),
             stringsAsFactors = FALSE)
}

#' Percentile labels of the prediction interval (e.g. P10 / P90)
#' @param level Interval level.
#' @return Character vector of length 2.
#' @export
interval_labels <- function(level) {
  a <- (1 - level) / 2
  paste0("P", round(100 * c(a, 1 - a)))
}

#' Typical multiplicative width of the prediction interval
#' @param record A model record.
#' @return Factor (e.g. 3 means the range spans median/3 to median x 3).
#' @export
range_factor <- function(record) {
  if (is.null(record)) return(NA_real_)
  exp(stats::qt(1 - (1 - record$level) / 2, record$df_residual) * record$sigma)
}

#' Publish an initial calibration for kinds without an active model
#' @param con A DBI connection.
#' @param cfg Configuration list.
#' @param user User recorded as publisher.
#' @return Kinds published.
#' @export
ensure_value_models <- function(con, cfg, user = "system") {
  pf <- compute_portfolio(db_portfolio(con), cfg)
  done <- character(0)
  for (k in names(value_model_kinds)) {
    if (!is.null(db_active_value_model(con, k))) next
    m <- fit_value_model(value_model_data(pf, cfg, k), cfg, k)
    if (m$ok) {
      db_publish_value_model(con, m$record, "Initial calibration", user)
      done <- c(done, k)
    }
  }
  done
}

#' ECharts option: observed relationship with the fitted line and range
#' @param model Output of [fit_value_model()].
#' @return An ECharts option list.
#' @export
value_correlation_option <- function(model) {
  d <- model$data
  spec <- model_spec(model$kind)
  pts <- lapply(seq_len(nrow(d)), function(i) list(
    value = unname(c(d$x[i], d$y[i])), id = d$id[i], name = d$name[i]))
  series <- list(list(name = "Observations", type = "scatter", symbolSize = 9, data = pts,
                      itemStyle = list(color = mono$c600), z = 3))
  if (nrow(d) >= 3 && length(unique(d$x)) > 1) {
    lx <- log(pmax(d$x, 1e-9))
    fit <- stats::lm(d$log_y ~ lx)
    xs <- exp(seq(min(lx), max(lx), length.out = 30))
    pr <- exp(stats::predict(fit, data.frame(lx = log(xs)), interval = "prediction",
                             level = model$level))
    line <- function(y) lapply(seq_along(xs), function(i) unname(c(xs[i], y[i])))
    lab <- paste(interval_labels(model$level), collapse = "\u2013")
    series <- c(series, list(
      list(name = "Fit", type = "line", showSymbol = FALSE, data = line(pr[, "fit"]),
           lineStyle = list(color = mono$c900, width = 2), itemStyle = list(color = mono$c900)),
      list(name = lab, type = "line", showSymbol = FALSE, data = line(pr[, "lwr"]),
           lineStyle = list(color = mono$c400, type = "dashed", width = 1),
           itemStyle = list(color = mono$c400)),
      list(name = lab, type = "line", showSymbol = FALSE, data = line(pr[, "upr"]),
           lineStyle = list(color = mono$c400, type = "dashed", width = 1),
           itemStyle = list(color = mono$c400))))
  }
  list(
    textStyle = list(fontSize = 10, color = mono$c700),
    grid = list(left = 55, right = 20, top = 30, bottom = 45),
    legend = list(top = 0, textStyle = list(fontSize = 10)),
    tooltip = list(trigger = "item", formatter = js(
      "function (p) { if (!p.data || !p.data.id) return p.seriesName;",
      " return '<b>' + p.data.id + ' \u00b7 ' + p.data.name + '</b><br/>x: ' +",
      " p.value[0].toFixed(2) + '<br/>y: ' + p.value[1].toFixed(2) + ' mm USD'; }")),
    xAxis = list(type = "log", name = spec$x_label, nameLocation = "middle", nameGap = 26,
                 splitLine = list(show = FALSE)),
    yAxis = list(type = "log", name = spec$y_label,
                 splitLine = list(lineStyle = list(color = mono$c100))),
    series = series
  )
}

#' ECharts option: predicted vs observed for a model record
#' @param assessed Output of [assess_record()] (`$data`).
#' @param kind Model kind.
#' @return An ECharts option list.
#' @export
value_fit_option <- function(assessed, kind) {
  d <- assessed[!is.na(assessed$predicted), , drop = FALSE]
  spec <- model_spec(kind)
  rng <- unname(range(c(d$predicted, d$y, 0.01, 1), na.rm = TRUE))
  list(
    textStyle = list(fontSize = 10, color = mono$c700),
    grid = list(left = 55, right = 20, top = 30, bottom = 45),
    legend = list(top = 0, textStyle = list(fontSize = 10)),
    tooltip = list(trigger = "item", formatter = js(
      "function (p) { if (!p.data || !p.data.id) return p.seriesName;",
      " return '<b>' + p.data.id + '</b><br/>Predicted: ' + p.value[0].toFixed(2) +",
      " ' mm USD<br/>Observed: ' + p.value[1].toFixed(2) + ' mm USD'; }")),
    xAxis = list(type = "log", name = "Predicted (mm USD)", nameLocation = "middle", nameGap = 26,
                 splitLine = list(show = FALSE)),
    yAxis = list(type = "log", name = spec$y_label,
                 splitLine = list(lineStyle = list(color = mono$c100))),
    series = list(
      list(name = "Inside range", type = "scatter", symbolSize = 9,
           itemStyle = list(color = mono$c700),
           data = lapply(which(d$inside), function(i)
             list(value = unname(c(d$predicted[i], d$y[i])), id = d$id[i]))),
      list(name = "Outside range", type = "scatter", symbolSize = 9, symbol = "emptyCircle",
           itemStyle = list(color = mono$c900, borderWidth = 2),
           data = lapply(which(!d$inside), function(i)
             list(value = unname(c(d$predicted[i], d$y[i])), id = d$id[i]))),
      list(name = "1:1", type = "line", showSymbol = FALSE,
           lineStyle = list(color = mono$c400, type = "dashed"), itemStyle = list(color = mono$c400),
           data = list(c(rng[1], rng[1]), c(rng[2], rng[2]))))
  )
}

#' Value-model estimates for every initiative of the portfolio
#' @param pf Output of [compute_portfolio()].
#' @param record Active `rice_to_review` record.
#' @param cfg Configuration list.
#' @return Data frame aligned with `pf`: `predicted`, `low`, `high` (mm USD).
#' @export
portfolio_estimates <- function(pf, record, cfg) {
  predict_value(record, pf$users, pf$impact, pf$confidence, pf$effort, cfg)
}
