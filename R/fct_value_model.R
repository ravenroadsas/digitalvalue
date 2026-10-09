# Value model ------------------------------------------------------------------
# Learns the relationship between the Phase I RICE inputs and the Phase II
# monetary value (PRMT total, mm USD) of the initiatives already valued, so
# that a value range can be anticipated while an initiative is still in
# Phase I.
#
# RICE is multiplicative, so the regression is log-linear:
#   log(value) = b0 + b1 * log10(users) + b2 * log(impact) + b3 * log(confidence)
#                   + b4 * log(effort) + e
# i.e. value = exp(b0) * users^(b1/ln 10) * impact^b2 * confidence^b3 * effort^b4.
# Effort is a predictor (bigger projects tend to carry bigger value), not a
# divisor as in the RICE score. The range is the regression prediction
# interval back-transformed from the log scale (median and P_low - P_high).
# With few valued initiatives a simple model on the RICE value
# (reach x impact x confidence) is used instead.

#' Training data for the value model
#'
#' Initiatives with a RICE score and a positive monetary value (planning
#' validated value when approved, otherwise the Phase II PRMT total).
#' @param pf Output of [compute_portfolio()].
#' @param cfg Configuration list.
#' @return Data frame `id`, `name`, `status`, `users`, `reach_value`, `impact`,
#'   `confidence`, `effort`, `rice_value`, `value_mm_usd`, `log_impact`,
#'   `log_confidence`, `log_effort`, `log_value`.
#' @export
value_model_data <- function(pf, cfg) {
  d <- pf[!is.na(pf$score) & !is.na(pf$planned_value_mm_usd) & pf$planned_value_mm_usd > 0, ,
          drop = FALSE]
  out <- value_model_features(d$users, d$impact, d$confidence, d$effort, cfg)
  out <- cbind(data.frame(id = d$id, name = d$name, status = d$status, stringsAsFactors = FALSE),
               out)
  out$value_mm_usd <- d$planned_value_mm_usd
  out$log_value <- log(out$value_mm_usd)
  out
}

#' Model features from RICE inputs
#' @param users,impact,confidence,effort RICE inputs.
#' @param cfg Configuration list.
#' @return Data frame of features.
#' @export
value_model_features <- function(users, impact, confidence, effort, cfg) {
  iw <- rice_weight(impact, "impact", cfg)
  cw <- rice_weight(confidence, "confidence", cfg)
  ew <- rice_weight(effort, "effort", cfg)
  rv <- reach_value(users)
  data.frame(users = as.numeric(users), reach_value = rv, impact = impact,
             confidence = confidence, effort = effort,
             rice_value = rv * iw * cw,
             log_impact = log(iw), log_confidence = log(cw), log_effort = log(ew),
             log_rice_value = log(pmax(rv * iw * cw, 1e-6)),
             stringsAsFactors = FALSE)
}

value_model_terms <- c(reach_value = "Reach (log10 users)", log_impact = "log Impact",
                       log_confidence = "log Confidence", log_effort = "log Effort")

#' Fit the value model
#'
#' Multivariable log-linear regression when at least `value_model_min_n`
#' valued initiatives exist; a simple regression on the RICE value
#' (reach x impact x confidence) with at least 4; otherwise no model.
#' Predictors without variation in the data are dropped.
#' @param data Output of [value_model_data()].
#' @param cfg Configuration list.
#' @return A list of class `dv_value_model` with `ok`, `type`, `n`, `fit`,
#'   `terms`, `coefficients`, `r2`, `adj_r2`, `sigma`, `cor_pearson`,
#'   `cor_spearman`, `level`, `data` (with fitted values) and `message`.
#' @export
fit_value_model <- function(data, cfg) {
  p <- cfg$params
  min_n <- as.integer(p$value_model_min_n %||% 8)
  level <- as.numeric(p$value_model_interval %||% 0.8)
  n <- nrow(data)
  out <- structure(list(ok = FALSE, type = "none", n = n, level = level, min_n = min_n,
                        data = data), class = "dv_value_model")
  if (n >= 3) {
    out$cor_pearson <- suppressWarnings(stats::cor(data$log_rice_value, data$log_value))
    out$cor_spearman <- suppressWarnings(stats::cor(data$rice_value, data$value_mm_usd,
                                                    method = "spearman"))
  }
  if (n < 4) {
    out$message <- sprintf("Not enough valued initiatives (%d); at least 4 are needed.", n)
    return(out)
  }
  varies <- function(v) length(unique(round(v, 9))) > 1
  if (n >= min_n) {
    terms <- names(value_model_terms)[vapply(names(value_model_terms),
                                             function(k) varies(data[[k]]), logical(1))]
    # keep at least 2 residual degrees of freedom
    terms <- terms[seq_len(min(length(terms), n - 3))]
    type <- "multivariable"
  } else {
    terms <- if (varies(data$log_rice_value)) "log_rice_value" else character(0)
    type <- "simple"
  }
  if (!length(terms)) {
    out$message <- "Predictors do not vary across the valued initiatives."
    return(out)
  }
  f <- stats::as.formula(paste("log_value ~", paste(terms, collapse = " + ")))
  fit <- stats::lm(f, data = data)
  s <- summary(fit)
  ct <- s$coefficients
  out$ok <- TRUE
  out$type <- type
  out$fit <- fit
  out$terms <- terms
  out$coefficients <- data.frame(
    term = rownames(ct),
    label = c(`(Intercept)` = "Intercept", value_model_terms,
              log_rice_value = "log RICE value (R \u00d7 I \u00d7 C)")[rownames(ct)],
    estimate = ct[, 1], std_error = ct[, 2], p_value = ct[, 4],
    stringsAsFactors = FALSE, row.names = NULL)
  out$r2 <- s$r.squared
  out$adj_r2 <- s$adj.r.squared
  out$sigma <- s$sigma
  pr <- exp(stats::predict(fit, data, interval = "prediction", level = level))
  out$data$predicted <- unname(pr[, "fit"])
  out$data$low <- unname(pr[, "lwr"])
  out$data$high <- unname(pr[, "upr"])
  out$data$inside <- unname(data$value_mm_usd >= pr[, "lwr"] & data$value_mm_usd <= pr[, "upr"])
  out$message <- if (type == "simple")
    sprintf("Simple model on RICE value: %d valued initiatives (multivariable from %d).", n, min_n)
  else sprintf("Multivariable model on %d valued initiatives.", n)
  out
}

#' Anticipate the monetary value from RICE inputs
#' @param model Output of [fit_value_model()].
#' @param users,impact,confidence,effort RICE inputs (vectors allowed).
#' @param cfg Configuration list.
#' @return Data frame `median`, `low`, `high` (mm USD); `NA` when no model.
#' @export
predict_value <- function(model, users, impact, confidence, effort, cfg) {
  nd <- value_model_features(users, impact, confidence, effort, cfg)
  na <- data.frame(median = rep(NA_real_, nrow(nd)), low = NA_real_, high = NA_real_)
  if (!isTRUE(model$ok) || !nrow(nd)) return(na)
  ok <- stats::complete.cases(nd[, model$terms, drop = FALSE])
  if (!any(ok)) return(na)
  pr <- exp(stats::predict(model$fit, nd[ok, , drop = FALSE], interval = "prediction",
                           level = model$level))
  na$median[ok] <- unname(pr[, "fit"])
  na$low[ok] <- unname(pr[, "lwr"])
  na$high[ok] <- unname(pr[, "upr"])
  na
}

#' Percentile labels of the prediction interval (e.g. P10 / P90)
#' @param level Interval level.
#' @return Character vector of length 2.
#' @export
interval_labels <- function(level) {
  a <- (1 - level) / 2
  paste0("P", round(100 * c(a, 1 - a)))
}

#' ECharts option: RICE value vs monetary value (log scales)
#'
#' Observed initiatives plus the simple regression line and its prediction
#' band, i.e. the bivariate correlation behind the model.
#' @param model Output of [fit_value_model()].
#' @return An ECharts option list.
#' @export
value_correlation_option <- function(model) {
  d <- model$data
  cols <- group_colors()
  pts <- lapply(seq_len(nrow(d)), function(i) list(
    value = unname(c(d$rice_value[i], d$value_mm_usd[i])), id = d$id[i], name = d$name[i],
    itemStyle = list(color = unname(cols[plot_group(d$status[i])]))))
  series <- list(list(name = "Valued initiatives", type = "scatter", symbolSize = 10,
                      data = pts, z = 3))
  ok <- nrow(d) >= 3 && length(unique(d$log_rice_value)) > 1
  if (ok) {
    fit <- stats::lm(log_value ~ log_rice_value, data = d)
    xs <- exp(seq(min(d$log_rice_value), max(d$log_rice_value), length.out = 30))
    nd <- data.frame(log_rice_value = log(xs))
    pr <- exp(stats::predict(fit, nd, interval = "prediction", level = model$level))
    line <- function(y) lapply(seq_along(xs), function(i) unname(c(xs[i], y[i])))
    lab <- interval_labels(model$level)
    series <- c(series, list(
      list(name = "Fit", type = "line", showSymbol = FALSE, data = line(pr[, "fit"]),
           lineStyle = list(color = "#1d2733", width = 2)),
      list(name = paste(lab[1], "\u2013", lab[2]), type = "line", showSymbol = FALSE,
           data = line(pr[, "lwr"]), lineStyle = list(color = "#C0392B", type = "dashed", width = 1)),
      list(name = paste(lab[1], "\u2013", lab[2]), type = "line", showSymbol = FALSE,
           data = line(pr[, "upr"]), lineStyle = list(color = "#C0392B", type = "dashed", width = 1))))
  }
  list(
    textStyle = list(fontSize = 10),
    color = c("#6E8BA8", "#1d2733", "#C0392B"),
    grid = list(left = 55, right = 20, top = 30, bottom = 45),
    legend = list(top = 0, textStyle = list(fontSize = 10)),
    tooltip = list(trigger = "item", formatter = js(
      "function (p) { if (!p.data || !p.data.id) return p.seriesName;",
      " return '<b>' + p.data.id + ' \u00b7 ' + p.data.name + '</b><br/>RICE value: ' +",
      " p.value[0].toFixed(2) + '<br/>Value: ' + p.value[1].toFixed(2) + ' mm USD'; }")),
    xAxis = list(type = "log", name = "RICE value (log10 users \u00d7 impact \u00d7 confidence)",
                 nameLocation = "middle", nameGap = 26, splitLine = list(show = FALSE)),
    yAxis = list(type = "log", name = "PRMT value (mm USD)",
                 splitLine = list(lineStyle = list(color = "#e3e6ea"))),
    series = series
  )
}

#' ECharts option: predicted vs actual (multivariable model)
#' @param model Output of [fit_value_model()].
#' @return An ECharts option list.
#' @export
value_fit_option <- function(model) {
  d <- model$data
  if (!isTRUE(model$ok)) d <- d[0, , drop = FALSE]
  rng <- unname(range(c(d$predicted, d$value_mm_usd, 0.01, 1), na.rm = TRUE))
  list(
    textStyle = list(fontSize = 10),
    grid = list(left = 55, right = 20, top = 30, bottom = 45),
    legend = list(top = 0, textStyle = list(fontSize = 10)),
    tooltip = list(trigger = "item", formatter = js(
      "function (p) { if (!p.data || !p.data.id) return p.seriesName;",
      " return '<b>' + p.data.id + '</b><br/>Predicted: ' + p.value[0].toFixed(2) +",
      " ' mm USD<br/>Actual: ' + p.value[1].toFixed(2) + ' mm USD'; }")),
    xAxis = list(type = "log", name = "Predicted (mm USD)", nameLocation = "middle", nameGap = 26,
                 splitLine = list(show = FALSE)),
    yAxis = list(type = "log", name = "PRMT value (mm USD)",
                 splitLine = list(lineStyle = list(color = "#e3e6ea"))),
    series = list(
      list(name = "Initiatives", type = "scatter", symbolSize = 9,
           itemStyle = list(color = "#1667D9"),
           data = lapply(seq_len(nrow(d)), function(i) list(
             value = unname(c(d$predicted[i], d$value_mm_usd[i])), id = d$id[i],
             itemStyle = list(color = if (isTRUE(d$inside[i])) "#1667D9" else "#C0392B")))),
      list(name = "1:1", type = "line", showSymbol = FALSE,
           lineStyle = list(color = "#9AA4AE", type = "dashed"),
           data = list(c(rng[1], rng[1]), c(rng[2], rng[2]))))
  )
}

#' Value-model estimates for every initiative of the portfolio
#' @param pf Output of [compute_portfolio()].
#' @param model Output of [fit_value_model()].
#' @param cfg Configuration list.
#' @return Data frame aligned with `pf`: `median`, `low`, `high` (mm USD).
#' @export
portfolio_estimates <- function(pf, model, cfg) {
  predict_value(model, pf$users, pf$impact, pf$confidence, pf$effort, cfg)
}
