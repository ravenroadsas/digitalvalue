#' Value model (RICE to PRMT regression) server
#' @param id Module id.
#' @param state Shared application state.
#' @export
mod_value_model_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    output$kpis <- shiny::renderUI({
      m <- state$value_model()
      lab <- interval_labels(m$level)
      cov <- if (isTRUE(m$ok)) 100 * mean(m$data$inside) else NA
      htmltools::div(class = "dv-kpis",
        kpi_tile("Model", switch(m$type, multivariable = "Multivariable", simple = "Simple", "None"),
                 m$message, if (isTRUE(m$ok)) "good" else "warn"),
        kpi_tile("Valued initiatives", m$n, sprintf("multivariable from %d", m$min_n)),
        kpi_tile("Correlation", fmt_num(m$cor_pearson %||% NA, 2),
                 sprintf("log RICE value vs log value \u00b7 Spearman %s", fmt_num(m$cor_spearman %||% NA, 2)),
                 "info"),
        kpi_tile("R\u00b2 / adj. R\u00b2", sprintf("%s / %s", fmt_num(m$r2 %||% NA, 2), fmt_num(m$adj_r2 %||% NA, 2)),
                 "share of log-value variance explained"),
        kpi_tile("Range width", if (isTRUE(m$ok)) sprintf("\u00d7%s", fmt_num(exp(stats::qnorm(1 - (1 - m$level) / 2) * m$sigma), 1)) else "\u2013",
                 sprintf("typical %s\u2013%s factor around the median", lab[1], lab[2])),
        kpi_tile("Interval coverage", fmt_num(cov, 0, "%"),
                 sprintf("observed values inside %s\u2013%s (target %d%%)", lab[1], lab[2], round(100 * m$level)),
                 if (is.na(cov)) "neutral" else if (abs(cov - 100 * m$level) <= 15) "good" else "warn"))
    })

    output$correlation <- echarts4r::renderEcharts4r(echart_from_option(value_correlation_option(state$value_model())))
    output$fit <- echarts4r::renderEcharts4r(echart_from_option(value_fit_option(state$value_model())))

    output$coefficients <- DT::renderDT({
      m <- state$value_model()
      cf <- if (isTRUE(m$ok)) m$coefficients else data.frame(label = character(0), estimate = numeric(0),
                                                             std_error = numeric(0), p_value = numeric(0))
      dt_compact(data.frame(Term = cf$label, Estimate = round(cf$estimate, 3),
                            `Std. error` = round(cf$std_error, 3), `p-value` = round(cf$p_value, 3),
                            check.names = FALSE), dom = "t", selection = "none", ordering = FALSE)
    })
    output$reading <- shiny::renderUI({
      m <- state$value_model()
      if (!isTRUE(m$ok)) return(callout(type = "warning", m$message))
      htmltools::div(class = "dv-muted", style = "margin-top:6px",
        "Coefficients are elasticities on the log scale: e.g. +1 in log10(users) multiplies the value by ",
        "exp(b1); doubling impact multiplies it by 2^b2. p-values above 0.1 mean the effect is not yet ",
        "distinguishable from noise \u2013 the range shown in Phase I already accounts for this uncertainty.")
    })

    output$data <- DT::renderDT({
      m <- state$value_model()
      d <- m$data
      has <- isTRUE(m$ok)
      dt_compact(data.frame(ID = d$id, Initiative = d$name, Users = d$users, I = d$impact,
                            C = d$confidence, E = d$effort, `RICE value` = round(d$rice_value, 2),
                            `Value mm$` = round(d$value_mm_usd, 2),
                            `Predicted mm$` = if (has) round(d$predicted, 2) else NA,
                            Range = if (has) sprintf("%.2f\u2013%.2f", d$low, d$high) else NA,
                            `Inside` = if (has) ifelse(d$inside, "yes", "no") else NA,
                            check.names = FALSE), page_length = 10, selection = "none")
    })
  })
}
