#' Value models (admin) server
#' @param id Module id.
#' @param state Shared application state.
#' @export
mod_models_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    cfg <- state$cfg
    lapply(names(value_model_kinds), function(k) {
      nm <- function(x) paste0(k, "_", x)
      data <- shiny::reactive(value_model_data(state$portfolio(), cfg, k))
      candidate <- shiny::reactive(fit_value_model(data(), cfg, k))
      active <- shiny::reactive(state$models()[[k]])
      active_fit <- shiny::reactive(assess_record(active(), data()))
      versions <- shiny::reactive({ state$version(); db_get_value_models(state$con, k) })

      output[[nm("kpis")]] <- shiny::renderUI({
        m <- candidate(); a <- active(); af <- active_fit()
        cov <- if (m$ok) mean(m$data$inside) else NA
        htmltools::div(class = "dv-kpis",
          kpi_tile("Candidate", if (m$ok) m$type else "none", m$message),
          kpi_tile("Observations", m$n, sprintf("multivariable from %d", m$min_n)),
          kpi_tile("Correlation", fmt_num(m$cor_pearson, 2),
                   sprintf("log x vs log y \u00b7 Spearman %s", fmt_num(m$cor_spearman, 2))),
          kpi_tile("R\u00b2 / adj. R\u00b2", if (m$ok) sprintf("%s / %s", fmt_num(m$record$r2, 2),
                                                              fmt_num(m$record$adj_r2, 2)) else "\u2013",
                   "candidate"),
          kpi_tile("Range width", if (m$ok) paste0("\u00d7", fmt_num(range_factor(m$record), 1)) else "\u2013",
                   sprintf("candidate %s", paste(interval_labels(m$level), collapse = "\u2013"))),
          kpi_tile("Coverage", fmt_num(100 * cov, 0, "%"),
                   sprintf("candidate, target %d%%", round(100 * m$level))),
          kpi_tile("Published", if (is.null(a)) "None" else substr(a$created_at, 1, 10),
                   if (is.null(a)) "no active version"
                   else sprintf("n = %d \u00b7 R\u00b2 %s \u00b7 coverage now %s", a$n, fmt_num(a$r2, 2),
                                fmt_num(100 * af$coverage, 0, "%")), "strong"))
      })
      output[[nm("corr")]] <- echarts4r::renderEcharts4r(echart_from_option(value_correlation_option(candidate())))
      output[[nm("fit")]] <- echarts4r::renderEcharts4r({
        m <- candidate()
        echart_from_option(value_fit_option(if (m$ok) m$data else assess_record(NULL, data())$data, k))
      })
      output[[nm("coef")]] <- DT::renderDT({
        m <- candidate()
        cc <- record_coefficients(if (m$ok) m$record else NULL)
        ac <- record_coefficients(active())
        terms <- union(cc$term, ac$term)
        lab <- c(stats::setNames(cc$label, cc$term), stats::setNames(ac$label, ac$term))
        g <- function(d, t, col) { v <- d[[col]][match(t, d$term)]; round(v, 3) }
        dt_compact(data.frame(Term = unname(lab[terms]),
                              `Candidate` = g(cc, terms, "estimate"), `p` = g(cc, terms, "p_value"),
                              `Published` = g(ac, terms, "estimate"), `p ` = g(ac, terms, "p_value"),
                              check.names = FALSE), dom = "t", selection = "none", ordering = FALSE)
      })
      output[[nm("versions")]] <- DT::renderDT({
        v <- versions()
        dt_compact(data.frame(Active = ifelse(v$active == 1, "\u25cf", ""),
                              Published = substr(v$created_at, 1, 16), By = v$created_by,
                              Type = v$type, n = v$n, R2 = round(v$r2, 2),
                              Comment = v$comment, check.names = FALSE), page_length = 5, dom = "tp")
      })

      shiny::observeEvent(input[[nm("publish")]], {
        m <- candidate()
        if (!state$can("calibrate")) return(shiny::showNotification("Superuser required", type = "error"))
        if (!m$ok) return(shiny::showNotification(m$message, type = "warning"))
        cm <- input[[nm("comment")]]
        db_publish_value_model(state$con, m$record, if (nzchar(cm %||% "")) cm else NA, state$user$user)
        shiny::updateTextInput(session, nm("comment"), value = "")
        state$refresh()
        shiny::showNotification(sprintf("%s: new version published", value_model_kinds[[k]]), type = "message")
      })
      shiny::observeEvent(input[[nm("activate")]], {
        i <- input[[nm("versions_rows_selected")]]
        if (is.null(i) || !state$can("calibrate")) return()
        db_activate_value_model(state$con, versions()$model_id[i])
        state$refresh()
        shiny::showNotification("Version re-activated", type = "message")
      })
    })
  })
}
