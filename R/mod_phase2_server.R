#' Phase II (PRMT monetary valuation) server
#' @param id Module id.
#' @param state Shared application state.
#' @export
mod_phase2_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    cfg <- state$cfg
    editable <- shiny::reactive({
      r <- state$row()
      !is.null(r) && r$status %in% status_assessment
    })
    lines <- mod_prmt_editor_server("editor", state, "plan", enabled = editable)

    output$gate <- shiny::renderUI({
      r <- state$row()
      if (is.null(r)) return(NULL)
      gr <- gate_reasons(r, cfg)
      htmltools::tagList(
        if (!r$has_rice) callout(type = "warning", title = "Phase I pending",
                                 "Score the initiative with RICE first.")
        else if (r$req_phase2) callout(type = "warning", title = paste(r$id, "\u00b7 Phase II required"),
                                       htmltools::tags$ul(lapply(gr$phase2, htmltools::tags$li)))
        else callout(type = "info", title = paste(r$id, "\u00b7 Phase II optional"),
                     "Below Phase II thresholds; a valuation can still be added."),
        if (r$req_phase3) callout(type = "danger", title = "Phase III planning review required",
                                  htmltools::tags$ul(lapply(gr$phase3, htmltools::tags$li))),
        if (!editable()) callout(type = "info", "Valuation is locked after the prioritisation decision.")
      )
    })

    summary <- shiny::reactive(prmt_summary(lines()))

    output$tiles <- shiny::renderUI({
      s <- summary()
      tile <- function(k, digits, tone) {
        i <- s$metric == k
        kpi_tile(paste(k, "\u00b7", s$label[i]), fmt_num(s$value[i], digits, paste0(" ", s$unit[i])),
                 sprintf("%s mm USD \u00b7 %d line(s)", fmt_num(s$value_mm_usd[i], 2), s$n_lines[i]), tone)
      }
      htmltools::div(class = "dv-kpis",
        tile("P", 0, "good"), tile("R", 3, "warn"), tile("M", 2, "info"), tile("T", 1, "neutral"),
        kpi_tile("Total value", fmt_num(sum(s$value_mm_usd), 2, " mm USD"),
                 "P, M, T annual \u00b7 R one-off", "good"))
    })

    output$pie <- echarts4r::renderEcharts4r(echart_from_option(prmt_breakdown_option(summary())))

    output$params <- shiny::renderUI({
      p <- cfg$params
      rv <- reserve_values(cfg)
      htmltools::div(class = "dv-muted",
        htmltools::tags$ul(
          htmltools::tags$li(sprintf("P: BOPD \u00d7 %s days \u00d7 netback %s USD/bbl", p$days_per_year, p$netback_usd_bbl)),
          htmltools::tags$li(paste0("R: MMbbl \u00d7 value per bbl (",
                                    paste(names(rv), rv, sep = " ", collapse = ", "), " USD/bbl)")),
          htmltools::tags$li("M: mm USD per year, as entered"),
          htmltools::tags$li(sprintf("T: khours \u00d7 1000 \u00d7 (%s USD/yr / %s h) \u00d7 productivity %s = %s USD/h",
                                     format(p$avg_salary_usd_year, big.mark = ","), p$work_hours_year,
                                     p$time_productivity_coef, fmt_num(time_value_usd_hour(cfg), 0)))))
    })
  })
}
