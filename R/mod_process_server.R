#' Process log (process mining) server
#' @param id Module id.
#' @param state Shared application state.
#' @export
mod_process_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    tick <- shiny::reactiveTimer(15000)
    raw <- shiny::reactive({ tick(); state$version(); db_get_events(state$con) })
    activity <- shiny::reactive(build_activity_log(raw(), state$cfg$event_map))
    business <- shiny::reactive(build_business_log(state$history()))

    output$kpis <- shiny::renderUI({
      a <- activity(); r <- raw()
      mapped <- if (nrow(r)) mean(!is.na(map_raw_events(r, state$cfg$event_map)$activity)) * 100 else NA
      htmltools::div(class = "dv-kpis",
        kpi_tile("Raw events", nrow(r), "input / navigation"),
        kpi_tile("Mapped", fmt_num(mapped, 0, "%"), "events mapped to activities", "info"),
        kpi_tile("Activity instances", nrow(a), "grouped"),
        kpi_tile("Cases", length(unique(a$case_id[a$case_id != "(none)"])), "initiatives touched"),
        kpi_tile("Users", length(unique(r$user_name)), "distinct"),
        kpi_tile("Sessions", length(unique(r$session_id)), "distinct"))
    })
    output$phases <- echarts4r::renderEcharts4r(echart_from_option(phase_effort_option(phase_effort(activity()))))
    output$business <- DT::renderDT({
      b <- business()
      dt_compact(b[order(b$timestamp, decreasing = TRUE), ], page_length = 8, selection = "none")
    })
    output$activity <- DT::renderDT({
      a <- activity()
      dt_compact(a[order(a$start, decreasing = TRUE), ], page_length = 10, selection = "none")
    })
    output$download <- shiny::downloadHandler(
      filename = function() paste0("digitalvalue_event_log_", format(Sys.Date()), ".csv"),
      content = function(file) utils::write.csv(export_event_log(activity(), business()), file,
                                                row.names = FALSE)
    )
  })
}
