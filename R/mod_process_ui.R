#' Process log (process mining) UI
#' @param id Module id.
#' @export
mod_process_ui <- function(id) {
  ns <- shiny::NS(id)
  htmltools::tagList(
    callout(type = "info", title = "Process mining",
      "Raw input and navigation events are buffered in the browser and mapped to activities ",
      "and process phases (event_map). Consecutive events are grouped into activity instances. ",
      "Export the combined event log (case = initiative) for bupaR, pm4py or Celonis."),
    shiny::uiOutput(ns("kpis")),
    bslib::layout_columns(col_widths = c(5, 7),
      box("Time per process phase", echarts4r::echarts4rOutput(ns("phases"), height = "300px")),
      box("Business log \u00b7 status transitions", DT::DTOutput(ns("business")))),
    box("Activity log", right = shiny::downloadButton(ns("download"), "Export event log (CSV)",
                                                       class = "btn-sm btn-outline-primary"),
        DT::DTOutput(ns("activity")))
  )
}
