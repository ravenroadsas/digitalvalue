#' Portfolio overview UI
#' @param id Module id.
#' @export
mod_overview_ui <- function(id) {
  ns <- shiny::NS(id)
  htmltools::tagList(
    shiny::uiOutput(ns("kpis")),
    bslib::layout_columns(col_widths = c(7, 5),
      box("Prioritisation \u00b7 value vs effort",
          right = shiny::radioButtons(ns("y_axis"), NULL, inline = TRUE,
                                      choices = c("RICE score" = "score", "Value mm USD" = "value")),
          echarts4r::echarts4rOutput(ns("scatter"), height = "360px"),
          htmltools::div(class = "dv-muted",
            "Bubble size = reach (log10 users). Green = prioritized, blue = in execution, amber = ready for decision. Dashed lines = evaluation gates.")),
      box("Portfolio by status", echarts4r::echarts4rOutput(ns("status"), height = "380px"))
    ),
    bslib::layout_columns(col_widths = c(5, 7),
      box("Alerts \u00b7 initiatives passing evaluation criteria",
          htmltools::div(class = "dv-muted", "Click a row to open the pending step."),
          DT::DTOutput(ns("alerts"))),
      box("Portfolio register", DT::DTOutput(ns("table")))
    )
  )
}
