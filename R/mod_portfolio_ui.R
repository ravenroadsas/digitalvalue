#' Portfolio tab UI
#' @param id Module id.
#' @export
mod_portfolio_ui <- function(id) {
  ns <- shiny::NS(id)
  htmltools::tagList(
    shiny::uiOutput(ns("kpis")),
    shiny::uiOutput(ns("gaps")),
    bslib::layout_columns(col_widths = c(7, 5),
      box("Prioritisation \u00b7 value vs effort",
          right = shiny::radioButtons(ns("y_axis"), NULL, inline = TRUE,
                                      choices = c("RICE score" = "score", "Ex-ante value" = "value",
                                                  "Value or estimate" = "estimate")),
          echarts4r::echarts4rOutput(ns("scatter"), height = "360px"),
          htmltools::div(class = "dv-muted",
            "Bubble size = reach. Darkest diamond = in execution, dark circle = prioritized, triangle = ready for decision. ",
            "Hollow dashed markers = value anticipated by the published model. Dashed lines = gates.")),
      box("Portfolio by status", echarts4r::echarts4rOutput(ns("status"), height = "390px"))),
    bslib::layout_columns(col_widths = c(5, 7),
      box("Alerts \u00b7 initiatives passing evaluation criteria",
          htmltools::div(class = "dv-muted", "Click a row to open the initiative on the pending step."),
          DT::DTOutput(ns("alerts"))),
      box("Portfolio register",
          htmltools::div(class = "dv-muted",
            "Click a row to open the initiative. Amber cells = evaluation required but missing."),
          DT::DTOutput(ns("table"))))
  )
}
