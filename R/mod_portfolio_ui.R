#' Portfolio tab UI
#' @param id Module id.
#' @export
mod_portfolio_ui <- function(id) {
  ns <- shiny::NS(id)
  htmltools::tagList(
    shiny::uiOutput(ns("kpis")),
    shiny::uiOutput(ns("mine")),
    shiny::uiOutput(ns("gaps")),
    htmltools::div(class = "dv-toolbar",
      htmltools::div(class = "dv-toolbar-select",
        shiny::selectizeInput(ns("highlight"), NULL, choices = NULL, width = "100%",
                              options = list(placeholder = "Highlight an initiative in the charts\u2026"))),
      shiny::actionButton(ns("clear"), "Clear", class = "btn-outline-secondary btn-sm"),
      shiny::actionButton(ns("open"), "Open initiative", class = "btn-primary btn-sm"),
      shiny::uiOutput(ns("highlight_info"), class = "dv-headline")),
    bslib::layout_columns(col_widths = c(7, 5),
      box("Prioritisation \u00b7 value vs effort",
          right = shiny::radioButtons(ns("y_axis"), NULL, inline = TRUE,
                                      choices = c("R \u00d7 I \u00d7 C" = "rice_value", "Ex-ante value" = "value",
                                                  "Value or estimate" = "estimate")),
          echarts4r::echarts4rOutput(ns("scatter"), height = "360px"),
          htmltools::div(class = "dv-muted",
            "Bubble size = reach. Darkest diamond = delivered, dark circle = evaluated, square = audited, light = recorded. ",
            "Amber ring = highlighted initiative. Hollow dashed markers = value anticipated by the published model. ",
            "Dashed lines = gates; the stepped line is the RICE-score gate (above it, R \u00d7 I \u00d7 C / effort \u2265 threshold).")),
      box("Portfolio by status", echarts4r::echarts4rOutput(ns("status"), height = "390px"))),
    bslib::layout_columns(col_widths = c(5, 7),
      box("Alerts \u00b7 pending actions",
          htmltools::div(class = "dv-muted", "Click a row to open the initiative on the pending step."),
          DT::DTOutput(ns("alerts"))),
      box("Portfolio register",
          htmltools::div(class = "dv-muted",
            "Click a row to highlight the initiative in the charts. Amber cells = evaluation required but missing."),
          DT::DTOutput(ns("table"))))
  )
}
