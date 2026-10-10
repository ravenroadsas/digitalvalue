#' Value audit UI (realisation, post-execution): actual 4MC plus adoption
#' @param id Module id.
#' @export
mod_audit_ui <- function(id) {
  ns <- shiny::NS(id)
  htmltools::tagList(
    shiny::uiOutput(ns("status")),
    shiny::uiOutput(ns("tiles")),
    bslib::layout_columns(col_widths = c(5, 7),
      box("Value audit \u00b7 actual 4MC, cost and adoption", class = "dv-step-card",
        collapsible = TRUE, id = ns("card_form"),
        htmltools::div(id = ns("form"),
          m4_inputs(ns),
          shiny::sliderInput(ns("adoption"), "Adoption (% of intended users actively using it)",
                             min = 0, max = 100, value = 80, step = 5, width = "100%"),
          shiny::textAreaInput(ns("comment"), "Audit conclusion / lessons learned", rows = 3,
                               width = "100%", resize = "vertical"),
          shiny::uiOutput(ns("preview")),
          shiny::actionButton(ns("save"), "Record audit", class = "btn-primary btn-sm")),
        shiny::uiOutput(ns("locked"))),
      box("4MC across the lifecycle", collapsible = TRUE, id = ns("card_compare"),
        echarts4r::echarts4rOutput(ns("chart"), height = "250px"),
        DT::DTOutput(ns("table")),
        htmltools::div(class = "dv-section", "Audit history"),
        DT::DTOutput(ns("history"))))
  )
}
