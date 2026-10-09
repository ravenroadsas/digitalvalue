#' Parameters UI
#' @param id Module id.
#' @export
mod_parameters_ui <- function(id) {
  ns <- shiny::NS(id)
  htmltools::tagList(
    shiny::uiOutput(ns("source")),
    bslib::layout_columns(col_widths = c(6, 6),
      box("Valuation parameters and gates", DT::DTOutput(ns("parameters"))),
      box("RICE weights", DT::DTOutput(ns("scales")))),
    box("Process-mining event map", DT::DTOutput(ns("event_map")))
  )
}
