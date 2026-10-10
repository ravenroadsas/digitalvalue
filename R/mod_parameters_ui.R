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
    bslib::layout_columns(col_widths = c(5, 7),
      box("User levels", DT::DTOutput(ns("permissions")), shiny::uiOutput(ns("roles"))),
      box("Process-mining event map", DT::DTOutput(ns("event_map"))))
  )
}
