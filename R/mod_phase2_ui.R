#' Phase II (PRMT monetary valuation) UI
#' @param id Module id.
#' @export
mod_phase2_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_columns(col_widths = c(4, 8),
    htmltools::tagList(
      shiny::uiOutput(ns("gate")),
      box("PRMT calculator", mod_prmt_editor_ui(ns("editor")))
    ),
    htmltools::tagList(
      shiny::uiOutput(ns("tiles")),
      bslib::layout_columns(col_widths = c(8, 4),
        box("Calculation lines \u00b7 formula, parameters and comments", mod_prmt_lines_ui(ns("editor"))),
        box("Value breakdown", echarts4r::echarts4rOutput(ns("pie"), height = "260px"))),
      box("Valuation parameters", shiny::uiOutput(ns("params")))
    )
  )
}
