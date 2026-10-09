#' Execution and audit UI
#' @param id Module id.
#' @export
mod_audit_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_columns(col_widths = c(4, 8),
    htmltools::tagList(
      box("Execution portfolio", DT::DTOutput(ns("queue"))),
      box("Actual metrics (materialised value)",
        shiny::uiOutput(ns("gate")),
        mod_prmt_editor_ui(ns("editor")))
    ),
    htmltools::tagList(
      shiny::uiOutput(ns("tiles")),
      bslib::layout_columns(col_widths = c(6, 6),
        box("Planned vs actual by metric", echarts4r::echarts4rOutput(ns("pva"), height = "260px")),
        box("Planned vs actual", DT::DTOutput(ns("pva_table")))),
      box("Actual calculation lines", mod_prmt_lines_ui(ns("editor"))),
      box("Complete audit",
        shiny::textAreaInput(ns("comment"), "Audit conclusion / lessons learned", rows = 2,
                             resize = "vertical"),
        shiny::actionButton(ns("complete"), "Complete audit", class = "btn-success btn-sm"),
        DT::DTOutput(ns("audits")))
    )
  )
}
