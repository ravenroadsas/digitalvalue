#' PRMT calculation editor UI (shared by Phase II and the audit)
#'
#' Two UI pieces share the same module id: the calculator inputs and the
#' table of stored calculation lines.
#' @param id Module id.
#' @export
mod_prmt_editor_ui <- function(id) {
  ns <- shiny::NS(id)
  htmltools::tagList(
    shiny::radioButtons(ns("metric"), "Metric", inline = TRUE,
      choices = c("P \u00b7 Production" = "P", "R \u00b7 Reserves" = "R",
                  "M \u00b7 Monetary" = "M", "T \u00b7 Time" = "T")),
    shiny::selectInput(ns("method"), "Calculation method", choices = NULL),
    shiny::uiOutput(ns("params")),
    shiny::uiOutput(ns("preview")),
    shiny::textAreaInput(ns("comment"), "Comment / assumptions", rows = 2, resize = "vertical",
                         placeholder = "e.g. 10 workovers producing each 30 BOPD, success rate from 60% to 70%"),
    shiny::actionButton(ns("add"), "Add calculation", class = "btn-primary btn-sm")
  )
}

#' @rdname mod_prmt_editor_ui
#' @export
mod_prmt_lines_ui <- function(id) {
  ns <- shiny::NS(id)
  htmltools::tagList(
    DT::DTOutput(ns("lines")),
    shiny::actionButton(ns("delete"), "Delete selected line", class = "btn-outline-danger btn-sm")
  )
}
