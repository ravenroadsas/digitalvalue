#' Methodology tab UI (English / Spanish)
#' @param id Module id.
#' @export
mod_methodology_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_columns(col_widths = c(3, 9),
    htmltools::div(class = "dv-doc-side",
      box(shiny::textOutput(ns("toc_title"), inline = TRUE),
        shiny::radioButtons(ns("lang"), NULL, choices = methodology_languages, selected = "en",
                            inline = TRUE),
        shiny::uiOutput(ns("toc")))),
    box(shiny::textOutput(ns("doc_title"), inline = TRUE),
      htmltools::div(class = "dv-doc", shiny::uiOutput(ns("doc"))))
  )
}
