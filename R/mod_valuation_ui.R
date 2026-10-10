#' 4M valuation UI (step 2)
#' @param id Module id.
#' @export
mod_valuation_ui <- function(id) {
  ns <- shiny::NS(id)
  box("2 \u00b7 4M valuation", class = "dv-step-card", right = shiny::uiOutput(ns("tag"), inline = TRUE),
    shiny::uiOutput(ns("gate")),
    bslib::layout_columns(col_widths = c(4, 8),
      htmltools::div(id = ns("calculator"),
        htmltools::div(class = "dv-section", "Calculator"),
        shiny::radioButtons(ns("metric"), "Metric", inline = TRUE,
          choices = c("P \u00b7 Production" = "P", "R \u00b7 Reserves" = "R",
                      "M \u00b7 Monetary" = "M", "T \u00b7 Time" = "T", "C \u00b7 Cost" = "C")),
        shiny::selectInput(ns("method"), "Calculation method", choices = NULL, selectize = FALSE,
                           width = "100%"),
        shiny::uiOutput(ns("params")),
        shiny::uiOutput(ns("calc_preview")),
        shiny::textAreaInput(ns("comment"), "Comment / assumptions", rows = 2, width = "100%",
                             resize = "vertical",
                             placeholder = "e.g. 10 workovers producing each 30 BOPD, success rate from 60% to 70%"),
        shiny::actionButton(ns("add"), "Add calculation", class = "btn-primary btn-sm")),
      htmltools::div(
        shiny::uiOutput(ns("tiles")),
        htmltools::div(class = "dv-section", "Calculation lines \u00b7 formula, parameters and comments"),
        DT::DTOutput(ns("lines")),
        shiny::actionButton(ns("delete"), "Delete selected line", class = "btn-outline-secondary btn-sm"),
        htmltools::div(class = "dv-muted", style = "margin-top:6px", shiny::uiOutput(ns("params_note"))))
    )
  )
}
