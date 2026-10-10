#' Initiative tab UI: select or register an initiative and follow its lifecycle
#'
#' Appraisal (pre-execution): registration & RICE, 4MC valuation, expert
#' review, decision. Realisation (post-execution): execution and value audit.
#' @param id Module id.
#' @export
mod_initiative_ui <- function(id) {
  ns <- shiny::NS(id)
  htmltools::tagList(
    htmltools::div(class = "dv-toolbar",
      htmltools::div(class = "dv-toolbar-select",
        shiny::selectizeInput(ns("select"), NULL, choices = NULL, width = "100%",
                              options = list(placeholder = "Select an initiative\u2026"))),
      shiny::actionButton(ns("new"), "New initiative", class = "btn-primary btn-sm"),
      shiny::uiOutput(ns("headline"), class = "dv-headline")),
    shiny::uiOutput(ns("stepper")),
    shiny::uiOutput(ns("workflow")),
    bslib::navset_underline(id = ns("stage"),
      bslib::nav_panel("Appraisal \u00b7 pre-execution", value = "appraisal",
        mod_register_ui(ns("register")),
        mod_valuation_ui(ns("valuation")),
        mod_review_ui(ns("review"))),
      bslib::nav_panel("Realisation \u00b7 post-execution", value = "realisation",
        mod_audit_ui(ns("audit")))
    )
  )
}
