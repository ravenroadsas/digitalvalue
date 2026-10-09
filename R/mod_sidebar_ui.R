#' Sidebar UI: initiative selector, process stepper and workflow actions
#' @param id Module id.
#' @export
mod_sidebar_ui <- function(id) {
  ns <- shiny::NS(id)
  htmltools::tagList(
    shiny::selectizeInput(ns("initiative"), "Active initiative", choices = NULL,
                          options = list(placeholder = "Select an initiative")),
    shiny::uiOutput(ns("summary")),
    htmltools::div(class = "dv-box-header", style = "margin-top:6px", "Process stage"),
    shiny::uiOutput(ns("stepper")),
    htmltools::div(class = "dv-box-header", style = "margin-top:6px", "Workflow decision"),
    shiny::uiOutput(ns("workflow")),
    htmltools::div(class = "dv-footer", shiny::textOutput(ns("user")))
  )
}
