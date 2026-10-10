#' Registration & RICE UI (step 1, recorded at once)
#' @param id Module id.
#' @export
mod_register_ui <- function(id) {
  ns <- shiny::NS(id)
  box(shiny::textOutput(ns("title"), inline = TRUE), class = "dv-step-card",
    right = shiny::uiOutput(ns("lock"), inline = TRUE),
    bslib::layout_columns(col_widths = c(6, 6),
      htmltools::div(
        htmltools::div(class = "dv-section", "Initiative"),
        shiny::textInput(ns("name"), "Name", width = "100%"),
        shiny::textAreaInput(ns("description"), "Brief description", rows = 3, width = "100%",
                             resize = "vertical"),
        bslib::layout_columns(col_widths = c(6, 6),
          shiny::textInput(ns("owner"), "Owner"),
          shiny::textInput(ns("business_unit"), "Business unit")),
        bslib::layout_columns(col_widths = c(4, 4, 4),
          shiny::numericInput(ns("cost_mm_usd"), "Cost (mm USD)", value = NA, min = 0, step = 0.05),
          shiny::dateInput(ns("start_date"), "Start"),
          shiny::dateInput(ns("end_date"), "End", value = Sys.Date() + 180))),
      htmltools::div(
        htmltools::div(class = "dv-section", "RICE"),
        bslib::layout_columns(col_widths = c(6, 6),
          shiny::numericInput(ns("users"), "Reach \u00b7 users impacted", value = 100, min = 1),
          shiny::selectInput(ns("impact"), "Impact", choices = NULL, selectize = FALSE)),
        bslib::layout_columns(col_widths = c(6, 6),
          shiny::selectInput(ns("confidence"), "Confidence", choices = NULL, selectize = FALSE),
          shiny::selectInput(ns("effort"), "Effort", choices = NULL, selectize = FALSE)),
        shiny::textAreaInput(ns("rationale"), "Rationale / evidence", rows = 2, width = "100%",
                             resize = "vertical"),
        shiny::uiOutput(ns("preview")))
    ),
    htmltools::div(class = "dv-actions",
      shiny::actionButton(ns("save"), "Register initiative", class = "btn-primary btn-sm"),
      shiny::actionButton(ns("update"), "Update registration", class = "btn-outline-secondary btn-sm"))
  )
}
