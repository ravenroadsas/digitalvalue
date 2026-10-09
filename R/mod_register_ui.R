#' Registration UI
#' @param id Module id.
#' @export
mod_register_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_columns(col_widths = c(4, 8),
    box("Initiative record",
      callout(type = "info", title = "Step 1 \u00b7 Register",
              "Describe the initiative briefly. Save as new, or update the active initiative. ",
              "Next: score it in Phase I (RICE)."),
      shiny::textInput(ns("name"), "Name"),
      shiny::textAreaInput(ns("description"), "Brief description", rows = 3, resize = "vertical"),
      bslib::layout_columns(col_widths = c(6, 6),
        shiny::textInput(ns("owner"), "Owner"),
        shiny::textInput(ns("business_unit"), "Business unit")),
      bslib::layout_columns(col_widths = c(4, 4, 4),
        shiny::numericInput(ns("cost_mm_usd"), "Cost (mm USD)", value = NA, min = 0, step = 0.05),
        shiny::dateInput(ns("start_date"), "Start"),
        shiny::dateInput(ns("end_date"), "End", value = Sys.Date() + 180)),
      htmltools::div(class = "dv-actions",
        shiny::actionButton(ns("save_new"), "Save as new", class = "btn-primary btn-sm"),
        shiny::actionButton(ns("update"), "Update active", class = "btn-outline-primary btn-sm"),
        shiny::actionButton(ns("clear"), "Clear form", class = "btn-outline-secondary btn-sm"))
    ),
    box("Registered initiatives", DT::DTOutput(ns("table")))
  )
}
