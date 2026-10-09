#' Phase I (RICE) UI
#' @param id Module id.
#' @export
mod_phase1_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_columns(col_widths = c(4, 8),
    htmltools::tagList(
      box(shiny::textOutput(ns("title"), inline = TRUE),
        shiny::numericInput(ns("users"), "Reach \u00b7 users impacted", value = 100, min = 1),
        shiny::selectInput(ns("impact"), "Impact (t-shirt)", choices = NULL),
        shiny::radioButtons(ns("confidence"), "Confidence", choices = "Moonshot", inline = TRUE),
        shiny::selectInput(ns("effort"), "Effort (t-shirt)", choices = NULL),
        shiny::textAreaInput(ns("rationale"), "Rationale / evidence", rows = 2, resize = "vertical"),
        shiny::uiOutput(ns("preview")),
        shiny::actionButton(ns("save"), "Save RICE score", class = "btn-primary btn-sm")
      ),
      box("Weights table", DT::DTOutput(ns("scales")))
    ),
    htmltools::tagList(
      box("Prioritisation \u00b7 value vs effort",
          right = shiny::radioButtons(ns("y_axis"), NULL, inline = TRUE,
                                      choices = c("RICE score" = "score", "Value mm USD" = "value")),
          echarts4r::echarts4rOutput(ns("scatter"), height = "380px")),
      bslib::layout_columns(col_widths = c(7, 5),
        box("RICE ranking", DT::DTOutput(ns("ranking"))),
        box("Gate alerts", shiny::uiOutput(ns("alerts"))))
    )
  )
}
