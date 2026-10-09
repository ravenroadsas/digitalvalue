#' Phase III (planning review) UI
#' @param id Module id.
#' @export
mod_phase3_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_columns(col_widths = c(4, 4, 4),
    htmltools::tagList(
      box("Planning queue", htmltools::div(class = "dv-muted",
            "Initiatives above the Phase III value or cost thresholds."),
          DT::DTOutput(ns("queue")))
    ),
    htmltools::tagList(
      box(shiny::textOutput(ns("title"), inline = TRUE), shiny::uiOutput(ns("dossier")))
    ),
    htmltools::tagList(
      box("Planning evaluation",
        shiny::uiOutput(ns("access")),
        shiny::radioButtons(ns("decision"), "Decision", choices = review_decisions, inline = TRUE),
        bslib::layout_columns(col_widths = c(6, 6),
          shiny::numericInput(ns("validated_value"), "Validated value (mm USD)", NA, min = 0),
          shiny::numericInput(ns("validated_cost"), "Validated cost (mm USD)", NA, min = 0)),
        shiny::textAreaInput(ns("comment"), "Evaluation notes (NPV, risks, conditions)", rows = 4,
                             resize = "vertical"),
        shiny::actionButton(ns("save"), "Record evaluation", class = "btn-primary btn-sm")),
      box("Review history", DT::DTOutput(ns("history")))
    )
  )
}
