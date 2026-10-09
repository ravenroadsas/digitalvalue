#' Parameters server
#' @param id Module id.
#' @param state Shared application state.
#' @export
mod_parameters_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    cfg <- state$cfg
    output$source <- shiny::renderUI(callout(type = "info", title = "Configuration source",
      sprintf("Loaded from %s. Edit the pins (dv_parameters, dv_rice_scales, dv_event_map) on Posit Connect ",
              toupper(cfg$source)),
      "or the CSV files in inst/config; changes apply when the app process restarts."))
    output$parameters <- DT::renderDT(dt_compact(cfg$parameters, page_length = 20, selection = "none"))
    output$scales <- DT::renderDT(dt_compact(cfg$scales, page_length = 20, selection = "none"))
    output$event_map <- DT::renderDT(dt_compact(cfg$event_map, page_length = 30, selection = "none"))
  })
}
