#' Parameters server
#' @param id Module id.
#' @param state Shared application state.
#' @export
mod_parameters_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    cfg <- state$cfg
    output$source <- shiny::renderUI(callout(type = "info", title = "Configuration source",
      sprintf("Loaded from %s. Edit the pins (dv_parameters, dv_rice_scales, dv_event_map, dv_roles) on Posit Connect ",
              toupper(cfg$source)),
      "or the CSV files in inst/config; changes apply when the app process restarts."))
    output$parameters <- DT::renderDT(dt_compact(cfg$parameters, page_length = 20, selection = "none"))
    output$scales <- DT::renderDT(dt_compact(cfg$scales, page_length = 20, selection = "none"))
    output$event_map <- DT::renderDT(dt_compact(cfg$event_map, page_length = 12, selection = "none"))
    output$permissions <- DT::renderDT({
      m <- permission_matrix()
      yn <- function(x) ifelse(x, "\u2713", "")
      dt_compact(data.frame(Action = m$label, Everyone = yn(m$everyone), Owner = yn(m$owner),
                            Validator = yn(m$validator), Superuser = yn(m$superuser)),
                 dom = "t", selection = "none", ordering = FALSE)
    })
    output$roles <- shiny::renderUI({
      su <- superusers(cfg$roles)
      htmltools::div(class = "dv-muted", style = "margin-top:6px",
        sprintf("Superusers (pin dv_roles / roles.csv, plus DV_SUPERUSERS / DV_SUPERUSER_GROUP): users %s; groups %s.",
                if (length(su$users)) paste(su$users, collapse = ", ") else "\u2013",
                if (length(su$groups)) paste(su$groups, collapse = ", ") else "\u2013"),
        htmltools::br(),
        sprintf("You are %s (%s); your Connect groups: %s.", state$user$user, state$user$role,
                if (length(state$user$groups)) paste(state$user$groups, collapse = ", ") else "none"))
    })
  })
}
