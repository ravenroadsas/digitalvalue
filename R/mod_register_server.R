#' Registration server
#' @param id Module id.
#' @param state Shared application state.
#' @export
mod_register_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    fill <- function(r = NULL) {
      g <- function(k) if (is.null(r) || is.na(r[[k]])) "" else r[[k]]
      shiny::updateTextInput(session, "name", value = g("name"))
      shiny::updateTextAreaInput(session, "description", value = g("description"))
      shiny::updateTextInput(session, "owner", value = g("owner"))
      shiny::updateTextInput(session, "business_unit", value = g("business_unit"))
      shiny::updateNumericInput(session, "cost_mm_usd",
                                value = if (is.null(r)) NA else r$cost_mm_usd)
      if (nzchar(g("start_date"))) shiny::updateDateInput(session, "start_date", value = g("start_date"))
      if (nzchar(g("end_date"))) shiny::updateDateInput(session, "end_date", value = g("end_date"))
    }
    shiny::observeEvent(state$selected(), fill(db_get_initiatives(state$con, state$selected())))
    shiny::observeEvent(input$clear, fill(NULL))

    form <- function() {
      d <- function(x) if (length(x) && !is.na(x)) format(x) else NA
      list(name = input$name, description = input$description, owner = input$owner,
           business_unit = input$business_unit, cost_mm_usd = input$cost_mm_usd,
           start_date = d(input$start_date), end_date = d(input$end_date))
    }

    shiny::observeEvent(input$save_new, {
      f <- form()
      id <- tryCatch(
        db_add_initiative(state$con, f$name, f$description, f$owner, f$business_unit,
                          f$cost_mm_usd, f$start_date, f$end_date, user = state$user$user),
        error = function(e) { shiny::showNotification(conditionMessage(e), type = "error"); NULL })
      if (is.null(id)) return()
      state$refresh()
      state$selected(id)
      shiny::showNotification(paste(id, "registered \u2013 continue with Phase I"), type = "message")
      state$goto("phase1")
    })

    shiny::observeEvent(input$update, {
      id <- state$selected()
      if (is.null(id)) return()
      if (!nzchar(trimws(input$name))) {
        shiny::showNotification("Name is required", type = "error"); return()
      }
      db_update_initiative(state$con, id, form())
      state$refresh()
      shiny::showNotification(paste(id, "updated"), type = "message")
    })

    tbl <- shiny::reactive({
      pf <- state$portfolio()
      data.frame(ID = pf$id, Initiative = pf$name, Description = pf$description,
                 Owner = pf$owner, Unit = pf$business_unit, `Cost mm$` = pf$cost_mm_usd,
                 Status = pf$status, `Created by` = pf$created_by,
                 Created = substr(pf$created_at, 1, 10), check.names = FALSE)
    })
    output$table <- DT::renderDT(dt_compact(tbl(), page_length = 15))
    shiny::observeEvent(input$table_rows_selected, {
      state$selected(tbl()$ID[input$table_rows_selected])
    })
  })
}
