#' Expert review UI (step 3, manual evaluation with the same 4MC metrics)
#' @param id Module id.
#' @export
mod_review_ui <- function(id) {
  ns <- shiny::NS(id)
  box("3 \u00b7 Expert review", class = "dv-step-card", right = shiny::uiOutput(ns("tag"), inline = TRUE),
    shiny::uiOutput(ns("gate")),
    bslib::layout_columns(col_widths = c(5, 7),
      htmltools::div(id = ns("form"),
        htmltools::div(class = "dv-section", "Evaluation"),
        shiny::radioButtons(ns("decision"), "Decision", choices = review_decisions, inline = TRUE),
        m4_inputs(ns),
        shiny::textAreaInput(ns("comment"), "Evaluation notes (NPV, risks, conditions)", rows = 3,
                             width = "100%", resize = "vertical"),
        shiny::uiOutput(ns("preview")),
        shiny::actionButton(ns("save"), "Record expert review", class = "btn-primary btn-sm")),
      htmltools::div(
        htmltools::div(class = "dv-section", "4MC estimate vs expert review"),
        DT::DTOutput(ns("compare")),
        htmltools::div(class = "dv-section", "Review history"),
        DT::DTOutput(ns("history"))))
  )
}

#' Inputs for a set of 4MC figures (shared by expert review and audit)
#' @param ns Namespace function.
#' @return UI tags.
#' @export
m4_inputs <- function(ns) {
  htmltools::tagList(
    bslib::layout_columns(col_widths = c(6, 6),
      shiny::numericInput(ns("P"), "P \u00b7 Production (BOPD)", 0, min = 0),
      shiny::numericInput(ns("M"), "M \u00b7 Monetary (mm USD/yr)", 0, min = 0)),
    bslib::layout_columns(col_widths = c(6, 6),
      shiny::numericInput(ns("T"), "T \u00b7 Time saved (khours/yr)", 0, min = 0),
      shiny::numericInput(ns("R"), "R \u00b7 Reserves (MMbbl)", 0, min = 0)),
    bslib::layout_columns(col_widths = c(6, 6),
      shiny::selectInput(ns("category"), "Reserve category", choices = c("1P", "2P", "3P", "contingent"),
                         selected = "2P", selectize = FALSE),
      shiny::numericInput(ns("C"), "C \u00b7 Cost (mm USD)", 0, min = 0))
  )
}

#' Fill 4MC inputs from a list of figures
#' @param session Module session.
#' @param v Named list `P`, `R`, `M`, `T`, `C`, `category`.
#' @export
update_m4_inputs <- function(session, v) {
  r <- function(x) {
    x <- suppressWarnings(as.numeric(x %||% 0))
    if (!length(x) || is.na(x)) 0 else round(x, 4)
  }
  for (k in c("P", "R", "M", "T", "C")) shiny::updateNumericInput(session, k, value = r(v[[k]]))
  if (!is.null(v$category) && !is.na(v$category))
    shiny::updateSelectInput(session, "category", selected = v$category)
}

#' 4MC figures implied by the calculation lines of an initiative
#' @param lines 4MC calculation lines.
#' @return Named list `P`, `R`, `M`, `T`, `C`, `category`.
#' @export
m4_from_lines <- function(lines) {
  s <- m4_summary(lines)
  v <- stats::setNames(as.list(s$value), s$metric)
  cats <- vapply(lines$params_json[lines$metric == "R"], function(j)
    jsonlite::fromJSON(j)$category %||% NA_character_, "")
  v$category <- if (length(cats) && !is.na(cats[1])) unname(cats[1]) else "2P"
  v
}
