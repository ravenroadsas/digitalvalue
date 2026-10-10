#' Methodology tab server
#' @param id Module id.
#' @param state Shared application state.
#' @export
mod_methodology_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    doc <- shiny::reactive({
      lang <- input$lang %||% "en"
      render_methodology(methodology_markdown(lang, state$cfg))
    })
    output$toc_title <- shiny::renderText(if (identical(input$lang, "es")) "Contenido" else "Contents")
    output$doc_title <- shiny::renderText(
      if (identical(input$lang, "es")) "Metodolog\u00eda de evaluaci\u00f3n del valor"
      else "Value assessment methodology")
    output$toc <- shiny::renderUI({
      t <- doc()$toc
      htmltools::tags$ul(class = "dv-toc", lapply(seq_len(nrow(t)), function(i)
        htmltools::tags$li(class = paste0("dv-toc-l", t$level[i]),
                           htmltools::tags$a(href = paste0("#", t$id[i]), t$title[i]))))
    })
    output$doc <- shiny::renderUI(htmltools::HTML(doc()$html))
  })
}
