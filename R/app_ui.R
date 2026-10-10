#' Application UI
#'
#' Two main tabs (Initiative, Portfolio), the Methodology page (English /
#' Spanish) and an Admin menu that is removed for non-superusers.
#' @param request Shiny request.
#' @return UI definition.
#' @export
app_ui <- function(request) {
  theme <- bslib::bs_theme(
    version = 5, bg = "#ffffff", fg = mono$c900, primary = mono$c800, secondary = mono$c500,
    success = mono$c700, info = mono$c600, warning = mono$c500, danger = mono$c900,
    "font-size-base" = "0.8rem", "border-radius" = "2px",
    "card-spacer-y" = "0.5rem", "card-spacer-x" = "0.6rem",
    "navbar-padding-y" = "0.2rem"
  )
  bslib::page_navbar(
    id = "nav",
    title = htmltools::span(class = "dv-brand", "Digital Value"),
    window_title = "Digital Value \u00b7 Initiative assessment",
    theme = theme,
    bg = mono$c900,
    inverse = TRUE,
    fillable = FALSE,
    header = htmltools::tagList(
      shinyjs::useShinyjs(),
      htmltools::tags$link(rel = "stylesheet", href = "dv-www/styles.css"),
      htmltools::tags$script(src = "dv-www/process_logger.js")
    ),
    bslib::nav_panel("Initiative", value = "initiative", mod_initiative_ui("initiative")),
    bslib::nav_panel("Portfolio", value = "portfolio", mod_portfolio_ui("portfolio")),
    bslib::nav_panel("Methodology", value = "methodology", mod_methodology_ui("methodology")),
    bslib::nav_spacer(),
    bslib::nav_menu("Admin", value = "admin", align = "right",
      bslib::nav_panel("Value models", value = "models", mod_models_ui("models")),
      bslib::nav_panel("Process log", value = "process", mod_process_ui("process")),
      bslib::nav_panel("Parameters", value = "parameters", mod_parameters_ui("parameters"))
    ),
    bslib::nav_item(shiny::uiOutput("whoami"))
  )
}
