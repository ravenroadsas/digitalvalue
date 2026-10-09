#' Application UI
#' @param request Shiny request.
#' @return UI definition.
#' @export
app_ui <- function(request) {
  theme <- bslib::bs_theme(
    version = 5, primary = "#1667d9", secondary = "#2b3a4a",
    "font-size-base" = "0.78rem", "border-radius" = "2px",
    "card-spacer-y" = "0.4rem", "card-spacer-x" = "0.5rem"
  )
  bslib::page_navbar(
    id = "nav",
    title = htmltools::span("Digital Value"),
    window_title = "Digital Value \u00b7 Initiative Assessment",
    theme = theme,
    bg = "#2b3a4a",
    inverse = TRUE,
    fillable = FALSE,
    header = htmltools::tagList(
      htmltools::tags$link(rel = "stylesheet", href = "dv-www/styles.css"),
      htmltools::tags$script(src = "dv-www/process_logger.js")
    ),
    sidebar = bslib::sidebar(width = 270, open = "desktop", mod_sidebar_ui("sidebar")),
    bslib::nav_panel("Portfolio", value = "overview", mod_overview_ui("overview")),
    bslib::nav_panel("1 Register", value = "register", mod_register_ui("register")),
    bslib::nav_panel("2 Phase I \u00b7 RICE", value = "phase1", mod_phase1_ui("phase1")),
    bslib::nav_panel("3 Phase II \u00b7 PRMT", value = "phase2", mod_phase2_ui("phase2")),
    bslib::nav_panel("4 Phase III \u00b7 Planning", value = "phase3", mod_phase3_ui("phase3")),
    bslib::nav_panel("5 Execution & audit", value = "audit", mod_audit_ui("audit")),
    bslib::nav_spacer(),
    bslib::nav_menu("Admin", align = "right",
      bslib::nav_panel("Process log", value = "process", mod_process_ui("process")),
      bslib::nav_panel("Parameters", value = "parameters", mod_parameters_ui("parameters"))
    )
  )
}
