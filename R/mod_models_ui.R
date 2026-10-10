#' Value models (admin) UI: calibrate, compare and publish
#' @param id Module id.
#' @export
mod_models_ui <- function(id) {
  ns <- shiny::NS(id)
  htmltools::tagList(
    callout(type = "info", title = "Calibrated value models",
      "Two log-linear regressions chain the lifecycle: RICE inputs \u2192 ex-ante value (expert review, ",
      "else 4MC estimate), and ex-ante value \u2192 audited value. A candidate is fitted on the current data; ",
      "it is used in the app only once a superuser publishes it. Every published version is kept and can ",
      "be re-activated."),
    do.call(bslib::navset_underline, c(list(id = ns("kind")), lapply(names(value_model_kinds), function(k)
      bslib::nav_panel(value_model_kinds[[k]], value = k, model_panel_ui(ns, k)))))
  )
}

model_panel_ui <- function(ns, k) {
  id <- function(x) ns(paste0(k, "_", x))
  htmltools::tagList(
    shiny::uiOutput(id("kpis")),
    bslib::layout_columns(col_widths = c(6, 6),
      box("Candidate \u00b7 observed relationship", echarts4r::echarts4rOutput(id("corr"), height = "300px")),
      box("Candidate \u00b7 predicted vs observed", echarts4r::echarts4rOutput(id("fit"), height = "300px"))),
    bslib::layout_columns(col_widths = c(6, 6),
      box("Coefficients \u00b7 candidate vs published", DT::DTOutput(id("coef")),
          htmltools::div(class = "dv-muted", style = "margin-top:6px",
            "Elasticities on the log scale. p-values above 0.1: effect not yet distinguishable from noise; ",
            "the prediction range already accounts for this uncertainty.")),
      box("Publish and versions",
        shiny::textInput(id("comment"), "Calibration comment", width = "100%",
                         placeholder = "e.g. quarterly recalibration after Q3 audits"),
        shiny::actionButton(id("publish"), "Publish candidate", class = "btn-primary btn-sm"),
        htmltools::div(class = "dv-section", "Published versions"),
        DT::DTOutput(id("versions")),
        shiny::actionButton(id("activate"), "Re-activate selected version",
                            class = "btn-outline-secondary btn-sm")))
  )
}
