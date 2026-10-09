#' Value model (RICE to PRMT regression) UI
#' @param id Module id.
#' @export
mod_value_model_ui <- function(id) {
  ns <- shiny::NS(id)
  htmltools::tagList(
    callout(type = "info", title = "Anticipating value from Phase I",
      "Log-linear regression of the monetary value (PRMT total, or the planning-validated value) on ",
      "the RICE inputs of every valued initiative: log(value) = b0 + b1 log10(users) + b2 log(impact) + ",
      "b3 log(confidence) + b4 log(effort). The prediction interval gives the anticipated range shown in ",
      "Phase I. The model refits as initiatives are valued; below the minimum sample a simple model on ",
      "RICE value (reach \u00d7 impact \u00d7 confidence) is used."),
    shiny::uiOutput(ns("kpis")),
    bslib::layout_columns(col_widths = c(6, 6),
      box("Correlation \u00b7 RICE value vs PRMT value",
          echarts4r::echarts4rOutput(ns("correlation"), height = "330px")),
      box("Predicted vs actual (red = outside the prediction interval)",
          echarts4r::echarts4rOutput(ns("fit"), height = "330px"))),
    bslib::layout_columns(col_widths = c(5, 7),
      box("Coefficients", DT::DTOutput(ns("coefficients")), shiny::uiOutput(ns("reading"))),
      box("Training data and residuals", DT::DTOutput(ns("data"))))
  )
}
