#' Load application configuration
#'
#' Configuration parameters (valuation parameters, gate thresholds, RICE
#' scales and the process-mining event map) are stored as tables. On Posit
#' Connect they are read from pins (set `DV_PINS_BOARD=connect`); otherwise
#' the CSV files shipped in `inst/config` (or `DV_CONFIG_DIR`) are used.
#'
#' Environment variables are reserved for infrastructure references:
#' `DV_PINS_BOARD`, `DV_PINS_OWNER`, `DV_CONFIG_DIR`.
#'
#' @param source `"auto"`, `"pins"` or `"csv"`.
#' @param dir Directory with the CSV files (csv source only).
#' @return A list with `params` (named list), `parameters` (data frame),
#'   `scales` (data frame), `event_map` (data frame) and `source`.
#' @export
load_config <- function(source = c("auto", "pins", "csv"),
                        dir = Sys.getenv("DV_CONFIG_DIR", config_dir())) {
  source <- match.arg(source)
  if (source == "auto") {
    source <- if (nzchar(Sys.getenv("DV_PINS_BOARD")) &&
                  requireNamespace("pins", quietly = TRUE)) "pins" else "csv"
  }
  tables <- switch(source,
    pins = read_config_pins(),
    csv  = read_config_csv(dir)
  )
  build_config(tables$parameters, tables$scales, tables$event_map, source)
}

#' Directory of the CSV configuration shipped with the package
#' @return A path.
#' @export
config_dir <- function() {
  system.file("config", package = "digitalvalue")
}

#' Names of the configuration tables (and of the pins that store them)
#' @noRd
config_tables <- c(parameters = "dv_parameters",
                   scales = "dv_rice_scales",
                   event_map = "dv_event_map")

read_config_csv <- function(dir) {
  rd <- function(f) utils::read.csv(file.path(dir, f), stringsAsFactors = FALSE,
                                    strip.white = TRUE)
  list(parameters = rd("parameters.csv"),
       scales = rd("rice_scales.csv"),
       event_map = rd("event_map.csv"))
}

pins_board <- function() {
  board <- Sys.getenv("DV_PINS_BOARD", "connect")
  if (board == "connect") pins::board_connect() else pins::board_folder(board)
}

pin_name <- function(name) {
  owner <- Sys.getenv("DV_PINS_OWNER")
  if (nzchar(owner)) paste0(owner, "/", name) else name
}

read_config_pins <- function() {
  board <- pins_board()
  lapply(config_tables, function(nm) as.data.frame(pins::pin_read(board, pin_name(nm))))
}

#' Publish the CSV configuration to a pins board
#'
#' One-off helper to seed (or update) the configuration pins on Posit Connect
#' from the CSV files.
#'
#' @param dir Directory with the CSV files.
#' @param board A pins board; defaults to the board from `DV_PINS_BOARD`.
#' @return Invisibly, the pin names written.
#' @export
publish_config_pins <- function(dir = config_dir(), board = pins_board()) {
  tables <- read_config_csv(dir)
  for (k in names(config_tables)) {
    pins::pin_write(board, tables[[k]], pin_name(config_tables[[k]]), type = "csv")
  }
  invisible(unname(config_tables))
}

#' Assemble and validate a configuration object from its tables
#'
#' @param parameters Data frame with columns `key`, `value`.
#' @param scales Data frame with columns `dimension`, `level`, `ordinal`, `value`.
#' @param event_map Data frame with columns `pattern`, `activity`, `process_phase`.
#' @param source Label of the configuration source.
#' @return A configuration list (see [load_config()]).
#' @export
build_config <- function(parameters, scales, event_map, source = "custom") {
  stopifnot(all(c("key", "value") %in% names(parameters)),
            all(c("dimension", "level", "ordinal", "value") %in% names(scales)),
            all(c("pattern", "activity", "process_phase") %in% names(event_map)))
  params <- lapply(as.character(parameters$value), function(v) {
    n <- suppressWarnings(as.numeric(v))
    if (is.na(n)) v else n
  })
  names(params) <- parameters$key
  required <- c("netback_usd_bbl", "days_per_year", "avg_salary_usd_year",
                "work_hours_year", "time_productivity_coef", "gate2_effort_min",
                "gate2_score_min", "gate3_value_min_mm_usd", "gate3_cost_min_mm_usd")
  missing <- setdiff(required, names(params))
  if (length(missing)) stop("Missing configuration parameters: ", paste(missing, collapse = ", "))
  scales$ordinal <- as.integer(scales$ordinal)
  scales$value <- as.numeric(scales$value)
  scales <- scales[order(scales$dimension, scales$ordinal), , drop = FALSE]
  list(params = params, parameters = parameters, scales = scales,
       event_map = event_map, source = source)
}

#' Reserve categories with their value per barrel
#' @param cfg Configuration list.
#' @return Named numeric vector (USD/bbl), names are categories.
#' @export
reserve_values <- function(cfg) {
  p <- cfg$params
  keys <- grep("^reserve_value_", names(p), value = TRUE)
  v <- vapply(keys, function(k) as.numeric(p[[k]]), numeric(1))
  names(v) <- sub("^reserve_value_", "", keys)
  v
}
