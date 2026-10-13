#' Load application configuration
#'
#' Configuration parameters (valuation parameters, gate thresholds, RICE
#' scales and the process-mining event map) are stored as tables. On Posit
#' Connect they are read from pins (set `DV_PINS_BOARD=connect`); otherwise
#' the CSV files shipped in `inst/config` (or `DV_CONFIG_DIR`) are used.
#'
#' The optional `roles` table (pin `dv_roles`, file `roles.csv`) lists the
#' superusers; see [read_roles()].
#'
#' Environment variables are reserved for infrastructure references:
#' `DV_PINS_BOARD`, `DV_PINS_OWNER`, `DV_CONFIG_DIR`.
#'
#' @param source `"auto"`, `"pins"` or `"csv"`.
#' @param dir Directory with the CSV files (csv source only).
#' @return A list with `params` (named list), `parameters` (data frame),
#'   `scales` (data frame), `event_map` (data frame), `roles` (data frame)
#'   and `source`.
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
  build_config(tables$parameters, tables$scales, tables$event_map, source, tables$roles)
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
       event_map = rd("event_map.csv"),
       roles = read_roles(file.path(dir, "roles.csv")))
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
  tables <- lapply(config_tables, function(nm) as.data.frame(pins::pin_read(board, pin_name(nm))))
  tables$roles <- read_roles(board = board)
  tables
}

#' Read the role assignments (superusers)
#'
#' Roles that are not relational (owner and validator depend on the
#' initiative) are assigned in a table with columns:
#'
#' * `type`: `"user"` (a Posit Connect user name) or `"group"` (a Posit
#'   Connect group name);
#' * `principal`: the user or group name, as Connect reports it in
#'   `session$user` / `session$groups`;
#' * `role`: `"superuser"` (the only assignable role; everyone else is a
#'   contributor);
#' * `comment`: free text, e.g. who approved the assignment.
#'
#' The table is read from the pin `dv_roles` on Posit Connect, or from
#' `roles.csv` elsewhere. A missing pin or file gives an empty table.
#' @param file CSV file (used when `board` is `NULL`).
#' @param board A pins board.
#' @return A validated data frame, see [normalise_roles()].
#' @export
read_roles <- function(file = NULL, board = NULL) {
  roles <- if (!is.null(board)) {
    tryCatch(as.data.frame(pins::pin_read(board, pin_name("dv_roles"))), error = function(e) {
      warning("Pin dv_roles not readable (", conditionMessage(e), "); no superusers from pins.",
              call. = FALSE)
      NULL
    })
  } else if (!is.null(file) && file.exists(file)) {
    utils::read.csv(file, stringsAsFactors = FALSE, strip.white = TRUE, colClasses = "character")
  }
  normalise_roles(roles)
}

#' Current role assignments, refreshed from their source
#'
#' The roles table is re-read (pin `dv_roles` or `roles.csv`) when the
#' cached copy is older than `ttl` seconds, so that adding or removing a
#' superuser does not require restarting the app. If the source cannot be
#' read the cached copy is kept.
#' @param ttl Cache lifetime in seconds (`DV_ROLES_TTL`, default 300).
#' @return A roles data frame, or `NULL` when no configuration is loaded.
#' @export
current_roles <- function(ttl = as.numeric(Sys.getenv("DV_ROLES_TTL", "300"))) {
  cfg <- .dv$cfg
  if (is.null(cfg)) return(NULL)
  age <- as.numeric(difftime(Sys.time(), .dv$roles_at %||% Sys.time(), units = "secs"))
  if (age >= ttl) {
    fresh <- tryCatch(suppressWarnings(switch(cfg$source,
      pins = {
        r <- pins::pin_read(pins_board(), pin_name("dv_roles"))
        normalise_roles(as.data.frame(r))
      },
      csv = read_roles(file.path(Sys.getenv("DV_CONFIG_DIR", config_dir()), "roles.csv")),
      cfg$roles)), error = function(e) NULL)
    if (!is.null(fresh)) .dv$cfg$roles <- fresh
    .dv$roles_at <- Sys.time()
  }
  .dv$cfg$roles
}

#' Validate and normalise a roles table
#' @param roles Data frame with `type`, `principal`, `role` (and optional
#'   `comment`), or `NULL`.
#' @return Data frame `type`, `principal`, `role`, `comment`.
#' @export
normalise_roles <- function(roles) {
  empty <- data.frame(type = character(0), principal = character(0), role = character(0),
                      comment = character(0), stringsAsFactors = FALSE)
  if (is.null(roles) || !nrow(roles)) return(empty)
  miss <- setdiff(c("type", "principal", "role"), names(roles))
  if (length(miss)) stop("Roles table: missing column(s) ", paste(miss, collapse = ", "))
  if (!"comment" %in% names(roles)) roles$comment <- NA_character_
  roles <- data.frame(type = tolower(trimws(roles$type)), principal = trimws(roles$principal),
                      role = tolower(trimws(roles$role)), comment = as.character(roles$comment),
                      stringsAsFactors = FALSE)
  roles <- roles[!is.na(roles$principal) & nzchar(roles$principal), , drop = FALSE]
  bad <- !roles$type %in% c("user", "group") | !roles$role %in% "superuser"
  if (any(bad)) stop("Roles table: type must be 'user' or 'group' and role 'superuser' (row(s) ",
                     paste(which(bad), collapse = ", "), ")")
  rownames(roles) <- NULL
  roles
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
  pins::pin_write(board, tables$roles, pin_name("dv_roles"), type = "csv",
                  description = "Digital Value superusers (type user/group, principal, role, comment)")
  invisible(c(unname(config_tables), "dv_roles"))
}

#' Assemble and validate a configuration object from its tables
#'
#' @param parameters Data frame with columns `key`, `value`.
#' @param scales Data frame with columns `dimension`, `level`, `ordinal`, `value`.
#' @param event_map Data frame with columns `pattern`, `activity`, `process_phase`.
#' @param source Label of the configuration source.
#' @param roles Roles table (see [read_roles()]), optional.
#' @return A configuration list (see [load_config()]).
#' @export
build_config <- function(parameters, scales, event_map, source = "custom", roles = NULL) {
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
       event_map = event_map, roles = normalise_roles(roles), source = source)
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
