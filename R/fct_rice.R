#' Levels of a RICE dimension, in ordinal order
#' @param cfg Configuration list.
#' @param dimension `"impact"`, `"confidence"` or `"effort"`.
#' @return Character vector.
#' @export
rice_levels <- function(cfg, dimension) {
  s <- cfg$scales[cfg$scales$dimension == dimension, , drop = FALSE]
  s$level[order(s$ordinal)]
}

#' Numeric weight of RICE levels
#' @param level Character vector of levels.
#' @param dimension Dimension name.
#' @param cfg Configuration list.
#' @return Numeric vector (`NA` for unknown levels).
#' @export
rice_weight <- function(level, dimension, cfg) {
  s <- cfg$scales[cfg$scales$dimension == dimension, , drop = FALSE]
  s$value[match(level, s$level)]
}

#' Ordinal position of RICE levels (1 = smallest)
#' @inheritParams rice_weight
#' @return Integer vector.
#' @export
rice_ordinal <- function(level, dimension, cfg) {
  s <- cfg$scales[cfg$scales$dimension == dimension, , drop = FALSE]
  s$ordinal[match(level, s$level)]
}

#' Reach value: log10 of the number of users reached
#'
#' Values below 1 user are floored to 1 (reach value 0).
#' @param users Number of users.
#' @return Numeric vector.
#' @export
reach_value <- function(users) {
  users <- suppressWarnings(as.numeric(users))
  log10(pmax(users, 1))
}

#' RICE score = log10(users) x impact x confidence / effort
#' @param users Number of users reached.
#' @param impact,confidence,effort Levels (see `rice_scales.csv`).
#' @param cfg Configuration list.
#' @return Numeric vector.
#' @export
rice_score <- function(users, impact, confidence, effort, cfg) {
  reach_value(users) *
    rice_weight(impact, "impact", cfg) *
    rice_weight(confidence, "confidence", cfg) /
    rice_weight(effort, "effort", cfg)
}

#' Full RICE assessment for one initiative
#' @inheritParams rice_score
#' @param rationale Free text supporting the scoring.
#' @return A list ready for [db_save_rice()].
#' @export
rice_assess <- function(users, impact, confidence, effort, cfg, rationale = NA) {
  if (is.null(users) || is.na(users) || users < 0) stop("Reach (users) must be a non-negative number")
  for (d in c("impact", "confidence", "effort")) {
    lv <- get(d)
    if (!lv %in% rice_levels(cfg, d)) stop("Unknown ", d, " level: ", lv)
  }
  list(users = users, impact = impact, confidence = confidence, effort = effort,
       reach_value = reach_value(users),
       score = rice_score(users, impact, confidence, effort, cfg),
       rationale = rationale)
}
