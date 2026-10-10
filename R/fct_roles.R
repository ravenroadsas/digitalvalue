# User levels ------------------------------------------------------------------
#   contributor : everyone. Registers initiatives with their RICE (then locked).
#   owner       : the initiative's owner (or its creator). Records the 4MC
#                 valuation while it is not under / past validation, sends it
#                 to a validator, and marks the initiative delivered.
#   validator   : the Posit Connect user the 4MC was sent to; validates it or
#                 requests changes.
#   superuser   : everything, including editing locked records, the expert
#                 review, the value audit, model calibration and admin.
#
# Superusers are identified from the Posit Connect identity: members of the
# group in `DV_SUPERUSER_GROUP`, or users listed in `DV_SUPERUSERS`
# (comma-separated). With neither variable set (local development) everyone
# is superuser; `DV_DEV_ROLE=contributor` simulates a contributor locally.

#' Permission matrix
#' @return Data frame `action`, `label`, `everyone`, `owner`, `validator`, `superuser`.
#' @export
permission_matrix <- function() {
  data.frame(
    action = c("register", "edit_registration", "valuate", "validate", "review", "deliver",
               "undeliver", "audit", "calibrate", "admin"),
    label = c("Register an initiative with its RICE", "Edit a registration / RICE after saving",
              "Record the 4MC valuation and send it to a validator", "Validate the 4MC valuation",
              "Record expert review", "Mark the initiative delivered", "Revert a delivery",
              "Record value audit", "Calibrate and publish value models", "Administration tabs"),
    everyone  = c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE),
    owner     = c(TRUE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE),
    validator = c(TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE),
    superuser = TRUE,
    stringsAsFactors = FALSE)
}

#' Resolve the role of a user
#' @param user User name.
#' @param groups Groups of the user (Posit Connect).
#' @return `"superuser"` or `"contributor"`.
#' @export
user_role <- function(user, groups = character(0)) {
  dev <- Sys.getenv("DV_DEV_ROLE")
  grp <- Sys.getenv("DV_SUPERUSER_GROUP")
  usr <- trimws(strsplit(Sys.getenv("DV_SUPERUSERS"), ",")[[1]])
  usr <- usr[nzchar(usr)]
  if (!nzchar(grp) && !length(usr)) {
    return(if (dev %in% c("contributor", "superuser")) dev else "superuser")
  }
  if ((nzchar(grp) && grp %in% groups) || user %in% usr) "superuser" else "contributor"
}

#' Is the user the owner (or creator) of an initiative?
#' @param usr User list from [app_user()].
#' @param row Initiative row (with `owner`, `created_by`).
#' @return Logical.
#' @export
is_owner <- function(usr, row) {
  !is.null(row) && nrow(row) > 0 && usr$user %in% stats::na.omit(c(row$owner, row$created_by))
}

#' Can a user perform an action (on an initiative)?
#'
#' Superusers can do everything. Otherwise the action is allowed to everyone,
#' to the owner of `row`, or to the validator a pending 4MC validation of
#' `row` is addressed to, as listed in [permission_matrix()].
#' @param usr A user list from [app_user()], or a role name.
#' @param action Action id (see [permission_matrix()]).
#' @param row Optional initiative row (see [compute_portfolio()]).
#' @return Logical.
#' @export
can <- function(usr, action, row = NULL) {
  if (is.character(usr)) usr <- list(user = NA_character_, role = usr)
  m <- permission_matrix()
  if (!action %in% m$action) stop("Unknown action: ", action)
  p <- m[m$action == action, ]
  if (identical(usr$role, "superuser") || p$everyone) return(TRUE)
  if (is.null(row) || !nrow(row)) return(FALSE)
  if (p$owner && is_owner(usr, row)) return(TRUE)
  if (p$validator && isTRUE(row$validation_status == "pending") && isTRUE(row$validator == usr$user))
    return(TRUE)
  FALSE
}
