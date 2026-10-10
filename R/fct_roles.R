# User levels ------------------------------------------------------------------
#   contributor : everyone. Can register an initiative with its RICE (once);
#                 the record is then locked for them. Read access elsewhere.
#   superuser   : edits registrations and RICE, 4M valuation, expert review,
#                 workflow decisions, value audit, model calibration, admin.
#
# Superusers are identified from the Posit Connect identity: members of the
# group in `DV_SUPERUSER_GROUP`, or users listed in `DV_SUPERUSERS`
# (comma-separated). With neither variable set (local development) everyone
# is superuser; `DV_DEV_ROLE=contributor` simulates a contributor locally.

#' Permission matrix
#' @return Data frame `action`, `label`, `contributor`, `superuser`.
#' @export
permission_matrix <- function() {
  data.frame(
    action = c("register", "edit_registration", "valuate", "review", "decide",
               "audit", "calibrate", "admin"),
    label = c("Register an initiative with its RICE", "Edit a registration / RICE after saving",
              "Record 4M valuation", "Record expert review", "Workflow decisions",
              "Record value audit", "Calibrate and publish value models", "Administration tabs"),
    contributor = c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE),
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

#' Can a role perform an action?
#' @param role Role (`"contributor"` / `"superuser"`), or a user list from [app_user()].
#' @param action Action id (see [permission_matrix()]).
#' @return Logical.
#' @export
can <- function(role, action) {
  if (is.list(role)) role <- role$role
  m <- permission_matrix()
  if (!action %in% m$action) stop("Unknown action: ", action)
  isTRUE(m[[role]][m$action == action])
}
