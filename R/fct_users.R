# User directory ----------------------------------------------------------------
# Owners and validators are Posit Connect users. On Connect the directory is
# read from the Connect Server API (GET /__api__/v1/users) using the
# `CONNECT_SERVER` and `CONNECT_API_KEY` environment variables (Connect
# provides CONNECT_SERVER; the API key is added as a content variable).
# Elsewhere it falls back to a CSV (`DV_USERS_FILE`, default
# inst/config/users.csv). The directory is cached for an hour.

#' Read the user directory
#' @param refresh Ignore the cache.
#' @return Data frame `username`, `name`, `email`.
#' @export
user_directory <- function(refresh = FALSE) {
  cached <- .dv$users
  if (!refresh && !is.null(cached) && difftime(Sys.time(), cached$at, units = "mins") < 60) {
    return(cached$users)
  }
  server <- Sys.getenv("CONNECT_SERVER")
  key <- Sys.getenv("CONNECT_API_KEY")
  users <- if (nzchar(server) && nzchar(key)) {
    tryCatch(connect_users(server, key), error = function(e) {
      warning("Posit Connect user directory unavailable: ", conditionMessage(e), call. = FALSE)
      local_users()
    })
  } else {
    local_users()
  }
  .dv$users <- list(users = users, at = Sys.time())
  users
}

#' Users from the Posit Connect Server API (all pages)
#' @param server Connect URL.
#' @param key API key.
#' @param page_size Page size.
#' @return Data frame `username`, `name`, `email`.
#' @export
connect_users <- function(server, key, page_size = 500) {
  out <- list()
  page <- 1
  repeat {
    url <- sprintf("%s/__api__/v1/users?page_size=%d&page_number=%d", sub("/+$", "", server),
                   page_size, page)
    h <- curl::new_handle()
    curl::handle_setheaders(h, Authorization = paste("Key", key))
    res <- curl::curl_fetch_memory(url, handle = h)
    if (res$status_code >= 400) stop("HTTP ", res$status_code)
    body <- jsonlite::fromJSON(rawToChar(res$content))
    r <- body$results
    if (is.null(r) || !NROW(r)) break
    keep <- if ("locked" %in% names(r)) !(r$locked %in% TRUE) else rep(TRUE, nrow(r))
    out[[length(out) + 1]] <- data.frame(
      username = r$username[keep],
      name = trimws(paste(r$first_name %||% "", r$last_name %||% ""))[keep],
      email = (r$email %||% rep(NA_character_, nrow(r)))[keep],
      stringsAsFactors = FALSE)
    if (page * page_size >= (body$total %||% 0)) break
    page <- page + 1
  }
  users <- do.call(rbind, out)
  if (is.null(users)) users <- data.frame(username = character(0), name = character(0), email = character(0))
  users$name[!nzchar(users$name)] <- users$username[!nzchar(users$name)]
  users[order(users$name), , drop = FALSE]
}

#' Local user list (development / fallback)
#' @param file CSV with columns `username`, `name`, `email`.
#' @return Data frame `username`, `name`, `email`.
#' @export
local_users <- function(file = Sys.getenv("DV_USERS_FILE",
                                         system.file("config", "users.csv", package = "digitalvalue"))) {
  if (!nzchar(file) || !file.exists(file)) {
    return(data.frame(username = character(0), name = character(0), email = character(0)))
  }
  u <- utils::read.csv(file, stringsAsFactors = FALSE, strip.white = TRUE)
  u[order(u$name), c("username", "name", "email"), drop = FALSE]
}

#' Choices for a user selector ("Name (username)" -> username)
#' @param users Output of [user_directory()].
#' @param extra Usernames to include even if not in the directory.
#' @return Named character vector.
#' @export
user_choices <- function(users, extra = character(0)) {
  extra <- setdiff(stats::na.omit(extra), users$username)
  extra <- extra[nzchar(extra)]
  stats::setNames(c(users$username, extra),
                  c(sprintf("%s (%s)", users$name, users$username), extra))
}

#' Display name of users
#' @param username User names.
#' @param users Output of [user_directory()].
#' @return Character vector (falls back to the user name).
#' @export
user_label <- function(username, users) {
  n <- users$name[match(username, users$username)]
  ifelse(is.na(n), username, n)
}
