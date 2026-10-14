`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

#' Format a number for display
#' @param x Number.
#' @param digits Decimals.
#' @param suffix Unit suffix.
#' @return Character.
#' @export
fmt_num <- function(x, digits = 1, suffix = "") {
  ifelse(is.na(x), "\u2013",
         paste0(formatC(x, format = "f", digits = digits, big.mark = ","), suffix))
}

#' Compact KPI tile
#' @param label Label.
#' @param value Main value (formatted).
#' @param sub Sub-text.
#' @param tone `"neutral"` or `"strong"` (dark accent).
#' @return HTML tag.
#' @export
kpi_tile <- function(label, value, sub = NULL, tone = "neutral") {
  htmltools::div(class = paste("dv-kpi", paste0("dv-kpi-", tone)),
    htmltools::div(class = "dv-kpi-label", label),
    htmltools::div(class = "dv-kpi-value", value),
    if (!is.null(sub)) htmltools::div(class = "dv-kpi-sub", sub))
}

#' Callout box
#' @param ... Content.
#' @param title Title.
#' @param type Emphasis: `"info"`, `"low"`, `"medium"` or `"high"` (darker = more urgent).
#' @return HTML tag.
#' @export
callout <- function(..., title = NULL, type = "info") {
  htmltools::div(class = paste("dv-callout", paste0("dv-callout-", type)),
    if (!is.null(title)) htmltools::div(class = "dv-callout-title", title),
    htmltools::div(class = "dv-callout-body", ...))
}

#' Status badge coloured as in the charts
#' @param status Status.
#' @return HTML tag.
#' @export
status_badge <- function(status) {
  col <- status_colors()[status]
  if (is.na(col)) col <- mono$c400
  dark <- col %in% c(mono$c500, mono$c600, mono$c700, mono$c800, mono$c900)
  htmltools::span(class = "dv-badge",
                  style = sprintf("background:%s;color:%s;", col, if (dark) "#ffffff" else mono$c900),
                  status)
}

#' Section box (compact card)
#' @param title Header title.
#' @param ... Body content.
#' @param right Optional header element aligned right.
#' @param class Extra CSS class.
#' @param collapsible Make the body collapsible by clicking the header.
#' @param id Id of the collapsible body (required when `collapsible`).
#' @param open Initially open.
#' @return A bslib card.
#' @export
box <- function(title, ..., right = NULL, class = NULL, collapsible = FALSE, id = NULL, open = TRUE) {
  body <- bslib::card_body(..., fillable = FALSE)
  if (!collapsible) {
    return(bslib::card(class = paste("dv-box", class),
      bslib::card_header(htmltools::div(class = "dv-box-header", htmltools::span(title), right)),
      body))
  }
  stopifnot(!is.null(id))
  bslib::card(class = paste("dv-box dv-collapsible", class),
    bslib::card_header(
      class = if (open) "dv-collapse-toggle" else "dv-collapse-toggle collapsed",
      `data-bs-toggle` = "collapse", `data-bs-target` = paste0("#", id),
      `aria-expanded` = tolower(as.character(open)), `aria-controls` = id, role = "button",
      htmltools::div(class = "dv-box-header",
        htmltools::span(htmltools::span(class = "dv-chevron"), title), right)),
    htmltools::div(id = id, class = if (open) "collapse show" else "collapse", body))
}

#' Identify the user (Posit Connect aware)
#'
#' On Posit Connect `session$user` and `session$groups` are populated from the
#' authenticated identity; locally the OS user is used.
#' @param session Shiny session.
#' @return List `user`, `groups`, `role` (see [user_role()]).
#' @export
app_user <- function(session) {
  user <- session$user %||% Sys.getenv("USER", Sys.getenv("USERNAME", "local-user"))
  groups <- session$groups %||% character(0)
  list(user = user, groups = groups, role = user_role(user, groups))
}

dt_compact <- function(df, ..., page_length = 10, selection = "single", escape = TRUE) {
  opts <- utils::modifyList(list(pageLength = page_length, dom = "ftip", scrollX = TRUE,
                                 autoWidth = FALSE), list(...))
  DT::datatable(df, rownames = FALSE, selection = selection, escape = escape,
                class = "compact stripe hover", options = opts)
}

#' Evaluation funnel strip
#'
#' One cell per stage of [evaluation_funnel()]: count, a bar showing the share
#' of registered initiatives (segmented by state) and the breakdown.
#' @param stages Output of [evaluation_funnel()].
#' @return A tag.
#' @export
funnel_ui <- function(stages) {
  total <- max(1, stages[[1]]$n)
  part_labels <- c(pending = "pending", in_validation = "pending validation", complete = "validated",
                   approved = "approved", awaiting_audit = "awaiting audit", audited = "audited")
  part_class <- c(pending = "todo", in_validation = "wait", complete = "done", approved = "done",
                  awaiting_audit = "wait", audited = "done")
  cell <- function(st) {
    parts <- st$parts %||% c(all = st$n)
    segs <- lapply(names(parts), function(k) {
      if (parts[[k]] == 0) return(NULL)
      htmltools::span(class = paste0("dv-funnel-seg dv-funnel-", part_class[k] %|na|% "all"),
                      style = sprintf("width:%.1f%%", 100 * parts[[k]] / total),
                      title = sprintf("%s: %d", part_labels[k] %|na|% st$label, parts[[k]]))
    })
    htmltools::div(class = "dv-funnel-stage", `data-stage` = st$id,
      htmltools::div(class = "dv-kpi-label", st$label),
      htmltools::div(class = "dv-funnel-n", st$n,
        htmltools::span(class = "dv-funnel-pct",
                        if (st$id != "registered") sprintf("%.0f%%", 100 * st$n / total))),
      htmltools::div(class = "dv-funnel-bar", segs),
      if (!is.null(st$parts)) htmltools::div(class = "dv-funnel-parts",
        lapply(names(parts), function(k) htmltools::span(
          htmltools::span(class = paste0("dv-funnel-key dv-funnel-", part_class[k])),
          sprintf("%d %s", parts[[k]], part_labels[k])))))
  }
  htmltools::div(class = "dv-funnel", role = "list", `aria-label` = "Evaluation funnel",
                 lapply(stages, cell))
}

`%|na|%` <- function(a, b) if (is.na(a)) b else a
