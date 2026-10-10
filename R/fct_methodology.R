# Methodology page ------------------------------------------------------------
# The methodology text lives in Markdown files (inst/app/methodology/
# methodology_<lang>.md) so it can be edited without touching code. Numbers
# that depend on the configuration (gates, weights, conversion factors) are
# written as {{placeholders}} and filled from the active configuration, so the
# document always describes what the app actually does.

#' Languages of the methodology page
#' @export
methodology_languages <- c("English" = "en", "Espa\u00f1ol" = "es")

methodology_dir <- function() system.file("app", "methodology", package = "digitalvalue")

#' Values substituted into the methodology placeholders
#' @param cfg Configuration list.
#' @param lang `"en"` or `"es"`.
#' @return Named character vector (placeholder -> text).
#' @export
methodology_values <- function(cfg, lang = "en") {
  p <- cfg$params
  es <- lang == "es"
  num <- function(x, d = 0) formatC(as.numeric(x), format = "f", digits = d, big.mark = if (es) "." else ",",
                                    decimal.mark = if (es) "," else ".", drop0trailing = TRUE)
  s <- cfg$scales
  dims <- c(impact = if (es) "Impacto" else "Impact", confidence = if (es) "Confianza" else "Confidence",
            effort = if (es) "Esfuerzo" else "Effort")
  rice_rows <- vapply(names(dims), function(d) {
    x <- s[s$dimension == d, ]
    x <- x[order(x$ordinal), ]
    sprintf("| %s | %s |", dims[[d]], paste(sprintf("%s = %s", x$level, num(x$value, 2)), collapse = " \u00b7 "))
  }, "")
  rice_table <- paste(c(if (es) "| Dimensi\u00f3n | Niveles y pesos |" else "| Dimension | Levels and weights |",
                        "|---|---|", rice_rows), collapse = "\n")
  rv <- reserve_values(cfg)
  reserve_table <- paste(c(if (es) "| Categor\u00eda | USD por barril |" else "| Category | USD per barrel |",
                           "|---|---|", sprintf("| %s | %s |", names(rv), num(rv, 2))), collapse = "\n")
  ex <- m4_calculate("p_jobs_success", list(n_jobs = 10, bopd_per_job = 30, success_before = 60,
                                           success_after = 70), cfg)
  lvl <- as.numeric(p$value_model_interval %||% 0.8)
  lab <- interval_labels(lvl)
  vals <- c(
    gate2_effort_min = p$gate2_effort_min, gate2_score_min = num(p$gate2_score_min, 2),
    gate3_value_min_mm_usd = num(p$gate3_value_min_mm_usd, 2),
    gate3_cost_min_mm_usd = num(p$gate3_cost_min_mm_usd, 2),
    netback_usd_bbl = num(p$netback_usd_bbl, 2), days_per_year = num(p$days_per_year),
    avg_salary_usd_year = num(p$avg_salary_usd_year), work_hours_year = num(p$work_hours_year),
    time_productivity_coef = num(p$time_productivity_coef, 2),
    time_value_usd_h = num(time_value_usd_hour(cfg)),
    value_model_min_n = num(p$value_model_min_n %||% 8),
    interval_pct = num(100 * lvl), p_low = lab[1], p_high = lab[2],
    example_workover_bopd = num(ex$value), example_workover_mm = num(ex$value_mm_usd, 3),
    rice_weights_table = rice_table, reserve_table = reserve_table)
  vapply(vals, as.character, "")
}

#' Methodology Markdown with placeholders filled in
#' @param lang `"en"` or `"es"`.
#' @param cfg Configuration list.
#' @param dir Folder with the Markdown files.
#' @return Character string (Markdown).
#' @export
methodology_markdown <- function(lang = "en", cfg, dir = methodology_dir()) {
  lang <- match.arg(lang, unname(methodology_languages))
  md <- paste(readLines(file.path(dir, paste0("methodology_", lang, ".md")), encoding = "UTF-8",
                        warn = FALSE), collapse = "\n")
  v <- methodology_values(cfg, lang)
  for (k in names(v)) md <- gsub(paste0("{{", k, "}}"), v[[k]], md, fixed = TRUE)
  left <- regmatches(md, gregexpr("\\{\\{[a-z0-9_]+\\}\\}", md))[[1]]
  if (length(left)) stop("Unresolved methodology placeholders: ", paste(unique(left), collapse = ", "))
  md
}

slugify <- function(x) {
  x <- iconv(tolower(x), "UTF-8", "ASCII//TRANSLIT", sub = "")
  x <- gsub("[^a-z0-9]+", "-", x)
  gsub("^-|-$", "", x)
}

#' Render the methodology to HTML with anchors and a table of contents
#' @param md Markdown (see [methodology_markdown()]).
#' @return List `html` (character) and `toc` (data frame `level`, `id`, `title`).
#' @export
render_methodology <- function(md) {
  html <- commonmark::markdown_html(md, extensions = TRUE)
  m <- gregexpr("<h([23])>(.*?)</h\\1>", html, perl = TRUE)
  heads <- regmatches(html, m)[[1]]
  level <- as.integer(sub("^<h([23])>.*", "\\1", heads))
  inner <- sub("^<h[23]>(.*)</h[23]>$", "\\1", heads)
  title <- gsub("<[^>]+>", "", inner)
  title <- gsub("&amp;", "&", title, fixed = TRUE)
  id <- paste0("m-", make.unique(vapply(title, slugify, ""), sep = "-"))
  regmatches(html, m) <- list(sprintf("<h%d id=\"%s\">%s</h%d>", level, id, inner, level))
  list(html = html, toc = data.frame(level = level, id = id, title = title, stringsAsFactors = FALSE))
}
