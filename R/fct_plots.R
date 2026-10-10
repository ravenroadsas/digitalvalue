# Charts -----------------------------------------------------------------------
# Charts are built as plain ECharts option lists (pure, unit-testable R) and
# rendered with echarts4r. Rendering happens in the browser (client-side),
# the R process only ships the JSON option.

#' Monochrome palette (slate), from lightest (`c050`) to darkest (`c900`)
#' @export
mono <- list(c050 = "#F5F7F9", c100 = "#E6EAEF", c200 = "#CDD5DE", c300 = "#A9B5C3",
             c400 = "#8494A7", c500 = "#64768B", c600 = "#4B5D72", c700 = "#364A60",
             c800 = "#24364A", c900 = "#142233")

#' Shade per status
#'
#' One hue: initiatives in appraisal are light, prioritised and in-execution
#' initiatives are the darkest so they stand out.
#' @return Named character vector.
#' @export
status_colors <- function() {
  c("Registered" = mono$c200, "4M valuation" = mono$c300, "Expert review" = mono$c400,
    "Ready" = mono$c500, "Prioritized" = mono$c700, "In execution" = mono$c900,
    "Closed" = mono$c600, "Audited" = mono$c600, "Rejected" = mono$c100)
}

#' Chart group of a status (prioritisation scatter)
#' @param status Status vector.
#' @return Character vector of groups.
#' @export
plot_group <- function(status) {
  ifelse(status == "Prioritized", "Prioritized",
  ifelse(status == "In execution", "In execution",
  ifelse(status %in% c("Closed", "Audited"), "Closed / audited",
  ifelse(status == "Rejected", "Rejected",
  ifelse(status == "Ready", "Ready for decision", "In appraisal")))))
}

#' Shade and marker per scatter group
#'
#' Prioritized (dark circle) and In execution (darkest diamond) are labelled
#' and drawn on top.
#' @return Data frame `group`, `color`, `symbol`.
#' @export
group_style <- function() {
  data.frame(group = c("In appraisal", "Ready for decision", "Prioritized", "In execution",
                       "Closed / audited", "Rejected"),
             color = c(mono$c300, mono$c500, mono$c700, mono$c900, mono$c400, mono$c200),
             symbol = c("circle", "triangle", "circle", "diamond", "rect", "circle"),
             stringsAsFactors = FALSE)
}

#' Data for the prioritisation scatter (value vs effort)
#' @param pf Output of [compute_portfolio()].
#' @param cfg Configuration list.
#' @param y `"rice_value"` (reach x impact x confidence, i.e. RICE without the
#'   effort divisor, so the chart reads as value vs effort), `"value"` (ex-ante
#'   mm USD) or `"estimate"` (ex-ante mm USD, or the value-model estimate when
#'   not yet valued).
#' @param estimate Value-model median per row of `pf` (used by `"estimate"`).
#' @return Data frame `id`, `name`, `status`, `group`, `effort`, `x`, `y`,
#'   `size`, `estimated`.
#' @export
prioritization_data <- function(pf, cfg, y = c("rice_value", "value", "estimate"), estimate = NULL) {
  y <- match.arg(y)
  d <- pf[!is.na(pf$score), , drop = FALSE]
  if (!nrow(d)) {
    return(data.frame(id = character(0), name = character(0), status = character(0),
                      group = character(0), effort = character(0), x = numeric(0),
                      y = numeric(0), size = numeric(0), estimated = logical(0)))
  }
  est <- if (is.null(estimate)) rep(NA_real_, nrow(pf)) else estimate
  est <- est[!is.na(pf$score)]
  x <- rice_ordinal(d$effort, "effort", cfg)
  # deterministic jitter so initiatives with the same effort do not overlap
  k <- stats::ave(seq_along(x), x, FUN = seq_along)
  n <- stats::ave(seq_along(x), x, FUN = length)
  jitter <- ifelse(n > 1, ((k - 1) / pmax(n - 1, 1) - 0.5) * 0.5, 0)
  valued <- !is.na(d$planned_value_mm_usd) & d$planned_value_mm_usd > 0
  estimated <- y == "estimate" & !valued & !is.na(est)
  rice_value <- d$reach_value * rice_weight(d$impact, "impact", cfg) *
    rice_weight(d$confidence, "confidence", cfg)
  yv <- switch(y, rice_value = rice_value, value = d$planned_value_mm_usd,
               estimate = ifelse(estimated, est, d$planned_value_mm_usd))
  data.frame(id = d$id, name = d$name, status = d$status, group = plot_group(d$status),
             effort = d$effort, x = x + jitter, y = yv,
             size = d$reach_value, estimated = estimated, stringsAsFactors = FALSE)
}

js <- function(...) htmlwidgets::JS(paste0(...))

#' ECharts option of the prioritisation scatter
#' @inheritParams prioritization_data
#' @param highlight Optional selected initiative id (drawn with a ring).
#' @return An ECharts option list.
#' @export
prioritization_option <- function(pf, cfg, y = c("rice_value", "value", "estimate"), highlight = NULL,
                                  estimate = NULL) {
  y <- match.arg(y)
  d <- prioritization_data(pf, cfg, y, estimate)
  gs <- group_style()
  lv <- rice_levels(cfg, "effort")
  ylab <- switch(y, rice_value = "R \u00d7 I \u00d7 C", value = "Ex-ante value (mm USD)",
                 estimate = "Value / est. mm USD")
  series <- lapply(gs$group, function(g) {
    s <- d[d$group == g, , drop = FALSE]
    col <- gs$color[gs$group == g]
    strong <- g %in% c("Prioritized", "In execution")
    list(
      name = g, type = "scatter",
      data = lapply(seq_len(nrow(s)), function(i) list(
        value = c(s$x[i], s$y[i], s$size[i]), name = s$name[i], id = s$id[i],
        effort = s$effort[i], status = s$status[i], estimated = s$estimated[i],
        # hollow markers = value anticipated by the value model
        itemStyle = c(
          if (s$estimated[i]) list(color = "#ffffff", borderColor = col, borderWidth = 2,
                                   borderType = "dashed", opacity = 1),
          if (!is.null(highlight) && s$id[i] == highlight) list(borderColor = "#111", borderWidth = 3)))),
      symbolSize = js("function (v) { return 8 + v[2] * 4; }"),
      symbol = gs$symbol[gs$group == g],
      itemStyle = list(color = col, opacity = if (strong) 1 else 0.85,
                       borderColor = if (g == "Rejected") mono$c400 else "#ffffff",
                       borderWidth = if (strong) 1.2 else 0.8),
      label = list(show = strong, position = "right", fontSize = 9, color = mono$c900,
                   formatter = js("function (p) { return p.data.id; }")),
      z = if (strong) 3 else 2
    )
  })
  p <- cfg$params
  gate_x <- rice_ordinal(p$gate2_effort_min, "effort", cfg) - 0.5
  if (y == "rice_value") {
    # score gate (R x I x C / effort >= threshold) as a staircase: R x I x C >= threshold x effort
    w <- rice_weight(lv, "effort", cfg)
    stair <- unlist(lapply(seq_along(lv), function(i)
      list(c(i - 0.5, p$gate2_score_min * w[i]), c(i + 0.5, p$gate2_score_min * w[i]))), recursive = FALSE)
    series[[length(series) + 1]] <- list(
      name = sprintf("Valuation gate (score \u2265 %s)", p$gate2_score_min), type = "line",
      data = stair, showSymbol = FALSE, silent = TRUE, z = 1,
      lineStyle = list(type = "dashed", color = mono$c500, width = 1), itemStyle = list(color = mono$c500))
    gate_lines <- list()
  } else {
    gate_lines <- list(list(yAxis = p$gate3_value_min_mm_usd, name = "Review gate (value)",
                            label = list(formatter = "Review gate (value)")))
  }
  # gate lines go on the first non-empty series
  host <- which(vapply(series, function(s) length(s$data) > 0, logical(1)))[1]
  if (is.na(host)) host <- 1
  series[[host]]$markLine <- list(
    silent = TRUE, symbol = "none",
    lineStyle = list(type = "dashed", color = mono$c500, width = 1),
    label = list(fontSize = 9, color = mono$c600, position = "insideEndTop"),
    data = c(gate_lines, list(list(xAxis = gate_x, name = "Valuation gate (effort)",
                                   label = list(formatter = "Valuation gate (effort)")))))
  # keep the axis on the initiatives, not on the far end of the gate staircase
  y_max <- if (y == "rice_value" && nrow(d)) ceiling(max(d$y, na.rm = TRUE) * 1.15) else NULL
  list(
    color = gs$color,
    textStyle = list(fontSize = 10, color = mono$c700),
    grid = list(left = 50, right = 20, top = 40, bottom = 40),
    legend = list(top = 0, textStyle = list(fontSize = 10), itemWidth = 10, itemHeight = 10),
    tooltip = list(trigger = "item", formatter = js(
      "function (p) { if (!p.data || !p.data.id) return p.name;",
      " return '<b>' + p.data.id + ' \u00b7 ' + p.data.name + '</b><br/>' +",
      " 'Status: ' + p.data.status + '<br/>Effort: ' + p.data.effort + '<br/>",
      ylab, ": ' + p.value[1].toFixed(2) + (p.data.estimated ? ' (model estimate)' : '') + '<br/>Reach: 10^' + p.value[2].toFixed(1) + ' users'; }")),
    xAxis = list(type = "value", name = "Effort", nameLocation = "middle", nameGap = 24,
                 min = 0, max = length(lv) + 1, interval = 1,
                 splitLine = list(show = FALSE),
                 axisLabel = list(formatter = js(
                   "function (v) { var l = ", jsonlite::toJSON(lv),
                   "; return Number.isInteger(v) ? (l[v - 1] || '') : ''; }"))),
    yAxis = list(type = "value", name = ylab, max = y_max, nameTextStyle = list(fontSize = 10),
                 splitLine = list(lineStyle = list(color = mono$c100))),
    series = series
  )
}

#' ECharts option: initiatives and value per status
#' @param pf Portfolio data frame.
#' @return An ECharts option list.
#' @export
status_option <- function(pf) {
  s <- status_summary(pf)
  cols <- status_colors()
  list(
    textStyle = list(fontSize = 10, color = mono$c700),
    grid = list(left = 40, right = 50, top = 30, bottom = 55),
    legend = list(top = 0, textStyle = list(fontSize = 10)),
    tooltip = list(trigger = "axis"),
    xAxis = list(type = "category", data = s$status,
                 axisLabel = list(rotate = 30, fontSize = 9, interval = 0)),
    yAxis = list(list(type = "value", name = "#", minInterval = 1,
                      splitLine = list(lineStyle = list(color = mono$c100))),
                 list(type = "value", name = "mm USD", splitLine = list(show = FALSE))),
    series = list(
      list(name = "Initiatives", type = "bar", barWidth = "55%", itemStyle = list(color = mono$c600),
           data = lapply(seq_len(nrow(s)), function(i)
             list(value = s$n[i], itemStyle = list(color = unname(cols[s$status[i]]),
                                                   borderColor = mono$c400, borderWidth = 0.5)))),
      list(name = "Ex-ante value (mm USD)", type = "line", yAxisIndex = 1,
           symbolSize = 6, lineStyle = list(color = mono$c900, width = 1),
           itemStyle = list(color = mono$c900), data = round(s$value_mm_usd, 2)))
  )
}

#' ECharts option: 4M estimate vs expert review vs audit, per metric (mm USD)
#' @param lc Output of [m4_lifecycle()].
#' @return An ECharts option list.
#' @export
lifecycle_option <- function(lc) {
  r <- function(x) ifelse(is.na(x), 0, round(x, 3))
  list(
    textStyle = list(fontSize = 10, color = mono$c700),
    color = c(mono$c300, mono$c600, mono$c900),
    grid = list(left = 50, right = 20, top = 30, bottom = 30),
    legend = list(top = 0, textStyle = list(fontSize = 10)),
    tooltip = list(trigger = "axis"),
    xAxis = list(type = "category", data = lc$label, axisLabel = list(interval = 0)),
    yAxis = list(type = "value", name = "mm USD", splitLine = list(lineStyle = list(color = mono$c100))),
    series = list(
      list(name = "4M estimate", type = "bar", data = r(lc$estimate_mm_usd)),
      list(name = "Expert review", type = "bar", data = r(lc$review_mm_usd)),
      list(name = "Audited", type = "bar", data = r(lc$actual_mm_usd)))
  )
}

#' ECharts option: value breakdown by 4M metric for one initiative
#' @param summary Output of [m4_summary()].
#' @return An ECharts option list.
#' @export
m4_breakdown_option <- function(summary) {
  s <- summary[summary$value_mm_usd != 0, , drop = FALSE]
  list(
    textStyle = list(fontSize = 10, color = mono$c700),
    color = c(mono$c900, mono$c600, mono$c400, mono$c200),
    tooltip = list(trigger = "item", formatter = "{b}: {c} mm USD ({d}%)"),
    series = list(list(
      type = "pie", radius = c("45%", "75%"),
      label = list(fontSize = 9, formatter = "{b}\n{c}"),
      data = lapply(seq_len(nrow(s)), function(i)
        list(name = s$label[i], value = round(s$value_mm_usd[i], 3)))))
  )
}

#' ECharts option: effort per process phase (process mining)
#' @param pe Output of [phase_effort()].
#' @return An ECharts option list.
#' @export
phase_effort_option <- function(pe) {
  list(
    textStyle = list(fontSize = 10, color = mono$c700),
    color = c(mono$c700),
    grid = list(left = 160, right = 20, top = 20, bottom = 30),
    tooltip = list(trigger = "axis"),
    xAxis = list(type = "value", name = "minutes", splitLine = list(lineStyle = list(color = mono$c100))),
    yAxis = list(type = "category", data = pe$process_phase, axisLabel = list(fontSize = 9)),
    series = list(list(name = "Minutes", type = "bar", data = round(pe$minutes, 1)))
  )
}

#' Render an ECharts option with echarts4r
#' @param option An ECharts option list.
#' @param height CSS height.
#' @return An echarts4r htmlwidget.
#' @export
echart_from_option <- function(option, height = NULL) {
  echarts4r::e_list(echarts4r::e_charts(height = height), option)
}
