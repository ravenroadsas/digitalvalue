# Charts -----------------------------------------------------------------------
# Charts are built as plain ECharts option lists (pure, unit-testable R) and
# rendered with echarts4r. Rendering happens in the browser (client-side),
# the R process only ships the JSON option.

#' Colour palette per status
#'
#' Prioritised and in-execution initiatives are highlighted (green / blue);
#' initiatives still in assessment are muted.
#' @return Named character vector.
#' @export
status_colors <- function() {
  c("Phase I" = "#9AA4AE", "Phase II" = "#C9A227", "Phase III" = "#D9822B",
    "Ready" = "#6E8BA8", "Prioritized" = "#1E9E5A", "In execution" = "#1667D9",
    "Closed" = "#7D6BC4", "Audited" = "#4B3F8C", "Rejected" = "#C0392B")
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
  ifelse(status == "Ready", "Ready for decision", "In assessment")))))
}

#' Group colours of the prioritisation scatter
#' @return Named character vector.
#' @export
group_colors <- function() {
  c("In assessment" = "#9AA4AE", "Ready for decision" = "#E0A526",
    "Prioritized" = "#1E9E5A", "In execution" = "#1667D9",
    "Closed / audited" = "#7D6BC4", "Rejected" = "#D98880")
}

#' Data for the prioritisation scatter (value vs effort)
#' @param pf Output of [compute_portfolio()].
#' @param cfg Configuration list.
#' @param y `"score"` (RICE) or `"value"` (planned mm USD).
#' @return Data frame `id`, `name`, `status`, `group`, `effort`, `x`, `y`, `size`.
#' @export
prioritization_data <- function(pf, cfg, y = c("score", "value")) {
  y <- match.arg(y)
  d <- pf[!is.na(pf$score), , drop = FALSE]
  if (!nrow(d)) {
    return(data.frame(id = character(0), name = character(0), status = character(0),
                      group = character(0), effort = character(0), x = numeric(0),
                      y = numeric(0), size = numeric(0)))
  }
  x <- rice_ordinal(d$effort, "effort", cfg)
  # deterministic jitter so initiatives with the same effort do not overlap
  k <- stats::ave(seq_along(x), x, FUN = seq_along)
  n <- stats::ave(seq_along(x), x, FUN = length)
  jitter <- ifelse(n > 1, ((k - 1) / pmax(n - 1, 1) - 0.5) * 0.5, 0)
  data.frame(id = d$id, name = d$name, status = d$status, group = plot_group(d$status),
             effort = d$effort, x = x + jitter,
             y = if (y == "score") d$score else d$planned_value_mm_usd,
             size = d$reach_value, stringsAsFactors = FALSE)
}

js <- function(...) htmlwidgets::JS(paste0(...))

#' ECharts option of the prioritisation scatter
#' @inheritParams prioritization_data
#' @param highlight Optional selected initiative id (drawn with a ring).
#' @return An ECharts option list.
#' @export
prioritization_option <- function(pf, cfg, y = c("score", "value"), highlight = NULL) {
  y <- match.arg(y)
  d <- prioritization_data(pf, cfg, y)
  cols <- group_colors()
  lv <- rice_levels(cfg, "effort")
  ylab <- if (y == "score") "RICE score" else "Value (mm USD)"
  series <- lapply(names(cols), function(g) {
    s <- d[d$group == g, , drop = FALSE]
    strong <- g %in% c("Prioritized", "In execution")
    list(
      name = g, type = "scatter",
      data = lapply(seq_len(nrow(s)), function(i) list(
        value = c(s$x[i], s$y[i], s$size[i]), name = s$name[i], id = s$id[i],
        effort = s$effort[i], status = s$status[i],
        itemStyle = if (!is.null(highlight) && s$id[i] == highlight)
          list(borderColor = "#111", borderWidth = 3) else NULL)),
      symbolSize = js("function (v) { return 8 + v[2] * 4; }"),
      itemStyle = list(color = unname(cols[g]), opacity = if (strong) 0.95 else 0.6,
                       borderColor = if (strong) "#0b2e1a" else "#ffffff",
                       borderWidth = if (strong) 1.2 else 0.5),
      label = list(show = strong, position = "right", fontSize = 9,
                   formatter = js("function (p) { return p.data.id; }")),
      z = if (strong) 3 else 2
    )
  })
  p <- cfg$params
  gate_x <- rice_ordinal(p$gate2_effort_min, "effort", cfg) - 0.5
  gate_y <- if (y == "score") p$gate2_score_min else p$gate3_value_min_mm_usd
  gate_lbl <- if (y == "score") "Phase II gate (score)" else "Phase III gate (value)"
  # gate lines go on the first non-empty series
  host <- which(vapply(series, function(s) length(s$data) > 0, logical(1)))[1]
  if (is.na(host)) host <- 1
  series[[host]]$markLine <- list(
    silent = TRUE, symbol = "none",
    lineStyle = list(type = "dashed", color = "#C0392B", width = 1),
    label = list(fontSize = 9, color = "#C0392B", position = "insideEndTop"),
    data = list(list(yAxis = gate_y, name = gate_lbl, label = list(formatter = gate_lbl)),
                list(xAxis = gate_x, name = "Phase II gate (effort)",
                     label = list(formatter = "Phase II gate (effort)"))))
  list(
    color = unname(cols),
    textStyle = list(fontSize = 10),
    grid = list(left = 50, right = 20, top = 40, bottom = 40),
    legend = list(top = 0, textStyle = list(fontSize = 10), itemWidth = 10, itemHeight = 10),
    tooltip = list(trigger = "item", formatter = js(
      "function (p) { if (!p.data || !p.data.id) return p.name;",
      " return '<b>' + p.data.id + ' \u00b7 ' + p.data.name + '</b><br/>' +",
      " 'Status: ' + p.data.status + '<br/>Effort: ' + p.data.effort + '<br/>",
      ylab, ": ' + p.value[1].toFixed(2) + '<br/>Reach: 10^' + p.value[2].toFixed(1) + ' users'; }")),
    xAxis = list(type = "value", name = "Effort", nameLocation = "middle", nameGap = 24,
                 min = 0, max = length(lv) + 1, interval = 1,
                 splitLine = list(show = FALSE),
                 axisLabel = list(formatter = js(
                   "function (v) { var l = ", jsonlite::toJSON(lv),
                   "; return Number.isInteger(v) ? (l[v - 1] || '') : ''; }"))),
    yAxis = list(type = "value", name = ylab, nameTextStyle = list(fontSize = 10),
                 splitLine = list(lineStyle = list(color = "#e3e6ea"))),
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
    textStyle = list(fontSize = 10),
    grid = list(left = 40, right = 50, top = 30, bottom = 50),
    legend = list(top = 0, textStyle = list(fontSize = 10)),
    tooltip = list(trigger = "axis"),
    xAxis = list(type = "category", data = s$status,
                 axisLabel = list(rotate = 30, fontSize = 9, interval = 0)),
    yAxis = list(list(type = "value", name = "#", minInterval = 1),
                 list(type = "value", name = "mm USD", splitLine = list(show = FALSE))),
    series = list(
      list(name = "Initiatives", type = "bar", barWidth = "55%",
           data = lapply(seq_len(nrow(s)), function(i)
             list(value = s$n[i], itemStyle = list(color = unname(cols[s$status[i]]))))),
      list(name = "Planned value (mm USD)", type = "line", yAxisIndex = 1,
           symbolSize = 6, lineStyle = list(color = "#333", width = 1),
           itemStyle = list(color = "#333"), data = round(s$value_mm_usd, 2)))
  )
}

#' ECharts option: planned vs actual value per PRMT metric
#' @param pva Output of [plan_vs_actual()].
#' @return An ECharts option list.
#' @export
plan_actual_option <- function(pva) {
  list(
    textStyle = list(fontSize = 10),
    color = c("#6E8BA8", "#1E9E5A"),
    grid = list(left = 50, right = 20, top = 30, bottom = 30),
    legend = list(top = 0, textStyle = list(fontSize = 10)),
    tooltip = list(trigger = "axis"),
    xAxis = list(type = "category", data = pva$label, axisLabel = list(interval = 0)),
    yAxis = list(type = "value", name = "mm USD"),
    series = list(
      list(name = "Planned", type = "bar", data = round(pva$planned_mm_usd, 3)),
      list(name = "Actual", type = "bar", data = round(pva$actual_mm_usd, 3)))
  )
}

#' ECharts option: value breakdown by PRMT metric for one initiative
#' @param summary Output of [prmt_summary()].
#' @return An ECharts option list.
#' @export
prmt_breakdown_option <- function(summary) {
  s <- summary[summary$value_mm_usd != 0, , drop = FALSE]
  list(
    textStyle = list(fontSize = 10),
    color = c("#1E9E5A", "#C9A227", "#1667D9", "#7D6BC4"),
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
    textStyle = list(fontSize = 10),
    color = c("#1667D9"),
    grid = list(left = 150, right = 20, top = 20, bottom = 30),
    tooltip = list(trigger = "axis"),
    xAxis = list(type = "value", name = "minutes"),
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
