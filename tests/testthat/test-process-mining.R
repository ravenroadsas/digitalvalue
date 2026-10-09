raw_events <- function() data.frame(
  ts = c("2026-01-01 10:00:00", "2026-01-01 10:00:05", "2026-01-01 10:00:20",
         "2026-01-01 10:00:30", "2026-01-01 10:30:00", "2026-01-01 10:30:10"),
  session_id = "s1", user_name = "u1",
  initiative_id = c("DV-0001", "DV-0001", "DV-0001", "DV-0001", "DV-0001", NA),
  source = "input",
  raw_name = c("nav", "phase1-users", "phase1-impact", "phase1-save", "phase1-users", "unknown-x"),
  raw_value = c("phase1", "10", "M", "1", "20", "1"),
  stringsAsFactors = FALSE)

test_that("raw events map to activities and phases", {
  m <- map_raw_events(raw_events(), test_cfg()$event_map)
  expect_equal(m$key[1], "nav:phase1")
  expect_equal(m$activity[1:4], c("Open RICE scoring", "Edit RICE inputs", "Edit RICE inputs",
                                  "Save RICE score"))
  expect_equal(m$process_phase[2], "2 Phase I - RICE")
  expect_true(is.na(m$activity[6]))
})

test_that("consecutive events collapse into activity instances with idle gaps", {
  a <- build_activity_log(raw_events(), test_cfg()$event_map, gap_minutes = 10)
  expect_equal(a$activity, c("Open RICE scoring", "Edit RICE inputs", "Save RICE score",
                             "Edit RICE inputs"))
  expect_equal(a$n_events, c(1L, 2L, 1L, 1L))
  expect_equal(a$start[2], "2026-01-01 10:00:05")
  expect_equal(a$complete[2], "2026-01-01 10:00:20")
  expect_equal(nrow(build_activity_log(raw_events()[0, ], test_cfg()$event_map)), 0)
})

test_that("business log and exported event log", {
  h <- data.frame(initiative_id = "DV-0001", from_status = c(NA, "Phase I"),
                  to_status = c("Phase I", "Phase II"), changed_by = "u1",
                  changed_at = c("2026-01-01 09:00:00", "2026-01-01 10:01:00"), comment = NA)
  b <- build_business_log(h)
  expect_equal(b$activity, c("Register initiative", "Enter Phase II"))
  a <- build_activity_log(raw_events(), test_cfg()$event_map)
  e <- export_event_log(a, b)
  expect_equal(nrow(e), 2 * nrow(a) + nrow(b))
  expect_true(all(c("case_id", "activity", "timestamp", "resource", "lifecycle") %in% names(e)))
  expect_false(is.unsorted(paste(e$case_id, e$timestamp)))
  expect_equal(nrow(export_event_log(a[0, ], b[0, ])), 0)
})

test_that("phase effort sums durations", {
  a <- build_activity_log(raw_events(), test_cfg()$event_map)
  pe <- phase_effort(a)
  expect_equal(pe$minutes[pe$process_phase == "2 Phase I - RICE"], 15 / 60)
  expect_equal(nrow(phase_effort(a[0, ])), 0)
})

test_that("browser batches are normalised", {
  js <- '[{"ts":"2026-01-01T10:00:00.123Z","name":"nav","value":"overview"},{"ts":"2026-01-01T10:00:01Z","name":"x"}]'
  n <- normalize_raw_batch(js, "s", "u", NULL)
  expect_equal(nrow(n), 2)
  expect_equal(n$ts[1], "2026-01-01 10:00:00")
  expect_true(is.na(n$raw_value[2]))
  expect_true(is.na(n$initiative_id[1]))
  expect_null(normalize_raw_batch(NULL, "s", "u"))
  expect_null(normalize_raw_batch("[]", "s", "u"))
})
