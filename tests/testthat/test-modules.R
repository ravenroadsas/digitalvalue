# Server-logic tests of the main workflow without a browser
make_state <- function(con, cfg) {
  version <- shiny::reactiveVal(0)
  selected <- shiny::reactiveVal(NULL)
  portfolio <- shiny::reactive({ version(); compute_portfolio(db_portfolio(con), cfg) })
  list(con = con, cfg = cfg, user = list(user = "tester", is_planning = TRUE),
       portfolio = portfolio, history = shiny::reactive({ version(); db_get_status_history(con) }),
       selected = selected, version = version,
       refresh = function() { sync_status(con, cfg, "tester"); version(shiny::isolate(version()) + 1) },
       goto = function(tab) invisible(tab),
       row = shiny::reactive({
         id <- selected(); pf <- portfolio()
         if (is.null(id) || !id %in% pf$id) NULL else pf[pf$id == id, , drop = FALSE]
       }))
}

test_that("register -> RICE -> PRMT -> review moves the initiative through the gates", {
  cfg <- test_cfg()
  con <- test_con()
  state <- make_state(con, cfg)

  shiny::testServer(mod_register_server, args = list(state = state), {
    session$setInputs(name = "Test initiative", description = "d", owner = "o",
                      business_unit = "bu", cost_mm_usd = 2, start_date = NA, end_date = NA)
    session$setInputs(save_new = 1)
  })
  shiny::isolate(expect_equal(state$selected(), "DV-0001"))

  shiny::testServer(mod_phase1_server, args = list(state = state), {
    session$setInputs(users = 1000, impact = "L", confidence = "High", effort = "L", rationale = "r")
    session$setInputs(save = 1)
  })
  expect_equal(db_get_initiatives(con, "DV-0001")$status, "Phase II")

  shiny::testServer(mod_prmt_editor_server, args = list(state = state, stage = "plan"), {
    session$setInputs(metric = "P", method = "p_jobs_success", p_n_jobs = 10, p_bopd_per_job = 30,
                      p_success_before = 60, p_success_after = 70, comment = "workovers")
    expect_equal(calc()$value, 30)
    session$setInputs(add = 1)
  })
  l <- db_get_prmt_lines(con, "DV-0001", "plan")
  expect_equal(l$comment, "workovers")
  expect_equal(db_get_initiatives(con, "DV-0001")$status, "Phase III")

  shiny::testServer(mod_phase3_server, args = list(state = state), {
    session$setInputs(decision = "Approve", validated_value = 0.4, validated_cost = 2, comment = "ok")
    session$setInputs(save = 1)
  })
  expect_equal(db_get_initiatives(con, "DV-0001")$status, "Ready")
})

test_that("planning-only actions are blocked for other users", {
  cfg <- test_cfg()
  con <- test_con()
  seed_demo_data(con, cfg)
  state <- make_state(con, cfg)
  state$user$is_planning <- FALSE
  id <- db_get_initiatives(con)$id[db_get_initiatives(con)$status == "Phase III"][1]
  shiny::isolate(state$selected(id))
  n <- nrow(db_get_reviews(con))
  shiny::testServer(mod_phase3_server, args = list(state = state), {
    session$setInputs(decision = "Approve", validated_value = 1, validated_cost = 1, comment = "")
    session$setInputs(save = 1)
  })
  expect_equal(nrow(db_get_reviews(con)), n)
})
