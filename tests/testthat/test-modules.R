# Server-logic tests of the lifecycle without a browser
make_state <- function(con, cfg, role = "superuser") {
  version <- shiny::reactiveVal(0)
  selected <- shiny::reactiveVal(NULL)
  portfolio <- shiny::reactive({ version(); compute_portfolio(db_portfolio(con), cfg) })
  usr <- list(user = paste0("a-", role), role = role)
  list(con = con, cfg = cfg, user = usr, can = function(action) can(usr, action),
       portfolio = portfolio, history = shiny::reactive({ version(); db_get_status_history(con) }),
       models = shiny::reactive({ version(); list(
         rice_to_review = db_active_value_model(con, "rice_to_review"),
         review_to_audit = db_active_value_model(con, "review_to_audit")) }),
       selected = selected, stage = shiny::reactiveVal("appraisal"), version = version,
       refresh = function() { sync_status(con, cfg, usr$user); version(shiny::isolate(version()) + 1) },
       open = function(id, view = NULL) selected(id),
       row = shiny::reactive({
         id <- selected(); pf <- portfolio()
         if (is.null(id) || !id %in% pf$id) NULL else pf[pf$id == id, , drop = FALSE]
       }))
}
reg_inputs <- function(session) {
  session$setInputs(name = "Test initiative", description = "d", owner = "o", business_unit = "bu",
                    cost_mm_usd = 2, start_date = NA, end_date = NA, users = 1000, impact = "L",
                    confidence = "High", effort = "L", rationale = "r")
}

test_that("full lifecycle: register -> 4M -> review -> decision -> audit", {
  cfg <- test_cfg()
  con <- test_con()
  state <- make_state(con, cfg)

  shiny::testServer(mod_register_server, args = list(state = state), {
    reg_inputs(session)
    session$setInputs(save = 1)
  })
  shiny::isolate(expect_equal(state$selected(), "DV-0001"))
  expect_equal(db_get_initiatives(con, "DV-0001")$status, "4M valuation")
  expect_equal(db_get_rice(con)$effort, "L")

  shiny::testServer(mod_valuation_server, args = list(state = state), {
    session$setInputs(metric = "P", method = "p_jobs_success", p_n_jobs = 10, p_bopd_per_job = 30,
                      p_success_before = 60, p_success_after = 70, comment = "workovers")
    expect_equal(calc()$value, 30)
    session$setInputs(add = 1)
  })
  expect_equal(db_get_m4_lines(con, "DV-0001")$comment, "workovers")
  expect_equal(db_get_initiatives(con, "DV-0001")$status, "Expert review")

  shiny::testServer(mod_review_server, args = list(state = state), {
    session$setInputs(decision = "Approve", P = 25, R = 0, M = 0.1, T = 0, category = "2P",
                      cost = 2, comment = "ok")
    session$setInputs(save = 1)
  })
  rv <- db_get_reviews(con, "DV-0001")
  expect_equal(rv$p_bopd, 25)
  expect_equal(db_get_initiatives(con, "DV-0001")$status, "Ready")

  for (s in c("Prioritized", "In execution", "Closed")) db_set_status(con, "DV-0001", s, "boss")
  state$refresh()
  shiny::testServer(mod_audit_server, args = list(state = state), {
    session$setInputs(P = 20, R = 0, M = 0.05, T = 0, category = "2P", adoption = 70, comment = "done")
    session$setInputs(save = 1)
  })
  au <- db_get_audits(con, "DV-0001")
  expect_equal(au$adoption_pct, 70)
  expect_equal(au$expected_value_mm_usd, rv$value_mm_usd)
  expect_equal(db_get_initiatives(con, "DV-0001")$status, "Audited")
})

test_that("contributors register once; edits and later steps need a superuser", {
  cfg <- test_cfg()
  con <- test_con()
  state <- make_state(con, cfg, "contributor")
  shiny::testServer(mod_register_server, args = list(state = state), {
    reg_inputs(session)
    session$setInputs(save = 1)
    expect_false(editable())
    session$setInputs(name = "Changed", update = 1)
  })
  expect_equal(db_get_initiatives(con, "DV-0001")$name, "Test initiative")
  expect_equal(db_get_initiatives(con, "DV-0001")$created_by, "a-contributor")

  shiny::testServer(mod_valuation_server, args = list(state = state), {
    session$setInputs(metric = "M", method = "m_direct", p_mm_usd = 3, comment = "")
    session$setInputs(add = 1)
  })
  expect_equal(nrow(db_get_m4_lines(con)), 0)

  shiny::testServer(mod_review_server, args = list(state = state), {
    session$setInputs(decision = "Approve", P = 1, R = 0, M = 0, T = 0, category = "2P", cost = 1,
                      comment = "")
    session$setInputs(save = 1)
  })
  expect_equal(nrow(db_get_reviews(con)), 0)
})

test_that("superusers can edit a registration and its RICE", {
  cfg <- test_cfg()
  con <- test_con()
  id <- reg(con, cfg, "Original")
  state <- make_state(con, cfg)
  shiny::isolate(state$selected(id))
  shiny::testServer(mod_register_server, args = list(state = state), {
    reg_inputs(session)
    session$setInputs(name = "Edited", users = 50, update = 1)
  })
  expect_equal(db_get_initiatives(con, id)$name, "Edited")
  expect_equal(db_get_rice(con)$users, 50)
})

test_that("publishing a candidate model requires a superuser", {
  cfg <- test_cfg()
  con <- test_con()
  seed_demo_data(con, cfg)
  n0 <- nrow(db_get_value_models(con, "rice_to_review"))
  state <- make_state(con, cfg)
  shiny::testServer(mod_models_server, args = list(state = state), {
    session$setInputs(rice_to_review_comment = "recal", rice_to_review_publish = 1)
  })
  v <- db_get_value_models(con, "rice_to_review")
  expect_equal(nrow(v), n0 + 1)
  expect_equal(v$comment[v$active == 1], "recal")
  st2 <- make_state(con, cfg, "contributor")
  shiny::testServer(mod_models_server, args = list(state = st2), {
    session$setInputs(rice_to_review_comment = "x", rice_to_review_publish = 1)
  })
  expect_equal(nrow(db_get_value_models(con, "rice_to_review")), n0 + 1)
})
