# Server-logic tests of the lifecycle without a browser
make_state <- function(con, cfg, role = "superuser", user = paste0("a-", role)) {
  version <- shiny::reactiveVal(0)
  selected <- shiny::reactiveVal(NULL)
  portfolio <- shiny::reactive({ version(); compute_portfolio(db_portfolio(con), cfg) })
  usr <- list(user = user, role = role)
  list(con = con, cfg = cfg, user = usr, can = function(action, row = NULL) can(usr, action, row),
       users = shiny::reactive(local_users()),
       portfolio = portfolio, history = shiny::reactive({ version(); db_get_status_history(con) }),
       models = shiny::reactive({ version(); list(
         rice_to_review = db_active_value_model(con, "rice_to_review"),
         review_to_audit = db_active_value_model(con, "review_to_audit")) }),
       selected = selected, stage = shiny::reactiveVal("evaluation"), version = version,
       refresh = function() { sync_status(con, cfg, usr$user); version(shiny::isolate(version()) + 1) },
       open = function(id, view = NULL) selected(id),
       row = shiny::reactive({
         id <- selected(); pf <- portfolio()
         if (is.null(id) || !id %in% pf$id) NULL else pf[pf$id == id, , drop = FALSE]
       }))
}
reg_inputs <- function(session, owner = "aruiz") {
  session$setInputs(name = "Test initiative", description = "d", owner = owner, business_unit = "bu",
                    start_date = NA, end_date = NA, users = 1000, impact = "L",
                    confidence = "High", effort = "L", rationale = "r")
}
status <- function(con, id = "DV-0001") db_get_initiatives(con, id)$status

test_that("full lifecycle: register -> 4MC -> validation -> review -> delivery -> audit", {
  cfg <- test_cfg()
  con <- test_con()
  owner <- make_state(con, cfg, "contributor", "aruiz")

  shiny::testServer(mod_register_server, args = list(state = owner), {
    reg_inputs(session)
    session$setInputs(save = 1)
  })
  shiny::isolate(expect_equal(owner$selected(), "DV-0001"))
  expect_equal(status(con), "Recorded")
  expect_equal(db_get_initiatives(con, "DV-0001")$owner, "aruiz")
  expect_true(is.na(db_get_initiatives(con, "DV-0001")$cost_mm_usd))

  # the owner records the 4MC (value + cost) and sends it to a validator
  shiny::testServer(mod_valuation_server, args = list(state = owner), {
    session$setInputs(metric = "P", method = "p_jobs_success", p_n_jobs = 10, p_bopd_per_job = 30,
                      p_success_before = 60, p_success_after = 70, comment = "workovers")
    expect_equal(calc()$value, 30)
    session$setInputs(add = 1)
    session$setInputs(metric = "C", method = "c_direct", p_capex_mm_usd = 1.5, p_opex_mm_usd_yr = 0,
                      p_years = 1, comment = "build")
    session$setInputs(add = 2)
    session$setInputs(validator = "dval", request_comment = "please check", send = 1)
    expect_false(editable())                       # locked while under validation
    session$setInputs(metric = "M", method = "m_direct", p_mm_usd = 9, comment = "")
    session$setInputs(add = 3)
  })
  expect_equal(nrow(db_get_m4_lines(con, "DV-0001")), 2)
  expect_equal(db_get_validations(con, "DV-0001")$status, "pending")
  expect_equal(status(con), "Recorded")

  # the validator requests changes, the owner fixes and resends, the validator validates
  validator <- make_state(con, cfg, "contributor", "dval")
  shiny::isolate(validator$selected("DV-0001"))
  shiny::testServer(mod_valuation_server, args = list(state = validator), {
    session$setInputs(decision_comment = "", request_changes = 1)   # comment is required
    session$setInputs(decision_comment = "use 2024 rates", request_changes = 2)
  })
  expect_equal(db_get_validations(con, "DV-0001")$status[1], "changes_requested")
  bump <- function(st) shiny::isolate(st$version(st$version() + 1))  # what the change poll does
  bump(owner)
  shiny::testServer(mod_valuation_server, args = list(state = owner), {
    expect_true(editable())
    session$setInputs(validator = "dval", request_comment = "updated", send = 1)
  })
  bump(validator)
  shiny::testServer(mod_valuation_server, args = list(state = validator), {
    session$setInputs(decision_comment = "ok", validate = 1)
  })
  expect_equal(db_get_validations(con, "DV-0001")$status[1], "validated")
  expect_equal(status(con), "Recorded")             # cost 1.5 >= 1: expert review required

  boss <- make_state(con, cfg)
  expect_match(db_change_stamp(con), "\\|")
  shiny::isolate(boss$selected("DV-0001"))
  shiny::testServer(mod_review_server, args = list(state = boss), {
    session$setInputs(decision = "Approve", P = 25, R = 0, M = 0.1, T = 0, C = 1.6,
                      category = "2P", comment = "ok")
    session$setInputs(save = 1)
  })
  expect_equal(db_get_reviews(con, "DV-0001")$cost_mm_usd, 1.6)
  expect_equal(status(con), "Evaluated")

  # the owner delivers
  bump(owner)
  shiny::testServer(mod_initiative_server, args = list(state = owner), {
    session$setInputs(decision_comment = "live")   # first flush (the button observer ignores init)
    session$setInputs(act_deliver = 1)
  })
  expect_equal(status(con), "Delivered")

  bump(boss)
  shiny::testServer(mod_audit_server, args = list(state = boss), {
    session$setInputs(P = 20, R = 0, M = 0.05, T = 0, C = 1.8, category = "2P", adoption = 70,
                      comment = "done")
    session$setInputs(save = 1)
  })
  expect_equal(db_get_audits(con, "DV-0001")$adoption_pct, 70)
  expect_equal(status(con), "Audited")
})

test_that("contributors register once; others cannot valuate, validate or deliver", {
  cfg <- test_cfg()
  con <- test_con()
  state <- make_state(con, cfg, "contributor", "aruiz")
  shiny::testServer(mod_register_server, args = list(state = state), {
    reg_inputs(session)
    session$setInputs(save = 1)
    expect_false(editable())
    session$setInputs(name = "Changed", update = 1)
  })
  expect_equal(db_get_initiatives(con, "DV-0001")$name, "Test initiative")

  other <- make_state(con, cfg, "contributor", "bob")
  shiny::isolate(other$selected("DV-0001"))
  shiny::testServer(mod_valuation_server, args = list(state = other), {
    expect_false(editable())
    session$setInputs(metric = "M", method = "m_direct", p_mm_usd = 3, comment = "")
    session$setInputs(add = 1)
  })
  expect_equal(nrow(db_get_m4_lines(con)), 0)
  shiny::testServer(mod_review_server, args = list(state = other), {
    session$setInputs(decision = "Approve", P = 1, R = 0, M = 0, T = 0, C = 0, category = "2P", comment = "")
    session$setInputs(save = 1)
  })
  expect_equal(nrow(db_get_reviews(con)), 0)

  # a validator who was not asked cannot validate
  db_add_m4_line(con, "DV-0001", m4_calculate("m_direct", list(mm_usd = 1), cfg))
  db_request_validation(con, "DV-0001", "dval", "aruiz")
  shiny::testServer(mod_valuation_server, args = list(state = other), {
    session$setInputs(decision_comment = "x", validate = 1)
  })
  expect_equal(db_get_validations(con, "DV-0001")$status, "pending")
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

test_that("clicking the portfolio register highlights instead of navigating", {
  cfg <- test_cfg()
  con <- test_con()
  seed_demo_data(con, cfg)
  state <- make_state(con, cfg)
  shiny::testServer(mod_portfolio_server, args = list(state = state), {
    session$setInputs(table_rows_selected = 3)
    expect_equal(highlight(), table_data()$ID[3])
    session$setInputs(highlight = "DV-0005")
    expect_equal(highlight(), "DV-0005")
    shiny::isolate(expect_null(state$selected()))
    session$setInputs(open = 1)
    shiny::isolate(expect_equal(state$selected(), "DV-0005"))
  })
})
