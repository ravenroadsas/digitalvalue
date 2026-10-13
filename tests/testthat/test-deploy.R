test_that("roles table: validation and superuser resolution", {
  withr::local_envvar(DV_SUPERUSER_GROUP = "", DV_SUPERUSERS = "", DV_DEV_ROLE = "",
                      RSTUDIO_PRODUCT = "")
  roles <- data.frame(type = c("user", "Group"), principal = c("cplan ", "dv-admins"),
                      role = c("superuser", "SUPERUSER"))
  r <- normalise_roles(roles)
  expect_equal(r$type, c("user", "group"))
  expect_equal(r$principal, c("cplan", "dv-admins"))
  expect_equal(user_role("cplan", roles = r), "superuser")
  expect_equal(user_role("ana", "dv-admins", roles = r), "superuser")
  expect_equal(user_role("ana", "eng", roles = r), "contributor")
  expect_error(normalise_roles(data.frame(type = "team", principal = "x", role = "superuser")),
               "type must be")
  expect_error(normalise_roles(data.frame(type = "user", principal = "x")), "missing column")
  expect_equal(nrow(normalise_roles(NULL)), 0)
  # environment variables are merged with the table
  withr::local_envvar(DV_SUPERUSERS = "dan")
  expect_setequal(superusers(r)$users, c("cplan", "dan"))
})

test_that("no superuser configured: everyone locally, nobody on Posit Connect", {
  withr::local_envvar(DV_SUPERUSER_GROUP = "", DV_SUPERUSERS = "", DV_DEV_ROLE = "",
                      RSTUDIO_PRODUCT = "")
  expect_equal(user_role("bob", roles = NULL), "superuser")
  withr::local_envvar(RSTUDIO_PRODUCT = "CONNECT")
  expect_true(on_connect())
  expect_equal(user_role("bob", roles = NULL), "contributor")
})

test_that("roles are read from roles.csv and from a pins board", {
  cfg <- test_cfg()
  expect_named(cfg$roles, c("type", "principal", "role", "comment"))
  f <- withr::local_tempfile(fileext = ".csv")
  utils::write.csv(data.frame(type = "group", principal = "dv-admins", role = "superuser",
                              comment = "IT ticket 123"), f, row.names = FALSE)
  expect_equal(read_roles(f)$principal, "dv-admins")
  expect_equal(nrow(read_roles(tempfile())), 0)
  skip_if_not_installed("pins")
  board <- pins::board_temp()
  withr::local_envvar(DV_PINS_OWNER = "")
  expect_warning(r <- read_roles(board = board), "dv_roles")
  expect_equal(nrow(r), 0)
  suppressMessages(pins::pin_write(board, data.frame(type = "user", principal = "cplan",
                                                     role = "superuser"), "dv_roles", type = "csv"))
  expect_equal(read_roles(board = board)$principal, "cplan")
})

test_that("configuration pins round trip, including dv_roles", {
  skip_if_not_installed("pins")
  dir <- withr::local_tempdir()
  withr::local_envvar(DV_PINS_BOARD = dir, DV_PINS_OWNER = "")
  suppressMessages(written <- publish_config_pins())
  expect_true("dv_roles" %in% written)
  cfg <- load_config()
  expect_equal(cfg$source, "pins")
  expect_equal(nrow(cfg$roles), 0)
  expect_equal(cfg$params$netback_usd_bbl, test_cfg()$params$netback_usd_bbl)
})

test_that("daily backup is written once and pruned", {
  cfg <- test_cfg()
  con <- test_con()
  reg(con, cfg, "Backup me")
  dir <- withr::local_tempdir()
  for (d in 0:3) db_backup(con, dir, keep = 2, date = as.Date("2026-10-01") + d)
  expect_equal(list.files(dir), c("2026-10-03", "2026-10-04"))
  expect_null(db_backup(con, dir, keep = 2, date = as.Date("2026-10-04")))
  expect_null(db_backup(con, "", keep = 2))
  skip_if_not(inherits(con, "duckdb_connection"))
  con2 <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con2, shutdown = TRUE), add = TRUE)
  DBI::dbExecute(con2, sprintf("IMPORT DATABASE '%s'", file.path(dir, "2026-10-04")))
  expect_equal(DBI::dbGetQuery(con2, "SELECT name FROM initiatives")$name, "Backup me")
})

test_that("sessions share one connection that is released when the last one ends", {
  path <- withr::local_tempfile(fileext = ".duckdb")
  withr::local_envvar(DV_DB_PATH = path, DV_DB_DRIVER = test_driver(), DV_BACKUP_DIR = "")
  old <- as.list(.dv)
  withr::defer({ db_release(); .dv$sessions <- 0L })
  .dv$con <- NULL; .dv$sessions <- 0L
  a <- db_acquire(); b <- db_acquire()
  expect_identical(a, b)
  db_release(force = FALSE)
  expect_false(is.null(.dv$con))
  db_release(force = FALSE)
  expect_null(.dv$con)
  expect_true(DBI::dbIsValid(c <- db_acquire()))
})

test_that("role changes are picked up without restarting the app", {
  withr::local_envvar(DV_SUPERUSER_GROUP = "", DV_SUPERUSERS = "", DV_DEV_ROLE = "",
                      RSTUDIO_PRODUCT = "CONNECT")
  dir <- withr::local_tempdir()
  file.copy(list.files(config_dir(), full.names = TRUE), dir)
  withr::local_envvar(DV_CONFIG_DIR = dir)
  old <- list(cfg = .dv$cfg, at = .dv$roles_at)
  withr::defer({ .dv$cfg <- old$cfg; .dv$roles_at <- old$at })
  .dv$cfg <- load_config("csv", dir); .dv$roles_at <- Sys.time()
  expect_equal(user_role("cplan"), "contributor")
  utils::write.csv(data.frame(type = "user", principal = "cplan", role = "superuser"),
                   file.path(dir, "roles.csv"), row.names = FALSE)
  expect_equal(user_role("cplan"), "contributor")          # cached
  expect_equal(user_role("cplan", roles = current_roles(ttl = 0)), "superuser")
  expect_equal(user_role("cplan"), "superuser")
})
