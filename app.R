# Entry point for Posit Connect / shiny::runApp().
# Deploy with: rsconnect::deployApp(appFiles = c("app.R", "DESCRIPTION", "NAMESPACE", "R", "inst"))
pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)
digitalvalue::run_app()
