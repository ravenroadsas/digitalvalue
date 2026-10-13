# Entry point for Posit Connect / shiny::runApp(). See docs/DEPLOY.md.
# Deploy with: rsconnect::deployApp(appFiles = c("app.R", "DESCRIPTION", "NAMESPACE", "R", "inst"))

# Packages used conditionally (pins configuration on Connect): referenced here
# so that rsconnect includes them in the deployment manifest.
if (FALSE) {
  library(pins)
}
pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)
digitalvalue::run_app()
