# Launch the ShinyApp (Do not remove this comment)
# To deploy, run: rsconnect::deployApp()
# Or use the blue button on top of this file
#options(rsconnect.max.bundle.size=5368709120)
#options(repos = BiocManager::repositories())
pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE) # use it when I need to deploy
options( "golem.app.prod" = TRUE)
ShinyCardinal::run_app() # add parameters here (if any)
