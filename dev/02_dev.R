# Building a Prod-Ready, Robust Shiny Application.
#
# README: each step of the dev files is optional, and you don't have to
# fill every dev scripts before getting started.
# 01_start.R should be filled at start.
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
#
#
###################################
#### CURRENT FILE: DEV SCRIPT #####
###################################

# Engineering

## Dependencies ----
## Amend DESCRIPTION with dependencies read from package code parsing
## install.packages('attachment') # if needed.
attachment::att_amend_desc()

## Add modules ----
## Create a module infrastructure in R/
golem::add_module(name = "home", with_test = FALSE) # Name of the module
golem::add_module(name = "uploadData", with_test = FALSE) # Name of the module
golem::add_module(name = "viewData", with_test = FALSE) # Name of the module
golem::add_module(name = "segmentation", with_test = FALSE) # Name of the module
golem::add_module(name = "network", with_test = FALSE) # Name of the module
golem::add_module(name = "identification", with_test = FALSE)
golem::add_module(name = "export", with_test = FALSE)
golem::add_module(name = "contact", with_test = FALSE) # Name of the module
golem::add_module(name = "name_of_module2", with_test = TRUE) # Name of the module
golem::add_module(name = "readRDS", with_test = FALSE)

## Add helper functions ----
## Creates fct_* and utils_*
golem::add_fct("helpers", with_test = TRUE)
golem::add_utils("helper", with_test = FALSE)
golem::add_fct("readMSI", with_test = FALSE)
golem::add_fct("getMeanSpec", with_test = FALSE)
golem::add_fct("plotMeanSpec", with_test = FALSE)
golem::add_fct("getRefPeaks", with_test = FALSE)
golem::add_fct("processMSIData", with_test = FALSE)
golem::add_fct("plotImage", with_test = FALSE)
golem::add_fct("plotPixelSpec", with_test = FALSE)
golem::add_fct("plotPCASpec", with_test = FALSE)
golem::add_fct("text2Num", with_test = FALSE)
golem::add_fct("plotSSCSpec", with_test = FALSE)
golem::add_fct("colocAnalysis", with_test = FALSE)
golem::add_fct("deisotoping", with_test = FALSE)
golem::add_fct("removeNoise", with_test = FALSE)
golem::add_fct("subsetMSIData", with_test = FALSE)
golem::add_fct("getROI", with_test = FALSE)
golem::add_fct("makeFactor2", with_test = FALSE)
golem::add_fct("combine2", with_test = FALSE)
golem::add_fct("roiStat", with_test = FALSE)
golem::add_fct("plotROIProfile", with_test = FALSE)
golem::add_fct("roiQuantification", with_test = FALSE)
golem::add_fct("plotCalCurve", with_test = FALSE)
golem::add_fct("getQuan", with_test = FALSE)
golem::add_fct("getPCA", with_test = FALSE)
golem::add_fct("getSSC", with_test = FALSE)
golem::add_fct("getSSCClusters", with_test = FALSE)
golem::add_fct("getPCC", with_test = FALSE)
golem::add_fct("plotPCAImage", with_test = FALSE)
golem::add_fct("plotSSCImage", with_test = FALSE)
golem::add_fct("plotAllNetwork", with_test = FALSE)
golem::add_fct("plotSingleNetwork", with_test = FALSE)
golem::add_fct("plotMSMS", with_test = FALSE)
golem::add_fct("cropData", with_test = FALSE)

## External resources
## Creates .js and .css files at inst/app/www
golem::add_js_file("script")
golem::add_js_handler("handlers")
golem::add_css_file("custom")
golem::add_sass_file("custom")

## Add internal datasets ----
## If you have data in your package
# usethis::use_data_raw(name = "my_dataset", open = FALSE)

## Tests ----
## Add one line by test you want to create
# usethis::use_test("app")

# Documentation

## Vignette ----
# usethis::use_vignette("ShinyCardinal")
# devtools::build_vignettes()

## Code Coverage----
## Set the code coverage service ("codecov" or "coveralls")
# usethis::use_coverage()

# Create a summary readme for the testthat subdirectory
#covrpage::covrpage()

## CI ----
## Use this part of the script if you need to set up a CI
## service for your application
##
## (You'll need GitHub there)
# usethis::use_github()

# GitHub Actions
# usethis::use_github_action()
# Chose one of the three
# See https://usethis.r-lib.org/reference/use_github_action.html
# usethis::use_github_action_check_release()
# usethis::use_github_action_check_standard()
# usethis::use_github_action_check_full()
# Add action for PR
# usethis::use_github_action_pr_commands()

# Travis CI
# usethis::use_travis()
# usethis::use_travis_badge()

# AppVeyor
# usethis::use_appveyor()
# usethis::use_appveyor_badge()

# Circle CI
# usethis::use_circleci()
# usethis::use_circleci_badge()

# Jenkins
# usethis::use_jenkins()

# GitLab CI
# usethis::use_gitlab_ci()

# You're now set! ----
# go to dev/03_deploy.R
# rstudioapi::navigateToFile("dev/03_deploy.R")
