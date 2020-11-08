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
## Add one line by package you want to add as dependency
usethis::use_package( "thinkr" )
usethis::use_package( "shinyWidgets" )
usethis::use_package( "shiny" )
usethis::use_package( "leaflet" )
usethis::use_package( "dplyr" )
usethis::use_package( "httr" )
usethis::use_package( "curl" )
usethis::use_package( "rgdal" )
usethis::use_package( "sp" )
usethis::use_package( "htmltools" )
usethis::use_package("RColorBrewer")
usethis::use_package("readr")
usethis::use_package("rgeos")
usethis::use_package("hash")
usethis::use_package("shinycssloaders")
usethis::use_package("here")
usethis::use_package("stringi")
usethis::use_package("DT")
usethis::use_package("auth0")
usethis::use_package("bs4Dash")
usethis::use_package("leaflet.extras")
# usethis::use_package("leafgl")
usethis::use_package("sf")
usethis::use_package("shinyBS")
usethis::use_package("DBI")
usethis::use_package("RPostgres")
# usethis::use_package("fontawesome")
usethis::use_package("data.table")
usethis::use_package("dbplyr")
usethis::use_package("pool")

usethis::use_package("leaflet.mapboxgl")
usethis::use_package("magrittr")
usethis::use_package("plotly")
usethis::use_package("leafem")


## Add modules ----
## Create a module infrastructure in R/
golem::add_module( name = "summary_stats" ) # Name of the module
golem::add_module( name = "bevs" ) # Name of the module
golem::add_module( name = "evses" ) # Name of the module

## Add helper functions ----
## Creates ftc_* and utils_*
golem::add_fct( "helpers" ) 
golem::add_utils( "helpers" )

## External resources
## Creates .js and .css files at inst/app/www
golem::add_js_file( "script" )
golem::add_js_handler( "handlers" )
golem::add_css_file( "custom" )

## Add internal datasets ----
## If you have data in your package
usethis::use_data_raw( name = "wa_roads", open = FALSE ) 

## Tests ----
## Add one line by test you want to create
usethis::use_test( "app" )

# Documentation

## Vignette ----
usethis::use_vignette("resview")
devtools::build_vignettes()

## Code coverage ----
## (You'll need GitHub there)
usethis::use_github()
usethis::use_travis()
usethis::use_appveyor()

# You're now set! ----
# go to dev/03_deploy.R
rstudioapi::navigateToFile("dev/03_deploy.R")

