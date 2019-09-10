# !diagnostics off

library(shinydashboard)
library(shinyWidgets)
library(shiny)
library(leaflet)
library(dplyr)
library(httr)
library(curl) # make the jsonlite suggested dependency explicit
library(rgdal)
library(sp)
library(htmltools)
library(Hmisc)
library(RColorBrewer)
library(shinythemes)
library(readr)
library(rgeos)
library(hash)
library(shinycssloaders)
library(ulid)
library(here)
library(stringi)
library(DT)
library(auth0)
library(bs4Dash)
library(leaflet.extras)
library(leafgl)
library(sf)
library(shinyBS)
library(DBI)
library(RPostgres)
library(rpostgis)
library(vroom)
library(fontawesome)
library(data.table)

con <-
    DBI::dbConnect(
        RPostgres::Postgres(),
        user = "postgres",
        password = Sys.getenv("PG_PWD"),
        host = "127.0.0.1",
        port = 5432,
        dbname = "wsdot_evse_sp"
    )

# WA roads data
wa_roads <- readRDS(here::here("data-raw", "wa_roads.Rds"))

# Read the EVSE information through the AFDC API
afdc_url  <-
    paste0(
        "https://developer.nrel.gov/api/alt-fuel-stations/v1.csv?fuel_type=ELEC&state=WA&ev_charging_level=dc_fast&status=E&access=public&api_key=",
        Sys.getenv('AFDC_API_KEY')
    )
evse_dcfc <- vroom::vroom(afdc_url, delim = ",")
nevses <- nrow(evse_dcfc)
# TODO: Add a fallback clause in the case the API is non-responsive

evse_dcfc <-
    tibble::add_column(evse_dcfc,
                       EV_Connector_Code = 0,
                       ChargingCost = 0)
#ibble::add_column(evse_dcfc,)

# Convert the connector type to code for easy parsing in GAMA
# CHADEMO only - 1
# J1772COMBO only - 2
# CHADEMO and J1772COMBO - 3
# TESLA - 4
# Ignore J1772 as it is level-2
for (i in 1:nrow(evse_dcfc)) {
    conns <- evse_dcfc$`EV Connector Types`[i]
    if (grepl("CHADEMO", conns)) {
        if (grepl("J1772COMBO", conns)) {
            evse_dcfc$EV_Connector_Code[i] <- 3
        } else {
            evse_dcfc$EV_Connector_Code[i] <- 1
        }
    } else if (grepl("J1772COMBO", conns)) {
        evse_dcfc$EV_Connector_Code[i] <- 2
    } else if (grepl("TESLA", conns)) {
        evse_dcfc$EV_Connector_Code[i] <- 4
    }
}

all_chargers_combo <-
    evse_dcfc[evse_dcfc$EV_Connector_Code == 2 |
                  evse_dcfc$EV_Connector_Code == 3,]

all_chargers_chademo <-
    evse_dcfc[evse_dcfc$EV_Connector_Code == 1 |
                  evse_dcfc$EV_Connector_Code == 3,]

base_layers <- c("Combo", "CHAdeMO")

combo_icons <-
    icons(
        iconUrl = "https://upload.wikimedia.org/wikipedia/commons/7/7d/Symbol_electric_vehicle_charging_stations.jpg",
        iconWidth = 10,
        iconHeight = 10,
        iconAnchorX = 0,
        iconAnchorY = 0
    )
car_icons <- awesomeIcons(icon = "car", library = "fa")
wa_map <- leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
    setMaxBounds(-124.8361, 45.5437,-116.9174, 49.0024) %>%
    addProviderTiles("MapBox",
                     options = providerTileOptions(
                         id = "mapbox.light",
                         noWrap = FALSE,
                         accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN')
                     ))  %>%
    addGlPolylines(data = wa_roads, opacity = 1) %>%
    addResetMapButton() %>%
    addSearchOSM()

finished_files <-
    list.files(path = here::here("finished"), pattern = "*.csv")

# This function returns the simulated date and
# simulation run date time from the file name
finished_filename_parser <- function(fname) {
    date_part <- substr(fname, nchar(fname) - 33, nchar(fname) - 4)
    # print(date_part)
    parts <- strsplit(date_part, split = "_")
    simulated_date <- parts[[1]][1]
    simulation_runtime <- parts[[1]][2]
    return(c(simulated_date, simulation_runtime))
}

finished_parts <- lapply(finished_files, finished_filename_parser)
finished_simulated_dates <- lapply(finished_parts, `[[`, 1)
finished_simulation_runtimes <- lapply(finished_parts, `[[`, 2)

sim_df <-
    data.frame(
        sim_dates = unlist(finished_simulated_dates),
        sim_times = unlist(finished_simulation_runtimes),
        stringsAsFactors = FALSE
    )
