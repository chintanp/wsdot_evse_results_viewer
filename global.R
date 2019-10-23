# !diagnostics off
# utility for checking is something is already installed, then loading.
usePackage <- function(p) {
    if (!is.element(p, installed.packages()[, 1])) {
        install.packages(p, dep = TRUE, repos = "http://cran.us.r-project.org")
    }
    require(p, character.only = TRUE)
}


usePackage("shinydashboard")
usePackage("shinyWidgets")
usePackage("shiny")
usePackage("leaflet")
usePackage("dplyr")
usePackage("httr")
usePackage("curl") # make the jsonlite suggested dependency explicit
usePackage("rgdal")
usePackage("sp")
usePackage("htmltools")
usePackage("Hmisc")
usePackage("RColorBrewer")
usePackage("shinythemes")
usePackage("readr")
usePackage("rgeos")
usePackage("hash")
usePackage("shinycssloaders")
usePackage("ulid")
usePackage("here")
usePackage("stringi")
usePackage("DT")
usePackage("auth0")
usePackage("bs4Dash")
usePackage("leaflet.extras")
# usePackage("leafgl")
usePackage("sf")
usePackage("shinyBS")
usePackage("DBI")
usePackage("RPostgres")
usePackage("rpostgis")
usePackage("vroom")
# usePackage("fontawesome")
usePackage("data.table")
usePackage("dbplyr")
usePackage("pool")

# Connection to the databse
# main_con <-
#     DBI::dbConnect(
#         RPostgres::Postgres(),
#         user = Sys.getenv("MAIN_USER"),
#         password = Sys.getenv("MAIN_PWD"),
#         host = Sys.getenv("MAIN_HOST"),
#         port = 5432,
#         dbname = Sys.getenv("MAIN_DB")
#     )

pool <- pool::dbPool(
    drv = RPostgres::Postgres(),
    dbname = Sys.getenv("MAIN_DB"),
    host = Sys.getenv("MAIN_HOST"),
    user = Sys.getenv("MAIN_USER"),
    password = Sys.getenv("MAIN_PWD"), 
    port = Sys.getenv("MAIN_PORT")
)
onStop(function() {
    poolClose(pool)
})

userid <- NULL
# WA roads data
wa_roads <- readRDS(here::here("data-raw", "wa_roads.Rds"))

# bevses_db <- dplyr::tbl(main_con, "built_evse")
bevses_db <- pool %>% dplyr::tbl( "built_evse")

bevse_dcfc <-
    bevses_db %>% dplyr::select(
        dcfc_count,
        latitude,
        longitude,
        bevse_id,
        connector_code
    ) %>% dplyr::filter(dcfc_count > 0) %>% dplyr::mutate(evse_id = paste0("b", bevse_id)) %>% dplyr::select(-c(bevse_id)) %>% collect()

# nevses <- nrow(evse_dcfc)
# TODO: Add a fallback clause in the case the API is non-responsive

# all_chargers_combo <-
#     evse_dcfc[evse_dcfc$EV_Connector_Code == 2 |
#                   evse_dcfc$EV_Connector_Code == 3, ]
# 
# all_chargers_chademo <-
#     evse_dcfc[evse_dcfc$EV_Connector_Code == 1 |
#                   evse_dcfc$EV_Connector_Code == 3, ]

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
    setMaxBounds(-124.8361, 45.5437, -116.9174, 49.0024) %>%
    addProviderTiles("MapBox",
                     options = providerTileOptions(
                         id = "mapbox.light",
                         noWrap = FALSE,
                         accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN')
                     ))  %>%
    # addPolylines(data = wa_roads, opacity = 1, weight = 2) %>%
    addResetMapButton() %>%
    addSearchOSM()

simulated_date <- "2019-07-01"
# finished_files <-
#     list.files(path = here::here("finished"), pattern = "*.csv")
# 
# # This function returns the simulated date and
# # simulation run date time from the file name
# finished_filename_parser <- function(fname) {
#     date_part <- substr(fname, nchar(fname) - 33, nchar(fname) - 4)
#     # print(date_part)
#     parts <- strsplit(date_part, split = "_")
#     simulated_date <- parts[[1]][1]
#     simulation_runtime <- parts[[1]][2]
#     return(c(simulated_date, simulation_runtime))
# }
# 
# finished_parts <- lapply(finished_files, finished_filename_parser)
# finished_simulated_dates <- lapply(finished_parts, `[[`, 1)
# finished_simulation_runtimes <- lapply(finished_parts, `[[`, 2)
# 
# sim_df <-
#     data.frame(
#         sim_dates = unlist(finished_simulated_dates),
#         sim_times = unlist(finished_simulation_runtimes),
#         stringsAsFactors = FALSE
#     )
