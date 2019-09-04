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
                  evse_dcfc$EV_Connector_Code == 3, ]

all_chargers_chademo <-
    evse_dcfc[evse_dcfc$EV_Connector_Code == 1 |
                  evse_dcfc$EV_Connector_Code == 3, ]

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

###################
### UI ###########
####################

finished_tab <- bs4TabItem(tabName = "finished",
                           fluidRow(column(
                               width = 9,
                               bs4Card(
                                   title = "Finished Details",
                                   closable = FALSE,
                                   status = "primary",
                                   collapsible = TRUE,
                                   labelText = 1,
                                   labelStatus = "primary",
                                   labelTooltip = "Finished Details",
                                   elevation = 4,
                                   width = NULL,
                                   solidHeader = TRUE,
                                   withSpinner(
                                       leafglOutput("wa_fin_mapout", height = 700),
                                       type = 8,
                                       color = "#0dc5c1"
                                   )
                               )
                           ), column(
                               width = 3,
                               bs4Card(
                                   width = 12,
                                   title = "Origin and Destination Selector",
                                   closable = FALSE,
                                   tags$div(
                                       id = "selectODDiv_fin",
                                       width = 12,
                                       tags$div(
                                           style = "display: inline-block;vertical-align:top; width: 250px;",
                                           selectInput(
                                               inputId = "select_origin_fin",
                                               label = "Select origin zip",
                                               choices = "",
                                               selected = NULL,
                                               multiple = FALSE,
                                               selectize = TRUE,
                                               width = NULL,
                                               size = NULL
                                           )
                                       ),
                                       tags$div(style = "display: inline-block;vertical-align:top; width: 50px;", HTML("<br>")),
                                       tags$div(
                                           style = "display: inline-block;vertical-align:top; width: 250px;",
                                           selectInput(
                                               inputId = "select_destination_fin",
                                               label = "Select destination zip",
                                               choices = "",
                                               selected = NULL,
                                               multiple = FALSE,
                                               selectize = TRUE,
                                               width = NULL,
                                               size = NULL
                                           )
                                       )
                                   )
                               )
                           )))

evse_util_tab <- bs4TabItem(tabName = "evse_util",
                            fluidRow(column(
                                width = 9,
                                bs4Card(
                                    title = "EVSE Utilization Details",
                                    closable = FALSE,
                                    status = "success",
                                    collapsible = TRUE,
                                    elevation = 4,
                                    width = NULL,
                                    solidHeader = TRUE,
                                    withSpinner(
                                        leafglOutput("wa_evse_mapout", height = 700),
                                        type = 8,
                                        color = "#0dc5c1"
                                    )
                                )
                            )))

out_of_charge_tab <- bs4TabItem(tabName = "out_of_charge",
                                fluidRow(column(
                                    width = 9,
                                    bs4Card(
                                        title = "Out of Charge EV Details",
                                        closable = FALSE,
                                        status = "danger",
                                        collapsible = TRUE,
                                        labelTooltip = "Out of Charge EV Detail",
                                        elevation = 4,
                                        width = NULL,
                                        solidHeader = TRUE,
                                        withSpinner(
                                            leafglOutput("wa_ooc_mapout", height = 700),
                                            type = 8,
                                            color = "#0dc5c1"
                                        )
                                    )
                                ), column(
                                    width = 3,
                                    bs4Card(
                                        width = 12,
                                        title = "Origin and Destination Selector",
                                        closable = FALSE,
                                        tags$div(
                                            style = "display: inline-block;vertical-align:top; width: 250px;",
                                            selectInput(
                                                inputId = "select_origin_ooc",
                                                label = "Select origin zip",
                                                choices = "",
                                                selected = NULL,
                                                multiple = FALSE,
                                                selectize = TRUE,
                                                width = NULL,
                                                size = NULL
                                            )
                                        ),
                                        tags$div(style = "display: inline-block;vertical-align:top; width: 50px;", HTML("<br>")),
                                        tags$div(
                                            style = "display: inline-block;vertical-align:top; width: 250px;",
                                            
                                            selectInput(
                                                inputId = "select_destination_ooc",
                                                label = "Select destination zip",
                                                choices = "",
                                                selected = NULL,
                                                multiple = FALSE,
                                                selectize = TRUE,
                                                width = NULL,
                                                size = NULL
                                            )
                                        )
                                    )
                                )))


summary_tab <- bs4TabItem(
    tabName = "summary",
    h4("Summary Statistics"),
    fluidRow(
        bs4InfoBoxOutput("vehicle_count"),
        bs4InfoBoxOutput("finished_count"),
        bs4InfoBoxOutput("out_of_charge_count"),
        bs4InfoBoxOutput("evmt_count"),
        bs4InfoBoxOutput("charging_session_count")
        
    )
)

ui <- bs4DashPage(
    navbar = bs4DashNavbar(
        skin = "dark",
        status = "info",
        border = TRUE,
        sidebarIcon = "bars",
        controlbarIcon = "th",
        fixed = FALSE,
        "WSDOT EVI-ABM Results Viewer",
        tags$div(style = "display: inline-block;vertical-align:top; width: 50px;", HTML("<br>")),
        tags$div(
            style = "display: inline-block;vertical-align:top;",
            selectInput(
                inputId = "select_date",
                label = "Select simulated date",
                choices = sim_df$sim_dates,
                selected = NULL,
                multiple = FALSE,
                selectize = TRUE,
                width = NULL,
                size = NULL
            )
        ),
        tags$div(style = "display: inline-block;vertical-align:top; width: 20px;", HTML("<br>")),
        tags$div(
            style = "display: inline-block;vertical-align:top; ",
            selectInput(
                inputId = "select_datetime",
                label = "Select simulation run datetime",
                choices = " " ,
                selected = NULL,
                multiple = FALSE,
                selectize = TRUE,
                width = NULL,
                size = NULL
            )
        )
    ),
    sidebar = bs4DashSidebar(
        skin = "dark",
        status = "info",
        brandColor = "info",
        url = "",
        src = "",
        elevation = 3,
        opacity = 0.3,
        width = 4,
        bs4SidebarMenu(
            bs4SidebarMenuItem(
                text = "View Results",
                icon = "poll",
                startExpanded = TRUE,
                bs4SidebarMenuSubItem(
                    text = "Finished",
                    tabName = "finished",
                    icon = "route"
                ),
                bs4SidebarMenuSubItem(
                    text = "EVSE Utilization",
                    tabName = "evse_util",
                    icon = "file-invoice"
                ),
                bs4SidebarMenuSubItem(
                    text = "Out of charge",
                    tabName = "out_of_charge",
                    icon = "battery-empty"
                ),
                bs4SidebarMenuSubItem(
                    text = "Summary Stats",
                    tabName = "summary",
                    icon = "chart-bar"
                )
            )
        )
    ),
    footer = bs4DashFooter(bs4DashFooter(
        copyrights = a(
            href = "https: /  / faculty.washington.edu / dwhm / ",
            target = "_blank",
            "Chintan Pathak and Don MacKenzie,
            UW"
        ),
        right_text = "2019"
        )),
    title = "WSDOT EVI-ABM Results Viewer",
    body = bs4DashBody(
        tags$head(tags$style(
            HTML(
                '
                .form-group, .selectize-control {
                margin-bottom: 0px;
                }
                .box-body {
                padding-bottom: 0px;
                }
                '
            )
            )),
        bs4TabItems(finished_tab,
                    evse_util_tab,
                    out_of_charge_tab,
                    summary_tab)
            )
        )

server <- function(input, output, session) {
    rvData <- reactiveValues(
        simulated_date = NULL,
        simulation_runtime = NULL,
        finished_df = data.frame(),
        out_of_charge_df = data.frame(),
        power_draw_df = data.frame(),
        charging_session_df = data.frame(),
        evse_util_df = data.frame(),
        soc_df = data.frame(),
        lat_df = data.frame(),
        lng_df = data.frame(),
        state_df = data.frame(),
        prob_df = data.frame(),
        tocharge_df = data.frame(),
        trip_scenario_day_df = data.frame(),
        od_layer = NULL,
        tp_layer = NULL
    )
    
    clearMapOverlay <- function(mapID) {
        print("clearing markers now")
        leafletProxy(mapId = mapID) %>%
            clearGroup(group = "od_points") %>%
            clearGroup(group = "travel_path") %>%
            clearGroup(group = "shortest_path") %>%
            clearGroup(group = "charging_sessions")
    }
    
    output$wa_fin_mapout <- renderLeaflet({
        wa_map %>%
            addMarkers(
                lng = ~ Longitude ,
                lat = ~ Latitude,
                layerId = ~ ID,
                icon = combo_icons,
                group = base_layers[1],
                data = all_chargers_combo
            )  %>%
            addMarkers(
                lng = ~ Longitude ,
                lat = ~ Latitude,
                layerId = ~ ID,
                icon = combo_icons,
                group = base_layers[2],
                data = all_chargers_chademo
            ) %>%
            addLayersControl(overlayGroups = base_layers,
                             options = layersControlOptions(collapsed = FALSE))
    })
    
    output$wa_ooc_mapout <- renderLeaflet({
        od_str <- NULL
        
        for (i in 1:length(rvData$out_of_charge_df)) {
            od_str <-
                append(
                    od_str,
                    paste(
                        rvData$out_of_charge_df$origin_zip[i],
                        rvData$out_of_charge_df$destination_zip[i],
                        sep = "->"
                    )
                )
        }
        wa_map %>%
            addMarkers(
                lng = ~ Longitude ,
                lat = ~ Latitude,
                layerId = ~ ID,
                icon = combo_icons,
                group = base_layers[1],
                data = all_chargers_combo
            )  %>%
            addMarkers(
                lng = ~ Longitude ,
                lat = ~ Latitude,
                layerId = ~ ID,
                icon = combo_icons,
                group = base_layers[2],
                data = all_chargers_chademo
            ) %>%
            addCircleMarkers(
                data = rvData$out_of_charge_df,
                lat = ~ latitude,
                lng = ~ longitude,
                radius = 4,
                color = "#D5696F",
                popup = od_str,
                label = od_str,
                stroke = FALSE,
                fillOpacity = 0.5
            ) %>%
            addLayersControl(overlayGroups = base_layers,
                             options = layersControlOptions(collapsed = FALSE))
        
        # print("After markers")
    })
    
    output$wa_evse_mapout <- renderLeaflet({
        wa_map %>%
            addMarkers(
                lng = ~ Longitude ,
                lat = ~ Latitude,
                layerId = ~ ID,
                icon = combo_icons,
                group = base_layers[1],
                data = all_chargers_combo
            )  %>%
            addMarkers(
                lng = ~ Longitude ,
                lat = ~ Latitude,
                layerId = ~ ID,
                icon = combo_icons,
                group = base_layers[2],
                data = all_chargers_chademo
            ) %>%
            addLayersControl(overlayGroups = base_layers,
                             options = layersControlOptions(collapsed = FALSE))
    })
    
    observeEvent(input$wa_evse_mapout_marker_click, {
        id = input$wa_evse_mapout_marker_click$id
        print(id)
        
        if (!rapportools::is.empty(rvData$simulation_runtime)) {
            power_draw_evse <-
                as.data.frame(rvData$power_draw_df[, names(rvData$power_draw_df) %in% c("datetime", id)])
            evse_util <-
                round(rvData$evse_util_df$energy_consumed[which(rvData$evse_util_df$evse_id == id)], 2)
            chademo_count <-
                all_chargers_chademo$`EV DC Fast Count`[all_chargers_chademo$ID == id]
            combo_count <-
                all_chargers_combo$`EV DC Fast Count`[all_chargers_combo$ID == id]
            
            
            
            if (ncol(power_draw_evse) == 2) {
                showModal(
                    modalDialog(
                        title = "Charging station details",
                        fluidRow(
                            bs4Table(
                                cardWrap = TRUE,
                                bordered = TRUE,
                                striped = TRUE,
                                headTitles = c(
                                    "ID",
                                    "EVSE Utilization (kWh)",
                                    "# Chademo",
                                    "# Combo",
                                    "# EVs Served",
                                    "# EVs Passed"
                                ),
                                bs4TableItems(
                                    bs4TableItem(id),
                                    bs4TableItem(dataCell = TRUE, evse_util),
                                    bs4TableItem(dataCell = TRUE,
                                                 chademo_count),
                                    bs4TableItem(dataCell = TRUE,
                                                 combo_count),
                                    bs4TableItem(dataCell = TRUE,
                                                 combo_count)
                                )
                            )
                        ),
                        renderPlot(
                            plot(
                                x = lubridate::as_datetime(power_draw_evse$datetime),
                                y = as.numeric(power_draw_evse[[as.character(id)]]),
                                type = "l",
                                main = "Power vs Time of day",
                                xlab = "Time of day (minutes)",
                                ylab = "Power (kW)"
                            )
                        ),
                        easyClose = TRUE,
                        size = "l",
                        fade = FALSE
                    )
                )
            } else {
                showModal(
                    modalDialog(
                        size = "s",
                        easyClose = TRUE,
                        "The charging station seems to be new and we do not have any data for it yet.",
                        fade = FALSE
                    )
                )
            }
        } else {
            showModal(
                modalDialog(
                    size = "s",
                    easyClose = TRUE,
                    "Select a simulation date and run time to see the EVSE utilization."
                )
            )
        }
    })
    
    session$onFlushed(function() {
        
    })
    
    observeEvent(input$select_date, {
        print("Date selected")
        
        rvData$simulated_date <- input$select_date
        
        updateSelectInput(session,
                          inputId = "select_datetime",
                          choices =  sim_df$sim_times[which(sim_df$sim_dates == input$select_date)])
    })
    
    observeEvent(input$select_origin_ooc, {
        if (!rapportools::is.empty(input$select_origin_ooc)) {
            updateSelectInput(
                session,
                inputId = 'select_destination_ooc',
                choices = sort(rvData$out_of_charge_df$destination_zip[which(rvData$out_of_charge_df$origin_zip == input$select_origin_ooc)])
            )
        }
    })
    
    observeEvent(input$select_origin_fin, {
        if (!rapportools::is.empty(input$select_origin_fin)) {
            updateSelectInput(
                session,
                inputId = 'select_destination_fin',
                choices = sort(rvData$finished_df$destination_zip[which(rvData$finished_df$origin_zip == input$select_origin_fin)])
            )
        }
    })
    
    observeEvent(input$select_destination_fin, {
        clearMapOverlay("wa_fin_mapout")
        
        if (!rapportools::is.empty(input$select_destination_fin)) {
            finished_row <-
                rvData$finished_df[(
                    rvData$finished_df$origin_zip == input$select_origin_fin &
                        rvData$finished_df$destination_zip == input$select_destination_fin
                ),]
            if (nrow(finished_row) == 1) {
                veh_id <-
                    finished_row$veh_ID # paste0("X", trimws(finished_row$veh_ID))
                dt <- rvData$lat_df$datetime
                lats <- as.numeric(rvData$lat_df[[veh_id]])
                lngs <- as.numeric(rvData$lng_df[[veh_id]])
                
                socs <-
                    paste("SOC:", round(as.numeric(rvData$soc_df[[veh_id]]), 2))
                tocharges <-
                    paste("To charge:", rvData$tocharge_df[[veh_id]])
                probs <-
                    paste("Probability:", round(as.numeric(rvData$prob_df[[veh_id]], 2)))
                states <- paste("State:", rvData$state_df[[veh_id]])
                
                ev_info_df <-
                    data.frame(socs,
                               tocharges,
                               probs,
                               states,
                               stringsAsFactors = FALSE)
                
                trip_row <-
                    rvData$trip_scenario_day_df[which(rvData$trip_scenario_day_df$ulid == trimws(finished_row$veh_ID)), ]
                od_lats <-
                    c(trip_row$Origin_Lat,
                      trip_row$Destination_Lat)
                od_lngs <-
                    c(trip_row$Origin_Lon,
                      trip_row$Destination_Lon)
                rvData$od_layer <- rep("od_points", length(od_lats))
                rvData$tp_layer <- rep("travel_path", length(lats))
                dbq <-
                    paste0(
                        "select * from shortest_path_od2(",
                        input$select_origin_fin,
                        " ," ,
                        input$select_destination_fin,
                        ")"
                    )
                sp_res <- DBI::dbSendQuery(con, dbq)
                sp_obj <- DBI::dbFetch(sp_res)
                spath <- st_as_sfc(sp_obj$shortest_path)
                DBI::dbClearResult(sp_res)
                
                cs_df <-
                    rvData$charging_session_df[which(rvData$charging_session_df$veh_ID == veh_id), ]
                cs_lats <-
                    evse_dcfc$Latitude[which(evse_dcfc$ID == cs_df$evse_id)]
                cs_lngs <-
                    evse_dcfc$Longitude[which(evse_dcfc$ID == cs_df$evse_id)]
                
                leafletProxy(mapId = "wa_fin_mapout") %>%
                    addMapPane(name = "od_points", zIndex = 410) %>%
                    addMapPane(name = "travel_path", zIndex = 490) %>%
                    addMapPane(name = "charging_sessions", zIndex = 491) %>%
                    addCircleMarkers(
                        lat = od_lats[1],
                        lng = od_lngs[1],
                        radius = 12,
                        color = "#960db5",
                        group = "od_points",
                        stroke = FALSE,
                        fillOpacity = 0.5,
                        label = paste0("Origin: ", input$select_origin_fin),
                        labelOptions = labelOptions(noHide = T),
                        options = pathOptions(pane = "od_points")
                        
                    ) %>% addCircleMarkers(
                        lat = od_lats[2],
                        lng = od_lngs[2],
                        radius = 12,
                        color = "#420db5",
                        group = "od_points",
                        stroke = FALSE,
                        fillOpacity = 0.5,
                        label = paste0(
                            "Destination: ",
                            input$select_destination_fin
                        ),
                        labelOptions = labelOptions(noHide = T),
                        options = pathOptions(pane = "od_points")
                        
                    ) %>%
                    addCircleMarkers(
                        lat = lats,
                        lng = lngs,
                        radius = 4,
                        color = " #75d654",
                        popup = paste(
                            sep = "<br>",
                            ev_info_df$socs,
                            ev_info_df$tocharges,
                            ev_info_df$probs,
                            ev_info_df$states
                        ),
                        label = paste(
                            sep = "\n",
                            ev_info_df$socs,
                            ev_info_df$tocharges,
                            ev_info_df$probs,
                            ev_info_df$states
                        ),
                        group = "travel_path",
                        stroke = FALSE,
                        fillOpacity = 0.5,
                        options = pathOptions(pane = "travel_path")
                    ) %>%
                    addPolylines(
                        data = spath,
                        color = "#3371ff",
                        group = "shortest_path",
                        opacity = 1
                    )
                
                if (nrow(cs_df) >= 1) {
                    leafletProxy(mapId = "wa_fin_mapout") %>%
                        addCircleMarkers(
                            lat = cs_lats,
                            lng = cs_lngs,
                            radius = 12,
                            group = "charging_sessions",
                            popup = paste(
                                sep = "<br>",
                                "Charging session:",
                                row.names(cs_df),
                                "Starting SOC:",
                                round(as.numeric(cs_df$starting_SOC), 2),
                                "Ending SOC:",
                                round(as.numeric(cs_df$ending_SOC))
                            ),
                            label = row.names(cs_df),
                            labelOptions = labelOptions(noHide = T, textsize = "18px"),
                            stroke = FALSE,
                            fillOpacity = 0.8,
                            options = pathOptions(pane = "charging_sessions")
                        )
                }
                
            }
            
        }
    })
    
    observeEvent(input$select_destination_ooc, {
        clearMapOverlay("wa_ooc_mapout")
        
        if (!rapportools::is.empty(input$select_destination_ooc)) {
            out_of_charge_row <-
                rvData$out_of_charge_df[(
                    rvData$out_of_charge_df$origin_zip == input$select_origin_ooc &
                        rvData$out_of_charge_df$destination_zip == input$select_destination_ooc
                ),]
            if (nrow(out_of_charge_row) == 1) {
                veh_id <-
                    out_of_charge_row$veh_ID # paste0("X", trimws(out_of_charge_row$veh_ID))
                dt <- rvData$lat_df$datetime
                lats <- as.numeric(rvData$lat_df[[veh_id]])
                lngs <- as.numeric(rvData$lng_df[[veh_id]])
                socs <-
                    paste("SOC:", round(as.numeric(rvData$soc_df[[veh_id]]), 2))
                tocharges <-
                    paste("To charge:", rvData$tocharge_df[[veh_id]])
                probs <-
                    paste("Probability:", round(as.numeric(rvData$prob_df[[veh_id]], 2)))
                states <- paste("State:", rvData$state_df[[veh_id]])
                
                ev_info_df <-
                    data.frame(socs,
                               tocharges,
                               probs,
                               states,
                               stringsAsFactors = FALSE)
                
                trip_row <-
                    rvData$trip_scenario_day_df[which(
                        rvData$trip_scenario_day_df$ulid == trimws(out_of_charge_row$veh_ID)
                    ), ]
                od_lats <-
                    c(trip_row$Origin_Lat,
                      trip_row$Destination_Lat)
                od_lngs <-
                    c(trip_row$Origin_Lon,
                      trip_row$Destination_Lon)
                dbq <-
                    paste0(
                        "select * from shortest_path_od2(",
                        input$select_origin_ooc,
                        " ," ,
                        input$select_destination_ooc,
                        ")"
                    )
                sp_res <- DBI::dbSendQuery(con, dbq)
                sp_obj <- DBI::dbFetch(sp_res)
                spath <- st_as_sfc(sp_obj$shortest_path)
                DBI::dbClearResult(sp_res)
                
                cs_df <-
                    rvData$charging_session_df[which(rvData$charging_session_df$veh_ID == veh_id), ]
                cs_lats <-
                    evse_dcfc$Latitude[which(evse_dcfc$ID == cs_df$evse_id)]
                cs_lngs <-
                    evse_dcfc$Longitude[which(evse_dcfc$ID == cs_df$evse_id)]
                
                leafletProxy(mapId = "wa_ooc_mapout") %>%
                    addMapPane(name = "od_points", zIndex = 410) %>%
                    addMapPane(name = "travel_path", zIndex = 490) %>%
                    addMapPane(name = "charging_sessions", zIndex = 491) %>%
                    addCircleMarkers(
                        lat = od_lats[1],
                        lng = od_lngs[1],
                        radius = 12,
                        color = "#FFC300",
                        group = "od_points",
                        stroke = FALSE,
                        fillOpacity = 0.5,
                        label = paste0("Origin: ", input$select_origin_ooc),
                        labelOptions = labelOptions(noHide = T),
                        options = pathOptions(pane = "od_points")
                        
                    ) %>% addCircleMarkers(
                        lat = od_lats[2],
                        lng = od_lngs[2],
                        radius = 12,
                        color = "#FF5733",
                        group = "od_points",
                        stroke = FALSE,
                        fillOpacity = 0.5,
                        label = paste0(
                            "Destination: ",
                            input$select_destination_ooc
                        ),
                        labelOptions = labelOptions(noHide = T),
                        options = pathOptions(pane = "od_points")
                        
                    ) %>%
                    addCircleMarkers(
                        lat = lats,
                        lng = lngs,
                        radius = 4,
                        color = "#b50d2c",
                        popup = paste(
                            sep = "<br>",
                            ev_info_df$socs,
                            ev_info_df$tocharges,
                            ev_info_df$probs,
                            ev_info_df$states
                        ),
                        label = paste(
                            sep = "\n",
                            ev_info_df$socs,
                            ev_info_df$tocharges,
                            ev_info_df$probs,
                            ev_info_df$states
                        ),
                        group = "travel_path",
                        stroke = FALSE,
                        fillOpacity = 0.5,
                        options = pathOptions(pane = "travel_path")
                    ) %>%
                    addPolylines(
                        data = spath,
                        color = "#3371ff",
                        group = "shortest_path",
                        opacity = 1
                    )
                
                if (nrow(cs_df) >= 1) {
                    leafletProxy(mapId = "wa_ooc_mapout") %>%
                        addCircleMarkers(
                            lat = cs_lats,
                            lng = cs_lngs,
                            radius = 12,
                            group = "charging_sessions",
                            popup = paste(
                                sep = "<br>",
                                "Charging session:",
                                row.names(cs_df),
                                "Starting SOC:",
                                round(as.numeric(cs_df$starting_SOC)),
                                "Ending SOC:",
                                round(as.numeric(cs_df$ending_SOC))
                            ),
                            label = row.names(cs_df),
                            labelOptions = labelOptions(noHide = T, textsize = "20px"),
                            stroke = FALSE,
                            fillOpacity = 0.8,
                            options = pathOptions(pane = "charging_sessions")
                        )
                }
                
            }
            
        }
    })
    
    observeEvent(input$select_datetime, {
        print("Datetime selected")
        rvData$simulation_runtime <- input$select_datetime
        if (!rapportools::is.empty(rvData$simulation_runtime)) {
            trip_scenario_day_file <-
                list.files(".", pattern = rvData$simulated_date)
            rvData$trip_scenario_day_df <-
                vroom::vroom(trip_scenario_day_file , delim = ",")
            nevs <- nrow(rvData$trip_scenario_day_df)
            
            print(paste0("Number of EVs for the day: ", nevs))
            
            sim_str <-
                paste(rvData$simulated_date,
                      rvData$simulation_runtime,
                      sep = "_")
            evse_util_file <-
                list.files("evse_util", pattern = sim_str)
            rvData$evse_util_df <-
                vroom::vroom(
                    here::here("evse_util", evse_util_file),
                    delim = ",",
                    col_types = "id"
                )
            charging_session_file <-
                list.files("charging_session", pattern = sim_str)
            rvData$charging_session_df <-
                vroom::vroom(
                    here::here("charging_session", charging_session_file),
                    delim = ",",
                    col_types = "cccddiii"
                )
            
            power_draw_file <-
                list.files("power_draw", pattern = sim_str)
            rvData$power_draw_df <-
                vroom::vroom(
                    here::here("power_draw", power_draw_file),
                    delim = ",")

            out_of_charge_file <-
                list.files("out_of_charge", pattern = sim_str)
            rvData$out_of_charge_df <-
                vroom::vroom(
                    here::here("out_of_charge", out_of_charge_file),
                    delim = ",",
                    col_types = "ccddii"
                )
            finished_file <-
                list.files("finished", pattern = sim_str)
            rvData$finished_df <-
                vroom::vroom(
                    here::here("finished", finished_file),
                    delim = ",",
                    col_types = "cciiddd"
                )
            
            soc_str <- paste("soc", sim_str, sep = "_")
            soc_file <-
                list.files("ev_agents_info", pattern = soc_str)
            rvData$soc_df <-
                vroom::vroom(
                    here::here("ev_agents_info", soc_file),
                    delim = ",",
                    col_types = paste0("c", strrep("c", nevs)),
                    .name_repair = "minimal"
                )
            
            lat_str <- paste("lat", sim_str, sep = "_")
            lat_file <-
                list.files("ev_agents_info", pattern = lat_str)
            rvData$lat_df <-
                vroom::vroom(
                    here::here("ev_agents_info", lat_file),
                    delim = ",",
                    col_types = paste0("c", strrep("c", nevs)),
                    .name_repair = "minimal"
                )
            
            lng_str <- paste("lng", sim_str, sep = "_")
            lng_file <-
                list.files("ev_agents_info", pattern = lng_str)
            rvData$lng_df <-
                vroom::vroom(
                    here::here("ev_agents_info", lng_file),
                    delim = ",",
                    col_types = paste0("c", strrep("c", nevs)),
                    .name_repair = "minimal"
                )
            
            # speed_str <- paste("soc", sim_str, sep = "_")
            # soc_file <- list.files("ev_agents_info/so", pattern = soc_str)
            # soc_df <- read.csv(here::here("ev_agents_info", soc_file),
            #                    stringsAsFactors = FALSE)
            
            prob_str <- paste("prob", sim_str, sep = "_")
            prob_file <-
                list.files("ev_agents_info", pattern = prob_str)
            rvData$prob_df <-
                vroom::vroom(
                    here::here("ev_agents_info", prob_file),
                    delim = ",",
                    col_types = paste0("c", strrep("c", nevs)),
                    .name_repair = "minimal"
                )
            
            tocharge_str <- paste("tocharge", sim_str, sep = "_")
            tocharge_file <-
                list.files("ev_agents_info", pattern = tocharge_str)
            rvData$tocharge_df <-
                vroom::vroom(
                    here::here("ev_agents_info", tocharge_file),
                    delim = ",",
                    col_types = paste0("c", strrep("c", nevs)),
                    .name_repair = "minimal"
                )
            
            state_str <- paste("state", sim_str, sep = "_")
            state_file <-
                list.files("ev_agents_info", pattern = state_str)
            rvData$state_df <-
                vroom::vroom(
                    here::here("ev_agents_info", state_file),
                    delim = ",",
                    col_types = paste0("c", strrep("c", nevs)),
                    .name_repair = "minimal"
                )
            
            
            updateSelectInput(
                session,
                inputId = 'select_origin_ooc',
                choices = sort(rvData$out_of_charge_df$origin_zip)
            )
            
            updateSelectInput(
                session,
                inputId = 'select_origin_fin',
                choices = sort(rvData$finished_df$origin_zip)
            )
            
            output$vehicle_count <- renderbs4InfoBox({
                bs4InfoBox(value = nevs,
                           title = "EVs in Simulation",
                           icon = "layer-group")
            })
            
            output$finished_count <- renderbs4InfoBox({
                bs4InfoBox(
                    value = nrow(rvData$finished_df),
                    title = "EVs finishing trip",
                    icon = "car"
                )
            })
            
            output$out_of_charge_count <- renderbs4InfoBox({
                bs4InfoBox(
                    value = nrow(rvData$out_of_charge_df),
                    title = "EVs out of charge during trip",
                    icon = "car-crash"
                )
            })
            
            output$evmt_count <- renderbs4InfoBox({
                bs4InfoBox(
                    value = sum(rvData$finished_df$distance_travelled),
                    title = "eVMT",
                    icon = "bolt"
                )
            })
            
            output$charging_session_count <- renderbs4InfoBox({
                bs4InfoBox(
                    value = nrow(rvData$charging_session_df),
                    title = "Number of charging sessions",
                    icon = "charging-station"
                )
            })
        }
        
    })
}


shiny::shinyApp(ui, server)