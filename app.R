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

# WA roads data
wa_roads <- readRDS(here::here("data-raw", "wa_roads.Rds"))

# Read the EVSE information through the AFDC API
afdc_url  <-
    paste0(
        "https://developer.nrel.gov/api/alt-fuel-stations/v1.csv?fuel_type=ELEC&state=WA&ev_charging_level=dc_fast&status=E&access=public&api_key=",
        Sys.getenv('AFDC_API_KEY')
    )
evse_dcfc <- read_csv(afdc_url)
# TODO: Add a fallback clause in the case the API is non-responsive

evse_dcfc$EV_Connector_Code <- 0
evse_dcfc$ChargingCost <- 0

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
                    text = "Routes",
                    tabName = "routes",
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
                    out_of_charge_tab)
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
        evse_util_df = data.frame()
    )
    
    output$wa_fin_mapout <- renderLeaflet(
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
            addLayersControl(
                overlayGroups = base_layers,
                options = layersControlOptions(collapsed = FALSE)
            )
    )
    
    output$wa_ooc_mapout <- renderLeaflet({
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
                color = "#AAD3DF"
            ) %>%
            addLayersControl(overlayGroups = base_layers,
                             options = layersControlOptions(collapsed = FALSE))
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
                as.data.frame(rvData$power_draw_df[, names(rvData$power_draw_df) %in% c("datetime", paste0("X", id))])
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
                                    "Number of Chademo Plugs",
                                    "Number of Combo Plugs"
                                ),
                                bs4TableItems(
                                    bs4TableItem(id),
                                    bs4TableItem(dataCell = TRUE, evse_util),
                                    bs4TableItem(dataCell = TRUE,
                                                 chademo_count),
                                    bs4TableItem(dataCell = TRUE,
                                                 combo_count)
                                )
                            )
                        ),
                        renderPlot(
                            plot(
                                x = lubridate::as_datetime(power_draw_evse$datetime),
                                y = power_draw_evse[[paste0("X", id)]],
                                type = "l"
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
                        "The charging station seems to be new and we do not have any data for it yet."
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
                choices = rvData$out_of_charge_df$destination_zip[which(rvData$out_of_charge_df$origin_zip == input$select_origin_ooc)]
            )
        }
    })
    
    observeEvent(input$select_origin_fin, {
        if (!rapportools::is.empty(input$select_origin_fin)) {
            updateSelectInput(
                session,
                inputId = 'select_destination_fin',
                choices = rvData$finished_df$destination_zip[which(rvData$finished_df$origin_zip == input$select_origin_fin)]
            )
        }
    })
    
    observeEvent(input$select_destination_fin, {
        if (!rapportools::is.empty(input$select_destination_fin)) {
            finished_row <-
                rvData$finished_df[(
                    rvData$finished_df$origin_zip == input$select_origin_fin &
                        rvData$finished_df$destination_zip == input$select_destination_fin
                ), ]
            if (nrow(finished_row) == 1) {
                leafletProxy(mapId = "wa_fin_mapout") %>%
                    addCircleMarkers(
                        data = finished_row,
                        lat = ~ latitude,
                        lng = ~ longitude,
                        radius = 8,
                        color = "#AAD3DF"
                        
                    )
            }
            
        }
    })
    
    observeEvent(input$select_destination_ooc, {
        if (!rapportools::is.empty(input$select_destination_ooc)) {
            out_of_charge_row <-
                rvData$out_of_charge_df[(
                    rvData$out_of_charge_df$origin_zip == input$select_origin_ooc &
                        rvData$out_of_charge_df$destination_zip == input$select_destination_ooc
                ), ]
            if (nrow(out_of_charge_row) == 1) {
                leafletProxy(mapId = "wa_ooc_mapout") %>%
                    addCircleMarkers(
                        data = out_of_charge_row,
                        lat = ~ latitude,
                        lng = ~ longitude,
                        radius = 8,
                        color = "#AAD3DF"
                        
                    )
            }
            
        }
    })
    
    observeEvent(input$select_datetime, {
        print("Datetime selected")
        rvData$simulation_runtime <- input$select_datetime
        if (!rapportools::is.empty(rvData$simulation_runtime)) {
            sim_str <-
                paste(rvData$selected_date,
                      rvData$simulation_runtime,
                      sep = "_")
            evse_util_file <-
                list.files("evse_util", pattern = sim_str)
            rvData$evse_util_df <-
                read.csv(here::here("evse_util", evse_util_file),
                         stringsAsFactors = FALSE)
            charging_session_file <-
                list.files("charging_session", pattern = sim_str)
            rvData$charging_session_df <-
                read.csv(
                    here::here("charging_session", charging_session_file),
                    stringsAsFactors = FALSE
                )
            power_draw_file <-
                list.files("power_draw", pattern = sim_str)
            rvData$power_draw_df <-
                read.csv(here::here("power_draw", power_draw_file),
                         stringsAsFactors = FALSE)
            out_of_charge_file <-
                list.files("out_of_charge", pattern = sim_str)
            rvData$out_of_charge_df <-
                read.csv(here::here("out_of_charge", out_of_charge_file),
                         stringsAsFactors = FALSE)
            finished_file <-
                list.files("finished", pattern = sim_str)
            rvData$finished_df <-
                read.csv(here::here("finished", finished_file),
                         stringsAsFactors = FALSE)
            
            updateSelectInput(
                session,
                inputId = 'select_origin_ooc',
                choices = rvData$out_of_charge_df$origin_zip
            )
            
            updateSelectInput(session,
                              inputId = 'select_origin_fin',
                              choices = rvData$finished$origin_zip)
        }
        
    })
}


shiny::shinyApp(ui, server)