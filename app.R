source("global.R")

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
                                        leafglOutput("wa_evse_util_mapout", height = 700),
                                        type = 8,
                                        color = "#0dc5c1"
                                    )
                                )
                            ), column(
                                width = 3,
                                bs4Card(
                                    width = 12,
                                    title = "Start and End Time Selector",
                                    closable = FALSE,
                                    sliderInput(
                                        "evse_util_slider",
                                        label = "Time Range",
                                        min = 0,
                                        max = 24,
                                        value = c(0, 24)
                                    )
                                )
                            )))

evse_busy_tab <- bs4TabItem(tabName = "evse_busy",
                            fluidRow(column(
                                width = 9,
                                bs4Card(
                                    title = "EVSE Busy Details",
                                    closable = FALSE,
                                    status = "warning",
                                    collapsible = TRUE,
                                    elevation = 4,
                                    width = NULL,
                                    solidHeader = TRUE,
                                    withSpinner(
                                        leafglOutput("wa_evse_busy_mapout", height = 700),
                                        type = 8,
                                        color = "#0dc5c1"
                                    )
                                )
                            ), column(
                                width = 3,
                                bs4Card(
                                    width = 12,
                                    title = "Start and End Time Selector",
                                    closable = FALSE,
                                    sliderInput(
                                        "evse_busy_slider",
                                        label = "Time Range",
                                        min = 0,
                                        max = 23,
                                        value = c(0, 24)
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
        bs4InfoBoxOutput("charging_session_count"),
        bs4InfoBoxOutput("evs_passed_count")
        
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
                    text = "Summary Stats",
                    tabName = "summary",
                    icon = "chart-bar"
                ),
                bs4SidebarMenuSubItem(
                    text = "Finished",
                    tabName = "finished",
                    icon = "route"
                ),
                bs4SidebarMenuSubItem(
                    text = "EVSE Utilization",
                    tabName = "evse_util",
                    icon = "charging-station"
                ),
                bs4SidebarMenuSubItem(
                    text = "EVSE Busy",
                    tabName = "evse_busy",
                    icon = "thermometer-full"
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
        bs4TabItems(
            summary_tab,
            finished_tab,
            evse_util_tab,
            evse_busy_tab,
            out_of_charge_tab
        )
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
        evs_passed_df = data.frame(),
        evse_util_df = data.frame(),
        soc_df = data.frame(),
        lat_df = data.frame(),
        lng_df = data.frame(),
        state_df = data.frame(),
        prob_df = data.frame(),
        tocharge_df = data.frame(),
        trip_scenario_day_df = data.frame(),
        od_layer = NULL,
        tp_layer = NULL,
        ev_pass_combo = data.frame(),
        ev_pass_chademo = data.frame()
    )
    
    clearMapOverlay <- function(mapID) {
        print("clearing markers now")
        leafletProxy(mapId = mapID) %>%
            clearGroup(group = "od_points") %>%
            clearGroup(group = "travel_path") %>%
            clearGroup(group = "shortest_path") %>%
            clearGroup(group = "charging_sessions")
    }
    
    clearMapBusy <- function() {
        print("clearing busy now")
        leafletProxy(mapId = "wa_evse_busy_mapout") %>%
            clearGroup(group = "busy_combo")  %>%
            clearGroup(group = "busy_chademo")
        
    }
    
    plot_trajectory <- function(tab_name) {
        if (tab_name == "finished") {
            relevant_df <- rvData$finished_df
            waypoint_color <- "#75d654"
            dest_color <- "#420db5"
            orig_color <- "#960db5"
            dest_ui_element <- input$select_destination_fin
            orig_ui_element <- input$select_origin_fin
            map_id <- "wa_fin_mapout"
        } else if (tab_name == "out_of_charge") {
            relevant_df <- rvData$out_of_charge_df
            waypoint_color <- "#b50d2c"
            dest_color <- "#FF5733"
            orig_color <- "#FFC300"
            dest_ui_element <- input$select_destination_ooc
            orig_ui_element <- input$select_origin_ooc
            map_id <- "wa_ooc_mapout"
        }
        
        clearMapOverlay(mapID = map_id)
        
        if (!rapportools::is.empty(dest_ui_element)) {
            relevant_row <-
                relevant_df[(
                    relevant_df$origin_zip == orig_ui_element &
                        relevant_df$destination_zip == dest_ui_element
                ), ]
            if (nrow(relevant_row) == 1) {
                veh_id <-
                    relevant_row$veh_ID # paste0("X", trimws(finished_row$veh_ID))
                dt <- rvData$lat_df$datetime
                lats <- as.numeric(rvData$lat_df[[veh_id]])
                lngs <- as.numeric(rvData$lng_df[[veh_id]])
                
                socs <-
                    paste("SOC:", round(as.numeric(rvData$soc_df[[veh_id]]), 2))
                tocharges <-
                    paste("To charge:", rvData$tocharge_df[[veh_id]])
                probs <-
                    paste("Probability:", round(as.numeric(rvData$prob_df[[veh_id]]), 3))
                states <- paste("State:", rvData$state_df[[veh_id]])
                
                ev_info_df <-
                    data.frame(socs,
                               tocharges,
                               probs,
                               states,
                               stringsAsFactors = FALSE)
                
                trip_row <-
                    rvData$trip_scenario_day_df[which(rvData$trip_scenario_day_df$ulid == trimws(relevant_row$veh_ID)),]
                od_lats <-
                    c(trip_row$Origin_Lat,
                      trip_row$Destination_Lat)
                od_lngs <-
                    c(trip_row$Origin_Lon,
                      trip_row$Destination_Lon)
                rvData$od_layer <- rep("od_points", length(od_lats))
                rvData$tp_layer <- rep("travel_path", length(lats))
                
                cs_df <-
                    rvData$charging_session_df[which(rvData$charging_session_df$veh_ID == veh_id),]
                
                evse_rows <- match(cs_df$evse_id, evse_dcfc$ID)
                cs_lats <-
                    evse_dcfc$Latitude[evse_rows]
                cs_lngs <-
                    evse_dcfc$Longitude[evse_rows]
                
                leafletProxy(mapId = map_id) %>%
                    addMapPane(name = "od_points", zIndex = 410) %>%
                    addMapPane(name = "travel_path", zIndex = 490) %>%
                    addMapPane(name = "charging_sessions", zIndex = 491) %>%
                    addCircleMarkers(
                        lat = od_lats[1],
                        lng = od_lngs[1],
                        radius = 12,
                        color = orig_color,
                        group = "od_points",
                        stroke = FALSE,
                        fillOpacity = 0.5,
                        label = paste0("Origin: ", orig_ui_element),
                        labelOptions = labelOptions(noHide = T),
                        options = pathOptions(pane = "od_points")
                        
                    ) %>% addCircleMarkers(
                        lat = od_lats[2],
                        lng = od_lngs[2],
                        radius = 12,
                        color = dest_color,
                        group = "od_points",
                        stroke = FALSE,
                        fillOpacity = 0.5,
                        label = paste0("Destination: ",
                                       dest_ui_element),
                        labelOptions = labelOptions(noHide = T),
                        options = pathOptions(pane = "od_points")
                        
                    ) %>%
                    addCircleMarkers(
                        lat = lats,
                        lng = lngs,
                        radius = 4,
                        color = waypoint_color,
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
                    )
                
                # Overlay charging session info
                if (nrow(cs_df) >= 1) {
                    leafletProxy(mapId = map_id) %>%
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
                lat = rvData$out_of_charge_df$latitude,
                lng = rvData$out_of_charge_df$longitude,
                radius = 4,
                color = "#D5696F",
                popup =                     paste(
                    rvData$out_of_charge_df$origin_zip,
                    rvData$out_of_charge_df$destination_zip,
                    sep = "->"
                ),
                label =                     paste(
                    rvData$out_of_charge_df$origin_zip,
                    rvData$out_of_charge_df$destination_zip,
                    sep = "->"
                ),
                stroke = FALSE,
                fillOpacity = 0.5
            ) %>%
            addLayersControl(overlayGroups = base_layers,
                             options = layersControlOptions(collapsed = FALSE))
        
        # print("After markers")
    })
    
    output$wa_evse_util_mapout <- renderLeaflet({
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
    
    output$wa_evse_busy_mapout <- renderLeaflet({
        wa_map %>%
            addMapPane(name = "chargers", zIndex = 410) %>%
            addMapPane(name = "busy_count", zIndex = 491) %>%
            addMarkers(
                lng = ~ Longitude ,
                lat = ~ Latitude,
                layerId = ~ ID,
                icon = combo_icons,
                group = base_layers[1],
                data = all_chargers_combo,
                options = pathOptions(pane = "chargers")
            )  %>%
            addMarkers(
                lng = ~ Longitude ,
                lat = ~ Latitude,
                layerId = ~ ID,
                icon = combo_icons,
                group = base_layers[2],
                data = all_chargers_chademo,
                options = pathOptions(pane = "chargers")
            ) %>%
            addLayersControl(overlayGroups = base_layers,
                             options = layersControlOptions(collapsed = FALSE))
    })
    
    observeEvent(input$wa_evse_util_mapout_marker_click, {
        id = input$wa_evse_util_mapout_marker_click$id
        print(id)
        ## TODO:
        ### Make the data filter with time
        ### Add other indication to the map, so we dont have to click each and every charger
        ### maybe evs_served
        
        if (!rapportools::is.empty(rvData$simulation_runtime)) {
            range_start_time <-
                as.POSIXct(paste(
                    input$select_date,
                    paste(input$evse_util_slider[1], 0, 0, sep = ":")
                ),
                tz = "Etc/GMT+8",
                format = "%Y-%m-%d %H:%M:%S")
            range_end_time <-
                as.POSIXct(paste(
                    input$select_date,
                    paste(input$evse_util_slider[2], 0, 0, sep = ":")
                ),
                tz = "Etc/GMT+8",
                format = "%Y-%m-%d %H:%M:%S")
            
            power_draw_evse <-
                as.data.frame(rvData$power_draw_df[, names(rvData$power_draw_df) %in% c("datetime", id)]) %>% 
                dplyr::mutate(datetime = as.POSIXct(as.numeric(datetime), origin = as.POSIXct("1970-01-01", tz = "Etc/GMT+8"), tz = "Etc/GMT+8")) %>%
                                  dplyr::filter(datetime >= range_start_time & datetime <= range_end_time)
            # evse_util <-
            #     round(rvData$evse_util_df$energy_consumed[which(rvData$evse_util_df$evse_id == id)], 2)
            # Evse util is Int(P)dt
            evse_util <- sum(as.numeric(power_draw_evse[[as.character(id)]])) / 60
            chademo_count <-
                all_chargers_chademo$`EV DC Fast Count`[all_chargers_chademo$ID == id]
            combo_count <-
                all_chargers_combo$`EV DC Fast Count`[all_chargers_combo$ID == id]
            
            evs_charged <-
                length(which(rvData$charging_session_df$evse_id == id))
            evs_passed <-
                length(which(rvData$evs_passed_df$evse_id == id))
            
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
                                                 evs_charged),
                                    bs4TableItem(dataCell = TRUE,
                                                 evs_passed)
                                )
                            )
                        ),
                        renderPlot(
                            plot(
                                x = power_draw_evse$datetime,
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
    
    # TODO: This just a placeholder for now
    observeEvent(input$wa_evse_busy_mapout_marker_click, {
        id = input$wa_evse_busy_mapout_marker_click$id
        print(id)
        
        if (!rapportools::is.empty(rvData$simulation_runtime)) {
            # else {
            #     showModal(
            #         modalDialog(
            #             size = "s",
            #             easyClose = TRUE,
            #             "The charging station seems to be new and we do not have any data for it yet.",
            #             fade = FALSE
            #         )
            #     )
            # }
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
    
    # When the date is selected, the simulated date time dropdown is populated
    observeEvent(input$select_date, {
        print("Date selected")
        
        rvData$simulated_date <- input$select_date
        
        updateSelectInput(session,
                          inputId = "select_datetime",
                          choices =  sim_df$sim_times[which(sim_df$sim_dates == input$select_date)])
    })
    
    # When origin zip is selected the corresponding destination dropdown is populated
    observeEvent(input$select_origin_ooc, {
        if (!rapportools::is.empty(input$select_origin_ooc)) {
            updateSelectInput(
                session,
                inputId = 'select_destination_ooc',
                choices = sort(rvData$out_of_charge_df$destination_zip[which(rvData$out_of_charge_df$origin_zip == input$select_origin_ooc)])
            )
        }
    })
    
    # When origin zip is selected the corresponding destination dropdown is populated
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
        plot_trajectory("finished")
    })
    
    observeEvent(input$select_destination_ooc, {
        plot_trajectory("out_of_charge")
    })
    
    observeEvent(input$select_datetime, {
        print("Datetime selected")
        clearMapBusy()
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
                vroom::vroom(here::here("power_draw", power_draw_file),
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
            
            evs_passed_file <-
                list.files("evs_passed", pattern = sim_str)
            rvData$evs_passed_df <-
                vroom::vroom(
                    here::here("evs_passed", evs_passed_file),
                    delim = ",",
                    col_types = "cicd"
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
            
            output$evs_passed_count <- renderbs4InfoBox({
                bs4InfoBox(
                    value = nrow(rvData$evs_passed_df),
                    title = "Number of EVs passed",
                    icon = "square-full"
                )
            })
            
            # Merge to get the connector code etc. info with the OOC cars
            rvData$out_of_charge_df <-
                merge(
                    rvData$out_of_charge_df,
                    rvData$trip_scenario_day_df[, c(
                        "ConnectorCode",
                        "County",
                        "Make",
                        "Model",
                        "Model.Year",
                        "Electric.Range",
                        "City",
                        "ZIP.Code",
                        "ulid"
                    )],
                    by.x = "veh_ID",
                    by.y = "ulid"
                )
            
            count_passed_evse <-
                rvData$evs_passed_df %>% dplyr::group_by(evse_id) %>% dplyr::summarise(evs_count = n())
            print(nrow(count_passed_evse))
            rvData$ev_pass_evse_combo <-
                merge(
                    all_chargers_combo[, c("ID", "Latitude", "Longitude")],
                    count_passed_evse,
                    by.x = "ID",
                    by.y = "evse_id",
                    all.x = TRUE
                )
            rvData$ev_pass_evse_chademo <-
                merge(
                    all_chargers_chademo[, c("ID", "Latitude", "Longitude")],
                    count_passed_evse,
                    by.x = "ID",
                    by.y = "evse_id",
                    all.x = TRUE
                )
            rvData$ev_pass_evse_combo <-
                tidyr::replace_na(rvData$ev_pass_evse_combo, list(evs_count = 0))
            rvData$ev_pass_evse_chademo <-
                tidyr::replace_na(rvData$ev_pass_evse_chademo, list(evs_count = 0))
            
            leafletProxy(mapId = "wa_evse_busy_mapout") %>%
                addCircleMarkers(
                    lng = ~ Longitude ,
                    lat = ~ Latitude,
                    group = "busy_combo",
                    data = rvData$ev_pass_evse_combo,
                    stroke = FALSE,
                    fillOpacity = 0.8,
                    color = "#ff5b33",
                    radius = 10 * as.numeric(rvData$ev_pass_evse_combo$evs_count),
                    label = ~ evs_count,
                    popup = ~ evs_count,
                    labelOptions = labelOptions(
                        noHide = TRUE,
                        offset = c(0, -12),
                        textOnly = TRUE
                    ),
                    options = pathOptions(pane = "busy_count")
                )  %>%
                addCircleMarkers(
                    lng = ~ Longitude ,
                    lat = ~ Latitude,
                    group = "busy_chademo",
                    data = rvData$ev_pass_evse_chademo,
                    stroke = FALSE,
                    fillOpacity = 0.8,
                    radius = 10 * as.numeric(rvData$ev_pass_evse_chademo$evs_count),
                    label = rvData$ev_pass_evse_chademo$evs_count,
                    popup = rvData$ev_pass_evse_chademo$evs_count,
                    color = "#9c0215",
                    labelOptions = labelOptions(noHide = TRUE,
                                                
                                                textOnly = TRUE),
                    options = pathOptions(pane = "busy_count")
                )
            
        }
        
    })
}


shiny::shinyApp(ui, server)