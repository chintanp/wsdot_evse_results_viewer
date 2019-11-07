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
                               labelTooltip = "Finished Details",
                               elevation = 4,
                               width = NULL,
                               solidHeader = TRUE,
                               withSpinner(
                                 leafletOutput("wa_fin_mapout", height = 700),
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
                                   style = "display: inline-block;vertical-align:top; width: 150px;",
                                   selectizeInput(
                                     inputId = "select_origin_fin",
                                     label = "Select origin zip",
                                     choices = "",
                                     options = list(
                                       placeholder = 'Please select an option below',
                                       onInitialize = I('function() { this.setValue(""); }')
                                     )
                                   )
                                 ),
                                 tags$div(style = "display: inline-block;vertical-align:top; width: 50px;", HTML("<br>")),
                                 tags$div(
                                   style = "display: inline-block;vertical-align:top; width: 150px;",
                                   selectizeInput(
                                     inputId = "select_destination_fin",
                                     label = "Select destination zip",
                                     choices = "",
                                     options = list(
                                       placeholder = 'Please select an option below',
                                       onInitialize = I('function() { this.setValue(""); }')
                                     )
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
                                  leafletOutput("wa_evse_util_mapout", height = 700),
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
                            )),
                            fluidRow(column(12,
                                            dataTableOutput("evse_table"))))

evse_serve_wait_tab <- bs4TabItem(tabName = "evse_serve_wait",
                                  fluidRow(column(
                                    width = 9,
                                    bs4Card(
                                      title = "EVs Served/Waited",
                                      closable = FALSE,
                                      status = "warning",
                                      collapsible = TRUE,
                                      elevation = 4,
                                      width = NULL,
                                      solidHeader = TRUE,
                                      withSpinner(
                                        leafletOutput("wa_evse_serve_wait_mapout", height = 700),
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
                                        "evse_serve_wait_slider",
                                        label = "Time Range",
                                        min = 0,
                                        max = 23,
                                        value = c(0, 24)
                                      )
                                    ),
                                    bs4Card(
                                      width = 12,
                                      title = "Served / Waited Selector",
                                      closable = FALSE,
                                      radioButtons(
                                        "evse_serve_wait_radio",
                                        label = NULL,
                                        choices = c("Served", "Waited")
                                      )
                                    )
                                  )))

stranded_tab <- bs4TabItem(tabName = "stranded",
                           fluidRow(column(
                             width = 9,
                             bs4Card(
                               title = "Stranded EV Details",
                               closable = FALSE,
                               status = "danger",
                               collapsible = TRUE,
                               labelTooltip = "Stranded EV Details",
                               elevation = 4,
                               width = NULL,
                               solidHeader = TRUE,
                               withSpinner(
                                 leafletOutput("wa_ooc_mapout", height = 700),
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
                                 style = "display: inline-block;vertical-align:top; width: 150px;",
                                 selectizeInput(
                                   inputId = "select_origin_ooc",
                                   label = "Select origin zip",
                                   choices = "",
                                   options = list(
                                     placeholder = 'Please select an option below',
                                     onInitialize = I('function() { this.setValue(""); }')
                                   )
                                 )
                               ),
                               tags$div(style = "display: inline-block;vertical-align:top; width: 50px;", HTML("<br>")),
                               tags$div(
                                 style = "display: inline-block;vertical-align:top; width: 150px;",
                                 
                                 selectizeInput(
                                   inputId = "select_destination_ooc",
                                   label = "Select destination zip",
                                   choices = "",
                                   options = list(
                                     placeholder = 'Please select an option below',
                                     onInitialize = I('function() { this.setValue(""); }')
                                   )
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
    bs4InfoBoxOutput("stranded_count"),
    bs4InfoBoxOutput("evmt_count"),
    bs4InfoBoxOutput("charging_session_count"),
    bs4InfoBoxOutput("evs_waited_count")
    
  )
)

ui <- bs4DashPage(
  navbar = bs4DashNavbar(
    skin = "dark",
    status = "dark",
    border = TRUE,
    sidebarIcon = "bars",
    controlbarIcon = "th",
    fixed = FALSE,
    tags$div(style = "display: inline-block;vertical-align:top; width: 50px;", HTML("<br>")),
    tags$div(style = "display: inline-block;vertical-align:top; width: 20px;", HTML("<br>")),
    tags$div(
      style = "display: inline-block;vertical-align:top; ",
      selectizeInput(
        inputId = "select_datetime",
        label = "Select simulation run datetime",
        choices = " " ,
        options = list(
          placeholder = 'Please select an option below',
          onInitialize = I('function() { this.setValue(""); }')
        )
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
    title = tags$b("Results Viewer"),
    imageOutput("logo2", width = 200, height = 66),
    bs4SidebarMenu(
      id = "sidebar",
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
          text = "EVSE Served/Waited",
          tabName = "evse_serve_wait",
          icon = "thermometer-full"
        ),
        bs4SidebarMenuSubItem(
          text = "Stranded",
          tabName = "stranded",
          icon = "battery-empty"
        )
        
      )
    )
  ),
  footer = bs4DashFooter(bs4DashFooter(
    copyrights = a(
      href = "https://faculty.washington.edu/dwhm/",
      target = "_blank",
      "Chintan Pathak and Don MacKenzie,
            UW"
    ),
    right_text = "2019"
  )),
  title = "Results Viewer",
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
      evse_serve_wait_tab,
      stranded_tab
    )
  )
)

server <- function(input, output, session) {
  rvData <- reactiveValues(
    userid = NULL,
    a_id = NULL,
    simulation_runtime = NULL,
    finished_df = data.frame(),
    stranded_df = data.frame(),
    power_draw_df = data.frame(),
    charging_session_df = data.frame(),
    evse_util_df = data.frame(),
    soc_df_db = data.frame(),
    lat_df_db = data.frame(),
    lng_df_db = data.frame(),
    state_df_db = data.frame(),
    prob_df_db = data.frame(),
    tocharge_df_db = data.frame(),
    trip_scenario_day_df_db = data.frame(),
    od_layer = NULL,
    tp_layer = NULL,
    ev_pass_combo = data.frame(),
    ev_pass_chademo = data.frame(),
    evse_evs_served = data.frame(),
    evse_evs_waiting = data.frame(),
    relevant_df = data.frame(),
    analyses = data.frame(),
    all_chargers_combo = data.frame(),
    all_chargers_chademo = data.frame(),
    evse_dcfc = data.frame()
  )
  
  clearMapOverlay <- function(mapID) {
    print("clearing markers now")
    leafletProxy(mapId = mapID) %>%
      clearGroup(group = "od_points") %>%
      clearGroup(group = "travel_path") %>%
      clearGroup(group = "shortest_path") %>%
      clearGroup(group = "charging_sessions")
  }
  
  clearEVSEmap <- function() {
    print("clearing map now")
    leafletProxy(mapId = "wa_evse_serve_wait_mapout") %>%
      clearGroup(group = "served") %>%
      clearGroup(group = "passed")
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
    } else if (tab_name == "stranded") {
      relevant_df <- rvData$stranded_df
      waypoint_color <- "#b50d2c"
      dest_color <- "#FF5733"
      orig_color <- "#FFC300"
      dest_ui_element <- input$select_destination_ooc
      orig_ui_element <- input$select_origin_ooc
      map_id <- "wa_ooc_mapout"
    }
    
    clearMapOverlay(mapID = map_id)
    
    if (!rapportools::is.empty(dest_ui_element)) {
      relevant_rows <-
        relevant_df[(
          relevant_df$origin_zip == orig_ui_element &
            relevant_df$destination_zip == dest_ui_element
        ), ]
      if (nrow(relevant_rows) >= 1) {
        veh_ids <-
          relevant_rows$veh_id # paste0("X", trimws(finished_row$veh_ID))
        # dt <- as.rvData$lat_df$datetime
        a_id <- rvData$a_id
        ev_info_db_df <-
          DBI::dbGetQuery(
            pool,
            sprintf(
              "select * from ev_info where analysis_id = %d and veh_id IN %s order by info_id",
              a_id,
              paste("(", toString(paste(
                "'", veh_ids, "'", sep = ''
              )), ")", sep = '')
            )
          )
        
        
        # lats_df <-
        #     rvData$lat_df_db %>% dplyr::filter(veh_id == veh_id) %>% dplyr::arrange(lat_id) %>% dplyr::select(lat_val) %>% collect()
        # lngs_df <-
        #     rvData$lng_df_db %>% dplyr::filter(veh_id == veh_id) %>% dplyr::arrange(lng_id) %>% dplyr::select(lng_val) %>% collect()
        #
        info_list <- vector(mode = "list", length = length(veh_ids))
        names(info_list) <- veh_ids
        
        color = grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)]
        
        od_zip <- c(orig_ui_element, dest_ui_element)
        od_zip_details <-
          DBI::dbGetQuery(
            pool,
            paste0(
              "SELECT u.*
FROM  (
   SELECT x.arr[x.rn] AS zip, x.rn
   FROM   (
      SELECT arr, generate_subscripts(arr, 1) AS rn
      FROM  (SELECT '{",
              orig_ui_element,
              ",",
              dest_ui_element,
              "}'::text[]) t(arr)
      ) x
   ) y
JOIN   zipcode_record u USING (zip)
ORDER  BY rn;"
            )
          )
        od_lats <- od_zip_details$latitude
        od_lngs <-
          od_zip_details$longitude
        
        
        cs_df <-
          pool %>% DBI::dbGetQuery(
            sprintf(
              "select * from evse_charging_session where analysis_id = %d and veh_id IN %s",
              a_id,
              paste("(", toString(paste(
                "'", veh_ids, "'", sep = ''
              )), ")", sep = '')
            )
          )
        
        evse_rows <-
          match(cs_df$evse_id, rvData$evse_dcfc$evse_id)
        cs_lats <-
          rvData$evse_dcfc$latitude[evse_rows]
        cs_lngs <-
          rvData$evse_dcfc$longitude[evse_rows]
        
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
            
          )
        
        for (i in veh_ids) {
          # info_list[[i]]
          # df_name <- paste("df", i)
          # assign(df_name, ev_info_db_df[ev_info_db_df$veh_id == i, ])
          
          if (length(veh_ids) > 1) {
            lats <-
              jitter(as.numeric(ev_info_db_df$lat_val[ev_info_db_df$veh_id == i]), amount = 0.01)
            lngs <-
              jitter(as.numeric(ev_info_db_df$lng_val[ev_info_db_df$veh_id == i]), amount = 0.01)
            
          } else {
            lats <-
              as.numeric(ev_info_db_df$lat_val[ev_info_db_df$veh_id == i])
            lngs <-
              as.numeric(ev_info_db_df$lng_val[ev_info_db_df$veh_id == i])
            
          }
          
          # socs_df <- rvData$soc_df_db %>% dplyr::filter(veh_id == veh_id) %>% dplyr::arrange(soc_id) %>% dplyr::select(soc_val) %>% collect()
          socs <-
            paste("SOC:", round(as.numeric(ev_info_db_df$soc_val[ev_info_db_df$veh_id == i]),
                                2))
          # tocharges_df <- rvData$tocharge_df_db %>% dplyr::filter(veh_id == veh_id) %>% dplyr::arrange(tocharge_id) %>% dplyr::select(tocharge_val) %>% collect()
          tocharges <-
            paste("To charge:", ev_info_db_df$tocharge_val[ev_info_db_df$veh_id == i])
          # probs_df <- rvData$prob_df_db %>% dplyr::filter(veh_id == veh_id) %>%  dplyr::arrange(prob_id) %>% dplyr::select(prob_val) %>% collect()
          probs <-
            paste("Probability:", round(as.numeric(ev_info_db_df$prob_val[ev_info_db_df$veh_id == i]),
                                        3))
          # states_df <- rvData$state_df_db %>% dplyr::filter(veh_id == veh_id) %>% dplyr::arrange(state_id) %>% dplyr::select(state_val) %>% collect()
          states <-
            paste("State:", ev_info_db_df$state_val[ev_info_db_df$veh_id == i])
          
          rvData$tp_layer <- rep("travel_path", length(lats))
          
          leafletProxy(mapId = map_id) %>%
            addCircleMarkers(
              lat = lats,
              lng = lngs,
              radius = 4,
              color = sample(color, 1),
              popup = paste(
                sep = "<br>",
                paste("Veh ID: ", i),
                socs,
                tocharges,
                probs,
                states
              ),
              label = paste(
                sep = "\n",
                paste("Veh ID: ", i),
                socs,
                tocharges,
                probs,
                states
              ),
              group = "travel_path",
              stroke = FALSE,
              fillOpacity = 0.5,
              options = pathOptions(pane = "travel_path")
            )
          
          rel_cs_df <- cs_df[cs_df$veh_id == i, ]
          evse_rows <-
            match(rel_cs_df$evse_id, rvData$evse_dcfc$evse_id)
          cs_lats <-
            rvData$evse_dcfc$latitude[evse_rows]
          cs_lngs <-
            rvData$evse_dcfc$longitude[evse_rows]
          
          # Overlay charging session info
          if (nrow(rel_cs_df) >= 1) {
            leafletProxy(mapId = map_id) %>%
              addCircleMarkers(
                lat = cs_lats,
                lng = cs_lngs,
                radius = 12,
                group = "charging_sessions",
                popup = paste(
                  sep = "<br>",
                  paste("Vehicle ID: ", i),
                  paste("Charging session: ",
                        row.names(rel_cs_df)),
                  paste("Starting SOC:",
                        round(
                          as.numeric(rel_cs_df$starting_soc), 2
                        )),
                  paste("Ending SOC:",
                        round(
                          as.numeric(rel_cs_df$ending_soc)
                        ))
                ),
                label = row.names(rel_cs_df),
                labelOptions = labelOptions(noHide = T, textsize = "18px"),
                stroke = FALSE,
                fillOpacity = 0.8,
                options = pathOptions(pane = "charging_sessions")
              )
          }
          
          
        }
        
        
        
        # ev_info_df <-
        #     data.frame(socs,
        #                tocharges,
        #                probs,
        #                states,
        #                stringsAsFactors = FALSE)
        
        # trip_row <-
        #     rvData$trip_scenario_day_df[which(rvData$trip_scenario_day_df$veh_id == trimws(relevant_row$veh_id)), ]
        
        # od_zip_details <-
        #     dplyr::tbl(main_con, "zipcode_record") %>% filter(zip %in% c(orig_ui_element, dest_ui_element)) %>% collect()
        
        
        # cs_df <- DBI::dbGetQuery(main_con, paste0("select * from evse_charging_session where veh_id = '", veh_id, "' and analysis_id = ", a_id))
        # cs_df <-
        #   pool %>% DBI::dbGetQuery(
        #     paste0(
        #       "select * from evse_charging_session where veh_id = '",
        #       veh_id,
        #       "' and analysis_id = ",
        #       a_id
        #     )
        #   )
        #
        # evse_rows <-
        #   match(cs_df$evse_id, rvData$evse_dcfc$evse_id)
        # cs_lats <-
        #   rvData$evse_dcfc$latitude[evse_rows]
        # cs_lngs <-
        #   rvData$evse_dcfc$longitude[evse_rows]
        #
        # leafletProxy(mapId = map_id) %>%
        #   addMapPane(name = "od_points", zIndex = 410) %>%
        #   addMapPane(name = "travel_path", zIndex = 490) %>%
        #   addMapPane(name = "charging_sessions", zIndex = 491) %>%
        #   addCircleMarkers(
        #     lat = od_lats[1],
        #     lng = od_lngs[1],
        #     radius = 12,
        #     color = orig_color,
        #     group = "od_points",
        #     stroke = FALSE,
        #     fillOpacity = 0.5,
        #     label = paste0("Origin: ", orig_ui_element),
        #     labelOptions = labelOptions(noHide = T),
        #     options = pathOptions(pane = "od_points")
        #
        #   ) %>% addCircleMarkers(
        #     lat = od_lats[2],
        #     lng = od_lngs[2],
        #     radius = 12,
        #     color = dest_color,
        #     group = "od_points",
        #     stroke = FALSE,
        #     fillOpacity = 0.5,
        #     label = paste0("Destination: ",
        #                    dest_ui_element),
        #     labelOptions = labelOptions(noHide = T),
        #     options = pathOptions(pane = "od_points")
        #
        #   ) %>%
        #   addCircleMarkers(
        #     lat = lats,
        #     lng = lngs,
        #     radius = 4,
        #     color = waypoint_color,
        #     popup = paste(sep = "<br>",
        #                   socs,
        #                   tocharges,
        #                   probs,
        #                   states),
        #     label = paste(sep = "\n",
        #                   socs,
        #                   tocharges,
        #                   probs,
        #                   states),
        #     group = "travel_path",
        #     stroke = FALSE,
        #     fillOpacity = 0.5,
        #     options = pathOptions(pane = "travel_path")
        #   )
        #
        # # Overlay charging session info
        # if (nrow(cs_df) >= 1) {
        #   leafletProxy(mapId = map_id) %>%
        #     addCircleMarkers(
        #       lat = cs_lats,
        #       lng = cs_lngs,
        #       radius = 12,
        #       group = "charging_sessions",
        #       popup = paste(
        #         sep = "<br>",
        #         "Charging session:",
        #         row.names(cs_df),
        #         "Starting SOC:",
        #         round(as.numeric(cs_df$starting_soc), 2),
        #         "Ending SOC:",
        #         round(as.numeric(cs_df$ending_soc))
        #       ),
        #       label = row.names(cs_df),
        #       labelOptions = labelOptions(noHide = T, textsize = "18px"),
        #       stroke = FALSE,
        #       fillOpacity = 0.8,
        #       options = pathOptions(pane = "charging_sessions")
        #     )
      }
      
    }
    
  }
  
  
  # Get the user id of the session from the URL
  observe({
    query <- parseQueryString(session$clientData$url_search)
    if (!is.null(query[['userid']])) {
      rvData$userid  <- query[['userid']]
      userid <- query[['userid']]
      # Get the rows from analysis_record matching the userid that are solved
      # rvData$analyses <-
      #     dplyr::tbl(main_con, "analysis_record") %>% dplyr::filter(user_id == userid &
      #                                                                   status == "solved") %>% collect()
      rvData$analyses <-
        pool %>% dplyr::tbl("analysis_record") %>% dplyr::filter(user_id == userid &
                                                                   status == "solved") %>% collect()
      updateSelectInput(session,
                        inputId = "select_datetime",
                        choices = rvData$analyses$sim_date_time)
      
    }
  })
  
  output$wa_fin_mapout <- renderLeaflet({
    if (!rapportools::is.empty(rvData$simulation_runtime)) {
      wa_map %>%
        addMarkers(
          lng = ~ longitude ,
          lat = ~ latitude,
          icon = evse_icon_blue,
          group = base_layers[1],
          data = rvData$all_chargers_combo
        )  %>%
        addMarkers(
          lng = ~ longitude ,
          lat = ~ latitude,
          icon = evse_icon_blue,
          group = base_layers[2],
          data = rvData$all_chargers_chademo
        ) %>%
        addLabelOnlyMarkers(
          lng = ~ longitude,
          lat = ~ latitude,
          data = dplyr::filter(rvData$evse_dcfc, grepl('n', evse_id)),
          label = "new",
          group = "new_labels",
          labelOptions = leaflet::labelOptions(
            noHide = TRUE,
            direction = "bottom",
            textOnly = TRUE,
            offset = c(0, -10),
            opacity = 1,
            style = list(
              "color" = "red",
              "font-family" = "serif",
              "font-style" = "italic",
              "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
              "font-size" = "15px",
              "border-color" = "rgba(0,0,0,0.5)"
            )
          )
        ) %>%
        addLayersControl(baseGroups = base_layers,
                         options = layersControlOptions(collapsed = FALSE))
    } else {
      showModal(
        modalDialog(
          size = "s",
          easyClose = TRUE,
          "Select a simulation run time to see the finished vehicles."
        )
      )
    }
  })
  
  output$wa_ooc_mapout <- renderLeaflet({
    od_str <- NULL
    if (!rapportools::is.empty(rvData$simulation_runtime)) {
      wa_map %>%
        addMarkers(
          lng = ~ longitude ,
          lat = ~ latitude,
          icon = evse_icon_blue,
          group = base_layers[1],
          data = rvData$all_chargers_combo
        )  %>%
        addMarkers(
          lng = ~ longitude ,
          lat = ~ latitude,
          icon = evse_icon_blue,
          group = base_layers[2],
          data = rvData$all_chargers_chademo
        ) %>%
        addCircleMarkers(
          lat = rvData$stranded_df$stranded_lat,
          lng = rvData$stranded_df$stranded_lng,
          radius = 4,
          color = "#D5696F",
          popup =                     paste(
            rvData$stranded_df$origin_zip,
            rvData$stranded_df$destination_zip,
            sep = "->"
          ),
          label =                     paste(
            rvData$stranded_df$origin_zip,
            rvData$stranded_df$destination_zip,
            sep = "->"
          ),
          stroke = FALSE,
          fillOpacity = 0.5
        ) %>%
        addLabelOnlyMarkers(
          lng = ~ longitude,
          lat = ~ latitude,
          data = dplyr::filter(rvData$evse_dcfc, grepl('n', evse_id)),
          label = "new",
          group = "new_labels",
          labelOptions = leaflet::labelOptions(
            noHide = TRUE,
            direction = "bottom",
            textOnly = TRUE,
            offset = c(0, -10),
            opacity = 1,
            style = list(
              "color" = "red",
              "font-family" = "serif",
              "font-style" = "italic",
              "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
              "font-size" = "15px",
              "border-color" = "rgba(0,0,0,0.5)"
            )
          )
        ) %>%
        addLayersControl(baseGroups = base_layers,
                         options = layersControlOptions(collapsed = FALSE))
    } else {
      showModal(
        modalDialog(
          size = "s",
          easyClose = TRUE,
          "Select a simulation run time to see the stranded vehicles."
        )
      )
    }
    
    # print("After markers")
  })
  
  output$wa_evse_util_mapout <- renderLeaflet({
    if (!rapportools::is.empty(rvData$simulation_runtime)) {
      wa_map %>%
        addMapPane(name = "chargers", zIndex = 410) %>%
        addMapPane(name = "overlay", zIndex = 491) %>%
        addMarkers(
          lng = ~ longitude ,
          lat = ~ latitude,
          layerId = ~ paste0("co", evse_id),
          icon = evse_icon_blue,
          group = base_layers[1],
          data = rvData$all_chargers_combo,
          options = pathOptions(pane = "chargers")
        )  %>%
        addMarkers(
          lng = ~ longitude ,
          lat = ~ latitude,
          layerId = ~ paste0("ch", evse_id),
          icon = evse_icon_blue,
          group = base_layers[2],
          data = rvData$all_chargers_chademo,
          options = pathOptions(pane = "chargers")
        ) %>%
        addLabelOnlyMarkers(
          lng = ~ longitude,
          lat = ~ latitude,
          data = dplyr::filter(rvData$evse_dcfc, grepl('n', evse_id)),
          label = "new",
          group = "new_labels",
          labelOptions = leaflet::labelOptions(
            noHide = TRUE,
            direction = "bottom",
            textOnly = TRUE,
            offset = c(0, -10),
            opacity = 1,
            style = list(
              "color" = "red",
              "font-family" = "serif",
              "font-style" = "italic",
              "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
              "font-size" = "15px",
              "border-color" = "rgba(0,0,0,0.5)"
            )
          )
        ) %>%
        addLayersControl(baseGroups = base_layers,
                         options = layersControlOptions(collapsed = FALSE))
    } else {
      showModal(
        modalDialog(
          size = "s",
          easyClose = TRUE,
          "Select a simulation run time to see the EVSE utilization."
        )
      )
    }
    
  })
  
  output$wa_evse_serve_wait_mapout <- renderLeaflet({
    if (!rapportools::is.empty(rvData$simulation_runtime)) {
      range_start_time <-
        as.POSIXct(paste(
          simulated_date,
          paste(input$evse_serve_wait_slider[1], 0, 0, sep = ":")
        ),
        tz = "Etc/GMT+8",
        format = "%Y-%m-%d %H:%M:%S")
      range_end_time <-
        as.POSIXct(paste(
          simulated_date,
          paste(input$evse_serve_wait_slider[2], 0, 0, sep = ":")
        ),
        tz = "Etc/GMT+8",
        format = "%Y-%m-%d %H:%M:%S")
      
      if (input$evse_serve_wait_radio == "Waited") {
        evs_waited_df_tw <-
          rvData$evs_waiting_df %>% dplyr::mutate(datetime = as.POSIXct(
            wait_start_time,
            origin = as.POSIXct("1970-01-01", tz = "Etc/GMT+8"),
            tz = "Etc/GMT+8"
          )) %>%
          dplyr::filter(datetime >= range_start_time &
                          datetime <= range_end_time) %>% dplyr::group_by(evse_id) %>% dplyr::summarise(count = n())
        
        evs_waited_df_tw_combo <-
          merge(
            evs_waited_df_tw,
            rvData$all_chargers_combo,
            by = "evse_id",
            all.x = TRUE
          )
        evs_waited_df_tw_chademo <-
          merge(
            evs_waited_df_tw,
            rvData$all_chargers_chademo,
            by = "evse_id",
            all.x = TRUE
          )
        overlay_text <- "Waited: "
        overlay_color <- "#b50d2c"
        overlay_combo <- na.omit(evs_waited_df_tw_combo)
        overlay_chademo <- na.omit(evs_waited_df_tw_chademo)
      } else if (input$evse_serve_wait_radio == "Served") {
        evs_served_df_tw <-
          rvData$charging_session_df %>% dplyr::mutate(datetime = as.POSIXct(
            charge_start_time,
            origin = as.POSIXct("1970-01-01", tz = "Etc/GMT+8"),
            tz = "Etc/GMT+8"
          )) %>%
          dplyr::filter(datetime >= range_start_time &
                          datetime <= range_end_time) %>% dplyr::group_by(evse_id) %>% dplyr::summarise(count = n())
        
        evs_served_df_tw_combo <-
          merge(
            evs_served_df_tw,
            rvData$all_chargers_combo,
            by = "evse_id",
            all.x = TRUE
          )
        evs_served_df_tw_chademo <-
          merge(
            evs_served_df_tw,
            rvData$all_chargers_chademo,
            by = "evse_id",
            all.x = TRUE
          )
        overlay_text <- "Served: "
        overlay_color <- "#75d654"
        overlay_combo <- na.omit(evs_served_df_tw_combo)
        overlay_chademo <- na.omit(evs_served_df_tw_chademo)
      }
      
      if (nrow(overlay_combo) > 0 & nrow(overlay_chademo) > 0) {
        wa_map %>%
          addMapPane(name = "chargers", zIndex = 410) %>%
          addMapPane(name = "overlay", zIndex = 491) %>%
          addMarkers(
            lng = rvData$all_chargers_combo$longitude ,
            lat = rvData$all_chargers_combo$latitude,
            icon = evse_icon_blue,
            group = base_layers[1],
            data = rvData$all_chargers_combo,
            options = pathOptions(pane = "chargers")
          )  %>%
          addMarkers(
            lng = rvData$all_chargers_chademo$longitude ,
            lat = rvData$all_chargers_chademo$latitude,
            icon = evse_icon_blue,
            group = base_layers[2],
            data = rvData$all_chargers_chademo,
            options = pathOptions(pane = "chargers")
          ) %>%
          addLabelOnlyMarkers(
            lng = ~ longitude,
            lat = ~ latitude,
            data = dplyr::filter(rvData$evse_dcfc, grepl('n', evse_id)),
            label = "new",
            group = "new_labels",
            labelOptions = leaflet::labelOptions(
              noHide = TRUE,
              direction = "bottom",
              textOnly = TRUE,
              offset = c(0, -10),
              opacity = 1,
              style = list(
                "color" = "red",
                "font-family" = "serif",
                "font-style" = "italic",
                "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                "font-size" = "15px",
                "border-color" = "rgba(0,0,0,0.5)"
              )
            )
          ) %>%
          addLabelOnlyMarkers(
            lng = overlay_combo$longitude,
            lat = overlay_combo$latitude,
            label = as.character(overlay_combo$count),
            group = base_layers[1],
            labelOptions = leaflet::labelOptions(
              noHide = TRUE,
              direction = "bottom",
              textOnly = TRUE,
              offset = c(0, -10),
              opacity = 1
            )
          ) %>%  addLabelOnlyMarkers(
            lng = overlay_chademo$longitude,
            lat = overlay_chademo$latitude,
            label = as.character(overlay_chademo$count),
            group = base_layers[2] ,
            labelOptions = leaflet::labelOptions(
              noHide = TRUE,
              direction = "bottom",
              textOnly = TRUE,
              offset = c(0, -10),
              opacity = 1
            )
          ) %>%
          addCircleMarkers(
            lng = overlay_combo$longitude,
            lat = overlay_combo$latitude,
            radius = log(100 * overlay_combo$count),
            group = base_layers[1],
            color = overlay_color,
            popup = paste0(overlay_text, overlay_combo$count),
            labelOptions = leaflet::labelOptions(
              noHide = TRUE,
              direction = "bottom",
              textOnly = TRUE,
              offset = c(0, -10),
              opacity = 1
            )
          ) %>%  addCircleMarkers(
            lng = overlay_chademo$longitude,
            lat = overlay_chademo$latitude,
            radius = log(100 * overlay_chademo$count),
            group = base_layers[2],
            color = overlay_color,
            popup = paste0(overlay_text, overlay_chademo$count),
            labelOptions = leaflet::labelOptions(
              noHide = TRUE,
              direction = "bottom",
              textOnly = TRUE,
              offset = c(0, -10),
              opacity = 1
            )
          ) %>%
          addLayersControl(baseGroups = base_layers,
                           options = layersControlOptions(collapsed = FALSE))
      }
      else
        (showModal(
          modalDialog(
            size = "s",
            easyClose = TRUE,
            "No data found. Select a wider time window."
          )
        ))
    } else {
      showModal(
        modalDialog(
          size = "s",
          easyClose = TRUE,
          "Select a simulation run time to see the EVs served / waited"
        )
      )
    }
    
  })
  
  observeEvent(input$wa_evse_util_mapout_marker_click, {
    id = substr(
      input$wa_evse_util_mapout_marker_click$id,
      3,
      nchar(input$wa_evse_util_mapout_marker_click$id)
    )
    print(id)
    ## TODO:
    ### Make the data filter with time
    ### Add other indication to the map, so we dont have to click each and every charger
    ### maybe evs_served
    
    if (!rapportools::is.empty(rvData$simulation_runtime)) {
      range_start_time <-
        as.POSIXct(paste(
          simulated_date,
          paste(input$evse_util_slider[1], 0, 0, sep = ":")
        ),
        tz = "Etc/GMT+8",
        format = "%Y-%m-%d %H:%M:%S")
      range_end_time <-
        as.POSIXct(paste(
          simulated_date,
          paste(input$evse_util_slider[2], 0, 0, sep = ":")
        ),
        tz = "Etc/GMT+8",
        format = "%Y-%m-%d %H:%M:%S")
      
      power_draw_evse <-
        rvData$power_draw_df %>% dplyr::filter(evse_id == paste0(id, ".0")) %>% dplyr::arrange(pd_id) %>% collect() %>% dplyr::mutate(datetime = as.POSIXct(
          simulation_ts,
          origin = as.POSIXct("1970-01-01", tz = "Etc/GMT+8"),
          tz = "Etc/GMT+8"
        )) %>%
        dplyr::filter(datetime >= range_start_time &
                        datetime <= range_end_time)
      
      
      # evse_util <-
      #     round(rvData$evse_util_df$energy_consumed[which(rvData$evse_util_df$evse_id == id)], 2)
      # Evse util is Int(P)dt
      evse_util <-
        sum(as.numeric(power_draw_evse$power_val)) / 60
      chademo_count <-
        rvData$all_chargers_chademo$dcfc_count[rvData$all_chargers_chademo$evse_id == id]
      combo_count <-
        rvData$all_chargers_combo$dcfc_count[rvData$all_chargers_combo$evse_id == id]
      relevant_charging_sessions <-
        rvData$charging_session_df[which(rvData$charging_session_df$evse_id == paste0(id, ".0")), ]
      relevant_charging_sessions_tw <-
        relevant_charging_sessions %>% dplyr::mutate(datetime = as.POSIXct(
          charge_start_time,
          origin = as.POSIXct("1970-01-01", tz = "Etc/GMT+8"),
          tz = "Etc/GMT+8"
        )) %>%
        dplyr::filter(datetime >= range_start_time &
                        datetime <= range_end_time)
      evs_charged <-
        nrow(relevant_charging_sessions_tw)
      
      relevant_evs_waiting <-
        rvData$evs_waiting_df[which(rvData$evs_waiting_df$evse_id == paste0(id, ".0")), ]
      relevant_evs_waiting_tw <-
        relevant_evs_waiting %>% dplyr::mutate(datetime = as.POSIXct(
          wait_start_time,
          origin = as.POSIXct("1970-01-01", tz = "Etc/GMT+8"),
          tz = "Etc/GMT+8"
        )) %>%
        dplyr::filter(datetime >= range_start_time &
                        datetime <= range_end_time)
      
      evs_waiting <-
        nrow(relevant_evs_waiting_tw)
      
      if (nrow(power_draw_evse) >= 1) {
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
                  "# EVs Waited"
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
                               evs_waiting)
                )
              )
            ),
            renderPlot(
              plot(
                x = power_draw_evse$datetime,
                y = power_draw_evse$power_val,
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
          "Select a simulation run time to see the EVSE utilization."
        )
      )
    }
  })
  
  session$onFlushed(function() {
    
  })
  
  # When the date is selected, the simulated date time dropdown is populated
  # observeEvent(input$select_date, {
  #     print("Date selected")
  #
  #     rvData$simulated_date <- input$select_date
  #
  #     updateSelectInput(session,
  #                       inputId = "select_datetime",
  #                       choices =  sim_df$sim_times[which(sim_df$sim_dates == input$select_date)])
  # })
  
  # When origin zip is selected the corresponding destination dropdown is populated
  observeEvent(input$select_origin_ooc, {
    if (!rapportools::is.empty(input$select_origin_ooc)) {
      updateSelectInput(
        session,
        inputId = 'select_destination_ooc',
        choices = sort(rvData$stranded_df$destination_zip[which(rvData$stranded_df$origin_zip == input$select_origin_ooc)])
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
    plot_trajectory("stranded")
  })
  
  observeEvent(input$select_datetime, {
    print("Datetime selected")
    
    rvData$simulation_runtime <- input$select_datetime
    if (!rapportools::is.empty(rvData$simulation_runtime)) {
      leafletProxy(mapId = 'wa_fin_mapout') %>%
        clearGroup(group = "new_labels")
      leafletProxy(mapId = 'wa_ooc_mapout') %>%
        clearGroup(group = "new_labels")
      leafletProxy(mapId = 'wa_evse_serve_pass_mapout') %>%
        clearGroup(group = "new_labels")
      leafletProxy(mapId = 'wa_evse_util_mapout') %>%
        clearGroup(group = "new_labels")
      
      a_id <-
        rvData$a_id <-
        rvData$analyses$analysis_id[rvData$analyses$sim_date_time == rvData$simulation_runtime]
      
      rvData$power_draw_df <-
        pool %>% dplyr::tbl("evse_power_draw") %>% filter(analysis_id == a_id)
      
      rvData$stranded_df <-
        pool %>% dplyr::tbl("ev_stranded") %>% filter(analysis_id == a_id) %>% collect()
      
      rvData$finished_df <-
        pool %>% dplyr::tbl("ev_finished") %>% filter(analysis_id == a_id) %>% collect()
      
      # rvData$evs_passed_df <-
      #   pool %>% dplyr::tbl("evse_evs_passed") %>% filter(analysis_id == a_id) %>% collect()
      rvData$evs_waiting_df <-
        pool %>% dplyr::tbl("evse_evs_waiting") %>% filter(analysis_id == a_id) %>% collect()
      
      updateSelectInput(
        session,
        inputId = 'select_origin_ooc',
        choices = sort(rvData$stranded_df$origin_zip)
      )
      
      updateSelectInput(
        session,
        inputId = 'select_origin_fin',
        choices = sort(rvData$finished_df$origin_zip)
      )
      
      # output$vehicle_count <- renderbs4InfoBox({
      #     bs4InfoBox(
      #         value = DBI::dbGetQuery(
      #             main_con,
      #             paste0(
      #                 'select count(*) from evtrip_scenarios where analysis_id = ',
      #                 a_id
      #             )
      #         ) ,
      #         title = "EVs in Simulation",
      #         icon = "layer-group"
      #     )
      # })
      ev_count <- DBI::dbGetQuery(pool,
                                  paste0(
                                    'select count(*) from evtrip_scenarios where analysis_id = ',
                                    a_id
                                  ))
      output$vehicle_count <- renderbs4InfoBox({
        bs4InfoBox(value = ev_count$count
                   ,
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
      
      output$stranded_count <- renderbs4InfoBox({
        bs4InfoBox(
          value = nrow(rvData$stranded_df),
          title = "EVs stranded",
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
      
      # cs_count <-
      #     as.character(DBI::dbGetQuery(
      #         main_con,
      #         paste0(
      #             'select count(*) from evse_charging_session where analysis_id = ',
      #             a_id
      #         )
      #     )$count)
      
      rvData$charging_session_df <- DBI::dbGetQuery(
        pool,
        paste0(
          'select evse_id, charge_start_time from evse_charging_session where analysis_id = ',
          a_id
        )
      )
      
      cs_count <-
        as.character(nrow(rvData$charging_session_df))
      
      output$charging_session_count <- renderbs4InfoBox({
        bs4InfoBox(value = cs_count,
                   title = "Number of charging sessions",
                   icon = "charging-station")
      })
      
      output$evs_waited_count <- renderbs4InfoBox({
        bs4InfoBox(
          value = nrow(rvData$evs_waiting_df),
          title = "Number of EVs waiting",
          icon = "square-full"
        )
      })
      
      nevse_query <-
        paste0(
          "SELECT concat('n', nevse_id) as evse_id, latitude, longitude, dcfc_plug_count as dcfc_count, connector_code from new_evses where dcfc_plug_count > 0 and analysis_id = ",
          a_id
        )
      # nevse_dcfc <- DBI::dbGetQuery(main_con, nevse_query)
      nevse_dcfc <- DBI::dbGetQuery(pool, nevse_query)
      
      leafletProxy(mapId = 'wa_evse_util_mapout') %>%
        addLabelOnlyMarkers(
          lng = nevse_dcfc$longitude,
          lat = nevse_dcfc$latitude,
          label = "new",
          group = "new_labels",
          labelOptions = leaflet::labelOptions(
            noHide = TRUE,
            direction = "bottom",
            textOnly = TRUE,
            offset = c(0, -10),
            opacity = 1
          )
        )
      
      leafletProxy(mapId = 'wa_evse_serve_wait_mapout') %>%
        addLabelOnlyMarkers(
          lng = nevse_dcfc$longitude,
          lat = nevse_dcfc$latitude,
          label = "new",
          group = "new_labels",
          labelOptions = leaflet::labelOptions(
            noHide = TRUE,
            direction = "bottom",
            textOnly = TRUE,
            offset = c(0, -10),
            opacity = 1
          )
        )
      
      leafletProxy(mapId = 'wa_fin_mapout') %>%
        addLabelOnlyMarkers (
          lng = nevse_dcfc$longitude,
          lat = nevse_dcfc$latitude,
          label = "new",
          group = "new_labels",
          labelOptions = leaflet::labelOptions(
            noHide = TRUE,
            direction = "bottom",
            textOnly = TRUE,
            offset = c(0, -10),
            opacity = 1
          )
        )
      
      leafletProxy(mapId = 'wa_ooc_mapout') %>%
        addLabelOnlyMarkers(
          lng = nevse_dcfc$longitude,
          lat = nevse_dcfc$latitude,
          label = "new",
          group = "new_labels",
          labelOptions = leaflet::labelOptions(
            noHide = TRUE,
            direction = "bottom",
            textOnly = TRUE,
            offset = c(0, -10),
            opacity = 1
          )
        )
      
      rvData$evse_dcfc <-
        rbind(bevse_dcfc, nevse_dcfc) %>% dplyr::filter(connector_code < 4)
      
      rvData$all_chargers_combo <-
        as.data.frame(rvData$evse_dcfc[rvData$evse_dcfc$connector_code == 2 |
                                         rvData$evse_dcfc$connector_code == 3,])
      
      rvData$all_chargers_chademo <-
        as.data.frame(rvData$evse_dcfc[rvData$evse_dcfc$connector_code == 1 |
                                         rvData$evse_dcfc$connector_code == 3,])
      
    }
    
  })
  
  if.is.empty <- function(x) {
    is.null(need(x, message = FALSE))
  }
  
  output$logo2 <- renderImage({
    list(
      src = "data-raw/logo2.png",
      width = 200,
      height = 66,
      contentType = "image/png",
      alt = "logo"
    )
  }, deleteFile = FALSE)
  
  output$evse_table <- renderDataTable({
    DT::datatable(rvData$evse_dcfc,
                  selection = "single",
                  options = list(stateSave = TRUE))
  })
  
  # to keep track of previously selected row
  prev_row <- reactiveVal()
  
  observeEvent(input$evse_table_rows_selected, {
    row_selected = rvData$evse_dcfc[input$evse_table_rows_selected, ]
    if (row_selected$connector_code == 1 |
        row_selected$connector_code == 3) {
      layer_id <- paste0("ch", row_selected$evse_id)
    } else if (row_selected$connector_code == 2 |
               row_selected$connector_code == 3) {
      layer_id <- paste0("co", row_selected$evse_id)
    }
    proxy <- leafletProxy('wa_evse_util_mapout')
    print(row_selected)
    proxy %>%
      addMarkers(
        popup = as.character(row_selected$evse_id),
        layerId = layer_id,
        group = as.character(row_selected$evse_id),
        lng = row_selected$longitude,
        lat = row_selected$latitude
      )
    
    # Reset previously selected marker
    if (!is.null(prev_row()))
    {
      proxy %>%
        clearGroup(group = as.character(prev_row()$evse_id))
    }
    # set new value to reactiveVal
    prev_row(row_selected)
  })
  
}


shiny::shinyApp(ui, server)