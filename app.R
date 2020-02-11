source("global.R")

###################
### UI ###########
####################

evse_util_tab <- bs4TabItem(tabName = "evse_util",
                            fluidRow(column(
                              6,
                              bs4Card(
                                title = "List of DCFCs",
                                closable = FALSE,
                                status = "success",
                                collapsible = TRUE,
                                elevation = 4,
                                width = NULL,
                                solidHeader = TRUE,
                                dataTableOutput("evse_table")
                              )
                            ),
                            column(
                              6,
                              bs4Accordion(
                                id = "evse_wait_serve_accordion",
                                bs4AccordionItem(
                                  id = "evse_wait",
                                  title =
                                    
                                    actionBttn(
                                      inputId = "evse_wait_btn",
                                      label = "Waiting",
                                      style = "material-flat",
                                      color = "danger"
                                    ),
                                  
                                  status = "sucess",
                                  dataTableOutput("ws_table")
                                ),
                                bs4AccordionItem(
                                  id = "evse_serve",
                                  title = actionBttn(
                                    inputId = "evse_serve_btn",
                                    label = "Serving",
                                    style = "material-flat",
                                    color = "success"
                                  ),
                                  status = "warning",
                                  dataTableOutput("cs_table")
                                )
                              )
                              # fluidRow(
                              #   column(
                              #     12,
                              # bs4Card(
                              #   title = "Charging Sessions",
                              #   closable = FALSE,
                              #   status = "success",
                              #   collapsible = TRUE,
                              #   elevation = 4,
                              #   width = NULL,
                              #   solidHeader = TRUE,
                              #   dataTableOutput("cs_table")
                              # )
                              #   )
                              # ),
                              # fluidRow(
                              #   column(
                              #     12,
                              #     bs4Card(
                              #       title = "Waiting Sessions",
                              #       closable = FALSE,
                              #       status = "danger",
                              #       collapsible = TRUE,
                              #       elevation = 4,
                              #       width = NULL,
                              #       solidHeader = TRUE,
                              #       dataTableOutput("ws_table")
                              #     )
                              #   )
                            )),
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
                                maximizable = TRUE,
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
                             4,
                             bs4Card(
                               title = "List of EVs",
                               closable = FALSE,
                               status = "primary",
                               collapsible = TRUE,
                               elevation = 4,
                               width = NULL,
                               solidHeader = TRUE,
                               maximizable = TRUE,
                               dataTableOutput("ev_table")
                             )
                           ),
                           column(
                             8,
                             bs4Card(
                               title = "EV trajectory",
                               closable = FALSE,
                               status = "success",
                               collapsible = TRUE,
                               labelTooltip = "EV trajectory",
                               elevation = 4,
                               width = NULL,
                               solidHeader = TRUE,
                               maximizable = TRUE,
                               withSpinner(
                                 leafletOutput("wa_ooc_mapout", height = 700),
                                 type = 8,
                                 color = "#0dc5c1"
                               )
                             )
                           )),
                           fluidRow(column(
                             12,
                             bs4Card(
                               title = "EV Trajectory Info",
                               closable = FALSE,
                               status = "primary",
                               collapsible = TRUE,
                               elevation = 4,
                               width = NULL,
                               solidHeader = TRUE,
                               maximizable = TRUE,
                               dataTableOutput("ev_info_table")
                             )
                           )))

roads_tab <- bs4TabItem(tabName = "roads",
                        fluidRow(column(
                          6,
                          bs4Card(
                            title = "List of roads",
                            closable = FALSE,
                            status = "success",
                            collapsible = TRUE,
                            elevation = 4,
                            width = NULL,
                            solidHeader = TRUE,
                            dataTableOutput("road_table")
                          )
                        )),
                        fluidRow(column(
                          width = 12,
                          bs4Card(
                            title = "WSDOT Road Network",
                            closable = FALSE,
                            status = "success",
                            collapsible = TRUE,
                            labelTooltip = "WSDOT Road Network",
                            elevation = 4,
                            width = NULL,
                            solidHeader = TRUE,
                            withSpinner(
                              leafletOutput("wa_road_mapout", height = 700),
                              type = 8,
                              color = "#0dc5c1"
                            )
                          )
                        )))

summary_tab <- bs4TabItem(
  tabName = "summary",
  h4("Simulation Summary"),
  fluidRow(
    bs4InfoBoxOutput("vehicle_count"),
    bs4InfoBoxOutput("finished_count"),
    bs4InfoBoxOutput("stranded_count"),
    bs4InfoBoxOutput("evmt_count"),
    bs4InfoBoxOutput("charging_session_count"),
    bs4InfoBoxOutput("evs_waited_count")
    
  ),
  fluidRow(column(
    6,
    bs4Card(
      title = "Overall EVSE Utilization",
      closable = FALSE,
      status = "success",
      collapsible = TRUE,
      labelTooltip = "Overall EVSE Utilization",
      elevation = 4,
      width = NULL,
      solidHeader = TRUE,
      maximizable = TRUE,
      plotOutput("stat_evse_util_plot")
    )
    
  ),
  column(
    6,
    bs4Card(
      title = "Wait-time Distribution",
      closable = FALSE,
      status = "danger",
      collapsible = TRUE,
      labelTooltip = "Wait-time Distribution",
      elevation = 4,
      width = NULL,
      solidHeader = TRUE,
      maximizable = TRUE,
      plotOutput("stat_evse_wait_plot")
    )
  )),
  fluidRow(column(
    6,
    bs4Card(
      title = "Charge-time Distribution",
      closable = FALSE,
      status = "success",
      collapsible = TRUE,
      labelTooltip = "Charge-time Distribution",
      elevation = 4,
      width = NULL,
      solidHeader = TRUE,
      maximizable = TRUE,
      plotOutput("stat_cs_plot")
    )
    
  ))
  
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
      # withAnim(),
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
    title = tags$b("EVI-ABM Results Viewer"),
    imageOutput("logo2", width = 200, height = 66),
    bs4SidebarMenu(
      id = "sidebar",
      bs4SidebarMenuItem(
        text = "View Results",
        icon = "poll",
        startExpanded = TRUE,
        bs4SidebarMenuSubItem(
          text = "Summary",
          tabName = "summary",
          icon = "chart-bar"
        ),
        # bs4SidebarMenuSubItem(
        #   text = "BEVs",
        #   tabName = "finished",
        #   icon = "route"
        # ),
        
        # bs4SidebarMenuSubItem(
        #   text = "EVSE Served/Waited",
        #   tabName = "evse_serve_wait",
        #   icon = "thermometer-full"
        # ),
        bs4SidebarMenuSubItem(
          text = "BEVs",
          tabName = "stranded",
          icon = "car"
        ),
        bs4SidebarMenuSubItem(
          text = "EVSEs",
          tabName = "evse_util",
          icon = "charging-station"
        )
        # bs4SidebarMenuSubItem(
        #   text = "Roads",
        #   tabName = "roads",
        #   icon = "route"
        # )
        
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
  title = "EVI-ABM Results Viewer",
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
    bs4TabItems(summary_tab,
                # finished_tab,
                evse_util_tab,
                # evse_serve_wait_tab,
                stranded_tab,
                roads_tab)
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
    evse_dcfc = data.frame(),
    bevs = data.frame()
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
  
  plot_trajectory <- function(row_df) {
    # if (tab_name == "finished") {
    #   relevant_df <- rvData$finished_df
    #   waypoint_color <- "#75d654"
    #   dest_color <- "#420db5"
    #   orig_color <- "#960db5"
    #   dest_ui_element <- input$select_destination_fin
    #   orig_ui_element <- input$select_origin_fin
    #   map_id <- "wa_fin_mapout"
    # } else if (tab_name == "stranded") {
    #   relevant_df <- rvData$stranded_df
    #   waypoint_color <- "#b50d2c"
    #   dest_color <- "#FF5733"
    #   orig_color <- "#FFC300"
    #   dest_ui_element <- input$select_destination_ooc
    #   orig_ui_element <- input$select_origin_ooc
    #   map_id <- "wa_ooc_mapout"
    # }
    #
    # clearMapOverlay(mapID = map_id)
    #
    # if (!rapportools::is.empty(dest_ui_element)) {
    #   relevant_rows <-
    #     relevant_df[(
    #       relevant_df$origin_zip == orig_ui_element &
    #         relevant_df$destination_zip == dest_ui_element
    #     ), ]
    if (nrow(row_df) >= 1) {
      veh_ids <-
        row_df$veh_id # paste0("X", trimws(finished_row$veh_ID))
      # dt <- as.rvData$lat_df$datetime
      a_id <- rvData$a_id
      ev_info_db_df <-
        DBI::dbGetQuery(
          pool,
          sprintf(
            "select simulation_ts, veh_id, lat_val, lng_val, soc_val, prob_val, state_val, tocharge_val, speed_val, nearest_evse_id, chargers_nearby, nearest_evses, charging_decision_time from ev_info where analysis_id = %d and veh_id IN %s order by info_id",
            a_id,
            paste("(", toString(paste(
              "'", veh_ids, "'", sep = ''
            )), ")", sep = '')
          )
        )
      
      info_list <- vector(mode = "list", length = length(veh_ids))
      names(info_list) <- veh_ids
      
      color = grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)]
      
      od_zip <- c(row_df$origin_zip, row_df$destination_zip)
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
            row_df$origin_zip,
            ",",
            row_df$destination_zip,
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
      
      # evse_rows <-
      #   match(cs_df$evse_id, rvData$evse_dcfc$evse_id)
      # cs_lats <-
      #   rvData$evse_dcfc$latitude[evse_rows]
      # cs_lngs <-
      #   rvData$evse_dcfc$longitude[evse_rows]
      dest_color <- "#420db5"
      orig_color <- "#960db5"
      map_id <- "wa_ooc_mapout"
      waypoint_color <- "#75d654"
      
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
          label = paste0("Origin: ", row_df$origin_zip),
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
                         row_df$destination_zip),
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
            color =  "#75d654",
            popup = paste(sep = "<br>",
                          paste("Veh ID: ", i),
                          socs,
                          tocharges,
                          probs,
                          states),
            label = paste(sep = "\n",
                          paste("Veh ID: ", i),
                          socs,
                          tocharges,
                          probs,
                          states),
            group = "travel_path",
            stroke = FALSE,
            fillOpacity = 0.5,
            options = pathOptions(pane = "travel_path")
          )
        
        rel_cs_df <- cs_df[cs_df$veh_id == i, ]
        evse_rows <-
          match(gsub("\\..*", "", rel_cs_df$evse_id),
                rvData$evse_dcfc$evse_id)
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
        
        output$ev_info_table <- renderDataTable({
          DT::datatable(
            ev_info_db_df,
            selection = "single",
            filter = 'top',
            options = list(
              pageLength = 10,
              autoWidth = TRUE,
              scrollX = TRUE,
              scrollY = "500px",
              paging = TRUE,
              columnDefs = list(list(
                className = 'dt-center', targets = "_all"
              )),
              dom = 'Bflrtip',
              buttons = c(('colvis')),
              initComplete = JS(
                "function(settings, json) {",
                "$(this.api().table().header()).css({'background-color': '#EBECEC', 'color': '#000'});",
                "}"
              )
            ),
            class = 'nowrap display'
          ) %>% formatRound('soc_val', 0) %>% formatRound('lat_val', 4) %>% formatRound('lng_val', 4)
        })
        
        
        
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
      # startAnim(session, 'select_datetime', 'shake')
      
    }
  })
  
  output$wa_road_mapout <- renderLeaflet({
    wa_map %>%
      addPolylines(data = wa_roads, color = "blue")
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
          icon = evse_icon_green,
          group = base_layers[2],
          data = rvData$all_chargers_chademo
        ) %>%
        addCircleMarkers(
          lat = rvData$stranded_df$stranded_lat,
          lng = rvData$stranded_df$stranded_lng,
          radius = 4,
          color = "#b50d2c",
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
        addLayersControl(
          overlayGroups = base_layers,
          baseGroups = tile_layers,
          options = layersControlOptions(collapsed = FALSE)
        )
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
      
      if (input$evse_serve_wait_radio == "Waited") {
        evs_waited_df_tw <-
          rvData$evs_waiting_df %>% dplyr::mutate(
            datetime = as.POSIXct(
              wait_start_time,
              origin = as.POSIXct("1970-01-01", tz = "Etc/GMT+8"),
              tz = "Etc/GMT+8"
            ),
            evse_id = gsub("\\..*", "", evse_id)
          ) %>%
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
          rvData$charging_session_df %>% dplyr::mutate(
            datetime = as.POSIXct(
              charge_start_time,
              origin = as.POSIXct("1970-01-01", tz = "Etc/GMT+8"),
              tz = "Etc/GMT+8"
            ),
            evse_id = gsub("\\..*", "", evse_id)
          ) %>%
          dplyr::filter(datetime >= range_start_time &
                          datetime <= range_end_time) %>% dplyr::group_by(evse_id) %>% dplyr::summarise(count = n())
        
        
        # evs_served_waited_combo_tw <- na.omit(merge(evs_waited_df_tw_combo, evs_served_df_tw, all.x = TRUE))
        #
        # evs_served_waited_chademo_tw <- na.omit(merge(evs_waited_df_tw_chademo, evs_served_df_tw, all.x = TRUE))
        #
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
          addMapPane(name = "chargers", zIndex = 500) %>%
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
            icon = evse_icon_green,
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
            layerId = paste0("cm", overlay_combo$evse_id),
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
            layerId = paste0("cd", overlay_chademo$evse_id),
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
            radius = 30 * sqrt(overlay_combo$count) / sqrt(max(overlay_combo$count)),
            #log(100 * overlay_combo$count),
            layerId = paste0("cc", overlay_combo$evse_id),
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
            radius = 30 * sqrt(overlay_chademo$count) / sqrt(max(overlay_chademo$count)),
            # log(100 * overlay_chademo$count),
            layerId = paste0("ce", overlay_chademo$evse_id),
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
          # addMinicharts(
          #   evs_served_waited_combo_tw$longitude,
          #   evs_served_waited_combo_tw$latitude,
          #   type = "pie",
          #   chartdata = evs_served_waited_combo_tw[, c("served", "waited")],
          #   colorPalette = colors,
          #   width = 10,
          #   transitionTime = 0
          #   ) %>%
          addLayersControl(
            overlayGroups = base_layers,
            baseGroups = tile_layers,
            options = layersControlOptions(collapsed = FALSE)
          )
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
          "Select a simulation run time to see the EVSE utilization."
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
        rvData$power_draw_df %>% dplyr::filter(evse_id == paste0(id, ".0") |
                                                 evse_id == id) %>% dplyr::arrange(pd_id) %>% collect() %>% dplyr::mutate(datetime = as.POSIXct(
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
  
  observeEvent(input$select_datetime, {
    print("Datetime selected")
    
    rvData$simulation_runtime <- input$select_datetime
    if (!rapportools::is.empty(rvData$simulation_runtime)) {
      # leafletProxy(mapId = 'wa_fin_mapout') %>%
      #   clearGroup(group = "new_labels")
      leafletProxy(mapId = 'wa_ooc_mapout') %>%
        clearGroup(group = "new_labels")
      # leafletProxy(mapId = 'wa_evse_serve_pass_mapout') %>%
      #   clearGroup(group = "new_labels")
      leafletProxy(mapId = 'wa_evse_util_mapout') %>%
        clearGroup(group = "new_labels")
      
      a_id <-
        rvData$a_id <-
        rvData$analyses$analysis_id[rvData$analyses$sim_date_time == rvData$simulation_runtime]
      
      rvData$power_draw_df <-
        pool %>% dplyr::tbl("evse_power_draw") %>% filter(analysis_id == a_id) %>% collect()
      
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
      rvData$bevs <- DBI::dbGetQuery(
        pool,
        paste0(
          'select es.origin_zip, es.destination_zip, es.soc, es.trip_start_time, es.veh_id, wb.county, wb.city, wb.zip_code, wb.model_year, wb.make, wb.model, wb.electric_range,  wb.legislative_district, wb.capacity, wb.fuel_consumption, wb.connector_code  from evtrip_scenarios es join wa_bevs wb on es.veh_id = wb.veh_id where es.analysis_id = ',
          a_id
        )
      )
      output$vehicle_count <- renderbs4InfoBox({
        bs4InfoBox(
          value = nrow(rvData$bevs)
          ,
          title = "EVs in Simulation",
          icon = "layer-group"
        )
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
          'select evse_id, charge_start_time, charge_end_time, veh_id, starting_soc, ending_soc from evse_charging_session where analysis_id = ',
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
      
      # leafletProxy(mapId = 'wa_evse_serve_wait_mapout') %>%
      #   addLabelOnlyMarkers(
      #     lng = nevse_dcfc$longitude,
      #     lat = nevse_dcfc$latitude,
      #     label = "new",
      #     group = "new_labels",
      #     labelOptions = leaflet::labelOptions(
      #       noHide = TRUE,
      #       direction = "bottom",
      #       textOnly = TRUE,
      #       offset = c(0,-10),
      #       opacity = 1
      #     )
      #   )
      
      # leafletProxy(mapId = 'wa_fin_mapout') %>%
      #   addLabelOnlyMarkers (
      #     lng = nevse_dcfc$longitude,
      #     lat = nevse_dcfc$latitude,
      #     label = "new",
      #     group = "new_labels",
      #     labelOptions = leaflet::labelOptions(
      #       noHide = TRUE,
      #       direction = "bottom",
      #       textOnly = TRUE,
      #       offset = c(0,-10),
      #       opacity = 1
      #     )
      #   )
      
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
        rbind(bevse_dcfc, nevse_dcfc) # %>% dplyr::filter(connector_code < 4)
      
      rvData$all_chargers_combo <-
        as.data.frame(rvData$evse_dcfc[rvData$evse_dcfc$connector_code == 2 |
                                         rvData$evse_dcfc$connector_code == 3,])
      
      rvData$all_chargers_chademo <-
        as.data.frame(rvData$evse_dcfc[rvData$evse_dcfc$connector_code == 1 |
                                         rvData$evse_dcfc$connector_code == 3,])
      
      # insertUI(
      #   selector = "#evs_waited_count",
      #   where = "afterEnd",
      #   ui = fluidRow(
      #     column(
      #       12,
      #       plotOutput("stat_evse_util_plot", height = "auto")
      #     )
      #   )
      # )
      power_draw_evse <-
        rvData$power_draw_df %>% dplyr::group_by(evse_id) %>% summarise(energy_consumed = round(sum(as.numeric(power_val)) / 60, digits = 0)) %>% mutate(evse_id = gsub("\\..*", "", evse_id))
      cs_evse <-
        rvData$charging_session_df %>% dplyr::group_by(evse_id) %>% summarise("# served" = n()) %>% mutate(evse_id = gsub("\\..*", "", evse_id))
      waiting_evse <-
        rvData$evs_waiting_df %>% dplyr::group_by(evse_id) %>% summarise("# waited" = n()) %>% mutate(evse_id = gsub("\\..*", "", evse_id))
      rvData$evse_dcfc_data <-
        merge(rvData$evse_dcfc,
              power_draw_evse,
              by = "evse_id",
              all.x = TRUE)
      rvData$evse_dcfc_data <-
        merge(rvData$evse_dcfc_data,
              cs_evse,
              by = "evse_id",
              all.x = TRUE)
      rvData$evse_dcfc_data <-
        merge(rvData$evse_dcfc_data,
              waiting_evse,
              by = "evse_id",
              all.x = TRUE) %>% tidyr::replace_na(list(
                energy_consumed = 0,
                "# served" = 0,
                "# waited" = 0
              ))
      
      output$stat_evse_util_plot <- renderPlot({
        total_power_draw <- rvData$power_draw_df %>%
          dplyr::group_by(simulation_ts) %>%
          dplyr::summarise(total_power = sum(power_val)) %>%
          dplyr::mutate(datetime = as.POSIXct(
            simulation_ts,
            origin = as.POSIXct("1970-01-01", tz = "Etc/GMT+8"),
            tz = "Etc/GMT+8"
          ))
        
        max_power <- max(total_power_draw$total_power)
        max_power_index <- which.max(total_power_draw$total_power)
        max_power_available <-
          50 * sum(rvData$evse_dcfc_data$dcfc_count)
        plot(
          x = total_power_draw$datetime,
          y = total_power_draw$total_power,
          type = "l",
          main = "Total Power vs Time of day",
          xlab = "Time of day (minutes)",
          ylab = "Power (kW)"
        )
        
        points(
          x = total_power_draw$datetime[max_power_index],
          y = max_power,
          col = "red",
          pch = 23
        )
        text(
          x = total_power_draw$datetime[max_power_index],
          y = max_power,
          label = paste0(
            "Maximum power used = ",
            round(max_power * 100 / max_power_available, 1),
            " % of maximum available "
          ),
          pos = 4,
          cex = 1.1
        )
      })
      
      
      
      output$stat_evse_wait_plot <- renderPlot({
        wait_time_mins <-
          difftime(
            rvData$evs_waiting_df$wait_end_time,
            rvData$evs_waiting_df$wait_start_time,
            units = "mins"
          )
        dens_wait_time <- density(as.numeric(wait_time_mins))
        # max_dens <- max(dens_wait_time)
        max_dens_index <- which.max(dens_wait_time$y)
        
        plot(
          x = dens_wait_time,
          type = "l",
          main = "Wait time distribution (mins)",
          ylab = "Density",
          xlim = c(0, max(dens_wait_time$x))
        )
        abline(v = dens_wait_time$x[max_dens_index], col = "red")
        text(
          x = dens_wait_time$x[max_dens_index],
          labels = as.character(round(dens_wait_time$x[max_dens_index]), 1),
          y = 0,
          col = "red",
          pos = 4,
          cex = 1.2,
          adj = c(0, 1)
        )
      })
      
      output$stat_cs_plot <- renderPlot({
        cs_time_mins <-
          difftime(
            rvData$charging_session_df$charge_end_time,
            rvData$charging_session_df$charge_start_time,
            units = "mins"
          )
        dens_charge_time <- density(as.numeric(cs_time_mins))
        # max_dens <- max(dens_wait_time)
        max_dens_index <- which.max(dens_charge_time$y)
        
        plot(
          x = dens_charge_time,
          type = "l",
          main = "Charge time distribution (mins)",
          ylab = "Density",
          xlim = c(0, max(dens_charge_time$x))
        )
        abline(v = dens_charge_time$x[max_dens_index], col = "blue")
        text(
          x = dens_charge_time$x[max_dens_index],
          labels = as.character(round(dens_charge_time$x[max_dens_index]), 1),
          y = 0,
          col = "blue",
          pos = 4,
          cex = 1.2,
          adj = c(0, 1)
        )
      })
      
    }
    else {
      # startAnim(session, 'select_datetime', 'highlight')
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
    if (nrow(rvData$evse_dcfc_data) > 0) {
      DT::datatable(
        rvData$evse_dcfc_data %>% filter(connector_code < 4),
        selection = "single",
        filter = 'top',
        options = list(
          pageLength = 4,
          autoWidth = FALSE,
          scrollX = TRUE,
          scrollY = "200px",
          columnDefs = list(list(
            className = 'dt-center', targets = "_all"
          )),
          dom = 'Bfrtip',
          buttons = c('csv', 'print', I('colvis')),
          scrollCollapse = T,
          paging = F,
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#EBECEC', 'color': '#000'});",
            "}"
          )
        ),
        class = 'nowrap display',
        extensions = c('Buttons', 'FixedColumns')
      )
    }
    else {
      showModal(
        modalDialog(
          size = "s",
          easyClose = TRUE,
          "Select a simulation run time to see the EVSE utilization."
        )
      )
    }
  })
  
  output$ev_table <- renderDataTable({
    if (nrow(rvData$bevs) > 0) {
      DT::datatable(
        rvData$bevs,
        selection = "single",
        filter = 'top',
        options = list(
          pageLength = 10,
          autoWidth = TRUE,
          scrollX = TRUE,
          scrollCollapse = TRUE,
          columnDefs = list(list(
            className = 'dt-center', targets = "_all"
          )),
          dom = 'Bfrtip',
          buttons = c(('colvis')),
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#EBECEC', 'color': '#000'});",
            "}"
          )
        ),
        class = 'nowrap display',
        extensions = c('Buttons', 'FixedColumns')
      ) %>% formatRound('capacity', 1) %>% formatRound('fuel_consumption', 1)
    }
    else {
      showModal(
        modalDialog(
          size = "s",
          easyClose = TRUE,
          "Select a simulation run time to see the EVs in a simulation."
        )
      )
    }
  })
  
  output$road_table <- renderDataTable({
    DT::datatable(
      as.data.frame(wa_roads)[, c('ID', 'Spd')],
      selection = "single",
      filter = 'top',
      options = list(
        pageLength = 5,
        autoWidth = TRUE,
        scrollX = TRUE,
        columnDefs = list(list(
          className = 'dt-center', targets = "_all"
        )),
        initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
          "}"
        )
      ),
      rownames = FALSE
    )
  })
  
  # to keep track of previously selected row
  prev_evse_row <- reactiveVal()
  # to keep track of previously selected row
  prev_ev_row <- reactiveVal()
  
  prev_road_row <- reactiveVal()
  
  observeEvent(input$evse_table_rows_selected, {
    evse_dcfc <-
      rvData$evse_dcfc_data %>% dplyr::filter(connector_code < 4)
    row_selected = evse_dcfc[input$evse_table_rows_selected, ]
    if (row_selected$connector_code == 1 |
        row_selected$connector_code == 3) {
      layer_id <- paste0("ch", row_selected$evse_id)
    } else if (row_selected$connector_code == 2 |
               row_selected$connector_code == 3) {
      layer_id <- paste0("co", row_selected$evse_id)
    }
    
    output$cs_table <- renderDataTable({
      cs_df <-
        rvData$charging_session_df %>% dplyr::filter(evse_id == paste0(row_selected$evse_id, ".0")) %>% dplyr::select(charge_start_time,
                                                                                                                      charge_end_time,
                                                                                                                      veh_id,
                                                                                                                      starting_soc,
                                                                                                                      ending_soc)
      DT::datatable(
        cs_df,
        selection = "single",
        filter = 'top',
        options = list(
          pageLength = 5,
          paging = F,
          autoWidth = TRUE,
          scrollX = TRUE,
          scrollY = "200px",
          columnDefs = list(list(
            className = 'dt-center', targets = "_all"
          )),
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
            "}"
          )
        ),
        class = 'nowrap display'
      ) %>% formatRound('starting_soc', 0) %>% formatRound('ending_soc', 0)
    })
    
    output$ws_table <- renderDataTable({
      ws_df <-
        rvData$evs_waiting_df %>% dplyr::filter(evse_id == paste0(row_selected$evse_id, ".0")) %>% dplyr::select(wait_start_time, wait_end_time, veh_id, soc_val)
      DT::datatable(
        ws_df,
        selection = "single",
        filter = 'top',
        options = list(
          pageLength = 5,
          autoWidth = TRUE,
          scrollX = TRUE,
          scrollY = "200px",
          paging = F,
          columnDefs = list(list(
            className = 'dt-center', targets = "_all"
          )),
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
            "}"
          )
        ),
        class = 'nowrap display'
      ) %>% formatRound('soc_val', 0)
    })
    
    proxy <- leafletProxy('wa_evse_util_mapout')
    # print(row_selected)
    proxy %>%
      addMarkers(
        popup = as.character(row_selected$evse_id),
        layerId = layer_id,
        group = as.character(row_selected$evse_id),
        lng = row_selected$longitude,
        lat = row_selected$latitude
      )
    
    # Reset previously selected marker
    if (!is.null(prev_evse_row()))
    {
      proxy %>%
        clearGroup(group = as.character(prev_evse_row()$evse_id))
    }
    # set new value to reactiveVal
    prev_evse_row(row_selected)
    
    
  })
  
  
  
  observeEvent(input$ev_table_rows_selected, {
    row_selected = rvData$bevs[input$ev_table_rows_selected, ]
    # Reset previously selected marker
    if (!is.null(prev_ev_row()))
    {
      clearMapOverlay(mapID = "wa_ooc_mapout")
    }
    
    plot_trajectory(row_selected)
    
    
    # set new value to reactiveVal
    prev_ev_row(row_selected)
  })
  
  observeEvent(input$road_table_rows_selected, {
    row_selected <- wa_roads[input$road_table_rows_selected, ]
    
    proxy <- leafletProxy('wa_road_mapout')
    
    proxy %>% addPolylines(
      data = row_selected,
      color = "red",
      group = as.character(row_selected$ID)
    )
    # Reset previously selected marker
    if (!is.null(prev_ev_row()))
    {
      proxy %>% clearGroup(group = as.character(row_selected$ID))
    }
    # set new value to reactiveVal
    prev_road_row(row_selected)
  })
  
  session$onFlushed(function() {
    #    if (rapportools::is.empty(rvData$simulation_runtime)) {
    #  startAnim(session, 'select_datetime', 'tada')
    #   }
    
  })
}

shiny::shinyApp(ui, server)