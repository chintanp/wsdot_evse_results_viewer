#' evses UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_evses_ui <- function(id) {
  ns <- NS(id)
  tagList(fluidRow(
    column(
      6,
      bs4Dash::bs4Card(
        title = "List of DCFCs",
        closable = FALSE,
        status = "success",
        collapsible = TRUE,
        elevation = 4,
        width = NULL,
        maximizable = TRUE,
        solidHeader = TRUE,
        DT::dataTableOutput(ns("evse_table"))
      )
    ),
    column(
      6,
      
      bs4Dash::bs4TabCard(
        id = "wait_charge_tabcard",
        title = tags$p("",
                       style = " font-size: 20px;
                                                    font-weight: 600;
                                                    margin: 0; "),
        elevation = 4,
        width = 12,
        collapsible = TRUE,
        maximizable = TRUE,
        closable = FALSE,
        type = "tabs",
        status = "purple",
        solidHeader = FALSE,
        bs4Dash::bs4TabPanel(tabName = "Charging Sessions",
                             DT::dataTableOutput(ns("cs_table"))),
        bs4Dash::bs4TabPanel(tabName = "Waiting Sessions",
                             DT::dataTableOutput(ns("ws_table")))
      )
    )
  ),
  fluidRow(
    column(
      width = 9,
      bs4Dash::bs4Card(
        title = "EVSE Utilization Details",
        closable = FALSE,
        status = "success",
        collapsible = TRUE,
        elevation = 4,
        width = NULL,
        solidHeader = TRUE,
        maximizable = TRUE,
        shinycssloaders::withSpinner(
          leaflet::leafletOutput(ns("wa_evse_util_mapout"), height = 700),
          type = 8,
          color = "#0dc5c1"
        )
      )
    ),
    column(
      width = 3,
      bs4Dash::bs4Card(
        width = 12,
        title = "Start and End Time Selector",
        closable = FALSE,
        sliderInput(
          ns("evse_util_slider"),
          label = "Time Range",
          min = 0,
          max = 24,
          value = c(0, 24)
        )
      ),
      bs4Dash::bs4Card(
        width = 12,
        title = "Served / Waited Selector",
        closable = FALSE,
        radioButtons(
          ns("evse_serve_wait_radio"),
          label = NULL,
          choices = c("Served", "Waited")
        )
      )
    )
  ))
}

#' evses Server Function
#'
#' @noRd
mod_evses_server <-
  function(input,
           output,
           session,
           globals,
           globalinput) {
    ns <- session$ns
    
    tile_layers <- c("light", "streets", "satellite-streets")
    base_layers <- c("Combo", "CHAdeMO")
    
    wa_map <- leaflet::leaflet() %>%
      leaflet.mapboxgl::addMapboxGL(
        style = "mapbox://styles/mapbox/satellite-streets-v11",
        group = tile_layers[3],
        setView = FALSE,
        accessToken = "pk.eyJ1IjoiY2hpbnRhbnAiLCJhIjoiY2ppYXU1anVuMThqazNwcDB2cGtneDdkYyJ9.TL6RTyRRFCbvJWyFa4P0Ow"
      ) %>%
      leaflet.mapboxgl::addMapboxGL(
        style = "mapbox://styles/mapbox/streets-v11",
        group = tile_layers[2],
        setView = FALSE,
        accessToken = "pk.eyJ1IjoiY2hpbnRhbnAiLCJhIjoiY2ppYXU1anVuMThqazNwcDB2cGtneDdkYyJ9.TL6RTyRRFCbvJWyFa4P0Ow"
      ) %>%
      leaflet.mapboxgl::addMapboxGL(
        style = "mapbox://styles/mapbox/light-v10",
        group = tile_layers[1],
        setView = FALSE,
        accessToken = "pk.eyJ1IjoiY2hpbnRhbnAiLCJhIjoiY2ppYXU1anVuMThqazNwcDB2cGtneDdkYyJ9.TL6RTyRRFCbvJWyFa4P0Ow"
      ) %>%
      # addPolylines(data = wa_roads, opacity = 1, weight = 2) %>%
      leaflet.extras::addResetMapButton() %>%
      leafem::addMouseCoordinates() %>%
      leaflet::addMapPane(name = "chargers", zIndex = 500) %>%
      leaflet::addMapPane(name = "overlay", zIndex = 491)
    
    simulated_date <- "2019-07-01"
    
    evse_icon_blue <-
      leaflet::icons(
        iconUrl = here::here("data-raw", "evse_icon_blue.jpg"),
        iconWidth = 10,
        iconHeight = 10,
        iconAnchorX = 0,
        iconAnchorY = 0
      )
    
    evse_icon_green <-
      leaflet::icons(
        iconUrl = here::here("data-raw", "evse_icon_green.jpg"),
        iconWidth = 10,
        iconHeight = 10,
        iconAnchorX = 0,
        iconAnchorY = 0
      )
    
    clearMapOverlay <- function(mapID) {
      # print("clearing markers now")
      leaflet::leafletProxy(mapId = mapID) %>%
        leaflet::clearGroup(group = base_layers[1]) %>%
        leaflet::clearGroup(group = base_layers[2])
      
    }
    
    rvData <- reactiveValues(cs_df = data.frame(),
                             ws_df = data.frame())
    
    observeEvent(globalinput$select_analysis,
                 {
                   # browser()
                   req(globalinput$select_analysis)
                   globals$stash$a_id <-
                     globals$stash$analyses$analysis_id[globals$stash$analyses$sim_date_time == globalinput$select_analysis]
                   # as.numeric(strsplit(globalinput$select_analysis, ' - ', fixed = TRUE)[[1]][2])
                   # print(globals$stash$a_id)
                   req(globals$stash$a_id)
                   a_id <- globals$stash$a_id
                   evse_query <-
                     paste0(
                       "SELECT evse_id, latitude, longitude, dcfc_count, connector_code from evses_now where analysis_id = ",
                       a_id,
                       " and dcfc_count > 0;"
                     )
                   
                   nevse_query <-
                     paste0(
                       "SELECT nevse_id, latitude, longitude, dcfc_plug_count, connector_code, station_type, comments from new_evses where analysis_id = ",
                       a_id
                     )
                   evse_dcfc <-
                     DBI::dbGetQuery(globals$stash$pool, evse_query)
                   
                   nevse_dcfc <-
                     DBI::dbGetQuery(globals$stash$pool, nevse_query)
                   
                   # evse_dcfc <-
                   #   rbind(globals$stash$bevse_dcfc, nevse_dcfc)
                   
                   
                   all_chargers_combo <-
                     as.data.frame(evse_dcfc[evse_dcfc$connector_code == 2 |
                                               evse_dcfc$connector_code == 3,])
                   
                   all_chargers_chademo <-
                     as.data.frame(evse_dcfc[evse_dcfc$connector_code == 1 |
                                               evse_dcfc$connector_code == 3,])
                   
                   globals$stash$all_chargers_combo <-
                     all_chargers_combo
                   globals$stash$all_chargers_chademo <-
                     all_chargers_chademo
                   # power_draw_evse <- data.frame()
                   
                   # power_draw_df <- globals$stash$pool %>%
                   #   dplyr::tbl("evse_power_draw") %>%
                   #   dplyr::filter(analysis_id == a_id) %>%
                   #   dplyr::collect()
                   #
                   # power_draw_evse <- power_draw_df %>%
                   #   dplyr::group_by(evse_id) %>%
                   #   dplyr::summarise(energy_consumed = round(sum(as.numeric(power_val)) / 60, digits = 0)) %>%
                   #   dplyr::mutate(evse_id = gsub("\\..*", "", evse_id))
                   
                   power_draw_evse <- globals$stash$pool %>%
                     dplyr::tbl("evse_power_draw") %>%
                     dplyr::select(evse_id, power_val, analysis_id) %>%
                     dplyr::filter(analysis_id == a_id)  %>%
                     dplyr::group_by(evse_id) %>%
                     dplyr::summarise(energy_consumed = round(sum(as.numeric(power_val)) / 60, digits = 0)) %>%
                     dplyr::collect() %>%
                     dplyr::mutate(evse_id = gsub("\\..*", "", evse_id))
                   # power_draw_evse <-
                   #   power_draw_df %>% dplyr::group_by(evse_id) %>% summarise(energy_consumed = round(sum(as.numeric(power_val)) / 60, digits = 0)) %>% mutate(evse_id = gsub("\\..*", "", evse_id))
                   
                   charging_session_df <- DBI::dbGetQuery(
                     globals$stash$pool,
                     paste0(
                       'select evse_id, charge_start_time, charge_end_time, veh_id, starting_soc, ending_soc from evse_charging_session where analysis_id = ',
                       a_id
                     )
                   )
                   globals$stash$charging_session_df <-
                     charging_session_df
                   cs_evse <-
                     charging_session_df %>%
                     dplyr::group_by(evse_id) %>%
                     dplyr::summarise("# served" = dplyr::n()) %>%
                     dplyr::mutate(evse_id = gsub("\\..*", "", evse_id))
                   
                   evs_waiting_df <-
                     globals$stash$pool %>%
                     dplyr::tbl("evse_evs_waiting") %>%
                     dplyr::filter(analysis_id == a_id) %>%
                     dplyr::collect()
                   waiting_evse <- evs_waiting_df %>%
                     dplyr::group_by(evse_id) %>%
                     dplyr::summarise("# waited" = dplyr::n()) %>%
                     dplyr::mutate(evse_id = gsub("\\..*", "", evse_id))
                   
                   globals$stash$evs_waiting_df <- evs_waiting_df
                   # browser()
                   
                   evse_dcfc_data <-
                     merge(evse_dcfc,
                           power_draw_evse,
                           by = "evse_id",
                           all.x = TRUE)
                   evse_dcfc_data <-
                     merge(evse_dcfc_data,
                           cs_evse,
                           by = "evse_id",
                           all.x = TRUE)
                   evse_dcfc_data <-
                     merge(evse_dcfc_data,
                           waiting_evse,
                           by = "evse_id",
                           all.x = TRUE) %>% tidyr::replace_na(list(
                             energy_consumed = 0,
                             "# served" = 0,
                             "# waited" = 0
                           ))
                   
                   globals$stash$evse_dcfc_data <- evse_dcfc_data
                   
                   output$evse_table <- DT::renderDataTable({
                     if (nrow(evse_dcfc_data) > 0) {
                       DT::datatable(
                         evse_dcfc_data %>% dplyr::filter(connector_code < 4),
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
                           initComplete = DT::JS(
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
                   
                   output$wa_evse_util_mapout <-
                     leaflet::renderLeaflet({
                       # browser()
                       
                       # print(rvData$date_selected)
                       # if(rvData$date_selected != FALSE) {
                       #   print("Date had been selected before")
                       #   clearMapOverlay("wa_evse_util_mapout")
                       # }
                       
                       # clearMapOverlay("wa_evse_util_mapout")
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
                           evs_waiting_df %>% dplyr::mutate(
                             datetime = as.POSIXct(
                               wait_start_time,
                               origin = as.POSIXct("1970-01-01", tz = "Etc/GMT+8"),
                               tz = "Etc/GMT+8"
                             ),
                             evse_id = gsub("\\..*", "", evse_id)
                           ) %>%
                           dplyr::filter(datetime >= range_start_time &
                                           datetime <= range_end_time) %>%
                           dplyr::group_by(evse_id) %>%
                           dplyr::summarise(count = dplyr::n())
                         
                         evs_waited_df_tw_combo <-
                           merge(evs_waited_df_tw,
                                 all_chargers_combo,
                                 by = "evse_id",
                                 all.x = TRUE)
                         evs_waited_df_tw_chademo <-
                           merge(evs_waited_df_tw,
                                 all_chargers_chademo,
                                 by = "evse_id",
                                 all.x = TRUE)
                         overlay_text <- "Waited: "
                         overlay_color <- "#b50d2c"
                         overlay_combo <-
                           na.omit(evs_waited_df_tw_combo)
                         overlay_chademo <-
                           na.omit(evs_waited_df_tw_chademo)
                       } else if (input$evse_serve_wait_radio == "Served") {
                         evs_served_df_tw <-
                           charging_session_df %>% dplyr::mutate(
                             datetime = as.POSIXct(
                               charge_start_time,
                               origin = as.POSIXct("1970-01-01", tz = "Etc/GMT+8"),
                               tz = "Etc/GMT+8"
                             ),
                             evse_id = gsub("\\..*", "", evse_id)
                           ) %>%
                           dplyr::filter(datetime >= range_start_time &
                                           datetime <= range_end_time) %>%
                           dplyr::group_by(evse_id) %>%
                           dplyr::summarise(count = dplyr::n())
                         
                         
                         # evs_served_waited_combo_tw <- na.omit(merge(evs_waited_df_tw_combo, evs_served_df_tw, all.x = TRUE))
                         #
                         # evs_served_waited_chademo_tw <- na.omit(merge(evs_waited_df_tw_chademo, evs_served_df_tw, all.x = TRUE))
                         #
                         evs_served_df_tw_combo <-
                           merge(evs_served_df_tw,
                                 all_chargers_combo,
                                 by = "evse_id",
                                 all.x = TRUE)
                         evs_served_df_tw_chademo <-
                           merge(evs_served_df_tw,
                                 all_chargers_chademo,
                                 by = "evse_id",
                                 all.x = TRUE)
                         overlay_text <- "Served: "
                         overlay_color <- "#75d654"
                         overlay_combo <-
                           na.omit(evs_served_df_tw_combo)
                         overlay_chademo <-
                           na.omit(evs_served_df_tw_chademo)
                       }
                       
                       if (nrow(overlay_combo) > 0 &
                           nrow(overlay_chademo) > 0) {
                         # browser()
                         wa_map <- wa_map %>%
                           leaflet::addMarkers(
                             lng = ~ longitude ,
                             lat = ~ latitude,
                             layerId = ~ paste0("co", evse_id),
                             icon = evse_icon_blue,
                             group = base_layers[1],
                             data = all_chargers_combo,
                             options = leaflet::pathOptions(pane = "chargers")
                           )  %>%
                           leaflet::addMarkers(
                             lng = ~ longitude ,
                             lat = ~ latitude,
                             layerId = ~ paste0("ch", evse_id),
                             icon = evse_icon_green,
                             group = base_layers[2],
                             data = all_chargers_chademo,
                             options = leaflet::pathOptions(pane = "chargers")
                           ) %>%
                           # leaflet::addLabelOnlyMarkers(
                           #   lng = ~ longitude,
                           #   lat = ~ latitude,
                           #   data = nevse_dcfc,
                           #   label = ~ station_type,
                           #   group = "new_labels",
                           #   labelOptions = leaflet::labelOptions(
                           #     noHide = TRUE,
                           #     direction = "bottom",
                           #     textOnly = TRUE,
                           #     offset = c(0,-10),
                         #     opacity = 1,
                         #     style = list(
                         #       "color" = "red",
                         #       "font-family" = "serif",
                         #       "font-style" = "italic",
                         #       "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                         #       "font-size" = "15px",
                         #       "border-color" = "rgba(0,0,0,0.5)"
                         #     )
                         #   )
                         # ) %>%
                         leaflet::addLabelOnlyMarkers(
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
                         ) %>%  leaflet::addLabelOnlyMarkers(
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
                           leaflet::addCircleMarkers(
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
                           ) %>%
                           leaflet::addCircleMarkers(
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
                           leaflet::addLayersControl(
                             overlayGroups = base_layers,
                             baseGroups = tile_layers,
                             options = leaflet::layersControlOptions(collapsed = FALSE)
                           )
                         
                         # rvData$date_selected <- TRUE
                       }
                       else
                         (showModal(
                           modalDialog(
                             size = "s",
                             easyClose = TRUE,
                             "No data found. Select a wider time window."
                           )
                         ))
                       # } else {
                       #   showModal(
                       #     modalDialog(
                       #       size = "s",
                       #       easyClose = TRUE,
                       #       "Select a simulation run time to see the EVSE utilization."
                       #     )
                       #   )
                       # }
                       
                       if (nrow(nevse_dcfc) > 0) {
                         wa_map %>%
                           leaflet::addLabelOnlyMarkers(
                             lng = ~ longitude,
                             lat = ~ latitude,
                             data = nevse_dcfc,
                             label = ~ station_type,
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
                           )
                       } else {
                         wa_map
                       }
                     })
                   
                 },
                 ignoreInit = TRUE,
                 autoDestroy = FALSE)
    
    
    observeEvent(input$wa_evse_util_mapout_marker_click, {
      req(input$wa_evse_util_mapout_marker_click$id)
      id = substr(
        input$wa_evse_util_mapout_marker_click$id,
        3,
        nchar(input$wa_evse_util_mapout_marker_click$id)
      )
      # print(id)
      ## TODO:
      ### Make the data filter with time
      ### Add other indication to the map, so we dont have to click each and every charger
      ### maybe evs_served
      
      #     if (!rapportools::is.empty(simulation_runtime)) {
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
      
      a_id <- globals$stash$a_id
      
      power_draw_evse <-
        globals$stash$pool %>%
        dplyr::tbl("evse_power_draw") %>%
        dplyr::filter(analysis_id == a_id) %>%
        dplyr::filter(evse_id == paste0(id, ".0") |
                        evse_id == id) %>%
        dplyr::arrange(pd_id) %>%
        dplyr::collect() %>%
        dplyr::mutate(
          datetime = as.POSIXct(
            simulation_ts,
            origin = as.POSIXct("1970-01-01", tz = "Etc/GMT+8"),
            tz = "Etc/GMT+8"
          ),
          waiting_count = 0
        ) %>%
        dplyr::filter(datetime >= range_start_time &
                        datetime <= range_end_time)
      
      
      # evse_util <-
      #     round(rvData$evse_util_df$energy_consumed[which(rvData$evse_util_df$evse_id == id)], 2)
      # Evse util is Int(P)dt
      evse_util <-
        sum(as.numeric(power_draw_evse$power_val)) / 60
      chademo_count <-
        globals$stash$all_chargers_chademo$dcfc_count[globals$stash$all_chargers_chademo$evse_id == id]
      combo_count <-
        globals$stash$all_chargers_combo$dcfc_count[globals$stash$all_chargers_combo$evse_id == id]
      # relevant_charging_sessions <-
      #   globals$stash$charging_session_df[which(globals$stash$charging_session_df$evse_id == paste0(id, ".0")), ]
      relevant_charging_sessions <-
        globals$stash$charging_session_df %>%
        dplyr::filter(evse_id == paste0(id, ".0") |
                        evse_id == id)
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
      
      # relevant_evs_waiting <-
      #   globals$stash$evs_waiting_df[which(globals$stash$evs_waiting_df$evse_id == paste0(id, ".0")), ]
      relevant_evs_waiting <-
        globals$stash$evs_waiting_df %>%
        dplyr::filter(evse_id == paste0(id, ".0") |
                        evse_id == id)
      
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
      
      for (waiting_row in rownames(relevant_evs_waiting_tw)) {
        # browser()
        #build an index-vector matching the condition
        index.v <-
          which(
            power_draw_evse$datetime >= relevant_evs_waiting_tw[waiting_row,]$datetime &
              power_draw_evse$datetime <= as.POSIXct(
                relevant_evs_waiting_tw[waiting_row,]$wait_end_time,
                origin = as.POSIXct("1970-01-01", tz = "Etc/GMT+8"),
                tz = "Etc/GMT+8"
              )
          )
        
        power_draw_evse[index.v, ]$waiting_count <-
          power_draw_evse[index.v, ]$waiting_count + 1
        # pd_subset$waiting_count <- pd_subset$waiting_count + 1
      }
      
      
      # Generate the plotly plot with two axes
      ay <- list(
        overlaying = "y",
        side = "right",
        title = "# EVs Waiting",
        dtick = 1,
        tick0 = 0,
        tickmode = "linear"
      )
      # fig <- plotly::plot_ly()
      fig <- plotly::plot_ly(
        data = power_draw_evse,
        x = ~ datetime,
        y = ~ power_val,
        name = "Power draw",
        type = 'scatter',
        mode = 'lines'
      )
      fig <-
        fig %>% plotly::add_lines(
          data = power_draw_evse,
          x = ~ datetime,
          y = ~ waiting_count,
          name = "# EVs Waiting",
          yaxis = "y2"
        )
      fig <- fig %>% plotly::layout(
        title = "Power draw and # EVs Waiting",
        yaxis2 = ay,
        xaxis = list(title = "Time of day (minutes)"),
        yaxis = list(title = "Power (kW)")
      )
      
      
      if (nrow(power_draw_evse) >= 1) {
        showModal(
          modalDialog(
            title = "Charging station details",
            fluidRow(
              bs4Dash::bs4Table(
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
                bs4Dash::bs4TableItems(
                  bs4Dash::bs4TableItem(id),
                  bs4Dash::bs4TableItem(dataCell = TRUE, evse_util),
                  bs4Dash::bs4TableItem(dataCell = TRUE,
                                        chademo_count),
                  bs4Dash::bs4TableItem(dataCell = TRUE,
                                        combo_count),
                  bs4Dash::bs4TableItem(dataCell = TRUE,
                                        evs_charged),
                  bs4Dash::bs4TableItem(dataCell = TRUE,
                                        evs_waiting)
                )
              )
            ),
            plotly::renderPlotly(# plot(
              #   x = power_draw_evse$datetime,
              #   y = power_draw_evse$power_val,
              #   type = "l",
              #   main = "Power vs Time of day",
              #   xlab = "Time of day (minutes)",
              #   ylab = "Power (kW)"
              # )
              
              # fig <-
              # plotly::plot_ly(
              #   power_draw_evse,
              #   x = ~ datetime,
              #   y = ~ power_val,
              #   type = 'scatter',
              #   mode = 'lines'
              # ) %>%
              #   plotly::layout(
              #     title = "Power vs Time of day",
              #     xaxis = list(title = "Time of day (minutes)"),
              #     yaxis = list(title = "Power (kW)")
              #   )
              #
              fig),
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
      
      #   } else {
      #   showModal(
      #     modalDialog(
      #       size = "s",
      #       easyClose = TRUE,
      #       "Select a simulation run time to see the EVSE utilization."
      #     )
      #   )
      # }
    })
    
    prev_evse_row <- reactiveVal()
    
    observeEvent(input$evse_table_rows_selected, {
      evse_dcfc <-
        globals$stash$evse_dcfc_data %>% dplyr::filter(connector_code < 4)
      row_selected = evse_dcfc[input$evse_table_rows_selected,]
      if (row_selected$connector_code == 1 |
          row_selected$connector_code == 3) {
        layer_id <- paste0("ch", row_selected$evse_id)
      } else if (row_selected$connector_code == 2 |
                 row_selected$connector_code == 3) {
        layer_id <- paste0("co", row_selected$evse_id)
      }
      
      # print("Inside evse_table_rows_selected")
      output$cs_table <- DT::renderDataTable({
        cs_df <-
          globals$stash$charging_session_df %>%
          dplyr::filter(evse_id == paste0(row_selected$evse_id, ".0") |
                          evse_id == row_selected$evse_id) %>%
          dplyr::select(charge_start_time,
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
            initComplete = DT::JS(
              "function(settings, json) {",
              "$(this.api().table().header()).css({'background-color': '#6c757d', 'color': '#fff'});",
              "}"
            )
          ),
          class = 'nowrap display'
        ) %>% DT::formatRound('starting_soc', 0) %>% DT::formatRound('ending_soc', 0)
      })
      
      output$ws_table <- DT::renderDataTable({
        ws_df <-
          globals$stash$evs_waiting_df %>%
          dplyr::filter(evse_id == paste0(row_selected$evse_id, ".0") |
                          evse_id == row_selected$evse_id) %>%
          dplyr::select(wait_start_time, wait_end_time, veh_id, soc_val)
        
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
            initComplete = DT::JS(
              "function(settings, json) {",
              "$(this.api().table().header()).css({'background-color': '#6c757d', 'color': '#fff'});",
              "}"
            )
          ),
          class = 'nowrap display'
        ) %>% DT::formatRound('soc_val', 0)
      })
      
      
      proxy <- leaflet::leafletProxy('wa_evse_util_mapout')
      # print(row_selected)
      proxy %>%
        leaflet::addMarkers(
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
          leaflet::clearGroup(group = as.character(prev_evse_row()$evse_id))
      }
      # set new value to reactiveVal
      prev_evse_row(row_selected)
    })
    
    
  }

## To be copied in the UI
# mod_evses_ui("evses_ui_1")

## To be copied in the server
# callModule(mod_evses_server, "evses_ui_1")