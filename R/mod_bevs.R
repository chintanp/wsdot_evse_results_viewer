#' bevs UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_bevs_ui <- function(id) {
  ns <- NS(id)
  tagList(fluidRow(column(
    4,
    bs4Dash::bs4Card(
      title = "List of EVs",
      closable = FALSE,
      status = "primary",
      collapsible = TRUE,
      elevation = 4,
      width = NULL,
      solidHeader = TRUE,
      maximizable = TRUE,
      DT::dataTableOutput(ns("ev_table"))
    )
  ),
  column(
    8,
    bs4Dash::bs4Card(
      title = "EV trajectory",
      closable = FALSE,
      status = "success",
      collapsible = TRUE,
      labelTooltip = "EV trajectory",
      elevation = 4,
      width = NULL,
      solidHeader = TRUE,
      maximizable = TRUE,
      shinycssloaders::withSpinner(
        leaflet::leafletOutput(ns("wa_ooc_mapout"), height = 700),
        type = 8,
        color = "#0dc5c1"
      )
    )
  )),
  fluidRow(column(
    12,
    bs4Dash::bs4Card(
      title = "EV Trajectory Info",
      closable = FALSE,
      status = "primary",
      collapsible = TRUE,
      elevation = 4,
      width = NULL,
      solidHeader = TRUE,
      maximizable = TRUE,
      DT::dataTableOutput(ns("ev_info_table"))
    )
  )))
}

#' bevs Server Function
#'
#' @noRd
mod_bevs_server <- function(input,
                            output,
                            session,
                            globals,
                            globalinput) {
  ns <- session$ns
  
  clearMapOverlay <- function(mapID) {
    # print("clearing markers now")
    leaflet::leafletProxy(mapId = mapID) %>%
      leaflet::clearGroup(group = "od_points") %>%
      leaflet::clearGroup(group = "travel_path") %>%
      leaflet::clearGroup(group = "shortest_path") %>%
      leaflet::clearGroup(group = "charging_sessions")
  }
  
  plot_trajectory <- function(row_df, a_id, evse_dcfc) {
    if (nrow(row_df) >= 1) {
      veh_ids <-
        row_df$veh_id # paste0("X", trimws(finished_row$veh_ID))
      # dt <- as.rvData$lat_df$datetime
      # a_id <- rvData$a_id
      ev_info_db_df <-
        DBI::dbGetQuery(
          globals$stash$pool,
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
          globals$stash$pool,
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
        globals$stash$pool %>% DBI::dbGetQuery(
          sprintf(
            "select * from evse_charging_session where analysis_id = %d and veh_id IN %s",
            a_id,
            paste("(", toString(paste(
              "'", veh_ids, "'", sep = ''
            )), ")", sep = '')
          )
        )
      
      dest_color <- "#420db5"
      orig_color <- "#960db5"
      map_id <- "wa_ooc_mapout"
      waypoint_color <- "#75d654"
      
      leaflet::leafletProxy(mapId = map_id) %>%
        leaflet::addMapPane(name = "od_points", zIndex = 410) %>%
        leaflet::addMapPane(name = "travel_path", zIndex = 490) %>%
        leaflet::addMapPane(name = "charging_sessions", zIndex = 491) %>%
        leaflet::addCircleMarkers(
          lat = od_lats[1],
          lng = od_lngs[1],
          radius = 12,
          color = orig_color,
          group = "od_points",
          stroke = FALSE,
          fillOpacity = 0.5,
          label = paste0("Origin: ", row_df$origin_zip),
          labelOptions = leaflet::labelOptions(noHide = T),
          options = leaflet::pathOptions(pane = "od_points")
          
        ) %>% leaflet::addCircleMarkers(
          lat = od_lats[2],
          lng = od_lngs[2],
          radius = 12,
          color = dest_color,
          group = "od_points",
          stroke = FALSE,
          fillOpacity = 0.5,
          label = paste0("Destination: ",
                         row_df$destination_zip),
          labelOptions = leaflet::labelOptions(noHide = T),
          options = leaflet::pathOptions(pane = "od_points")
          
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
        
        tp_layer <- rep("travel_path", length(lats))
        
        leaflet::leafletProxy(mapId = map_id) %>%
          leaflet::addCircleMarkers(
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
            options = leaflet::pathOptions(pane = "travel_path")
          )
        
        rel_cs_df <- cs_df[cs_df$veh_id == i,]
        evse_rows <-
          match(gsub("\\..*", "", rel_cs_df$evse_id),
                evse_dcfc$evse_id)
        cs_lats <-
          evse_dcfc$latitude[evse_rows]
        cs_lngs <-
          evse_dcfc$longitude[evse_rows]
        
        # Overlay charging session info
        if (nrow(rel_cs_df) >= 1) {
          leaflet::leafletProxy(mapId = map_id) %>%
            leaflet::addCircleMarkers(
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
              labelOptions = leaflet::labelOptions(noHide = T, textsize = "18px"),
              stroke = FALSE,
              fillOpacity = 0.8,
              options = leaflet::pathOptions(pane = "charging_sessions")
            )
        }
        
        output$ev_info_table <- DT::renderDataTable({
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
              initComplete = DT::JS(
                "function(settings, json) {",
                "$(this.api().table().header()).css({'background-color': '#EBECEC', 'color': '#000'});",
                "}"
              )
            ),
            class = 'nowrap display'
          ) %>% DT::formatRound('soc_val', 0) %>% DT::formatRound('lat_val', 4) %>% DT::formatRound('lng_val', 4)
        })
      }
    }
  }
  # to keep track of previously selected row
  prev_ev_row <- reactiveVal()
  
  tile_layers <- c("light", "streets", "satellite-streets")
  base_layers <- c("Combo", "CHAdeMO")
  
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
  
  observeEvent(globalinput$select_analysis,
               {
                 req(globalinput$select_analysis)
                 # print("Date time selected")
                 globals$stash$a_id <-
                   globals$stash$analyses$analysis_id[globals$stash$analyses$sim_date_time == globalinput$select_analysis]
                 # as.numeric(strsplit(globalinput$select_analysis, ' - ', fixed = TRUE)[[1]][2])
                 req(globals$stash$a_id)
                 # print(globals$stash$a_id)
                 # req(globals$stash$a_id)
                 bevs <- DBI::dbGetQuery(
                   globals$stash$pool,
                   glue::glue(
                     "select es.origin_zip,
       es.destination_zip,
       es.soc,
       es.trip_start_time,
       es.veh_id,
       wb.county,
       wb.city,
       wb.zip_code,
       wb.model_year,
       wb.make,
       wb.model,
       wb.electric_range,
       wb.legislative_district,
       wb.capacity,
       wb.fuel_consumption,
       wb.connector_code,
       CASE
           WHEN ef.veh_id IS NOT NULL
               THEN 'finished'
           WHEN est.veh_id IS NOT NULL
               THEN 'stranded'
           ELSE 'not sure'
           END as ending
from evtrip_scenarios es
         join wa_bevs wb on es.veh_id = wb.veh_id
         left join (select veh_id, analysis_id from ev_finished where analysis_id = {globals$stash$a_id}) as ef on es.veh_id = ef.veh_id
         left join (select veh_id, analysis_id from ev_stranded where analysis_id = {globals$stash$a_id}) as est on es.veh_id = est.veh_id
where es.analysis_id = {globals$stash$a_id};"
                   )
                 )
                 
                 a_id <- globals$stash$a_id
                 evse_query <-
                   paste0(
                     "SELECT evse_id, latitude, longitude, dcfc_count, connector_code from evses_now where analysis_id = ",
                     a_id,
                     " and dcfc_count > 0;"
                   )
                 evse_dcfc <-
                   DBI::dbGetQuery(globals$stash$pool, evse_query)
                 
                 # evse_dcfc <-
                 #   rbind(globals$stash$bevse_dcfc, nevse_dcfc)
                 
                 output$ev_table <- DT::renderDataTable({
                   if (nrow(bevs) > 0) {
                     DT::datatable(
                       bevs,
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
                         initComplete = DT::JS(
                           "function(settings, json) {",
                           "$(this.api().table().header()).css({'background-color': '#EBECEC', 'color': '#000'});",
                           "}"
                         )
                       ),
                       class = 'nowrap display',
                       extensions = c('Buttons', 'FixedColumns')
                     ) %>% DT::formatRound('capacity', 1) %>% DT::formatRound('fuel_consumption', 1)
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

                 # browser()
                 
                 output$wa_ooc_mapout <- leaflet::renderLeaflet({
                   # browser()
                   od_str <- NULL
                   # if (!rapportools::is.empty(rvData$simulation_runtime)) {
                   # req(globals$stash$a_id)
                   
                   all_chargers_combo <-
                     as.data.frame(evse_dcfc[evse_dcfc$connector_code == 2 |
                                               evse_dcfc$connector_code == 3,])
                   
                   all_chargers_chademo <-
                     as.data.frame(evse_dcfc[evse_dcfc$connector_code == 1 |
                                               evse_dcfc$connector_code == 3,])
                   
                   stranded_df <-
                     globals$stash$pool %>%
                     dplyr::tbl("ev_stranded") %>%
                     dplyr::filter(analysis_id == a_id) %>%
                     dplyr::collect()
                   
                   nevse_query <-
                     paste0(
                       "SELECT nevse_id, latitude, longitude, dcfc_plug_count, connector_code, station_type, comments from new_evses where analysis_id = ",
                       a_id
                     )
                   
                   nevse_dcfc <-
                     DBI::dbGetQuery(globals$stash$pool, nevse_query)
                   
                   
                   vmap <- leaflet::leaflet() %>%
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
                     leaflet::addMarkers(
                       lng = ~ longitude ,
                       lat = ~ latitude,
                       icon = evse_icon_blue,
                       group = base_layers[1],
                       data = all_chargers_combo
                     )  %>%
                     leaflet::addMarkers(
                       lng = ~ longitude ,
                       lat = ~ latitude,
                       icon = evse_icon_green,
                       group = base_layers[2],
                       data = all_chargers_chademo
                     ) %>%
                     # leaflet::addLabelOnlyMarkers(
                     #   lng = ~ longitude,
                     #   lat = ~ latitude,
                     #   data = dplyr::filter(evse_dcfc, grepl('n', evse_id)),
                     #   label = "new",
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
                     leaflet::addLayersControl(
                       overlayGroups = base_layers,
                       baseGroups = tile_layers,
                       options = leaflet::layersControlOptions(collapsed = FALSE)
                     )
                   # } else {
                   #   showModal(
                   #     modalDialog(
                   #       size = "s",
                   #       easyClose = TRUE,
                   #       "Select a simulation run time to see the stranded vehicles."
                   #     )
                   #   )
                   # }
                   
                   # print("After markers")
                   
                   if (dim(stranded_df)[1] != 0) {
                     vmap <- vmap %>% leaflet::addCircleMarkers(
                       lat = stranded_df$stranded_lat,
                       lng = stranded_df$stranded_lng,
                       radius = 4,
                       color = "#b50d2c",
                       popup = paste(
                         stranded_df$origin_zip,
                         stranded_df$destination_zip,
                         sep = "->"
                       ),
                       label = paste(
                         stranded_df$origin_zip,
                         stranded_df$destination_zip,
                         sep = "->"
                       ),
                       stroke = FALSE,
                       fillOpacity = 0.5
                     )
                   }
                   
                   
                   if (nrow(nevse_dcfc) > 0) {
                     vmap %>%
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
                     vmap
                   }
                   
                 })
                 
                 observeEvent(input$ev_table_rows_selected, {
                   row_selected = bevs[input$ev_table_rows_selected, ]
                   # Reset previously selected marker
                   if (!is.null(prev_ev_row()))
                   {
                     clearMapOverlay(mapID = "wa_ooc_mapout")
                   }
                   
                   plot_trajectory(row_selected, globals$stash$a_id, evse_dcfc)
                   
                   
                   # set new value to reactiveVal
                   prev_ev_row(row_selected)
                 })
                 
               },
               ignoreInit = TRUE,
               autoDestroy = FALSE)
}

## To be copied in the UI
# mod_bevs_ui("bevs_ui_1")

## To be copied in the server
# callModule(mod_bevs_server, "bevs_ui_1")
