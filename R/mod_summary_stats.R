#' summary_stats UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_summary_stats_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h4("Simulation Summary"),
    fluidRow(
      bs4Dash::bs4InfoBoxOutput(ns("vehicle_count")),
      bs4Dash::bs4InfoBoxOutput(ns("evse_count")),
      bs4Dash::bs4InfoBoxOutput(ns("plug_count")),
      bs4Dash::bs4InfoBoxOutput(ns("finished_count")),
      bs4Dash::bs4InfoBoxOutput(ns("stranded_count")),
      bs4Dash::bs4InfoBoxOutput(ns("evmt_count")),
      bs4Dash::bs4InfoBoxOutput(ns("charging_session_count")),
      bs4Dash::bs4InfoBoxOutput(ns("evs_waited_count"))
      
    ),
    fluidRow(column(
      12,
      bs4Dash::bs4TabCard(
        id = "plots_tabcard",
        title = tags$p("Plots",
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
        bs4Dash::bs4TabPanel(tabName = "EVSE Utilization",
                             plotly::plotlyOutput(ns(
                               "stat_evse_util_plot"
                             ))),
        bs4Dash::bs4TabPanel(tabName = "Wait Time",
                             plotly::plotlyOutput(ns(
                               "stat_evse_wait_plot"
                             ))),
        bs4Dash::bs4TabPanel(tabName = "Charge Time",
                             plotly::plotlyOutput(ns("stat_cs_plot")))
      )
    ))
  )
}

#' summary_stats Server Function
#'
#' @noRd
mod_summary_stats_server <-
  function(input,
           output,
           session,
           globals) {
    ns <- session$ns
    
    observe({
      # req(globalinput$select_datetime)
      # print("Date time selected")
      # print(globalinput$select_datetime)
      # globals$stash$a_id <-
      #   globals$stash$analyses$analysis_id[globals$stash$analyses$sim_date_time == globalinput$select_datetime]
      print(globals$stash$a_id)
      req(globals$stash$a_id)
      
      nevse_query <-
        paste0(
          "SELECT concat('n', nevse_id) as evse_id, latitude, longitude, dcfc_plug_count as dcfc_count, connector_code from new_evses where dcfc_plug_count > 0 and analysis_id = ",
          globals$stash$a_id
        )
      nevse_dcfc <-
        DBI::dbGetQuery(globals$stash$pool, nevse_query)
      
      evse_dcfc <-
        rbind(globals$stash$bevse_dcfc, nevse_dcfc)
      
      output$vehicle_count <- bs4Dash::renderbs4InfoBox({
        bs4Dash::bs4InfoBox(
          value = paste0(
            DBI::dbGetQuery(
              globals$stash$pool,
              paste0(
                "select count(veh_id) from evtrip_scenarios where analysis_id = ",
                globals$stash$a_id
              )
            )$count,
            ' | ',
            DBI::dbGetQuery(
              globals$stash$pool,
              paste0("select count(veh_id) from wa_bevs")
            )$count
          )
          ,
          title = "Simulated | Total EVs",
          icon = "layer-group"
        )
      })
      
      output$evse_count <- bs4Dash::renderbs4InfoBox({
        bs4Dash::bs4InfoBox(
          value = paste0(nrow(globals$stash$bevse_dcfc), ' | ', nrow(nevse_dcfc))
          ,
          title = "Built | New EVSEs",
          icon = "layer-group"
        )
      })
      
      output$plug_count <- bs4Dash::renderbs4InfoBox({
        chademo_total <-
          evse_dcfc %>% dplyr::filter(connector_code == 1 |
                                        connector_code == 3)
        combo_total <-
          evse_dcfc %>% dplyr::filter(connector_code == 2 |
                                        connector_code == 3)
        
        bs4Dash::bs4InfoBox(
          value = paste0(as.character(sum(
            chademo_total$dcfc_count
          )), ' | ',
          as.character(sum(
            combo_total$dcfc_count
          ))),
          title = "CHAdeMO | COMBO Plug Count",
          icon = "layer-group"
        )
      })
      
      output$finished_count <- bs4Dash::renderbs4InfoBox({
        bs4Dash::bs4InfoBox(
          value = DBI::dbGetQuery(
            globals$stash$pool,
            paste0(
              "select count(veh_id) from ev_finished where analysis_id = ",
              globals$stash$a_id
            )
          )$count,
          title = "EVs finishing trip",
          icon = "car"
        )
      })
      #
      output$stranded_count <- bs4Dash::renderbs4InfoBox({
        bs4Dash::bs4InfoBox(
          value = DBI::dbGetQuery(
            globals$stash$pool,
            paste0(
              "select count(veh_id) from ev_stranded where analysis_id = ",
              globals$stash$a_id
            )
          )$count,
          title = "EVs stranded",
          icon = "car-crash"
        )
      })
      #
      output$evmt_count <- bs4Dash::renderbs4InfoBox({
        bs4Dash::bs4InfoBox(
          value = DBI::dbGetQuery(
            globals$stash$pool,
            paste0(
              "select sum(distance_travelled) from ev_finished where analysis_id = ",
              globals$stash$a_id
            )
          )$sum,
          title = "eVMT",
          icon = "bolt"
        )
      })
      #
      output$charging_session_count <- bs4Dash::renderbs4InfoBox({
        bs4Dash::bs4InfoBox(
          value = DBI::dbGetQuery(
            globals$stash$pool,
            paste0(
              "select count(cs_id) from evse_charging_session where analysis_id = ",
              globals$stash$a_id
            )
          )$count,
          title = "Number of charging sessions",
          icon = "charging-station"
        )
      })
      
      output$evs_waited_count <- bs4Dash::renderbs4InfoBox({
        bs4Dash::bs4InfoBox(
          value = DBI::dbGetQuery(
            globals$stash$pool,
            paste0(
              "select count(wait_id) from evse_evs_waiting where analysis_id = ",
              globals$stash$a_id
            )
          )$count,
          title = "Number of EVs waiting",
          icon = "square-full"
        )
      })
      
      output$built_stn_count <- renderText({
        nrow(globals$stash$bevse_dcfc)
      })
      
      output$new_stn_count <- renderText({
        nrow(nevse_dcfc)
      })
      
      output$built_new_chademo_plugcount <- renderText({
        chademo_built <-
          globals$stash$bevse_dcfc %>% dplyr::filter(connector_code == 1 |
                                                       connector_code == 3)
        chademo_new <-
          nevse_dcfc %>% dplyr::filter(connector_code == 1 |
                                         connector_code == 3)
        
        paste0(as.character(sum(chademo_built$dcfc_count)), " (", as.character(sum(chademo_new$dcfc_plug_count)), ")")
      })
      
      output$built_new_combo_plugcount <- renderText({
        combo_built <-
          globals$stash$bevse_dcfc %>% dplyr::filter(connector_code == 2 |
                                                       connector_code == 3)
        combo_new <-
          nevse_dcfc %>% dplyr::filter(connector_code == 2 |
                                         connector_code == 3)
        
        paste0(as.character(sum(combo_built$dcfc_count)), " (", as.character(sum(combo_new$dcfc_plug_count)), ")")
      })
      
      output$stat_evse_util_plot <- plotly::renderPlotly({
        total_power_draw <-
          DBI::dbGetQuery(
            globals$stash$pool,
            paste0(
              "SELECT  epd.simulation_ts::timestamp as datetime,
                       sum(epd.power_val) as total_power
                FROM evse_power_draw epd
                WHERE analysis_id = ",
              globals$stash$a_id,
              " GROUP BY epd.simulation_ts
                  ORDER BY epd.simulation_ts::timestamp;"
            )
          )
        
        max_power <- max(total_power_draw$total_power)
        max_power_index <- which.max(total_power_draw$total_power)
        
        fig <-
          plotly::plot_ly(
            total_power_draw,
            x = ~ datetime,
            y = ~ total_power,
            type = 'scatter',
            mode = 'lines'
          ) %>%
          plotly::layout(
            xaxis = list(title = "Time of day (minutes)"),
            yaxis = list(title = "Power (kW)")
          )
        
        fig
        
      })
      
      
      output$stat_evse_wait_plot <- plotly::renderPlotly({
        # browser()
        wait_time_mins <-
          DBI::dbGetQuery(
            globals$stash$pool,
            paste0(
              "SELECT
                  ((DATE_PART('day', evw.wait_end_time::TIMESTAMP - evw.wait_start_time::TIMESTAMP) * 24 + DATE_PART('hour', evw.wait_end_time::TIMESTAMP - evw.wait_start_time::TIMESTAMP)) * 60 + DATE_PART('minute', evw.wait_end_time::TIMESTAMP - evw.wait_start_time::TIMESTAMP)) AS wait_time_mins
                  FROM evse_evs_waiting evw
                  WHERE evw.analysis_id = ",
              globals$stash$a_id
            )
          )$wait_time_mins
        dens_wait_time <- density(as.numeric(wait_time_mins))
        # max_dens <- max(dens_wait_time)
        max_dens_index <- which.max(dens_wait_time$y)
        
        df <-
          data.frame(x = unlist(dens_wait_time$x),
                     y = unlist(dens_wait_time$y))
        
        fig <-
          plotly::plot_ly(
            df,
            x = ~ x,
            y = ~ y,
            type = 'scatter',
            mode = 'lines'
          ) %>%
          plotly::layout(
            xaxis = list(title = "Waiting Time (minutes)", range = c(0, max(
              dens_wait_time$x
            ))),
            yaxis = list(title = "Density")
          )
        
        fig
        
      })
      #
      output$stat_cs_plot <- plotly::renderPlotly({
        # browser()
        cs_time_mins <- DBI::dbGetQuery(
          globals$stash$pool,
          paste0(
            "SELECT
                  ((DATE_PART('day', ecs.charge_end_time::TIMESTAMP - ecs.charge_start_time::TIMESTAMP) * 24 + DATE_PART('hour', ecs.charge_end_time::TIMESTAMP - ecs.charge_start_time::TIMESTAMP)) * 60 + DATE_PART('minute', ecs.charge_end_time::TIMESTAMP - ecs.charge_start_time::TIMESTAMP)) AS cs_time_mins
                  FROM evse_charging_session ecs
                  WHERE ecs.analysis_id = ",
            globals$stash$a_id
          )
        )$cs_time_mins
        
        dens_charge_time <- density(as.numeric(cs_time_mins))
        # max_dens <- max(dens_wait_time)
        max_dens_index <- which.max(dens_charge_time$y)
        
        df <-
          data.frame(x = unlist(dens_charge_time$x),
                     y = unlist(dens_charge_time$y))
        
        # browser()
        fig <-
          plotly::plot_ly(
            df,
            x = ~ x,
            y = ~ y,
            type = 'scatter',
            mode = 'lines'
          ) %>%
          plotly::layout(
            xaxis = list(title = "Charging Time (minutes)", range = c(0, max(
              dens_charge_time$x
            ))),
            yaxis = list(title = "Density")
          )
        
        fig
      })
      
    })
  }

## To be copied in the UI
# mod_summary_stats_ui("summary_stats_ui_1")

## To be copied in the server
# callModule(mod_summary_stats_server, "summary_stats_ui_1")
