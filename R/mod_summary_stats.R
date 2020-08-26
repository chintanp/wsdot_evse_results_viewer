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
      bs4Dash::bs4InfoBoxOutput(ns("finished_count")),
      bs4Dash::bs4InfoBoxOutput(ns("stranded_count")),
      bs4Dash::bs4InfoBoxOutput(ns("evmt_count")),
      bs4Dash::bs4InfoBoxOutput(ns("charging_session_count")),
      bs4Dash::bs4InfoBoxOutput(ns("evs_waited_count"))
      
    ),
    fluidRow(column(
      6,
      bs4Dash::bs4Card(
        title = "Overall EVSE Utilization",
        closable = FALSE,
        status = "success",
        collapsible = TRUE,
        labelTooltip = "Overall EVSE Utilization",
        elevation = 4,
        width = NULL,
        solidHeader = TRUE,
        maximizable = TRUE,
        plotly::plotlyOutput(ns("stat_evse_util_plot"))
      )
      
    ),
    column(
      6,
      bs4Dash::bs4Card(
        title = "Wait-time Distribution",
        closable = FALSE,
        status = "danger",
        collapsible = TRUE,
        labelTooltip = "Wait-time Distribution",
        elevation = 4,
        width = NULL,
        solidHeader = TRUE,
        maximizable = TRUE,
        plotly::plotlyOutput(ns("stat_evse_wait_plot"))
      )
    )),
    fluidRow(column(
      6,
      bs4Dash::bs4Card(
        title = "Charge-time Distribution",
        closable = FALSE,
        status = "success",
        collapsible = TRUE,
        labelTooltip = "Charge-time Distribution",
        elevation = 4,
        width = NULL,
        solidHeader = TRUE,
        maximizable = TRUE,
        plotly::plotlyOutput(ns("stat_cs_plot"))
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
           globals,
           globalinput) {
    ns <- session$ns
    
    observeEvent(globalinput$select_datetime, {
      req(globalinput$select_datetime)
      print("Date time selected")
      print(globalinput$select_datetime)
      globals$stash$a_id <- globals$stash$analyses$analysis_id[globals$stash$analyses$sim_date_time == globalinput$select_datetime]
      print(globals$stash$a_id)
      req(globals$stash$a_id)
      output$vehicle_count <- bs4Dash::renderbs4InfoBox({
        bs4Dash::bs4InfoBox(
          value = DBI::dbGetQuery(
            globals$stash$pool,
            paste0(
              "select count(veh_id) from evtrip_scenarios where analysis_id = ",
              globals$stash$a_id
            )
          )$count
          ,
          title = "EVs in Simulation",
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
