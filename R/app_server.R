#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import magrittr
#' @noRd
app_server <- function(input, output, session) {
  # reactlog::reactlog_render()
  output$logo3 <- renderImage({
    list(
      src = here::here("data-raw", "logo3c.png"),
      width = 200,
      height = 66,
      contentType = "image/png",
      alt = "logo"
    )
  }, deleteFile = FALSE)
  
  # GlobalData contains the globals and
  # these need to be passed to the modules in need
  GlobalData = callModule(GlobalModule, "globals")
  
  # Get the user id of the session from the URL
  observe({
    query <- parseQueryString(session$clientData$url_search)
    if (!is.null(query[['userid']])) {
      userid <- query[['userid']]
    } else {
      userid <- '5df30c5cceb5030df24a5d0f'
    }
    
    GlobalData$stash$analyses <-
      # GlobalData$stash$pool %>% dplyr::tbl("analysis_record") %>% dplyr::filter(user_id == userid &
      #                                                           status == "solved") %>% dplyr::collect()
      GlobalData$stash$pool %>% DBI::dbGetQuery(
        glue::glue(
          "select set_id, analysis_id, timezone('{Sys.timezone()}', date_trunc('second', sim_date_time)) as sim_date_time from analysis_record where status = 'solved' and user_id = '{userid}' order by sim_date_time desc"
        )
      )
    # browser()
    updateSelectInput(
      session,
      inputId = "select_analysis",
      choices = paste(as.character(GlobalData$stash$analyses$set_id),
        as.character(GlobalData$stash$analyses$analysis_id),
        GlobalData$stash$analyses$sim_date_time, sep = ' - '
      )
    )
    
    
  })
  
  # observeEvent(input$select_datetime, {
  #   req(input$select_datetime)
  #
  #   # GlobalData$stash$a_id <- GlobalData$stash$analyses$analysis_id[GlobalData$stash$analyses$sim_date_time == input$select_datetime]
  #   # print(GlobalData$stash$a_id)
  # })
  # List the first level callModules here
  callModule(mod_bevs_server, "bevs_ui_1", GlobalData, input)
  callModule(mod_evses_server, "evses_ui_1", GlobalData, input)
  callModule(mod_summary_stats_server,
             "summary_stats_ui_1",
             GlobalData,
             input)
}
