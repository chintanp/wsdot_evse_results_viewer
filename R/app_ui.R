#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  evses_tab <- bs4Dash::bs4TabItem(tabName = "evses",
                                   mod_evses_ui("evses_ui_1"))
  
  bevs_tab <- bs4Dash::bs4TabItem(tabName = "bevs",
                                  mod_bevs_ui("bevs_ui_1"))
  
  summary_tab <- bs4Dash::bs4TabItem(tabName = "summary",
                                     mod_summary_stats_ui("summary_stats_ui_1"))
  
  
  
  tagList(# Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here
    fluidPage(
      bs4Dash::bs4DashPage(
        navbar = bs4Dash::bs4DashNavbar(
          skin = "light",
          status = "light",
          border = TRUE,
          elevation = 10,
          opacity = 0.5,
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
        sidebar = bs4Dash::bs4DashSidebar(
          skin = "dark",
          status = "info",
          brandColor = "info",
          url = "",
          src = "",
          elevation = 3,
          opacity = 0.3,
          width = 4,
          title = tags$b("EVI-DSS Results Viewer"),
          imageOutput("logo3", width = 200, height = 66),
          bs4Dash::bs4SidebarMenu(
            id = "sidebar",
            bs4Dash::bs4SidebarMenuItem(
              text = "View Results",
              icon = "poll",
              startExpanded = TRUE,
              bs4Dash::bs4SidebarMenuSubItem(
                text = "Summary",
                tabName = "summary",
                icon = "chart-bar"
              ),
              bs4Dash::bs4SidebarMenuSubItem(
                text = "BEVs",
                tabName = "bevs",
                icon = "car"
              ),
              bs4Dash::bs4SidebarMenuSubItem(
                text = "EVSEs",
                tabName = "evses",
                icon = "charging-station"
              )
            )
          )
        ),
        footer = bs4Dash::bs4DashFooter(
          bs4Dash::bs4DashFooter(
            copyrights = a(
              href = "https://faculty.washington.edu/dwhm/",
              target = "_blank",
              "Chintan Pathak and Don MacKenzie,
            UW"
            ),
            right_text = "2019-2020"
          )
        ),
        title = "EVI-ABM Results Viewer",
        body = bs4Dash::bs4DashBody(
          tags$head(tags$style(
            HTML(
              '
                .form-group, .selectize-control {
                  margin-bottom: 0px;
                }
                .box-body {
                  padding-bottom: 0px;
                }
                /* body 
                .content-wrapper, .right-side {
                  background-color: #343A40;
                } */
                '
            )
          )),
          bs4Dash::bs4TabItems(summary_tab,
                               evses_tab,
                               bevs_tab)
        )
      )
    ))
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path('www', app_sys('app/www'))
  
  tags$head(favicon(),
            bundle_resources(path = app_sys('app/www'),
                             app_title = 'resview'))
  # Add here other external resources
  # for example, you can add shinyalert::useShinyalert() )
}
