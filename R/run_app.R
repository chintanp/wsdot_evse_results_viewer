#' Run the Shiny Application
#'
#' @param ... A series of options to be used inside the app.
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_app <- function(
  port = as.numeric(Sys.getenv("RESVIEW_PORT")),
  launch.browser = TRUE,
  ...
) {
  with_golem_options(
    app = shinyApp(
      ui = app_ui, 
      server = app_server, 
      options = list(port = port)
    ), 
    golem_opts = list(...)
  )
}
