#' Run [Code Usage Explorer](https://milanwiedemann.shinyapps.io/opencodes/) Shiny App
#'
#' @export
run_app <- function() {
  shiny::shinyApp(
    ui = app_ui,
    server = app_server,
  )
}
