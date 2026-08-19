#' Run `opencodecounts` [Shiny App](https://milanwiedemann-opencodecounts.share.connect.posit.cloud/) locally
#'
#' @export
run_app <- function() {
  ensure_app_datasets_cached()

  shiny::addResourcePath(
    "www",
    system.file("shiny/www", package = "opencodecounts")
  )

  shiny::shinyApp(
    ui = app_ui,
    server = app_server
  )
}
