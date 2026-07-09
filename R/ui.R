#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#' @noRd
#' @import shiny
#' @import bslib
app_ui <- function(request) {
  tagList(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "www/styles.css")
    ),
    page_sidebar(
      theme = app_theme(),
      title = NULL,
      window_title = "opencodecounts",
      # Spinners on recalculating outputs so slow (re)loads never look broken
      useBusyIndicators(),
      sidebar = mod_sidebar_ui("sidebar"),
      # Main page
      # Value boxes
      mod_value_boxes_ui("value_boxes"),
      # Plots and tables
      navset_card_tab(
        full_screen = TRUE,
        mod_trends_ui("trends"),
        mod_usage_table_ui("usage_table"),
        mod_codes_table_ui("codes_table")
      )
    ),
    footer_ui()
  )
}
