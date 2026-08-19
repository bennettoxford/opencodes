#' Trends module: usage over time plot with individual codes switch
#'
#' Returns a [bslib::nav_panel()] for use inside the main navset.
#'
#' @param id Module id.
#' @noRd
#' @import shiny
#' @import bslib
#' @import bsicons
#' @importFrom plotly plotlyOutput
mod_trends_ui <- function(id) {
  ns <- NS(id)

  nav_panel(
    p(bs_icon("graph-up"), "Trends over time"),
    input_switch(
      ns("show_individual_codes"),
      tooltip_label(
        "Show individual codes",
        "This is only supported for up to 500 selected codes.",
        placement = "right"
      ),
      value = FALSE
    ),
    plotlyOutput(ns("usage_plot"))
  )
}

#' Trends module server
#'
#' @param id Module id.
#' @param filtered_data Reactive returning the filtered usage data.
#' @param dataset Reactive returning the selected dataset id; the individual
#'   codes switch is reset when it changes.
#' @param reset Reactive returning the reset button value; the switch is also
#'   reset when it fires.
#' @noRd
#' @import shiny
#' @import bslib
#' @import dplyr
#' @import ggplot2
#' @importFrom plotly renderPlotly ggplotly
mod_trends_server <- function(id, filtered_data, dataset, reset) {
  moduleServer(id, function(input, output, session) {
    # Reset the switch when the dataset changes or the code selection is reset
    observe({
      update_switch("show_individual_codes", value = FALSE, session = session)
    }) |>
      bindEvent(dataset(), reset())

    # PLOT: Trends over time
    output$usage_plot <- renderPlotly({
      withProgress(message = "Plotting data ...", {
        # Check if filtered_data is empty
        # As a workaround we are adding a plot with text only if the
        # search criteria match no data. At some point in the future we
        # should reconsider if this is the best approach.
        # text if there are no codes
        if (nrow(filtered_data()) == 0) {
          # Create a text-only plot for empty data
          p <- ggplot() +
            geom_text(
              aes(
                x = 1,
                y = 1,
                label = "No data matches the search criteria."
              ),
              size = 6
            ) +
            theme_void() +
            theme(
              axis.line = element_blank(),
              panel.grid = element_blank()
            )

          return(
            ggplotly(p, tooltip = "text") |>
              plotly::config(displayModeBar = FALSE)
          )
        }

        unique_codes <- length(unique(filtered_data()$code))

        # When there are 500 or less selected codes, impute 0 usage
        # in the annual usage gaps. We do this because the absence of data for
        # any particular year between two available years implies that the usage
        # for that year is 0. However, the plots do not show zero but interpolate
        # a line between the two last available data points, which is misleading.
        # We impute 0 usage in annual usage gaps in the summary plot
        # when the number of selected codes is =< 500 and in the individual
        # codes plot (which, by default, can only be displayed with =< 500
        # selected codes). This cut-off was selected for efficiency, since if the user
        # selects >500 codes, the imputation will take longer, but the total usage
        # shown on the summary plot for any year is unlikely to be zero, making
        # imputation redundant.
        if (unique_codes <= 500) {
          df_plot <- complete_usage_gaps_with_zeros(filtered_data())
        } else {
          df_plot <- filtered_data()
        }

        # Handle individual code display based on number of unique codes
        if (input$show_individual_codes & unique_codes <= 500) {
          p <- df_plot |>
            plot_individual()
        } else {
          if (input$show_individual_codes & unique_codes > 500) {
            showNotification(
              "Too many codes to show individually. To show individual code usage reduce to 500 or fewer selected codes.",
              type = "error"
            )
          }

          p <- df_plot |>
            group_by(start_date, end_date) |>
            summarise(total_usage = sum(usage, na.rm = TRUE)) |>
            plot_summary()
        }

        ggplotly(p, tooltip = "text") |>
          plotly::config(displayModeBar = FALSE)
      })
    })
  })
}
