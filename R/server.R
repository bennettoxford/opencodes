#' The application server-side
#'
#' Wires the sidebar module's returns into the central `filtered_data()`
#' reactive and passes it to the display modules.
#'
#' @param input,output,session Internal parameters for {shiny}.
#' @noRd
#' @import shiny
#' @import dplyr
app_server <- function(input, output, session) {
  sidebar <- mod_sidebar_server("sidebar")

  # Filtered usage data
  filtered_data <- reactive({
    req(sidebar$selected_data(), sidebar$date_range())

    withProgress(message = "Filtering data ...", {
      data <- sidebar$selected_data()

      data <- data |>
        filter(
          end_date >= sidebar$date_range()[1] &
            end_date <= sidebar$date_range()[2]
        )

      # Apply filters based on the current filtering method
      if (sidebar$search_method() == "search") {
        if (
          !is.null(sidebar$code_specific_search()) &&
            length(sidebar$code_specific_search()) > 0
        ) {
          data <- data |>
            filter(code %in% sidebar$code_specific_search())
        }

        if (
          !is.null(sidebar$code_pattern_search()) &&
            sidebar$code_pattern_search() != ""
        ) {
          data <- data |>
            filter(grepl(
              paste("^", sidebar$code_pattern_search(), sep = ""),
              code,
              ignore.case = TRUE
            ))
        }

        if (
          !is.null(sidebar$description_search()) &&
            sidebar$description_search() != ""
        ) {
          data <- data |>
            filter(grepl(
              sidebar$description_search(),
              description,
              ignore.case = TRUE
            ))
        }
      } else if (sidebar$search_method() == "codelist") {
        req(sidebar$codelist())
        data <- data |>
          filter(code %in% sidebar$codelist()$code)
      }

      if (nrow(data) == 0) {
        showNotification(
          "No data matches your current filters.",
          type = "warning"
        )
      }
      data
    })
  })

  mod_value_boxes_server("value_boxes", filtered_data)
  mod_trends_server(
    "trends",
    filtered_data,
    dataset = sidebar$dataset,
    reset = sidebar$reset
  )
  mod_usage_table_server(
    "usage_table",
    filtered_data,
    dataset = sidebar$dataset
  )
  mod_codes_table_server(
    "codes_table",
    filtered_data,
    search_method = sidebar$search_method,
    codelist = sidebar$codelist,
    dataset = sidebar$dataset
  )
}
