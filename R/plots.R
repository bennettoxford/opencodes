#' Helper function to plot code usage summary
#' @importFrom ggplot2 ggplot aes geom_line geom_point scale_x_date scale_y_continuous labs theme theme_classic element_text
#' @importFrom lubridate month year
#' @importFrom scales label_date_short label_comma comma
#' @keywords internal
plot_summary <- function(data) {
  scale_x_date_breaks <- unique(data$end_date)

  ggplot(
    data,
    aes(x = end_date, y = total_usage)
  ) +
    geom_line(
      colour = "#239b89ff",
      alpha = .4
    ) +
    geom_point(
      colour = "#239b89ff",
      size = 2,
      aes(text = paste0(
        "<b>Timeframe:</b> ",
        lubridate::month(start_date, label = TRUE), " ",
        lubridate::year(start_date), " to ",
        lubridate::month(end_date, label = TRUE), " ",
        lubridate::year(end_date),
        "<br>",
        "<b>Code usage:</b> ", scales::comma(total_usage)
      ))
    ) +
    scale_x_date(
      breaks = scale_x_date_breaks,
      labels = scales::label_date("%b\n%Y")
    ) +
    scale_y_continuous(
      limits = c(0, NA),
      labels = scales::label_number(accuracy = 1)
    ) +
    labs(x = NULL, y = NULL) +
    theme_classic() +
    theme(text = element_text(size = 14))
}

#' Helper function to fill 0 in missing years for plotting individual code usage
#' @importFrom tidyr complete
#' @keywords internal
fill_missing_usage_with_zeros <- function(data){
  data <- data |>
    group_by(code)|>
    complete(end_date = seq.Date(from = min(end_date), 
                                 to = max(end_date), 
                                 by = "year"),
             fill = list(usage = 0))|>
    arrange(code, end_date) |>
    mutate(start_date = seq.Date(from = min(start_date, na.rm = TRUE), 
                                 to = max(start_date, na.rm = TRUE), 
                                 by = "year"))|>
    ungroup()
  return(data)
}

#' Helper function to plot individual code usage
#' @importFrom ggplot2 ggplot aes geom_line geom_point scale_x_date scale_y_continuous labs theme theme_classic element_text
#' @importFrom lubridate month year
#' @importFrom scales label_date_short label_comma comma
#' @keywords internal
plot_individual <- function(data) {
  scale_x_date_breaks <- unique(data$end_date)
  
  data <- data |> fill_missing_usage_with_zeros()

  data <- data |>
    group_by(start_date, end_date) |>
    summarise(
      code = code,
      description = description,
      usage = usage,
      annual_proportion = usage / sum(usage, na.rm = TRUE)
    )


  ggplot(
    data,
    aes(
      x = end_date,
      y = usage,
      colour = code
    )
  ) +
    geom_line(alpha = .4) +
    geom_point(
      size = 2,
      aes(text = paste0(
        "<b>Timeframe:</b> ",
        lubridate::month(start_date, label = TRUE), " ",
        lubridate::year(start_date), " to ",
        lubridate::month(end_date, label = TRUE), " ",
        lubridate::year(end_date),
        "<br>",
        "<b>Code:</b> ", code, "<br>",
        "<b>Description:</b> ", description, "<br>",
        "<b>Code usage:</b> ", scales::comma(usage), "<br>",
        "<b>Proportion of annual usage: </b>", scales::percent(annual_proportion, accuracy = 0.01)
      ))
    ) +
    scale_x_date(
      breaks = scale_x_date_breaks,
      labels = scales::label_date("%b\n%Y")
    ) +
    scale_y_continuous(
      limits = c(0, NA),
      labels = scales::label_number(accuracy = 1)
    ) +
    ggplot2::scale_colour_viridis_d() +
    labs(x = NULL, y = NULL) +
    theme_classic() +
    theme(
      text = element_text(size = 14),
      legend.position = "none"
    )
}

#' Helper function to plot sparkline
#' @importFrom dplyr group_by summarise
#' @importFrom plotly plot_ly add_lines layout config
#' @keywords internal
plot_sparkline <- function(data) {
  data_spark <- data |>
    group_by(end_date) |>
    summarise(total_usage = sum(usage, na.rm = TRUE))

  plot_ly(data_spark, hoverinfo = "none") |>
    add_lines(
      x = ~end_date, y = ~total_usage,
      color = I("black"), span = I(1),
      fill = "tozeroy", alpha = 0.2
    ) |>
    layout(
      xaxis = list(visible = F, showgrid = F, title = ""),
      yaxis = list(visible = F, showgrid = F, title = ""),
      hovermode = "x",
      margin = list(t = 0, r = 0, l = 0, b = 0),
      font = list(color = "white"),
      paper_bgcolor = "transparent",
      plot_bgcolor = "transparent"
    ) |>
    config(displayModeBar = FALSE) |>
    layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))
}
