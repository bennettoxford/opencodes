#' Helper function to create code usage table
#' @importFrom DT datatable formatPercentage formatRound formatStyle styleEqual
#' @keywords internal
datatable_usage <- function(data) {
  datatable(
    data,
    colnames = c("Code", "Description", "Usage", "Percentage"),
    rownames = FALSE,
    options = list(
      pageLength = 10,
      scrollY = TRUE,
      scrollX = TRUE,
      searching = TRUE
    ),
    width = "100%",
    fillContainer = TRUE
  ) |>
    formatPercentage(
      "total_pct",
      digits = 3
    ) |>
    formatRound(
      "total_usage",
      digits = 0
    )
}


#' Helper function to create codelist / selected codes table
#' @importFrom DT datatable
#' @keywords internal
datatable_codelist <- function(data, data_desc) {
  datatable(
    data,
    colnames = c("Code", "Description", "Usage data"),
    rownames = FALSE,
    options = list(
      language = list(
        emptyTable = "No codes selected, please select codes or load a codelist."
      ),
      order = list(list(2, "desc")),
      pageLength = 10,
      scrollY = FALSE,
      scrollX = TRUE,
      searching = TRUE
    ),
    width = "100%",
    fillContainer = TRUE
  ) |>
    formatStyle(
      "usage_data_available",
      color = styleEqual(
        c("Usage data available", "No usage data reported"),
        c("#35B779FF", "#ED6925FF")
      ),
      fontWeight = "bold"
    )
}
