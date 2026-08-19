#' Load the Shiny app's dataset registry
#'
#' Reads `inst/config/shiny_app_datasets.yml`, which lists every dataset the app's
#' "Select data" dropdown offers, keyed by dataset id (e.g. "snomedct").
#'
#' @return A named list of dataset configs
#'
#' @importFrom yaml read_yaml
#'
#' @keywords internal
load_dataset_registry <- function() {
  config_path <- system.file(
    "config",
    "shiny_app_datasets.yml",
    package = "opencodecounts"
  )

  if (config_path == "") {
    stop("shiny_app_datasets.yml not found in package installation", call. = FALSE)
  }

  read_yaml(config_path)$datasets
}

#' Dataset choices for the app's dataset radioButtons
#'
#' @return A named character vector suitable for `shiny::radioButtons(choices = ...)`:
#'   labels are names, dataset ids are values
#'
#' @keywords internal
dataset_choices <- function() {
  registry <- load_dataset_registry()
  labels <- vapply(registry, `[[`, character(1), "label")
  stats::setNames(names(registry), labels)
}

#' Get one dataset's config from the registry
#'
#' @param dataset_id String, dataset id as listed in `shiny_app_datasets.yml` (e.g. "snomedct")
#'
#' @return A list with `label`, `dataset`, `get_function`, `code_column`,
#'   `description_column`, `has_code_pattern_search`, `code_pattern_label`,
#'   `source_label`, `source_url`
#'
#' @keywords internal
get_dataset_by_id <- function(dataset_id) {
  registry <- load_dataset_registry()

  if (!dataset_id %in% names(registry)) {
    stop(
      "Dataset '",
      dataset_id,
      "' not found in shiny_app_datasets.yml. ",
      "Available datasets: ",
      paste(names(registry), collapse = ", "),
      call. = FALSE
    )
  }

  registry[[dataset_id]]
}
