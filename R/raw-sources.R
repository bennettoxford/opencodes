#' Get raw NHS Digital source locations for a dataset
#'
#' Reads the matching `inst/config/raw_<publication>.yml` file and returns
#' its `periods` list - the same shape as the hardcoded url lists that used
#' to live at the top of each `data-raw/*.R` script. To add a new year of
#' data, add one entry to that file's `periods:` map - the `data-raw/*.R`
#' scripts don't need to change.
#'
#' A "publication" is one NHS Digital release (snomed, icd10, opcs4). Some
#' publications back more than one dataset - icd10_usage and
#' icd10_usage_breakdowns, for example, both read the same xlsx file per
#' period, just a different sheet region - so `raw_icd10.yml` stores the
#' shared `url`/`sheet` once per period, with `usage`/`breakdowns` sub-fields
#' for what each dataset reads from it. This function resolves the dataset
#' name to its publication file and merges in the right sub-fields.
#'
#' @param dataset String, dataset name, e.g. "snomed_usage", "icd10_usage",
#'   "icd10_usage_breakdowns"
#'
#' @return A named list, one entry per period. For `snomed_usage`, each entry
#'   is a single url string. For the others, each entry is a list with `url`
#'   plus the fields that dataset's data-raw script needs to read the right
#'   part of the file (e.g. `sheet`, `skip_rows`, `range`).
#'
#' @importFrom yaml read_yaml
#'
#' @keywords internal
get_raw_source_periods <- function(dataset) {
  publication <- sub("_usage(_breakdowns)?$", "", dataset)

  config_path <- system.file(
    "config",
    paste0("raw_", publication, ".yml"),
    package = "opencodecounts"
  )

  if (config_path == "") {
    stop(
      "inst/config/raw_",
      publication,
      ".yml not found in package installation",
      call. = FALSE
    )
  }

  periods <- read_yaml(config_path)$periods

  # snomed_usage is the only dataset for its publication, so periods are
  # already the final shape (plain url strings) - nothing to merge in.
  if (!is.list(periods[[1]])) {
    return(periods)
  }

  variant <- if (grepl("_breakdowns$", dataset)) "breakdowns" else "usage"
  lapply(periods, function(period) {
    shared_fields <- period[setdiff(names(period), c("usage", "breakdowns"))]
    c(shared_fields, period[[variant]])
  })
}
