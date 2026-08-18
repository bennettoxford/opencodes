#' Get a dataset, downloading and caching it if necessary
#'
#' @param dataset String, dataset name as listed in `tidy_data_sources.yml`
#' @param version String, dataset version, or `NULL` for the latest
#'
#' @return Tibble
#'
#' @keywords internal
get_dataset <- function(dataset, version = NULL) {
  cfg <- get_tidy_source_config(dataset, version)
  resolved_version <- cfg$version

  if (!tidy_source_cache_is_current(dataset, resolved_version)) {
    download_tidy_source(dataset, cfg$url, resolved_version)
  }

  load_tidy_source(dataset, resolved_version)
}

#' Get yearly SNOMED CT code usage in primary care in England
#'
#' Downloads (if not already cached) and returns the yearly summary of
#' SNOMED CT code usage from 1st August 2011 onwards. The variables in this
#' dataset include:
#' \describe{
#'   \item{start_date}{Start date of code usage count}
#'   \item{end_date}{End date of code usage count}
#'   \item{snomed_code}{SNOMED Concept ID}
#'   \item{usage}{Yearly summary of code usage.
#'   Note that counts are rounded to the nearest 10.
#'   Counts of 5 or below are displayed as 5.}
#'   \item{active_at_start}{Specifying whether code was active at the start date.}
#'   \item{active_at_end}{Specifying whether code was active at the end date.}
#'   \item{description}{Description of SNOMED Concept ID}
#' }
#'
#' @param version String, dataset version, or `NULL` for the latest (default)
#'
#' @return A tibble
#'
#' @source <https://digital.nhs.uk/data-and-information/publications/statistical/mi-snomed-code-usage-in-primary-care>
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Filter for code usage records from 2022-08-01 onwards
#' get_snomed_usage() |>
#'   dplyr::filter(start_date >= "2022-08-1")
#' }
get_snomed_usage <- function(version = NULL) {
  get_dataset("snomed_usage", version)
}

#' Get yearly ICD-10 code usage from Hospital Admitted Patient Care Activity in England
#'
#' Downloads (if not already cached) and returns the yearly summary of
#' 4-character ICD-10 code usage from 1st April 2012 onwards. The code usage
#' represents the annual count of all episodes which record the given ICD-10
#' code in any primary or secondary position. Restricted codes for which
#' annual usage is not published have been removed.
#' \describe{
#'   \item{start_date}{Start date of code usage count}
#'   \item{end_date}{End date of code usage count}
#'   \item{icd10_code}{The 4-character ICD-10 Code.
#'   Note that the punctuation from the code has been removed for compatibility with OpenCodelists.}
#'   \item{usage}{Annual count of code usage.}
#'   \item{description}{Description of the ICD-10 Code}
#' }
#'
#' @param version String, dataset version, or `NULL` for the latest (default)
#'
#' @return A tibble
#'
#' @source <https://digital.nhs.uk/data-and-information/publications/statistical/hospital-admitted-patient-care-activity>
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Filter to codes in the ICD-10 Chapter XIX: "Injury, poisoning..."
#' # (codes begin with letters "S" or "T"), with usage > 10,000.
#' get_icd10_usage() |>
#'   dplyr::filter(grepl("^[ST]", icd10_code) & usage > 10000)
#' }
get_icd10_usage <- function(version = NULL) {
  get_dataset("icd10_usage", version)
}

#' Get yearly ICD-10 code usage breakdowns from Hospital Admitted Patient Care Activity in England
#'
#' Downloads (if not already cached) and returns the yearly summary of
#' 4-character ICD-10 code usage with demographic breakdowns from 1st April
#' 2012 onwards. Includes breakdowns by diagnosis type (all/main), sex, and
#' age group. Restricted codes for which annual usage is not published have
#' been removed.
#' \describe{
#'   \item{start_date}{Start date of code usage count}
#'   \item{end_date}{End date of code usage count}
#'   \item{icd10_code}{The 4-character ICD-10 Code.
#'   Note that the punctuation from the code has been removed for compatibility with OpenCodelists.}
#'   \item{description}{Description of the ICD-10 Code}
#'   \item{breakdown}{Type of breakdown: all_diagnoses, main_diagnosis, male, female,
#'   gender_unknown, or age groups (age_0, age_1_4, age_5_9, ..., age_85_89, age_90plus)}
#'   \item{usage}{Annual count of code usage. NA where suppressed due to small numbers.}
#' }
#'
#' @param version String, dataset version, or `NULL` for the latest (default)
#'
#' @return A tibble
#'
#' @source <https://digital.nhs.uk/data-and-information/publications/statistical/hospital-admitted-patient-care-activity>
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Compare male vs female usage for codes containing "pregnancy"
#' get_icd10_usage_breakdowns() |>
#'   dplyr::filter(grepl("pregnancy", description, ignore.case = TRUE)) |>
#'   dplyr::filter(breakdown %in% c("male", "female"))
#' }
get_icd10_usage_breakdowns <- function(version = NULL) {
  get_dataset("icd10_usage_breakdowns", version)
}

#' Get yearly OPCS-4 code usage from Hospital Admitted Patient Care Activity in England
#'
#' Downloads (if not already cached) and returns the yearly summary of
#' 4-character OPCS-4 code usage from 1st April 2012 onwards. The code usage
#' represents the total annual count of each procedure, recorded across the
#' primary and the secondary procedure positions. Restricted codes for which
#' annual usage is not published have been removed.
#' \describe{
#'   \item{start_date}{Start date of code usage count}
#'   \item{end_date}{End date of code usage count}
#'   \item{opcs4_code}{The 4-character OPCS-4 code.
#'   Note that the punctuation from the code has been removed for compatibility with OpenCodelists.}
#'   \item{usage}{Annual count of code usage.}
#'   \item{description}{Description of the OPCS-4 Code}
#' }
#'
#' @param version String, dataset version, or `NULL` for the latest (default)
#'
#' @return A tibble
#'
#' @source <https://digital.nhs.uk/data-and-information/publications/statistical/hospital-admitted-patient-care-activity>
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Filter to procedures involving "biopsy" after March 2020 (note each year runs April - March).
#' get_opcs4_usage() |>
#'   dplyr::filter(grepl("biopsy", description, ignore.case = TRUE) & lubridate::year(end_date) > 2020)
#' }
get_opcs4_usage <- function(version = NULL) {
  get_dataset("opcs4_usage", version)
}

#' Get yearly OPCS-4 code usage breakdowns from Hospital Admitted Patient Care Activity in England
#'
#' Downloads (if not already cached) and returns the yearly summary of
#' 4-character OPCS-4 code usage with demographic breakdowns from 1st April
#' 2012 onwards. Includes breakdowns by procedure type (all/main), sex, and
#' age group. Restricted codes for which annual usage is not published have
#' been removed.
#' \describe{
#'   \item{start_date}{Start date of code usage count}
#'   \item{end_date}{End date of code usage count}
#'   \item{opcs4_code}{The 4-character OPCS-4 code.
#'   Note that the punctuation from the code has been removed for compatibility with OpenCodelists.}
#'   \item{description}{Description of the OPCS-4 Code}
#'   \item{breakdown}{Type of breakdown: all_procedures, main_procedure, male, female,
#'   gender_unknown, or age groups (age_0, age_1_4, age_5_9, ..., age_85_89, age_90plus)}
#'   \item{usage}{Annual count of code usage. NA where suppressed due to small numbers.}
#' }
#'
#' @param version String, dataset version, or `NULL` for the latest (default)
#'
#' @return A tibble
#'
#' @source <https://digital.nhs.uk/data-and-information/publications/statistical/hospital-admitted-patient-care-activity>
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Get sex breakdown for hip replacement procedures
#' get_opcs4_usage_breakdowns() |>
#'   dplyr::filter(grepl("hip replacement", description, ignore.case = TRUE)) |>
#'   dplyr::filter(breakdown %in% c("male", "female"))
#' }
get_opcs4_usage_breakdowns <- function(version = NULL) {
  get_dataset("opcs4_usage_breakdowns", version)
}
