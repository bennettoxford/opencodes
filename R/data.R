#' @importFrom tibble tibble
NULL

#' Yearly SNOMED CT Code Usage in Primary Care in England
#'
#' Yearly summary of SNOMED CT code usage from 1st August 2011 to 31st July 2025.
#' The variables in this dataset include:
#' @format A data frame with 1,682,534 rows and 7 columns:
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
#' @source <https://digital.nhs.uk/data-and-information/publications/statistical/mi-snomed-code-usage-in-primary-care>
#' @examples
#' # Filter for code usage records from 2022-08-01 onwards
#' snomed_usage |>
#'   dplyr::filter(start_date >= "2022-08-1")
#'
#' # Filter for code usage records from 2022-08-01 onwards
#' # where the description contains the word "anxiety"
#' snomed_usage |>
#'   dplyr::filter(start_date >= "2022-08-1") |>
#'   dplyr::filter(grepl("anxiety", description, ignore.case = TRUE))
"snomed_usage"

#' Yearly ICD-10 Code Usage from Hospital Admitted Patient Care Activity in England
#'
#' Yearly summary of 4-character ICD-10 code usage from 1st April 2013 to 31st March 2025.
#' The code usage represents the annual count of all episodes which record the given ICD-10 code in any primary or secondary position.
#' Restricted codes for which annual usage is not published have been removed.
#' Yearly summary of 4-character ICD-10 code usage from 1st April 2013 to 31st March 2025.
#' The code usage represents the annual count of all episodes which record the given ICD-10 code in any primary or secondary position.
#' Restricted codes for which annual usage is not published have been removed.
#' @format A data frame with 147,483 rows and 5 columns:
#' \describe{
#'   \item{start_date}{Start date of code usage count}
#'   \item{end_date}{End date of code usage count}
#'   \item{icd10_code}{The 4-character ICD-10 Code.
#'   Note that the punctuation from the code has been removed for compatibility with OpenCodelists.}
#'   \item{usage}{Annual count of code usage.}
#'   \item{description}{Description of the ICD-10 Code}
#' }
#' @source <https://digital.nhs.uk/data-and-information/publications/statistical/hospital-admitted-patient-care-activity>
#' @examples
#' # Filter to codes in the ICD-10 Chapter XIX: "Injury, poisoning..."
#' # (codes begin with letters "S" or "T"), with usage > 10,000.
#' # For each of these, select the year with the highest count.
#' icd10_usage |>
#'   dplyr::filter(grepl("^[ST]", icd10_code) & usage > 10000) |>
#'   dplyr::group_by(description) |>
#'   dplyr::slice_max(usage)
#' # Filter to codes present in the CPRD Aurum ICD-10 pregnancy codelist.
#' # This codelist is available in OpenCodelists.org
#' codelist <- read.csv(
#'   "https://www.opencodelists.org/codelist/opensafely/pregnancy-icd10-aurum/5a7d8d12/download.csv"
#' )
#' icd10_usage |>
#'   dplyr::filter(icd10_code %in% codelist$code)
#' @examples
#' # Filter to codes in the ICD-10 Chapter XIX: "Injury, poisoning..."
#' # (codes begin with letters "S" or "T"), with usage > 10,000.
#' # For each of these, select the year with the highest count.
#' icd10_usage |>
#'   dplyr::filter(grepl("^[ST]", icd10_code) & usage > 10000) |>
#'   dplyr::group_by(description) |>
#'   dplyr::slice_max(usage)
#' # Filter to codes present in the CPRD Aurum ICD-10 pregnancy codelist.
#' # This codelist is available in OpenCodelists.org
#' codelist <- read.csv(
#'   "https://www.opencodelists.org/codelist/opensafely/pregnancy-icd10-aurum/5a7d8d12/download.csv"
#' )
#' icd10_usage |>
#'   dplyr::filter(icd10_code %in% codelist$code)
"icd10_usage"

#' Yearly ICD-10 Code Usage Breakdowns from Hospital Admitted Patient Care Activity in England
#'
#' Yearly summary of 4-character ICD-10 code usage with demographic breakdowns
#' from 1st April 2012 to 31st March 2025.
#' Includes breakdowns by diagnosis type (all/main), sex, and age group.
#' Restricted codes for which annual usage is not published have been removed.
#' @format A data frame with 6 columns:
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
#' @source <https://digital.nhs.uk/data-and-information/publications/statistical/hospital-admitted-patient-care-activity>
#' @examples
#' # Compare male vs female usage for codes containing "pregnancy"
#' icd10_usage_breakdowns |>
#'   dplyr::filter(grepl("pregnancy", description, ignore.case = TRUE)) |>
#'   dplyr::filter(breakdown %in% c("male", "female"))
#'
#' # Get age distribution for a specific code in the most recent year
#' icd10_usage_breakdowns |>
#'   dplyr::filter(icd10_code == "I251" & start_date == "2024-04-01") |>
#'   dplyr::filter(grepl("^age_", breakdown))
"icd10_usage_breakdowns"

#' Yearly OPCS-4 Code Usage from Hospital Admitted Patient Care Activity in England
#'
#' Yearly summary of 4-character OPCS-4 code usage from 1st April 2013 to 31st March 2025.
#' The code usage represents the total annual count of each procedure, recorded across the primary and the secondary procedure positions.
#' Restricted codes for which annual usage is not published have been removed.
#' @format A data frame with 116,680 rows and 5 columns:
#' \describe{
#'   \item{start_date}{Start date of code usage count}
#'   \item{end_date}{End date of code usage count}
#'   \item{opcs4_code}{The 4-character OPCS-4 code.
#'   Note that the punctuation from the code has been removed for compatibility with OpenCodelists.}
#'   \item{usage}{Annual count of code usage.}
#'   \item{description}{Description of the OPCS-4 Code}
#' }
#' @source <https://digital.nhs.uk/data-and-information/publications/statistical/hospital-admitted-patient-care-activity>
#' @examples
#' # Filter to procedures involving "biopsy" after March 2020 (note each year runs April - March).
#' opcs4_usage |>
#'   dplyr::filter(grepl("biopsy", description, ignore.case = TRUE) & lubridate::year(end_date) > 2020)
"opcs4_usage"

#' Yearly OPCS-4 Code Usage Breakdowns from Hospital Admitted Patient Care Activity in England
#'
#' Yearly summary of 4-character OPCS-4 code usage with demographic breakdowns
#' from 1st April 2012 to 31st March 2025.
#' Includes breakdowns by procedure type (all/main), sex, and age group.
#' Restricted codes for which annual usage is not published have been removed.
#' @format A data frame with 6 columns:
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
#' @source <https://digital.nhs.uk/data-and-information/publications/statistical/hospital-admitted-patient-care-activity>
#' @examples
#' # Get sex breakdown for hip replacement procedures
#' opcs4_usage_breakdowns |>
#'   dplyr::filter(grepl("hip replacement", description, ignore.case = TRUE)) |>
#'   dplyr::filter(breakdown %in% c("male", "female"))
#'
#' # Get age distribution for a specific procedure code
#' opcs4_usage_breakdowns |>
#'   dplyr::filter(opcs4_code == "W371" & start_date == "2024-04-01") |>
#'   dplyr::filter(grepl("^age_", breakdown))
"opcs4_usage_breakdowns"
