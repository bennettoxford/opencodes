# This script loads OPCS-4 code usage data with demographic breakdowns
# from files.digital.nhs.uk
library(tidyverse)
library(janitor)
library(here)
library(httr)

# Using xlsx files because csv structure varies across years, xlsx stays consistent
# All data from sheet "All Diagnoses 4 Character"

url_start <- "https://files.digital.nhs.uk/"

# Selects columns by name - will break if column names change spelling/order
icd10_breakdowns_xlsx_urls <- list(
  "fy24to25" = list(
    url = paste0(
      url_start,
      "CC/EA025D/hosp-epis-stat-admi-diag-2024-25-tab.xlsx"
    ),
    sheet = 6,
    range = "A11:AK11359"
  ),
  "fy23to24" = list(
    url = paste0(
      url_start,
      "A5/5B8474/hosp-epis-stat-admi-diag-2023-24-tab.xlsx"
    ),
    sheet = 6,
    range = "A11:AK11359"
  ),
  "fy22to23" = list(
    url = paste0(
      url_start,
      "7A/DB1B00/hosp-epis-stat-admi-diag-2022-23-tab_V2.xlsx"
    ),
    sheet = 6,
    range = "A11:AK11337"
  ),
  "fy21to22" = list(
    url = paste0(
      url_start,
      "0E/E70963/hosp-epis-stat-admi-diag-2021-22-tab.xlsx"
    ),
    sheet = 6,
    range = "A11:AK11341"
  ),
  "fy20to21" = list(
    url = paste0(
      url_start,
      "5B/AD892C/hosp-epis-stat-admi-diag-2020-21-tab.xlsx"
    ),
    sheet = 6,
    range = "A11:AK11218"
  ),
  "fy19to20" = list(
    url = paste0(
      url_start,
      "37/8D9781/hosp-epis-stat-admi-diag-2019-20-tab%20supp.xlsx"
    ),
    sheet = 6,
    range = "A11:AK11390"
  ),
  "fy18to19" = list(
    url = paste0(
      url_start,
      "1C/B2AD9B/hosp-epis-stat-admi-diag-2018-19-tab.xlsx"
    ),
    sheet = 6,
    range = "A11:AK11392"
  ),
  "fy17to18" = list(
    url = paste0(
      url_start,
      "B2/5CEC8D/hosp-epis-stat-admi-diag-2017-18-tab.xlsx"
    ),
    sheet = 6,
    range = "A11:AK11386"
  ),
  "fy16to17" = list(
    url = paste0(
      url_start,
      "publication/7/d/hosp-epis-stat-admi-diag-2016-17-tab.xlsx"
    ),
    sheet = 6,
    range = "A11:AK11418"
  ),
  "fy15to16" = list(
    url = paste0(
      url_start,
      "publicationimport/pub22xxx/pub22378/hosp-epis-stat-admi-diag-2015-16-tab.xlsx"
    ),
    sheet = 6,
    range = "A11:AK11353"
  ),
  "fy14to15" = list(
    url = paste0(
      url_start,
      "publicationimport/pub19xxx/pub19124/hosp-epis-stat-admi-diag-2014-15-tab.xlsx"
    ),
    sheet = 6,
    range = "A11:AK11345"
  ),
  "fy13to14" = list(
    url = paste0(
      url_start,
      "publicationimport/pub16xxx/pub16719/hosp-epis-stat-admi-diag-2013-14-tab.xlsx"
    ),
    sheet = 6,
    range = "A17:AF11357"
  ),
  "fy12to13" = list(
    url = paste0(
      url_start,
      "publicationimport/pub12xxx/pub12566/hosp-epis-stat-admi-diag-2012-13-tab.xlsx"
    ),
    sheet = 6,
    range = "A18:AF11400"
  )
)

# Download xlsx from URL and read with cleaned column names
read_icd10_usage_xlsx_from_url <- function(url_list, ...) {
  temp_file <- tempfile(fileext = ".xlsx")
  GET(
    url_list$url,
    write_disk(temp_file, overwrite = TRUE)
  )
  readxl::read_xlsx(
    temp_file,
    col_names = TRUE,
    .name_repair = janitor::make_clean_names,
    sheet = url_list$sheet,
    range = url_list$range,
    ...
  )
}

# Download and read all xlsx files
icd10_usage_raw_list <- icd10_breakdowns_xlsx_urls |>
  map(read_icd10_usage_xlsx_from_url)

# Check raw column names before cleaning
icd10_usage_raw_list |>
  map(names)

# Select and standardise columns for all diagnoses breakdowns
select_all_diag_breakdowns <- function(data) {
  dplyr::select(
    data,
    icd10_code = 1,
    description = 2,
    c("all_diagnoses", "main_diagnosis"),
    c("male", "female", "gender_unknown"),
    starts_with("age")
  ) |>
    remove_empty("rows") |>
    rename(age_90plus = age_90)
}

# Convert to numeric - suppressed counts ("-") become NA
set_col_types <- function(data) {
  suppressWarnings(
    data |>
      mutate(
        across(c(icd10_code, description), as.character),
        across(!c(icd10_code, description), as.numeric)
      )
  )
}

# Verify all years have identical column names after cleaning
icd10_usage_raw_list |>
  map(select_all_diag_breakdowns) |>
  map(names) |>
  unique()

# Combine all years and parse fiscal year dates
icd10_usage_breakdowns_long <- icd10_usage_raw_list |>
  map(select_all_diag_breakdowns) |>
  map(set_col_types) |>
  bind_rows(.id = "nhs_fy") |>
  separate(nhs_fy, c("start_date", "end_date"), "to") |>
  mutate(
    start_date = as.Date(
      paste0("20", str_extract_all(start_date, "\\d+"), "-04-01")
    ),
    end_date = as.Date(
      paste0("20", str_extract_all(end_date, "\\d+"), "-03-31")
    ),
    icd10_code = gsub("\\s?[^[:alnum:]]+\\s?", "", icd10_code)
  )

# Pivot breakdowns to long format
icd10_usage_breakdowns <- icd10_usage_breakdowns_long |>
  pivot_longer(
    cols = all_diagnoses:age_90plus,
    names_to = "breakdown",
    values_to = "usage"
  ) |>
  dplyr::mutate(
    usage = as.integer(usage)
  )

# Check codes with missing description
icd10_usage_breakdowns |>
  filter(is.na(description)) |>
  select(icd10_code, description, usage) |>
  distinct()

# Remove "codes" with missing description
icd10_usage_breakdowns <- icd10_usage_breakdowns |>
  filter(!is.na(description))

# Fix encoding problems
icd10_usage_breakdowns <- icd10_usage_breakdowns |>
  mutate(description = opencodecounts:::fix_encoding(description))

# Check encoding problems after fix
opencodecounts:::get_codes_with_encoding_problems(
  icd10_usage_breakdowns,
  icd10_code
)
# character(0)

usethis::use_data(
  icd10_usage_breakdowns,
  compress = "bzip2",
  overwrite = TRUE
)
