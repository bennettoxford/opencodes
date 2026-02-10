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
opcs4_breakdowns_xlsx_urls <- list(
  "fy24to25" = list(
    url = paste0(
      url_start,
      "6D/C40538/hosp-epis-stat-admi-proc-2024-25-tab.xlsx"
    ),
    sheet = 6,
    range = "A10:AK9312"
  ),
  "fy23to24" = list(
    url = paste0(
      url_start,
      "92/DB66C9/hosp-epis-stat-admi-proc-2023-24-tab-v2.xlsx"
    ),
    sheet = 6,
    range = "A10:AK9270"
  ),
  "fy22to23" = list(
    url = paste0(
      url_start,
      "CB/515826/hosp-epis-stat-admi-proc-2022-23-tab-V2.xlsx"
    ),
    sheet = 6,
    range = "A10:AK9034"
  ),
  "fy21to22" = list(
    url = paste0(
      url_start,
      "FA/DA0567/hosp-epis-stat-admi-proc-2021-22-tab.xlsx"
    ),
    sheet = 6,
    range = "A10:AK9065"
  ),
  "fy20to21" = list(
    url = paste0(
      url_start,
      "A6/43CDC1/hosp-epis-stat-admi-proc-2020-21-tab.xlsx"
    ),
    sheet = 6,
    range = "A10:AK8935"
  ),
  "fy19to20" = list(
    url = paste0(
      url_start,
      "20/0864E6/hosp-epis-stat-admi-proc-2019-20-tab.xlsx"
    ),
    sheet = 6,
    range = "A10:AK8875"
  ),
  "fy18to19" = list(
    url = paste0(
      url_start,
      "77/0C8B3F/hosp-epis-stat-admi-proc-2018-19-tab.xlsx"
    ),
    sheet = 6,
    range = "A10:AK8932"
  ),
  "fy17to18" = list(
    url = paste0(
      url_start,
      "B6/E239FA/hosp-epis-stat-admi-proc-2017-18-tab.xlsx"
    ),
    sheet = 6,
    range = "A10:AK8940"
  ),
  "fy16to17" = list(
    url = paste0(
      url_start,
      "publication/7/g/hosp-epis-stat-admi-proc-2016-17-tab.xlsx"
    ),
    sheet = 6,
    range = "A10:AK8909"
  ),
  "fy15to16" = list(
    url = paste0(
      url_start,
      "publicationimport/pub22xxx/pub22378/hosp-epis-stat-admi-proc-2015-16-tab.xlsx"
    ),
    sheet = 6,
    range = "A10:AK8911"
  ),
  "fy14to15" = list(
    url = paste0(
      url_start,
      "publicationimport/pub19xxx/pub19124/hosp-epis-stat-admi-proc-2014-15-tab.xlsx"
    ),
    sheet = 6,
    range = "A10:AK8963"
  ),
  "fy13to14" = list(
    url = paste0(
      url_start,
      "publicationimport/pub16xxx/pub16719/hosp-epis-stat-admi-proc-2013-14-tab.xlsx"
    ),
    sheet = 6,
    range = "A17:AF8841"
  ),
  "fy12to13" = list(
    url = paste0(
      url_start,
      "publicationimport/pub12xxx/pub12566/hosp-epis-stat-admi-proc-2012-13-tab.xlsx"
    ),
    sheet = 6,
    range = "A18:AF8851"
  )
)

# Download xlsx from URL and read with cleaned column names
read_opcs4_usage_xlsx_from_url <- function(url_list, ...) {
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
opcs4_usage_raw_list <- opcs4_breakdowns_xlsx_urls |>
  map(read_opcs4_usage_xlsx_from_url)

# Check raw column names before cleaning
opcs4_usage_raw_list |>
  map(names)

# Select and standardise columns for all diagnoses breakdowns
select_all_diag_breakdowns <- function(data) {
  dplyr::select(
    data,
    opcs4_code = 1,
    description = 2,
    c("all_procedures", "main_procedure"),
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
        across(c(opcs4_code, description), as.character),
        across(!c(opcs4_code, description), as.numeric)
      )
  )
}

# Verify all years have identical column names after cleaning
opcs4_usage_raw_list |>
  map(select_all_diag_breakdowns) |>
  map(names) |>
  unique()

# Combine all years and parse fiscal year dates
opcs4_usage_breakdowns_long <- opcs4_usage_raw_list |>
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
    opcs4_code = gsub("\\s?[^[:alnum:]]+\\s?", "", opcs4_code)
  )

# Pivot breakdowns to long format
opcs4_usage_breakdowns <- opcs4_usage_breakdowns_long |>
  pivot_longer(
    cols = all_procedures:age_90plus,
    names_to = "breakdown",
    values_to = "usage"
  ) |>
  dplyr::mutate(
    usage = as.integer(usage)
  )

# Check codes with missing description
opcs4_usage_breakdowns |>
  filter(is.na(description)) |>
  select(opcs4_code, description, usage) |>
  distinct()

usethis::use_data(
  opcs4_usage_breakdowns,
  compress = "bzip2",
  overwrite = TRUE
)
