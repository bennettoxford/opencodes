# This script loads OPCS-4 code usage data with demographic breakdowns
# from files.digital.nhs.uk
library(tidyverse)
library(janitor)
library(here)
library(httr)

# Using xlsx files because csv structure varies across years, xlsx stays consistent
# All data from sheet "All Diagnoses 4 Character"

# Source urls per year, defined in inst/config/raw_opcs4.yml (shared with
# opcs4_code_usage.R, which reads the same files)
opcs4_breakdowns_xlsx_urls <- opencodecounts:::get_raw_source_periods(
  "opcs4_usage_breakdowns"
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
  opencodecounts:::add_period_dates("nhs_fy", "04-01", "03-31") |>
  mutate(
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

arrow::write_parquet(
  opcs4_usage_breakdowns,
  here("data-raw", "opcs4_usage_breakdowns.parquet")
)
