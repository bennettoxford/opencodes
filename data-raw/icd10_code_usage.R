# This script loads all available code usage data from files.digital.nhs.uk
library(tidyverse)
library(janitor)
library(here)
library(httr)

# Source urls per year, defined in inst/config/raw_icd10.yml (shared with
# icd10_usage_breakdowns.R, which reads the same files)
icd10_code_usage_urls <- opencodecounts:::get_raw_source_periods("icd10_usage")

# Function to download and read the xlsx files
read_icd10_usage_xlsx_from_url <- function(url_list, ...) {
  temp_file <- tempfile(fileext = ".xlsx")
  GET(
    url_list$url,
    write_disk(temp_file, overwrite = TRUE)
  )
  readxl::read_xlsx(
    temp_file,
    col_names = FALSE,
    .name_repair = janitor::make_clean_names,
    sheet = url_list$sheet,
    skip = url_list$skip,
    ...
  )
}

# Function to select the correct columns
select_all_diag_counts <- function(data, url_list) {
  dplyr::select(
    data,
    icd10_code = 1,
    description = 2,
    usage = url_list$usage_col
  ) |>
    dplyr::mutate(
      usage = as.integer(usage)
    )
}

# Combine both functions
get_icd10_data <- function(url_list, ...) {
  df_temp <- read_icd10_usage_xlsx_from_url(url_list, ...)
  select_all_diag_counts(df_temp, url_list)
}

icd10_usage <- icd10_code_usage_urls |>
  map(get_icd10_data) |>
  bind_rows(.id = "nhs_fy") |>
  opencodecounts:::add_period_dates("nhs_fy", "04-01", "03-31") |>
  mutate(
    icd10_code = gsub("\\s?[^[:alnum:]]+\\s?", "", icd10_code)
  )

# Count number of usage with NAs
sum(is.na(icd10_usage$usage))
# [1] 341

# Replace NAs with 5
icd10_usage <- icd10_usage |>
  mutate(usage = replace_na(usage, 5))

# Check number of usage with NAs is 0
sum(is.na(icd10_usage$usage)) == 0

# Check codes with missing description
icd10_usage |>
  filter(is.na(description)) |>
  select(icd10_code, description, usage) |>
  distinct() |>
  print(n = 39)
# A tibble: 38 × 3

# Remove "codes" with missing description
icd10_usage <- icd10_usage |>
  filter(!is.na(description))

# Check encoding problems before fix
codes_with_encoding_problems <- opencodecounts:::get_codes_with_encoding_problems(
  icd10_usage,
  icd10_code
)
# [1] "C841" "C880" "D510" "D511" "D513" "D518" "D519" "E672" "E750" "G375" "G610" "H810" "L705"
# [14] "L813" "M350" "M352" "M911" "M931" "T470" "Y441" "Y530"

# Fix encoding problems
icd10_usage <- icd10_usage |>
  mutate(description = opencodecounts:::fix_encoding(description))

# Check encoding problems after fix
opencodecounts:::get_codes_with_encoding_problems(icd10_usage, icd10_code)
# character(0)

# Check (but dont fix) codes with multiple descriptions
codes_with_multiple_desc <- opencodecounts:::get_codes_with_multiple_desc(
  icd10_usage,
  icd10_code
)
length(codes_with_multiple_desc)
# [1] 214

arrow::write_parquet(icd10_usage, here("data-raw", "icd10_usage.parquet"))
