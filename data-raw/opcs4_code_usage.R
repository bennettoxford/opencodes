library(tidyverse)
library(janitor)
library(here)
library(httr)

# Source urls per year, defined in inst/config/raw_opcs4.yml (shared with
# opcs4_usage_breakdowns.R, which reads the same files)
opcs4_code_usage_urls <- opencodecounts:::get_raw_source_periods("opcs4_usage")

# Function to download and read the xlsx files
read_opcs4_usage_xlsx_from_url <- function(url_list, ...) {
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
    opcs4_code = 1,
    description = 2,
    usage = url_list$usage_col
  ) |>
    dplyr::mutate(
      usage = as.integer(usage)
    )
}

# Combine both functions
get_opcs4_data <- function(url_list, ...) {
  df_temp <- read_opcs4_usage_xlsx_from_url(url_list, ...)
  select_all_diag_counts(df_temp, url_list)
}

opcs4_usage <- opcs4_code_usage_urls |>
  map(get_opcs4_data) |>
  bind_rows(.id = "nhs_fy") |>
  opencodecounts:::add_period_dates("nhs_fy", "04-01", "03-31") |>
  mutate(
    opcs4_code = gsub("\\s?[^[:alnum:]]+\\s?", "", opcs4_code)
  )

# Count number of usage with NAs
sum(is.na(opcs4_usage$usage))
# [1] 151

# Replace NAs with 5
opcs4_usage <- opcs4_usage |>
  mutate(usage = replace_na(usage, 5))

# Check number of usage with NAs is 0
sum(is.na(opcs4_usage$usage)) == 0

# Check codes with missing description
opcs4_usage |>
  filter(is.na(description)) |>
  select(opcs4_code, description, usage) |>
  distinct() |>
  print(n = 32)
# A tibble: 32 × 3

# Remove "codes" with missing description
opcs4_usage <- opcs4_usage |>
  filter(!is.na(description))

# Check encoding problems before fix
codes_with_encoding_problems <- opencodecounts:::get_codes_with_encoding_problems(
  opcs4_usage,
  opcs4_code
)
# character(0)

# Check (but dont fix) codes with multiple descriptions
codes_with_multiple_desc <- opencodecounts:::get_codes_with_multiple_desc(
  opcs4_usage,
  opcs4_code
)
length(codes_with_multiple_desc)
# [1] 99

arrow::write_parquet(opcs4_usage, here("data-raw", "opcs4_usage.parquet"))
