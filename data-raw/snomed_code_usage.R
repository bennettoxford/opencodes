# This script loads all available code usage data from files.digital.nhs.uk
# and combines the files into two files:
# (1) snomed_code_usage: yearly summary of code usage (no code description)
# (2) snomed_code_dict: code descriptions for each year (as there are sometimes small differences across years)

library(tidyverse)
library(janitor)
library(here)

url_start <- "https://files.digital.nhs.uk/"

snomed_code_usage_urls <- list(
  "2023to2024" = paste0(url_start, "B8/7D8335/SNOMED_code_usage_2023-24.txt"),
  "2022to2023" = paste0(url_start, "09/E1218D/SNOMED_code_usage_2022-23.txt"),
  "2021to2022" = paste0(url_start, "71/6C02F5/SNOMED_code_usage_2021-22.txt"),
  "2020to2021" = paste0(url_start, "8A/09BBE6/SNOMED_code_usage_2020-21.txt"),
  "2019to2020" = paste0(url_start, "8F/882EB3/SNOMED_code_usage_2019-20.txt"),
  "2018to2019" = paste0(url_start, "13/F2956B/SNOMED_code_usage_2018-19.txt"),
  "2017to2018" = paste0(url_start, "9F/024949/SNOMED_code_usage_2017-18.txt"),
  "2016to2017" = paste0(url_start, "E2/79561E/SNOMED_code_usage_2016-17.txt"),
  "2015to2016" = paste0(url_start, "8B/15EAA1/SNOMED_code_usage_2015-16.txt"),
  "2014to2015" = paste0(url_start, "BB/47E566/SNOMED_code_usage_2014-15.txt"),
  "2013to2014" = paste0(url_start, "82/40F702/SNOMED_code_usage_2013-14.txt"),
  "2012to2013" = paste0(url_start, "69/866A44/SNOMED_code_usage_2012-13.txt"),
  "2011to2012" = paste0(url_start, "53/C8F877/SNOMED_code_usage_2011-12.txt")
)

# Data dictionary from SNOMED_code_usage_metadata.xlsx
# https://files.digital.nhs.uk/31/097702/SNOMED_code_usage_metadata.xlsx

# * snomed_code (Text string of digits up to 18 characters long)
#   SNOMED concepts which have been added to a patient record in a general practice system during the reporting period.
# * Description (Text string)
#   The fully specified name associated with the snomed_code on the final day of the reporting period (31 July).
# * Usage (Numeric (integer) or *)
#   The number of times that the snomed_code was added into any patient record within the reporting period, rounded to the nearerst 10.
#   Usage of 1 to 4 is displayed as *.
# * Active_at_Start
#   Active status of the snomed_code on the first day of the reporting period.
#   This is taken from the most recent UK clinical extension, or associated International extention, which was published up to the start of the reporting year (1 August).
#   1 = SNOMED concept was published and was active (active = 1).
#   0 = SNOMED concept was either not yet available or was inactive (active = 0).
# * Active_at_End	"Active status of the snomed_code on the first day of the reporting period.
#   This is taken from the most recent UK clinical extension, or associated International extention, which was published up to the end of the reporting year (31 July).
#   1 = SNOMED concept was published and was active (active = 1).
#   0 = SNOMED concept was either not yet available or was inactive (active = 0).

# The following files show the number of times each listed SNOMED code was added to a GP patient record within the period 1 Aug to 31 July for the years available, aggregated at England level.

# Disanble scientific notation
options(scipen = 999)

snomed_usage <- snomed_code_usage_urls %>%
  map(read_tsv,
    col_types = list(
      snomed_code = "c",
      Description = "c",
      Usage = "i",
      Active_at_Start = "l",
      Active_at_End = "l"
    )
  ) %>%
  bind_rows(.id = "nhs_fy") |>
  clean_names() |>
  separate(nhs_fy, c("start_date", "end_date"), "to") |>
  mutate(
    start_date = as.Date(paste0(start_date, "-08-01")),
    end_date = as.Date(paste0(end_date, "-07-31")),
    usage = replace_na(usage, 5)
  )

# Manipulation required due to variable name change
snomed_usage <- snomed_usage |>
  rename(snomed_code = snomed_concept_id) |>
  mutate(snomed_code = as.character(snomed_code))

usethis::use_data(
  snomed_usage,
  compress = "bzip2",
  overwrite = TRUE
)
