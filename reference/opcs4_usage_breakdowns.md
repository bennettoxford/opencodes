# Yearly OPCS-4 Code Usage Breakdowns from Hospital Admitted Patient Care Activity in England

Yearly summary of 4-character OPCS-4 code usage with demographic
breakdowns from 1st April 2012 to 31st March 2025. Includes breakdowns
by procedure type (all/main), sex, and age group. Restricted codes for
which annual usage is not published have been removed.

## Usage

``` r
opcs4_usage_breakdowns
```

## Format

A data frame with 6 columns:

- start_date:

  Start date of code usage count

- end_date:

  End date of code usage count

- opcs4_code:

  The 4-character OPCS-4 code. Note that the punctuation from the code
  has been removed for compatibility with OpenCodelists.

- description:

  Description of the OPCS-4 Code

- breakdown:

  Type of breakdown: all_procedures, main_procedure, male, female,
  gender_unknown, or age groups (age_0, age_1_4, age_5_9, ...,
  age_85_89, age_90plus)

- usage:

  Annual count of code usage. NA where suppressed due to small numbers.

## Source

<https://digital.nhs.uk/data-and-information/publications/statistical/hospital-admitted-patient-care-activity>

## Examples

``` r
# Get sex breakdown for hip replacement procedures
opcs4_usage_breakdowns |>
  dplyr::filter(grepl("hip replacement", description, ignore.case = TRUE)) |>
  dplyr::filter(breakdown %in% c("male", "female"))
#> # A tibble: 0 × 6
#> # ℹ 6 variables: start_date <date>, end_date <date>, opcs4_code <chr>,
#> #   description <chr>, breakdown <chr>, usage <int>

# Get age distribution for a specific procedure code
opcs4_usage_breakdowns |>
  dplyr::filter(opcs4_code == "W371" & start_date == "2024-04-01") |>
  dplyr::filter(grepl("^age_", breakdown))
#> # A tibble: 24 × 6
#>    start_date end_date   opcs4_code description                  breakdown usage
#>    <date>     <date>     <chr>      <chr>                        <chr>     <int>
#>  1 2024-04-01 2025-03-31 W371       Primary total prosthetic re… age_0         0
#>  2 2024-04-01 2025-03-31 W371       Primary total prosthetic re… age_1_4       0
#>  3 2024-04-01 2025-03-31 W371       Primary total prosthetic re… age_5_9       0
#>  4 2024-04-01 2025-03-31 W371       Primary total prosthetic re… age_10_14     1
#>  5 2024-04-01 2025-03-31 W371       Primary total prosthetic re… age_15        1
#>  6 2024-04-01 2025-03-31 W371       Primary total prosthetic re… age_16        1
#>  7 2024-04-01 2025-03-31 W371       Primary total prosthetic re… age_17        3
#>  8 2024-04-01 2025-03-31 W371       Primary total prosthetic re… age_18        0
#>  9 2024-04-01 2025-03-31 W371       Primary total prosthetic re… age_19        1
#> 10 2024-04-01 2025-03-31 W371       Primary total prosthetic re… age_20_24     6
#> # ℹ 14 more rows
```
