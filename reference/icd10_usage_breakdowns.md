# Yearly ICD-10 Code Usage Breakdowns from Hospital Admitted Patient Care Activity in England

Yearly summary of 4-character ICD-10 code usage with demographic
breakdowns from 1st April 2012 to 31st March 2025. Includes breakdowns
by diagnosis type (all/main), sex, and age group. Restricted codes for
which annual usage is not published have been removed.

## Usage

``` r
icd10_usage_breakdowns
```

## Format

A data frame with 6 columns:

- start_date:

  Start date of code usage count

- end_date:

  End date of code usage count

- icd10_code:

  The 4-character ICD-10 Code. Note that the punctuation from the code
  has been removed for compatibility with OpenCodelists.

- description:

  Description of the ICD-10 Code

- breakdown:

  Type of breakdown: all_diagnoses, main_diagnosis, male, female,
  gender_unknown, or age groups (age_0, age_1_4, age_5_9, ...,
  age_85_89, age_90plus)

- usage:

  Annual count of code usage. NA where suppressed due to small numbers.

## Source

<https://digital.nhs.uk/data-and-information/publications/statistical/hospital-admitted-patient-care-activity>

## Examples

``` r
# Compare male vs female usage for codes containing "pregnancy"
icd10_usage_breakdowns |>
  dplyr::filter(grepl("pregnancy", description, ignore.case = TRUE)) |>
  dplyr::filter(breakdown %in% c("male", "female"))
#> # A tibble: 2,920 × 6
#>    start_date end_date   icd10_code description                  breakdown usage
#>    <date>     <date>     <chr>      <chr>                        <chr>     <int>
#>  1 2024-04-01 2025-03-31 O000       Abdominal pregnancy          male          0
#>  2 2024-04-01 2025-03-31 O000       Abdominal pregnancy          female       70
#>  3 2024-04-01 2025-03-31 O001       Tubal pregnancy              male          1
#>  4 2024-04-01 2025-03-31 O001       Tubal pregnancy              female     9237
#>  5 2024-04-01 2025-03-31 O002       Ovarian pregnancy            male          0
#>  6 2024-04-01 2025-03-31 O002       Ovarian pregnancy            female      302
#>  7 2024-04-01 2025-03-31 O008       Other ectopic pregnancy      male          3
#>  8 2024-04-01 2025-03-31 O008       Other ectopic pregnancy      female      979
#>  9 2024-04-01 2025-03-31 O009       Ectopic pregnancy, unspecif… male          0
#> 10 2024-04-01 2025-03-31 O009       Ectopic pregnancy, unspecif… female     2339
#> # ℹ 2,910 more rows

# Get age distribution for a specific code in the most recent year
icd10_usage_breakdowns |>
  dplyr::filter(icd10_code == "I251" & start_date == "2024-04-01") |>
  dplyr::filter(grepl("^age_", breakdown))
#> # A tibble: 24 × 6
#>    start_date end_date   icd10_code description                  breakdown usage
#>    <date>     <date>     <chr>      <chr>                        <chr>     <int>
#>  1 2024-04-01 2025-03-31 I251       Atherosclerotic heart disea… age_0        10
#>  2 2024-04-01 2025-03-31 I251       Atherosclerotic heart disea… age_1_4      16
#>  3 2024-04-01 2025-03-31 I251       Atherosclerotic heart disea… age_5_9      12
#>  4 2024-04-01 2025-03-31 I251       Atherosclerotic heart disea… age_10_14    12
#>  5 2024-04-01 2025-03-31 I251       Atherosclerotic heart disea… age_15        1
#>  6 2024-04-01 2025-03-31 I251       Atherosclerotic heart disea… age_16        5
#>  7 2024-04-01 2025-03-31 I251       Atherosclerotic heart disea… age_17        6
#>  8 2024-04-01 2025-03-31 I251       Atherosclerotic heart disea… age_18       23
#>  9 2024-04-01 2025-03-31 I251       Atherosclerotic heart disea… age_19       14
#> 10 2024-04-01 2025-03-31 I251       Atherosclerotic heart disea… age_20_24   211
#> # ℹ 14 more rows
```
