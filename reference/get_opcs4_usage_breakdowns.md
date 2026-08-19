# Get yearly OPCS-4 code usage breakdowns from Hospital Admitted Patient Care Activity in England

Downloads (if not already cached) and returns the yearly summary of
4-character OPCS-4 code usage with demographic breakdowns from 1st April
2012 onwards. Includes breakdowns by procedure type (all/main), sex, and
age group. Restricted codes for which annual usage is not published have
been removed.

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

## Usage

``` r
get_opcs4_usage_breakdowns(version = NULL)
```

## Source

<https://digital.nhs.uk/data-and-information/publications/statistical/hospital-admitted-patient-care-activity>

## Arguments

- version:

  String, dataset version, or `NULL` for the latest (default)

## Value

A tibble

## Examples

``` r
if (FALSE) { # \dontrun{
# Get sex breakdown for hip replacement procedures
get_opcs4_usage_breakdowns() |>
  dplyr::filter(grepl("hip replacement", description, ignore.case = TRUE)) |>
  dplyr::filter(breakdown %in% c("male", "female"))
} # }
```
