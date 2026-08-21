# Get yearly SNOMED CT code usage in GP electronic health records in England

Downloads (if not already cached) and returns the yearly summary of
SNOMED CT code usage from 1st August 2011 onwards. The variables in this
dataset include:

- start_date:

  Start date of code usage count

- end_date:

  End date of code usage count

- snomed_code:

  SNOMED Concept ID

- usage:

  Yearly summary of code usage. Note that counts are rounded to the
  nearest 10. Counts of 5 or below are displayed as 5.

- active_at_start:

  Specifying whether code was active at the start date.

- active_at_end:

  Specifying whether code was active at the end date.

- description:

  Description of SNOMED Concept ID

## Usage

``` r
get_gp_snomed(version = NULL)
```

## Source

<https://digital.nhs.uk/data-and-information/publications/statistical/mi-snomed-code-usage-in-primary-care>

## Arguments

- version:

  String, dataset version, or `NULL` for the latest (default)

## Value

A tibble

## Examples

``` r
if (FALSE) { # \dontrun{
# Filter for code usage records from 2022-08-01 onwards
get_gp_snomed() |>
  dplyr::filter(start_date >= "2022-08-1")
} # }
```
