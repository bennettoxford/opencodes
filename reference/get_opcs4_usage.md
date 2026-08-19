# Get yearly OPCS-4 code usage from Hospital Admitted Patient Care Activity in England

Downloads (if not already cached) and returns the yearly summary of
4-character OPCS-4 code usage from 1st April 2012 onwards. The code
usage represents the total annual count of each procedure, recorded
across the primary and the secondary procedure positions. Restricted
codes for which annual usage is not published have been removed.

- start_date:

  Start date of code usage count

- end_date:

  End date of code usage count

- opcs4_code:

  The 4-character OPCS-4 code. Note that the punctuation from the code
  has been removed for compatibility with OpenCodelists.

- usage:

  Annual count of code usage.

- description:

  Description of the OPCS-4 Code

## Usage

``` r
get_opcs4_usage(version = NULL)
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
# Filter to procedures involving "biopsy" after March 2020 (note each year runs April - March).
get_opcs4_usage() |>
  dplyr::filter(grepl("biopsy", description, ignore.case = TRUE) & lubridate::year(end_date) > 2020)
} # }
```
