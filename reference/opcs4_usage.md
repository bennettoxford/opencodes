# Yearly OPCS-4 Code Usage from Hospital Admitted Patient Care Activity in England

Yearly summary of 4-character OPCS-4 code usage from 1st April 2013 to
31st March 2025. The code usage represents the total annual count of
each procedure, recorded across the primary and the secondary procedure
positions. Restricted codes for which annual usage is not published have
been removed.

## Usage

``` r
opcs4_usage
```

## Format

A data frame with 116,680 rows and 5 columns:

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

## Source

<https://digital.nhs.uk/data-and-information/publications/statistical/hospital-admitted-patient-care-activity>

## Examples

``` r
# Filter to procedures involving "biopsy" after March 2020 (note each year runs April - March).
opcs4_usage |>
  dplyr::filter(grepl("biopsy", description, ignore.case = TRUE) & lubridate::year(end_date) > 2020)
#> # A tibble: 1,288 × 5
#>    start_date end_date   opcs4_code description                            usage
#>    <date>     <date>     <chr>      <chr>                                  <int>
#>  1 2024-04-01 2025-03-31 A041       Open biopsy of lesion of tissue of fr…   228
#>  2 2024-04-01 2025-03-31 A042       Open biopsy of lesion of tissue of te…   103
#>  3 2024-04-01 2025-03-31 A043       Open biopsy of lesion of tissue of pa…   108
#>  4 2024-04-01 2025-03-31 A044       Open biopsy of lesion of tissue of oc…    15
#>  5 2024-04-01 2025-03-31 A045       Open biopsy of lesion of tissue of ce…    30
#>  6 2024-04-01 2025-03-31 A046       Open biopsy of lesion of tissue of br…    21
#>  7 2024-04-01 2025-03-31 A048       Other specified open biopsy of lesion…    41
#>  8 2024-04-01 2025-03-31 A049       Unspecified open biopsy of lesion of …    10
#>  9 2024-04-01 2025-03-31 A081       Biopsy of lesion of tissue of frontal…   330
#> 10 2024-04-01 2025-03-31 A082       Biopsy of lesion of tissue of tempora…   166
#> # ℹ 1,278 more rows
```
