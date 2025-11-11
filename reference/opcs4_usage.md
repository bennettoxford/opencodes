# Yearly OPCS-4 Code Usage from Hospital Admitted Patient Care Activity in England

Yearly summary of 4-character OPCS-4 code usage from 1st April 2013 to
31st March 2024. The code usage represents the total annual count of
each procedure, recorded across the primary and the secondary procedure
positions. Restricted codes for which annual usage is not published have
been removed.

## Usage

``` r
opcs4_usage
```

## Format

A data frame with 107,376 rows and 5 columns:

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
#> # A tibble: 1,029 × 5
#>    start_date end_date   opcs4_code description                            usage
#>    <date>     <date>     <chr>      <chr>                                  <int>
#>  1 2023-04-01 2024-03-31 A041       Open biopsy of lesion of tissue of fr…   210
#>  2 2023-04-01 2024-03-31 A042       Open biopsy of lesion of tissue of te…   121
#>  3 2023-04-01 2024-03-31 A043       Open biopsy of lesion of tissue of pa…   101
#>  4 2023-04-01 2024-03-31 A044       Open biopsy of lesion of tissue of oc…    24
#>  5 2023-04-01 2024-03-31 A045       Open biopsy of lesion of tissue of ce…    35
#>  6 2023-04-01 2024-03-31 A046       Open biopsy of lesion of tissue of br…    21
#>  7 2023-04-01 2024-03-31 A048       Other specified open biopsy of lesion…    75
#>  8 2023-04-01 2024-03-31 A049       Unspecified open biopsy of lesion of …     6
#>  9 2023-04-01 2024-03-31 A081       Biopsy of lesion of tissue of frontal…   294
#> 10 2023-04-01 2024-03-31 A082       Biopsy of lesion of tissue of tempora…   135
#> # ℹ 1,019 more rows
```
