# Yearly SNOMED CT Code Usage in Primary Care in England

Yearly summary of SNOMED CT code usage from 1st August 2011 to 31st July
2023. The variables in this dataset include:

## Usage

``` r
snomed_usage
```

## Format

A data frame with 1,366,513 rows and 6 columns:

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

## Source

<https://digital.nhs.uk/data-and-information/publications/statistical/mi-snomed-code-usage-in-primary-care>

## Examples

``` r
# Filter for code usage records from 2022-08-01 onwards
snomed_usage |>
  dplyr::filter(start_date >= "2022-08-1")
#> # A tibble: 306,888 × 7
#>    start_date end_date   snomed_code      description      usage active_at_start
#>    <date>     <date>     <chr>            <chr>            <int> <lgl>          
#>  1 2023-08-01 2024-07-31 279991000000102  Short message … 4.41e8 TRUE           
#>  2 2023-08-01 2024-07-31 184103008        Patient teleph… 1.91e8 TRUE           
#>  3 2023-08-01 2024-07-31 428481002        Patient mobile… 1.16e8 TRUE           
#>  4 2023-08-01 2024-07-31 423876004        Clinical docum… 7.81e7 TRUE           
#>  5 2023-08-01 2024-07-31 72313002         Systolic arter… 6.87e7 TRUE           
#>  6 2023-08-01 2024-07-31 1091811000000102 Diastolic arte… 6.87e7 TRUE           
#>  7 2023-08-01 2024-07-31 1000731000000107 Serum creatini… 4.82e7 TRUE           
#>  8 2023-08-01 2024-07-31 60621009         Body mass inde… 4.65e7 TRUE           
#>  9 2023-08-01 2024-07-31 1000661000000107 Serum sodium l… 4.63e7 TRUE           
#> 10 2023-08-01 2024-07-31 1000651000000109 Serum potassiu… 4.62e7 TRUE           
#> # ℹ 306,878 more rows
#> # ℹ 1 more variable: active_at_end <lgl>

# Filter for code usage records from 2022-08-01 onwards
# where the description contains the word "anxiety"
snomed_usage |>
  dplyr::filter(start_date >= "2022-08-1") |>
  dplyr::filter(grepl("anxiety", description, ignore.case = TRUE))
#> # A tibble: 326 × 7
#>    start_date end_date   snomed_code     description       usage active_at_start
#>    <date>     <date>     <chr>           <chr>             <int> <lgl>          
#>  1 2023-08-01 2024-07-31 231504006       Mixed anxiety a… 2.38e6 TRUE           
#>  2 2023-08-01 2024-07-31 197480006       Anxiety disorde… 1.22e6 TRUE           
#>  3 2023-08-01 2024-07-31 445455005       Generalized Anx… 1.01e6 TRUE           
#>  4 2023-08-01 2024-07-31 198288003       Anxiety state (… 5.23e5 TRUE           
#>  5 2023-08-01 2024-07-31 836571000000106 Generalised anx… 5.02e5 TRUE           
#>  6 2023-08-01 2024-07-31 48694002        Anxiety (findin… 5.00e5 TRUE           
#>  7 2023-08-01 2024-07-31 21897009        Generalized anx… 1.46e5 TRUE           
#>  8 2023-08-01 2024-07-31 908501000000101 Anxiety screeni… 8.86e4 TRUE           
#>  9 2023-08-01 2024-07-31 286644009       Level of anxiet… 6.72e4 TRUE           
#> 10 2023-08-01 2024-07-31 401320004       Hospital Anxiet… 4.42e4 TRUE           
#> # ℹ 316 more rows
#> # ℹ 1 more variable: active_at_end <lgl>
```
