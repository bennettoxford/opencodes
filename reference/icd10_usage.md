# Yearly ICD-10 Code Usage from Hospital Admitted Patient Care Activity in England

Yearly summary of 4-character ICD-10 code usage from 1st April 2013 to
31st March 2024. The code usage represents the annual count of all
episodes which record the given ICD-10 code in any primary or secondary
position. Restricted codes for which annual usage is not published have
been removed. Yearly summary of 4-character ICD-10 code usage from 1st
April 2013 to 31st March 2024. The code usage represents the annual
count of all episodes which record the given ICD-10 code in any primary
or secondary position. Restricted codes for which annual usage is not
published have been removed.

## Usage

``` r
icd10_usage
```

## Format

A data frame with 135,951 rows and 5 columns:

- start_date:

  Start date of code usage count

- end_date:

  End date of code usage count

- icd10_code:

  The 4-character ICD-10 Code. Note that the punctuation from the code
  has been removed for compatibility with OpenCodelists.

- usage:

  Annual count of code usage.

- description:

  Description of the ICD-10 Code

## Source

<https://digital.nhs.uk/data-and-information/publications/statistical/hospital-admitted-patient-care-activity>

## Examples

``` r
# Filter to codes in the ICD-10 Chapter XIX: "Injury, poisoning..."
# (codes begin with letters "S" or "T"), with usage > 10,000.
# For each of these, select the year with the highest count.
icd10_usage |>
  dplyr::filter(grepl("^[ST]", icd10_code) & usage > 10000) |>
  dplyr::group_by(description) |>
  dplyr::slice_max(usage)
#> # A tibble: 82 × 5
#> # Groups:   description [82]
#>    start_date end_date   icd10_code description                            usage
#>    <date>     <date>     <chr>      <chr>                                  <int>
#>  1 2023-04-01 2024-03-31 T812       Accidental puncture and laceration du… 23982
#>  2 2023-04-01 2024-03-31 T784       Allergy, unspecified                   10214
#>  3 2018-04-01 2019-03-31 T860       Bone-marrow transplant rejection       13927
#>  4 2023-04-01 2024-03-31 T310       Burns involving less than 10% of body… 14410
#>  5 2021-04-01 2022-03-31 S001       Contusion of eyelid and periocular ar… 25842
#>  6 2023-04-01 2024-03-31 S800       Contusion of knee                      11954
#>  7 2023-04-01 2024-03-31 S400       Contusion of shoulder and upper arm    14177
#>  8 2023-04-01 2024-03-31 T813       Disruption of operation wound, not el… 27140
#>  9 2023-04-01 2024-03-31 S324       Fracture of acetabulum                 11339
#> 10 2022-04-01 2023-03-31 S021       Fracture of base of skull              12294
#> # ℹ 72 more rows
# Filter to codes present in the CPRD Aurum ICD-10 pregnancy codelist.
# This codelist is available in OpenCodelists.org
codelist <- read.csv(
  "https://www.opencodelists.org/codelist/opensafely/pregnancy-icd10-aurum/5a7d8d12/download.csv"
)
icd10_usage |>
  dplyr::filter(icd10_code %in% codelist$code)
#> # A tibble: 5,365 × 5
#>    start_date end_date   icd10_code description                            usage
#>    <date>     <date>     <chr>      <chr>                                  <int>
#>  1 2023-04-01 2024-03-31 F530       Mild mental and behavioural disorders…  4206
#>  2 2023-04-01 2024-03-31 F531       Severe mental and behavioural disorde…   559
#>  3 2023-04-01 2024-03-31 F538       Other mental and behavioural disorder…    17
#>  4 2023-04-01 2024-03-31 F539       Puerperal mental disorder, unspecified   185
#>  5 2023-04-01 2024-03-31 O000       Abdominal pregnancy                       73
#>  6 2023-04-01 2024-03-31 O001       Tubal pregnancy                         9224
#>  7 2023-04-01 2024-03-31 O002       Ovarian pregnancy                        239
#>  8 2023-04-01 2024-03-31 O008       Other ectopic pregnancy                  847
#>  9 2023-04-01 2024-03-31 O009       Ectopic pregnancy, unspecified          1877
#> 10 2023-04-01 2024-03-31 O010       Classical hydatidiform mole              278
#> # ℹ 5,355 more rows
# Filter to codes in the ICD-10 Chapter XIX: "Injury, poisoning..."
# (codes begin with letters "S" or "T"), with usage > 10,000.
# For each of these, select the year with the highest count.
icd10_usage |>
  dplyr::filter(grepl("^[ST]", icd10_code) & usage > 10000) |>
  dplyr::group_by(description) |>
  dplyr::slice_max(usage)
#> # A tibble: 82 × 5
#> # Groups:   description [82]
#>    start_date end_date   icd10_code description                            usage
#>    <date>     <date>     <chr>      <chr>                                  <int>
#>  1 2023-04-01 2024-03-31 T812       Accidental puncture and laceration du… 23982
#>  2 2023-04-01 2024-03-31 T784       Allergy, unspecified                   10214
#>  3 2018-04-01 2019-03-31 T860       Bone-marrow transplant rejection       13927
#>  4 2023-04-01 2024-03-31 T310       Burns involving less than 10% of body… 14410
#>  5 2021-04-01 2022-03-31 S001       Contusion of eyelid and periocular ar… 25842
#>  6 2023-04-01 2024-03-31 S800       Contusion of knee                      11954
#>  7 2023-04-01 2024-03-31 S400       Contusion of shoulder and upper arm    14177
#>  8 2023-04-01 2024-03-31 T813       Disruption of operation wound, not el… 27140
#>  9 2023-04-01 2024-03-31 S324       Fracture of acetabulum                 11339
#> 10 2022-04-01 2023-03-31 S021       Fracture of base of skull              12294
#> # ℹ 72 more rows
# Filter to codes present in the CPRD Aurum ICD-10 pregnancy codelist.
# This codelist is available in OpenCodelists.org
codelist <- read.csv(
  "https://www.opencodelists.org/codelist/opensafely/pregnancy-icd10-aurum/5a7d8d12/download.csv"
)
icd10_usage |>
  dplyr::filter(icd10_code %in% codelist$code)
#> # A tibble: 5,365 × 5
#>    start_date end_date   icd10_code description                            usage
#>    <date>     <date>     <chr>      <chr>                                  <int>
#>  1 2023-04-01 2024-03-31 F530       Mild mental and behavioural disorders…  4206
#>  2 2023-04-01 2024-03-31 F531       Severe mental and behavioural disorde…   559
#>  3 2023-04-01 2024-03-31 F538       Other mental and behavioural disorder…    17
#>  4 2023-04-01 2024-03-31 F539       Puerperal mental disorder, unspecified   185
#>  5 2023-04-01 2024-03-31 O000       Abdominal pregnancy                       73
#>  6 2023-04-01 2024-03-31 O001       Tubal pregnancy                         9224
#>  7 2023-04-01 2024-03-31 O002       Ovarian pregnancy                        239
#>  8 2023-04-01 2024-03-31 O008       Other ectopic pregnancy                  847
#>  9 2023-04-01 2024-03-31 O009       Ectopic pregnancy, unspecified          1877
#> 10 2023-04-01 2024-03-31 O010       Classical hydatidiform mole              278
#> # ℹ 5,355 more rows
```
