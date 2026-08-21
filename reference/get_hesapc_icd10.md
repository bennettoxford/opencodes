# Get yearly ICD-10 diagnosis code usage from Hospital Episode Statistics Admitted Patient Care in England

Downloads (if not already cached) and returns the yearly summary of
4-character ICD-10 code usage from 1st April 2012 onwards. The code
usage represents the annual count of all episodes which record the given
ICD-10 code in any primary or secondary position. Restricted codes for
which annual usage is not published have been removed.

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

## Usage

``` r
get_hesapc_icd10(version = NULL)
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
# Filter to codes in the ICD-10 Chapter XIX: "Injury, poisoning..."
# (codes begin with letters "S" or "T"), with usage > 10,000.
get_hesapc_icd10() |>
  dplyr::filter(grepl("^[ST]", icd10_code) & usage > 10000)
} # }
```
