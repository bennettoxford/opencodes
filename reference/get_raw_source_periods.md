# Get raw NHS Digital source locations for a dataset

Reads the matching `inst/config/raw_<publication>.yml` file and returns
its `periods` list - the same shape as the hardcoded url lists that used
to live at the top of each `data-raw/*.R` script. To add a new year of
data, add one entry to that file's `periods:` map - the `data-raw/*.R`
scripts don't need to change.

## Usage

``` r
get_raw_source_periods(dataset)
```

## Arguments

- dataset:

  String, dataset name, e.g. "snomed_usage", "icd10_usage",
  "icd10_usage_breakdowns"

## Value

A named list, one entry per period. For `snomed_usage`, each entry is a
single url string. For the others, each entry is a list with `url` plus
the fields that dataset's data-raw script needs to read the right part
of the file (e.g. `sheet`, `skip_rows`, `range`).

## Details

A "publication" is one NHS Digital release (snomed, icd10, opcs4). Some
publications back more than one dataset - icd10_usage and
icd10_usage_breakdowns, for example, both read the same xlsx file per
period, just a different sheet region - so `raw_icd10.yml` stores the
shared `url`/`sheet` once per period, with `usage`/`breakdowns`
sub-fields for what each dataset reads from it. This function resolves
the dataset name to its publication file and merges in the right
sub-fields.
