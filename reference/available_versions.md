# List available versions for a dataset

Returns all data versions that can be pinned with the `version` argument
of the corresponding `get_*()` function. Versions are listed newest
first.

## Usage

``` r
available_versions(dataset)
```

## Arguments

- dataset:

  String, dataset name as listed in `tidy_data_sources.yml` (e.g.
  `"snomed_usage"`, `"icd10_usage"`)

## Value

Character vector of available versions, newest first

## Examples

``` r
if (FALSE) { # \dontrun{
available_versions("snomed_usage")
} # }
```
