# Get tidy source configuration for a dataset

Get tidy source configuration for a dataset

## Usage

``` r
get_tidy_source_config(dataset, version = NULL)
```

## Arguments

- dataset:

  String, dataset name (e.g. "snomed_usage")

- version:

  String, specific version to use, or `NULL` for the latest

## Value

List with fields: version, versions, url
