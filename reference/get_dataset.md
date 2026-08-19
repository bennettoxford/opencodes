# Get a dataset, downloading and caching it if necessary

This is what every exported `get_*()` accessor calls, so it is the
public contract package users rely on: always download-and-cache,
regardless of what happens to be on disk. It deliberately never looks at
`inst/app-data/` (see
[`get_app_dataset()`](https://bennettoxford.github.io/opencodecounts/reference/get_app_dataset.md)
for the app's fast path).

## Usage

``` r
get_dataset(dataset, version = NULL)
```

## Arguments

- dataset:

  String, dataset name as listed in `tidy_data_sources.yml`

- version:

  String, dataset version, or `NULL` for the latest

## Value

Tibble
