# Get a dataset for the Shiny app, preferring the bundled local copy

Used only by the app's own dataset dispatch (`mod_sidebar.R`), never
exposed to package users. Reads straight from `inst/app-data/` when the
deployed app has one (see
[`get_local_data_path()`](https://bennettoxford.github.io/opencodecounts/reference/get_local_data_path.md)),
otherwise falls back to
[`get_dataset()`](https://bennettoxford.github.io/opencodecounts/reference/get_dataset.md)'s
normal download-and-cache path, so this also works for local development
where `inst/app-data/` doesn't exist.

## Usage

``` r
get_app_dataset(dataset, version = NULL)
```

## Arguments

- dataset:

  String, dataset name as listed in `tidy_data_sources.yml`

- version:

  String, dataset version, or `NULL` for the latest

## Value

Tibble
