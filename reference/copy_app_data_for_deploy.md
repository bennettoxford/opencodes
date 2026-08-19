# Copy the released parquet for every dataset into a local directory

Used by `just deploy` to populate `inst/app-data/` before deploying the
Shiny app, so the hosted app ships with its data instead of downloading
it on every cold start (see
[`get_local_data_path()`](https://bennettoxford.github.io/opencodecounts/reference/get_local_data_path.md)).
Always uses the released version from `tidy_data_sources.yml`,
downloading it first if it isn't already in the local cache - never
bundles unreleased data.

## Usage

``` r
copy_app_data_for_deploy(dest_dir)
```

## Arguments

- dest_dir:

  String, directory to copy the parquet files into

## Value

Invisible character vector of the copied file paths
