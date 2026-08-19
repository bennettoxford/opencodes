# Path to a copy of a dataset shipped inside the app, if there is one

`just deploy` copies the current released parquet for every dataset into
`inst/app-data/` before deploying the Shiny app, so the hosted app reads
data straight off disk instead of downloading it on every cold start
(see
[`copy_app_data_for_deploy()`](https://bennettoxford.github.io/opencodecounts/reference/copy_app_data_for_deploy.md)).
Local installs and package users never have this directory, so this
returns `NULL` for them and
[`get_dataset()`](https://bennettoxford.github.io/opencodecounts/reference/get_dataset.md)
falls back to the normal cache-and-download path.

## Usage

``` r
get_local_data_path(dataset, version)
```

## Arguments

- dataset:

  String, dataset name

- version:

  String, dataset version

## Value

Character path, or `NULL` if no local copy exists
