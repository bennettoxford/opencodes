# Path to a copy of a dataset shipped inside the app, if there is one

`just deploy` copies the current released parquet for every dataset into
`inst/app-data/` before deploying the Shiny app, so the hosted app reads
data straight off disk instead of downloading it on every cold start
(see
[`copy_app_data_for_deploy()`](https://bennettoxford.github.io/opencodecounts/reference/copy_app_data_for_deploy.md)).
Local installs and package users never have this directory, so this
returns `NULL` for them.

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

## Details

Only
[`get_app_dataset()`](https://bennettoxford.github.io/opencodecounts/reference/get_app_dataset.md)
calls this.
[`get_dataset()`](https://bennettoxford.github.io/opencodecounts/reference/get_dataset.md)
(and therefore every exported `get_*()` accessor) never does, so a stray
`inst/app-data/` left over from a deploy cannot change what a package
user's `get_*()` call does: it always downloads and caches.
