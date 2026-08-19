# Make sure every dataset the Shiny app can show is available before it starts

Called once from
[`run_app()`](https://bennettoxford.github.io/opencodecounts/reference/run_app.md).
On the deployed app, every dataset is already bundled in
`inst/app-data/`, so this is a no-op: no downloads, no parquet reads. In
local development it downloads and caches whatever isn't already there,
so picking a dataset from the "Select data" dropdown never stalls the UI
on a first-time download once the app has started.

## Usage

``` r
ensure_app_datasets_cached()
```
