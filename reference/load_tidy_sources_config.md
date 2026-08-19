# Load tidy data sources configuration

Every version entry in `tidy_data_sources.yml` must have an explicit
`url` pointing at the GitHub Release parquet asset.

## Usage

``` r
load_tidy_sources_config()
```

## Value

List of dataset configurations keyed by dataset name. Each entry has
`version` (latest), `versions` (all known), and `url` (latest).
