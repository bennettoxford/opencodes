# Make sure a dataset's parquet is in the cache, downloading it if not

Make sure a dataset's parquet is in the cache, downloading it if not

## Usage

``` r
ensure_tidy_source_cached(dataset, version = NULL)
```

## Arguments

- dataset:

  String, dataset name as listed in `tidy_data_sources.yml`

- version:

  String, dataset version, or `NULL` for the latest

## Value

The resolved version string
