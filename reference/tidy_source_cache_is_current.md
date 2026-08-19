# Check whether a versioned dataset is cached

Returns `TRUE` if the parquet for this exact dataset+version exists on
disk. Because the version is part of the filename, no sidecar comparison
is needed.

## Usage

``` r
tidy_source_cache_is_current(dataset, version)
```

## Arguments

- dataset:

  String, dataset name

- version:

  String, expected version

## Value

Logical
