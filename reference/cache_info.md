# Display cache information

Shows the cache directory location, the number and total size of cached
datasets, and warns if the cache exceeds a recommended size limit.

## Usage

``` r
cache_info(max_size_mb = 1000)
```

## Arguments

- max_size_mb:

  Numeric, specifying maximum recommended cache size in MB. Default 1000

## Value

Invisibly returns a list with cache information

## Examples

``` r
if (FALSE) { # \dontrun{
cache_info()
cache_info(max_size_mb = 500)
} # }
```
