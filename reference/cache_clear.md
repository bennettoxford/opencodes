# Clear the cache

Removes all cached dataset parquet files, e.g. to force a fresh download
on the next call to a `get_*()` function.

## Usage

``` r
cache_clear()
```

## Value

Invisible `TRUE`

## Examples

``` r
if (FALSE) { # \dontrun{
cache_clear()
} # }
```
