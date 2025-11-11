# Get codelist from [OpenCodelists](https://www.opencodelists.org)

Get codelist from [OpenCodelists](https://www.opencodelists.org)

## Usage

``` r
get_codelist(url)
```

## Arguments

- url:

  String, specifying URL to codelist on
  [OpenCodelists](https://www.opencodelists.org)

## Examples

``` r
# Get the 'cpeptide_cod' codelist from OpenCodelists.org
cpeptide_cod <- get_codelist("https://www.opencodelists.org/codelist/nhsd-primary-care-domain-refsets/cpeptide_cod/20200812/")

# Return all codes
cpeptide_cod$code
#> [1] "1106701000000107" "1106721000000103" "271227006"        "401124003"       
#> [5] "88705004"         "999351000000102" 

# Return 'coding_system' of codelist
cpeptide_cod@coding_system
#> [1] "snomedct"

# Return 'full_slug' of codelist
cpeptide_cod@full_slug
#> [1] "nhsd-primary-care-domain-refsets/cpeptide_cod/20200812"
```
