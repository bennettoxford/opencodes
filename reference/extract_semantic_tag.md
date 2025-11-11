# Extract semantic tag from SNOMED CT description

Add description

## Usage

``` r
extract_semantic_tag(string)
```

## Arguments

- string:

  String, description of SNOMED CT codes

## Examples

``` r
extract_semantic_tag("Blood Pressure (observable entity)")
#> [1] "observable entity"
```
