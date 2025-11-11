# Strip semantic tag from SNOMED CT description

Removes semantic tag from the description

## Usage

``` r
strip_semantic_tag(string)
```

## Arguments

- string:

  String, description of SNOMED CT codes

## Examples

``` r
strip_semantic_tag("Blood Pressure (observable entity)")
#> [1] "Blood Pressure"
```
