# Get one dataset's config from the registry

Get one dataset's config from the registry

## Usage

``` r
get_dataset_by_id(dataset_id)
```

## Arguments

- dataset_id:

  String, dataset id as listed in `shiny_app_datasets.yml` (e.g.
  "snomedct")

## Value

A list with `label`, `get_function`, `code_column`,
`description_column`, `has_code_pattern_search`, `code_pattern_label`,
`source_label`, `source_url`
