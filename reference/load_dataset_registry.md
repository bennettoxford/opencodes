# Load the Shiny app's dataset registry

Reads `inst/config/shiny_app_datasets.yml`, which lists every dataset
the app's "Select data" dropdown offers, keyed by dataset id (e.g.
"snomedct").

## Usage

``` r
load_dataset_registry()
```

## Value

A named list of dataset configs
