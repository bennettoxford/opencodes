
<!-- DEVELOPERS.md is generated from DEVELOPERS.Rmd. Please edit that file -->

# Notes for developers

## Requirements

- R (\>= 4.1)
- [just](https://github.com/casey/just) - runs the commands below
  (`just list` to see them all)
- [air](https://github.com/posit-dev/air/) - formats R code
- [gh](https://cli.github.com/) - publishes releases to GitHub

## Configuration

All in `inst/config/`:

- `tidy_data_sources.yml` - current version and download url per
  dataset, read by `get_*()`
- `raw_<name>.yml` - source urls per NHS Digital publication, read only
  when rebuilding data
- `shiny_app_datasets.yml` - what shows up in the Shiny app’s dataset
  picker

## Building the data

Each `data-raw/<name>.R` script downloads the source files from NHS
Digital, cleans them, and writes a parquet file to `data-raw/`. Run one
script directly, or `just build-data` to rebuild everything (this takes
some time as it downloads the data).

## Publishing a new version

1.  Add the new year’s source url to `inst/config/raw_<name>.yml`, then
    run `data-raw/<name>.R` (or `just build-data`) to rebuild the
    parquet in `data-raw/`
2.  Add a new entry at the top of the dataset’s list in
    `tidy_data_sources.yml`, with a placeholder url
3.  `just release <dataset> <version> "<notes>"` - uploads the parquet
    as a GitHub release
4.  Copy the real download url from the release page over the
    placeholder

## Adding a new dataset

1.  Write `data-raw/<name>.R` and `inst/config/raw_<name>.yml`,
    following the existing pattern
2.  Add an entry to `tidy_data_sources.yml`
3.  Export a `get_<name>()` function in `R/get-data.R`
4.  `just release` to publish it
5.  Add it to `shiny_app_datasets.yml` if it should appear in the app

## Running the app locally

`run_app()` downloads and caches data like any other user. Run
`cache_clear()` first if you want to test against data you just
published.

## Deploying

Still done by hand with `rsconnect::deployApp()`.
