# opencodecounts

The `opencodecounts` R package provides easy access to yearly summaries
of clinical code usage in England. The package makes NHS England’s
published datasets available in R and an interactive online tool,
covering SNOMED CT codes in primary care and ICD-10/OPCS-4 codes in
secondary care.

You can launch the interactive Shiny app by clicking on [Launch Shiny
app](https://bennettoxford.github.io/opencodecounts/articles/app.html).
For work in R, all available functions are documented in the [R
Reference](https://bennettoxford.github.io/opencodecounts/reference/index.html)
section.

## Installation

You can install the `opencodecounts` package in R with:

``` r

# install.packages("pak")
pak::pak("bennettoxford/opencodecounts")
```

## How-to guides

- [How to use the Shiny
  app](https://bennettoxford.github.io/opencodecounts/articles/how-to-use-shiny-app.html)
- [How to use the R
  package](https://bennettoxford.github.io/opencodecounts/articles/how-to-use-R-pkg.html)
- [How to use ICD-10 and OPCS-4
  breakdowns](https://bennettoxford.github.io/opencodecounts/articles/how-to-use-breakdowns.html)
- [How to extract semantic tags from SNOMED CT
  descriptions](https://bennettoxford.github.io/opencodecounts/articles/extract-snomedct-sem-tag.html)

## Available datasets

The table below gives an overview of all available datasets in the
opencodecounts R package, including links to the tidy data as Parquet
files. To learn more about the available datasets see the vignette:
[Available datasets in
opencodecounts](https://bennettoxford.github.io/opencodecounts/articles/available-datasets.html).

| Function | First period | Last period | Version | Tidy data |
|:---|---:|---:|---:|:---|
| [`get_snomed_usage()`](https://bennettoxford.github.io/opencodecounts/reference/get_snomed_usage.md) | 2011-08-01 | 2025-07-31 | 0.1.0 | [Download](https://github.com/bennettoxford/opencodecounts/releases/download/snomed-usage-v0.1.0/snomed_usage.parquet) |
| [`get_icd10_usage()`](https://bennettoxford.github.io/opencodecounts/reference/get_icd10_usage.md) | 2012-04-01 | 2025-03-31 | 0.1.0 | [Download](https://github.com/bennettoxford/opencodecounts/releases/download/icd10-usage-v0.1.0/icd10_usage.parquet) |
| [`get_icd10_usage_breakdowns()`](https://bennettoxford.github.io/opencodecounts/reference/get_icd10_usage_breakdowns.md) | 2012-04-01 | 2025-03-31 | 0.1.0 | [Download](https://github.com/bennettoxford/opencodecounts/releases/download/icd10-usage-breakdowns-v0.1.0/icd10_usage_breakdowns.parquet) |
| [`get_opcs4_usage()`](https://bennettoxford.github.io/opencodecounts/reference/get_opcs4_usage.md) | 2012-04-01 | 2025-03-31 | 0.1.0 | [Download](https://github.com/bennettoxford/opencodecounts/releases/download/opcs4-usage-v0.1.0/opcs4_usage.parquet) |
| [`get_opcs4_usage_breakdowns()`](https://bennettoxford.github.io/opencodecounts/reference/get_opcs4_usage_breakdowns.md) | 2012-04-01 | 2025-03-31 | 0.1.0 | [Download](https://github.com/bennettoxford/opencodecounts/releases/download/opcs4-usage-breakdowns-v0.1.0/opcs4_usage_breakdowns.parquet) |

## For developers

See
[DEVELOPERS.md](https://bennettoxford.github.io/opencodecounts/DEVELOPERS.md).

## Licence

### R package

The `opencodecounts` package is licensed under the [MIT
License](https://bennettoxford.github.io/opencodecounts/LICENSE.md).

### Code usage datasets

All code usage datasets are Copyright NHS England and licensed under the
[Open Government Licence
v3.0](https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/).
Contains public sector information licensed under the Open Government
Licence v3.0.
