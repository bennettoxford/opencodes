
<!-- README.md is generated from README.Rmd. Please edit that file -->

# opencodecounts

<!-- badges: start -->

[![R-CMD-check](https://github.com/bennettoxford/opencodecounts/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/bennettoxford/opencodecounts/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The `opencodecounts` R package provides easy access to yearly summaries
of clinical code usage in England. The package makes NHS England’s
published datasets available in R and an interactive online tool,
covering SNOMED CT codes in primary care and ICD-10/OPCS-4 codes in
secondary care.

You can launch the interactive Shiny app by clicking on [*Launch Shiny
app*](https://bennettoxford.github.io/opencodecounts/articles/app.html).
For work in R, all available functions (including datasets) are
documented in the [*R
Reference*](https://bennettoxford.github.io/opencodecounts/reference/index.html)
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
- [Learn about the available
  datasets](https://bennettoxford.github.io/opencodecounts/articles/available-datasets.html)

## Available datasets

| Function | Source | First period | Last period | Tidy data |
|:---|:---|---:|---:|:---|
| `get_snomed_usage()` | [Code Usage in Primary Care](https://digital.nhs.uk/data-and-information/publications/statistical/mi-snomed-code-usage-in-primary-care) | 2011-08-01 | 2025-07-31 | [Download](https://github.com/bennettoxford/opencodecounts/releases/download/snomed-usage-v0.1.0/snomed_usage.parquet) |
| `get_icd10_usage()` | [Hospital Admitted Patient Care Activity](https://digital.nhs.uk/data-and-information/publications/statistical/hospital-admitted-patient-care-activity) | 2012-04-01 | 2025-03-31 | [Download](https://github.com/bennettoxford/opencodecounts/releases/download/icd10-usage-v0.1.0/icd10_usage.parquet) |
| `get_icd10_usage_breakdowns()` | [Hospital Admitted Patient Care Activity](https://digital.nhs.uk/data-and-information/publications/statistical/hospital-admitted-patient-care-activity) | 2012-04-01 | 2025-03-31 | [Download](https://github.com/bennettoxford/opencodecounts/releases/download/icd10-usage-breakdowns-v0.1.0/icd10_usage_breakdowns.parquet) |
| `get_opcs4_usage()` | [Hospital Admitted Patient Care Activity](https://digital.nhs.uk/data-and-information/publications/statistical/hospital-admitted-patient-care-activity) | 2012-04-01 | 2025-03-31 | [Download](https://github.com/bennettoxford/opencodecounts/releases/download/opcs4-usage-v0.1.0/opcs4_usage.parquet) |
| `get_opcs4_usage_breakdowns()` | [Hospital Admitted Patient Care Activity](https://digital.nhs.uk/data-and-information/publications/statistical/hospital-admitted-patient-care-activity) | 2012-04-01 | 2025-03-31 | [Download](https://github.com/bennettoxford/opencodecounts/releases/download/opcs4-usage-breakdowns-v0.1.0/opcs4_usage_breakdowns.parquet) |

## For developers

See [DEVELOPERS.md](DEVELOPERS.md).
