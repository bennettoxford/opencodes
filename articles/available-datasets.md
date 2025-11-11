# Available datasets in opencodecounts

Clinical codes are unique numeric or alphanumeric identifiers used in
healthcare settings for administrative, billing and clinical care
purposes. They are grouped into different classification systems to
allow standardised recording of diagnoses, procedures or medications, as
well as other clinical, demographic and administrative data. In England,
three clinical coding classification systems are used: SNOMED Clinical
Terms ([SNOMED
CT](https://www.england.nhs.uk/digitaltechnology/digital-primary-care/snomed-ct/))
in primary care (GP practices); and International Statistical
Classification of Diseases and Related Health Problems, 10th Revision
([ICD-10](https://digital.nhs.uk/developer/guides-and-documentation/building-healthcare-software/clinical-coding-classifications-and-terminology#icd-10))
and OPCS Classification of Interventions and Procedures, version 4
([OPCS-4](https://digital.nhs.uk/developer/guides-and-documentation/building-healthcare-software/clinical-coding-classifications-and-terminology#opcs-4))
in secondary care.

NHS England makes annual summaries of clinical coding activity in
general practice (SNOMED CT UK) and NHS hospitals (ICD-10 and OPCS-4,
with the most complete data available for inpatient admissions) openly
available. For each of the three classification systems, the summaries
provide annual usage of each code used at least once each year. The
*opencodecounts* package aggregates these summaries to allow data
exploration. The key consideration is that instances of clinical code
recording should not be misinterpreted as estimates of incidence,
prevalence or healthcare utilisation.

### R setup

To work with the datasets in R, ensure that the *opencodecounts* package
is loaded.

``` r
# Load opencodecounts package
library(opencodecounts)
#> To cite opencodecounts use: https://doi.org/10.1101/2025.10.14.25338005
```

## SNOMED CT code usage in GP practices in England

SNOMED CT is the world’s most comprehensive clinical terminology system,
encoding clinical findings, observations, anatomical terms, disease
causes, products and procedures. In England, the SNOMED CT UK edition
has been used in general practices since 2019, and contains 357,000
globally common codes, alongside UK-specific extension for local
screening procedures and products. In most cases, the information coded
prior to 2019 has been translated to SNOMED CT.

The annual code usage is
[published](https://digital.nhs.uk/data-and-information/publications/statistical/mi-snomed-code-usage-in-primary-care)
every October by NHS England, covering the preceding August to July. The
number of individuals included has been increasing, reaching over 62
million patient records across 6,600 providers in 2023/24. The annual
usage count reflects how many times each listed SNOMED code was added to
a GP patient record in England in a given year. The codes are included
if they have been used at least ones across the selected years. The
counts are rounded to the nearest 10, while counts between 1 and 4 are
withheld. The codes with no usage are excluded.

This is only a selection of the full dataset published by NHS Digital,
for data pre-processing see `/data-raw/snomed_code_usage.R`.

``` r
# Return SNOMED code usage data
snomed_usage
#> # A tibble: 1,523,967 × 7
#>    start_date end_date   snomed_code      description      usage active_at_start
#>    <date>     <date>     <chr>            <chr>            <int> <lgl>          
#>  1 2023-08-01 2024-07-31 279991000000102  Short message … 4.41e8 TRUE           
#>  2 2023-08-01 2024-07-31 184103008        Patient teleph… 1.91e8 TRUE           
#>  3 2023-08-01 2024-07-31 428481002        Patient mobile… 1.16e8 TRUE           
#>  4 2023-08-01 2024-07-31 423876004        Clinical docum… 7.81e7 TRUE           
#>  5 2023-08-01 2024-07-31 72313002         Systolic arter… 6.87e7 TRUE           
#>  6 2023-08-01 2024-07-31 1091811000000102 Diastolic arte… 6.87e7 TRUE           
#>  7 2023-08-01 2024-07-31 1000731000000107 Serum creatini… 4.82e7 TRUE           
#>  8 2023-08-01 2024-07-31 60621009         Body mass inde… 4.65e7 TRUE           
#>  9 2023-08-01 2024-07-31 1000661000000107 Serum sodium l… 4.63e7 TRUE           
#> 10 2023-08-01 2024-07-31 1000651000000109 Serum potassiu… 4.62e7 TRUE           
#> # ℹ 1,523,957 more rows
#> # ℹ 1 more variable: active_at_end <lgl>
```

## ICD-10 code usage in inpatient admissions in England

ICD-10 is a global classification system, containing 18,000 diagnoses
coded as four or five-character alphanumeric codes. It is used to
determine financial reimbursement for admitted patient care in all
NHS-commissioned acute hospitals in England. The coding data submitted
by the hospitals is aggregated and published openly by NHS England as
the Admitted Patient Care Activity of the Hospital Episode Statistics
([HES-APC](https://digital.nhs.uk/data-and-information/publications/statistical/hospital-admitted-patient-care-activity)).
It is released in annual intervals, covering April to March. Hospital
Episode Statistics also include information on the emergency, outpatient
and critical care, which is seperate from the Admitted Patient Care
activity presented in *opencodecounts*.

Activity in HES-APC is captured in episodes of care under one
consultant, known as Finished Consultant Episodes (FCEs). Each FCE can
be assigned up to 20 clinical diagnoses (ICD-10 codes). The usage count
for ICD-10 codes equals the number of FCEs with a specific
four-character ICD-10 code recorded in any diagnostic position. This
means that each diagnoses can be counted only once per FCE. To be
included in the dataset, each diagnosis code must have been used at
least once across the selected years. The counts in HES-APC are not
rounded and codes with no usage are excluded.

This is only a selection of the full dataset published by NHS Digital,
for data pre-processing see `/data-raw/icd10_usage.R`.

``` r
# Return ICD-10 code usage data
icd10_usage
#> # A tibble: 136,136 × 5
#>    start_date end_date   icd10_code description                            usage
#>    <date>     <date>     <chr>      <chr>                                  <int>
#>  1 2023-04-01 2024-03-31 A000       Cholera due to Vibrio cholerae 01, bi…     2
#>  2 2023-04-01 2024-03-31 A009       Cholera, unspecified                      40
#>  3 2023-04-01 2024-03-31 A010       Typhoid fever                            884
#>  4 2023-04-01 2024-03-31 A011       Paratyphoid fever A                      139
#>  5 2023-04-01 2024-03-31 A012       Paratyphoid fever B                       13
#>  6 2023-04-01 2024-03-31 A013       Paratyphoid fever C                        2
#>  7 2023-04-01 2024-03-31 A014       Paratyphoid fever, unspecified            68
#>  8 2023-04-01 2024-03-31 A020       Salmonella enteritis                    2165
#>  9 2023-04-01 2024-03-31 A021       Salmonella sepsis                        319
#> 10 2023-04-01 2024-03-31 A022       Localized salmonella infections           82
#> # ℹ 136,126 more rows
```

## OPCS-4 code usage in inpatient admissions in England

OPCS-4 consists of 11,500 four-letter alphanumeric codes and was
developed by NHS Digital for recording procedures. Similarly to ICD-10,
it determines financial reimbursement for admitted patient care in all
NHS-commissioned acute hospitals in England and is published in annual
April - March intervals by the NHS England as the Admitted Patient Care
Activity of the Hospital Episode Statistics
([HES-APC](https://digital.nhs.uk/data-and-information/publications/statistical/hospital-admitted-patient-care-activity)).

Activity in HES-APC is captured in episodes of care under one
consultant, known as Finished Consultant Episodes (FCEs). Each FCE can
be assigned up to 24 procedures (OPCS-4 codes). The usage count for the
OPCS-4 codes equals the number of times each 4-character OPCS-4 code is
listed across all FCEs in a given year. This means that unlike
diagnoses, a procedure can be counted multiple times within one FCE. To
be included in the dataset, each procedure code must have been used at
least once across the selected years. The counts in HES-APC are not
rounded and codes with no usage are excluded.

This is only a selection of the full dataset published by NHS Digital,
for the data pre-processing see `/data-raw/opcs4_usage.R`.

``` r
# Return OPCS-4 code usage data
opcs4_usage
#> # A tibble: 107,379 × 5
#>    start_date end_date   opcs4_code description                            usage
#>    <date>     <date>     <chr>      <chr>                                  <int>
#>  1 2023-04-01 2024-03-31 A011       Hemispherectomy                            7
#>  2 2023-04-01 2024-03-31 A012       Total lobectomy of brain                  36
#>  3 2023-04-01 2024-03-31 A013       Partial lobectomy of brain               134
#>  4 2023-04-01 2024-03-31 A018       Other specified major excision of tis…    27
#>  5 2023-04-01 2024-03-31 A019       Unspecified major excision of tissue …     3
#>  6 2023-04-01 2024-03-31 A021       Excision of lesion of tissue of front…  1399
#>  7 2023-04-01 2024-03-31 A022       Excision of lesion of tissue of tempo…   971
#>  8 2023-04-01 2024-03-31 A023       Excision of lesion of tissue of parie…   704
#>  9 2023-04-01 2024-03-31 A024       Excision of lesion of tissue of occip…   260
#> 10 2023-04-01 2024-03-31 A025       Excision of lesion of tissue of cereb…   604
#> # ℹ 107,369 more rows
```
