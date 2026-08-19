# Turn a "to" period column into start_date/end_date

The data-raw build scripts key each NHS Digital source file by a period
label like "2024to2025" (see e.g. inst/config/raw_snomed.yml). This
turns that label into real dates once the files for all periods have
been combined with `bind_rows(.id = period_col)`. Datasets differ in
which month/day their reporting period starts and ends on (e.g.
snomed_usage runs 1 Aug - 31 Jul, icd10/opcs4 run the NHS financial year
1 Apr - 31 Mar), so that's passed in rather than assumed.

## Usage

``` r
add_period_dates(data, period_col, start_month_day, end_month_day)
```

## Arguments

- data:

  A data frame with a character column of period labels

- period_col:

  String, name of the period column to parse and replace

- start_month_day:

  String, "MM-DD" of the first day of the period

- end_month_day:

  String, "MM-DD" of the last day of the period
