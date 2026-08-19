# These tests call the get_*() accessors, which read from the local cache
# (downloading first if the cache is empty - see R/get-data.R). They stay in
# the fast unit suite rather than tests/integration.R because a populated
# cache makes them just as fast as reading a lazy-loaded data object; only a
# cold cache adds a one-off download.

snomed_usage <- get_snomed_usage()
icd10_usage <- get_icd10_usage()
icd10_usage_breakdowns <- get_icd10_usage_breakdowns()
opcs4_usage <- get_opcs4_usage()
opcs4_usage_breakdowns <- get_opcs4_usage_breakdowns()

# SNOMED tests

test_that("Test snomed_usage column names", {
  test_names <- names(snomed_usage)
  expect_equal(
    test_names,
    c(
      "start_date",
      "end_date",
      "snomed_code",
      "description",
      "usage",
      "active_at_start",
      "active_at_end"
    )
  )
})


test_that("Test snomed_usage column types", {
  expect_s3_class(snomed_usage$start_date, "Date")
  expect_s3_class(snomed_usage$end_date, "Date")
  expect_type(snomed_usage$snomed_code, "character")
  expect_type(snomed_usage$description, "character")
  expect_type(snomed_usage$usage, "integer")
  expect_type(snomed_usage$active_at_start, "logical")
  expect_type(snomed_usage$active_at_end, "logical")
})


test_that("Test snomed_usage rows", {
  test_nrow <- nrow(snomed_usage)
  expect_equal(test_nrow, 1682534L)
})

test_that("Test snomed_usage date range", {
  test_range_start_date <- range(snomed_usage$start_date)
  test_range_end_date <- range(snomed_usage$end_date)

  expect_equal(
    test_range_start_date,
    c(as.Date("2011-08-01"), as.Date("2024-08-01"))
  )
  expect_equal(
    test_range_end_date,
    c(as.Date("2012-07-31"), as.Date("2025-07-31"))
  )
})

test_that("Test sum of usage", {
  test_usage_sum <- sum(snomed_usage$usage)
  expect_equal(test_usage_sum, 47861217775)
})

test_that("Minimum SNOMED usage", {
  min_snomed_usage <- min(snomed_usage$usage)
  expect_equal(min_snomed_usage, 5)
})

test_that("Test no non-alphanumeric characters in SNOMED codes", {
  non_alphanumeric_codes_snomed <- snomed_usage$snomed_code[grep(
    "\\s?[^[:alnum:]]+\\s?",
    snomed_usage$snomed_code
  )]
  expect_equal(length(non_alphanumeric_codes_snomed), 0)
})

test_that("Test SNOMEDCT missing description", {
  test_sum_missing_description <- sum(is.na(snomed_usage$description))
  expect_equal(test_sum_missing_description, 0)
})

# This tests if we read in the codes correctly. We might want to improve this to test more than one case in the future.
# This illustrates the problem:
# $ python -c 'print(int(float("39733011000001106")))'
# 39733011000001104
test_that("Atorvastatin code is correct", {
  atorvastatin_code <- snomed_usage[
    snomed_usage$description == "Atorvastatin 20mg tablets (product)",
  ]$snomed_code
  expect_identical(unique(atorvastatin_code), "39733011000001106")
})

# ICD-10 Tests

test_that("Test icd10_usage column types", {
  expect_s3_class(icd10_usage$start_date, "Date")
  expect_s3_class(icd10_usage$end_date, "Date")
  expect_type(icd10_usage$icd10_code, "character")
  expect_type(icd10_usage$description, "character")
  expect_type(icd10_usage$usage, "integer")
})

test_that("Test icd10_usage rows", {
  test_nrow <- nrow(icd10_usage)
  expect_equal(test_nrow, 147483L)
})

test_that("Test icd10_usage date range", {
  test_range_start_date <- range(icd10_usage$start_date)
  test_range_end_date <- range(icd10_usage$end_date)

  expect_equal(
    test_range_start_date,
    c(as.Date("2012-04-01"), as.Date("2024-04-01"))
  )
  expect_equal(
    test_range_end_date,
    c(as.Date("2013-03-31"), as.Date("2025-03-31"))
  )
})

test_that("Test icd10_usage minimum usage", {
  test_min_usage <- min(icd10_usage$usage)
  expect_equal(test_min_usage, 1)
})

test_that("Test cummulative ICD-10 usage", {
  test_sum_usage <- sum(icd10_usage$usage)
  expect_equal(test_sum_usage, 1498723323)
})

test_that("Test ICD-10 usage are all integers", {
  test_sum_non_integers <- sum(!is.integer(icd10_usage$usage))
  expect_equal(test_sum_non_integers, 0)
})

test_that("Test no non-alphanumeric characters in ICD-10 codes", {
  non_alphanumeric_codes_icd10 <- icd10_usage$icd10_code[grep(
    "\\s?[^[:alnum:]]+\\s?",
    icd10_usage$icd10_code
  )]
  expect_equal(length(non_alphanumeric_codes_icd10), 0)
})

test_that("Test ICD10 missing description", {
  test_sum_missing_description <- sum(is.na(icd10_usage$description))
  expect_equal(test_sum_missing_description, 0)
})

# ICD-10 Breakdowns Tests
test_that("Test icd10_usage_breakdowns column names", {
  test_names <- names(icd10_usage_breakdowns)
  expect_equal(
    test_names,
    c(
      "start_date",
      "end_date",
      "icd10_code",
      "description",
      "breakdown",
      "usage"
    )
  )
})

test_that("Test icd10_usage_breakdowns column types", {
  expect_s3_class(icd10_usage_breakdowns$start_date, "Date")
  expect_s3_class(icd10_usage_breakdowns$end_date, "Date")
  expect_type(icd10_usage_breakdowns$icd10_code, "character")
  expect_type(icd10_usage_breakdowns$description, "character")
  expect_type(icd10_usage_breakdowns$breakdown, "character")
  expect_type(icd10_usage_breakdowns$usage, "integer")
})

test_that("Test icd10_usage_breakdowns date range", {
  test_range_start_date <- range(icd10_usage_breakdowns$start_date)
  test_range_end_date <- range(icd10_usage_breakdowns$end_date)

  expect_equal(
    test_range_start_date,
    c(as.Date("2012-04-01"), as.Date("2024-04-01"))
  )
  expect_equal(
    test_range_end_date,
    c(as.Date("2013-03-31"), as.Date("2025-03-31"))
  )
})

test_that("Test icd10_usage_breakdowns breakdown values", {
  expected_breakdowns <- c(
    "all_diagnoses",
    "main_diagnosis",
    "male",
    "female",
    "gender_unknown",
    "age_0",
    "age_1_4",
    "age_5_9",
    "age_10_14",
    "age_15",
    "age_16",
    "age_17",
    "age_18",
    "age_19",
    "age_20_24",
    "age_25_29",
    "age_30_34",
    "age_35_39",
    "age_40_44",
    "age_45_49",
    "age_50_54",
    "age_55_59",
    "age_60_64",
    "age_65_69",
    "age_70_74",
    "age_75_79",
    "age_80_84",
    "age_85_89",
    "age_90plus"
  )
  actual_breakdowns <- unique(icd10_usage_breakdowns$breakdown)
  expect_setequal(actual_breakdowns, expected_breakdowns)
})

test_that("Test no non-alphanumeric characters in ICD-10 breakdown codes", {
  non_alphanumeric_codes <- icd10_usage_breakdowns$icd10_code[grep(
    "\\s?[^[:alnum:]]+\\s?",
    icd10_usage_breakdowns$icd10_code
  )]
  expect_equal(length(non_alphanumeric_codes), 0)
})

test_that("Test ICD-10 breakdowns missing description", {
  test_sum_missing_description <- sum(is.na(icd10_usage_breakdowns$description))
  expect_equal(test_sum_missing_description, 0)
})

test_that("Test ICD-10 breakdowns totals match main dataset", {
  # Get first 3 unique codes
  test_codes <- unique(icd10_usage$icd10_code)[1:3]

  for (code in test_codes) {
    # Get usage from main dataset
    main_usage <- icd10_usage |>
      dplyr::filter(icd10_code == code) |>
      dplyr::select(start_date, end_date, usage) |>
      dplyr::arrange(start_date)

    # Get all_diagnoses breakdown usage
    breakdown_usage <- icd10_usage_breakdowns |>
      dplyr::filter(icd10_code == code, breakdown == "all_diagnoses") |>
      dplyr::select(start_date, end_date, usage) |>
      dplyr::arrange(start_date)

    expect_equal(
      main_usage,
      breakdown_usage,
      info = paste("Mismatch for ICD-10 code:", code)
    )
  }
})

# OPCS-4 Tests

test_that("Test opcs4_usage column types", {
  expect_s3_class(opcs4_usage$start_date, "Date")
  expect_s3_class(opcs4_usage$end_date, "Date")
  expect_type(opcs4_usage$opcs4_code, "character")
  expect_type(opcs4_usage$description, "character")
  expect_type(opcs4_usage$usage, "integer")
})

test_that("Test opcs4_usage rows", {
  test_nrow <- nrow(opcs4_usage)
  expect_equal(test_nrow, 116680L)
})

test_that("Test opcs4_usage date range", {
  test_range_start_date <- range(opcs4_usage$start_date)
  test_range_end_date <- range(opcs4_usage$end_date)

  expect_equal(
    test_range_start_date,
    c(as.Date("2012-04-01"), as.Date("2024-04-01"))
  )
  expect_equal(
    test_range_end_date,
    c(as.Date("2013-03-31"), as.Date("2025-03-31"))
  )
})

test_that("Test opcs4_usage minimum usage", {
  test_min_usage <- min(opcs4_usage$usage)
  expect_equal(test_min_usage, 1)
})

test_that("Test OPCS-4 usage are all integers", {
  test_sum_non_integers <- sum(!is.integer(opcs4_usage$usage))
  expect_equal(test_sum_non_integers, 0)
})

test_that("Test no non-alphanumeric characters in OPCS-4 codes", {
  non_alphanumeric_codes_opcs4 <- opcs4_usage$opcs4_code[grep(
    "\\s?[^[:alnum:]]+\\s?",
    opcs4_usage$opcs4_code
  )]
  expect_equal(length(non_alphanumeric_codes_opcs4), 0)
})

test_that("Test OPCS-4 missing description", {
  test_sum_missing_description <- sum(is.na(opcs4_usage$description))
  expect_equal(test_sum_missing_description, 0)
})


# OPCS-4 Breakdowns Tests

test_that("Test opcs4_usage_breakdowns column names", {
  test_names <- names(opcs4_usage_breakdowns)
  expect_equal(
    test_names,
    c(
      "start_date",
      "end_date",
      "opcs4_code",
      "description",
      "breakdown",
      "usage"
    )
  )
})

test_that("Test opcs4_usage_breakdowns column types", {
  expect_s3_class(opcs4_usage_breakdowns$start_date, "Date")
  expect_s3_class(opcs4_usage_breakdowns$end_date, "Date")
  expect_type(opcs4_usage_breakdowns$opcs4_code, "character")
  expect_type(opcs4_usage_breakdowns$description, "character")
  expect_type(opcs4_usage_breakdowns$breakdown, "character")
  expect_type(opcs4_usage_breakdowns$usage, "integer")
})

test_that("Test opcs4_usage_breakdowns date range", {
  test_range_start_date <- range(opcs4_usage_breakdowns$start_date)
  test_range_end_date <- range(opcs4_usage_breakdowns$end_date)

  expect_equal(
    test_range_start_date,
    c(as.Date("2012-04-01"), as.Date("2024-04-01"))
  )
  expect_equal(
    test_range_end_date,
    c(as.Date("2013-03-31"), as.Date("2025-03-31"))
  )
})

test_that("Test opcs4_usage_breakdowns breakdown values", {
  expected_breakdowns <- c(
    "all_procedures",
    "main_procedure",
    "male",
    "female",
    "gender_unknown",
    "age_0",
    "age_1_4",
    "age_5_9",
    "age_10_14",
    "age_15",
    "age_16",
    "age_17",
    "age_18",
    "age_19",
    "age_20_24",
    "age_25_29",
    "age_30_34",
    "age_35_39",
    "age_40_44",
    "age_45_49",
    "age_50_54",
    "age_55_59",
    "age_60_64",
    "age_65_69",
    "age_70_74",
    "age_75_79",
    "age_80_84",
    "age_85_89",
    "age_90plus"
  )
  actual_breakdowns <- unique(opcs4_usage_breakdowns$breakdown)
  expect_setequal(actual_breakdowns, expected_breakdowns)
})

test_that("Test no non-alphanumeric characters in OPCS-4 breakdown codes", {
  non_alphanumeric_codes <- opcs4_usage_breakdowns$opcs4_code[grep(
    "\\s?[^[:alnum:]]+\\s?",
    opcs4_usage_breakdowns$opcs4_code
  )]
  expect_equal(length(non_alphanumeric_codes), 0)
})

test_that("Test OPCS-4 breakdowns missing description", {
  test_sum_missing_description <- sum(is.na(opcs4_usage_breakdowns$description))
  expect_equal(test_sum_missing_description, 0)
})

test_that("Test OPCS-4 breakdowns totals match main dataset", {
  # Get first 3 unique codes
  test_codes <- unique(opcs4_usage$opcs4_code)[1:3]

  for (code in test_codes) {
    # Get usage from main dataset
    main_usage <- opcs4_usage |>
      dplyr::filter(opcs4_code == code) |>
      dplyr::select(start_date, end_date, usage) |>
      dplyr::arrange(start_date)

    # Get all_procedures breakdown usage
    breakdown_usage <- opcs4_usage_breakdowns |>
      dplyr::filter(opcs4_code == code, breakdown == "all_procedures") |>
      dplyr::select(start_date, end_date, usage) |>
      dplyr::arrange(start_date)

    expect_equal(
      main_usage,
      breakdown_usage,
      info = paste("Mismatch for OPCS-4 code:", code)
    )
  }
})
