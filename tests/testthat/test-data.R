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
  expect_equal(test_nrow, 1523967L)
})

test_that("Test snomed_usage date range", {
  test_range_start_date <- range(snomed_usage$start_date)
  test_range_end_date <- range(snomed_usage$end_date)

  expect_equal(
    test_range_start_date,
    c(as.Date("2011-08-01"), as.Date("2023-08-01"))
  )
  expect_equal(
    test_range_end_date,
    c(as.Date("2012-07-31"), as.Date("2024-07-31"))
  )
})

test_that("Test sum of usage", {
  test_usage_sum <- sum(snomed_usage$usage)
  expect_equal(test_usage_sum, 41721589830)
})

test_that("Test no non-alphanumeric characters in SNOMED codes", {
  non_alphanumeric_codes_snomed <- snomed_usage$snomed_code[grep("\\s?[^[:alnum:]]+\\s?", snomed_usage$snomed_code)]
  expect_equal(length(non_alphanumeric_codes_snomed), 0)
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
  expect_equal(test_nrow, 135951)
})

test_that("Test icd10_usage date range", {
  test_range_start_date <- range(icd10_usage$start_date)
  test_range_end_date <- range(icd10_usage$end_date)

  expect_equal(
    test_range_start_date,
    c(as.Date("2012-04-01"), as.Date("2023-04-01"))
  )
  expect_equal(
    test_range_end_date,
    c(as.Date("2013-03-31"), as.Date("2024-03-31"))
  )
})

test_that("Test icd10_usage minimum usage", {
  test_min_usage <- min(icd10_usage$usage)
  expect_equal(test_min_usage, 1)
})

test_that("Test cummulative ICD-10 usage", {
  test_sum_usage <- sum(icd10_usage$usage)
  expect_equal(test_sum_usage, 1333658601)
})

test_that("Test ICD-10 usage are all integers", {
  test_sum_non_integers <- sum(!is.integer(icd10_usage$usage))
  expect_equal(test_sum_non_integers, 0)
})

test_that("Test no non-alphanumeric characters in ICD-10 codes", {
  non_alphanumeric_codes_icd10 <- icd10_usage$icd10_code[grep("\\s?[^[:alnum:]]+\\s?", icd10_usage$icd10_code)]
  expect_equal(length(non_alphanumeric_codes_icd10), 0)
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
  expect_equal(test_nrow, 107376)
})

test_that("Test opcs4_usage date range", {
  test_range_start_date <- range(opcs4_usage$start_date)
  test_range_end_date <- range(opcs4_usage$end_date)

  expect_equal(
    test_range_start_date,
    c(as.Date("2012-04-01"), as.Date("2023-04-01"))
  )
  expect_equal(
    test_range_end_date,
    c(as.Date("2013-03-31"), as.Date("2024-03-31"))
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

sum_all_nas <- function(df) {
  na_sum <- sum(colSums(is.na(df)))
  return(na_sum)
}

test_that("Test no missing data points", {
  test_sum_nas <- sum_all_nas(opcs4_usage)
  expect_equal(test_sum_nas, 0)
})

test_that("Test no non-alphanumeric characters in OPCS-4 codes", {
  non_alphanumeric_codes_opcs4 <- opcs4_usage$opcs4_code[grep("\\s?[^[:alnum:]]+\\s?", opcs4_usage$opcs4_code)]
  expect_equal(length(non_alphanumeric_codes_opcs4), 0)
})
