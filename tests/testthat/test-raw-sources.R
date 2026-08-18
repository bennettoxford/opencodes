test_that("get_raw_source_periods() returns all periods for snomed_usage", {
  periods <- get_raw_source_periods("snomed_usage")

  expect_length(periods, 14)
  expect_true(all(vapply(periods, is.character, logical(1))))
  expect_equal(
    periods[["2024to2025"]],
    "https://files.digital.nhs.uk/9F/527A2C/SNOMED_code_usage_2024-25.txt"
  )
})

test_that("get_raw_source_periods() returns url/sheet/skip_rows/usage_col for icd10_usage", {
  periods <- get_raw_source_periods("icd10_usage")

  expect_length(periods, 13)
  expect_named(
    periods[["2024to2025"]],
    c("url", "sheet", "skip_rows", "usage_col")
  )
  expect_equal(periods[["2013to2014"]]$skip_rows, 18)
  expect_equal(periods[["2013to2014"]]$usage_col, 3)
})

test_that("get_raw_source_periods() returns url/sheet/range for the breakdowns datasets", {
  periods <- get_raw_source_periods("icd10_usage_breakdowns")

  expect_length(periods, 13)
  expect_named(periods[["2012to2013"]], c("url", "sheet", "range"))
  expect_equal(periods[["2012to2013"]]$range, "A18:AF11400")
})

test_that("period keys use the same <start-year>to<end-year> format across all datasets", {
  key_pattern <- "^\\d{4}to\\d{4}$"

  for (dataset in c(
    "snomed_usage",
    "icd10_usage",
    "icd10_usage_breakdowns",
    "opcs4_usage",
    "opcs4_usage_breakdowns"
  )) {
    keys <- names(get_raw_source_periods(dataset))
    expect_true(all(grepl(key_pattern, keys)), info = dataset)
  }
})

test_that("get_raw_source_periods() has matching period counts for opcs4 datasets", {
  expect_length(get_raw_source_periods("opcs4_usage"), 13)
  expect_length(get_raw_source_periods("opcs4_usage_breakdowns"), 13)
})

test_that("usage and breakdowns datasets share the same url/sheet per period", {
  icd10_usage <- get_raw_source_periods("icd10_usage")
  icd10_breakdowns <- get_raw_source_periods("icd10_usage_breakdowns")
  opcs4_usage <- get_raw_source_periods("opcs4_usage")
  opcs4_breakdowns <- get_raw_source_periods("opcs4_usage_breakdowns")

  for (period in names(icd10_usage)) {
    expect_equal(
      icd10_usage[[period]]$url,
      icd10_breakdowns[[period]]$url,
      info = period
    )
    expect_equal(
      icd10_usage[[period]]$sheet,
      icd10_breakdowns[[period]]$sheet,
      info = period
    )
  }
  for (period in names(opcs4_usage)) {
    expect_equal(
      opcs4_usage[[period]]$url,
      opcs4_breakdowns[[period]]$url,
      info = period
    )
    expect_equal(
      opcs4_usage[[period]]$sheet,
      opcs4_breakdowns[[period]]$sheet,
      info = period
    )
  }
})

test_that("get_raw_source_periods() errors for an unknown dataset", {
  expect_error(get_raw_source_periods("not_a_real_dataset"), "not found")
})
