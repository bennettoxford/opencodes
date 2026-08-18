test_that("get_dataset() downloads when not cached, then loads from cache", {
  download_calls <- 0
  load_calls <- 0

  local_mocked_bindings(
    tidy_source_cache_is_current = function(dataset, version) FALSE,
    download_tidy_source = function(dataset, url, version) {
      download_calls <<- download_calls + 1
      invisible(NULL)
    },
    load_tidy_source = function(dataset, version) {
      load_calls <<- load_calls + 1
      tibble::tibble(x = 1)
    }
  )

  result <- get_dataset("snomed_usage")

  expect_equal(download_calls, 1)
  expect_equal(load_calls, 1)
  expect_equal(result, tibble::tibble(x = 1))
})

test_that("get_dataset() skips downloading when already cached", {
  download_calls <- 0

  local_mocked_bindings(
    tidy_source_cache_is_current = function(dataset, version) TRUE,
    download_tidy_source = function(dataset, url, version) {
      download_calls <<- download_calls + 1
      invisible(NULL)
    },
    load_tidy_source = function(dataset, version) tibble::tibble(x = 1)
  )

  get_dataset("snomed_usage")

  expect_equal(download_calls, 0)
})

test_that("each get_*() accessor requests the correct dataset name", {
  requested <- character(0)

  local_mocked_bindings(
    get_dataset = function(dataset, version = NULL) {
      requested <<- c(requested, dataset)
      tibble::tibble(x = 1)
    }
  )

  get_snomed_usage()
  get_icd10_usage()
  get_icd10_usage_breakdowns()
  get_opcs4_usage()
  get_opcs4_usage_breakdowns()

  expect_equal(
    requested,
    c(
      "snomed_usage",
      "icd10_usage",
      "icd10_usage_breakdowns",
      "opcs4_usage",
      "opcs4_usage_breakdowns"
    )
  )
})

test_that("get_*() accessors pass version through to get_dataset()", {
  captured_version <- NULL

  local_mocked_bindings(
    get_dataset = function(dataset, version = NULL) {
      captured_version <<- version
      tibble::tibble(x = 1)
    }
  )

  get_snomed_usage(version = "1.0.0")

  expect_equal(captured_version, "1.0.0")
})
