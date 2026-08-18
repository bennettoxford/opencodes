test_that("load_tidy_sources_config() parses the package's tidy_data_sources.yml", {
  sources <- load_tidy_sources_config()

  expect_true(all(
    c(
      "snomed_usage",
      "icd10_usage",
      "icd10_usage_breakdowns",
      "opcs4_usage",
      "opcs4_usage_breakdowns"
    ) %in%
      names(sources)
  ))
  expect_equal(sources$snomed_usage$version, "0.1.0")
  expect_true(grepl("^https://", sources$snomed_usage$url))
})

test_that("get_tidy_source_config() returns the latest version by default", {
  cfg <- get_tidy_source_config("snomed_usage")

  expect_equal(cfg$version, "0.1.0")
  expect_true(grepl("snomed_usage\\.parquet$", cfg$url))
})

test_that("get_tidy_source_config() errors for an unknown dataset", {
  expect_error(get_tidy_source_config("not_a_real_dataset"), "not found")
})

test_that("get_tidy_source_config() errors for an unknown version", {
  expect_error(
    get_tidy_source_config("snomed_usage", version = "99.0.0"),
    "not available"
  )
})

test_that("available_versions() lists known versions newest first", {
  expect_equal(available_versions("snomed_usage"), "0.1.0")
})

test_that("available_versions() errors for an unknown dataset", {
  expect_error(available_versions("not_a_real_dataset"), "not found")
})

test_that("get_tidy_source_cache_path() and sidecar path live under the cache data dir", {
  test_dir <- withr::local_tempdir()
  withr::local_envvar(OPENCODECOUNTS_TEST_CACHE_DIR = test_dir)

  expect_equal(
    get_tidy_source_cache_path("snomed_usage", "1.0.0"),
    file.path(test_dir, "data", "snomed_usage_1.0.0.parquet")
  )
  expect_equal(
    get_tidy_source_sidecar_path("snomed_usage", "1.0.0"),
    file.path(test_dir, "data", "snomed_usage_1.0.0.json")
  )
})

test_that("tidy_source_cache_is_current() reflects presence of the cached file", {
  test_dir <- withr::local_tempdir()
  withr::local_envvar(OPENCODECOUNTS_TEST_CACHE_DIR = test_dir)

  expect_false(tidy_source_cache_is_current("snomed_usage", "1.0.0"))

  writeLines(
    "fake parquet",
    get_tidy_source_cache_path("snomed_usage", "1.0.0")
  )

  expect_true(tidy_source_cache_is_current("snomed_usage", "1.0.0"))
})

test_that("download_tidy_source() writes the parquet into the cache and a sidecar JSON", {
  test_dir <- withr::local_tempdir()
  withr::local_envvar(OPENCODECOUNTS_TEST_CACHE_DIR = test_dir)

  local_mocked_bindings(
    req_perform = function(req, path = NULL, ...) {
      writeLines("fake parquet bytes", path)
      invisible(NULL)
    }
  )

  cache_path <- download_tidy_source(
    "snomed_usage",
    "https://example.com/snomed_usage.parquet",
    "1.0.0"
  )

  expect_true(file.exists(cache_path))
  expect_true(file.exists(get_tidy_source_sidecar_path(
    "snomed_usage",
    "1.0.0"
  )))

  sidecar <- jsonlite::read_json(get_tidy_source_sidecar_path(
    "snomed_usage",
    "1.0.0"
  ))
  expect_equal(sidecar$dataset, "snomed_usage")
  expect_equal(sidecar$version, "1.0.0")
})

test_that("download_tidy_source() cleans up the temp file and errors on failure", {
  test_dir <- withr::local_tempdir()
  withr::local_envvar(OPENCODECOUNTS_TEST_CACHE_DIR = test_dir)

  local_mocked_bindings(
    req_perform = function(req, path = NULL, ...) stop("network error")
  )

  expect_error(
    download_tidy_source(
      "snomed_usage",
      "https://example.com/snomed_usage.parquet",
      "1.0.0"
    ),
    "Failed to download"
  )
  expect_false(file.exists(get_tidy_source_cache_path("snomed_usage", "1.0.0")))
  expect_false(file.exists(paste0(
    get_tidy_source_cache_path("snomed_usage", "1.0.0"),
    ".tmp"
  )))
})

test_that("load_tidy_source() errors when the cache is empty", {
  test_dir <- withr::local_tempdir()
  withr::local_envvar(OPENCODECOUNTS_TEST_CACHE_DIR = test_dir)

  expect_error(load_tidy_source("snomed_usage", "1.0.0"), "Cache not found")
})
