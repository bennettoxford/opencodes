test_that("get_cache_dir() respects OPENCODECOUNTS_TEST_CACHE_DIR", {
  test_dir <- withr::local_tempdir()
  withr::local_envvar(OPENCODECOUNTS_TEST_CACHE_DIR = test_dir)

  expect_equal(get_cache_dir(), test_dir)
  expect_true(dir.exists(test_dir))
})

test_that("get_data_cache_dir() creates a data/ subdirectory inside the cache dir", {
  test_dir <- withr::local_tempdir()
  withr::local_envvar(OPENCODECOUNTS_TEST_CACHE_DIR = test_dir)

  data_dir <- get_data_cache_dir()

  expect_equal(data_dir, file.path(test_dir, "data"))
  expect_true(dir.exists(data_dir))
})

test_that("cache_info() reports zero datasets when the cache is empty", {
  test_dir <- withr::local_tempdir()
  withr::local_envvar(OPENCODECOUNTS_TEST_CACHE_DIR = test_dir)

  info <- cache_info()

  expect_equal(info$data_count, 0)
  expect_equal(info$data_size, 0)
})

test_that("cache_info() counts cached parquet files", {
  test_dir <- withr::local_tempdir()
  withr::local_envvar(OPENCODECOUNTS_TEST_CACHE_DIR = test_dir)

  data_dir <- get_data_cache_dir()
  writeLines("fake parquet", file.path(data_dir, "snomed_usage_1.0.0.parquet"))

  info <- cache_info()

  expect_equal(info$data_count, 1)
  expect_gt(info$data_size, 0)
})

test_that("cache_clear() removes cached data", {
  test_dir <- withr::local_tempdir()
  withr::local_envvar(OPENCODECOUNTS_TEST_CACHE_DIR = test_dir)

  data_dir <- get_data_cache_dir()
  writeLines("fake parquet", file.path(data_dir, "snomed_usage_1.0.0.parquet"))

  cache_clear()

  expect_false(dir.exists(data_dir))
})
