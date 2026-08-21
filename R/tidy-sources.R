#' Load tidy data sources configuration
#'
#' Every version entry in `tidy_data_sources.yml` must have an explicit `url`
#' pointing at the GitHub Release parquet asset.
#'
#' @return List of dataset configurations keyed by dataset name. Each entry
#'   has `version` (latest), `versions` (all known), and `url` (latest).
#'
#' @importFrom yaml read_yaml
#' @importFrom stats setNames
#'
#' @keywords internal
load_tidy_sources_config <- function() {
  config_path <- system.file(
    "config",
    "tidy_data_sources.yml",
    package = "opencodecounts"
  )

  if (config_path == "") {
    cli::cli_abort(
      "{.file tidy_data_sources.yml} not found in package installation",
      class = "opencodecounts_error_config_not_found"
    )
  }

  config <- read_yaml(config_path)

  if (!"datasets" %in% names(config)) {
    cli::cli_abort(
      "{.file tidy_data_sources.yml} must have a {.field datasets} section",
      class = "opencodecounts_error_config_invalid"
    )
  }

  sources <- config$datasets
  for (dataset in names(sources)) {
    cfg <- sources[[dataset]]

    if (is.null(cfg$versions) || length(cfg$versions) == 0) {
      cli::cli_abort(
        "Dataset {.val {dataset}} in {.file tidy_data_sources.yml} has no versions",
        class = "opencodecounts_error_config_invalid"
      )
    }

    for (i in seq_along(cfg$versions)) {
      entry <- cfg$versions[[i]]
      if (!is.list(entry) || is.null(entry$version)) {
        cli::cli_abort(
          "Dataset {.val {dataset}} version entry {i} must have a {.field version} field",
          class = "opencodecounts_error_config_invalid"
        )
      }
      if (is.null(entry$url)) {
        cli::cli_abort(
          c(
            "Dataset {.val {dataset}} version {.val {entry$version}} has no {.field url} field.",
            "i" = "Copy the parquet asset url from the GitHub Release page into {.file tidy_data_sources.yml}."
          ),
          class = "opencodecounts_error_config_invalid"
        )
      }
    }

    latest <- cfg$versions[[1]]
    sources[[dataset]]$version <- latest$version
    sources[[dataset]]$url <- latest$url
    version_numbers <- vapply(cfg$versions, `[[`, character(1), "version")
    version_urls <- vapply(cfg$versions, `[[`, character(1), "url")
    sources[[dataset]]$versions <- version_numbers
    sources[[dataset]]$version_urls <- setNames(version_urls, version_numbers)
  }

  sources
}

#' Get tidy source configuration for a dataset
#'
#' @param dataset String, dataset name (e.g. "hesapc_icd10")
#' @param version String, specific version to use, or `NULL` for the latest
#'
#' @return List with fields: version, versions, url, publication, variant
#'
#' @keywords internal
get_tidy_source_config <- function(dataset, version = NULL) {
  sources <- load_tidy_sources_config()

  if (!dataset %in% names(sources)) {
    cli::cli_abort(
      c(
        "Dataset {.val {dataset}} not found in {.file tidy_data_sources.yml}.",
        "i" = "Available datasets: {.val {names(sources)}}."
      ),
      class = "opencodecounts_error_dataset_not_found"
    )
  }

  cfg <- sources[[dataset]]

  if (!is.null(version)) {
    known <- cfg$versions
    if (!version %in% known) {
      cli::cli_abort(
        c(
          "Version {.val {version}} is not available for {.val {dataset}}.",
          "i" = "Available versions: {.val {known}}."
        ),
        class = "opencodecounts_error_version_not_found"
      )
    }
    cfg$version <- version
    cfg$url <- cfg$version_urls[[version]]
  }

  cfg
}

#' List available versions for a dataset
#'
#' Returns all data versions that can be pinned with the `version` argument of
#' the corresponding `get_*()` function. Versions are listed newest first.
#'
#' @param dataset String, dataset name as listed in `tidy_data_sources.yml`
#'   (e.g. `"gp_snomed"`, `"hesapc_icd10"`)
#'
#' @return Character vector of available versions, newest first
#'
#' @export
#'
#' @examples
#' \dontrun{
#' available_versions("hesapc_icd10")
#' }
available_versions <- function(dataset) {
  sources <- load_tidy_sources_config()

  if (!dataset %in% names(sources)) {
    cli::cli_abort(
      c(
        "Dataset {.val {dataset}} not found in {.file tidy_data_sources.yml}.",
        "i" = "Available datasets: {.val {names(sources)}}."
      ),
      class = "opencodecounts_error_dataset_not_found"
    )
  }

  unlist(sources[[dataset]]$versions)
}

#' Get path to cached dataset parquet
#'
#' @param dataset String, dataset name
#' @param version String, dataset version
#'
#' @return Character path
#'
#' @keywords internal
get_tidy_source_cache_path <- function(dataset, version) {
  file.path(get_data_cache_dir(), paste0(dataset, "_", version, ".parquet"))
}

#' Get path to a cached dataset's sidecar JSON
#'
#' @param dataset String, dataset name
#' @param version String, dataset version
#'
#' @return Character path
#'
#' @keywords internal
get_tidy_source_sidecar_path <- function(dataset, version) {
  file.path(get_data_cache_dir(), paste0(dataset, "_", version, ".json"))
}

#' Check whether a versioned dataset is cached
#'
#' Returns `TRUE` if the parquet for this exact dataset+version exists on
#' disk. Because the version is part of the filename, no sidecar comparison
#' is needed.
#'
#' @param dataset String, dataset name
#' @param version String, expected version
#'
#' @return Logical
#'
#' @keywords internal
tidy_source_cache_is_current <- function(dataset, version) {
  file.exists(get_tidy_source_cache_path(dataset, version))
}

#' Download a dataset parquet and store it in the cache
#'
#' @param dataset String, dataset name
#' @param url String, download URL
#' @param version String, dataset version (included in the cached filename)
#'
#' @return Invisible path to the cached parquet
#'
#' @importFrom httr2 request req_user_agent req_retry req_perform
#' @importFrom jsonlite write_json
#'
#' @keywords internal
download_tidy_source <- function(dataset, url, version) {
  cache_path <- get_tidy_source_cache_path(dataset, version)
  sidecar_path <- get_tidy_source_sidecar_path(dataset, version)
  temp_path <- paste0(cache_path, ".tmp")

  cli::cli_progress_step("Downloading {.val {dataset}} (v{version})")

  tryCatch(
    {
      request(url) |>
        req_user_agent(
          "opencodecounts (https://github.com/bennettoxford/opencodecounts)"
        ) |>
        req_retry(max_tries = 3) |>
        req_perform(path = temp_path)

      if (!suppressWarnings(file.rename(temp_path, cache_path))) {
        copied <- file.copy(temp_path, cache_path, overwrite = TRUE)
        unlink(temp_path)
        if (!copied) {
          cli::cli_abort(
            "Could not move downloaded file into the cache",
            class = "opencodecounts_error_download_failed"
          )
        }
      }
    },
    error = function(e) {
      if (file.exists(temp_path)) {
        unlink(temp_path)
      }
      cli::cli_abort(
        "Failed to download {.val {dataset}} from {.url {url}}: {conditionMessage(e)}",
        class = "opencodecounts_error_download_failed"
      )
    }
  )

  write_json(
    list(
      dataset = dataset,
      version = version,
      downloaded_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ"),
      url = url
    ),
    sidecar_path,
    auto_unbox = TRUE,
    pretty = TRUE
  )

  invisible(cache_path)
}

#' Load a dataset parquet from the cache
#'
#' @param dataset String, dataset name
#' @param version String, dataset version
#'
#' @return Tibble
#'
#' @importFrom arrow read_parquet
#'
#' @keywords internal
load_tidy_source <- function(dataset, version) {
  cache_path <- get_tidy_source_cache_path(dataset, version)

  if (!file.exists(cache_path)) {
    cli::cli_abort(
      "Cache not found for {.val {dataset}} (v{version})",
      class = "opencodecounts_error_cache_missing"
    )
  }

  read_parquet(cache_path)
}
