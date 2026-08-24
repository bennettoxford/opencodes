.onLoad <- function(...) {
  S7::methods_register()
  options(rmarkdown.html_vignette.check_title = FALSE)
}

#' @importFrom utils citation
.onAttach <- function(libname, pkgname) {
  cit <- utils::citation(pkgname)
  doi_url <- paste0("https://doi.org/", cit$doi)
  packageStartupMessage("To cite ", pkgname, " use: ", doi_url)
}

utils::globalVariables(c(
  "annual_proportion",
  "start_date",
  "end_date",
  "snomed_code",
  "description",
  "usage",
  "usage_data_available",
  "icd10_code",
  "opcs4_code",
  "total_usage",
  "full_slug",
  "usage_data_available"
))
