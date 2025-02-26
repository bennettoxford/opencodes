#' Find codes with multiple description
#' @importFrom dplyr count distinct filter group_by pull select
#' @keywords internal
get_codes_with_multiple_desc <- function(code_usage_data) {
  select(code_usage_data, code = 3, description) |>
    distinct() |>
    group_by(code) |>
    count() |>
    filter(n > 1) |>
    pull(code) |>
    unique()
}

#' Find codes with encoding problems
#' @importFrom dplyr filter distinct select
#' @importFrom stringr str_detect
#' @keywords internal
get_codes_with_encoding_problems <- function(code_usage_data) {
  select(code_usage_data, code = 3, description) |>
    filter(str_detect(description, "\\S*\u00C3|\u00E2\\S*")) |>
    distinct() |>
    pull(code)
}

#' Fix encoding problems
#' @importFrom stringr str_replace_all
#' @keywords internal
fix_encoding <- function(string) {
  # This isn't the best way of doing this but it's quick and addresses the problem
  # We should explore other ways to read the data and fix the problems as part of
  # the data cleaning problem.

  # This includes sequences starting with:
  # - Latin Capital letter A with tilde (U+00C3)
  # - Latin Small Letter A with circumflex (U+00E2)
  replacement_dict <- c(
    # Although some numbers here are technically subscript
    # we are using the normal format to match the most used formatting.
    # We might also want to replace subscript numbers to regular text
    # e.g., 1 instead of subscript 1, but for now we wont address this and
    # accept the differences in the raw data.
    
    # The following mapping is used below:
    # \u00E2 = Latin Small Letter A with circumflex
    # \u002C = Comma
    # \u0081 = High Octet Preset
    # \u20AC = Euro sign
    # \u2122 = Trade Mark Sign
    # \u2020 = Dagger
    # \u00B6 = Pilcrow Sign
    # \u00A9 = Copyright Symbol
    "\u00E2\u002C\u0081" = "1",
    "\u00E2\u20AC\u2122" = "'",
    "\u00E2\u20AC\u201C" = "-",
    "\u00E2\u002C\u2020" = "6",
    "\u00E2\u002C\u002C" = "2",
    
    "\u00C3\u00B6" = "\u00F6",
    "\u00C3\u00A9" = "\u00E9",
    "\u00C3\u00A8" = "\u00E8"  
  )

  str_replace_all(string, replacement_dict)
}





