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
  # - Latin Capital letter
  # - Latin Small Letter A with circumflex
  replacement_dict <- c(
    # All these still need to be replaces with unicode

    # Although some numbers here are technically subscript
    # we are using the normal format to match the most used formatting.
    # We might also want to replace subscript numbers to regular text
    # e.g., 1 instead of subscript 1, but for now we wont address this and
    # accept the differences in the raw data.
    "\u00E2\u002C\u0081" = "1",
    "\u00E2\u20AC\u2122" = "'",
    "\u00E2\u20AC\u201C" = "-",
    "\u00E2\u002C\u2020" = "6",
    "\u00E2\u002C\u002C" = "2"
  )

  str_replace_all(string, replacement_dict)
}
