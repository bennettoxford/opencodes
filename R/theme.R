#' Bennett Institute brand colours
#'
#' Named colours used across the app. `navy` and `sand` are taken from the
#' Bennett Institute website (bennett.ox.ac.uk); the remaining colours are
#' the palette already used in the plots and tables.
#' Defined in one place so plots, tables, and the theme stay consistent.
#' @keywords internal
bennett_colours <- list(
  navy = "#011e41",
  sand = "#e4dfd4",
  teal = "#239b89ff",
  green = "#35B779FF",
  orange = "#ED6925FF"
)

#' The application theme
#'
#' Single central [bslib::bs_theme()] definition used by `app_ui()`.
#' Currently the plain Lumen preset; brand colours can be applied here later
#' in one place (e.g. `primary = bennett_colours$navy`).
#' @keywords internal
app_theme <- function() {
  bslib::bs_theme(
    version = 5,
    bootswatch = "lumen"
  )
}
