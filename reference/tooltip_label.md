# Input label with an info tooltip

Wraps the repeated
`tooltip(span(<label>, bs_icon("info-circle")), <text>)` pattern used
for input labels across the app.

## Usage

``` r
tooltip_label(
  label,
  text,
  ...,
  options = list(customClass = "left-align-tooltip")
)
```

## Arguments

- label:

  Character, the visible label text.

- text:

  Character, the tooltip body.

- ...:

  Passed on to
  [`bslib::tooltip()`](https://rstudio.github.io/bslib/reference/tooltip.html)
  (e.g. `placement`).
