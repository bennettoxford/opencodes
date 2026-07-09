# Smoke tests for the Shiny modules and server wiring.
# These guard the Stage 1 modular restructure and later UI changes.

test_usage_data <- function() {
  tibble::tibble(
    start_date = as.Date(c(
      "2022-08-01",
      "2023-08-01",
      "2022-08-01",
      "2023-08-01"
    )),
    end_date = as.Date(c(
      "2023-07-31",
      "2024-07-31",
      "2023-07-31",
      "2024-07-31"
    )),
    code = c("100", "100", "200", "200"),
    description = c("Code 100", "Code 100", "Code 200", "Code 200"),
    usage = c(10, 20, 30, 40)
  )
}

test_that("summarise_usage computes totals and percentages per code", {
  result <- summarise_usage(test_usage_data())

  expect_equal(result$code, c("100", "200"))
  expect_equal(result$total_usage, c(30, 70))
  expect_equal(result$total_pct, c(0.3, 0.7))
})

test_that("empty_selected_codes has the expected structure", {
  result <- empty_selected_codes()

  expect_equal(nrow(result), 0)
  expect_named(result, c("code", "description", "usage_data_available"))
  expect_equal(
    levels(result$usage_data_available),
    c("Usage data available", "No usage data reported")
  )
})

test_that("end_date_breaks keeps actual end dates and thins when busy", {
  few <- as.Date("2020-07-31") + (0:4) * 365
  expect_equal(end_date_breaks(few), few)

  many <- sort(as.Date("2012-07-31") + (0:13) * 365)
  breaks <- end_date_breaks(many)

  expect_lte(length(breaks), 6)
  # breaks are always real reporting end dates, anchored on the latest
  expect_true(all(breaks %in% many))
  expect_equal(breaks[length(breaks)], max(many))
})

test_that("sidebar module returns selected data for each dataset", {
  shiny::testServer(mod_sidebar_server, {
    session$setInputs(dataset = "icd10")
    expect_equal(session$returned$dataset(), "icd10")
    expect_named(
      session$returned$selected_data(),
      c("start_date", "end_date", "code", "description", "usage")
    )

    session$setInputs(dataset = "opcs4")
    expect_true(
      all(
        session$returned$selected_data()$code %in%
          opencodecounts::opcs4_usage$opcs4_code
      )
    )
  })
})

test_that("sidebar module tracks the search method", {
  shiny::testServer(mod_sidebar_server, {
    session$setInputs(dataset = "snomedct")
    expect_equal(session$returned$search_method(), "none")

    session$setInputs(code_specific_search = "123456789")
    expect_equal(session$returned$search_method(), "search")

    session$setInputs(reset_search_methods = 1)
    expect_equal(session$returned$search_method(), "none")
    expect_null(session$returned$codelist())
  })
})

test_that("app server filters data by dataset, date range, and code", {
  shiny::testServer(app_server, {
    dates <- range(opencodecounts::opcs4_usage$end_date)
    session$setInputs(
      `sidebar-dataset` = "opcs4",
      `sidebar-date_range` = dates
    )

    expect_gt(nrow(filtered_data()), 0)
    expect_named(
      filtered_data(),
      c("start_date", "end_date", "code", "description", "usage")
    )

    a_code <- opencodecounts::opcs4_usage$opcs4_code[[1]]
    session$setInputs(`sidebar-code_specific_search` = a_code)
    expect_equal(unique(filtered_data()$code), a_code)
  })
})

test_that("value boxes module renders counts from filtered data", {
  shiny::testServer(
    mod_value_boxes_server,
    args = list(filtered_data = shiny::reactive(test_usage_data())),
    {
      expect_equal(output$unique_codes, "2")
      expect_equal(output$total_activity, "100")
    }
  )
})

test_that("trends module renders a plot from filtered data", {
  shiny::testServer(
    mod_trends_server,
    args = list(
      filtered_data = shiny::reactive(test_usage_data()),
      dataset = shiny::reactive("snomedct"),
      reset = shiny::reactive(0)
    ),
    {
      session$setInputs(show_individual_codes = FALSE)
      # suppressWarnings: the "unknown aesthetics: text" warning is the
      # standard ggplotly tooltip idiom used in plot_summary()
      expect_no_error(suppressWarnings(output$usage_plot))
    }
  )
})

test_that("codes table module renders for each search method", {
  shiny::testServer(
    mod_codes_table_server,
    args = list(
      filtered_data = shiny::reactive(test_usage_data()),
      search_method = shiny::reactiveVal("none"),
      codelist = shiny::reactiveVal(NULL),
      dataset = shiny::reactive("snomedct")
    ),
    {
      expect_no_error(output$codes_table)

      search_method("search")
      expect_no_error(output$codes_table)

      search_method("codelist")
      codelist(tibble::tibble(
        code = c("100", "999"),
        description = c("Code 100", "Code 999")
      ))
      expect_no_error(output$codes_table)
    }
  )
})

test_that("usage table module renders from filtered data", {
  shiny::testServer(
    mod_usage_table_server,
    args = list(
      filtered_data = shiny::reactive(test_usage_data()),
      dataset = shiny::reactive("snomedct")
    ),
    {
      expect_no_error(output$usage_table)
    }
  )
})
