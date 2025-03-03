test_that("Test get codes with multiple desc", {
  df_test <- tibble::tribble(
    ~start_date, ~end_date, ~snomed_code, ~description, ~active_at_start, ~active_at_start, ~active_at_end,
    "2023-08-01", "2024-07-31", "100", "One code one description", 100, TRUE, TRUE,
    "2022-08-01", "2023-07-31", "100", "One code one description", 100, TRUE, TRUE,
    "2023-08-01", "2024-07-31", "200", " One code two description", 200, TRUE, TRUE,
    "2022-08-01", "2023-07-31", "200", " One code two descriptions", 200, TRUE, TRUE
  )

  expect_equal(
    get_codes_with_multiple_desc(df_test, snomed_code),
    c("200")
  )
})

test_that("Test get codes with encoding problem", {
  df_test <- tibble::tribble(
    ~start_date, ~end_date, ~snomed_code, ~description, ~active_at_start, ~active_at_start, ~active_at_end,
    "2023-08-01", "2024-07-31", "100", "No encoding problems", 100, TRUE, TRUE,
    "2022-08-01", "2023-07-31", "200", " Encoding problems Ã", 200, TRUE, TRUE,
    "2022-08-01", "2023-07-31", "300", " Encoding problems â", 300, TRUE, TRUE
  )

  expect_equal(
    get_codes_with_encoding_problems(df_test, snomed_code),
    c("200", "300")
  )
})

test_that("Test fix a circumflex lowercase encoding", {
  a_circumflex_lower <- c(
    "Provision of information about Infant crying is normal, Comforting methods can help, Itâ€™s OK to walk away, Never, ever shake a baby (procedure)",
    "Breast implantâ€“associated anaplastic large-cell lymphoma (disorder)",
    "Vitamin Bâ,\u0081â,, deficiency anaemia due to intrinsic factor deficiency",
    "Vitamin Bâ,\u0081â,, deficiency anaemia due to selective vitamin Bâ,\u0081â,, malabsorption with proteinuria",
    "Other dietary vitamin Bâ,\u0081â,, deficiency anaemia",
    "Other vitamin Bâ,\u0081â,, deficiency anaemias",
    "Vitamin Bâ,\u0081â,, deficiency anaemia, unspecified",
    "Megavitamin-Bâ,† syndrome",
    "GMâ,, gangliosidosis",
    "Poisoning: Histamine Hâ,,-receptor antagonists",
    "Vitamin Bâ,\u0081â,,, folic acid and other anti-megaloblastic-anaemia preparations",
    "Histamine Hâ,,-receptor antagonists"
  )

  expected_a_circumflex_lower_fix <- c(
    "Provision of information about Infant crying is normal, Comforting methods can help, It's OK to walk away, Never, ever shake a baby (procedure)",
    "Breast implant-associated anaplastic large-cell lymphoma (disorder)",
    "Vitamin B12 deficiency anaemia due to intrinsic factor deficiency",
    "Vitamin B12 deficiency anaemia due to selective vitamin B12 malabsorption with proteinuria",
    "Other dietary vitamin B12 deficiency anaemia",
    "Other vitamin B12 deficiency anaemias",
    "Vitamin B12 deficiency anaemia, unspecified",
    "Megavitamin-B6 syndrome",
    "GM2 gangliosidosis",
    "Poisoning: Histamine H2-receptor antagonists",
    "Vitamin B12, folic acid and other anti-megaloblastic-anaemia preparations",
    "Histamine H2-receptor antagonists"
  )

  expect_equal(
    fix_encoding(a_circumflex_lower),
    expected_a_circumflex_lower_fix
  )
})

test_that("Test fix a tilde uppercase encoding", {
  a_tilde_upper <- c(
    "Serum SjÃ¶gren's syndrome A 60 kiloDalton antibody (Ro) concentration (observable entity)",
    "ChÃ©diak-Higashi syndrome (disorder)",
    "DÃ©jÃ©rine-Sottas disease (disorder)",
    "MÃ©niÃ¨re's disease (disorder)",
    "LasÃ¨gue sign (finding)",
    "DÃ©jÃ\u00A0 pensÃ© (finding)",
    "DÃ©jÃ\u00A0 vu (finding)",
    "KÃ¼mmell disease (disorder)",
    "Hand-SchÃ¼ller-Christian disease (disorder)",
    "Montgomery-Ã…sberg depression rating scale (assessment scale)",
    "Pelger-HuÃ«t anomaly (disorder)",
    "Born in CuraÃ§ao (finding)",
    "Main spoken language Norwegian BokmÃ¥l (finding)",
    "Concentric sclerosis [BalÃ³]",
    "BehÃ§et disease"
  )

  expected_a_tilde_upper_fix <- c(
    "Serum Sjögren's syndrome A 60 kiloDalton antibody (Ro) concentration (observable entity)",
    "Chédiak-Higashi syndrome (disorder)",
    "Déjérine-Sottas disease (disorder)",
    "Ménière's disease (disorder)",
    "Lasègue sign (finding)",
    "Déjà pensé (finding)",
    "Déjà vu (finding)",
    "Kümmell disease (disorder)",
    "Hand-Schüller-Christian disease (disorder)",
    "Montgomery-Åsberg depression rating scale (assessment scale)",
    "Pelger-Huët anomaly (disorder)",
    "Born in Curaçao (finding)",
    "Main spoken language Norwegian Bokmål (finding)",
    "Concentric sclerosis [Baló]",
    "Behçet disease"
  )

  expect_equal(
    fix_encoding(a_tilde_upper),
    expected_a_tilde_upper_fix
  )
})

test_that("Test a circumflex lower singlelow 9 quotation mark encoding problems", {
  a_circumflex_lower_singlelow_9_quotation_mark <- c(
    "Vitamin Bâ‚\u0081â‚‚ deficiency anaemia due to intrinsic factor deficiency",
    "Vitamin Bâ‚\u0081â‚‚ deficiency anaemia due to selective vitamin Bâ‚\u0081â‚‚ malabsorption with proteinuria",
    "Other dietary vitamin Bâ‚\u0081â‚‚ deficiency anaemia",
    "Other vitamin Bâ‚\u0081â‚‚ deficiency anaemias",
    "Vitamin Bâ‚\u0081â‚‚ deficiency anaemia, unspecified",
    "Megavitamin-Bâ‚† syndrome",
    "GMâ‚‚ gangliosidosis",
    "Poisoning: Histamine Hâ‚‚-receptor antagonists",
    "Vitamin Bâ‚\u0081â‚‚, folic acid and other anti-megaloblastic-anaemia preparations",
    "Histamine Hâ‚‚-receptor antagonists"
  )

  expected_a_circumflex_lower_singlelow_9_quotation_mark_fix <- c(
    "Vitamin B12 deficiency anaemia due to intrinsic factor deficiency",
    "Vitamin B12 deficiency anaemia due to selective vitamin B12 malabsorption with proteinuria",
    "Other dietary vitamin B12 deficiency anaemia",
    "Other vitamin B12 deficiency anaemias",
    "Vitamin B12 deficiency anaemia, unspecified",
    "Megavitamin-B6 syndrome",
    "GM2 gangliosidosis",
    "Poisoning: Histamine H2-receptor antagonists",
    "Vitamin B12, folic acid and other anti-megaloblastic-anaemia preparations",
    "Histamine H2-receptor antagonists"
  )

  expect_equal(
    fix_encoding(a_circumflex_lower_singlelow_9_quotation_mark),
    expected_a_circumflex_lower_singlelow_9_quotation_mark_fix
  )
})
