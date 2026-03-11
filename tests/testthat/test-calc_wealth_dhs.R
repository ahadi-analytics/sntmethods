# ============================================================================
# Tests for calc_wealth_dhs() — long-format wealth indicators
# ============================================================================

# --- Shared mock data -------------------------------------------------------

make_wealth_mock <- function(n_hh = 30, seed = 42) {
  set.seed(seed)
  data.frame(
    hv001 = rep(1:6, each = 5),
    hv005 = 1000000,
    hv022 = rep(1:2, each = 15),
    hv024 = rep(1:2, each = 15),
    hv270 = sample(1:5, n_hh, replace = TRUE),
    hv271 = rnorm(n_hh),
    hv012 = sample(3:8, n_hh, replace = TRUE),
    hv000 = "SL7",
    hv007 = 2019,
    stringsAsFactors = FALSE
  )
}


# --- Test: basic structure ---------------------------------------------------

test_that("calc_wealth_dhs returns named list with adm0", {
  mock_hr <- make_wealth_mock()
  result <- calc_wealth_dhs(dhs_hr = mock_hr)

  expect_type(result, "list")
  expect_true("adm0" %in% names(result))
  expect_s3_class(result$adm0, "tbl_df")
})


test_that("adm0 has correct column structure", {
  mock_hr <- make_wealth_mock()
  result <- calc_wealth_dhs(dhs_hr = mock_hr)

  expected_cols <- c(
    "survey_id", "iso3", "iso2", "survey_type", "survey_year",
    "adm0", "type", "geo_source",
    "point", "ci_l", "ci_u", "numerator", "denominator",
    "indicator", "indicator_code",
    "numerator_description", "denominator_description", "denominator_code"
  )
  expect_true(all(expected_cols %in% names(result$adm0)))
})


test_that("adm0 contains all 6 wealth indicators", {
  mock_hr <- make_wealth_mock()
  result <- calc_wealth_dhs(dhs_hr = mock_hr)

  indicator_codes <- unique(result$adm0$indicator_code)

  expect_true("wealth_q1" %in% indicator_codes)
  expect_true("wealth_q2" %in% indicator_codes)
  expect_true("wealth_q3" %in% indicator_codes)
  expect_true("wealth_q4" %in% indicator_codes)
  expect_true("wealth_q5" %in% indicator_codes)
  expect_true("gini" %in% indicator_codes)
  expect_equal(length(indicator_codes), 6)
})


# --- Test: input validation --------------------------------------------------

test_that("calc_wealth_dhs validates input data", {
  expect_error(
    calc_wealth_dhs(dhs_hr = data.frame()),
    "empty"
  )

  expect_error(
    calc_wealth_dhs(dhs_hr = "not a dataframe"),
    "data.frame"
  )
})


# --- Test: quintile proportions are valid ------------------------------------

test_that("calc_wealth_dhs point estimates are valid proportions", {
  mock_hr <- data.frame(
    hv001 = rep(1:4, each = 5),
    hv005 = 1000000,
    hv022 = rep(1:2, each = 10),
    hv024 = 1,
    hv270 = c(rep(1, 4), rep(2, 4), rep(3, 4), rep(4, 4), rep(5, 4)),
    hv271 = rnorm(20),
    hv012 = 4,
    hv000 = "SL7",
    hv007 = 2019,
    stringsAsFactors = FALSE
  )

  result <- calc_wealth_dhs(dhs_hr = mock_hr)
  adm0 <- result$adm0

  # Check that quintile proportions are between 0 and 1
  q_rows <- adm0[grepl("^wealth_q", adm0$indicator_code), ]
  expect_true(all(q_rows$point >= 0))
  expect_true(all(q_rows$point <= 1))

  # Each quintile should have proportion 0.2 (equal distribution)
  for (qcode in paste0("wealth_q", 1:5)) {
    est <- adm0$point[adm0$indicator_code == qcode]
    expect_equal(est, 0.2, tolerance = 0.01)
  }
})


# --- Test: auto-fallback to hv024 for adm1 -----------------------------------

test_that("auto-falls back to hv024 for adm1", {
  mock_hr <- make_wealth_mock()
  result <- calc_wealth_dhs(dhs_hr = mock_hr)

  expect_false(is.null(result$adm1))
  expect_s3_class(result$adm1, "tbl_df")
  expect_true("adm1" %in% names(result$adm1))

  # Region names should be uppercase
  expect_true(all(result$adm1$adm1 == toupper(result$adm1$adm1)))
})


# --- Test: CI ordering -------------------------------------------------------

test_that("CI bounds are ordered correctly", {
  mock_hr <- make_wealth_mock()
  result <- calc_wealth_dhs(dhs_hr = mock_hr)

  adm0 <- result$adm0
  valid <- !is.na(adm0$point) & !is.na(adm0$ci_l) & !is.na(adm0$ci_u)
  expect_true(all(adm0$ci_l[valid] <= adm0$point[valid]))
  expect_true(all(adm0$point[valid] <= adm0$ci_u[valid]))
})


# --- Test: Gini indicator present --------------------------------------------

test_that("Gini coefficient is included in results", {
  mock_hr <- make_wealth_mock(n_hh = 60)
  result <- calc_wealth_dhs(dhs_hr = mock_hr)

  adm0 <- result$adm0
  gini_row <- adm0[adm0$indicator_code == "gini", ]
  expect_equal(nrow(gini_row), 1)
  # Gini is between 0 and 1 (or NA)
  if (!is.na(gini_row$point)) {
    expect_true(gini_row$point >= 0 && gini_row$point <= 1)
  }
})


# --- Test: type column --------------------------------------------------------

test_that("type column is survey_weighted", {
  mock_hr <- make_wealth_mock()
  result <- calc_wealth_dhs(dhs_hr = mock_hr)

  expect_true(all(result$adm0$type == "survey_weighted"))
})


# --- Test: wealth_dictionary() -----------------------------------------------

test_that("wealth_dictionary returns correct structure", {
  dict <- wealth_dictionary()

  expect_s3_class(dict, "tbl_df")
  expect_true("indicator" %in% names(dict))
  expect_true("indicator_code" %in% names(dict))
  expect_true("numerator_description" %in% names(dict))
  expect_true("denominator_description" %in% names(dict))
  expect_true("denominator_code" %in% names(dict))

  # 6 indicators: wealth_q1 through wealth_q5 + gini
  expect_equal(nrow(dict), 6)
  expect_setequal(
    dict$indicator_code,
    c("wealth_q1", "wealth_q2", "wealth_q3", "wealth_q4", "wealth_q5", "gini")
  )
})


# ============================================================================
# Tests for calculate_dhs_gini() — UNCHANGED (core utility)
# ============================================================================

test_that("calculate_dhs_gini returns valid coefficient", {
  wealth_scores <- c(rep(-2, 20), rep(0, 30), rep(2, 50))
  weights <- rep(1, 100)
  population <- rep(4, 100)

  gini <- calculate_dhs_gini(wealth_scores, weights, population)

  expect_true(gini >= 0 && gini <= 1)
  expect_type(gini, "double")
})

test_that("calculate_dhs_gini handles perfect equality", {
  wealth_scores <- rep(5, 100)
  weights <- rep(1, 100)
  population <- rep(4, 100)

  gini <- calculate_dhs_gini(wealth_scores, weights, population)

  expect_equal(gini, 0)
})

test_that("calculate_dhs_gini handles insufficient data", {
  wealth_scores <- c(1, 2, 3)
  weights <- c(1, 1, 1)
  population <- c(4, 4, 4)

  expect_warning(
    gini <- calculate_dhs_gini(wealth_scores, weights, population),
    "fewer than 10"
  )

  expect_true(is.na(gini))
})
