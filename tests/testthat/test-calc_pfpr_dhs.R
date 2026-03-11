# ============================================================================
# Tests for calc_pfpr_dhs() — WMR-format PfPR indicators
# ============================================================================

# --- Shared mock data -------------------------------------------------------
make_pfpr_mock <- function(n = 200) {
  set.seed(42)

  data.frame(
    hv000 = "SL7",
    hv007 = 2019L,
    hv001 = rep(1:20, each = n / 20),
    hv021 = rep(1:20, each = n / 20),
    hv005 = 1000000,
    hv022 = rep(1:4, each = n / 4),
    hv024 = rep(1:2, each = n / 2),
    hc1 = sample(6:59, n, replace = TRUE),
    hv103 = 1L,
    hv042 = 1L,
    hml35 = sample(c(0, 1, NA), n, replace = TRUE, prob = c(0.5, 0.3, 0.2)),
    hml32 = sample(c(0, 1, 6, NA), n, replace = TRUE,
                   prob = c(0.5, 0.2, 0.1, 0.2)),
    hv270 = sample(1:5, n, replace = TRUE),
    hv025 = sample(1:2, n, replace = TRUE),
    stringsAsFactors = FALSE
  )
}


# --- Test: basic structure ---------------------------------------------------

test_that("calc_pfpr_dhs returns named list with adm0", {
  skip_if_not_installed("survey")
  pr <- make_pfpr_mock()

  result <- calc_pfpr_dhs(dhs_pr = pr)

  expect_type(result, "list")
  expect_true("adm0" %in% names(result))
  expect_s3_class(result$adm0, "tbl_df")
})


test_that("adm0 has correct column structure", {
  skip_if_not_installed("survey")
  pr <- make_pfpr_mock()

  result <- calc_pfpr_dhs(dhs_pr = pr)

  expected_cols <- c(
    "survey_id", "iso3", "iso2", "survey_type", "survey_year",
    "adm0", "type", "geo_source",
    "point", "ci_l", "ci_u", "numerator", "denominator",
    "indicator", "indicator_code",
    "numerator_description", "denominator_description", "denominator_code"
  )
  expect_true(all(expected_cols %in% names(result$adm0)))
})


test_that("adm0 contains PFPR_RDT and PFPR_MIC indicators", {
  skip_if_not_installed("survey")
  pr <- make_pfpr_mock()

  result <- calc_pfpr_dhs(dhs_pr = pr)
  codes <- unique(result$adm0$indicator_code)

  expect_true("pfpr_rdt" %in% codes)
  expect_true("pfpr_mic" %in% codes)
})


# --- Test: survey metadata ---------------------------------------------------

test_that("survey metadata is correctly extracted", {
  skip_if_not_installed("survey")
  pr <- make_pfpr_mock()

  result <- calc_pfpr_dhs(dhs_pr = pr)
  adm0 <- result$adm0

  expect_equal(adm0$iso2[1], "SL")
  expect_equal(adm0$iso3[1], "SLE")
  expect_equal(adm0$survey_year[1], 2019L)
  expect_equal(adm0$adm0[1], "SIERRA LEONE")
})


# --- Test: auto-fallback to hv024 for adm1 ----------------------------------

test_that("auto-falls back to hv024 for adm1", {
  skip_if_not_installed("survey")
  pr <- make_pfpr_mock()

  # Give hv024 labels
  attr(pr$hv024, "labels") <- c("Northern" = 1L, "Southern" = 2L)
  class(pr$hv024) <- c("haven_labelled", "vctrs_vcl",
                        class(pr$hv024))

  result <- calc_pfpr_dhs(dhs_pr = pr)

  expect_false(is.null(result$adm1))
  expect_s3_class(result$adm1, "tbl_df")
  expect_true("adm1" %in% names(result$adm1))
  expect_true(all(result$adm1$adm1 == toupper(result$adm1$adm1)))
})


# --- Test: values are valid --------------------------------------------------

test_that("point estimates are between 0 and 1", {
  skip_if_not_installed("survey")
  pr <- make_pfpr_mock()

  result <- calc_pfpr_dhs(dhs_pr = pr)
  adm0 <- result$adm0

  valid <- !is.na(adm0$point)
  expect_true(all(adm0$point[valid] >= 0))
  expect_true(all(adm0$point[valid] <= 1))
})


test_that("CI bounds are ordered correctly", {
  skip_if_not_installed("survey")
  pr <- make_pfpr_mock()

  result <- calc_pfpr_dhs(dhs_pr = pr)
  adm0 <- result$adm0

  valid <- !is.na(adm0$point) & !is.na(adm0$ci_l) & !is.na(adm0$ci_u)
  expect_true(all(adm0$ci_l[valid] <= adm0$point[valid]))
  expect_true(all(adm0$point[valid] <= adm0$ci_u[valid]))
})


test_that("numerator and denominator are non-negative integers", {
  skip_if_not_installed("survey")
  pr <- make_pfpr_mock()

  result <- calc_pfpr_dhs(dhs_pr = pr)
  adm0 <- result$adm0

  expect_true(all(adm0$numerator >= 0, na.rm = TRUE))
  expect_true(all(adm0$denominator > 0, na.rm = TRUE))
  expect_true(is.integer(adm0$numerator))
  expect_true(is.integer(adm0$denominator))
})


# --- Test: type column -------------------------------------------------------

test_that("type column is survey_weighted", {
  skip_if_not_installed("survey")
  pr <- make_pfpr_mock()

  result <- calc_pfpr_dhs(dhs_pr = pr)
  expect_true(all(result$adm0$type == "survey_weighted"))
})


# --- Test: indicator filtering -----------------------------------------------

test_that("indicators parameter filters correctly", {
  skip_if_not_installed("survey")
  pr <- make_pfpr_mock()

  result <- calc_pfpr_dhs(dhs_pr = pr, indicators = "PFPR_RDT")
  adm0 <- result$adm0

  expect_equal(unique(adm0$indicator_code), "pfpr_rdt")
  expect_equal(nrow(adm0), 1)
})


# --- Test: pfpr_wmr_dictionary() --------------------------------------------

test_that("pfpr_wmr_dictionary returns correct structure", {
  dict <- pfpr_wmr_dictionary()

  expect_s3_class(dict, "tbl_df")
  expect_true("indicator" %in% names(dict))
  expect_true("indicator_code" %in% names(dict))
  expect_true("numerator_description" %in% names(dict))
  expect_true("denominator_description" %in% names(dict))
  expect_equal(nrow(dict), 2)
})


# --- Test: input validation --------------------------------------------------

test_that("calc_pfpr_dhs validates inputs", {
  expect_error(calc_pfpr_dhs(dhs_pr = "not a df"))
  expect_error(calc_pfpr_dhs(dhs_pr = data.frame()))
})
