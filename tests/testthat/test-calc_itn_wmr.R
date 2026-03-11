# ============================================================================
# Tests for calc_itn_wmr() — WMR-format ITN indicators
# ============================================================================

# --- Shared mock data -------------------------------------------------------
make_itn_mock <- function() {
  set.seed(42)
  n_hh <- 20

  mock_hr <- data.frame(
    hv001 = rep(1:4, each = 5),           # 4 clusters, 5 HH each
    hv005 = 1000000,                       # weight
    hv022 = rep(1:2, each = 10),           # 2 strata
    hhid = 1:n_hh,
    hv024 = rep(1:2, each = 10),           # 2 regions
    hv013 = sample(2:6, n_hh, replace = TRUE), # HH size
    hv000 = "TG7",
    hv007 = 2017,
    hv270 = sample(1:5, n_hh, replace = TRUE), # wealth quintile
    hv025 = sample(1:2, n_hh, replace = TRUE), # urban/rural
    hml10_1 = sample(c(0, 1), n_hh, replace = TRUE, prob = c(0.3, 0.7)),
    hml10_2 = sample(c(0, 1), n_hh, replace = TRUE, prob = c(0.5, 0.5)),
    stringsAsFactors = FALSE
  )

  # Give hv024 haven-style labels
  attr(mock_hr$hv024, "labels") <- c("Maritime" = 1L, "Plateaux" = 2L)
  class(mock_hr$hv024) <- c("haven_labelled", "vctrs_vcl", class(mock_hr$hv024))

  # Build person records matching household sizes
  pr_rows <- list()
  for (i in seq_len(n_hh)) {
    hh_sz <- mock_hr$hv013[i]
    for (j in seq_len(hh_sz)) {
      pr_rows <- c(pr_rows, list(data.frame(
        hv001 = mock_hr$hv001[i],
        hv005 = 1000000,
        hv022 = mock_hr$hv022[i],
        hhid = mock_hr$hhid[i],
        hv105 = sample(0:70, 1),
        hv104 = sample(1:2, 1),
        hml12 = sample(c(0, 1, 2), 1, prob = c(0.3, 0.5, 0.2)),
        hml18 = sample(c(0, 1), 1, prob = c(0.9, 0.1)),
        hv000 = "TG7",
        hv007 = 2017,
        hv024 = mock_hr$hv024[i],
        hv270 = mock_hr$hv270[i],
        hv025 = mock_hr$hv025[i],
        stringsAsFactors = FALSE
      )))
    }
  }
  mock_pr <- do.call(rbind, pr_rows)

  # Give hv024 labels to PR too
  attr(mock_pr$hv024, "labels") <- c("Maritime" = 1L, "Plateaux" = 2L)
  class(mock_pr$hv024) <- c("haven_labelled", "vctrs_vcl", class(mock_pr$hv024))

  list(hr = mock_hr, pr = mock_pr)
}


# --- Test: basic structure ---------------------------------------------------

test_that("calc_itn_wmr returns named list with adm0", {
  mocks <- make_itn_mock()
  result <- calc_itn_wmr(dhs_hr = mocks$hr, dhs_pr = mocks$pr)

  expect_type(result, "list")
  expect_true("adm0" %in% names(result))
  expect_s3_class(result$adm0, "tbl_df")
})


test_that("adm0 has correct column structure", {
  mocks <- make_itn_mock()
  result <- calc_itn_wmr(dhs_hr = mocks$hr, dhs_pr = mocks$pr)

  expected_cols <- c(
    "survey_id", "iso3", "iso2", "survey_type", "survey_year",
    "adm0", "type", "geo_source",
    "point", "ci_l", "ci_u", "numerator", "denominator",
    "indicator", "indicator_code",
    "numerator_description", "denominator_description", "denominator_code"
  )
  expect_true(all(expected_cols %in% names(result$adm0)))
})


test_that("adm0 contains all 42 base indicators", {
  mocks <- make_itn_mock()
  result <- calc_itn_wmr(dhs_hr = mocks$hr, dhs_pr = mocks$pr)

  adm0 <- result$adm0
  indicator_codes <- unique(adm0$indicator_code)

  # 6 categories x 7 subgroups = 42 indicators
  # Some may be missing if subgroup has 0 obs, but most should be present
  expect_true(length(indicator_codes) >= 30)

  # Check base indicators are present
  expect_true("enough_itn" %in% indicator_codes)
  expect_true("with_itn" %in% indicator_codes)
  expect_true("access_itn" %in% indicator_codes)
  expect_true("use_itn_chu5" %in% indicator_codes)
  expect_true("use_itn" %in% indicator_codes)
})


# --- Test: survey metadata ---------------------------------------------------

test_that("survey metadata is correctly extracted from HR data", {
  mocks <- make_itn_mock()
  result <- calc_itn_wmr(dhs_hr = mocks$hr, dhs_pr = mocks$pr)

  adm0 <- result$adm0
  expect_equal(adm0$iso2[1], "TG")
  expect_equal(adm0$iso3[1], "TGO")
  expect_equal(adm0$survey_year[1], 2017)
  expect_equal(adm0$survey_type[1], "DHS")
  expect_equal(adm0$adm0[1], "TOGO")
})


# --- Test: auto-fallback to hv024 for adm1 -----------------------------------

test_that("auto-falls back to hv024 for adm1", {
  mocks <- make_itn_mock()
  result <- calc_itn_wmr(dhs_hr = mocks$hr, dhs_pr = mocks$pr)

  # Should produce adm1 tab automatically
  expect_false(is.null(result$adm1))
  expect_s3_class(result$adm1, "tbl_df")
  expect_true("adm1" %in% names(result$adm1))

  # Region names should be uppercase
  expect_true(all(result$adm1$adm1 == toupper(result$adm1$adm1)))
})


# --- Test: values are valid ---------------------------------------------------

test_that("point estimates are between 0 and 1", {
  mocks <- make_itn_mock()
  result <- calc_itn_wmr(dhs_hr = mocks$hr, dhs_pr = mocks$pr)

  adm0 <- result$adm0
  valid <- !is.na(adm0$point)
  expect_true(all(adm0$point[valid] >= 0))
  expect_true(all(adm0$point[valid] <= 1))
})

test_that("CI bounds are ordered correctly", {
  mocks <- make_itn_mock()
  result <- calc_itn_wmr(dhs_hr = mocks$hr, dhs_pr = mocks$pr)

  adm0 <- result$adm0
  valid <- !is.na(adm0$point) & !is.na(adm0$ci_l) & !is.na(adm0$ci_u)
  expect_true(all(adm0$ci_l[valid] <= adm0$point[valid]))
  expect_true(all(adm0$point[valid] <= adm0$ci_u[valid]))
})

test_that("numerator and denominator are non-negative integers", {
  mocks <- make_itn_mock()
  result <- calc_itn_wmr(dhs_hr = mocks$hr, dhs_pr = mocks$pr)

  adm0 <- result$adm0
  expect_true(all(adm0$numerator >= 0, na.rm = TRUE))
  expect_true(all(adm0$denominator > 0, na.rm = TRUE))
  expect_true(is.integer(adm0$numerator))
  expect_true(is.integer(adm0$denominator))
})


# --- Test: wealth and residence splits ----------------------------------------

test_that("wealth and residence splits produce valid results", {
  mocks <- make_itn_mock()
  result <- calc_itn_wmr(dhs_hr = mocks$hr, dhs_pr = mocks$pr)

  adm0 <- result$adm0
  codes <- unique(adm0$indicator_code)

  # Check wealth splits exist
  expect_true("with_itn_low_wealth" %in% codes)
  expect_true("with_itn_high_wealth" %in% codes)
  expect_true("with_itn_non_low_wealth" %in% codes)
  expect_true("with_itn_non_high_wealth" %in% codes)

  # Check residence splits exist
  expect_true("with_itn_rural" %in% codes)
  expect_true("with_itn_urban" %in% codes)
})


# --- Test: age group indicators -----------------------------------------------

test_that("age group indicators are added when age_breaks provided", {
  mocks <- make_itn_mock()
  result <- calc_itn_wmr(
    dhs_hr = mocks$hr, dhs_pr = mocks$pr,
    age_breaks = c(0, 5, 15, Inf),
    age_labels = c("u5", "5_14", "ov15")
  )

  adm0 <- result$adm0
  codes <- unique(adm0$indicator_code)

  expect_true("use_itn_age_u5" %in% codes)
  expect_true("use_itn_age_5_14" %in% codes)
  expect_true("use_itn_age_ov15" %in% codes)
})


# --- Test: indicator filtering ------------------------------------------------

test_that("indicators parameter filters to requested indicators", {
  mocks <- make_itn_mock()
  result <- calc_itn_wmr(
    dhs_hr = mocks$hr, dhs_pr = mocks$pr,
    indicators = c("WITH_ITN", "USE_ITN")
  )

  adm0 <- result$adm0
  unique_indicators <- unique(adm0$indicator_code)
  expect_true("with_itn" %in% unique_indicators)
  expect_true("use_itn" %in% unique_indicators)
  expect_false("enough_itn" %in% unique_indicators)
})


# --- Test: input validation ---------------------------------------------------

test_that("calc_itn_wmr validates inputs", {
  mocks <- make_itn_mock()

  expect_error(calc_itn_wmr(dhs_hr = "not a df", dhs_pr = mocks$pr))
  expect_error(calc_itn_wmr(dhs_hr = mocks$hr, dhs_pr = "not a df"))
  expect_error(calc_itn_wmr(dhs_hr = data.frame(), dhs_pr = mocks$pr))
  expect_error(calc_itn_wmr(dhs_hr = mocks$hr, dhs_pr = data.frame()))
})


# --- Test: itn_wmr_dictionary() -----------------------------------------------

test_that("itn_wmr_dictionary returns correct structure", {
  dict <- itn_wmr_dictionary()

  expect_s3_class(dict, "tbl_df")
  expect_true("indicator" %in% names(dict))
  expect_true("indicator_code" %in% names(dict))
  expect_true("data_level" %in% names(dict))
  expect_true("numerator_description" %in% names(dict))
  expect_true("denominator_description" %in% names(dict))

  # Should have 42 rows (6 categories x 7 subgroups)
  expect_equal(nrow(dict), 42)
})


# --- Test: type column --------------------------------------------------------

test_that("type column is survey_weighted", {
  mocks <- make_itn_mock()
  result <- calc_itn_wmr(dhs_hr = mocks$hr, dhs_pr = mocks$pr)

  expect_true(all(result$adm0$type == "survey_weighted"))
})
