test_that("calc_irs_dhs_core validates input data", {
  expect_error(
    calc_irs_dhs_core("not a dataframe"),
    "must be a data.frame"
  )

  expect_error(
    calc_irs_dhs_core(data.frame()),
    "is empty"
  )

  # Missing IRS variable
  hr_data <- data.frame(
    hv021 = 1:10,
    hv005 = rep(1000000, 10),
    hv022 = rep(1, 10)
  )

  expect_error(
    calc_irs_dhs_core(hr_data),
    "not found"
  )
})

test_that("calc_irs_dhs_core calculates correct IRS coverage", {
  skip_if_not_installed("survey")

  # 10 households, 4 sprayed
  hr_data <- data.frame(
    hv021 = 1:10,
    hv005 = rep(1000000, 10),
    hv022 = rep(1, 10),
    hv024 = rep("REGION1", 10),
    hv253 = c(1, 1, 1, 1, 0, 0, 0, 0, 0, 0)
  )

  result <- calc_irs_dhs_core(hr_data)

  expect_s3_class(result, "tbl_df")
  expect_true("dhs_irs" %in% names(result))
  expect_true("dhs_irs_low" %in% names(result))
  expect_true("dhs_irs_upp" %in% names(result))
  expect_true("dhs_n_households_irs" %in% names(result))
  expect_true("dhs_n_sprayed" %in% names(result))

  expect_equal(result$dhs_irs, 0.40)
  expect_equal(result$dhs_n_households_irs, 10L)
  expect_equal(result$dhs_n_sprayed, 4L)
})

test_that("calc_irs_dhs_core handles NA values", {
  skip_if_not_installed("survey")

  hr_data <- data.frame(
    hv021 = 1:10,
    hv005 = rep(1000000, 10),
    hv022 = rep(1, 10),
    hv024 = rep("REGION1", 10),
    hv253 = c(1, 1, 0, 0, 0, NA, NA, NA, NA, NA)
  )

  result <- calc_irs_dhs_core(hr_data)

  # Only 5 valid households should be included
  expect_equal(result$dhs_n_households_irs, 5L)
  expect_equal(result$dhs_n_sprayed, 2L)
})

test_that("calc_irs_dhs returns named list with adm0", {
  skip_if_not_installed("survey")

  hr_data <- data.frame(
    hv021 = rep(1:5, each = 10),
    hv005 = rep(1000000, 50),
    hv022 = rep(1:2, each = 25),
    hv024 = rep("REGION1", 50),
    hv253 = sample(c(0, 1), 50, replace = TRUE)
  )

  result <- calc_irs_dhs(hr_data)

  expect_type(result, "list")
  expect_true("adm0" %in% names(result))
  expect_s3_class(result$adm0, "tbl_df")
})

test_that("calc_irs_dhs adm0 has correct column structure", {
  skip_if_not_installed("survey")

  hr_data <- data.frame(
    hv021 = rep(1:5, each = 10),
    hv005 = rep(1000000, 50),
    hv022 = rep(1:2, each = 25),
    hv024 = rep("REGION1", 50),
    hv253 = sample(c(0, 1), 50, replace = TRUE)
  )

  result <- calc_irs_dhs(hr_data)

  expected_cols <- c(
    "survey_id", "iso3", "iso2", "survey_type", "survey_year",
    "adm0", "type", "geo_source",
    "point", "ci_l", "ci_u", "numerator", "denominator",
    "indicator", "indicator_code",
    "numerator_description", "denominator_description", "denominator_code"
  )
  expect_true(all(expected_cols %in% names(result$adm0)))
})

test_that("calc_irs_dhs adm0 contains irs indicator", {
  skip_if_not_installed("survey")

  hr_data <- data.frame(
    hv021 = rep(1:5, each = 10),
    hv005 = rep(1000000, 50),
    hv022 = rep(1:2, each = 25),
    hv024 = rep("REGION1", 50),
    hv253 = sample(c(0, 1), 50, replace = TRUE)
  )

  result <- calc_irs_dhs(hr_data)

  expect_true("irs" %in% result$adm0$indicator_code)
})

test_that("irs_dictionary returns correct structure", {
  dict <- irs_dictionary()

  expect_s3_class(dict, "tbl_df")
  expect_true("indicator_code" %in% names(dict))
  expect_true("indicator" %in% names(dict))
  expect_true("numerator_description" %in% names(dict))
  expect_true("denominator_description" %in% names(dict))
  expect_equal(nrow(dict), 1)
})
