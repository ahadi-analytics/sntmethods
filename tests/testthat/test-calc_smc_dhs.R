test_that("calc_smc_dhs_core validates input data", {
  expect_error(
    calc_smc_dhs_core("not a dataframe"),
    "must be a data.frame"
  )

  expect_error(
    calc_smc_dhs_core(data.frame()),
    "is empty"
  )

  # Missing SMC variable
  kr_data <- data.frame(
    v021 = 1:10,
    v005 = rep(1000000, 10),
    v022 = rep(1, 10),
    hw1 = rep(24, 10)
  )

  expect_error(
    calc_smc_dhs_core(kr_data),
    "No SMC variable found"
  )
})

test_that("calc_smc_dhs_core calculates correct SMC coverage", {
  skip_if_not_installed("survey")

  # 10 children, 6 received SMC
  kr_data <- data.frame(
    v021 = 1:10,
    v005 = rep(1000000, 10),
    v022 = rep(1, 10),
    v024 = rep("REGION1", 10),
    hw1 = rep(24, 10),
    hml43 = c(1, 1, 1, 1, 1, 1, 0, 0, 0, 0)
  )

  result <- calc_smc_dhs_core(kr_data)

  expect_s3_class(result, "tbl_df")
  expect_true("dhs_smc" %in% names(result))
  expect_true("dhs_smc_low" %in% names(result))
  expect_true("dhs_smc_upp" %in% names(result))
  expect_true("dhs_n_smc_eligible" %in% names(result))
  expect_true("dhs_n_smc_received" %in% names(result))

  expect_equal(result$dhs_smc, 0.60)
  expect_equal(result$dhs_n_smc_eligible, 10L)
  expect_equal(result$dhs_n_smc_received, 6L)
})

test_that("calc_smc_dhs_core uses alternative variable when primary missing", {
  skip_if_not_installed("survey")

  # No hml43 but has ml13g
  kr_data <- data.frame(
    v021 = 1:10,
    v005 = rep(1000000, 10),
    v022 = rep(1, 10),
    v024 = rep("REGION1", 10),
    hw1 = rep(24, 10),
    ml13g = c(1, 1, 0, 0, 0, 0, 0, 0, 0, 0)
  )

  result <- calc_smc_dhs_core(kr_data)

  expect_equal(result$dhs_smc, 0.20)
  expect_equal(result$dhs_n_smc_eligible, 10L)
})

test_that("calc_smc_dhs_core filters by age (U5 only)", {
  skip_if_not_installed("survey")

  kr_data <- data.frame(
    v021 = 1:10,
    v005 = rep(1000000, 10),
    v022 = rep(1, 10),
    v024 = rep("REGION1", 10),
    # 5 U5, 5 older
    hw1 = c(6, 12, 24, 36, 48, 60, 72, 84, 96, 108),
    hml43 = rep(1, 10)  # All received
  )

  result <- calc_smc_dhs_core(kr_data)

  # Only 5 U5 children
  expect_equal(result$dhs_n_smc_eligible, 5L)
  expect_equal(result$dhs_n_smc_received, 5L)
})

test_that("calc_smc_dhs_core handles NA and invalid responses", {
  skip_if_not_installed("survey")

  kr_data <- data.frame(
    v021 = 1:10,
    v005 = rep(1000000, 10),
    v022 = rep(1, 10),
    v024 = rep("REGION1", 10),
    hw1 = rep(24, 10),
    hml43 = c(1, 1, 0, 0, 0, NA, NA, 8, 9, 99)
  )

  result <- calc_smc_dhs_core(kr_data)

  # Only values 0 and 1 are valid
  expect_equal(result$dhs_n_smc_eligible, 5L)
  expect_equal(result$dhs_n_smc_received, 2L)
})

test_that("calc_smc_dhs returns named list with adm0", {
  skip_if_not_installed("survey")

  kr_data <- data.frame(
    v021 = rep(1:5, each = 10),
    v005 = rep(1000000, 50),
    v022 = rep(1:2, each = 25),
    v024 = rep("REGION1", 50),
    hw1 = sample(6:48, 50, replace = TRUE),
    hml43 = sample(c(0, 1), 50, replace = TRUE)
  )

  result <- calc_smc_dhs(kr_data)

  expect_type(result, "list")
  expect_true("adm0" %in% names(result))
  expect_s3_class(result$adm0, "tbl_df")
})

test_that("calc_smc_dhs adm0 has correct column structure", {
  skip_if_not_installed("survey")

  kr_data <- data.frame(
    v021 = rep(1:5, each = 10),
    v005 = rep(1000000, 50),
    v022 = rep(1:2, each = 25),
    v024 = rep("REGION1", 50),
    hw1 = sample(6:48, 50, replace = TRUE),
    hml43 = sample(c(0, 1), 50, replace = TRUE)
  )

  result <- calc_smc_dhs(kr_data)

  expected_cols <- c(
    "survey_id", "iso3", "iso2", "survey_type", "survey_year",
    "adm0", "type", "geo_source",
    "point", "ci_l", "ci_u", "numerator", "denominator",
    "indicator", "indicator_code",
    "numerator_description", "denominator_description", "denominator_code"
  )
  expect_true(all(expected_cols %in% names(result$adm0)))
})

test_that("calc_smc_dhs adm0 contains smc indicator", {
  skip_if_not_installed("survey")

  kr_data <- data.frame(
    v021 = rep(1:5, each = 10),
    v005 = rep(1000000, 50),
    v022 = rep(1:2, each = 25),
    v024 = rep("REGION1", 50),
    hw1 = sample(6:48, 50, replace = TRUE),
    hml43 = sample(c(0, 1), 50, replace = TRUE)
  )

  result <- calc_smc_dhs(kr_data)

  expect_true("smc" %in% result$adm0$indicator_code)
})

test_that("smc_dictionary returns correct structure", {
  dict <- smc_dictionary()

  expect_s3_class(dict, "tbl_df")
  expect_true("indicator_code" %in% names(dict))
  expect_true("indicator" %in% names(dict))
  expect_true("numerator_description" %in% names(dict))
  expect_true("denominator_description" %in% names(dict))
  expect_equal(nrow(dict), 1)
})
