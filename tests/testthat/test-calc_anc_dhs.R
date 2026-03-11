test_that("calc_anc_dhs_core validates input data", {
  expect_error(
    calc_anc_dhs_core("not a dataframe"),
    "must be a data.frame"
  )

  expect_error(
    calc_anc_dhs_core(data.frame()),
    "is empty"
  )

  # Invalid birth window
  ir_data <- data.frame(v021 = 1:5, v005 = rep(1e6, 5), v022 = rep(1, 5))
  expect_error(
    calc_anc_dhs_core(ir_data, birth_window_months = 100),
    "must be between 1 and 60"
  )
})

test_that("calc_anc_dhs_core calculates correct ANC coverage", {
  skip_if_not_installed("survey")

  # 10 women with known ANC visits
  # Interview at CMC 1440, births in last 24 months
  ir_data <- data.frame(
    v021 = 1:10,
    v005 = rep(1000000, 10),
    v022 = rep(1, 10),
    v024 = rep("REGION1", 10),
    v008 = rep(1440, 10),
    b3_01 = rep(1425, 10),     # 15 months ago
    m14_1 = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 10)
  )

  result <- calc_anc_dhs_core(ir_data)

  expect_s3_class(result, "tbl_df")
  expect_true("dhs_anc_1plus" %in% names(result))
  expect_true("dhs_anc_4plus" %in% names(result))
  expect_true("dhs_anc_8plus" %in% names(result))
  expect_true("dhs_n_recent_births" %in% names(result))

  # ANC 1+: 9 of 10 (all except first)
  expect_equal(result$dhs_anc_1plus, 0.90)
  # ANC 4+: 6 of 10 (visits >= 4: values 4,5,6,7,8,10)
  expect_equal(result$dhs_anc_4plus, 0.60)
  # ANC 8+: 2 of 10 (visits >= 8: values 8 and 10)
  expect_equal(result$dhs_anc_8plus, 0.20)
  expect_equal(result$dhs_n_recent_births, 10L)
})

test_that("calc_anc_dhs_core filters by birth window", {
  skip_if_not_installed("survey")

  ir_data <- data.frame(
    v021 = 1:10,
    v005 = rep(1000000, 10),
    v022 = rep(1, 10),
    v024 = rep("REGION1", 10),
    v008 = rep(1440, 10),
    # 5 births within 24 months, 5 outside
    b3_01 = c(rep(1425, 5), rep(1400, 5)),
    m14_1 = c(4, 4, 4, 4, 4, 8, 8, 8, 8, 8)
  )

  result <- calc_anc_dhs_core(ir_data, birth_window_months = 24)

  # Only 5 births within window
  expect_equal(result$dhs_n_recent_births, 5L)
})

test_that("calc_anc_dhs_core excludes don't know responses", {
  skip_if_not_installed("survey")

  ir_data <- data.frame(
    v021 = 1:10,
    v005 = rep(1000000, 10),
    v022 = rep(1, 10),
    v024 = rep("REGION1", 10),
    v008 = rep(1440, 10),
    b3_01 = rep(1425, 10),
    # 98 = "don't know" in DHS
    m14_1 = c(4, 4, 4, 98, 98, NA, 0, 1, 2, 3)
  )

  result <- calc_anc_dhs_core(ir_data)

  # Exclude NA and 98 (don't know) = 7 valid
  expect_equal(result$dhs_n_recent_births, 7L)
})

test_that("anc_3plus is present in output with correct CI columns", {
  skip_if_not_installed("survey")

  ir_data <- data.frame(
    v021 = 1:10,
    v005 = rep(1000000, 10),
    v022 = rep(1, 10),
    v024 = rep("REGION1", 10),
    v008 = rep(1440, 10),
    b3_01 = rep(1425, 10),
    m14_1 = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 10)
  )

  result <- calc_anc_dhs_core(ir_data)

  expect_true("dhs_anc_3plus" %in% names(result))
  expect_true("dhs_anc_3plus_low" %in% names(result))
  expect_true("dhs_anc_3plus_upp" %in% names(result))
})

test_that("anc_3plus computes correct value with known data", {
  skip_if_not_installed("survey")

  ir_data <- data.frame(
    v021 = 1:10,
    v005 = rep(1000000, 10),
    v022 = rep(1, 10),
    v024 = rep("REGION1", 10),
    v008 = rep(1440, 10),
    b3_01 = rep(1425, 10),
    m14_1 = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 10)
  )

  result <- calc_anc_dhs_core(ir_data)

  # 7 of 10 have >= 3 visits (values: 3,4,5,6,7,8,10)
  expect_equal(result$dhs_anc_3plus, 0.70)
})

test_that("anc monotonicity: anc_1plus >= anc_3plus >= anc_4plus >= anc_8plus", {
  skip_if_not_installed("survey")

  ir_data <- data.frame(
    v021 = 1:10,
    v005 = rep(1000000, 10),
    v022 = rep(1, 10),
    v024 = rep("REGION1", 10),
    v008 = rep(1440, 10),
    b3_01 = rep(1425, 10),
    m14_1 = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 10)
  )

  result <- calc_anc_dhs_core(ir_data)

  expect_gte(result$dhs_anc_1plus, result$dhs_anc_3plus)
  expect_gte(result$dhs_anc_3plus, result$dhs_anc_4plus)
  expect_gte(result$dhs_anc_4plus, result$dhs_anc_8plus)
})

test_that("anc_3plus CIs satisfy low <= estimate <= upp and are clamped to [0, 1]", {
  skip_if_not_installed("survey")

  ir_data <- data.frame(
    v021 = 1:10,
    v005 = rep(1000000, 10),
    v022 = rep(1, 10),
    v024 = rep("REGION1", 10),
    v008 = rep(1440, 10),
    b3_01 = rep(1425, 10),
    m14_1 = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 10)
  )

  result <- calc_anc_dhs_core(ir_data)

  expect_lte(result$dhs_anc_3plus_low, result$dhs_anc_3plus)
  expect_gte(result$dhs_anc_3plus_upp, result$dhs_anc_3plus)
  expect_gte(result$dhs_anc_3plus_low, 0)
  expect_lte(result$dhs_anc_3plus_upp, 1)
})

test_that("calc_anc_dhs returns named list with adm0", {
  skip_if_not_installed("survey")

  ir_data <- data.frame(
    v021 = rep(1:5, each = 10),
    v005 = rep(1000000, 50),
    v022 = rep(1:2, each = 25),
    v024 = rep("REGION1", 50),
    v008 = rep(1440, 50),
    b3_01 = rep(1425, 50),
    m14_1 = sample(0:10, 50, replace = TRUE)
  )

  result <- calc_anc_dhs(ir_data)

  expect_type(result, "list")
  expect_true("adm0" %in% names(result))
  expect_s3_class(result$adm0, "tbl_df")
})

test_that("calc_anc_dhs adm0 has correct column structure", {
  skip_if_not_installed("survey")

  ir_data <- data.frame(
    v021 = rep(1:5, each = 10),
    v005 = rep(1000000, 50),
    v022 = rep(1:2, each = 25),
    v024 = rep("REGION1", 50),
    v008 = rep(1440, 50),
    b3_01 = rep(1425, 50),
    m14_1 = sample(0:10, 50, replace = TRUE)
  )

  result <- calc_anc_dhs(ir_data)

  expected_cols <- c(
    "survey_id", "iso3", "iso2", "survey_type", "survey_year",
    "adm0", "type", "geo_source",
    "point", "ci_l", "ci_u", "numerator", "denominator",
    "indicator", "indicator_code",
    "numerator_description", "denominator_description", "denominator_code"
  )
  expect_true(all(expected_cols %in% names(result$adm0)))
})

test_that("calc_anc_dhs adm0 contains all ANC indicator codes", {
  skip_if_not_installed("survey")

  ir_data <- data.frame(
    v021 = rep(1:5, each = 10),
    v005 = rep(1000000, 50),
    v022 = rep(1:2, each = 25),
    v024 = rep("REGION1", 50),
    v008 = rep(1440, 50),
    b3_01 = rep(1425, 50),
    m14_1 = sample(0:10, 50, replace = TRUE)
  )

  result <- calc_anc_dhs(ir_data)
  codes <- unique(result$adm0$indicator_code)

  expect_true("anc_1plus" %in% codes)
  expect_true("anc_2plus" %in% codes)
  expect_true("anc_3plus" %in% codes)
  expect_true("anc_4plus" %in% codes)
  expect_true("anc_8plus" %in% codes)
})

test_that("calc_anc_dhs point estimates are between 0 and 1", {
  skip_if_not_installed("survey")

  ir_data <- data.frame(
    v021 = rep(1:5, each = 10),
    v005 = rep(1000000, 50),
    v022 = rep(1:2, each = 25),
    v024 = rep("REGION1", 50),
    v008 = rep(1440, 50),
    b3_01 = rep(1425, 50),
    m14_1 = sample(0:10, 50, replace = TRUE)
  )

  result <- calc_anc_dhs(ir_data)
  adm0 <- result$adm0

  valid <- !is.na(adm0$point)
  expect_true(all(adm0$point[valid] >= 0))
  expect_true(all(adm0$point[valid] <= 1))
})

test_that("calc_anc_dhs CI bounds are ordered correctly", {
  skip_if_not_installed("survey")

  ir_data <- data.frame(
    v021 = rep(1:5, each = 10),
    v005 = rep(1000000, 50),
    v022 = rep(1:2, each = 25),
    v024 = rep("REGION1", 50),
    v008 = rep(1440, 50),
    b3_01 = rep(1425, 50),
    m14_1 = sample(0:10, 50, replace = TRUE)
  )

  result <- calc_anc_dhs(ir_data)
  adm0 <- result$adm0

  valid <- !is.na(adm0$point) & !is.na(adm0$ci_l) & !is.na(adm0$ci_u)
  expect_true(all(adm0$ci_l[valid] <= adm0$point[valid]))
  expect_true(all(adm0$point[valid] <= adm0$ci_u[valid]))
})

test_that("anc_dictionary returns correct structure", {
  dict <- anc_dictionary()

  expect_s3_class(dict, "tbl_df")
  expect_true("indicator" %in% names(dict))
  expect_true("indicator_code" %in% names(dict))
  expect_true("numerator_description" %in% names(dict))
  expect_true("denominator_description" %in% names(dict))
  expect_equal(nrow(dict), 5)
})
