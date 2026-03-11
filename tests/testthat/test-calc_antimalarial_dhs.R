test_that("calc_antimalarial_dhs_core validates input data", {
  expect_error(
    calc_antimalarial_dhs_core("not a dataframe"),
    "must be a data.frame"
  )

  expect_error(
    calc_antimalarial_dhs_core(data.frame()),
    "is empty"
  )

  kr_data <- data.frame(
    v021 = 1:10,
    v005 = rep(1000000, 10)
  )

  expect_error(
    calc_antimalarial_dhs_core(
      kr_data,
      survey_vars = list(
        cluster = "v021",
        weight = "v005",
        stratum = "v022",
        age = "hw1",
        fever = "h22"
      )
    ),
    "Required variables not found"
  )
})

test_that("calc_antimalarial_dhs_core errors when no ml13 variables found", {
  skip_if_not_installed("survey")

  kr_data <- data.frame(
    v021 = rep(1:10, each = 10),
    v005 = rep(1000000, 100),
    v022 = rep(1:2, each = 50),
    hw1 = sample(0:59, 100, replace = TRUE),
    h22 = sample(c(0, 1), 100, replace = TRUE, prob = c(0.6, 0.4)),
    stringsAsFactors = FALSE
  )

  expect_error(
    calc_antimalarial_dhs_core(kr_data),
    "No antimalarial treatment variables"
  )
})

test_that("calc_antimalarial_dhs_core calculates antimalarial rate", {
  skip_if_not_installed("survey")

  set.seed(42)
  n <- 200

  kr_data <- data.frame(
    v021 = rep(1:20, each = 10),
    v005 = rep(1000000, n),
    v022 = rep(1:4, each = 50),
    v024 = rep(c("REGION1", "REGION2"), each = 100),
    hw1 = sample(0:59, n, replace = TRUE),
    h22 = sample(c(0, 1), n, replace = TRUE, prob = c(0.6, 0.4)),
    ml13a = NA_real_,
    ml13b = NA_real_,
    ml13e = NA_real_,
    stringsAsFactors = FALSE
  )

  febrile <- kr_data$h22 == 1
  kr_data$ml13a[febrile] <- sample(c(0, 1), sum(febrile), replace = TRUE, prob = c(0.8, 0.2))
  kr_data$ml13b[febrile] <- sample(c(0, 1), sum(febrile), replace = TRUE, prob = c(0.9, 0.1))
  kr_data$ml13e[febrile] <- sample(c(0, 1), sum(febrile), replace = TRUE, prob = c(0.7, 0.3))

  result <- calc_antimalarial_dhs_core(kr_data)

  expect_s3_class(result, "tbl_df")
  expect_true("dhs_antimalarial" %in% names(result))
  expect_true("dhs_antimalarial_low" %in% names(result))
  expect_true("dhs_antimalarial_upp" %in% names(result))
  expect_true("dhs_n_febrile" %in% names(result))
  expect_true("dhs_n_antimalarial" %in% names(result))

  # Proportions between 0 and 1
  expect_true(all(result$dhs_antimalarial >= 0 & result$dhs_antimalarial <= 1))
  expect_true(all(result$dhs_antimalarial_low >= 0))
  expect_true(all(result$dhs_antimalarial_upp <= 1))

  # Count columns are integers
  expect_type(result$dhs_n_febrile, "integer")
  expect_type(result$dhs_n_antimalarial, "integer")

  # Denominator is febrile children (summed across regions)
  expect_equal(sum(result$dhs_n_febrile), sum(febrile))
})

test_that("calc_antimalarial_dhs_core detects multiple ml13 variables", {
  skip_if_not_installed("survey")

  set.seed(123)
  n <- 150

  kr_data <- data.frame(
    v021 = rep(1:15, each = 10),
    v005 = rep(1000000, n),
    v022 = rep(1:3, each = 50),
    hw1 = sample(0:59, n, replace = TRUE),
    h22 = sample(c(0, 1), n, replace = TRUE, prob = c(0.6, 0.4)),
    ml13a = NA_real_,
    ml13b = NA_real_,
    ml13c = NA_real_,
    ml13d = NA_real_,
    ml13e = NA_real_,
    ml13aa = NA_real_,
    stringsAsFactors = FALSE
  )

  febrile <- kr_data$h22 == 1
  for (v in c("ml13a", "ml13b", "ml13c", "ml13d", "ml13e", "ml13aa")) {
    kr_data[[v]][febrile] <- sample(c(0, 1), sum(febrile), replace = TRUE, prob = c(0.85, 0.15))
  }

  result <- calc_antimalarial_dhs_core(kr_data)

  # Antimalarial rate should be higher than any single drug
  # (since it's OR of all ml13 variables)
  expect_true(result$dhs_antimalarial > 0)
  expect_true(result$dhs_n_antimalarial > 0)
})

test_that("calc_antimalarial_dhs_core composite indicator works correctly", {
  skip_if_not_installed("survey")

  set.seed(456)
  n <- 100

  kr_data <- data.frame(
    v021 = rep(1:10, each = 10),
    v005 = rep(1000000, n),
    v022 = rep(1:2, each = 50),
    hw1 = sample(0:59, n, replace = TRUE),
    h22 = rep(1, n),  # All febrile
    ml13a = rep(0, n),
    ml13e = rep(0, n),
    stringsAsFactors = FALSE
  )

  # Set some to have ml13a=1 (but not ml13e)
  kr_data$ml13a[1:30] <- 1

  # Set some to have ml13e=1 (but not ml13a)
  kr_data$ml13e[31:50] <- 1

  result <- calc_antimalarial_dhs_core(kr_data)

  # 50 out of 100 should have received any antimalarial
  expect_equal(result$dhs_n_antimalarial, 50L)
})

test_that("calc_antimalarial_dhs_core works with region_var", {
  skip_if_not_installed("survey")

  set.seed(789)
  n <- 200

  kr_data <- data.frame(
    v021 = rep(1:20, each = 10),
    v005 = rep(1000000, n),
    v022 = rep(1:4, each = 50),
    v024 = rep(c("REGION1", "REGION2"), each = 100),
    hw1 = sample(0:59, n, replace = TRUE),
    h22 = sample(c(0, 1), n, replace = TRUE, prob = c(0.6, 0.4)),
    ml13a = NA_real_,
    ml13e = NA_real_,
    stringsAsFactors = FALSE
  )

  febrile <- kr_data$h22 == 1
  kr_data$ml13a[febrile] <- sample(c(0, 1), sum(febrile), replace = TRUE)
  kr_data$ml13e[febrile] <- sample(c(0, 1), sum(febrile), replace = TRUE)

  result <- calc_antimalarial_dhs_core(kr_data, region_var = "v024")

  expect_s3_class(result, "tbl_df")
  expect_true("v024" %in% names(result))
  expect_equal(nrow(result), 2)
})

test_that("calc_antimalarial_dhs returns named list with adm0", {
  skip_if_not_installed("survey")

  set.seed(111)
  n <- 200

  kr_data <- data.frame(
    v021 = rep(1:20, each = 10),
    v005 = rep(1000000, n),
    v022 = rep(1:4, each = 50),
    hw1 = sample(0:59, n, replace = TRUE),
    h22 = sample(c(0, 1), n, replace = TRUE, prob = c(0.6, 0.4)),
    ml13a = NA_real_,
    ml13e = NA_real_,
    stringsAsFactors = FALSE
  )

  febrile <- kr_data$h22 == 1
  kr_data$ml13a[febrile] <- sample(c(0, 1), sum(febrile), replace = TRUE)
  kr_data$ml13e[febrile] <- sample(c(0, 1), sum(febrile), replace = TRUE)

  result <- calc_antimalarial_dhs(kr_data)

  expect_type(result, "list")
  expect_true("adm0" %in% names(result))
  expect_s3_class(result$adm0, "tbl_df")

  # Long-format output should contain indicator_code column

  # antimalarial indicator should always be present
  expect_true("antimalarial" %in% result$adm0$indicator_code)

  # Expected long-format columns
  expected_cols <- c(
    "survey_id", "iso3", "iso2", "survey_type", "survey_year",
    "adm0", "type", "geo_source", "point", "ci_l", "ci_u",
    "numerator", "denominator", "indicator", "indicator_code",
    "numerator_description", "denominator_description", "denominator_code"
  )
  for (col in expected_cols) {
    expect_true(col %in% names(result$adm0), info = paste("Missing column:", col))
  }
})

test_that("antimalarial_dictionary returns expected indicators", {
  dict <- antimalarial_dictionary()

  expect_s3_class(dict, "tbl_df")
  expect_true("indicator_code" %in% names(dict))
  expect_true("antimalarial" %in% dict$indicator_code)
  expect_true("antimalarial_public" %in% dict$indicator_code)
  expect_equal(nrow(dict), 2)
})

test_that("calc_antimalarial_dhs_core computes antimalarial_public when h32 vars present", {
  skip_if_not_installed("survey")

  set.seed(222)
  n <- 200

  kr_data <- data.frame(
    v021 = rep(1:20, each = 10),
    v005 = rep(1000000, n),
    v022 = rep(1:4, each = 50),
    v024 = rep(c("REGION1", "REGION2"), each = 100),
    hw1 = sample(0:59, n, replace = TRUE),
    h22 = sample(c(0, 1), n, replace = TRUE, prob = c(0.6, 0.4)),
    ml13a = NA_real_,
    ml13e = NA_real_,
    h32a = 0L,  # public hospital
    h32b = 0L,  # public health center
    h32c = 0L,  # private hospital
    stringsAsFactors = FALSE
  )

  febrile <- kr_data$h22 == 1
  kr_data$ml13a[febrile] <- sample(c(0, 1), sum(febrile), replace = TRUE, prob = c(0.6, 0.4))
  kr_data$ml13e[febrile] <- sample(c(0, 1), sum(febrile), replace = TRUE, prob = c(0.7, 0.3))
  # Some febrile children sought care at public facility
  kr_data$h32a[febrile] <- sample(c(0, 1), sum(febrile), replace = TRUE, prob = c(0.5, 0.5))

  result <- calc_antimalarial_dhs_core(kr_data)

  expect_s3_class(result, "tbl_df")
  expect_true("dhs_antimalarial" %in% names(result))
  # antimalarial_public should be present when h32 vars are available
  expect_true("dhs_antimalarial_public" %in% names(result))
  expect_true("dhs_n_antimalarial_public" %in% names(result))

  # antimalarial_public should be <= antimalarial (subset)
  expect_true(all(result$dhs_antimalarial_public <= result$dhs_antimalarial))
})
