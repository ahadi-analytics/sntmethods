test_that("calc_malaria_dx_dhs_core validates input data", {
  expect_error(
    calc_malaria_dx_dhs_core("not a dataframe"),
    "must be a data.frame"
  )

  expect_error(
    calc_malaria_dx_dhs_core(data.frame()),
    "is empty"
  )

  kr_data <- data.frame(
    v021 = 1:10,
    v005 = rep(1000000, 10)
  )

  expect_error(
    calc_malaria_dx_dhs_core(
      kr_data,
      survey_vars = list(
        cluster = "v021",
        weight = "v005",
        stratum = "v022",
        age = "hw1",
        fever = "h22",
        malaria_dx = "h47"
      )
    ),
    "Required variables not found"
  )
})

test_that("calc_malaria_dx_dhs_core errors when h47 missing", {
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
    calc_malaria_dx_dhs_core(kr_data),
    "not found in data"
  )
})

test_that("calc_malaria_dx_dhs_core calculates diagnostic testing rate", {
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
    h47 = NA_real_,
    stringsAsFactors = FALSE
  )

  # Only febrile children get h47 values
  febrile <- kr_data$h22 == 1
  kr_data$h47[febrile] <- sample(
    c(0, 1), sum(febrile), replace = TRUE, prob = c(0.5, 0.5)
  )

  result <- calc_malaria_dx_dhs_core(kr_data)

  expect_s3_class(result, "tbl_df")
  expect_true("dhs_malaria_dx" %in% names(result))
  expect_true("dhs_malaria_dx_low" %in% names(result))
  expect_true("dhs_malaria_dx_upp" %in% names(result))
  expect_true("dhs_n_febrile" %in% names(result))
  expect_true("dhs_n_tested" %in% names(result))

  # Proportions between 0 and 1
  expect_true(all(result$dhs_malaria_dx >= 0 & result$dhs_malaria_dx <= 1))
  expect_true(all(result$dhs_malaria_dx_low >= 0))
  expect_true(all(result$dhs_malaria_dx_upp <= 1))

  # Count columns are integers
  expect_type(result$dhs_n_febrile, "integer")
  expect_type(result$dhs_n_tested, "integer")

  # Denominator is febrile children (summed across regions)
  expect_equal(sum(result$dhs_n_febrile), sum(febrile))
})

test_that("calc_malaria_dx_dhs_core uses febrile children as denominator", {
  skip_if_not_installed("survey")

  set.seed(123)
  n <- 150

  kr_data <- data.frame(
    v021 = rep(1:15, each = 10),
    v005 = rep(1000000, n),
    v022 = rep(1:3, each = 50),
    hw1 = sample(0:59, n, replace = TRUE),
    h22 = sample(c(0, 1), n, replace = TRUE, prob = c(0.7, 0.3)),
    h47 = NA_real_,
    stringsAsFactors = FALSE
  )

  febrile <- kr_data$h22 == 1
  kr_data$h47[febrile] <- sample(c(0, 1), sum(febrile), replace = TRUE)

  result <- calc_malaria_dx_dhs_core(kr_data)

  # Denominator should be febrile children only
  expect_equal(result$dhs_n_febrile, sum(febrile))
  expect_true(result$dhs_n_tested <= result$dhs_n_febrile)
})

test_that("calc_malaria_dx_dhs_core works with region_var", {
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
    h47 = NA_real_,
    stringsAsFactors = FALSE
  )

  febrile <- kr_data$h22 == 1
  kr_data$h47[febrile] <- sample(c(0, 1), sum(febrile), replace = TRUE)

  result <- calc_malaria_dx_dhs_core(kr_data, region_var = "v024")

  expect_s3_class(result, "tbl_df")
  expect_true("v024" %in% names(result))
  expect_equal(nrow(result), 2)
  expect_equal(sort(result$v024), c("REGION1", "REGION2"))
})

test_that("calc_malaria_dx_dhs returns list with data, dict, metadata", {
  skip_if_not_installed("survey")

  set.seed(111)
  n <- 200

  kr_data <- data.frame(
    v021 = rep(1:20, each = 10),
    v005 = rep(1000000, n),
    v022 = rep(1:4, each = 50),
    hw1 = sample(0:59, n, replace = TRUE),
    h22 = sample(c(0, 1), n, replace = TRUE, prob = c(0.6, 0.4)),
    h47 = NA_real_,
    stringsAsFactors = FALSE
  )

  febrile <- kr_data$h22 == 1
  kr_data$h47[febrile] <- sample(c(0, 1), sum(febrile), replace = TRUE)

  result <- calc_malaria_dx_dhs(kr_data)

  expect_type(result, "list")
  expect_named(result, c("data", "dict", "metadata"))
  expect_s3_class(result$data, "tbl_df")
  expect_type(result$metadata, "list")
  expect_equal(result$metadata$analysis_type, "Malaria Diagnostic Testing")
  expect_equal(result$metadata$cascade_step, 2L)
})

test_that("calc_malaria_dx_dhs_core errors when no febrile children", {
  skip_if_not_installed("survey")

  kr_data <- data.frame(
    v021 = rep(1:5, each = 10),
    v005 = rep(1000000, 50),
    v022 = rep(1, 50),
    hw1 = sample(0:59, 50, replace = TRUE),
    h22 = rep(0, 50),
    h47 = rep(1, 50),
    stringsAsFactors = FALSE
  )

  expect_error(
    calc_malaria_dx_dhs_core(kr_data),
    "No children with fever"
  )
})
