test_that("calc_fever_dhs_core validates input data", {
  expect_error(
    calc_fever_dhs_core("not a dataframe"),
    "must be a data.frame"
  )

  expect_error(
    calc_fever_dhs_core(data.frame()),
    "is empty"
  )

  kr_data <- data.frame(
    v021 = 1:10,
    v005 = rep(1000000, 10)
  )

  expect_error(
    calc_fever_dhs_core(
      kr_data,
      survey_vars = list(
        cluster = "v021",
        weight = "v005",
        stratum = "v022",
        age = "hw1",
        fever = "h22",
        alive = "b5"
      )
    ),
    "Required variables not found"
  )
})

test_that("calc_fever_dhs_core calculates fever prevalence", {
  skip_if_not_installed("survey")

  set.seed(42)
  n <- 200

  kr_data <- data.frame(
    v021 = rep(1:20, each = 10),
    v005 = rep(1000000, n),
    v022 = rep(1:4, each = 50),
    v024 = rep(c("REGION1", "REGION2"), each = 100),
    hw1 = sample(0:59, n, replace = TRUE),
    h22 = sample(c(0, 1), n, replace = TRUE, prob = c(0.7, 0.3)),
    b5 = rep(1, n),
    stringsAsFactors = FALSE
  )

  result <- calc_fever_dhs_core(kr_data)

  expect_s3_class(result, "tbl_df")
  expect_true("dhs_fever" %in% names(result))
  expect_true("dhs_fever_low" %in% names(result))
  expect_true("dhs_fever_upp" %in% names(result))
  expect_true("dhs_n_children" %in% names(result))
  expect_true("dhs_n_fever" %in% names(result))

  # Proportions between 0 and 1
  expect_true(all(result$dhs_fever >= 0 & result$dhs_fever <= 1))
  expect_true(all(result$dhs_fever_low >= 0))
  expect_true(all(result$dhs_fever_upp <= 1))

  # CI ordering
  expect_true(all(result$dhs_fever_low <= result$dhs_fever))
  expect_true(all(result$dhs_fever_upp >= result$dhs_fever))

  # Count columns are integers
  expect_type(result$dhs_n_children, "integer")
  expect_type(result$dhs_n_fever, "integer")
})

test_that("calc_fever_dhs_core uses ALL U5 children as denominator", {
  skip_if_not_installed("survey")

  set.seed(123)
  n <- 100

  kr_data <- data.frame(
    v021 = rep(1:10, each = 10),
    v005 = rep(1000000, n),
    v022 = rep(1:2, each = 50),
    hw1 = sample(0:59, n, replace = TRUE),
    h22 = sample(c(0, 1), n, replace = TRUE, prob = c(0.7, 0.3)),
    b5 = rep(1, n),
    stringsAsFactors = FALSE
  )

  result <- calc_fever_dhs_core(kr_data)

  # Denominator should be ALL U5 children, not just febrile
  expect_equal(result$dhs_n_children, n)
  expect_equal(result$dhs_n_fever, sum(kr_data$h22 == 1))
})

test_that("calc_fever_dhs_core filters to alive children", {
  skip_if_not_installed("survey")

  set.seed(456)
  n <- 100

  kr_data <- data.frame(
    v021 = rep(1:10, each = 10),
    v005 = rep(1000000, n),
    v022 = rep(1:2, each = 50),
    hw1 = sample(0:59, n, replace = TRUE),
    h22 = sample(c(0, 1), n, replace = TRUE, prob = c(0.7, 0.3)),
    b5 = sample(c(0, 1), n, replace = TRUE, prob = c(0.2, 0.8)),
    stringsAsFactors = FALSE
  )

  result <- calc_fever_dhs_core(kr_data)

  # Should only include alive children
  n_alive <- sum(kr_data$b5 == 1)
  expect_equal(result$dhs_n_children, n_alive)
})

test_that("calc_fever_dhs_core works with region_var", {
  skip_if_not_installed("survey")

  set.seed(789)
  n <- 200

  kr_data <- data.frame(
    v021 = rep(1:20, each = 10),
    v005 = rep(1000000, n),
    v022 = rep(1:4, each = 50),
    v024 = rep(c("REGION1", "REGION2"), each = 100),
    hw1 = sample(0:59, n, replace = TRUE),
    h22 = sample(c(0, 1), n, replace = TRUE, prob = c(0.7, 0.3)),
    b5 = rep(1, n),
    stringsAsFactors = FALSE
  )

  result <- calc_fever_dhs_core(kr_data, region_var = "v024")

  expect_s3_class(result, "tbl_df")
  expect_true("v024" %in% names(result))
  expect_equal(nrow(result), 2)
  expect_equal(sort(result$v024), c("REGION1", "REGION2"))

  # Each region should have correct children count
  expect_equal(sum(result$dhs_n_children), n)
})

test_that("calc_fever_dhs returns list with data, dict, metadata", {
  skip_if_not_installed("survey")

  set.seed(111)
  n <- 200

  kr_data <- data.frame(
    v021 = rep(1:20, each = 10),
    v005 = rep(1000000, n),
    v022 = rep(1:4, each = 50),
    v024 = rep(c("REGION1", "REGION2"), each = 100),
    hw1 = sample(0:59, n, replace = TRUE),
    h22 = sample(c(0, 1), n, replace = TRUE, prob = c(0.7, 0.3)),
    b5 = rep(1, n),
    stringsAsFactors = FALSE
  )

  result <- calc_fever_dhs(kr_data, region_var = "v024")

  expect_type(result, "list")
  expect_named(result, c("data", "dict", "metadata"))
  expect_s3_class(result$data, "tbl_df")
  expect_s3_class(result$dict, "data.frame")
  expect_type(result$metadata, "list")
  expect_equal(result$metadata$analysis_type, "Fever Prevalence")
  expect_equal(result$metadata$cascade_step, 0L)
})

test_that("calc_fever_dhs_core errors when no children found", {
  skip_if_not_installed("survey")

  # Create data with all children older than 59 months
  kr_data <- data.frame(
    v021 = rep(1:5, each = 10),
    v005 = rep(1000000, 50),
    v022 = rep(1, 50),
    hw1 = rep(70, 50),
    h22 = rep(1, 50),
    b5 = rep(1, 50),
    stringsAsFactors = FALSE
  )

  expect_error(
    calc_fever_dhs_core(kr_data),
    "No alive children under 5"
  )
})
