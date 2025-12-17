test_that("calc_wealth_dhs returns list with data, dict, and metadata", {
  mock_hr <- data.frame(
    hv001 = rep(1:2, each = 10),
    hv005 = 1000000,
    hv022 = 1,
    hv024 = rep(1:2, each = 10),
    hv270 = c(
      1, 1, 2, 2, 3, 3, 4, 4, 5, 5,
      1, 2, 3, 4, 5, 1, 2, 3, 4, 5
    ),
    hv271 = rnorm(20, mean = 0, sd = 1),
    hv012 = sample(3:8, 20, replace = TRUE),
    hv000 = "SL7",
    hv007 = 2019
  )

  result <- calc_wealth_dhs(dhs_hr = mock_hr)

  expect_type(result, "list")
  expect_setequal(names(result), c("data", "dict", "metadata"))

  expect_true("dhs_prop_poorest" %in% names(result$data))
  expect_true("dhs_prop_richest" %in% names(result$data))
  expect_true("dhs_dominant_quintile" %in% names(result$data))
  expect_true("dhs_gini" %in% names(result$data))

  expect_equal(result$metadata$country_code, "SL7")
  expect_equal(result$metadata$survey_year, 2019)
  expect_equal(result$metadata$file_type, "HR")
})

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

test_that("calc_wealth_dhs calculates correct proportions", {
  mock_hr <- data.frame(
    hv001 = rep(1, 20),
    hv005 = 1000000,
    hv022 = 1,
    hv024 = 1,
    hv270 = c(rep(1, 4), rep(2, 4), rep(3, 4), rep(4, 4), rep(5, 4)),
    hv271 = rnorm(20),
    hv012 = 4,
    hv000 = "SL7",
    hv007 = 2019
  )

  result <- calc_wealth_dhs(dhs_hr = mock_hr)

  # Proportions are 0-1 scale
  expect_equal(result$data$dhs_prop_poorest, 0.20)
  expect_equal(result$data$dhs_prop_poorer, 0.20)
  expect_equal(result$data$dhs_prop_middle, 0.20)
  expect_equal(result$data$dhs_prop_richer, 0.20)
  expect_equal(result$data$dhs_prop_richest, 0.20)
})

test_that("calc_wealth_dhs identifies dominant quintile correctly", {
  mock_hr <- data.frame(
    hv001 = rep(1, 20),
    hv005 = 1000000,
    hv022 = 1,
    hv024 = 1,
    hv270 = c(rep(1, 10), rep(2, 3), rep(3, 3), rep(4, 2), rep(5, 2)),
    hv271 = rnorm(20),
    hv012 = 4,
    hv000 = "SL7",
    hv007 = 2019
  )

  result <- calc_wealth_dhs(dhs_hr = mock_hr)

  expect_equal(as.character(result$data$dhs_dominant_quintile), "Poorest")
  expect_s3_class(result$data$dhs_dominant_quintile, "ordered")
})

test_that("calc_wealth_dhs works with cluster-level GPS data", {
  mock_hr <- data.frame(
    hv001 = rep(1:3, each = 10),
    hv005 = 1000000,
    hv022 = 1,
    hv024 = 1,
    hv270 = sample(1:5, 30, replace = TRUE),
    hv271 = rnorm(30),
    hv012 = 4,
    hv000 = "SL7",
    hv007 = 2019
  )

  mock_gps <- data.frame(
    DHSCLUST = 1:3,
    LATNUM = c(8.5, 8.6, 8.7),
    LONGNUM = c(-11.5, -11.4, -11.3)
  )

  result <- calc_wealth_dhs(dhs_hr = mock_hr, gps_data = mock_gps)

  expect_equal(nrow(result$data), 3)
  expect_true("cluster_id" %in% names(result$data))
  expect_true("lat" %in% names(result$data))
  expect_true("lon" %in% names(result$data))
  expect_equal(result$metadata$aggregation_level, "cluster")
})

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

test_that("calc_wealth_dhs outputs only key columns", {
  mock_hr <- data.frame(
    hv001 = rep(1:2, each = 15),
    hv005 = 1000000,
    hv022 = 1,
    hv024 = rep(1:2, each = 15),
    hv270 = sample(1:5, 30, replace = TRUE),
    hv271 = rnorm(30),
    hv012 = 4,
    hv000 = "SL7",
    hv007 = 2019
  )

  result <- calc_wealth_dhs(dhs_hr = mock_hr)

  expected_cols <- c(
    "adm1",
    "dhs_prop_poorest",
    "dhs_prop_poorer",
    "dhs_prop_middle",
    "dhs_prop_richer",
    "dhs_prop_richest",
    "dhs_dominant_quintile",
    "dhs_dominant_prop",
    "dhs_gini"
  )

  expect_setequal(names(result$data), expected_cols)

  expect_false("dhs_n_households" %in% names(result$data))
  expect_false("dhs_weighted_households" %in% names(result$data))
  expect_false("dhs_gini_sample_size" %in% names(result$data))
  expect_false("dhs_gini_reliable" %in% names(result$data))
})
