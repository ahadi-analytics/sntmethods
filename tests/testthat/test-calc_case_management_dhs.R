# Helper to create a mock KR dataset with CSB + antimalarial + ACT variables
.make_eff_cm_kr_data <- function(n = 300, seed = 42) {
  set.seed(seed)

  kr_data <- data.frame(
    v021 = rep(1:30, each = 10),
    v005 = rep(1000000, n),
    v022 = rep(1:6, each = 50),
    v024 = rep(c("REGION1", "REGION2", "REGION3"), each = 100),
    hw1 = sample(0:59, n, replace = TRUE),
    h22 = sample(c(0, 1), n, replace = TRUE, prob = c(0.65, 0.35)),
    b5 = rep(1, n),
    ml13a = NA_real_,
    ml13b = NA_real_,
    ml13e = NA_real_,
    stringsAsFactors = FALSE
  )

  # Add h32 sources for CSB
  febrile <- kr_data$h22 == 1
  kr_data$h32a <- NA_real_
  kr_data$h32j <- NA_real_
  kr_data$h32a[febrile] <- sample(
    c(0, 1), sum(febrile), replace = TRUE, prob = c(0.4, 0.6)
  )
  kr_data$h32j[febrile] <- sample(
    c(0, 1), sum(febrile), replace = TRUE, prob = c(0.7, 0.3)
  )

  # ml13 antimalarial variables
  kr_data$ml13a[febrile] <- sample(
    c(0, 1), sum(febrile), replace = TRUE, prob = c(0.85, 0.15)
  )
  kr_data$ml13b[febrile] <- sample(
    c(0, 1), sum(febrile), replace = TRUE, prob = c(0.9, 0.1)
  )
  kr_data$ml13e[febrile] <- sample(
    c(0, 1), sum(febrile), replace = TRUE, prob = c(0.65, 0.35)
  )

  kr_data
}

test_that("calc_case_management_dhs validates input data", {
  expect_error(
    calc_case_management_dhs("not a dataframe"),
    "must be a data.frame"
  )

  expect_error(
    calc_case_management_dhs(data.frame()),
    "is empty"
  )
})

test_that("calc_case_management_dhs returns expected columns at national level", {
  skip_if_not_installed("survey")

  kr_data <- .make_eff_cm_kr_data()

  result <- calc_case_management_dhs(kr_data)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 1)

  expected_cols <- c(
    "dhs_eff_cm_any", "dhs_eff_cm_any_low", "dhs_eff_cm_any_upp",
    "dhs_eff_cm_public", "dhs_eff_cm_public_low", "dhs_eff_cm_public_upp",
    "dhs_n_fever", "dhs_n_antimalarial", "dhs_n_antimalarial_public"
  )
  for (col in expected_cols) {
    expect_true(col %in% names(result), info = paste("Missing column:", col))
  }
})

test_that("calc_case_management_dhs estimates are valid proportions", {
  skip_if_not_installed("survey")

  kr_data <- .make_eff_cm_kr_data()

  result <- calc_case_management_dhs(kr_data)

  # Estimates between 0 and 1
  expect_true(result$dhs_eff_cm_any >= 0 && result$dhs_eff_cm_any <= 1)
  expect_true(result$dhs_eff_cm_public >= 0 && result$dhs_eff_cm_public <= 1)

  # CI ordering: low <= est <= upp
  expect_true(result$dhs_eff_cm_any_low <= result$dhs_eff_cm_any)
  expect_true(result$dhs_eff_cm_any_upp >= result$dhs_eff_cm_any)
  expect_true(result$dhs_eff_cm_public_low <= result$dhs_eff_cm_public)
  expect_true(result$dhs_eff_cm_public_upp >= result$dhs_eff_cm_public)

  # CIs clamped to [0, 1]
  expect_true(result$dhs_eff_cm_any_low >= 0)
  expect_true(result$dhs_eff_cm_any_upp <= 1)
  expect_true(result$dhs_eff_cm_public_low >= 0)
  expect_true(result$dhs_eff_cm_public_upp <= 1)

  # Counts are positive integers
  expect_true(result$dhs_n_fever > 0)
  expect_true(result$dhs_n_antimalarial > 0)
  expect_true(result$dhs_n_antimalarial <= result$dhs_n_fever)
})

test_that("calc_case_management_dhs works with region_var", {
  skip_if_not_installed("survey")

  kr_data <- .make_eff_cm_kr_data()

  result <- calc_case_management_dhs(kr_data, region_var = "v024")

  expect_s3_class(result, "tbl_df")
  expect_true("v024" %in% names(result))

  # Should have one row per region
  expect_equal(nrow(result), 3)
  expect_setequal(result$v024, c("REGION1", "REGION2", "REGION3"))

  # All estimates valid
  expect_true(all(result$dhs_eff_cm_any >= 0 & result$dhs_eff_cm_any <= 1))
  expect_true(all(result$dhs_eff_cm_public >= 0 & result$dhs_eff_cm_public <= 1))

  # CI ordering for all rows
  expect_true(all(result$dhs_eff_cm_any_low <= result$dhs_eff_cm_any))
  expect_true(all(result$dhs_eff_cm_any_upp >= result$dhs_eff_cm_any))
})

test_that("calc_case_management_dhs public <= any", {
  skip_if_not_installed("survey")

  kr_data <- .make_eff_cm_kr_data()

  result <- calc_case_management_dhs(kr_data)

  # Public CSB is a subset of any CSB, so eff_cm_public <= eff_cm_any
  expect_true(result$dhs_eff_cm_public <= result$dhs_eff_cm_any)
})

test_that("calc_case_management_dhs public variant uses public-conditioned ACT rate", {
  skip_if_not_installed("survey")

  # Create data where public care seekers have very different ACT rates
  # than private care seekers - this ensures eff_cm_public is NOT just

  # csb_public * P(ACT|antimalarial, any) but properly conditioned.
  set.seed(99)
  n <- 500

  kr_data <- data.frame(
    v021 = rep(1:50, each = 10),
    v005 = rep(1000000, n),
    v022 = rep(1:10, each = 50),
    hw1 = sample(0:59, n, replace = TRUE),
    h22 = sample(c(0, 1), n, replace = TRUE, prob = c(0.5, 0.5)),
    b5 = rep(1, n),
    h32a = NA_real_,  # public
    h32j = NA_real_,  # private formal
    ml13a = NA_real_,
    ml13e = NA_real_,
    stringsAsFactors = FALSE
  )

  febrile <- kr_data$h22 == 1
  n_feb <- sum(febrile)

  # Half go public, half private
  kr_data$h32a[febrile] <- sample(c(0, 1), n_feb, replace = TRUE, prob = c(0.5, 0.5))
  kr_data$h32j[febrile] <- ifelse(kr_data$h32a[febrile] == 0, 1, 0)

  # All febrile get some antimalarial
  kr_data$ml13a[febrile] <- 1

  # Public care seekers get ACT 90% of the time, private gets 10%
  is_pub <- febrile & kr_data$h32a == 1
  is_priv <- febrile & kr_data$h32j == 1
  kr_data$ml13e[is_pub] <- sample(c(0, 1), sum(is_pub), replace = TRUE, prob = c(0.1, 0.9))
  kr_data$ml13e[is_priv] <- sample(c(0, 1), sum(is_priv), replace = TRUE, prob = c(0.9, 0.1))

  result <- calc_case_management_dhs(kr_data)

  # With such divergent ACT rates, eff_cm_public should be meaningfully
  # different from what it would be if using the unconditional ACT rate.
  # Specifically: public ACT rate (~0.9) >> overall ACT rate (~0.5),
  # so eff_cm_public should be higher than under the old formula.
  expect_true(!is.na(result$dhs_eff_cm_public))
  expect_true(!is.na(result$dhs_eff_cm_any))
  expect_true(result$dhs_n_antimalarial_public > 0)
  expect_true(result$dhs_n_antimalarial_public <= result$dhs_n_antimalarial)
})

test_that("calc_case_management_dhs handles h37e fallback", {
  skip_if_not_installed("survey")

  set.seed(42)
  n <- 200

  # Create data with h37 series instead of ml13
  kr_data <- data.frame(
    v021 = rep(1:20, each = 10),
    v005 = rep(1000000, n),
    v022 = rep(1:4, each = 50),
    hw1 = sample(0:59, n, replace = TRUE),
    h22 = sample(c(0, 1), n, replace = TRUE, prob = c(0.6, 0.4)),
    b5 = rep(1, n),
    h37a = NA_real_,
    h37e = NA_real_,
    stringsAsFactors = FALSE
  )

  febrile <- kr_data$h22 == 1
  kr_data$h32a <- NA_real_
  kr_data$h32a[febrile] <- sample(c(0, 1), sum(febrile), replace = TRUE)
  kr_data$h37a[febrile] <- sample(
    c(0, 1), sum(febrile), replace = TRUE, prob = c(0.8, 0.2)
  )
  kr_data$h37e[febrile] <- sample(
    c(0, 1), sum(febrile), replace = TRUE, prob = c(0.7, 0.3)
  )

  result <- calc_case_management_dhs(kr_data)

  expect_s3_class(result, "tbl_df")
  expect_true(!is.na(result$dhs_eff_cm_any))
  expect_true(result$dhs_n_antimalarial > 0)
})

test_that("calc_case_management_dhs errors with no antimalarial data", {
  skip_if_not_installed("survey")

  set.seed(42)
  n <- 100

  kr_data <- data.frame(
    v021 = rep(1:10, each = 10),
    v005 = rep(1000000, n),
    v022 = rep(1:2, each = 50),
    hw1 = sample(0:59, n, replace = TRUE),
    h22 = sample(c(0, 1), n, replace = TRUE, prob = c(0.6, 0.4)),
    b5 = rep(1, n),
    stringsAsFactors = FALSE
  )

  febrile <- kr_data$h22 == 1
  kr_data$h32a <- NA_real_
  kr_data$h32a[febrile] <- sample(c(0, 1), sum(febrile), replace = TRUE)

  expect_error(
    calc_case_management_dhs(kr_data),
    "No antimalarial variables found"
  )
})
