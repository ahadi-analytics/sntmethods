# Helper to create a complete mock KR dataset with all cascade variables
.make_cascade_kr_data <- function(n = 300, seed = 42) {
  set.seed(seed)

  kr_data <- data.frame(
    v021 = rep(1:30, each = 10),
    v005 = rep(1000000, n),
    v022 = rep(1:6, each = 50),
    v024 = rep(c("REGION1", "REGION2", "REGION3"), each = 100),
    hw1 = sample(0:59, n, replace = TRUE),
    h22 = sample(c(0, 1), n, replace = TRUE, prob = c(0.65, 0.35)),
    b5 = rep(1, n),
    h47 = NA_real_,
    ml13a = NA_real_,
    ml13b = NA_real_,
    ml13e = NA_real_,
    stringsAsFactors = FALSE
  )

  # Add h32 sources for CSB (public and private)
  febrile <- kr_data$h22 == 1
  kr_data$h32a <- NA_real_
  kr_data$h32j <- NA_real_
  kr_data$h32a[febrile] <- sample(
    c(0, 1), sum(febrile), replace = TRUE, prob = c(0.5, 0.5)
  )
  kr_data$h32j[febrile] <- sample(
    c(0, 1), sum(febrile), replace = TRUE, prob = c(0.7, 0.3)
  )

  # h47: blood taken for malaria test
  kr_data$h47[febrile] <- sample(
    c(0, 1), sum(febrile), replace = TRUE, prob = c(0.5, 0.5)
  )

  # ml13 antimalarial variables
  kr_data$ml13a[febrile] <- sample(
    c(0, 1), sum(febrile), replace = TRUE, prob = c(0.85, 0.15)
  )
  kr_data$ml13b[febrile] <- sample(
    c(0, 1), sum(febrile), replace = TRUE, prob = c(0.9, 0.1)
  )
  kr_data$ml13e[febrile] <- sample(
    c(0, 1), sum(febrile), replace = TRUE, prob = c(0.75, 0.25)
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

test_that("calc_case_management_dhs validates step names", {
  kr_data <- .make_cascade_kr_data()

  expect_error(
    calc_case_management_dhs(kr_data, steps = c("fever", "invalid_step")),
    "Invalid cascade steps"
  )
})

test_that("calc_case_management_dhs produces complete cascade", {
  skip_if_not_installed("survey")

  kr_data <- .make_cascade_kr_data()

  # Use region_var to get predictable grouping (3 regions)
  result <- calc_case_management_dhs(kr_data, region_var = "v024")

  expect_type(result, "list")
  expect_named(result, c("cascade", "data", "dict", "metadata"))

  # Check cascade table structure
  cascade <- result$cascade
  expect_s3_class(cascade, "tbl_df")
  expect_true(all(c("step", "indicator", "estimate", "low", "upp",
                     "n_eligible", "n_positive") %in% names(cascade)))

  # Should have 5 steps x 3 regions = 15 rows
  expect_equal(nrow(cascade), 15)

  # Each region should have steps 0-4
  for (region in unique(cascade$v024)) {
    region_data <- cascade[cascade$v024 == region, ]
    expect_equal(region_data$step, 0:4)
  }

  # Indicators should be in order within each region
  expect_equal(
    unique(cascade$indicator),
    c("fever", "sought_care", "tested", "any_antimalarial", "received_act")
  )

  # All estimates between 0 and 1
  expect_true(all(cascade$estimate >= 0 & cascade$estimate <= 1))

  # Check wide-format data
  expect_s3_class(result$data, "tbl_df")

  # Check metadata
  expect_equal(result$metadata$analysis_type, "Case Management Cascade")
  expect_equal(result$metadata$n_steps, 5)
})

test_that("calc_case_management_dhs cascade has valid proportions", {
  skip_if_not_installed("survey")

  kr_data <- .make_cascade_kr_data()

  result <- calc_case_management_dhs(kr_data)
  cascade <- result$cascade

  # All estimates should be between 0 and 1
  expect_true(all(cascade$estimate >= 0 & cascade$estimate <= 1))
  expect_true(all(cascade$low >= 0))
  expect_true(all(cascade$upp <= 1))

  # CI ordering
  expect_true(all(cascade$low <= cascade$estimate))
  expect_true(all(cascade$upp >= cascade$estimate))

  # n_positive <= n_eligible
  expect_true(all(cascade$n_positive <= cascade$n_eligible))
})

test_that("calc_case_management_dhs works with region_var", {
  skip_if_not_installed("survey")

  kr_data <- .make_cascade_kr_data()

  result <- calc_case_management_dhs(kr_data, region_var = "v024")
  cascade <- result$cascade

  expect_true("v024" %in% names(cascade))

  # Should have 5 steps x 3 regions = 15 rows
  expect_equal(nrow(cascade), 15)

  # Each region should have all 5 steps
  for (region in c("REGION1", "REGION2", "REGION3")) {
    region_cascade <- cascade[cascade$v024 == region, ]
    expect_equal(nrow(region_cascade), 5)
    expect_equal(region_cascade$step, 0:4)
  }

  # Wide data should have 3 rows (one per region)
  expect_equal(nrow(result$data), 3)
})

test_that("calc_case_management_dhs supports subset of steps", {
  skip_if_not_installed("survey")

  kr_data <- .make_cascade_kr_data()

  result <- calc_case_management_dhs(
    kr_data,
    region_var = "v024",
    steps = c("fever", "received_act")
  )

  cascade <- result$cascade
  # 2 steps x 3 regions = 6 rows
  expect_equal(nrow(cascade), 6)
  expect_equal(unique(cascade$indicator), c("fever", "received_act"))
})

test_that("calc_case_management_dhs skips steps with missing variables", {
  skip_if_not_installed("survey")

  set.seed(42)
  n <- 200

  # Create data without h47 (malaria_dx)
  kr_data <- data.frame(
    v021 = rep(1:20, each = 10),
    v005 = rep(1000000, n),
    v022 = rep(1:4, each = 50),
    hw1 = sample(0:59, n, replace = TRUE),
    h22 = sample(c(0, 1), n, replace = TRUE, prob = c(0.65, 0.35)),
    b5 = rep(1, n),
    ml13a = NA_real_,
    ml13e = NA_real_,
    stringsAsFactors = FALSE
  )

  febrile <- kr_data$h22 == 1
  kr_data$h32a <- NA_real_
  kr_data$h32j <- NA_real_
  kr_data$h32a[febrile] <- sample(c(0, 1), sum(febrile), replace = TRUE)
  kr_data$h32j[febrile] <- sample(c(0, 1), sum(febrile), replace = TRUE)
  kr_data$ml13a[febrile] <- sample(c(0, 1), sum(febrile), replace = TRUE)
  kr_data$ml13e[febrile] <- sample(c(0, 1), sum(febrile), replace = TRUE)

  # Should succeed but skip the "tested" step
  result <- calc_case_management_dhs(kr_data)

  cascade <- result$cascade
  # "tested" step should be missing since h47 is not in the data
  expect_false("tested" %in% cascade$indicator)
  # Other steps should still be present
  expect_true("fever" %in% cascade$indicator)
  expect_true("received_act" %in% cascade$indicator)
})

test_that("calc_case_management_dhs wide data has correct columns", {
  skip_if_not_installed("survey")

  kr_data <- .make_cascade_kr_data()

  result <- calc_case_management_dhs(kr_data)
  wide <- result$data

  # Should have columns from all steps

  expect_true("dhs_fever" %in% names(wide))
  expect_true("dhs_csb_any" %in% names(wide))
  expect_true("dhs_malaria_dx" %in% names(wide))
  expect_true("dhs_antimalarial" %in% names(wide))
  expect_true("dhs_act" %in% names(wide))
})

test_that("calc_case_management_dhs dict is valid", {
  skip_if_not_installed("survey")

  kr_data <- .make_cascade_kr_data()

  result <- calc_case_management_dhs(kr_data)

  expect_s3_class(result$dict, "data.frame")
  expect_true(nrow(result$dict) > 0)
})
