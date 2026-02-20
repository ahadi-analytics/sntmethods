test_that("calc_epi_dhs_core validates input data", {
  expect_error(
    calc_epi_dhs_core("not a dataframe"),
    "must be a data.frame"
  )

  expect_error(
    calc_epi_dhs_core(data.frame()),
    "is empty"
  )
})

test_that("calc_epi_dhs_core calculates correct vaccination coverage", {
  skip_if_not_installed("survey")

  # 10 children aged 12-23 months
  kr_data <- data.frame(
    v021 = 1:10,
    v005 = rep(1000000, 10),
    v022 = rep(1, 10),
    v024 = rep("REGION1", 10),
    hw1 = rep(18, 10),         # All 18 months old
    h2 = c(1, 1, 1, 1, 1, 1, 1, 1, 0, 0),  # BCG: 8/10
    h5 = c(1, 1, 1, 1, 1, 0, 0, 0, 0, 0),  # DPT3: 5/10
    h9 = c(1, 1, 1, 1, 1, 1, 1, 0, 0, 0)   # Measles1: 7/10
  )

  result <- calc_epi_dhs_core(
    kr_data,
    indicators = c("bcg", "dpt3", "measles1")
  )

  expect_s3_class(result, "tbl_df")
  expect_true("dhs_epi_bcg" %in% names(result))
  expect_true("dhs_epi_dpt3" %in% names(result))
  expect_true("dhs_epi_measles1" %in% names(result))
  expect_true("dhs_n_epi_eligible" %in% names(result))

  expect_equal(result$dhs_epi_bcg, 0.80)
  expect_equal(result$dhs_epi_dpt3, 0.50)
  expect_equal(result$dhs_epi_measles1, 0.70)
  expect_equal(result$dhs_n_epi_eligible, 10L)
})

test_that("calc_epi_dhs_core calculates fully_vaccinated", {
  skip_if_not_installed("survey")

  # 10 children: 3 fully vaccinated (BCG + DPT3 + Polio3 + Measles1)
  kr_data <- data.frame(
    v021 = 1:10,
    v005 = rep(1000000, 10),
    v022 = rep(1, 10),
    v024 = rep("REGION1", 10),
    hw1 = rep(18, 10),
    h2 = c(1, 1, 1, 1, 1, 0, 0, 0, 0, 0),  # BCG
    h5 = c(1, 1, 1, 0, 0, 1, 0, 0, 0, 0),  # DPT3
    h8 = c(1, 1, 1, 1, 0, 0, 1, 0, 0, 0),  # Polio3
    h9 = c(1, 1, 1, 1, 1, 1, 1, 0, 0, 0)   # Measles1
  )

  result <- calc_epi_dhs_core(
    kr_data,
    indicators = "fully_vaccinated"
  )

  expect_true("dhs_epi_fully_vaccinated" %in% names(result))
  # Only first 3 have all 4 vaccines
  expect_equal(result$dhs_epi_fully_vaccinated, 0.30)
})

test_that("calc_epi_dhs_core filters by age range", {
  skip_if_not_installed("survey")

  kr_data <- data.frame(
    v021 = 1:10,
    v005 = rep(1000000, 10),
    v022 = rep(1, 10),
    v024 = rep("REGION1", 10),
    # Ages: 5 eligible (12-23 months) + 5 ineligible
    hw1 = c(12, 15, 18, 20, 23, 6, 8, 24, 36, 48),
    h2 = rep(1, 10)  # All vaccinated
  )

  result <- calc_epi_dhs_core(kr_data, indicators = "bcg")

  expect_equal(result$dhs_n_epi_eligible, 5L)
})

test_that("calc_epi_dhs_core recognizes DHS vaccination codes", {
  skip_if_not_installed("survey")

  # DHS codes: 1=card, 2=mother report, 3=both → all count as vaccinated
  kr_data <- data.frame(
    v021 = 1:6,
    v005 = rep(1000000, 6),
    v022 = rep(1, 6),
    v024 = rep("REGION1", 6),
    hw1 = rep(18, 6),
    h2 = c(1, 2, 3, 0, NA, 8)  # 3 vaccinated, 3 not
  )

  result <- calc_epi_dhs_core(kr_data, indicators = "bcg")

  expect_equal(result$dhs_epi_bcg, 0.50)
})

test_that("calc_epi_dhs returns list with metadata", {
  skip_if_not_installed("survey")
  skip_if_not_installed("sntutils")

  kr_data <- data.frame(
    v021 = rep(1:5, each = 10),
    v005 = rep(1000000, 50),
    v022 = rep(1:2, each = 25),
    v024 = rep("REGION1", 50),
    hw1 = sample(12:23, 50, replace = TRUE),
    h2 = sample(c(0, 1), 50, replace = TRUE),
    h5 = sample(c(0, 1), 50, replace = TRUE),
    h9 = sample(c(0, 1), 50, replace = TRUE)
  )

  result <- calc_epi_dhs(kr_data, indicators = c("bcg", "dpt3", "measles1"))

  expect_type(result, "list")
  expect_named(result, c("data", "dict", "metadata"))
  expect_equal(result$metadata$analysis_type, "EPI (Expanded Programme on Immunization)")
  expect_equal(result$metadata$file_type, "KR")
})
