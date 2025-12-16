test_that("calc_severe_anemia_dhs_core validates input data", {
  # Test with non-dataframe input
  expect_error(
    calc_severe_anemia_dhs_core("not a dataframe"),
    "must be a data.frame"
  )

  # Test with empty dataframe
  expect_error(
    calc_severe_anemia_dhs_core(data.frame()),
    "is empty"
  )

  # Test with missing required columns
  pr_data <- data.frame(
    hv001 = 1:10,
    hv005 = rep(1000000, 10)
  )

  expect_error(
    calc_severe_anemia_dhs_core(
      pr_data,
      survey_vars = list(
        cluster = "hv001",
        weight = "hv005",
        age = "hc1",          # Missing
        hemoglobin = "hc56",  # Missing
        present = "hv103",    # Missing
        mother = "hv042"      # Missing
      )
    ),
    "Columns not found"
  )
})

test_that("calc_severe_anemia_dhs_core calculates correct prevalence", {
  skip_if_not_installed("survey")

  # Create mock PR data with known severe anemia prevalence
  # 10 children, 3 with severe anemia (Hb < 80 in tenths)
  set.seed(123)
  pr_data <- data.frame(
    hv001 = 1:10,           # 10 clusters
    hv005 = rep(1000000, 10),
    hv022 = rep(1, 10),
    hv024 = rep("REGION1", 10),
    hc1 = rep(24, 10),      # All 24 months old
    hc56 = c(60, 70, 75, 85, 90, 95, 100, 110, 120, 130),  # Hb in tenths
    hv103 = rep(1, 10),     # All present
    hv042 = rep(1, 10)      # All have mother listed
  )

  # Children 1, 2, 3 have Hb < 80 (8.0 g/dL) = severe anemia
  # Expected: 30% severe anemia

  # Use altitude_adjusted = FALSE to use raw hc56 values
  result <- calc_severe_anemia_dhs_core(pr_data, altitude_adjusted = FALSE)

  expect_s3_class(result, "tbl_df")
  expect_true("dhs_severe_anemia" %in% names(result))
  expect_true("dhs_severe_anemia_low" %in% names(result))
  expect_true("dhs_severe_anemia_upp" %in% names(result))
  expect_true("dhs_n_tested_hb" %in% names(result))
  expect_true("dhs_n_severe_anemia" %in% names(result))

  # Check values
  expect_equal(result$dhs_severe_anemia, 30)
  expect_equal(result$dhs_n_tested_hb, 10)
  expect_equal(result$dhs_n_severe_anemia, 3)
})

test_that("calc_severe_anemia_dhs_core respects custom threshold", {
  skip_if_not_installed("survey")

  # Create data where threshold matters
  pr_data <- data.frame(
    hv001 = 1:10,
    hv005 = rep(1000000, 10),
    hv022 = rep(1, 10),
    hv024 = rep("REGION1", 10),
    hc1 = rep(24, 10),
    # All Hb values between 70 and 90 (7.0 - 9.0 g/dL)
    hc56 = c(70, 72, 75, 78, 80, 82, 85, 88, 90, 92),
    hv103 = rep(1, 10),
    hv042 = rep(1, 10)
  )

  # With default threshold (8.0): children 1-4 are severe (Hb < 80)
  result_default <- calc_severe_anemia_dhs_core(
    pr_data,
    hb_threshold = 8.0,
    altitude_adjusted = FALSE
  )
  expect_equal(result_default$dhs_n_severe_anemia, 4)

  # With threshold 7.5: only children 1-3 are severe (Hb < 75)
  result_lower <- calc_severe_anemia_dhs_core(
    pr_data,
    hb_threshold = 7.5,
    altitude_adjusted = FALSE
  )
  expect_equal(result_lower$dhs_n_severe_anemia, 2)

  # With threshold 9.0: children 1-8 are severe (Hb < 90)
  result_higher <- calc_severe_anemia_dhs_core(
    pr_data,
    hb_threshold = 9.0,
    altitude_adjusted = FALSE
  )
  expect_equal(result_higher$dhs_n_severe_anemia, 8)
})

test_that("calc_severe_anemia_dhs_core filters by age correctly", {
  skip_if_not_installed("survey")

  # Mix of eligible (6-59 months) and ineligible children
  pr_data <- data.frame(
    hv001 = 1:10,
    hv005 = rep(1000000, 10),
    hv022 = rep(1, 10),
    hv024 = rep("REGION1", 10),
    # Ages: 3, 5, 6, 12, 24, 36, 48, 59, 60, 72 months
    hc1 = c(3, 5, 6, 12, 24, 36, 48, 59, 60, 72),
    hc56 = rep(70, 10),  # All would be severe anemia
    hv103 = rep(1, 10),
    hv042 = rep(1, 10)
  )

  # Only children aged 6-59 months should be included (indices 3-8)
  result <- calc_severe_anemia_dhs_core(pr_data, altitude_adjusted = FALSE)

  expect_equal(result$dhs_n_tested_hb, 6)  # 6 eligible children
  expect_equal(result$dhs_n_severe_anemia, 6)  # All have severe anemia
})

test_that("calc_severe_anemia_dhs returns list with data, dict, and metadata", {
  skip_if_not_installed("survey")
  skip_if_not_installed("sntutils")

  set.seed(456)
  n_children <- 100

  pr_data <- data.frame(
    hv000 = rep("BF8", n_children),
    hv007 = rep(2021, n_children),
    hv001 = rep(1:10, each = 10),
    hv005 = rep(1000000, n_children),
    hv022 = rep(1:2, each = 50),
    hv024 = rep("CENTRE", n_children),
    hc1 = sample(6:59, n_children, replace = TRUE),
    hc56 = sample(50:150, n_children, replace = TRUE),  # Mix of severe and not
    hv103 = rep(1, n_children),
    hv042 = rep(1, n_children)
  )

  result <- calc_severe_anemia_dhs(pr_data, altitude_adjusted = FALSE)

  # Check structure
  expect_type(result, "list")
  expect_named(result, c("data", "dict", "metadata"))

  # Check data
  expect_s3_class(result$data, "tbl_df")
  expect_true("dhs_severe_anemia" %in% names(result$data))

  # Check metadata
  expect_type(result$metadata, "list")
  expect_equal(result$metadata$country_code, "BF8")
  expect_equal(result$metadata$survey_year, 2021)
  expect_equal(result$metadata$file_type, "PR")
  expect_equal(result$metadata$analysis_type, "Severe Anemia (Hb < 8.0 g/dL)")
  expect_equal(result$metadata$age_group, "6-59 months")

  # Check dictionary
  expect_s3_class(result$dict, "data.frame")
  expect_true("variable" %in% names(result$dict))
})

test_that("extract_dhs_metadata_anemia extracts correct metadata", {
  pr_data <- data.frame(
    hv000 = rep("ML8", 150),
    hv007 = rep(2021, 150),
    hv001 = rep(1:15, each = 10),
    hc1 = sample(6:59, 150, replace = TRUE),
    hc56 = sample(50:150, 150, replace = TRUE),
    hv103 = rep(1, 150),
    hv042 = rep(1, 150)
  )

  metadata <- extract_dhs_metadata_anemia(
    pr_data,
    survey_vars = list(
      cluster = "hv001",
      age = "hc1",
      hemoglobin = "hc56"
    ),
    altitude_adjusted = FALSE
  )

  expect_equal(metadata$country_code, "ML8")
  expect_equal(metadata$survey_year, 2021)
  expect_equal(metadata$file_type, "PR")
  expect_equal(metadata$total_records, 150)
  expect_equal(metadata$total_clusters, 15)
  expect_true(metadata$has_hemoglobin)
  expect_equal(metadata$analysis_type, "Severe Anemia (Hb < 8.0 g/dL)")
  expect_false(metadata$altitude_adjusted)
  expect_equal(metadata$hemoglobin_variable, "hc56")
})

test_that("calc_severe_anemia_dhs_core handles missing hemoglobin values", {
  skip_if_not_installed("survey")

  # Mix of children with and without Hb measurements
  pr_data <- data.frame(
    hv001 = 1:10,
    hv005 = rep(1000000, 10),
    hv022 = rep(1, 10),
    hv024 = rep("REGION1", 10),
    hc1 = rep(24, 10),
    # 5 with valid Hb, 5 with NA
    hc56 = c(70, 75, 80, 85, 90, NA, NA, NA, NA, NA),
    hv103 = rep(1, 10),
    hv042 = rep(1, 10)
  )

  result <- calc_severe_anemia_dhs_core(pr_data, altitude_adjusted = FALSE)

  # Only 5 children should be included (those with valid Hb)
  expect_equal(result$dhs_n_tested_hb, 5)
  # Children 1-2 have severe anemia (Hb < 80)
  expect_equal(result$dhs_n_severe_anemia, 2)
})

test_that("calc_severe_anemia_dhs_core produces consistent results", {
  skip_if_not_installed("survey")

  set.seed(999)

  pr_data <- data.frame(
    hv001 = rep(1:5, each = 20),
    hv005 = rep(1000000, 100),
    hv022 = rep(1, 100),
    hv024 = rep("REGION1", 100),
    hc1 = rep(24, 100),
    hc56 = c(rep(70, 30), rep(100, 70)),  # 30% severe anemia
    hv103 = rep(1, 100),
    hv042 = rep(1, 100)
  )

  result1 <- calc_severe_anemia_dhs_core(pr_data, altitude_adjusted = FALSE)
  result2 <- calc_severe_anemia_dhs_core(pr_data, altitude_adjusted = FALSE)

  expect_equal(result1$dhs_severe_anemia, result2$dhs_severe_anemia)
  expect_equal(result1$dhs_n_tested_hb, result2$dhs_n_tested_hb)
  expect_equal(result1$dhs_n_severe_anemia, result2$dhs_n_severe_anemia)
})

# ---- Altitude adjustment tests ----

test_that("calc_severe_anemia_dhs_core uses altitude-adjusted Hb by default", {
  skip_if_not_installed("survey")

  # Data with both raw (hc56) and altitude-adjusted (hw57) hemoglobin
  # Values are different to verify correct variable selection
  pr_data <- data.frame(
    hv001 = 1:10,
    hv005 = rep(1000000, 10),
    hv022 = rep(1, 10),
    hv024 = rep("REGION1", 10),
    hc1 = rep(24, 10),
    # Raw hemoglobin: 2 with severe anemia (Hb < 80)
    hc56 = c(60, 70, 85, 90, 95, 100, 110, 120, 130, 140),
    # Altitude-adjusted: 4 with severe anemia (Hb < 80)
    hw57 = c(60, 65, 70, 75, 90, 95, 100, 110, 120, 130),
    hv103 = rep(1, 10),
    hv042 = rep(1, 10)
  )

  # Default (altitude_adjusted = TRUE) should use hw57
  result_default <- calc_severe_anemia_dhs_core(pr_data)
  expect_equal(result_default$dhs_n_severe_anemia, 4)

  # Explicitly set altitude_adjusted = FALSE should use hc56
  result_raw <- calc_severe_anemia_dhs_core(pr_data, altitude_adjusted = FALSE)
  expect_equal(result_raw$dhs_n_severe_anemia, 2)
})

test_that("calc_severe_anemia_dhs_core errors when altitude-adjusted var missing", {
  skip_if_not_installed("survey")

  # Data without hw57 (altitude-adjusted variable)
  pr_data <- data.frame(
    hv001 = 1:10,
    hv005 = rep(1000000, 10),
    hv022 = rep(1, 10),
    hv024 = rep("REGION1", 10),
    hc1 = rep(24, 10),
    hc56 = c(60, 70, 85, 90, 95, 100, 110, 120, 130, 140),
    hv103 = rep(1, 10),
    hv042 = rep(1, 10)
  )

  # Default (altitude_adjusted = TRUE) should error because hw57 is missing
  expect_error(
    calc_severe_anemia_dhs_core(pr_data),
    "Altitude-adjusted hemoglobin variable"
  )

  # Should work with altitude_adjusted = FALSE
  result <- calc_severe_anemia_dhs_core(pr_data, altitude_adjusted = FALSE)
  expect_s3_class(result, "tbl_df")
})

test_that("calc_severe_anemia_dhs metadata includes altitude adjustment info", {
  skip_if_not_installed("survey")
  skip_if_not_installed("sntutils")

  set.seed(789)
  n_children <- 50

  pr_data <- data.frame(
    hv000 = rep("BF8", n_children),
    hv007 = rep(2021, n_children),
    hv001 = rep(1:5, each = 10),
    hv005 = rep(1000000, n_children),
    hv022 = rep(1:2, each = 25),
    hv024 = rep("CENTRE", n_children),
    hc1 = sample(6:59, n_children, replace = TRUE),
    hc56 = sample(50:150, n_children, replace = TRUE),
    hw57 = sample(50:150, n_children, replace = TRUE),
    hv103 = rep(1, n_children),
    hv042 = rep(1, n_children)
  )

  # Test with altitude_adjusted = TRUE (default)
  result_adj <- calc_severe_anemia_dhs(pr_data, altitude_adjusted = TRUE)
  expect_true(result_adj$metadata$altitude_adjusted)
  expect_equal(result_adj$metadata$hemoglobin_variable, "hw57")

  # Test with altitude_adjusted = FALSE
  result_raw <- calc_severe_anemia_dhs(pr_data, altitude_adjusted = FALSE)
  expect_false(result_raw$metadata$altitude_adjusted)
  expect_equal(result_raw$metadata$hemoglobin_variable, "hc56")
})
