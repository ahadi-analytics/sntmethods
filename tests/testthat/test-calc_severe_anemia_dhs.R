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

  # Check values (proportions 0-1)
  expect_equal(result$dhs_severe_anemia, 0.30)
  expect_equal(result$dhs_n_tested_hb, 10)
  expect_equal(result$dhs_n_severe_anemia, 3)

  # Check additional anemia severity indicators are present
  expect_true("dhs_anemia_any" %in% names(result))
  expect_true("dhs_anemia_moderate_plus" %in% names(result))
  expect_true("dhs_anemia_mild_only" %in% names(result))
  expect_true("dhs_anemia_moderate_only" %in% names(result))
  expect_true("dhs_anemia_severe_only" %in% names(result))

  # Hb values in g/dL: 6.0, 7.0, 7.5, 8.5, 9.0, 9.5, 10.0, 11.0, 12.0, 13.0
  # any (< 11): 7/10 = 0.70 (6.0, 7.0, 7.5, 8.5, 9.0, 9.5, 10.0)
  expect_equal(result$dhs_anemia_any, 0.70)
  # moderate_plus (< 10): 6/10 = 0.60 (6.0, 7.0, 7.5, 8.5, 9.0, 9.5)
  expect_equal(result$dhs_anemia_moderate_plus, 0.60)
  # mild_only (>= 10 & < 11): 1/10 = 0.10 (Hb=10.0)
  expect_equal(result$dhs_anemia_mild_only, 0.10)
  # moderate_only (>= 8 & < 10): 3/10 = 0.30 (Hb=8.5, 9.0, 9.5)
  expect_equal(result$dhs_anemia_moderate_only, 0.30)
  # severe_only (< 8): 3/10 = 0.30 (Hb=6.0, 7.0, 7.5)
  expect_equal(result$dhs_anemia_severe_only, 0.30)
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

test_that("calc_severe_anemia_dhs returns named list with adm0", {
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
  expect_true("adm0" %in% names(result))
  expect_s3_class(result$adm0, "tbl_df")

  # Long-format output should contain indicator_code column
  expect_true("severe_anemia" %in% result$adm0$indicator_code)

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

test_that("severe_anemia_dictionary returns expected indicators", {
  dict <- severe_anemia_dictionary()

  expect_s3_class(dict, "tbl_df")
  expect_true("indicator_code" %in% names(dict))
  expected_codes <- c(
    "severe_anemia", "anemia_any", "anemia_moderate_plus",
    "anemia_mild_only", "anemia_moderate_only", "anemia_severe_only"
  )
  for (code in expected_codes) {
    expect_true(code %in% dict$indicator_code, info = paste("Missing:", code))
  }
  expect_equal(nrow(dict), 6)
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

  # Data with both raw (hc56) and altitude-adjusted (hw53) hemoglobin
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
    hw53 = c(60, 65, 70, 75, 90, 95, 100, 110, 120, 130),
    hv103 = rep(1, 10),
    hv042 = rep(1, 10)
  )

  # Default (altitude_adjusted = TRUE) should use hw53
  result_default <- calc_severe_anemia_dhs_core(pr_data)
  expect_equal(result_default$dhs_n_severe_anemia, 4)

  # Explicitly set altitude_adjusted = FALSE should use hc56
  result_raw <- calc_severe_anemia_dhs_core(pr_data, altitude_adjusted = FALSE)
  expect_equal(result_raw$dhs_n_severe_anemia, 2)
})

test_that("calc_severe_anemia_dhs_core errors when altitude-adjusted var missing", {
  skip_if_not_installed("survey")

  # Data without hw53 (altitude-adjusted variable)
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

  # Default (altitude_adjusted = TRUE) should error because hw53 is missing
  expect_error(
    calc_severe_anemia_dhs_core(pr_data),
    "Altitude-adjusted hemoglobin variable"
  )

  # Should work with altitude_adjusted = FALSE
  result <- calc_severe_anemia_dhs_core(pr_data, altitude_adjusted = FALSE)
  expect_s3_class(result, "tbl_df")
})

test_that("calc_severe_anemia_dhs works with both altitude adjusted and raw Hb", {
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
    hw53 = sample(50:150, n_children, replace = TRUE),
    hv103 = rep(1, n_children),
    hv042 = rep(1, n_children)
  )

  # Test with altitude_adjusted = TRUE (default)
  result_adj <- calc_severe_anemia_dhs(pr_data, altitude_adjusted = TRUE)
  expect_type(result_adj, "list")
  expect_true("adm0" %in% names(result_adj))
  expect_s3_class(result_adj$adm0, "tbl_df")
  expect_true("severe_anemia" %in% result_adj$adm0$indicator_code)

  # Test with altitude_adjusted = FALSE
  result_raw <- calc_severe_anemia_dhs(pr_data, altitude_adjusted = FALSE)
  expect_type(result_raw, "list")
  expect_true("adm0" %in% names(result_raw))
  expect_s3_class(result_raw$adm0, "tbl_df")
  expect_true("severe_anemia" %in% result_raw$adm0$indicator_code)
})
