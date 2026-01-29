test_that("calc_tpr validates input data", {
  # Test with non-dataframe input
  expect_error(
    calc_tpr("not a dataframe"),
    "must be a data.frame"
  )

  # Test with empty dataframe
  expect_error(
    calc_tpr(data.frame()),
    "is empty"
  )

  # Test with missing required columns
  incomplete_data <- data.frame(
    hf_uid = c("HF001", "HF002"),
    adm1 = c("Region1", "Region1")
  )

  expect_error(
    calc_tpr(incomplete_data),
    "Missing required columns"
  )

  # Test with invalid extreme_threshold
  valid_data <- data.frame(
    hf_uid = "HF001",
    adm1 = "Region1",
    adm2 = "District1",
    date = as.Date("2023-01-01"),
    conf = 10,
    test = 100,
    pres = 5
  )

  expect_error(
    calc_tpr(valid_data, extreme_threshold = c(0.5)),
    "numeric length 2"
  )

  expect_error(
    calc_tpr(valid_data, extreme_threshold = c(0.9, 0.1)),
    "must be <"
  )
})

test_that("calc_tpr validates fallback parameters", {
  valid_data <- data.frame(
    hf_uid = "HF001",
    adm1 = "Region1",
    adm2 = "District1",
    date = as.Date("2023-01-01"),
    conf = 10,
    test = 100,
    pres = 5
  )

  # Test invalid fallback_method
  expect_error(
    calc_tpr(valid_data, fallback_method = c("invalid_method")),
    "Invalid fallback method"
  )

  # Test invalid prev_year_window
  expect_error(
    calc_tpr(valid_data, prev_year_window = 10),
    "must be between 0 and 6"
  )

  expect_error(
    calc_tpr(valid_data, prev_year_window = -1),
    "must be between 0 and 6"
  )

  # Test invalid fallback_triggers
  expect_error(
    calc_tpr(valid_data, fallback_triggers = c("invalid_trigger")),
    "Invalid fallback trigger"
  )
})

test_that("calc_tpr calculates raw TPR correctly", {
  skip_if_not_installed("sntutils")

  # Create simple test data
  test_data <- data.frame(
    hf_uid = c("HF001", "HF002", "HF003"),
    adm1 = rep("Region1", 3),
    adm2 = rep("District1", 3),
    date = rep(as.Date("2023-01-01"), 3),
    conf = c(10, 20, 50),
    test = c(100, 100, 100),
    pres = c(5, 10, 20)
  )

  result <- calc_tpr(test_data, include_flags = TRUE)

  # Check return structure
  expect_s3_class(result, "tbl_df")
  expect_true("tpr" %in% names(result))
  expect_true("tpr_source" %in% names(result))
  expect_true("adm0" %in% names(result))

  # Check TPR values
  expect_equal(result$tpr[1], 0.10)
  expect_equal(result$tpr[2], 0.20)
  expect_equal(result$tpr[3], 0.50)

  # Check source is facility_raw
  expect_equal(result$tpr_source[1], "facility_raw")
  expect_equal(result$tpr_source[2], "facility_raw")
  expect_equal(result$tpr_source[3], "facility_raw")

  # Check flags
  expect_true(all(result$flag_tpr_valid))
  expect_false(any(result$flag_tpr_proxy))
  expect_true(all(result$adm0 == "country"))
})

test_that("calc_tpr applies adm2 proxy fallback correctly", {
  skip_if_not_installed("sntutils")

  # Create test data with missing TPR that needs proxy
  test_data <- data.frame(
    hf_uid = c("HF001", "HF002", "HF003", "HF004"),
    adm1 = rep("Region1", 4),
    adm2 = c("District1", "District1", "District2", "District2"),
    date = rep(as.Date("2023-01-01"), 4),
    conf = c(10, NA, 20, NA),
    test = c(100, 100, 100, 100),
    pres = c(5, 5, 10, 10)
  )

  result <- calc_tpr(test_data, fallback_method = "adm2")

  # HF001: Direct calculation (0.10)
  expect_equal(result$tpr[1], 0.10)
  expect_equal(result$tpr_source[1], "facility_raw")

  # HF002: Should use district proxy (same adm2 as HF001)
  expect_equal(result$tpr[2], 0.10)
  expect_equal(result$tpr_source[2], "proxy_adm2")

  # HF003: Direct calculation (0.20)
  expect_equal(result$tpr[3], 0.20)
  expect_equal(result$tpr_source[3], "facility_raw")

  # HF004: Should use district proxy (same adm2 as HF003)
  expect_equal(result$tpr[4], 0.20)
  expect_equal(result$tpr_source[4], "proxy_adm2")
})

test_that("calc_tpr applies adm1 fallback correctly", {
  skip_if_not_installed("sntutils")

  # Facility in district with no other valid data
  test_data <- data.frame(
    hf_uid = c("HF001", "HF002", "HF003"),
    adm1 = rep("Region1", 3),
    adm2 = c("District1", "District2", "District3"),
    date = rep(as.Date("2023-01-01"), 3),
    conf = c(30, NA, 10),
    test = c(100, 100, 100),
    pres = rep(5, 3)
  )

  result <- calc_tpr(test_data, fallback_method = c("adm2", "adm1"))

  # HF002 is alone in District2, should fall back to adm1
  # adm1 TPR = (30 + 10) / (100 + 100) = 0.20
  expect_equal(result$tpr[2], 0.20)
  expect_equal(result$tpr_source[2], "proxy_adm1")
})

test_that("calc_tpr applies prev_year fallback with window = 0", {
  skip_if_not_installed("sntutils")

  # Create data spanning two years with isolated facilities
  test_data <- data.frame(
    hf_uid = c("HF001", "HF001", "HF002"),
    adm1 = c("Region1", "Region2", "Region3"),
    adm2 = c("District1", "District2", "District3"),
    date = as.Date(c("2022-06-01", "2023-06-01", "2023-06-01")),
    conf = c(25, NA, NA),
    test = c(100, 100, 100),
    pres = c(10, 10, 10)
  )

  result <- calc_tpr(
    test_data,
    fallback_method = "prev_year",
    prev_year_window = 0
  )

  # HF001 2023-06: Should use previous year (exact month match)
  hf001_2023 <- result[result$hf_uid == "HF001" & result$year == 2023, ]
  expect_equal(hf001_2023$tpr, 0.25)
  expect_equal(hf001_2023$tpr_source, "proxy_prev_year")
})

test_that("calc_tpr applies prev_year fallback with seasonal window", {
  skip_if_not_installed("sntutils")

  # Data with multiple months for weighted average
  test_data <- data.frame(
    hf_uid = rep("HF001", 5),
    adm1 = rep("Region1", 5),
    adm2 = rep("District1", 5),
    date = as.Date(c(
      "2022-05-01", "2022-06-01", "2022-07-01",  # Previous year
      "2023-06-01", "2023-07-01"  # Current year (missing)
    )),
    conf = c(20, 30, 40, NA, 10),
    test = c(100, 100, 100, 100, 100),
    pres = rep(10, 5)
  )

  result <- calc_tpr(
    test_data,
    fallback_method = "prev_year",
    prev_year_window = 1
  )

  # 2023-06 should use weighted average of 2022-05, 2022-06, 2022-07
  # (20 + 30 + 40) / (100 + 100 + 100) = 0.30
  hf001_202306 <- result[
    result$hf_uid == "HF001" & result$date == as.Date("2023-06-01"),
  ]
  expect_equal(hf001_202306$tpr, 0.30)
  expect_equal(hf001_202306$tpr_source, "proxy_prev_year")
})

test_that("calc_tpr applies rolling fallback correctly", {
  skip_if_not_installed("sntutils")

  # Data with consecutive months
  test_data <- data.frame(
    hf_uid = rep("HF001", 5),
    adm1 = rep("Region1", 5),
    adm2 = rep("District1", 5),
    date = as.Date(c(
      "2023-01-01", "2023-02-01", "2023-03-01",
      "2023-04-01", "2023-05-01"
    )),
    conf = c(10, 20, NA, 40, 50),
    test = c(100, 100, 100, 100, 100),
    pres = rep(10, 5)
  )

  result <- calc_tpr(test_data, fallback_method = "rolling")

  # 2023-03 should use rolling average of Feb and Apr (excluding target)
  # (20 + 40) / (100 + 100) = 0.30
  feb_march <- result[result$date == as.Date("2023-03-01"), ]
  expect_equal(feb_march$tpr, 0.30)
  expect_equal(feb_march$tpr_source, "proxy_rolling")
})

test_that("calc_tpr applies adm0 fallback correctly", {
  skip_if_not_installed("sntutils")

  # Isolated facility that needs national fallback
  test_data <- data.frame(
    hf_uid = c("HF001", "HF002"),
    adm1 = c("Region1", "Region2"),
    adm2 = c("District1", "District2"),
    date = rep(as.Date("2023-01-01"), 2),
    conf = c(25, NA),
    test = c(100, 100),
    pres = c(10, 10)
  )

  result <- calc_tpr(test_data, fallback_method = "adm0")

  # HF002 should use national TPR (25/100 = 0.25)
  expect_equal(result$tpr[2], 0.25)
  expect_equal(result$tpr_source[2], "proxy_adm0")
})

test_that("calc_tpr respects fallback order", {
  skip_if_not_installed("sntutils")

  # Complex scenario testing fallback hierarchy
  test_data <- data.frame(
    hf_uid = c("HF001", "HF002", "HF003"),
    adm1 = c("Region1", "Region1", "Region2"),
    adm2 = c("District1", "District2", "District3"),
    date = rep(as.Date("2023-01-01"), 3),
    conf = c(30, NA, 10),
    test = c(100, 100, 100),
    pres = c(10, 10, 10)
  )

  # Test with adm2 -> adm1 order
  result <- calc_tpr(
    test_data,
    fallback_method = c("adm2", "adm1")
  )

  # HF002 has no adm2 peers, should use adm1 (Region1)
  # Region1 TPR = 30/100 = 0.30
  expect_equal(result$tpr[2], 0.30)
  expect_equal(result$tpr_source[2], "proxy_adm1")
})

test_that("calc_tpr flags inactive facilities correctly", {
  skip_if_not_installed("sntutils")

  test_data <- data.frame(
    hf_uid = c("HF001", "HF002"),
    adm1 = rep("Region1", 2),
    adm2 = rep("District1", 2),
    date = rep(as.Date("2023-01-01"), 2),
    conf = c(10, 20),
    test = c(100, 100),
    pres = c(5, 10)
  )

  result <- calc_tpr(test_data, include_flags = TRUE)

  # Check flag_inactive exists
  expect_true("flag_inactive" %in% names(result))

  # Inactive facilities should have NA TPR (not proxy)
  # Note: Actual inactive detection requires proper activity data
})

test_that("calc_tpr handles conf > test correctly (now receives proxy by default)", {
  skip_if_not_installed("sntutils")

  test_data <- data.frame(
    hf_uid = c("HF001", "HF002", "HF003"),
    adm1 = rep("Region1", 3),
    adm2 = rep("District1", 3),
    date = rep(as.Date("2023-01-01"), 3),
    conf = c(150, 10, 20),  # HF001 has impossible value
    test = c(100, 100, 100),
    pres = c(5, 5, 10)
  )

  result <- calc_tpr(test_data, include_flags = TRUE)

  # Check impossible value flag
  expect_true(result$flag_conf_gt_test[1])
  expect_false(result$flag_conf_gt_test[2])

  # HF001 should now GET a proxy TPR (behavior change)
  # Impossible values now receive fallback values by default
  expect_false(is.na(result$tpr[1]))
  expect_false(result$flag_tpr_valid[1])  # Still not valid raw TPR
  expect_true(result$flag_tpr_proxy[1])    # But has proxy
  # Will get proxy_adm2 since rolling requires multiple months per facility
  expect_equal(result$tpr_source[1], "proxy_adm2")

  # HF001 should receive proxy with fallback enabled (consistent behavior)
  result_with_fallback <- calc_tpr(
    test_data,
    fallback_method = c("adm2", "adm1", "adm0"),
    include_flags = TRUE
  )
  expect_false(is.na(result_with_fallback$tpr[1]))
  expect_equal(result_with_fallback$tpr_source[1], "proxy_adm2")
})

test_that("calc_tpr flags extreme TPR values", {
  skip_if_not_installed("sntutils")

  test_data <- data.frame(
    hf_uid = c("HF001", "HF002", "HF003"),
    adm1 = rep("Region1", 3),
    adm2 = rep("District1", 3),
    date = rep(as.Date("2023-01-01"), 3),
    conf = c(0, 50, 100),
    test = c(100, 100, 100),
    pres = c(5, 5, 5)
  )

  result <- calc_tpr(test_data, include_flags = TRUE)

  # Check extreme values with default threshold (0.01, 0.99)
  expect_true(result$flag_tpr_extreme[1])   # 0.00 < 0.01
  expect_false(result$flag_tpr_extreme[2])  # 0.50 is normal
  expect_true(result$flag_tpr_extreme[3])   # 1.00 > 0.99
})

test_that("calc_tpr handles custom variable mapping", {
  skip_if_not_installed("sntutils")

  # Data with non-standard column names
  test_data <- data.frame(
    facility_id = c("HF001", "HF002"),
    region = rep("Region1", 2),
    district = rep("District1", 2),
    report_date = rep(as.Date("2023-01-01"), 2),
    confirmed_cases = c(10, 20),
    tested = c(100, 100),
    presumed_cases = c(5, 10)
  )

  result <- calc_tpr(
    test_data,
    hf_var = "facility_id",
    adm1_var = "region",
    adm2_var = "district",
    date_var = "report_date",
    conf_var = "confirmed_cases",
    test_var = "tested",
    activity_indicators = c("confirmed_cases", "tested")
  )

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 2)
  expect_equal(result$tpr[1], 0.10)
  expect_equal(result$tpr[2], 0.20)
})

test_that("calc_tpr include_flags parameter works correctly", {
  skip_if_not_installed("sntutils")

  test_data <- data.frame(
    hf_uid = c("HF001", "HF002"),
    adm1 = rep("Region1", 2),
    adm2 = rep("District1", 2),
    date = rep(as.Date("2023-01-01"), 2),
    conf = c(10, 20),
    test = c(100, 100),
    pres = c(5, 10)
  )

  # With include_flags = FALSE (default)
  result_no_flags <- calc_tpr(test_data, include_flags = FALSE)
  expect_false("flag_tpr_valid" %in% names(result_no_flags))
  expect_false("flag_tpr_extreme" %in% names(result_no_flags))

  # With include_flags = TRUE
  result_with_flags <- calc_tpr(test_data, include_flags = TRUE)
  expect_true("flag_tpr_valid" %in% names(result_with_flags))
  expect_true("flag_tpr_extreme" %in% names(result_with_flags))
  expect_true("flag_tpr_proxy" %in% names(result_with_flags))
  expect_true("flag_tpr_missing" %in% names(result_with_flags))
  expect_true("flag_conf_gt_test" %in% names(result_with_flags))
  expect_true("flag_inactive" %in% names(result_with_flags))
})

test_that("calc_tpr handles zero test values", {
  skip_if_not_installed("sntutils")

  test_data <- data.frame(
    hf_uid = c("HF001", "HF002"),
    adm1 = rep("Region1", 2),
    adm2 = rep("District1", 2),
    date = rep(as.Date("2023-01-01"), 2),
    conf = c(10, 0),
    test = c(0, 100),  # HF001 has zero tests
    pres = c(5, 5)
  )

  result <- calc_tpr(test_data, include_flags = TRUE)

  # HF001 should not have raw TPR (test = 0 converted to NA)
  expect_false(result$flag_tpr_valid[1])
  expect_true(result$flag_zero_test[1])

  # HF002 should have raw TPR
  expect_true(result$flag_tpr_valid[2])
  expect_equal(result$tpr[2], 0.00)
})

test_that("calc_tpr flags low test counts", {
  skip_if_not_installed("sntutils")

  test_data <- data.frame(
    hf_uid = c("HF001", "HF002", "HF003"),
    adm1 = rep("Region1", 3),
    adm2 = rep("District1", 3),
    date = rep(as.Date("2023-01-01"), 3),
    conf = c(1, 2, 10),
    test = c(3, 5, 100),  # HF001 has low test count
    pres = c(1, 2, 5)
  )

  result <- calc_tpr(test_data, include_flags = TRUE)

  # Check low test flag
  expect_true(result$flag_low_test[1])   # test < 5
  expect_false(result$flag_low_test[2])  # test = 5
  expect_false(result$flag_low_test[3])  # test = 100
})

test_that("calc_tpr handles adm0_var parameter", {
  skip_if_not_installed("sntutils")

  test_data <- data.frame(
    hf_uid = c("HF001", "HF002"),
    country = c("Kenya", "Kenya"),
    adm1 = rep("Region1", 2),
    adm2 = rep("District1", 2),
    date = rep(as.Date("2023-01-01"), 2),
    conf = c(10, 20),
    test = c(100, 100),
    pres = c(5, 10)
  )

  result <- calc_tpr(test_data, adm0_var = "country")

  expect_equal(unique(result$adm0), "Kenya")
})

test_that("calc_tpr disables all fallbacks when requested", {
  skip_if_not_installed("sntutils")

  test_data <- data.frame(
    hf_uid = c("HF001", "HF002", "HF003"),
    adm1 = rep("Region1", 3),
    adm2 = c("District1", "District1", "District2"),
    date = rep(as.Date("2023-01-01"), 3),
    conf = c(10, NA, 20),
    test = c(100, 100, 100),
    pres = c(5, 5, 10)
  )

  result <- calc_tpr(test_data, fallback_method = character(0))

  # HF002 should have NA (no fallback applied)
  expect_true(is.na(result$tpr[2]))
  expect_true(is.na(result$tpr_source[2]))
})

test_that("calc_tpr produces consistent results", {
  skip_if_not_installed("sntutils")

  set.seed(123)

  test_data <- data.frame(
    hf_uid = paste0("HF", 1:10),
    adm1 = rep("Region1", 10),
    adm2 = rep("District1", 10),
    date = rep(as.Date("2023-01-01"), 10),
    conf = sample(10:50, 10),
    test = rep(100, 10),
    pres = sample(5:20, 10)
  )

  result1 <- calc_tpr(test_data)
  result2 <- calc_tpr(test_data)

  # Results should be identical for the same input
  expect_equal(result1$tpr, result2$tpr)
  expect_equal(result1$tpr_source, result2$tpr_source)
})

test_that("calc_tpr extracts year and month correctly", {
  skip_if_not_installed("sntutils")

  test_data <- data.frame(
    hf_uid = c("HF001", "HF001", "HF001"),
    adm1 = rep("Region1", 3),
    adm2 = rep("District1", 3),
    date = as.Date(c("2023-01-15", "2023-06-20", "2024-12-05")),
    conf = c(10, 20, 30),
    test = c(100, 100, 100),
    pres = c(5, 10, 15)
  )

  result <- calc_tpr(test_data)

  # Check year and month extraction
  expect_equal(result$year, c(2023, 2023, 2024))
  expect_equal(result$month, c(1, 6, 12))

  # Check date standardization to first of month
  expect_equal(
    result$date,
    as.Date(c("2023-01-01", "2023-06-01", "2024-12-01"))
  )
})

test_that("calc_tpr handles fallback_triggers correctly (impossible values now included by default)", {
  skip_if_not_installed("sntutils")

  test_data <- data.frame(
    hf_uid = c("HF001", "HF002", "HF003"),
    adm1 = rep("Region1", 3),
    adm2 = rep("District1", 3),
    date = rep(as.Date("2023-01-01"), 3),
    conf = c(10, 150, NA),  # HF002 has conf > test, HF003 missing
    test = c(100, 100, 100),
    pres = c(5, 5, 5)
  )

  # With default trigger (missing only)
  result_default <- calc_tpr(
    test_data,
    fallback_method = "adm2",
    fallback_triggers = "missing",
    include_flags = TRUE
  )

  # HF002 (conf > test) should NOW get proxy by default (behavior change)
  # Impossible values are now included in fallback by default
  expect_equal(result_default$tpr[2], 0.10)
  expect_equal(result_default$tpr_source[2], "proxy_adm2")

  # HF003 (missing) should get proxy
  expect_equal(result_default$tpr[3], 0.10)
  expect_equal(result_default$tpr_source[3], "proxy_adm2")

  # Both facility-months should be flagged appropriately
  expect_true(result_default$flag_conf_gt_test[2])  # HF002 impossible
  expect_true(result_default$flag_missing_conf[3])  # HF003 missing
})

test_that("calc_tpr does not return reprate column", {
  skip_if_not_installed("sntutils")

  test_data <- data.frame(
    hf_uid = c("HF001", "HF002"),
    adm1 = rep("Region1", 2),
    adm2 = rep("District1", 2),
    date = rep(as.Date("2023-01-01"), 2),
    conf = c(10, 20),
    test = c(100, 100),
    pres = c(5, 10)
  )

  result <- calc_tpr(test_data)

  # Check reprate column does not exist
  expect_false("reprate" %in% names(result))

  # Check flag_low_reprate does not exist
  result_with_flags <- calc_tpr(test_data, include_flags = TRUE)
  expect_false("flag_low_reprate" %in% names(result_with_flags))
})
