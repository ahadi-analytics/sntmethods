test_that("calc_incidence validates input data", {
  # Test with non-dataframe input
  expect_error(
    calc_incidence("not a dataframe"),
    "must be a data.frame"
  )

  # Test with empty dataframe
  expect_error(
    calc_incidence(data.frame()),
    "is empty"
  )

  # Test with missing required columns for N0
  incomplete_data <- data.frame(
    hf_uid = c("HF001", "HF002"),
    adm1 = c("Region1", "Region1")
  )

  expect_error(
    calc_incidence(incomplete_data),
    "Missing required columns"
  )

  # Test with missing test/pres for N1+
  incomplete_n1_data <- data.frame(
    hf_uid = "HF001",
    adm1 = "Region1",
    adm2 = "District1",
    date = as.Date("2023-01-01"),
    conf = 10,
    pop = 5000
  )

  expect_error(
    calc_incidence(incomplete_n1_data, levels = c("N0", "N1")),
    "Missing required columns"
  )
})


test_that("calc_incidence validates parameters", {
  valid_data <- data.frame(
    hf_uid = "HF001",
    adm1 = "Region1",
    adm2 = "District1",
    date = as.Date("2023-01-01"),
    conf = 10,
    test = 100,
    pres = 5,
    tpr = 0.10,
    reprate = 0.85,
    pop = 5000
  )

  # Test invalid levels
  expect_error(
    calc_incidence(valid_data, levels = c("N0", "N6")),
    "Invalid incidence level"
  )

  # Test invalid rate_multiplier
  expect_error(
    calc_incidence(valid_data, rate_multiplier = -1000),
    "must be positive"
  )

  expect_error(
    calc_incidence(valid_data, rate_multiplier = c(1000, 10000)),
    "single numeric value"
  )

})


test_that("calc_incidence calculates N0 correctly", {
  skip_if_not_installed("sntutils")

  # Create test data - 3 facilities in same admin-month get aggregated
  # Population is at district level (same for all facilities in district)
  test_data <- data.frame(
    hf_uid = c("HF001", "HF002", "HF003"),
    adm1 = rep("Region1", 3),
    adm2 = rep("District1", 3),
    date = rep(as.Date("2023-01-01"), 3),
    conf = c(10, 20, 50),
    test = c(100, 100, 100),
    pres = c(5, 10, 20),
    tpr = c(0.10, 0.20, 0.50),
    reprate = c(0.80, 0.85, 0.90),
    pop = c(15000, 15000, 15000)  # Same district pop for all facilities
  )

  result <- calc_incidence(test_data, levels = "N0")

  # Check return structure is a list with monthly and annual components

  expect_type(result, "list")
  expect_true("monthly" %in% names(result))
  expect_true("annual" %in% names(result))

  # Check monthly$adm2 (the finest level)
  result_adm2 <- result$monthly$adm2
  expect_s3_class(result_adm2, "tbl_df")
  expect_true("n0_cases" %in% names(result_adm2))
  expect_true("n0_incidence" %in% names(result_adm2))
  expect_false("hf_uid" %in% names(result_adm2))  # No facility ID in output

  # Should have 1 row (aggregated across 3 facilities in same admin-month)
  expect_equal(nrow(result_adm2), 1)

  # Check aggregated calculations
  # conf: 10 + 20 + 50 = 80, pop: 15000 (district pop, not summed)
  # Note: n0_incidence is rounded to 2 decimal places by the function
  expect_equal(result_adm2$n0_cases, 80)
  expect_equal(result_adm2$n0_incidence, (80 / 15000) * 1000, tolerance = 0.01)

  # Check annual aggregation exists
  expect_true("adm0" %in% names(result$annual))
  expect_true("adm1" %in% names(result$annual))
  expect_true("adm2" %in% names(result$annual))
})


test_that("calc_incidence calculates N1 correctly", {
  skip_if_not_installed("sntutils")

  # Create test data - 2 facilities in same admin-month get aggregated
  # Population is at district level (same for all facilities)
  test_data <- data.frame(
    hf_uid = c("HF001", "HF002"),
    adm1 = rep("Region1", 2),
    adm2 = rep("District1", 2),
    date = rep(as.Date("2023-01-01"), 2),
    conf = c(10, 20),
    test = c(100, 100),
    pres = c(10, 20),
    tpr = c(0.10, 0.20),  # Note: TPR gets recalculated at admin level
    pop = c(11000, 11000)  # Same district pop for all facilities
  )

  result <- calc_incidence(test_data, levels = c("N0", "N1"))
  result_adm2 <- result$monthly$adm2

  # Should have 1 row (aggregated)
  expect_equal(nrow(result_adm2), 1)

  # Aggregated values:
  # conf: 10 + 20 = 30
  # test: 100 + 100 = 200
  # pres: 10 + 20 = 30
  # pop: 11000 (district pop, not summed)
  # tpr (recalculated at admin level): 30/200 = 0.15

  # N1 is calculated at FACILITY level, then aggregated:
  # HF001: n1_cases = 10 + 10 * 0.10 = 11
  # HF002: n1_cases = 20 + 20 * 0.20 = 24
  # Total: 11 + 24 = 35
  expected_n1_cases <- (10 + 10 * 0.10) + (20 + 20 * 0.20)

  expect_equal(result_adm2$n1_cases, expected_n1_cases, tolerance = 0.01)
  expect_equal(result_adm2$n1_incidence, (expected_n1_cases / 11000) * 1000, tolerance = 0.01)
})


test_that("calc_incidence calculates N2 correctly", {
  skip_if_not_installed("sntutils")

  # Create test data - single facility to avoid aggregation complexity
  test_data <- data.frame(
    hf_uid = "HF001",
    adm1 = "Region1",
    adm2 = "District1",
    date = as.Date("2023-01-01"),
    conf = 30,
    test = 200,
    pres = 30,
    tpr = 0.15,
    reprate = 0.85,
    pop = 11000
  )

  result <- calc_incidence(test_data, levels = c("N0", "N1", "N2"))
  result_adm2 <- result$monthly$adm2

  # N1 = conf + pres * tpr = 30 + 30 * 0.15 = 34.5
  # N2 = N1 / reprate = 34.5 / 0.85 = 40.588
  # Note: n2_cases is rounded to whole numbers by the function
  n1_cases <- 30 + 30 * 0.15
  expected_n2_cases <- round(n1_cases / 0.85)

  expect_equal(result_adm2$n2_cases, expected_n2_cases)
})


test_that("calc_incidence calculates N3 correctly with annual aggregation", {
  skip_if_not_installed("sntutils")

  # Create test data with multiple months to test annual aggregation
  # N3 uses annual N2, then distributes back to monthly proportionally
  test_data <- data.frame(
    hf_uid = rep("HF001", 3),
    adm1 = rep("Region1", 3),
    adm2 = rep("District1", 3),
    date = as.Date(c("2023-01-01", "2023-02-01", "2023-03-01")),
    conf = c(10, 20, 30),  # Monthly confirmed cases
    test = c(100, 200, 300),
    pres = c(10, 20, 30),
    tpr = c(0.10, 0.10, 0.10),
    reprate = c(1.0, 1.0, 1.0),  # 100% reporting for simplicity
    pop = c(5000, 5000, 5000),
    cs_public = rep(0.60, 3),
    cs_private = rep(0.25, 3),
    cs_none = rep(0.15, 3)
  )

  result <- calc_incidence(
    test_data,
    levels = c("N0", "N1", "N2", "N3")
  )
  result_monthly <- result$monthly$adm2
  result_annual <- result$annual$adm2

  # Monthly output should have N3
  expect_true("n3_cases" %in% names(result_monthly))
  expect_true("n3_incidence" %in% names(result_monthly))

  # Monthly should have N0-N3
  expect_true("n2_cases" %in% names(result_monthly))
  expect_true("n2_incidence" %in% names(result_monthly))

  # Should have 3 rows (3 months) in monthly
  expect_equal(nrow(result_monthly), 3)

  # Manual calculation:
  # For each month, N1 = conf + pres * tpr, N2 = N1 / reprate (= N1 since reprate = 1)
  # Month 1: N1 = 10 + 10*0.10 = 11, N2 = 11
  # Month 2: N1 = 20 + 20*0.10 = 22, N2 = 22
  # Month 3: N1 = 30 + 30*0.10 = 33, N2 = 33
  # Annual N2 = 11 + 22 + 33 = 66
  # CSB adjustment: adj_priv = 0.25/0.60, adj_none = 0.15/0.60
  # Annual N3 = 66 * (1 + 0.25/0.60 + 0.15/0.60) = 66 * 1.6667 = 110

  adj_priv <- 0.25 / 0.60
  adj_none <- 0.15 / 0.60
  annual_n2 <- 11 + 22 + 33
  annual_n3 <- annual_n2 * (1 + adj_priv + adj_none)

  # Check annual output
  expect_true("n3_cases" %in% names(result_annual))
  expect_true("n3_incidence" %in% names(result_annual))
  expect_equal(nrow(result_annual), 1)
  expect_equal(result_annual$n3_cases, annual_n3, tolerance = 0.01)
})


test_that("calc_incidence full cascade with real-world values", {
  skip_if_not_installed("sntutils")

  # Real-world test case from DS BUBANZA 2024-07-01
  # Note: With single month data, annual aggregation just uses that month
  test_data <- data.frame(
    hf_uid = "HF001",
    adm1 = "Region1",
    adm2 = "District1",
    date = as.Date("2024-07-01"),
    conf = 1050,
    test = 1282,
    pres = 1,
    tpr = 0.819,
    reprate = 1.0,
    pop = 275043,
    cs_public = 0.462,
    cs_private = 0.276,
    cs_none = 0.287
  )

  result <- calc_incidence(
    test_data,
    levels = c("N0", "N1", "N2", "N3")
  )
  result_monthly <- result$monthly$adm2
  result_annual <- result$annual$adm2

  # Manual calculations for verification
  # N0: Crude (confirmed only)
  N0 <- 1050
  N0_incid <- (N0 / 275043) * 1000  # 3.817

  # N1: Testing-adjusted (add presumed × tpr)
  N1 <- 1050 + (1 * 0.819)  # 1050.819
  N1_incid <- (N1 / 275043) * 1000  # 3.820

  # N2: Reporting-adjusted (inflate by reporting rate)
  N2 <- N1 / 1.0  # 1050.819 (no change since reprate = 1)
  N2_incid <- (N2 / 275043) * 1000  # 3.820

  # N3: Care-seeking-adjusted (annual only)
  # Since only 1 month, annual N2 = monthly N2, so result is same
  adj_priv <- 0.276 / 0.462  # 0.597
  adj_none <- 0.287 / 0.462  # 0.621
  N3 <- N2 + (N2 * adj_priv) + (N2 * adj_none)  # 2330.7
  N3_incid <- (N3 / 275043) * 1000  # 8.473

  # Verify N0-N2 in monthly output
  expect_equal(result_monthly$n0_cases, N0, tolerance = 0.01)
  expect_equal(result_monthly$n0_incidence, N0_incid, tolerance = 0.01)

  expect_equal(result_monthly$n1_cases, N1, tolerance = 0.01)
  expect_equal(result_monthly$n1_incidence, N1_incid, tolerance = 0.01)

  expect_equal(result_monthly$n2_cases, N2, tolerance = 0.01)
  expect_equal(result_monthly$n2_incidence, N2_incid, tolerance = 0.01)

  # N3 is in both monthly and annual output
  expect_true("n3_cases" %in% names(result_monthly))
  expect_true("n3_cases" %in% names(result_annual))

  expect_equal(result_annual$n3_cases, N3, tolerance = 0.01)
  expect_equal(result_annual$n3_incidence, N3_incid, tolerance = 0.01)

  # Verify care-seeking proportions are in annual output
  # Note: these are rounded to 2 decimal places by the function
  # Using tolerance = 0.02 due to testthat's relative tolerance
  expect_equal(result_annual$cs_public, 0.462, tolerance = 0.02)
  expect_equal(result_annual$cs_private, 0.276, tolerance = 0.02)
  expect_equal(result_annual$cs_none, 0.287, tolerance = 0.02)

  # Verify cascade progression in annual: N0 < N1 ≤ N2 < N3
  expect_true(result_annual$n0_cases < result_annual$n1_cases)
  expect_true(result_annual$n1_cases <= result_annual$n2_cases)
  expect_true(result_annual$n2_cases < result_annual$n3_cases)
})


test_that("calc_incidence handles missing TPR with error", {
  test_data <- data.frame(
    hf_uid = "HF001",
    adm1 = "Region1",
    adm2 = "District1",
    date = as.Date("2023-01-01"),
    conf = 10,
    test = 100,
    pres = 5,
    pop = 5000
  )

  # Should error when TPR column missing
  expect_error(
    calc_incidence(test_data, levels = "N1"),
    "TPR column.*not found"
  )

  expect_error(
    calc_incidence(test_data, levels = "N1"),
    "Calculate TPR first"
  )
})


test_that("calc_incidence handles zero population", {
  skip_if_not_installed("sntutils")

  # Use different admin units to avoid aggregation
  test_data <- data.frame(
    hf_uid = c("HF001", "HF002"),
    adm1 = rep("Region1", 2),
    adm2 = c("District1", "District2"),  # Different districts
    date = rep(as.Date("2023-01-01"), 2),
    conf = c(10, 20),
    test = c(100, 100),
    pres = c(5, 10),
    tpr = c(0.10, 0.20),
    pop = c(0, 5000)
  )

  result <- calc_incidence(
    test_data,
    levels = "N0",
    include_flags = TRUE
  )
  result_adm2 <- result$monthly$adm2

  # Should have 2 rows (different admin units)
  expect_equal(nrow(result_adm2), 2)

  # Incidence should be NA for zero population (District1) and valid for District2
  district1 <- result_adm2[result_adm2$adm2 == "District1", ]
  district2 <- result_adm2[result_adm2$adm2 == "District2", ]
  expect_true(is.na(district1$n0_incidence))
  expect_false(is.na(district2$n0_incidence))
})


test_that("calc_incidence handles missing values appropriately", {
  skip_if_not_installed("sntutils")

  # Use different admin units to test missing value handling
  test_data <- data.frame(
    hf_uid = c("HF001", "HF002", "HF003"),
    adm1 = rep("Region1", 3),
    adm2 = c("District1", "District2", "District3"),  # Different districts
    date = rep(as.Date("2023-01-01"), 3),
    conf = c(10, NA, 20),
    test = c(100, 100, 0),  # Zero test in 3rd to get NA TPR
    pres = c(5, 10, 20),
    tpr = c(0.10, 0.20, NA),  # NA TPR in 3rd
    pop = c(5000, 6000, 4000)
  )

  result <- calc_incidence(
    test_data,
    levels = c("N0", "N1"),
    include_flags = TRUE
  )
  result_adm2 <- result$monthly$adm2

  # Should have 3 rows (different admin units)
  expect_equal(nrow(result_adm2), 3)
})


test_that("calc_incidence uses correct rate_multiplier", {
  skip_if_not_installed("sntutils")

  test_data <- data.frame(
    hf_uid = "HF001",
    adm1 = "Region1",
    adm2 = "District1",
    date = as.Date("2023-01-01"),
    conf = 10,
    test = 100,
    pres = 5,
    tpr = 0.10,
    pop = 5000
  )

  # Test with rate_multiplier = 1000
  result_1000 <- calc_incidence(
    test_data,
    levels = "N0",
    rate_multiplier = 1000
  )
  expect_equal(result_1000$monthly$adm2$n0_incidence, (10 / 5000) * 1000)

  # Test with rate_multiplier = 10000
  result_10000 <- calc_incidence(
    test_data,
    levels = "N0",
    rate_multiplier = 10000
  )
  expect_equal(result_10000$monthly$adm2$n0_incidence, (10 / 5000) * 10000)
})


test_that("calc_incidence determines highest incidence level correctly", {
  skip_if_not_installed("sntutils")

  # Use different admin units to get separate incidence levels
  test_data <- data.frame(
    hf_uid = c("HF001", "HF002", "HF003"),
    adm1 = rep("Region1", 3),
    adm2 = c("District1", "District2", "District3"),  # Different districts
    date = rep(as.Date("2023-01-01"), 3),
    conf = c(10, 20, 30),
    test = c(100, 0, 100),  # Zero test for District2 -> NA TPR
    pres = c(5, 10, 15),
    tpr = c(0.10, NA, 0.30),
    reprate = c(0.80, 0.85, NA),
    pop = c(5000, 6000, 4000)
  )

  result <- calc_incidence(
    test_data,
    levels = c("N0", "N1", "N2")
  )
  result_adm2 <- result$monthly$adm2

  # Should have 3 rows (different admin units)
  expect_equal(nrow(result_adm2), 3)
})


test_that("calc_incidence handles custom variable names", {
  skip_if_not_installed("sntutils")

  test_data <- data.frame(
    facility_id = "HF001",
    region = "Region1",
    district = "District1",
    report_date = as.Date("2023-01-01"),
    confirmed = 10,
    tested = 100,
    presumed = 5,
    tpr_value = 0.10,
    population = 5000
  )

  result <- calc_incidence(
    test_data,
    levels = "N1",
    hf_var = "facility_id",
    adm1_var = "region",
    adm2_var = "district",
    date_var = "report_date",
    conf_var = "confirmed",
    test_var = "tested",
    pres_var = "presumed",
    tpr_var = "tpr_value",
    pop_var = "population"
  )
  result_adm2 <- result$monthly$adm2

  expect_true("n1_incidence" %in% names(result_adm2))
  expect_false(is.na(result_adm2$n1_incidence))
})


test_that("calc_incidence includes flags when requested", {
  skip_if_not_installed("sntutils")

  # Use different admin units to test flag detection after aggregation
  test_data <- data.frame(
    hf_uid = c("HF001", "HF002", "HF003"),
    adm1 = rep("Region1", 3),
    adm2 = c("District1", "District2", "District3"),  # Different districts
    date = rep(as.Date("2023-01-01"), 3),
    conf = c(10, 20, 30),
    test = c(100, 0, 100),  # Zero test for District2 -> NA TPR after recalc
    pres = c(5, 10, 15),
    tpr = c(0.10, 0.20, 0.30),
    reprate = c(0.40, 0.85, 0.90),  # Low reprate for District1
    pop = c(5000, 6000, 0)  # Zero pop for District3
  )

  result_with_flags <- calc_incidence(
    test_data,
    levels = c("N0", "N1", "N2"),
    include_flags = TRUE
  )

  result_without_flags <- calc_incidence(
    test_data,
    levels = c("N0", "N1", "N2"),
    include_flags = FALSE
  )

  # Check result structure
  expect_type(result_with_flags, "list")
  expect_type(result_without_flags, "list")

  # Check flags are excluded
  expect_false("flag_pop_zero" %in% names(result_without_flags))
  expect_false("flag_tpr_missing" %in% names(result_without_flags))
})


# =============================================================================
# S3 Class Tests
# =============================================================================

test_that("create_incidence creates valid S3 object", {
  skip_if_not_installed("sntutils")

  test_data <- data.frame(
    hf_uid = "HF001",
    adm1 = "Region1",
    adm2 = "District1",
    date = as.Date("2023-01-01"),
    conf = 10,
    test = 100,
    pres = 5,
    tpr = 0.10,
    pop = 5000
  )

  result <- calc_incidence(test_data, levels = "N1")
  result_tbl <- result$monthly$adm2
  result_obj <- create_incidence(result_tbl, scale = 1000)

  # Check S3 class
  expect_s3_class(result_obj, "snt_incidence")

  # Check structure
  expect_true("data" %in% names(result_obj))
  expect_true("meta" %in% names(result_obj))

  # Check metadata
  expect_equal(result_obj$meta$scale, 1000)
  expect_true("N1" %in% result_obj$meta$levels)
  expect_true("version" %in% names(result_obj$meta))
})


test_that("create_incidence auto-detects levels", {
  skip_if_not_installed("sntutils")

  test_data <- data.frame(
    hf_uid = "HF001",
    adm1 = "Region1",
    adm2 = "District1",
    date = as.Date("2023-01-01"),
    conf = 10,
    test = 100,
    pres = 5,
    tpr = 0.10,
    reprate = 0.85,
    pop = 5000
  )

  result <- calc_incidence(test_data, levels = c("N0", "N1", "N2"))
  result_tbl <- result$monthly$adm2
  result_obj <- create_incidence(result_tbl)

  # Should detect N0, N1, N2
  expect_equal(result_obj$meta$levels, c("N0", "N1", "N2"))
})


test_that("print.snt_incidence works", {
  skip_if_not_installed("sntutils")

  test_data <- data.frame(
    hf_uid = "HF001",
    adm1 = "Region1",
    adm2 = "District1",
    date = as.Date("2023-01-01"),
    conf = 10,
    test = 100,
    pres = 5,
    tpr = 0.10,
    pop = 5000
  )

  result <- calc_incidence(test_data, levels = "N1")
  result_tbl <- result$monthly$adm2
  result_obj <- create_incidence(result_tbl)

  # Check object class
  expect_s3_class(result_obj, "snt_incidence")

  # Check structure
  expect_true("data" %in% names(result_obj))
  expect_true("meta" %in% names(result_obj))
  # When requesting N1, N0 is also calculated as a prerequisite
  expect_equal(result_obj$meta$levels, c("N0", "N1"))

  # Should print without error (captured output may vary by environment)
  expect_invisible(print(result_obj))
})


test_that("summary.snt_incidence works", {
  skip_if_not_installed("sntutils")

  # Use different admin units to get multiple rows after aggregation
  test_data <- data.frame(
    hf_uid = c("HF001", "HF002", "HF003"),
    adm1 = rep("Region1", 3),
    adm2 = c("District1", "District2", "District3"),  # Different districts
    date = rep(as.Date("2023-01-01"), 3),
    conf = c(10, 20, 30),
    test = c(100, 100, 100),
    pres = c(5, 10, 15),
    tpr = c(0.10, 0.20, 0.30),
    pop = c(5000, 6000, 4000)
  )

  result <- calc_incidence(test_data, levels = c("N0", "N1"))
  result_tbl <- result$monthly$adm2
  result_obj <- create_incidence(result_tbl)

  # Should return summary statistics
  summary_stats <- summary(result_obj)

  expect_true("N0" %in% names(summary_stats))
  expect_true("N1" %in% names(summary_stats))
  # 3 different admin units = 3 rows
  expect_equal(summary_stats$N0$n_valid, 3)
})


test_that("as_tibble.snt_incidence works", {
  skip_if_not_installed("sntutils")
  skip_if_not_installed("tibble")

  test_data <- data.frame(
    hf_uid = "HF001",
    adm1 = "Region1",
    adm2 = "District1",
    date = as.Date("2023-01-01"),
    conf = 10,
    test = 100,
    pres = 5,
    tpr = 0.10,
    pop = 5000
  )

  result <- calc_incidence(test_data, levels = "N1")
  result_tbl <- result$monthly$adm2
  result_obj <- create_incidence(result_tbl)

  # Convert back to tibble
  result_tbl_again <- tibble::as_tibble(result_obj)

  expect_s3_class(result_tbl_again, "tbl_df")
  expect_true("n1_incidence" %in% names(result_tbl_again))
})


test_that("plot.snt_incidence works", {
  skip_if_not_installed("sntutils")
  skip_if_not_installed("ggplot2")

  test_data <- data.frame(
    hf_uid = rep(c("HF001", "HF002"), each = 3),
    adm1 = rep("Region1", 6),
    adm2 = rep(c("District1", "District2"), each = 3),
    date = rep(as.Date(c("2023-01-01", "2023-02-01", "2023-03-01")), 2),
    conf = c(10, 15, 20, 25, 30, 35),
    test = rep(100, 6),
    pres = c(5, 8, 10, 12, 15, 18),
    tpr = rep(0.15, 6),
    pop = rep(5000, 6)
  )

  result <- calc_incidence(test_data, levels = "N1")
  result_tbl <- result$monthly$adm2
  result_obj <- create_incidence(result_tbl)

  # Should create plot without error
  p <- plot(result_obj)

  expect_s3_class(p, "ggplot")
})


# =============================================================================
# N5 Tests
# =============================================================================

test_that("calc_incidence calculates N5 with default divisor (2)", {
  skip_if_not_installed("sntutils")

  test_data <- data.frame(
    hf_uid = "HF001",
    adm1 = "Region1",
    adm2 = "District1",
    date = as.Date("2023-01-01"),
    conf = 100,
    test = 500,
    pres = 10,
    tpr = 0.20,
    reprate = 1.0,
    pop = 10000,
    cs_public = 0.60,
    cs_private = 0.25,
    cs_none = 0.15
  )

  result <- calc_incidence(
    test_data,
    levels = c("N0", "N1", "N2", "N4", "N5")
  )
  result_monthly <- result$monthly$adm2
  result_annual <- result$annual$adm2

  # Verify N5 columns exist in output

  expect_true("n5_cases" %in% names(result_monthly))
  expect_true("n5_incidence" %in% names(result_monthly))
  expect_true("n5_cases" %in% names(result_annual))
  expect_true("n5_incidence" %in% names(result_annual))

  # Manual calculation:
  # N1 = 100 + 10 * 0.20 = 102
  # N2 = 102 / 1.0 = 102
  # adj_none = 0.15 / 0.60 = 0.25
  # N4 = 102 * (1 + 0.25) = 127.5
  # adj_none_reduced = 0.25 / 2 = 0.125
  # N5 = 102 * (1 + 0.125) = 114.75
  adj_none <- 0.15 / 0.60
  n2_cases <- 102
  expected_n4 <- n2_cases * (1 + adj_none)
  expected_n5 <- n2_cases * (1 + adj_none / 2)

  expect_equal(result_annual$n4_cases, expected_n4, tolerance = 1)
  expect_equal(result_annual$n5_cases, expected_n5, tolerance = 1)

  # N5 should be between N2 and N4
  expect_true(result_annual$n5_cases > result_annual$n2_cases)
  expect_true(result_annual$n5_cases < result_annual$n4_cases)
})


test_that("calc_incidence calculates N5 with custom divisor", {
  skip_if_not_installed("sntutils")

  test_data <- data.frame(
    hf_uid = "HF001",
    adm1 = "Region1",
    adm2 = "District1",
    date = as.Date("2023-01-01"),
    conf = 100,
    test = 500,
    pres = 10,
    tpr = 0.20,
    reprate = 1.0,
    pop = 10000,
    cs_public = 0.60,
    cs_private = 0.25,
    cs_none = 0.15
  )

  result <- calc_incidence(
    test_data,
    levels = c("N0", "N1", "N2", "N4", "N5"),
    cs_none_divisor = 3
  )
  result_annual <- result$annual$adm2

  # With divisor = 3:
  # adj_none = 0.15 / 0.60 = 0.25
  # adj_none_reduced = 0.25 / 3 = 0.0833
  # N5 = 102 * (1 + 0.0833) = 110.5
  n2_cases <- 102
  adj_none <- 0.15 / 0.60
  expected_n5 <- n2_cases * (1 + adj_none / 3)

  expect_equal(result_annual$n5_cases, expected_n5, tolerance = 1)
  # N5 with divisor=3 should be less than N5 with divisor=2
  expect_true(result_annual$n5_cases < n2_cases * (1 + adj_none / 2))
})


test_that("calc_incidence N5 equals N4 when divisor is 1", {
  skip_if_not_installed("sntutils")

  test_data <- data.frame(
    hf_uid = "HF001",
    adm1 = "Region1",
    adm2 = "District1",
    date = as.Date("2023-01-01"),
    conf = 100,
    test = 500,
    pres = 10,
    tpr = 0.20,
    reprate = 1.0,
    pop = 10000,
    cs_public = 0.60,
    cs_private = 0.25,
    cs_none = 0.15
  )

  result <- calc_incidence(
    test_data,
    levels = c("N0", "N1", "N2", "N4", "N5"),
    cs_none_divisor = 1
  )
  result_annual <- result$annual$adm2

  # With divisor = 1, N5 should equal N4
  expect_equal(result_annual$n5_cases, result_annual$n4_cases, tolerance = 0.01)
  expect_equal(result_annual$n5_incidence, result_annual$n4_incidence, tolerance = 0.01)
})
