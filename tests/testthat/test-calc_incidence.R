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
    calc_incidence(valid_data, levels = c("N0", "N5")),
    "Invalid incidence level"
  )

  # Test invalid scale_factor
  expect_error(
    calc_incidence(valid_data, scale_factor = -1000),
    "must be positive"
  )

  expect_error(
    calc_incidence(valid_data, scale_factor = c(1000, 10000)),
    "single numeric value"
  )

})


test_that("calc_incidence calculates N0 correctly", {
  skip_if_not_installed("sntutils")

  # Create simple test data
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
    pop = c(5000, 6000, 4000)
  )

  result <- calc_incidence(test_data, levels = "N0")

  # Check return structure
  expect_s3_class(result, "tbl_df")
  expect_true("n0_cases" %in% names(result))
  expect_true("n0_incidence" %in% names(result))
  expect_true("incidence_level" %in% names(result))

  # Check calculations
  expect_equal(result$n0_cases, c(10, 20, 50))
  expect_equal(
    result$n0_incidence,
    c(
      (10 / 5000) * 1000,
      (20 / 6000) * 1000,
      (50 / 4000) * 1000
    )
  )
  expect_equal(result$incidence_level, rep("N0", 3))
})


test_that("calc_incidence calculates N1 correctly", {
  skip_if_not_installed("sntutils")

  # Create test data with known TPR
  test_data <- data.frame(
    hf_uid = c("HF001", "HF002"),
    adm1 = rep("Region1", 2),
    adm2 = rep("District1", 2),
    date = rep(as.Date("2023-01-01"), 2),
    conf = c(10, 20),
    test = c(100, 100),
    pres = c(10, 20),
    tpr = c(0.10, 0.20),
    pop = c(5000, 6000)
  )

  result <- calc_incidence(test_data, levels = c("N0", "N1"))

  # Check N1 calculations: N1 = conf + pres * tpr
  expected_n1_cases <- c(
    10 + 10 * 0.10,  # 11
    20 + 20 * 0.20   # 24
  )

  expect_equal(result$n1_cases, expected_n1_cases)
  expect_equal(
    result$n1_incidence,
    c(
      (11 / 5000) * 1000,
      (24 / 6000) * 1000
    )
  )
  expect_equal(result$incidence_level, rep("N1", 2))
})


test_that("calc_incidence calculates N2 correctly", {
  skip_if_not_installed("sntutils")

  # Create test data
  test_data <- data.frame(
    hf_uid = c("HF001", "HF002"),
    adm1 = rep("Region1", 2),
    adm2 = rep("District1", 2),
    date = rep(as.Date("2023-01-01"), 2),
    conf = c(10, 20),
    test = c(100, 100),
    pres = c(10, 20),
    tpr = c(0.10, 0.20),
    reprate = c(0.80, 0.90),
    pop = c(5000, 6000)
  )

  result <- calc_incidence(test_data, levels = c("N0", "N1", "N2"))

  # Check N2 calculations: N2 = N1 / reprate
  n1_cases <- c(10 + 10 * 0.10, 20 + 20 * 0.20)
  expected_n2_cases <- c(
    n1_cases[1] / 0.80,
    n1_cases[2] / 0.90
  )

  expect_equal(result$n2_cases, expected_n2_cases)
  expect_equal(result$incidence_level, rep("N2", 2))
})


test_that("calc_incidence calculates N3 correctly", {
  skip_if_not_installed("sntutils")

  # Create test data with care-seeking
  test_data <- data.frame(
    hf_uid = "HF001",
    adm1 = "Region1",
    adm2 = "District1",
    date = as.Date("2023-01-01"),
    conf = 10,
    test = 100,
    pres = 10,
    tpr = 0.10,
    reprate = 0.80,
    pop = 5000,
    cs_public = 0.60,
    cs_private = 0.25,
    cs_none = 0.15
  )

  result <- calc_incidence(
    test_data,
    levels = c("N0", "N1", "N2", "N3")
  )

  # Check N3 structure
  expect_true("n3_cases" %in% names(result))
  expect_true("n3_incidence" %in% names(result))

  # N3 calculation: N3 = N2 + (N2 * CS_Priv / CS_Pub) + (N2 * CS_None / CS_Pub)
  n1_cases <- 10 + 10 * 0.10  # 11
  n2_cases <- n1_cases / 0.80  # 13.75
  adj_private <- 0.25 / 0.60  # 0.4167
  adj_none <- 0.15 / 0.60  # 0.25
  expected_n3_cases <- n2_cases + n2_cases * adj_private + n2_cases * adj_none

  expect_equal(result$n3_cases, expected_n3_cases, tolerance = 0.001)
  expect_equal(result$incidence_level, "N3")
})


test_that("calc_incidence full cascade with real-world values", {
  skip_if_not_installed("sntutils")

  # Real-world test case from DS BUBANZA 2024-07-01
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

  # Manual calculations for verification
  # N0: Crude (confirmed only)
  N0 = 1050
  N0_incid = (N0 / 275043) * 1000  # 3.817

  # N1: Testing-adjusted (add presumed × tpr)
  N1 = 1050 + (1 * 0.819)  # 1050.819
  N1_incid = (N1 / 275043) * 1000  # 3.820

  # N2: Reporting-adjusted (inflate by reporting rate)
  N2 = N1 / 1.0  # 1050.819 (no change since reprate = 1)
  N2_incid = (N2 / 275043) * 1000  # 3.820

  # N3: Care-seeking-adjusted
  adj_priv = 0.276 / 0.462  # 0.597
  adj_none = 0.287 / 0.462  # 0.621
  N3 = N2 + (N2 * adj_priv) + (N2 * adj_none)  # 2330.7
  N3_incid = (N3 / 275043) * 1000  # 8.473

  # Verify all levels match
  expect_equal(result$n0_cases, N0, tolerance = 0.01)
  expect_equal(result$n0_incidence, N0_incid, tolerance = 0.01)

  expect_equal(result$n1_cases, N1, tolerance = 0.01)
  expect_equal(result$n1_incidence, N1_incid, tolerance = 0.01)

  expect_equal(result$n2_cases, N2, tolerance = 0.01)
  expect_equal(result$n2_incidence, N2_incid, tolerance = 0.01)

  expect_equal(result$n3_cases, N3, tolerance = 0.01)
  expect_equal(result$n3_incidence, N3_incid, tolerance = 0.01)

  # Verify adjustment factors are included in output
  expect_true("adj_priv" %in% names(result))
  expect_true("adj_none" %in% names(result))
  expect_equal(result$adj_priv, adj_priv, tolerance = 0.001)
  expect_equal(result$adj_none, adj_none, tolerance = 0.001)

  # Verify care-seeking proportions are in output
  expect_equal(result$cs_public, 0.462)
  expect_equal(result$cs_private, 0.276)
  expect_equal(result$cs_none, 0.287)

  # Verify cascade progression: N0 < N1 ≤ N2 < N3
  expect_true(result$n0_cases < result$n1_cases)
  expect_true(result$n1_cases <= result$n2_cases)
  expect_true(result$n2_cases < result$n3_cases)

  expect_equal(result$incidence_level, "N3")
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

  test_data <- data.frame(
    hf_uid = c("HF001", "HF002"),
    adm1 = rep("Region1", 2),
    adm2 = rep("District1", 2),
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

  # Should flag zero population
  expect_true("flag_pop_zero" %in% names(result))
  expect_equal(result$flag_pop_zero, c(TRUE, FALSE))

  # Incidence should be NA for zero population
  expect_true(is.na(result$n0_incidence[1]))
  expect_false(is.na(result$n0_incidence[2]))
})


test_that("calc_incidence handles missing values appropriately", {
  skip_if_not_installed("sntutils")

  test_data <- data.frame(
    hf_uid = c("HF001", "HF002", "HF003"),
    adm1 = rep("Region1", 3),
    adm2 = rep("District1", 3),
    date = rep(as.Date("2023-01-01"), 3),
    conf = c(10, NA, 20),
    test = c(100, 100, 100),
    pres = c(5, 10, 20),
    tpr = c(0.10, 0.20, NA),
    pop = c(5000, 6000, 4000)
  )

  result <- calc_incidence(
    test_data,
    levels = c("N0", "N1"),
    include_flags = TRUE
  )

  # N0 should be NA when conf is NA
  expect_true(is.na(result$n0_incidence[2]))

  # N1 should be NA when TPR is NA
  expect_true(is.na(result$n1_incidence[3]))

  # Check flags
  expect_true("flag_tpr_missing" %in% names(result))
  expect_equal(result$flag_tpr_missing, c(FALSE, FALSE, TRUE))
})


test_that("calc_incidence uses correct scale_factor", {
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

  # Test with scale_factor = 1000
  result_1000 <- calc_incidence(
    test_data,
    levels = "N0",
    scale_factor = 1000
  )
  expect_equal(result_1000$n0_incidence, (10 / 5000) * 1000)

  # Test with scale_factor = 10000
  result_10000 <- calc_incidence(
    test_data,
    levels = "N0",
    scale_factor = 10000
  )
  expect_equal(result_10000$n0_incidence, (10 / 5000) * 10000)
})


test_that("calc_incidence determines highest incidence level correctly", {
  skip_if_not_installed("sntutils")

  test_data <- data.frame(
    hf_uid = c("HF001", "HF002", "HF003"),
    adm1 = rep("Region1", 3),
    adm2 = rep("District1", 3),
    date = rep(as.Date("2023-01-01"), 3),
    conf = c(10, 20, 30),
    test = c(100, 100, 100),
    pres = c(5, 10, 15),
    tpr = c(0.10, NA, 0.30),
    reprate = c(0.80, 0.85, NA),
    pop = c(5000, 6000, 4000)
  )

  result <- calc_incidence(
    test_data,
    levels = c("N0", "N1", "N2")
  )

  # HF001: Can calculate N2
  expect_equal(result$incidence_level[1], "N2")

  # HF002: TPR missing, can only calculate N0
  expect_equal(result$incidence_level[2], "N0")

  # HF003: reprate missing, can calculate N1
  expect_equal(result$incidence_level[3], "N1")
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

  expect_true("n1_incidence" %in% names(result))
  expect_false(is.na(result$n1_incidence))
})


test_that("calc_incidence includes flags when requested", {
  skip_if_not_installed("sntutils")

  test_data <- data.frame(
    hf_uid = c("HF001", "HF002"),
    adm1 = rep("Region1", 2),
    adm2 = rep("District1", 2),
    date = rep(as.Date("2023-01-01"), 2),
    conf = c(10, 20),
    test = c(100, 100),
    pres = c(5, 10),
    tpr = c(0.10, NA),
    reprate = c(0.40, 0.85),
    pop = c(5000, 0)
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

  # Check flags are included
  expect_true("flag_pop_zero" %in% names(result_with_flags))
  expect_true("flag_tpr_missing" %in% names(result_with_flags))
  expect_true("flag_reprate_low" %in% names(result_with_flags))

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

  result_tbl <- calc_incidence(test_data, levels = "N1")
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

  result_tbl <- calc_incidence(test_data, levels = c("N0", "N1", "N2"))
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

  result_tbl <- calc_incidence(test_data, levels = "N1")
  result_obj <- create_incidence(result_tbl)

  # Check object class
  expect_s3_class(result_obj, "snt_incidence")

  # Check structure
  expect_true("data" %in% names(result_obj))
  expect_true("meta" %in% names(result_obj))
  expect_equal(result_obj$meta$levels, "N1")

  # Should print without error (captured output may vary by environment)
  expect_invisible(print(result_obj))
})


test_that("summary.snt_incidence works", {
  skip_if_not_installed("sntutils")

  test_data <- data.frame(
    hf_uid = c("HF001", "HF002", "HF003"),
    adm1 = rep("Region1", 3),
    adm2 = rep("District1", 3),
    date = rep(as.Date("2023-01-01"), 3),
    conf = c(10, 20, 30),
    test = c(100, 100, 100),
    pres = c(5, 10, 15),
    tpr = c(0.10, 0.20, 0.30),
    pop = c(5000, 6000, 4000)
  )

  result_tbl <- calc_incidence(test_data, levels = c("N0", "N1"))
  result_obj <- create_incidence(result_tbl)

  # Should return summary statistics
  summary_stats <- summary(result_obj)

  expect_true("N0" %in% names(summary_stats))
  expect_true("N1" %in% names(summary_stats))
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

  result_tbl <- calc_incidence(test_data, levels = "N1")
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

  result_tbl <- calc_incidence(test_data, levels = "N1")
  result_obj <- create_incidence(result_tbl)

  # Should create plot without error
  p <- plot(result_obj)

  expect_s3_class(p, "ggplot")
})
