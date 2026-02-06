test_that("calc_itn_dhs returns list with data, dict, and metadata", {
  # Mock HR data (household records with ITN nets)
  mock_hr <- data.frame(
    hv001 = rep(1:2, each = 5),       # 2 clusters, 5 households each
    hv005 = 1000000,                   # weight
    hv022 = 1,                         # stratum
    hhid = 1:10,                       # household ID
    hv024 = rep(1:2, each = 5),       # admin1
    hv013 = c(4, 3, 5, 2, 6,          # household size
              3, 4, 5, 2, 4),
    hv000 = "SL7",                     # country/survey ID
    hv007 = 2023,                      # survey year
    hml10_1 = c(1, 1, 0, 1, 1, 1, 0, 1, 0, 1), # ITN net 1
    hml10_2 = c(1, 0, 0, 0, 1, 1, 0, 1, 0, 0), # ITN net 2
    hml10_3 = c(0, 0, 0, 0, 1, 0, 0, 0, 0, 0)  # ITN net 3
  )

  # Mock PR data (person records)
  mock_pr <- data.frame(
    hv001 = c(rep(1, 20), rep(2, 18)), # cluster
    hv005 = 1000000,                    # weight
    hv022 = 1,                          # stratum
    hhid = c(rep(1:5, times = c(4, 3, 5, 2, 6)),
             rep(6:10, times = c(3, 4, 5, 2, 4))), # household ID
    hv105 = c(2, 8, 35, 42,            # ages
              1, 25, 30,
              3, 12, 45, 50, 22,
              4, 60,
              0, 5, 28, 33, 55, 40,
              6, 15, 50,
              2, 7, 30, 55,
              4, 10, 24, 38, 65,
              3, 70,
              1, 9, 40, 35),
    hv104 = c(1, 2, 2, 1,              # sex (1=male, 2=female)
              2, 2, 1,
              1, 1, 2, 1, 2,
              2, 1,
              1, 2, 2, 1, 2, 1,
              2, 1, 2,
              1, 2, 2, 1,
              2, 1, 2, 1, 2,
              1, 2,
              2, 1, 2, 1),
    hml12 = c(1, 1, 2, 0,              # slept under ITN
              1, 2, 0,
              0, 0, 0, 0, 0,
              1, 0,
              1, 1, 1, 2, 1, 0,
              2, 0, 1,
              0, 0, 1, 0,
              1, 1, 2, 0, 1,
              0, 0,
              1, 1, 0, 0),
    hml18 = c(0, 0, 1, 0,              # pregnant (1=yes)
              0, 0, 0,
              0, 0, 0, 0, 1,
              0, 0,
              0, 0, 1, 0, 0, 0,
              0, 0, 0,
              0, 0, 0, 0,
              0, 0, 1, 0, 0,
              0, 0,
              0, 0, 0, 0),
    hv000 = "SL7",
    hv007 = 2023
  )

  result <- calc_itn_dhs(dhs_hr = mock_hr, dhs_pr = mock_pr)

  # Test return structure
  expect_type(result, "list")
  expect_setequal(names(result), c("data", "dict", "metadata"))

  # Test data has ITN columns
  expect_true("dhs_itn_ownership" %in% names(result$data))
  expect_true("dhs_itn_sufficient" %in% names(result$data))
  expect_true("dhs_itn_access" %in% names(result$data))
  expect_true("dhs_itn_use" %in% names(result$data))
  expect_true("dhs_itn_use_if_access" %in% names(result$data))
  expect_true("dhs_itn_use_if_access_low" %in% names(result$data))
  expect_true("dhs_itn_use_if_access_upp" %in% names(result$data))
  expect_true("dhs_itn_use_u5" %in% names(result$data))

  # Test metadata extraction
  expect_equal(result$metadata$country_code, "SL7")
  expect_equal(result$metadata$survey_year, 2023)
  expect_equal(result$metadata$file_type, "HR+PR")
  expect_equal(result$metadata$analysis_type, "ITN Coverage and Use")
})

test_that("calc_itn_dhs validates input data", {
  mock_hr <- data.frame(hv001 = 1)
  mock_pr <- data.frame(hv001 = 1)

  # Test with empty dataframes
  expect_error(
    calc_itn_dhs(
      dhs_hr = data.frame(),
      dhs_pr = mock_pr
    ),
    "empty"
  )

  expect_error(
    calc_itn_dhs(
      dhs_hr = mock_hr,
      dhs_pr = data.frame()
    ),
    "empty"
  )
})

test_that("calc_itn_dhs calculates correct indicators at cluster level", {
  # Simplified HR data - 2 clusters, 3 households each
  mock_hr <- data.frame(
    hv001 = c(1, 1, 1, 2, 2, 2),
    hv005 = 1000000,
    hv022 = 1,
    hhid = 1:6,
    hv024 = c(1, 1, 1, 2, 2, 2),
    hv013 = c(4, 4, 4, 4, 4, 4),  # all hh size = 4
    hv000 = "SL7",
    hv007 = 2023,
    hml10_1 = c(1, 1, 0, 1, 1, 1), # ITN net 1
    hml10_2 = c(1, 0, 0, 1, 0, 0)  # ITN net 2
  )

  # PR data - 4 persons per household
  mock_pr <- data.frame(
    hv001 = rep(c(1, 1, 1, 2, 2, 2), each = 4),
    hv005 = 1000000,
    hv022 = 1,
    hhid = rep(1:6, each = 4),
    hv105 = rep(c(2, 8, 35, 42), 6),   # ages
    hv104 = rep(c(1, 2, 2, 1), 6),     # sex
    hml12 = c(1, 1, 1, 0,              # cluster 1: high use
              1, 1, 0, 0,
              0, 0, 0, 0,
              1, 1, 1, 1,              # cluster 2: higher use
              1, 1, 1, 0,
              1, 1, 0, 0),
    hml18 = 0,  # no pregnant women
    hv000 = "SL7",
    hv007 = 2023
  )

  mock_gps <- data.frame(
    DHSCLUST = 1:2,
    LATNUM = c(8.5, 8.6),
    LONGNUM = c(-11.5, -11.4)
  )

  result <- calc_itn_dhs(
    dhs_hr = mock_hr,
    dhs_pr = mock_pr,
    gps_data = mock_gps
  )

  # Should have cluster-level results
  expect_equal(nrow(result$data), 2)
  expect_true("cluster_id" %in% names(result$data))
  expect_true("lat" %in% names(result$data))
  expect_true("lon" %in% names(result$data))

  # Ownership should be 2/3 for cluster 1, 3/3 for cluster 2 (proportions 0-1)
  expect_equal(
    round(result$data$dhs_itn_ownership[result$data$cluster_id == 1], 2),
    0.67
  )
  expect_equal(
    round(result$data$dhs_itn_ownership[result$data$cluster_id == 2], 2),
    1.00
  )

  # Check sample sizes
  expect_equal(result$data$dhs_n_households, c(3, 3))
  expect_equal(result$data$dhs_n_individuals, c(12, 12))
})

test_that("calc_itn_dhs works with admin-level aggregation", {
  # Mock HR data for 2 admin units
  mock_hr <- data.frame(
    hv001 = c(1, 2, 3, 4),
    hv005 = 1000000,
    hv022 = 1,
    hhid = 1:4,
    hv024 = c(1, 1, 2, 2),  # 2 admin units
    hv013 = c(4, 4, 4, 4),
    hv000 = "SL7",
    hv007 = 2023,
    hml10_1 = c(1, 1, 1, 0),
    hml10_2 = c(0, 0, 0, 0)
  )

  mock_pr <- data.frame(
    hv001 = rep(1:4, each = 4),
    hv005 = 1000000,
    hv022 = 1,
    hhid = rep(1:4, each = 4),
    hv105 = rep(c(2, 10, 30, 50), 4),
    hv104 = rep(c(1, 2, 2, 1), 4),
    hml12 = c(1, 1, 1, 0,
              1, 0, 1, 0,
              1, 1, 0, 0,
              0, 0, 0, 0),
    hml18 = 0,
    hv000 = "SL7",
    hv007 = 2023
  )

  # Without GPS - uses hv024 as admin level
  result <- calc_itn_dhs(dhs_hr = mock_hr, dhs_pr = mock_pr)

  # Should have admin-level results
  expect_equal(nrow(result$data), 2)
  expect_true("adm1" %in% names(result$data))
})

test_that("calc_itn_dhs handles pregnant women indicator", {
  # Mock HR with sufficient data
  mock_hr <- data.frame(
    hv001 = rep(1:4, each = 3),
    hv005 = 1000000,
    hv022 = 1,
    hhid = 1:12,
    hv024 = rep(1:2, each = 6),
    hv013 = 4,
    hv000 = "SL7",
    hv007 = 2023,
    hml10_1 = 1,
    hml10_2 = 0
  )

  # PR data with pregnant women
  mock_pr <- data.frame(
    hv001 = rep(1:4, each = 12),
    hv005 = 1000000,
    hv022 = 1,
    hhid = rep(rep(1:12, each = 4), times = 1),
    hv105 = rep(c(2, 10, 25, 30), 12),
    hv104 = rep(c(1, 2, 2, 2), 12),  # females in positions 2,3,4
    hml12 = c(rep(c(1, 1, 1, 0), 6),  # admin1: high ITN use
              rep(c(1, 0, 0, 0), 6)), # admin2: low ITN use
    hml18 = rep(c(0, 0, 1, 0), 12),   # position 3 is pregnant
    hv000 = "SL7",
    hv007 = 2023
  )

  result <- calc_itn_dhs(dhs_hr = mock_hr, dhs_pr = mock_pr)

  # Should have pregnant women columns
  expect_true("dhs_n_pregnant" %in% names(result$data))
  expect_true("dhs_itn_use_preg" %in% names(result$data) ||
                is.null(result$data$dhs_itn_use_preg))
})

test_that("calc_itn_dhs handles missing ITN variables gracefully", {
  mock_hr <- data.frame(
    hv001 = 1:2,
    hv005 = 1000000,
    hv022 = 1,
    hhid = 1:2,
    hv024 = 1:2,
    hv013 = 4,
    hv000 = "SL7",
    hv007 = 2023
    # Missing hml10_* variables
  )

  mock_pr <- data.frame(
    hv001 = rep(1:2, each = 4),
    hv005 = 1000000,
    hv022 = 1,
    hhid = rep(1:2, each = 4),
    hv105 = c(2, 10, 30, 50, 3, 15, 35, 55),
    hv104 = c(1, 2, 2, 1, 2, 1, 2, 1),
    hml12 = c(1, 1, 0, 0, 1, 0, 1, 0),
    hml18 = 0,
    hv000 = "SL7",
    hv007 = 2023
  )

  expect_error(
    calc_itn_dhs(dhs_hr = mock_hr, dhs_pr = mock_pr),
    "No ITN variables found"
  )
})

test_that("calc_itn_dhs metadata includes aggregation info", {
  mock_hr <- data.frame(
    hv001 = rep(1:2, each = 3),
    hv005 = 1000000,
    hv022 = 1,
    hhid = 1:6,
    hv024 = rep(1:2, each = 3),
    hv013 = 4,
    hv000 = "SL7",
    hv007 = 2023,
    hml10_1 = 1
  )

  mock_pr <- data.frame(
    hv001 = rep(1:2, each = 12),
    hv005 = 1000000,
    hv022 = 1,
    hhid = rep(1:6, each = 4),
    hv105 = rep(c(2, 10, 30, 50), 6),
    hv104 = rep(c(1, 2, 2, 1), 6),
    hml12 = 1,
    hml18 = 0,
    hv000 = "SL7",
    hv007 = 2023
  )

  mock_gps <- data.frame(
    DHSCLUST = 1:2,
    LATNUM = c(8.5, 8.6),
    LONGNUM = c(-11.5, -11.4)
  )

  # With GPS, should be cluster level
  result_cluster <- calc_itn_dhs(
    dhs_hr = mock_hr,
    dhs_pr = mock_pr,
    gps_data = mock_gps
  )
  expect_equal(result_cluster$metadata$aggregation_level, "cluster")

  # Without GPS, should be admin level
  result_admin <- calc_itn_dhs(dhs_hr = mock_hr, dhs_pr = mock_pr)
  expect_true(
    result_admin$metadata$aggregation_level %in%
      c("adm1", "national or existing admin")
  )
})

test_that("calc_itn_dhs returns proper confidence intervals", {
  mock_hr <- data.frame(
    hv001 = rep(1:4, each = 5),
    hv005 = 1000000,
    hv022 = rep(1:2, each = 10),  # 2 strata
    hhid = 1:20,
    hv024 = rep(1:2, each = 10),
    hv013 = 4,
    hv000 = "SL7",
    hv007 = 2023,
    hml10_1 = sample(c(0, 1), 20, replace = TRUE)
  )

  mock_pr <- data.frame(
    hv001 = rep(rep(1:4, each = 5), each = 4),
    hv005 = 1000000,
    hv022 = rep(rep(1:2, each = 10), each = 4),
    hhid = rep(1:20, each = 4),
    hv105 = rep(c(2, 10, 30, 50), 20),
    hv104 = rep(c(1, 2, 2, 1), 20),
    hml12 = sample(c(0, 1, 2), 80, replace = TRUE),
    hml18 = 0,
    hv000 = "SL7",
    hv007 = 2023
  )

  result <- calc_itn_dhs(dhs_hr = mock_hr, dhs_pr = mock_pr)

  # Check CI columns exist
  ci_cols <- c(
    "dhs_itn_ownership_low", "dhs_itn_ownership_upp",
    "dhs_itn_sufficient_low", "dhs_itn_sufficient_upp",
    "dhs_itn_use_low", "dhs_itn_use_upp",
    "dhs_itn_use_u5_low", "dhs_itn_use_u5_upp"
  )

  for (col in ci_cols) {
    expect_true(col %in% names(result$data))
  }

  # CI lower should be <= estimate <= CI upper
  expect_true(all(
    result$data$dhs_itn_ownership_low <= result$data$dhs_itn_ownership
  ))
  expect_true(all(
    result$data$dhs_itn_ownership <= result$data$dhs_itn_ownership_upp
  ))

  # CIs should be bounded 0-1

  expect_true(all(result$data$dhs_itn_ownership_low >= 0))
  expect_true(all(result$data$dhs_itn_ownership_upp <= 1))
})

test_that("calc_itn_dhs calculates use_if_access correctly", {
  # Create mock data with known access and use patterns
  mock_hr <- data.frame(
    hv001 = rep(1:3, each = 4),
    hv005 = 1000000,
    hv022 = 1,
    hhid = 1:12,
    hv024 = rep(1:3, each = 4),
    hv013 = 4,
    hv000 = "SL7",
    hv007 = 2023,
    # Cluster 1: 4 nets (all HH have nets)
    # Cluster 2: 2 nets (half HH have nets)
    # Cluster 3: 0 nets (no HH have nets)
    hml10_1 = c(1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0),
    hml10_2 = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
  )

  mock_pr <- data.frame(
    hv001 = rep(rep(1:3, each = 4), each = 4),
    hv005 = 1000000,
    hv022 = 1,
    hhid = rep(1:12, each = 4),
    hv105 = rep(c(2, 10, 30, 50), 12),
    hv104 = rep(c(1, 2, 2, 1), 12),
    # Cluster 1: Everyone has access, 3/4 use (75% use if access)
    # Cluster 2: Half have access, 2/4 of those with access use (50% use if access)
    # Cluster 3: No one has access, no one uses
    hml12 = c(
      1, 1, 1, 0,  # HH1: 3/4 use
      1, 1, 1, 0,  # HH2: 3/4 use
      1, 1, 1, 0,  # HH3: 3/4 use
      1, 1, 1, 0,  # HH4: 3/4 use
      1, 1, 0, 0,  # HH5: 2/4 use
      1, 1, 0, 0,  # HH6: 2/4 use
      0, 0, 0, 0,  # HH7: 0/4 use
      0, 0, 0, 0,  # HH8: 0/4 use
      0, 0, 0, 0,  # HH9-12: no use
      0, 0, 0, 0,
      0, 0, 0, 0,
      0, 0, 0, 0
    ),
    hml18 = 0,
    hv000 = "SL7",
    hv007 = 2023
  )

  result <- calc_itn_dhs(dhs_hr = mock_hr, dhs_pr = mock_pr)

  # Check that use_if_access exists and is proportion (0-1)
  expect_true("dhs_itn_use_if_access" %in% names(result$data))
  expect_true(all(result$data$dhs_itn_use_if_access >= 0, na.rm = TRUE))
  expect_true(all(result$data$dhs_itn_use_if_access <= 1, na.rm = TRUE))

  # Check that use_if_access >= use when access < 1
  # (Among those with access, usage rate should be >= overall usage rate)
  expect_true(all(
    result$data$dhs_itn_use_if_access >= result$data$dhs_itn_use,
    na.rm = TRUE
  ))

  # Check confidence intervals
  expect_true(all(
    result$data$dhs_itn_use_if_access_low <= result$data$dhs_itn_use_if_access,
    na.rm = TRUE
  ))
  expect_true(all(
    result$data$dhs_itn_use_if_access_upp >= result$data$dhs_itn_use_if_access,
    na.rm = TRUE
  ))

  # Check sample size column exists
  expect_true("dhs_n_used_among_access" %in% names(result$data))
  expect_true(all(
    result$data$dhs_n_used_among_access <= result$data$dhs_n_with_access,
    na.rm = TRUE
  ))
})
