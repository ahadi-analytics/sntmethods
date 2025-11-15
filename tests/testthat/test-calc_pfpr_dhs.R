test_that("calc_pfpr_dhs returns list with data, dict, and metadata", {
  # Minimal DHS data with children in target age group (6-59 months)
  mock_pr <- data.frame(
    hv001 = rep(1:2, each = 5),    # 2 clusters
    hv005 = 1000000,                # weight
    hv022 = 1,                      # stratum (avoids empty strata error)
    hv000 = "SL7",                  # country/survey ID
    hv007 = 2023,                   # survey year
    hc1 = c(7, 12, 24, 36, 48,     # ages 6-59 months for testing
            8, 15, 30, 42, 55),
    hv103 = 1,                      # present
    hv042 = 1,                      # mother listed
    hml35 = c(0,1,0,1,0, 1,1,0,0,1), # RDT results
    hml32 = c(0,1,6,0,1, 1,0,0,1,6)  # Microscopy results
  )

  # Minimal GPS data for the 2 clusters
  mock_gps <- data.frame(
    DHSCLUST = 1:2,
    LATNUM = c(8.5, 8.6),
    LONGNUM = c(-11.5, -11.4)
  )

  result <- calc_pfpr_dhs(dhs_pr = mock_pr, gps_data = mock_gps)

  # Test return structure
  expect_type(result, "list")
  expect_setequal(names(result), c("data", "dict", "metadata"))

  # Test data has PfPR columns
  expect_true(all(c("dhs_pfpr_rdt", "dhs_pfpr_mic") %in% names(result$data)))

  # Test metadata extraction
  expect_equal(result$metadata$country_code, "SL7")
  expect_equal(result$metadata$survey_year, 2023)
  expect_equal(result$metadata$file_type, "PR")
})