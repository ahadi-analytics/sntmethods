test_that("calc_csb_dhs_core validates input data", {
  # Test with non-dataframe input
  expect_error(
    calc_csb_dhs_core("not a dataframe"),
    "must be a data.frame"
  )

  # Test with empty dataframe
  expect_error(
    calc_csb_dhs_core(data.frame()),
    "is empty"
  )

  # Test with missing required columns
  kr_data <- data.frame(
    v021 = 1:10,
    v005 = rep(1000000, 10)
  )

  expect_error(
    calc_csb_dhs_core(
      kr_data,
      survey_vars = list(
        cluster = "v021",
        weight = "v005",
        stratum = "v022",    # Missing
        age = "hw1",         # Missing
        fever = "h22"        # Missing
      )
    ),
    "Required variables not found"
  )
})

test_that("calc_csb_dhs_core works with minimal valid data", {
  skip_if_not_installed("survey")

  # Create minimal mock KR data with fever cases
  set.seed(123)
  n_children <- 200

  kr_data <- data.frame(
    v021 = rep(1:20, each = 10),           # 20 clusters
    v005 = rep(1000000, n_children),       # Standard weight
    v022 = rep(1:4, each = 50),            # 4 strata
    v024 = rep(c("REGION1", "REGION2"), each = 100),  # 2 regions
    hw1 = sample(0:59, n_children, replace = TRUE),   # Age in months
    h22 = sample(c(0, 1), n_children, replace = TRUE, prob = c(0.7, 0.3)),  # 30% had fever
    h32 = ifelse(
      kr_data$h22 == 1,
      sample(c(0, 1), sum(kr_data$h22 == 1), replace = TRUE, prob = c(0.3, 0.7)),
      NA
    ),  # 70% of fever cases sought care
    h32a = ifelse(
      kr_data$h22 == 1,
      sample(c(0, 1), sum(kr_data$h22 == 1), replace = TRUE, prob = c(0.7, 0.3)),
      NA
    ),  # 30% sought public care
    h32b = ifelse(
      kr_data$h22 == 1,
      sample(c(0, 1), sum(kr_data$h22 == 1), replace = TRUE, prob = c(0.5, 0.5)),
      NA
    )   # 50% sought private care
  )

  result <- calc_csb_dhs_core(kr_data)

  # Check structure of results
  expect_s3_class(result, "tbl_df")
  expect_true("dhs_csb_any" %in% names(result))
  expect_true("dhs_csb_public" %in% names(result))
  expect_true("dhs_csb_private" %in% names(result))
  expect_true("dhs_csb_none" %in% names(result))
  expect_true("dhs_n_fever" %in% names(result))

  # Check that all percentages are between 0 and 100
  expect_true(all(result$dhs_csb_any >= 0 & result$dhs_csb_any <= 100, na.rm = TRUE))
  expect_true(all(result$dhs_csb_public >= 0 & result$dhs_csb_public <= 100, na.rm = TRUE))
  expect_true(all(result$dhs_csb_private >= 0 & result$dhs_csb_private <= 100, na.rm = TRUE))
  expect_true(all(result$dhs_csb_none >= 0 & result$dhs_csb_none <= 100, na.rm = TRUE))

  # Check that confidence intervals are within bounds
  expect_true(all(result$dhs_csb_any_low >= 0, na.rm = TRUE))
  expect_true(all(result$dhs_csb_any_upp <= 100, na.rm = TRUE))
})

test_that("calc_csb_dhs_core handles children without fever correctly", {
  skip_if_not_installed("survey")

  # Create data where no children had fever
  kr_data <- data.frame(
    v021 = rep(1:5, each = 10),
    v005 = rep(1000000, 50),
    v022 = rep(1, 50),
    v024 = rep("REGION1", 50),
    hw1 = sample(0:59, 50, replace = TRUE),
    h22 = rep(0, 50),  # No fever
    h32 = rep(NA, 50),
    h32a = rep(NA, 50),
    h32b = rep(NA, 50)
  )

  expect_error(
    calc_csb_dhs_core(kr_data),
    "No children with fever"
  )
})

test_that("calc_csb_dhs returns list with data, dict, and metadata", {
  skip_if_not_installed("survey")
  skip_if_not_installed("sntutils")

  # Create mock KR data
  set.seed(456)
  n_children <- 100

  kr_data <- data.frame(
    v000 = rep("SL7", n_children),         # Country code
    v007 = rep(2019, n_children),          # Survey year
    v021 = rep(1:10, each = 10),           # 10 clusters
    v005 = rep(1000000, n_children),       # Weight
    v022 = rep(1:2, each = 50),            # Strata
    v024 = rep("WESTERN", n_children),     # Admin1
    hw1 = sample(0:59, n_children, replace = TRUE),
    h22 = sample(c(0, 1), n_children, replace = TRUE, prob = c(0.6, 0.4)),  # 40% fever
    h32 = sample(c(0, 1, NA), n_children, replace = TRUE),
    h32a = sample(c(0, 1, NA), n_children, replace = TRUE),
    h32b = sample(c(0, 1, NA), n_children, replace = TRUE)
  )

  result <- calc_csb_dhs(kr_data)

  # Check that result is a list with expected components
  expect_type(result, "list")
  expect_named(result, c("data", "dict", "metadata"))

  # Check data component
  expect_s3_class(result$data, "tbl_df")
  expect_true("dhs_csb_any" %in% names(result$data))
  expect_true("dhs_csb_public" %in% names(result$data))
  expect_true("dhs_csb_private" %in% names(result$data))

  # Check metadata component
  expect_type(result$metadata, "list")
  expect_equal(result$metadata$country_code, "SL7")
  expect_equal(result$metadata$survey_year, 2019)
  expect_equal(result$metadata$file_type, "KR")
  expect_equal(result$metadata$analysis_type, "CSB (Care-Seeking Behavior)")
  expect_equal(result$metadata$age_group, "0-59 months")

  # Check dictionary component
  expect_s3_class(result$dict, "data.frame")
  expect_true("variable" %in% names(result$dict))
})

test_that("extract_dhs_metadata_csb extracts correct metadata", {
  kr_data <- data.frame(
    v000 = rep("GH8", 150),
    v007 = rep(2021, 150),
    v021 = rep(1:15, each = 10),
    hw1 = sample(0:59, 150, replace = TRUE),
    h22 = sample(c(0, 1), 150, replace = TRUE, prob = c(0.6, 0.4))
  )

  metadata <- extract_dhs_metadata_csb(
    kr_data,
    survey_vars = list(
      cluster = "v021",
      age = "hw1",
      fever = "h22"
    )
  )

  expect_equal(metadata$country_code, "GH8")
  expect_equal(metadata$survey_year, 2021)
  expect_equal(metadata$file_type, "KR")
  expect_equal(metadata$total_records, 150)
  expect_equal(metadata$total_clusters, 15)
  expect_equal(metadata$total_eligible_children, 150)
  expect_true(metadata$total_fever_cases > 0)  # Should have some fever cases
})

test_that("calc_csb_dhs_core handles multiple admin levels", {
  skip_if_not_installed("survey")
  skip_if_not_installed("sf")

  # Create mock data
  set.seed(789)
  n_children <- 200

  kr_data <- data.frame(
    v021 = rep(1:20, each = 10),
    v005 = rep(1000000, n_children),
    v022 = rep(1:4, each = 50),
    hw1 = sample(0:59, n_children, replace = TRUE),
    h22 = sample(c(0, 1), n_children, replace = TRUE, prob = c(0.7, 0.3)),
    h32 = sample(c(0, 1, NA), n_children, replace = TRUE),
    h32a = sample(c(0, 1, NA), n_children, replace = TRUE),
    h32b = sample(c(0, 1, NA), n_children, replace = TRUE)
  )

  # Create mock GPS data
  gps_data <- data.frame(
    DHSCLUST = 1:20,
    LATNUM = runif(20, -9, -8),
    LONGNUM = runif(20, -12, -11)
  )

  # Create mock shapefile with admin levels
  library(sf)
  polygons <- st_sfc(
    st_polygon(list(
      cbind(c(-12.5, -12.5, -11.5, -11.5, -12.5),
            c(-9.5, -8.5, -8.5, -9.5, -9.5))
    )),
    st_polygon(list(
      cbind(c(-11.5, -11.5, -10.5, -10.5, -11.5),
            c(-9.5, -8.5, -8.5, -9.5, -9.5))
    )),
    st_polygon(list(
      cbind(c(-12.5, -12.5, -11.5, -11.5, -12.5),
            c(-8.5, -7.5, -7.5, -8.5, -8.5))
    )),
    st_polygon(list(
      cbind(c(-11.5, -11.5, -10.5, -10.5, -11.5),
            c(-8.5, -7.5, -7.5, -8.5, -8.5))
    ))
  )

  shapefile <- st_sf(
    adm1 = c("NORTH", "NORTH", "SOUTH", "SOUTH"),
    adm2 = c("DISTRICT1", "DISTRICT2", "DISTRICT3", "DISTRICT4"),
    adm1_name = c("Northern Province", "Northern Province",
                  "Southern Province", "Southern Province"),
    adm2_name = c("District One", "District Two",
                  "District Three", "District Four"),
    geometry = polygons,
    crs = 4326
  )

  result <- calc_csb_dhs_core(
    kr_data,
    gps_data = gps_data,
    shapefile = shapefile,
    admin_level = c("adm1", "adm2")
  )

  # Check that admin columns are properly split
  expect_true("adm1" %in% names(result))
  expect_true("adm2" %in% names(result))
  expect_false("admin_class" %in% names(result))  # Should be removed

  # Check that admin name columns are included
  expect_true("adm1_name" %in% names(result))
  expect_true("adm2_name" %in% names(result))

  # Check that results are grouped by both admin levels
  expect_true(length(unique(result$adm1)) <= 2)
  expect_true(length(unique(result$adm2)) <= 4)
})

test_that("aggregate_csb_admin works with mock data", {
  skip_if_not_installed("sf")

  # Create mock cluster results
  cluster_results <- data.frame(
    cluster_id = 1:6,
    lat = c(-8.5, -8.3, -8.7, -8.4, -8.6, -8.2),
    lon = c(-11.2, -11.0, -11.5, -11.3, -11.1, -11.4),
    dhs_csb_any = c(65.2, 72.1, 58.3, 70.5, 62.8, 68.9),
    dhs_csb_public = c(25.5, 30.2, 22.1, 28.7, 24.3, 27.5),
    dhs_csb_private = c(45.2, 50.1, 38.5, 48.3, 41.2, 46.8),
    dhs_csb_none = c(34.8, 27.9, 41.7, 29.5, 37.2, 31.1),
    dhs_n_fever = c(25, 30, 28, 22, 26, 24)
  )

  # Create mock shapefile
  library(sf)
  polygons <- st_sfc(
    st_polygon(list(
      cbind(c(-11.6, -11.6, -11.0, -11.0, -11.6),
            c(-8.8, -8.2, -8.2, -8.8, -8.8))
    )),
    st_polygon(list(
      cbind(c(-11.0, -11.0, -10.4, -10.4, -11.0),
            c(-8.8, -8.2, -8.2, -8.8, -8.8))
    ))
  )

  shapefile <- st_sf(
    adm1 = c("REGION1", "REGION2"),
    adm1_name = c("Region One", "Region Two"),
    geometry = polygons,
    crs = 4326
  )

  # Test aggregation
  result <- aggregate_csb_admin(
    cluster_results,
    shapefile,
    admin_level = "adm1",
    weighted = TRUE
  )

  expect_s3_class(result, "sf")
  expect_true("dhs_csb_any" %in% names(result))
  expect_true("dhs_csb_public" %in% names(result))
  expect_true("dhs_csb_private" %in% names(result))
  expect_true("dhs_n_fever" %in% names(result))
  expect_true("adm1" %in% names(result))
  expect_true("adm1_name" %in% names(result))
  expect_equal(nrow(result), 2)  # Two regions

  # Check that aggregated values are reasonable
  expect_true(all(result$dhs_csb_any >= 0 & result$dhs_csb_any <= 100))
  expect_true(all(result$dhs_n_fever > 0))
})

test_that("calc_csb_dhs_core handles missing care-seeking variables gracefully", {
  skip_if_not_installed("survey")

  # Create data with only some care-seeking variables
  kr_data <- data.frame(
    v021 = rep(1:10, each = 10),
    v005 = rep(1000000, 100),
    v022 = rep(1:2, each = 50),
    v024 = rep("REGION1", 100),
    hw1 = sample(0:59, 100, replace = TRUE),
    h22 = sample(c(0, 1), 100, replace = TRUE, prob = c(0.6, 0.4)),
    h32 = sample(c(0, 1), 100, replace = TRUE),  # Has general care-seeking
    # Missing h32a and h32b
    stringsAsFactors = FALSE
  )

  result <- calc_csb_dhs_core(
    kr_data,
    survey_vars = list(
      cluster = "v021",
      weight = "v005",
      stratum = "v022",
      age = "hw1",
      fever = "h22",
      sought_care = "h32",
      public_sector = "h32a_missing",   # Not in data
      private_sector = "h32b_missing"    # Not in data
    )
  )

  # Should still calculate what it can
  expect_s3_class(result, "tbl_df")
  expect_true("dhs_csb_any" %in% names(result))
  expect_true("dhs_csb_none" %in% names(result))
  expect_true("dhs_n_fever" %in% names(result))

  # Public and private should be NA if variables missing
  if ("dhs_csb_public" %in% names(result)) {
    expect_true(all(is.na(result$dhs_csb_public)))
  }
  if ("dhs_csb_private" %in% names(result)) {
    expect_true(all(is.na(result$dhs_csb_private)))
  }
})

test_that("calc_csb_dhs_core produces consistent results", {
  skip_if_not_installed("survey")

  # Set seed for reproducibility
  set.seed(999)

  # Create consistent test data
  kr_data <- data.frame(
    v021 = rep(1:5, each = 20),
    v005 = rep(1000000, 100),
    v022 = rep(1, 100),
    v024 = rep("REGION1", 100),
    hw1 = rep(c(6, 12, 24, 36, 48), 20),  # Fixed ages
    h22 = rep(c(0, 0, 1, 1, 1), 20),      # 60% with fever
    h32 = rep(c(NA, NA, 1, 1, 0), 20),    # Varied care-seeking
    h32a = rep(c(NA, NA, 1, 0, 0), 20),   # Public care
    h32b = rep(c(NA, NA, 0, 1, 0), 20)    # Private care
  )

  result1 <- calc_csb_dhs_core(kr_data)
  result2 <- calc_csb_dhs_core(kr_data)

  # Results should be identical for the same input
  expect_equal(result1$dhs_csb_any, result2$dhs_csb_any)
  expect_equal(result1$dhs_csb_public, result2$dhs_csb_public)
  expect_equal(result1$dhs_csb_private, result2$dhs_csb_private)
  expect_equal(result1$dhs_n_fever, result2$dhs_n_fever)
})