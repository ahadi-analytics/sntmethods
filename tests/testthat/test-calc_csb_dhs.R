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

test_that("calc_csb_dhs_core errors when no h32 variables found", {
  skip_if_not_installed("survey")

  # Create data without any h32 variables
  kr_data <- data.frame(
    v021 = rep(1:10, each = 10),
    v005 = rep(1000000, 100),
    v022 = rep(1:2, each = 50),
    v024 = rep("REGION1", 100),
    hw1 = sample(0:59, 100, replace = TRUE),
    h22 = sample(c(0, 1), 100, replace = TRUE, prob = c(0.6, 0.4)),
    stringsAsFactors = FALSE
  )

  # Should error when no h32 treatment-seeking variables found
  expect_error(
    calc_csb_dhs_core(kr_data),
    "No h32 treatment-seeking variables found"
  )
})

test_that("calc_csb_dhs_core works with multiple h32 sources", {
  skip_if_not_installed("survey")

  # Create mock KR data with multiple h32 source variables
  set.seed(123)
  n_children <- 200

  kr_data <- data.frame(
    v021 = rep(1:20, each = 10),           # 20 clusters
    v005 = rep(1000000, n_children),       # Standard weight
    v022 = rep(1:4, each = 50),            # 4 strata
    v024 = rep(c("REGION1", "REGION2"), each = 100),  # 2 regions
    hw1 = sample(0:59, n_children, replace = TRUE),   # Age in months
    h22 = sample(c(0, 1), n_children, replace = TRUE, prob = c(0.7, 0.3)),  # 30% had fever
    b5 = rep(1, n_children)  # All children alive
  )

  # Add h32 source variables (DHS standard facility codes)
  # Public sources
  kr_data$h32a <- ifelse(kr_data$h22 == 1,
    sample(c(0, 1), sum(kr_data$h22 == 1), replace = TRUE, prob = c(0.8, 0.2)), NA)
  kr_data$h32b <- ifelse(kr_data$h22 == 1,
    sample(c(0, 1), sum(kr_data$h22 == 1), replace = TRUE, prob = c(0.7, 0.3)), NA)
  # Private sources
  kr_data$h32j <- ifelse(kr_data$h22 == 1,
    sample(c(0, 1), sum(kr_data$h22 == 1), replace = TRUE, prob = c(0.6, 0.4)), NA)
  kr_data$h32k <- ifelse(kr_data$h22 == 1,
    sample(c(0, 1), sum(kr_data$h22 == 1), replace = TRUE, prob = c(0.5, 0.5)), NA)
  # Traditional healer (excluded by default)
  kr_data$h32t <- ifelse(kr_data$h22 == 1,
    sample(c(0, 1), sum(kr_data$h22 == 1), replace = TRUE, prob = c(0.9, 0.1)), NA)

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

  # Check that sought_any + sought_none approximately equals 100
  # (allowing for rounding)
  expect_true(all(abs(result$dhs_csb_any + result$dhs_csb_none - 100) < 1, na.rm = TRUE))
})

test_that("calc_csb_dhs_core filters out deceased children", {
  skip_if_not_installed("survey")

  set.seed(456)
  n_children <- 100

  # Create data with some deceased children
  kr_data <- data.frame(
    v021 = rep(1:10, each = 10),
    v005 = rep(1000000, n_children),
    v022 = rep(1:2, each = 50),
    v024 = rep("REGION1", n_children),
    hw1 = sample(0:59, n_children, replace = TRUE),
    h22 = sample(c(0, 1), n_children, replace = TRUE, prob = c(0.5, 0.5)),
    b5 = sample(c(0, 1), n_children, replace = TRUE, prob = c(0.2, 0.8))  # 20% deceased
  )

  # Add h32 sources
  kr_data$h32a <- ifelse(kr_data$h22 == 1,
    sample(c(0, 1), sum(kr_data$h22 == 1), replace = TRUE), NA)
  kr_data$h32j <- ifelse(kr_data$h22 == 1,
    sample(c(0, 1), sum(kr_data$h22 == 1), replace = TRUE), NA)

  # Count expected fever cases (living children only)
  expected_fever <- sum(kr_data$h22 == 1 & kr_data$b5 == 1)

  result <- calc_csb_dhs_core(kr_data)

  # The sample size should reflect only living children
  expect_equal(result$dhs_n_fever, expected_fever)
})

test_that("calc_csb_dhs_core handles custom source_config", {
  skip_if_not_installed("survey")

  set.seed(789)
  n_children <- 150

  kr_data <- data.frame(
    v021 = rep(1:15, each = 10),
    v005 = rep(1000000, n_children),
    v022 = rep(1:3, each = 50),
    hw1 = sample(0:59, n_children, replace = TRUE),
    h22 = sample(c(0, 1), n_children, replace = TRUE, prob = c(0.6, 0.4)),
    b5 = rep(1, n_children)
  )

  # Add many h32 sources
  kr_data$h32a <- ifelse(kr_data$h22 == 1, sample(c(0, 1), sum(kr_data$h22 == 1), replace = TRUE), NA)
  kr_data$h32b <- ifelse(kr_data$h22 == 1, sample(c(0, 1), sum(kr_data$h22 == 1), replace = TRUE), NA)
  kr_data$h32c <- ifelse(kr_data$h22 == 1, sample(c(0, 1), sum(kr_data$h22 == 1), replace = TRUE), NA)
  kr_data$h32j <- ifelse(kr_data$h22 == 1, sample(c(0, 1), sum(kr_data$h22 == 1), replace = TRUE), NA)
  kr_data$h32k <- ifelse(kr_data$h22 == 1, sample(c(0, 1), sum(kr_data$h22 == 1), replace = TRUE), NA)
  kr_data$h32t <- ifelse(kr_data$h22 == 1, sample(c(0, 1), sum(kr_data$h22 == 1), replace = TRUE), NA)

  # Test with custom source_config
  result <- calc_csb_dhs_core(
    kr_data,
    survey_vars = list(
      cluster = "v021",
      weight = "v005",
      stratum = "v022",
      age = "hw1",
      fever = "h22",
      alive = "b5"
    ),
    source_config = list(
      public = c("h32a", "h32b"),  # Only use these as public
      private = c("h32j"),          # Only use this as private
      excluded = c("h32t", "h32k")  # Exclude pharmacy and traditional
    )
  )

  expect_s3_class(result, "tbl_df")
  expect_true("dhs_csb_any" %in% names(result))
  expect_true("dhs_csb_public" %in% names(result))
  expect_true("dhs_csb_private" %in% names(result))
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
    b5 = rep(1, 50),
    h32a = rep(NA, 50),
    h32j = rep(NA, 50)
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
    b5 = rep(1, n_children)
  )

  # Add h32 sources
  kr_data$h32a <- ifelse(kr_data$h22 == 1, sample(c(0, 1), sum(kr_data$h22 == 1), replace = TRUE), NA)
  kr_data$h32b <- ifelse(kr_data$h22 == 1, sample(c(0, 1), sum(kr_data$h22 == 1), replace = TRUE), NA)
  kr_data$h32j <- ifelse(kr_data$h22 == 1, sample(c(0, 1), sum(kr_data$h22 == 1), replace = TRUE), NA)

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
  expect_true(result$metadata$n_h32_sources > 0)

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
    h22 = sample(c(0, 1), 150, replace = TRUE, prob = c(0.6, 0.4)),
    b5 = rep(1, 150),
    h32a = sample(c(0, 1, NA), 150, replace = TRUE),
    h32b = sample(c(0, 1, NA), 150, replace = TRUE),
    h32j = sample(c(0, 1, NA), 150, replace = TRUE)
  )

  metadata <- extract_dhs_metadata_csb(
    kr_data,
    survey_vars = list(
      cluster = "v021",
      age = "hw1",
      fever = "h22",
      alive = "b5"
    )
  )

  expect_equal(metadata$country_code, "GH8")
  expect_equal(metadata$survey_year, 2021)
  expect_equal(metadata$file_type, "KR")
  expect_equal(metadata$total_records, 150)
  expect_equal(metadata$total_clusters, 15)
  expect_equal(metadata$total_eligible_children, 150)
  expect_true(metadata$total_fever_cases > 0)
  expect_equal(metadata$n_h32_sources, 3)
  expect_true(metadata$has_alive_var)
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
    b5 = rep(1, n_children)
  )

  # Add h32 sources
  kr_data$h32a <- ifelse(kr_data$h22 == 1, sample(c(0, 1), sum(kr_data$h22 == 1), replace = TRUE), NA)
  kr_data$h32j <- ifelse(kr_data$h22 == 1, sample(c(0, 1), sum(kr_data$h22 == 1), replace = TRUE), NA)

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
    b5 = rep(1, 100),                      # All alive
    h32a = rep(c(NA, NA, 1, 0, 0), 20),   # Public source
    h32b = rep(c(NA, NA, 0, 1, 0), 20),   # Public source
    h32j = rep(c(NA, NA, 0, 1, 0), 20)    # Private source
  )

  result1 <- calc_csb_dhs_core(kr_data)
  result2 <- calc_csb_dhs_core(kr_data)

  # Results should be identical for the same input
  expect_equal(result1$dhs_csb_any, result2$dhs_csb_any)
  expect_equal(result1$dhs_csb_public, result2$dhs_csb_public)
  expect_equal(result1$dhs_csb_private, result2$dhs_csb_private)
  expect_equal(result1$dhs_n_fever, result2$dhs_n_fever)
})

test_that("calc_csb_dhs_core calculates mutually exclusive categories with precedence", {
  skip_if_not_installed("survey")

  # Create data where we can verify the calculation
  # 10 children with fever, each visited different combinations of sources
  # With precedence rule: public > private > none
  kr_data <- data.frame(
    v021 = 1:10,
    v005 = rep(1000000, 10),
    v022 = rep(1, 10),
    hw1 = rep(24, 10),
    h22 = rep(1, 10),  # All have fever
    b5 = rep(1, 10),   # All alive
    # Child 1-3: visited public only (h32a or h32b) → category: "public"
    # Child 4-6: visited private only (h32j) → category: "private"
    # Child 7-8: visited BOTH public and private → category: "public" (precedence!)
    # Child 9-10: visited none → category: "none"
    h32a = c(1, 0, 0, 0, 0, 0, 1, 0, 0, 0),
    h32b = c(0, 1, 1, 0, 0, 0, 0, 1, 0, 0),
    h32j = c(0, 0, 0, 1, 1, 1, 1, 1, 0, 0)
  )

  result <- calc_csb_dhs_core(kr_data)

  # Categories are MUTUALLY EXCLUSIVE:
  # - Public: children 1,2,3,7,8 = 5/10 = 50%
  # - Private: children 4,5,6 = 3/10 = 30% (NOT 50%, because 7,8 are counted as public!)
  # - None: children 9,10 = 2/10 = 20%
  # - Any: public + private = 80%

  expect_equal(result$dhs_csb_public, 50)
  expect_equal(result$dhs_csb_private, 30)
  expect_equal(result$dhs_csb_none, 20)
  expect_equal(result$dhs_csb_any, 80)

  # CRITICAL: Categories must sum to 100% by construction
  expect_equal(result$dhs_csb_public + result$dhs_csb_private + result$dhs_csb_none, 100)
})

test_that("care categories are mutually exclusive - all visited both sectors", {
  skip_if_not_installed("survey")

  # All children visited BOTH public AND private
  # With precedence rule, all should be classified as "public"
  kr_data <- data.frame(
    v021 = 1:10,              # 10 clusters
    v005 = rep(1000000, 10),
    v022 = rep(1, 10),
    hw1 = rep(24, 10),
    h22 = rep(1, 10),
    b5 = rep(1, 10),
    h32a = rep(1, 10),  # All visited public
    h32j = rep(1, 10)   # All also visited private
  )

  result <- calc_csb_dhs_core(kr_data)

  # With public precedence, ALL should be classified as "public"
  expect_equal(result$dhs_csb_public, 100)
  expect_equal(result$dhs_csb_private, 0)
  expect_equal(result$dhs_csb_none, 0)
  expect_equal(result$dhs_csb_any, 100)

  # Sum must be 100%
  expect_equal(result$dhs_csb_public + result$dhs_csb_private + result$dhs_csb_none, 100)
})
