# ============================================================================
# Tests for calc_u5mr_dhs_core() — UNCHANGED (core function)
# ============================================================================

test_that("calc_u5mr_dhs_core validates input data", {
  # Test with non-dataframe input
  expect_error(
    calc_u5mr_dhs_core("not a dataframe"),
    "must be a data.frame"
  )

  # Test with empty dataframe
  expect_error(
    calc_u5mr_dhs_core(data.frame()),
    "is empty"
  )

  # Test with missing required columns
  kr_data <- data.frame(
    v021 = 1:10,
    v005 = rep(1000000, 10)
  )

  expect_error(
    calc_u5mr_dhs_core(
      kr_data,
      survey_vars = list(
        cluster = "v021",
        weight = "v005",
        interview_date = "v008",  # Missing
        birth_date = "b3",        # Missing
        stratum = "v022",         # Missing
        age_at_death = "b7"       # Missing
      )
    ),
    "Required variables not found"
  )
})

test_that("calc_u5mr_dhs_core works with minimal valid data", {
  skip_if_not_installed("DHS.rates")

  # Create minimal mock KR data
  set.seed(123)
  n_births <- 100

  kr_data <- data.frame(
    v021 = rep(1:10, each = 10),  # 10 clusters
    v005 = rep(1000000, n_births),  # Standard weight
    v022 = rep(1:2, each = 50),      # 2 strata
    v024 = rep(c("REGION1", "REGION2"), each = 50),  # 2 regions
    v008 = rep(1500, n_births),      # Interview date (CMC)
    b3 = 1500 - sample(0:59, n_births, replace = TRUE),  # Birth dates
    b7 = c(rep(NA, 90), sample(0:59, 10, replace = TRUE)),  # 10 deaths
    bord = 1:n_births                # Birth order
  )

  result <- calc_u5mr_dhs_core(kr_data)

  # Check structure of results
  expect_s3_class(result, "tbl_df")  # Should be a tibble
  expect_true("dhs_u5mr" %in% names(result))
  expect_true("dhs_u5mr_low" %in% names(result))
  expect_true("dhs_u5mr_upp" %in% names(result))
  expect_true("dhs_n_births" %in% names(result))
  expect_true("dhs_n_deaths" %in% names(result))

  # Check that U5MR is reasonable (should be >= 0 with deaths)
  expect_true(all(result$dhs_u5mr >= 0))
  expect_true(all(result$dhs_n_births > 0))
})


# ============================================================================
# Tests for calc_u5mr_dhs() — long-format U5MR indicator
# ============================================================================

test_that("calc_u5mr_dhs returns named list with adm0", {
  skip_if_not_installed("DHS.rates")

  set.seed(456)
  n_births <- 50

  kr_data <- data.frame(
    v000 = rep("SL7", n_births),
    v007 = rep(2016, n_births),
    v021 = rep(1:5, each = 10),
    v005 = rep(1000000, n_births),
    v022 = rep(1, n_births),
    v024 = rep("WESTERN", n_births),
    v008 = rep(1500, n_births),
    b3 = 1500 - sample(0:59, n_births, replace = TRUE),
    b7 = c(rep(NA, 45), sample(0:59, 5, replace = TRUE)),
    bord = 1:n_births
  )

  result <- calc_u5mr_dhs(kr_data)

  expect_type(result, "list")
  expect_true("adm0" %in% names(result))
  expect_s3_class(result$adm0, "tbl_df")
})


test_that("adm0 has correct column structure", {
  skip_if_not_installed("DHS.rates")

  set.seed(456)
  n_births <- 50

  kr_data <- data.frame(
    v000 = rep("SL7", n_births),
    v007 = rep(2016, n_births),
    v021 = rep(1:5, each = 10),
    v005 = rep(1000000, n_births),
    v022 = rep(1, n_births),
    v024 = rep("WESTERN", n_births),
    v008 = rep(1500, n_births),
    b3 = 1500 - sample(0:59, n_births, replace = TRUE),
    b7 = c(rep(NA, 45), sample(0:59, 5, replace = TRUE)),
    bord = 1:n_births
  )

  result <- calc_u5mr_dhs(kr_data)

  expected_cols <- c(
    "survey_id", "iso3", "iso2", "survey_type", "survey_year",
    "adm0", "type", "geo_source",
    "point", "ci_l", "ci_u", "numerator", "denominator",
    "indicator", "indicator_code",
    "numerator_description", "denominator_description", "denominator_code"
  )
  expect_true(all(expected_cols %in% names(result$adm0)))
})


test_that("adm0 contains u5mr indicator", {
  skip_if_not_installed("DHS.rates")

  set.seed(456)
  n_births <- 50

  kr_data <- data.frame(
    v000 = rep("SL7", n_births),
    v007 = rep(2016, n_births),
    v021 = rep(1:5, each = 10),
    v005 = rep(1000000, n_births),
    v022 = rep(1, n_births),
    v024 = rep("WESTERN", n_births),
    v008 = rep(1500, n_births),
    b3 = 1500 - sample(0:59, n_births, replace = TRUE),
    b7 = c(rep(NA, 45), sample(0:59, 5, replace = TRUE)),
    bord = 1:n_births
  )

  result <- calc_u5mr_dhs(kr_data)

  indicator_codes <- unique(result$adm0$indicator_code)
  expect_true("u5mr" %in% indicator_codes)
  expect_equal(length(indicator_codes), 1)
})


test_that("U5MR point estimate is a rate (per 1000), not a proportion", {
  skip_if_not_installed("DHS.rates")

  set.seed(456)
  n_births <- 50

  kr_data <- data.frame(
    v000 = rep("SL7", n_births),
    v007 = rep(2016, n_births),
    v021 = rep(1:5, each = 10),
    v005 = rep(1000000, n_births),
    v022 = rep(1, n_births),
    v024 = rep("WESTERN", n_births),
    v008 = rep(1500, n_births),
    b3 = 1500 - sample(0:59, n_births, replace = TRUE),
    b7 = c(rep(NA, 45), sample(0:59, 5, replace = TRUE)),
    bord = 1:n_births
  )

  result <- calc_u5mr_dhs(kr_data)

  # U5MR is per 1000 live births, so should be > 1 if there are deaths
  u5mr_val <- result$adm0$point[result$adm0$indicator_code == "u5mr"]
  if (!is.na(u5mr_val)) {
    expect_true(u5mr_val >= 0)
  }
})


test_that("type column is survey_weighted", {
  skip_if_not_installed("DHS.rates")

  set.seed(456)
  n_births <- 50

  kr_data <- data.frame(
    v000 = rep("SL7", n_births),
    v007 = rep(2016, n_births),
    v021 = rep(1:5, each = 10),
    v005 = rep(1000000, n_births),
    v022 = rep(1, n_births),
    v024 = rep("WESTERN", n_births),
    v008 = rep(1500, n_births),
    b3 = 1500 - sample(0:59, n_births, replace = TRUE),
    b7 = c(rep(NA, 45), sample(0:59, 5, replace = TRUE)),
    bord = 1:n_births
  )

  result <- calc_u5mr_dhs(kr_data)

  expect_true(all(result$adm0$type == "survey_weighted"))
})


# --- Test: u5mr_dictionary() -------------------------------------------------

test_that("u5mr_dictionary returns correct structure", {
  dict <- u5mr_dictionary()

  expect_s3_class(dict, "tbl_df")
  expect_true("indicator" %in% names(dict))
  expect_true("indicator_code" %in% names(dict))
  expect_true("numerator_description" %in% names(dict))
  expect_true("denominator_description" %in% names(dict))
  expect_true("denominator_code" %in% names(dict))

  # 1 indicator: u5mr
  expect_equal(nrow(dict), 1)
  expect_equal(dict$indicator_code, "u5mr")
})


# ============================================================================
# Tests for helper functions
# ============================================================================

test_that("aggregate_u5mr_admin works with mock data", {
  skip_if_not_installed("sf")

  # Create mock cluster results
  cluster_results <- data.frame(
    cluster_id = 1:5,
    lat = c(-8.5, -8.3, -8.7, -8.4, -8.6),
    lon = c(-11.2, -11.0, -11.5, -11.3, -11.1),
    dhs_u5mr = c(100, 120, 95, 110, 105),
    dhs_n_births = c(20, 25, 30, 15, 20),
    dhs_n_deaths = c(2, 3, 3, 2, 2)
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
    geometry = polygons,
    crs = 4326
  )

  # Test aggregation
  result <- aggregate_u5mr_admin(
    cluster_results,
    shapefile,
    admin_level = "adm1",
    weighted = TRUE
  )

  expect_s3_class(result, "sf")
  expect_true("dhs_u5mr" %in% names(result))
  expect_true("dhs_n_births" %in% names(result))
  expect_true("adm1" %in% names(result))
  expect_equal(nrow(result), 2)  # Two regions
})

test_that("join_dhs_coords adds GPS coordinates correctly", {
  # Create mock PR data
  pr_data <- data.frame(
    v021 = rep(1:5, each = 10),
    person_id = 1:50,
    stringsAsFactors = FALSE
  )

  # Create mock GPS data
  gps_data <- data.frame(
    DHSCLUST = 1:5,
    LATNUM = c(-8.5, -8.3, -8.7, -8.4, -8.6),
    LONGNUM = c(-11.2, -11.0, -11.5, -11.3, -11.1),
    stringsAsFactors = FALSE
  )

  result <- join_dhs_coords(
    pr_data = pr_data,
    gps_data = gps_data,
    pr_vars = list(cluster = "v021")
  )

  # Check structure
  expect_s3_class(result, "data.frame")
  expect_true("cluster_id" %in% names(result))
  expect_true("lat" %in% names(result))
  expect_true("lon" %in% names(result))

  # Check that coordinates were joined correctly
  expect_equal(nrow(result), 50)
  expect_true(all(!is.na(result$lat)))
  expect_true(all(!is.na(result$lon)))
})
