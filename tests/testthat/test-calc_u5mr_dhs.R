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

test_that("calc_u5mr_dhs returns list with data, dict, and metadata", {
  skip_if_not_installed("DHS.rates")
  skip_if_not_installed("sntutils")

  # Create mock KR data
  set.seed(456)
  n_births <- 50

  kr_data <- data.frame(
    v000 = rep("SL7", n_births),     # Country code
    v007 = rep(2016, n_births),      # Survey year
    v021 = rep(1:5, each = 10),      # 5 clusters
    v005 = rep(1000000, n_births),   # Weight
    v022 = rep(1, n_births),         # Stratum
    v024 = rep("WESTERN", n_births), # Admin1
    v008 = rep(1500, n_births),      # Interview date
    b3 = 1500 - sample(0:59, n_births, replace = TRUE),
    b7 = c(rep(NA, 45), sample(0:59, 5, replace = TRUE)),
    bord = 1:n_births
  )

  result <- calc_u5mr_dhs(kr_data)

  # Check that result is a list with expected components
  expect_type(result, "list")
  expect_named(result, c("data", "dict", "metadata"))

  # Check data component
  expect_s3_class(result$data, "tbl_df")
  expect_true("dhs_u5mr" %in% names(result$data))

  # Check metadata component
  expect_type(result$metadata, "list")
  expect_equal(result$metadata$country_code, "SL7")
  expect_equal(result$metadata$survey_year, 2016)
  expect_equal(result$metadata$file_type, "KR")
  expect_equal(result$metadata$analysis_type, "U5MR (Under-5 Mortality Rate)")

  # Check dictionary component
  expect_s3_class(result$dict, "data.frame")
  expect_true("variable" %in% names(result$dict))
})

test_that("extract_dhs_metadata_kr extracts correct metadata", {
  kr_data <- data.frame(
    v000 = rep("TZ8", 100),
    v007 = rep(2022, 100),
    v021 = rep(1:10, each = 10),
    b7 = c(rep(NA, 95), rep(10, 5))  # 5 deaths
  )

  metadata <- extract_dhs_metadata_kr(
    kr_data,
    survey_vars = list(
      cluster = "v021",
      death_age = "b7"
    )
  )

  expect_equal(metadata$country_code, "TZ8")
  expect_equal(metadata$survey_year, 2022)
  expect_equal(metadata$file_type, "KR")
  expect_equal(metadata$total_records, 100)
  expect_equal(metadata$total_clusters, 10)
  expect_equal(metadata$total_births, 100)
  expect_equal(metadata$total_deaths_u5, 5)
})

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