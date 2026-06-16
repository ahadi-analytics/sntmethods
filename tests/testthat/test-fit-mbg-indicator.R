# ============================================================================
# Tests for fit_mbg_indicator() -- generic MBG fit wrapper
# ============================================================================
# These tests focus on argument validation, coercion, optional-metadata
# behaviour and filename resolution. The full INLA fit is not exercised
# (requires the INLA package and is expensive). Tests that would
# otherwise require running the model are skipped when {mbg} is not
# available, and use mocking when feasible.

# ---- Helpers ---------------------------------------------------------------

.skip_unless_spatial <- function() {
  testthat::skip_if_not_installed("terra")
  testthat::skip_if_not_installed("sf")
  testthat::skip_if_not_installed("fs")
  testthat::skip_if_not_installed("data.table")
  # fit_mbg_indicator() checks for the MBG engine up front (.check_pkg),
  # so every test that calls it needs these too. They are not installable
  # in CI (INLA is off-CRAN), so skip rather than error there.
  testthat::skip_if_not_installed("INLA")
  testthat::skip_if_not_installed("mbg")
}

.mock_clusters <- function(n = 20, seed = 1) {
  set.seed(seed)
  data.frame(
    cluster_id = seq_len(n),
    x          = stats::runif(n, 30, 31),
    y          = stats::runif(n, -2, -1),
    indicator  = sample(0:5, n, replace = TRUE),
    samplesize = sample(20:50, n, replace = TRUE)
  )
}

.mock_admin_sf <- function(n = 4, name_prefix = "Adm") {
  testthat::skip_if_not_installed("sf")
  # Build a 2x2 grid of unit squares as toy polygons
  side <- ceiling(sqrt(n))
  polys <- list()
  ids   <- character()
  nms   <- character()
  k <- 0
  for (i in seq_len(side)) {
    for (j in seq_len(side)) {
      k <- k + 1
      if (k > n) break
      x0 <- 30 + (i - 1) * 0.5
      y0 <- -2 + (j - 1) * 0.5
      poly <- sf::st_polygon(list(rbind(
        c(x0, y0), c(x0 + 0.5, y0),
        c(x0 + 0.5, y0 + 0.5), c(x0, y0 + 0.5),
        c(x0, y0)
      )))
      polys[[k]] <- poly
      ids[k]    <- paste0("ID", k)
      nms[k]    <- paste0(name_prefix, k)
    }
  }
  sf::st_sf(
    shapeID   = ids,
    shapeName = nms,
    geometry  = sf::st_sfc(polys, crs = 4326)
  )
}

.mock_pop_raster <- function(adm_sf) {
  testthat::skip_if_not_installed("terra")
  ext <- terra::ext(terra::vect(adm_sf))
  r <- terra::rast(ext, ncols = 20, nrows = 20, crs = "EPSG:4326")
  terra::values(r) <- runif(terra::ncell(r), 1, 100)
  r
}

# ---- Input validation ------------------------------------------------------

test_that("fit_mbg_indicator errors when no shapefile supplied", {
  .skip_unless_spatial()
  cd <- .mock_clusters()
  pop <- .mock_pop_raster(.mock_admin_sf(2))
  expect_error(
    fit_mbg_indicator(
      cluster_data      = cd,
      indicator_name    = "test",
      population_raster = pop
    ),
    "shapefile"
  )
})

test_that("fit_mbg_indicator errors when indicator_name is empty", {
  .skip_unless_spatial()
  cd <- .mock_clusters()
  adm2 <- .mock_admin_sf(4)
  pop <- .mock_pop_raster(adm2)
  expect_error(
    fit_mbg_indicator(
      cluster_data      = cd,
      indicator_name    = "",
      population_raster = pop,
      adm2_sf           = adm2
    ),
    "indicator_name"
  )
})

test_that("fit_mbg_indicator errors when population_raster is not SpatRaster", {
  .skip_unless_spatial()
  cd <- .mock_clusters()
  adm2 <- .mock_admin_sf(4)
  expect_error(
    fit_mbg_indicator(
      cluster_data      = cd,
      indicator_name    = "test",
      population_raster = "not a raster",
      adm2_sf           = adm2
    ),
    "SpatRaster"
  )
})

test_that("fit_mbg_indicator errors when adm shapefile is wrong class", {
  .skip_unless_spatial()
  cd <- .mock_clusters()
  pop <- .mock_pop_raster(.mock_admin_sf(2))
  expect_error(
    fit_mbg_indicator(
      cluster_data      = cd,
      indicator_name    = "test",
      population_raster = pop,
      adm2_sf           = data.frame(x = 1)
    ),
    "sf"
  )
})

test_that("fit_mbg_indicator errors when cluster_data missing required col", {
  .skip_unless_spatial()
  cd <- .mock_clusters()
  cd$indicator <- NULL
  adm2 <- .mock_admin_sf(4)
  pop <- .mock_pop_raster(adm2)
  expect_error(
    fit_mbg_indicator(
      cluster_data      = cd,
      indicator_name    = "test",
      population_raster = pop,
      adm2_sf           = adm2
    ),
    "indicator"
  )
})

test_that("fit_mbg_indicator errors when primary_level has no shapefile", {
  .skip_unless_spatial()
  cd <- .mock_clusters()
  adm2 <- .mock_admin_sf(4)
  pop <- .mock_pop_raster(adm2)
  expect_error(
    fit_mbg_indicator(
      cluster_data      = cd,
      indicator_name    = "test",
      population_raster = pop,
      adm2_sf           = adm2,
      primary_level     = "adm3"
    ),
    "primary_level"
  )
})

test_that("fit_mbg_indicator errors when no cleanable cluster rows remain", {
  .skip_unless_spatial()
  cd <- data.frame(
    cluster_id = 1:3,
    x          = c(NA, 0, NA),
    y          = c(NA, 0, NA),
    indicator  = c(1, 2, 3),
    samplesize = c(10, 20, 30)
  )
  adm2 <- .mock_admin_sf(4)
  pop <- .mock_pop_raster(adm2)
  expect_error(
    fit_mbg_indicator(
      cluster_data      = cd,
      indicator_name    = "test",
      population_raster = pop,
      adm2_sf           = adm2
    ),
    "0 usable rows"
  )
})

# ---- Cluster data coercion -------------------------------------------------

test_that("fit_mbg_indicator coerces tibble to data.table internally", {
  .skip_unless_spatial()
  testthat::skip_if_not_installed("tibble")
  # We exercise only the input-validation path by deliberately routing
  # to an early failure (no shapefile) so the coercion code runs but
  # we don't need INLA. The important assertion is that the call does
  # not error on coercion.
  cd <- tibble::as_tibble(.mock_clusters())
  pop <- .mock_pop_raster(.mock_admin_sf(2))
  expect_error(
    fit_mbg_indicator(
      cluster_data      = cd,
      indicator_name    = "test",
      population_raster = pop
    ),
    "shapefile"  # not a coercion error
  )
})

test_that("fit_mbg_indicator honours custom cluster_cols", {
  .skip_unless_spatial()
  cd <- data.frame(
    cid    = 1:5,
    lon    = stats::runif(5, 30, 31),
    lat    = stats::runif(5, -2, -1),
    pos    = c(1, 2, 3, 4, 5),
    n      = c(10, 20, 30, 40, 50)
  )
  pop <- .mock_pop_raster(.mock_admin_sf(2))
  # Should fail with no-shapefile error, NOT with column-rename error
  expect_error(
    fit_mbg_indicator(
      cluster_data      = cd,
      indicator_name    = "test",
      population_raster = pop,
      cluster_cols      = list(
        cluster_id = "cid", x = "lon", y = "lat",
        indicator = "pos", samplesize = "n"
      )
    ),
    "shapefile"
  )
})

test_that("fit_mbg_indicator errors clearly when cluster_cols misnames a col", {
  .skip_unless_spatial()
  cd <- .mock_clusters()
  adm2 <- .mock_admin_sf(4)
  pop <- .mock_pop_raster(adm2)
  expect_error(
    fit_mbg_indicator(
      cluster_data      = cd,
      indicator_name    = "test",
      population_raster = pop,
      adm2_sf           = adm2,
      cluster_cols      = list(
        cluster_id = "cluster_id", x = "x", y = "y",
        indicator = "MISSING_COL", samplesize = "samplesize"
      )
    ),
    "MISSING_COL"
  )
})

# ---- Function exists with expected signature -------------------------------

test_that("fit_mbg_indicator is exported with the documented arguments", {
  expect_true(exists("fit_mbg_indicator", mode = "function"))
  fmls <- names(formals(fit_mbg_indicator))
  required <- c(
    "cluster_data", "indicator_name", "population_raster",
    "adm0_sf", "adm1_sf", "adm2_sf", "adm3_sf",
    "primary_level", "output_levels",
    "covariates", "pixel_size", "n_samples", "seed",
    "cluster_cols", "id_field",
    "indicator_title", "indicator_unit_scale",
    "survey_year", "source_label",
    "output_dir", "cache_dir", "use_cache", "overwrite",
    "return_draws", "verbose"
  )
  for (arg in required) {
    expect_true(arg %in% fmls, info = paste0("missing arg: ", arg))
  }
  expect_true("..." %in% fmls)
})

test_that("optional metadata defaults are NULL", {
  fmls <- formals(fit_mbg_indicator)
  expect_null(eval(fmls$survey_year))
  expect_null(eval(fmls$source_label))
  expect_null(eval(fmls$output_dir))
  expect_null(eval(fmls$cache_dir))
})

test_that("country_iso3 is no longer a parameter", {
  fmls <- names(formals(fit_mbg_indicator))
  expect_false("country_iso3" %in% fmls)
})

# ---- Additional bug-sweep tests --------------------------------------------

test_that("fit_mbg_indicator rejects invalid primary_level values", {
  .skip_unless_spatial()
  cd <- .mock_clusters()
  adm2 <- .mock_admin_sf(4)
  pop <- .mock_pop_raster(adm2)
  expect_error(
    fit_mbg_indicator(
      cluster_data      = cd,
      indicator_name    = "test",
      population_raster = pop,
      adm2_sf           = adm2,
      primary_level     = "adm5"
    ),
    "primary_level"
  )
})

test_that("fit_mbg_indicator rejects invalid output_levels values", {
  .skip_unless_spatial()
  cd <- .mock_clusters()
  adm2 <- .mock_admin_sf(4)
  pop <- .mock_pop_raster(adm2)
  expect_error(
    fit_mbg_indicator(
      cluster_data      = cd,
      indicator_name    = "test",
      population_raster = pop,
      adm2_sf           = adm2,
      output_levels     = c("adm2", "adm9")
    ),
    "output_levels"
  )
})

test_that("fit_mbg_indicator detects cluster_cols rename collisions", {
  .skip_unless_spatial()
  cd <- .mock_clusters()
  cd$samplesize2 <- cd$samplesize
  adm2 <- .mock_admin_sf(4)
  pop <- .mock_pop_raster(adm2)
  expect_error(
    fit_mbg_indicator(
      cluster_data      = cd,
      indicator_name    = "test",
      population_raster = pop,
      adm2_sf           = adm2,
      cluster_cols      = list(
        cluster_id = "samplesize2",
        x = "x", y = "y",
        indicator = "indicator", samplesize = "samplesize"
      )
    ),
    "collision|already exists"
  )
})

test_that("fit_mbg_indicator drops sf geometry from cluster_data", {
  .skip_unless_spatial()
  testthat::skip_if_not_installed("sf")
  cd <- .mock_clusters()
  cd_sf <- sf::st_as_sf(cd, coords = c("x", "y"), crs = 4326, remove = FALSE)
  pop <- .mock_pop_raster(.mock_admin_sf(2))
  # Should fail with the no-shapefile error (not a geometry/data.table error)
  expect_error(
    fit_mbg_indicator(
      cluster_data      = cd_sf,
      indicator_name    = "test",
      population_raster = pop
    ),
    "shapefile"
  )
})
