# Tests for the sf-based admin aggregators:
#   aggregate_iptp_admin()
#   aggregate_severe_anemia_admin()
#   aggregate_wealth_admin()
#   aggregate_pfpr_admin()
# And the format-detection helper:
#   .detect_file_type_and_adjust_vars()

# Two adjacent square polygons covering (0,0)-(2,1).
# Region A: x in [0,1], Region B: x in [1,2].
make_two_region_shapefile <- function() {
  skip_if_not_installed("sf")
  poly_a <- sf::st_polygon(list(matrix(
    c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0), ncol = 2, byrow = TRUE
  )))
  poly_b <- sf::st_polygon(list(matrix(
    c(1, 0, 2, 0, 2, 1, 1, 1, 1, 0), ncol = 2, byrow = TRUE
  )))
  sf::st_sf(
    adm1      = c("A", "B"),
    adm1_name = c("Alpha", "Beta"),
    geometry  = sf::st_sfc(poly_a, poly_b, crs = 4326)
  )
}

# 4 cluster points: 2 in A, 2 in B
make_cluster_points <- function(extra_cols = list()) {
  base <- data.frame(
    cluster_id = 1:4,
    lat = c(0.25, 0.75, 0.25, 0.75),
    lon = c(0.25, 0.5, 1.25, 1.5)
  )
  for (col in names(extra_cols)) base[[col]] <- extra_cols[[col]]
  base
}


# --- aggregate_iptp_admin ---------------------------------------------------

test_that("aggregate_iptp_admin aggregates IPTp cluster values to adm1", {
  skip_if_not_installed("sf")
  shp <- make_two_region_shapefile()

  clusters <- make_cluster_points(list(
    dhs_iptp_1plus = c(0.7, 0.9, 0.4, 0.6),
    dhs_iptp_2plus = c(0.5, 0.7, 0.2, 0.3),
    dhs_iptp_3plus = c(0.3, 0.5, 0.1, 0.2),
    dhs_n_women    = c(50, 50, 30, 30)
  ))

  out <- suppressMessages(
    aggregate_iptp_admin(clusters, shp, admin_level = "adm1", weighted = TRUE)
  )

  expect_s3_class(out, "sf")
  expect_setequal(out$adm1, c("A", "B"))
  expect_true(all(c("dhs_iptp_1plus", "dhs_n_women") %in% names(out)))
  # Weighted: Region A has equal weights → average of (0.7, 0.9) = 0.8
  a_row <- out[out$adm1 == "A", ]
  expect_equal(a_row$dhs_iptp_1plus, 0.8)
})

test_that("aggregate_iptp_admin unweighted path also works", {
  skip_if_not_installed("sf")
  shp <- make_two_region_shapefile()

  clusters <- make_cluster_points(list(
    dhs_iptp_1plus = c(0.7, 0.9, 0.4, 0.6),
    dhs_iptp_2plus = c(0.5, 0.7, 0.2, 0.3),
    dhs_iptp_3plus = c(0.3, 0.5, 0.1, 0.2),
    dhs_n_women    = c(50, 50, 30, 30)
  ))

  out <- suppressMessages(
    aggregate_iptp_admin(clusters, shp, admin_level = "adm1", weighted = FALSE)
  )
  expect_setequal(out$adm1, c("A", "B"))
})

test_that("aggregate_iptp_admin requires lat/lon when not already sf", {
  skip_if_not_installed("sf")
  shp <- make_two_region_shapefile()
  bad <- data.frame(cluster_id = 1, dhs_iptp_1plus = 0.5)

  expect_error(
    aggregate_iptp_admin(bad, shp),
    "lat and lon"
  )
})


# --- aggregate_severe_anemia_admin -----------------------------------------

test_that("aggregate_severe_anemia_admin aggregates anemia cluster values", {
  skip_if_not_installed("sf")
  shp <- make_two_region_shapefile()

  clusters <- make_cluster_points(list(
    dhs_severe_anemia   = c(2.0, 4.0, 6.0, 8.0),
    dhs_n_tested_hb     = c(20, 20, 10, 10),
    dhs_n_severe_anemia = c(1, 2, 1, 1)
  ))

  out <- suppressMessages(
    aggregate_severe_anemia_admin(clusters, shp, admin_level = "adm1", weighted = TRUE)
  )

  expect_s3_class(out, "data.frame")
  expect_setequal(out$adm1, c("A", "B"))
  expect_true(all(c("dhs_severe_anemia", "dhs_n_tested_hb",
                    "dhs_n_severe_anemia") %in% names(out)))
  # Region A: equal n_tested_hb → weighted mean = 3.0
  a <- out[out$adm1 == "A", ]
  expect_equal(a$dhs_severe_anemia, 3.0)
})

test_that("aggregate_severe_anemia_admin unweighted path works", {
  skip_if_not_installed("sf")
  shp <- make_two_region_shapefile()

  clusters <- make_cluster_points(list(
    dhs_severe_anemia   = c(2.0, 4.0, 6.0, 8.0),
    dhs_n_tested_hb     = c(20, 20, 10, 10),
    dhs_n_severe_anemia = c(1, 2, 1, 1)
  ))

  out <- suppressMessages(
    aggregate_severe_anemia_admin(clusters, shp, admin_level = "adm1", weighted = FALSE)
  )
  expect_setequal(out$adm1, c("A", "B"))
})


# --- aggregate_wealth_admin ------------------------------------------------

test_that("aggregate_wealth_admin aggregates wealth quintile proportions", {
  skip_if_not_installed("sf")
  shp <- make_two_region_shapefile()

  clusters <- make_cluster_points(list(
    dhs_prop_poorest        = c(0.30, 0.40, 0.10, 0.05),
    dhs_prop_poorer         = c(0.20, 0.20, 0.20, 0.15),
    dhs_prop_middle         = c(0.20, 0.20, 0.20, 0.20),
    dhs_prop_richer         = c(0.15, 0.10, 0.25, 0.30),
    dhs_prop_richest        = c(0.15, 0.10, 0.25, 0.30),
    dhs_gini                = c(0.45, 0.50, 0.40, 0.42),
    dhs_n_households        = c(40, 60, 50, 70),
    dhs_weighted_households = c(40, 60, 50, 70),
    dhs_gini_sample_size    = c(40, 60, 50, 70)
  ))

  out <- suppressMessages(
    aggregate_wealth_admin(clusters, shp, admin_level = "adm1", weighted = TRUE)
  )
  expect_setequal(out$adm1, c("A", "B"))
  expect_true("dhs_dominant_quintile" %in% names(out))
})


# --- aggregate_pfpr_admin --------------------------------------------------

test_that("aggregate_pfpr_admin aggregates PfPR cluster values", {
  skip_if_not_installed("sf")
  shp <- make_two_region_shapefile()

  clusters <- make_cluster_points(list(
    dhs_pfpr_rdt     = c(0.20, 0.30, 0.10, 0.05),
    dhs_pfpr_mic     = c(0.15, 0.25, 0.08, 0.04),
    dhs_n_tested_rdt = c(50, 60, 30, 40),
    dhs_n_pos_rdt    = c(10, 18, 3, 2),
    dhs_n_tested_mic = c(50, 60, 30, 40),
    dhs_n_pos_mic    = c(8, 15, 2, 2)
  ))

  out <- suppressMessages(
    aggregate_pfpr_admin(clusters, shp, admin_level = "adm1", weighted = TRUE)
  )
  expect_setequal(out$adm1, c("A", "B"))
  expect_true(all(c("dhs_pfpr_rdt", "dhs_pfpr_mic",
                    "dhs_n_tested_rdt", "dhs_n_clusters") %in% names(out)))
})


# --- .detect_file_type_and_adjust_vars ------------------------------------

test_that(".detect_file_type_and_adjust_vars identifies IR data by default", {
  ir <- data.frame(
    ml1_1 = 1, m49a_1 = 1, v005 = 1, v021 = 1, v022 = 1, v024 = 1, v008 = 1
  )
  sv <- list(sp_doses = "ml1_1", sp_taken = "m49a_1",
             birth_cmc = "b3_01", interview_cmc = "v008")
  out <- suppressMessages(sntmethods:::.detect_file_type_and_adjust_vars(ir, sv))

  # No remapping for IR
  expect_equal(out$sp_doses, "ml1_1")
  expect_equal(out$sp_taken, "m49a_1")
})

test_that(".detect_file_type_and_adjust_vars remaps SP vars in KR data", {
  kr <- data.frame(
    ml1 = 1, m49a = 1, b3 = 1, v008 = 1, b8 = 1   # b8 is KR-specific
  )
  sv <- list(sp_doses = "ml1_1", sp_taken = "m49a_1",
             birth_cmc = NULL, interview_cmc = NULL)
  out <- suppressMessages(sntmethods:::.detect_file_type_and_adjust_vars(kr, sv))

  expect_equal(out$sp_doses, "ml1")
  expect_equal(out$sp_taken, "m49a")
  expect_equal(out$birth_date, "b3")
  expect_equal(out$interview_date, "v008")
})
