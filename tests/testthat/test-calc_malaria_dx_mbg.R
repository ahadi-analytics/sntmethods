# ---- Helper: create mock KR data with malaria_dx variables ----
.mock_kr_malaria_dx <- function(n = 200, n_clusters = 20, seed = 42) {
  set.seed(seed)
  kr <- data.frame(
    v001 = rep(seq_len(n_clusters), length.out = n),
    hw1 = sample(0:59, n, replace = TRUE),
    h22 = sample(c(0, 1), n, replace = TRUE, prob = c(0.6, 0.4)),
    h47 = NA_real_,
    stringsAsFactors = FALSE
  )
  febrile <- kr$h22 == 1
  kr$h47[febrile] <- sample(c(0, 1), sum(febrile), replace = TRUE, prob = c(0.5, 0.5))
  kr
}

.mock_gps_malaria_dx <- function(n_clusters = 20) {
  data.frame(
    DHSCLUST = seq_len(n_clusters),
    LATNUM = runif(n_clusters, -5, 5),
    LONGNUM = runif(n_clusters, 25, 35),
    stringsAsFactors = FALSE
  )
}


# ---- Input validation ----

test_that("calc_malaria_dx_mbg rejects non-dataframe inputs", {
  expect_error(
    calc_malaria_dx_mbg("not a df", data.frame()),
    "must be a data.frame"
  )

  expect_error(
    calc_malaria_dx_mbg(data.frame(v001 = 1), "not a df"),
    "must be a data.frame"
  )
})

test_that("calc_malaria_dx_mbg rejects invalid indicator names", {
  kr <- .mock_kr_malaria_dx()
  gps <- .mock_gps_malaria_dx()

  expect_error(
    calc_malaria_dx_mbg(kr, gps, indicators = c("malaria_dx", "bogus")),
    "Invalid indicators"
  )
})


# ---- Malaria Dx indicator computation ----

test_that("calc_malaria_dx_mbg computes malaria_dx indicator with valid data", {
  kr <- .mock_kr_malaria_dx()
  gps <- .mock_gps_malaria_dx()

  result <- calc_malaria_dx_mbg(kr, gps, indicators = "malaria_dx")

  expect_type(result, "list")
  expect_true("malaria_dx" %in% names(result))

  dt <- result[["malaria_dx"]]
  expect_s3_class(dt, "data.table")
  expect_true(all(c("cluster_id", "indicator", "samplesize", "x", "y") %in% names(dt)))

  # Numerator <= denominator
  expect_true(all(dt$indicator <= dt$samplesize))

  # Coordinates are non-zero
  expect_true(all(dt$x != 0))
  expect_true(all(dt$y != 0))
})

test_that("calc_malaria_dx_mbg uses ml1 fallback when h47 missing", {
  kr <- .mock_kr_malaria_dx()
  # Replace h47 with ml1
  kr$ml1 <- kr$h47
  kr$h47 <- NULL
  gps <- .mock_gps_malaria_dx()

  result <- calc_malaria_dx_mbg(kr, gps)

  expect_type(result, "list")
  expect_true("malaria_dx" %in% names(result))

  dt <- result[["malaria_dx"]]
  expect_s3_class(dt, "data.table")
  expect_true(all(dt$indicator <= dt$samplesize))
})

test_that("calc_malaria_dx_mbg returns empty list when neither h47 nor ml1 present", {
  kr <- .mock_kr_malaria_dx()
  kr$h47 <- NULL
  gps <- .mock_gps_malaria_dx()

  suppressWarnings(
    result <- calc_malaria_dx_mbg(kr, gps)
  )
  expect_equal(length(result), 0)
})

test_that("calc_malaria_dx_mbg returns empty list when h47 is all NA", {
  kr <- .mock_kr_malaria_dx()
  kr$h47 <- NA_real_
  gps <- .mock_gps_malaria_dx()

  suppressWarnings(
    result <- calc_malaria_dx_mbg(kr, gps)
  )
  expect_equal(length(result), 0)
})

test_that("calc_malaria_dx_mbg uses febrile children as denominator", {
  set.seed(123)
  kr <- .mock_kr_malaria_dx(n = 200, n_clusters = 10)
  gps <- .mock_gps_malaria_dx(n_clusters = 10)

  result <- calc_malaria_dx_mbg(kr, gps)
  dt <- result[["malaria_dx"]]

  total_sample <- sum(dt$samplesize)
  n_febrile <- sum(kr$h22 == 1)
  expect_equal(total_sample, n_febrile)
})


# ---- Output format ----

test_that("calc_malaria_dx_mbg output has correct column types", {
  kr <- .mock_kr_malaria_dx()
  gps <- .mock_gps_malaria_dx()

  result <- calc_malaria_dx_mbg(kr, gps, indicators = "malaria_dx")
  dt <- result[["malaria_dx"]]

  expect_true(is.numeric(dt$cluster_id) || is.integer(dt$cluster_id))
  expect_true(is.numeric(dt$indicator) || is.integer(dt$indicator))
  expect_true(is.numeric(dt$samplesize) || is.integer(dt$samplesize))
  expect_true(is.numeric(dt$x))
  expect_true(is.numeric(dt$y))
})


# ---- GPS filtering ----

test_that("calc_malaria_dx_mbg excludes (0,0) GPS coordinates", {
  kr <- .mock_kr_malaria_dx(n = 100, n_clusters = 10)
  gps <- .mock_gps_malaria_dx(n_clusters = 10)

  gps$LATNUM[1] <- 0
  gps$LONGNUM[1] <- 0

  result <- calc_malaria_dx_mbg(kr, gps)

  if ("malaria_dx" %in% names(result)) {
    dt <- result[["malaria_dx"]]
    expect_false(1 %in% dt$cluster_id)
  }
})


# ---- prep_malaria_dx_mbg ----

test_that("prep_malaria_dx_mbg returns single data.table", {
  kr <- .mock_kr_malaria_dx()
  gps <- .mock_gps_malaria_dx()

  dt <- prep_malaria_dx_mbg(kr, gps, indicator = "malaria_dx")

  expect_s3_class(dt, "data.table")
  expect_true(all(c("cluster_id", "indicator", "samplesize", "x", "y") %in% names(dt)))
})

test_that("prep_malaria_dx_mbg errors when no data available", {
  kr <- .mock_kr_malaria_dx()
  kr$h47 <- NULL
  gps <- .mock_gps_malaria_dx()

  expect_error(
    prep_malaria_dx_mbg(kr, gps),
    "No data returned"
  )
})
