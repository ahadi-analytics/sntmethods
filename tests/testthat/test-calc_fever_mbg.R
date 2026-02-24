# ---- Helper: create mock KR data with fever variables ----
.mock_kr_fever <- function(n = 200, n_clusters = 20, seed = 42) {
  set.seed(seed)
  data.frame(
    v001 = rep(seq_len(n_clusters), length.out = n),
    hw1 = sample(0:59, n, replace = TRUE),
    h22 = sample(c(0, 1), n, replace = TRUE, prob = c(0.7, 0.3)),
    b5 = rep(1, n),
    stringsAsFactors = FALSE
  )
}

.mock_gps_fever <- function(n_clusters = 20) {
  data.frame(
    DHSCLUST = seq_len(n_clusters),
    LATNUM = runif(n_clusters, -5, 5),
    LONGNUM = runif(n_clusters, 25, 35),
    stringsAsFactors = FALSE
  )
}


# ---- Input validation ----

test_that("calc_fever_mbg rejects non-dataframe inputs", {
  expect_error(
    calc_fever_mbg("not a df", data.frame()),
    "must be a data.frame"
  )

  expect_error(
    calc_fever_mbg(data.frame(v001 = 1), "not a df"),
    "must be a data.frame"
  )
})

test_that("calc_fever_mbg rejects invalid indicator names", {
  kr <- .mock_kr_fever()
  gps <- .mock_gps_fever()

  expect_error(
    calc_fever_mbg(kr, gps, indicators = c("fever", "bogus")),
    "Invalid indicators"
  )
})


# ---- Fever indicator computation ----

test_that("calc_fever_mbg computes fever indicator with valid data", {
  kr <- .mock_kr_fever()
  gps <- .mock_gps_fever()

  result <- calc_fever_mbg(kr, gps, indicators = "fever")

  expect_type(result, "list")
  expect_true("fever" %in% names(result))

  dt <- result[["fever"]]
  expect_s3_class(dt, "tbl_df")
  expect_true(all(c("cluster_id", "indicator", "samplesize", "x", "y") %in% names(dt)))

  # Numerator <= denominator
  expect_true(all(dt$indicator <= dt$samplesize))

  # Coordinates are non-zero
  expect_true(all(dt$x != 0))
  expect_true(all(dt$y != 0))
})

test_that("calc_fever_mbg returns empty list when h22 is all NA", {
  kr <- .mock_kr_fever()
  kr$h22 <- NA_real_
  gps <- .mock_gps_fever()

  suppressWarnings(
    result <- calc_fever_mbg(kr, gps, indicators = "fever")
  )
  expect_equal(length(result), 0)
})

test_that("calc_fever_mbg uses ALL U5 children as denominator", {
  set.seed(123)
  kr <- .mock_kr_fever(n = 200, n_clusters = 10)
  gps <- .mock_gps_fever(n_clusters = 10)

  result <- calc_fever_mbg(kr, gps)
  dt <- result[["fever"]]

  # Total sample size across clusters should equal number of U5 children
  # (all children in mock data are U5 with b5=1)
  total_sample <- sum(dt$samplesize)
  expect_equal(total_sample, nrow(kr))
})

test_that("calc_fever_mbg filters to alive children", {
  set.seed(456)
  n <- 200
  kr <- data.frame(
    v001 = rep(1:20, each = 10),
    hw1 = sample(0:59, n, replace = TRUE),
    h22 = sample(c(0, 1), n, replace = TRUE, prob = c(0.7, 0.3)),
    b5 = sample(c(0, 1), n, replace = TRUE, prob = c(0.2, 0.8)),
    stringsAsFactors = FALSE
  )
  gps <- .mock_gps_fever()

  result <- calc_fever_mbg(kr, gps)
  dt <- result[["fever"]]

  n_alive <- sum(kr$b5 == 1)
  total_sample <- sum(dt$samplesize)
  expect_equal(total_sample, n_alive)
})


# ---- Output format ----

test_that("calc_fever_mbg output has correct column types", {
  kr <- .mock_kr_fever()
  gps <- .mock_gps_fever()

  result <- calc_fever_mbg(kr, gps, indicators = "fever")
  dt <- result[["fever"]]

  expect_true(is.numeric(dt$cluster_id) || is.integer(dt$cluster_id))
  expect_true(is.numeric(dt$indicator) || is.integer(dt$indicator))
  expect_true(is.numeric(dt$samplesize) || is.integer(dt$samplesize))
  expect_true(is.numeric(dt$x))
  expect_true(is.numeric(dt$y))
})


# ---- GPS filtering ----

test_that("calc_fever_mbg excludes (0,0) GPS coordinates", {
  kr <- .mock_kr_fever(n = 100, n_clusters = 10)
  gps <- .mock_gps_fever(n_clusters = 10)

  gps$LATNUM[1] <- 0
  gps$LONGNUM[1] <- 0

  result <- calc_fever_mbg(kr, gps, indicators = "fever")

  if ("fever" %in% names(result)) {
    dt <- result[["fever"]]
    expect_false(1 %in% dt$cluster_id)
  }
})


# ---- prep_fever_mbg ----

test_that("prep_fever_mbg returns single tibble", {
  kr <- .mock_kr_fever()
  gps <- .mock_gps_fever()

  dt <- prep_fever_mbg(kr, gps, indicator = "fever")

  expect_s3_class(dt, "tbl_df")
  expect_true(all(c("cluster_id", "indicator", "samplesize", "x", "y") %in% names(dt)))
})

test_that("prep_fever_mbg errors when no data available", {
  kr <- .mock_kr_fever()
  kr$h22 <- NA_real_
  gps <- .mock_gps_fever()

  expect_error(
    prep_fever_mbg(kr, gps, indicator = "fever"),
    "No data returned"
  )
})
