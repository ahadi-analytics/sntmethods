# ---- Helper: create mock KR data with antimalarial variables ----
.mock_kr_antimalarial <- function(n = 200, n_clusters = 20, seed = 42) {
  set.seed(seed)
  kr <- data.frame(
    v001 = rep(seq_len(n_clusters), length.out = n),
    hw1 = sample(0:59, n, replace = TRUE),
    h22 = sample(c(0, 1), n, replace = TRUE, prob = c(0.6, 0.4)),
    ml13a = NA_real_,
    ml13b = NA_real_,
    ml13e = NA_real_,
    stringsAsFactors = FALSE
  )
  febrile <- kr$h22 == 1
  kr$ml13a[febrile] <- sample(c(0, 1), sum(febrile), replace = TRUE, prob = c(0.8, 0.2))
  kr$ml13b[febrile] <- sample(c(0, 1), sum(febrile), replace = TRUE, prob = c(0.9, 0.1))
  kr$ml13e[febrile] <- sample(c(0, 1), sum(febrile), replace = TRUE, prob = c(0.7, 0.3))
  kr
}

.mock_gps_antimalarial <- function(n_clusters = 20) {
  data.frame(
    DHSCLUST = seq_len(n_clusters),
    LATNUM = runif(n_clusters, -5, 5),
    LONGNUM = runif(n_clusters, 25, 35),
    stringsAsFactors = FALSE
  )
}


# ---- Input validation ----

test_that("calc_antimalarial_mbg rejects non-dataframe inputs", {
  expect_error(
    calc_antimalarial_mbg("not a df", data.frame()),
    "must be a data.frame"
  )

  expect_error(
    calc_antimalarial_mbg(data.frame(v001 = 1), "not a df"),
    "must be a data.frame"
  )
})

test_that("calc_antimalarial_mbg rejects invalid indicator names", {
  kr <- .mock_kr_antimalarial()
  gps <- .mock_gps_antimalarial()

  expect_error(
    calc_antimalarial_mbg(kr, gps, indicators = c("antimalarial", "bogus")),
    "Invalid indicators"
  )
})


# ---- Antimalarial indicator computation ----

test_that("calc_antimalarial_mbg computes antimalarial indicator with valid data", {
  kr <- .mock_kr_antimalarial()
  gps <- .mock_gps_antimalarial()

  result <- calc_antimalarial_mbg(kr, gps, indicators = "antimalarial")

  expect_type(result, "list")
  expect_true("antimalarial" %in% names(result))

  dt <- result[["antimalarial"]]
  expect_s3_class(dt, "tbl_df")
  expect_true(all(c("cluster_id", "indicator", "samplesize", "x", "y") %in% names(dt)))

  # Numerator <= denominator
  expect_true(all(dt$indicator <= dt$samplesize))

  # Coordinates are non-zero
  expect_true(all(dt$x != 0))
  expect_true(all(dt$y != 0))
})

test_that("calc_antimalarial_mbg returns empty list when no ml13 variables", {
  set.seed(42)
  kr <- data.frame(
    v001 = rep(1:20, each = 10),
    hw1 = sample(0:59, 200, replace = TRUE),
    h22 = sample(c(0, 1), 200, replace = TRUE, prob = c(0.6, 0.4)),
    stringsAsFactors = FALSE
  )
  gps <- .mock_gps_antimalarial()

  suppressWarnings(
    result <- calc_antimalarial_mbg(kr, gps)
  )
  expect_equal(length(result), 0)
})

test_that("calc_antimalarial_mbg returns empty list when all ml13 are NA", {
  kr <- .mock_kr_antimalarial()
  kr$ml13a <- NA_real_
  kr$ml13b <- NA_real_
  kr$ml13e <- NA_real_
  gps <- .mock_gps_antimalarial()

  suppressWarnings(
    result <- calc_antimalarial_mbg(kr, gps)
  )
  expect_equal(length(result), 0)
})

test_that("calc_antimalarial_mbg uses febrile children as denominator", {
  set.seed(123)
  kr <- .mock_kr_antimalarial(n = 200, n_clusters = 10)
  gps <- .mock_gps_antimalarial(n_clusters = 10)

  result <- calc_antimalarial_mbg(kr, gps)
  dt <- result[["antimalarial"]]

  total_sample <- sum(dt$samplesize)
  n_febrile <- sum(kr$h22 == 1)
  expect_equal(total_sample, n_febrile)
})

test_that("calc_antimalarial_mbg composite indicator counts any ml13", {
  set.seed(456)
  n <- 100
  kr <- data.frame(
    v001 = rep(1:10, each = 10),
    hw1 = sample(0:59, n, replace = TRUE),
    h22 = rep(1, n),  # All febrile
    ml13a = rep(0, n),
    ml13e = rep(0, n),
    stringsAsFactors = FALSE
  )
  # 30 have ml13a only, 20 have ml13e only
  kr$ml13a[1:30] <- 1
  kr$ml13e[31:50] <- 1

  gps <- .mock_gps_antimalarial(n_clusters = 10)

  result <- calc_antimalarial_mbg(kr, gps)
  dt <- result[["antimalarial"]]

  total_positive <- sum(dt$indicator)
  expect_equal(total_positive, 50)
})


# ---- antimalarial_public indicator ----

# Helper: mock KR data with h32 columns for CSB classification
.mock_kr_antimalarial_with_csb <- function(n = 200, n_clusters = 20, seed = 42) {
  set.seed(seed)
  kr <- data.frame(
    v001 = rep(seq_len(n_clusters), length.out = n),
    hw1 = sample(0:59, n, replace = TRUE),
    h22 = sample(c(0, 1), n, replace = TRUE, prob = c(0.6, 0.4)),
    ml13a = NA_real_,
    ml13b = NA_real_,
    ml13e = NA_real_,
    h32a = NA_real_,
    h32j = NA_real_,
    stringsAsFactors = FALSE
  )
  febrile <- kr$h22 == 1
  kr$ml13a[febrile] <- sample(c(0, 1), sum(febrile), replace = TRUE, prob = c(0.8, 0.2))
  kr$ml13b[febrile] <- sample(c(0, 1), sum(febrile), replace = TRUE, prob = c(0.9, 0.1))
  kr$ml13e[febrile] <- sample(c(0, 1), sum(febrile), replace = TRUE, prob = c(0.7, 0.3))
  kr$h32a[febrile] <- sample(c(0, 1), sum(febrile), replace = TRUE, prob = c(0.4, 0.6))
  kr$h32j[febrile] <- sample(c(0, 1), sum(febrile), replace = TRUE, prob = c(0.7, 0.3))
  kr
}

test_that("calc_antimalarial_mbg computes antimalarial_public with h32 variables", {
  kr <- .mock_kr_antimalarial_with_csb()
  gps <- .mock_gps_antimalarial()

  result <- calc_antimalarial_mbg(
    kr, gps, indicators = c("antimalarial", "antimalarial_public")
  )

  expect_type(result, "list")
  expect_true("antimalarial" %in% names(result))
  expect_true("antimalarial_public" %in% names(result))

  dt <- result[["antimalarial_public"]]
  expect_s3_class(dt, "tbl_df")
  expect_true(all(c("cluster_id", "indicator", "samplesize", "x", "y") %in% names(dt)))

  # Numerator <= denominator
  expect_true(all(dt$indicator <= dt$samplesize))

  # Public sample size <= overall sample size
  am_dt <- result[["antimalarial"]]
  expect_true(sum(dt$samplesize) <= sum(am_dt$samplesize))
})

test_that("calc_antimalarial_mbg skips antimalarial_public when no h32 variables", {
  kr <- .mock_kr_antimalarial()  # No h32 columns
  gps <- .mock_gps_antimalarial()

  result <- suppressWarnings(
    calc_antimalarial_mbg(
      kr, gps, indicators = c("antimalarial", "antimalarial_public")
    )
  )

  expect_true("antimalarial" %in% names(result))
  expect_false("antimalarial_public" %in% names(result))
})


# ---- Output format ----

test_that("calc_antimalarial_mbg output has correct column types", {
  kr <- .mock_kr_antimalarial()
  gps <- .mock_gps_antimalarial()

  result <- calc_antimalarial_mbg(kr, gps, indicators = "antimalarial")
  dt <- result[["antimalarial"]]

  expect_true(is.numeric(dt$cluster_id) || is.integer(dt$cluster_id))
  expect_true(is.numeric(dt$indicator) || is.integer(dt$indicator))
  expect_true(is.numeric(dt$samplesize) || is.integer(dt$samplesize))
  expect_true(is.numeric(dt$x))
  expect_true(is.numeric(dt$y))
})


# ---- GPS filtering ----

test_that("calc_antimalarial_mbg excludes (0,0) GPS coordinates", {
  kr <- .mock_kr_antimalarial(n = 100, n_clusters = 10)
  gps <- .mock_gps_antimalarial(n_clusters = 10)

  gps$LATNUM[1] <- 0
  gps$LONGNUM[1] <- 0

  result <- calc_antimalarial_mbg(kr, gps)

  if ("antimalarial" %in% names(result)) {
    dt <- result[["antimalarial"]]
    expect_false(1 %in% dt$cluster_id)
  }
})


# ---- prep_antimalarial_mbg ----

test_that("prep_antimalarial_mbg returns single tibble", {
  kr <- .mock_kr_antimalarial()
  gps <- .mock_gps_antimalarial()

  dt <- prep_antimalarial_mbg(kr, gps, indicator = "antimalarial")

  expect_s3_class(dt, "tbl_df")
  expect_true(all(c("cluster_id", "indicator", "samplesize", "x", "y") %in% names(dt)))
})

test_that("prep_antimalarial_mbg errors when no data available", {
  set.seed(42)
  kr <- data.frame(
    v001 = rep(1:20, each = 10),
    hw1 = sample(0:59, 200, replace = TRUE),
    h22 = sample(c(0, 1), 200, replace = TRUE),
    stringsAsFactors = FALSE
  )
  gps <- .mock_gps_antimalarial()

  expect_error(
    prep_antimalarial_mbg(kr, gps),
    "No data returned|No ml13"
  )
})
