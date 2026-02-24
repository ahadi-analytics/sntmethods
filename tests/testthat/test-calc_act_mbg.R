# ---- Helper: create mock KR data with ACT variables ----
.mock_kr_act <- function(n = 200, n_clusters = 20, seed = 42) {
  set.seed(seed)
  data.frame(
    v001 = rep(seq_len(n_clusters), length.out = n),
    hw1 = sample(0:59, n, replace = TRUE),
    h22 = sample(c(0, 1), n, replace = TRUE, prob = c(0.6, 0.4)),
    ml13e = sample(c(0, 1, NA), n, replace = TRUE, prob = c(0.4, 0.3, 0.3)),
    ml13a = sample(c(0, 1, NA), n, replace = TRUE, prob = c(0.5, 0.3, 0.2)),
    stringsAsFactors = FALSE
  )
}

.mock_gps <- function(n_clusters = 20) {
  data.frame(
    DHSCLUST = seq_len(n_clusters),
    LATNUM = runif(n_clusters, -5, 5),
    LONGNUM = runif(n_clusters, 25, 35),
    stringsAsFactors = FALSE
  )
}


# ---- Input validation ----

test_that("calc_act_mbg rejects non-dataframe inputs", {
  expect_error(
    calc_act_mbg("not a df", data.frame()),
    "must be a data.frame"
  )

  expect_error(
    calc_act_mbg(data.frame(v001 = 1), "not a df"),
    "must be a data.frame"
  )
})

test_that("calc_act_mbg rejects invalid indicator names", {
  kr <- .mock_kr_act()
  gps <- .mock_gps()

  expect_error(
    calc_act_mbg(kr, gps, indicators = c("act", "bogus")),
    "Invalid indicators"
  )
})


# ---- ACT indicator computation ----

test_that("calc_act_mbg computes act indicator with valid ml13e", {
  kr <- .mock_kr_act()
  gps <- .mock_gps()

  result <- calc_act_mbg(kr, gps, indicators = "act")

  expect_type(result, "list")
  expect_true("act" %in% names(result))

  dt <- result[["act"]]
  expect_s3_class(dt, "tbl_df")
  expect_true(all(c("cluster_id", "indicator", "samplesize", "x", "y") %in% names(dt)))

  # Numerator <= denominator

  expect_true(all(dt$indicator <= dt$samplesize))

  # Coordinates are non-zero
  expect_true(all(dt$x != 0))
  expect_true(all(dt$y != 0))
})

test_that("calc_act_mbg returns empty list when ml13e is all NA", {
  kr <- .mock_kr_act()
  kr$ml13e <- NA_real_
  gps <- .mock_gps()

  suppressWarnings(
    result <- calc_act_mbg(kr, gps, indicators = "act")
  )
  expect_equal(length(result), 0)
})

test_that("calc_act_mbg returns empty list when ACT variable missing", {
  kr <- .mock_kr_act()
  kr$ml13e <- NULL
  gps <- .mock_gps()

  suppressWarnings(
    result <- calc_act_mbg(kr, gps)
  )
  expect_equal(length(result), 0)
})


# ---- act_tested indicator ----

test_that("calc_act_mbg computes act_tested with valid data", {
  kr <- .mock_kr_act()
  gps <- .mock_gps()

  result <- calc_act_mbg(kr, gps, indicators = "act_tested")

  # act_tested may or may not be present depending on data
  if ("act_tested" %in% names(result)) {
    dt <- result[["act_tested"]]
    expect_s3_class(dt, "tbl_df")
    expect_true(all(c("cluster_id", "indicator", "samplesize", "x", "y") %in% names(dt)))
    expect_true(all(dt$indicator <= dt$samplesize))
  }
})

test_that("calc_act_mbg skips act_tested when test variable missing", {
  kr <- .mock_kr_act()
  kr$ml13a <- NULL
  gps <- .mock_gps()

  expect_message(
    result <- calc_act_mbg(kr, gps, indicators = "act_tested"),
    "not found"
  )
  # act_tested should not be in results
  expect_false("act_tested" %in% names(result))
})

test_that("calc_act_mbg skips act_tested when test variable is all NA", {
  kr <- .mock_kr_act()
  kr$ml13a <- NA_real_
  gps <- .mock_gps()

  expect_message(
    result <- calc_act_mbg(kr, gps, indicators = "act_tested"),
    "all NA"
  )
  expect_false("act_tested" %in% names(result))
})


# ---- Output format ----

test_that("calc_act_mbg output has correct column types", {
  kr <- .mock_kr_act()
  gps <- .mock_gps()

  result <- calc_act_mbg(kr, gps, indicators = "act")
  dt <- result[["act"]]

  expect_true(is.numeric(dt$cluster_id) || is.integer(dt$cluster_id))
  expect_true(is.numeric(dt$indicator) || is.integer(dt$indicator))
  expect_true(is.numeric(dt$samplesize) || is.integer(dt$samplesize))
  expect_true(is.numeric(dt$x))
  expect_true(is.numeric(dt$y))
})


# ---- GPS filtering ----

test_that("calc_act_mbg excludes (0,0) GPS coordinates", {
  kr <- .mock_kr_act(n = 100, n_clusters = 10)
  gps <- .mock_gps(n_clusters = 10)

  # Set first cluster to (0,0)
  gps$LATNUM[1] <- 0
  gps$LONGNUM[1] <- 0

  result <- calc_act_mbg(kr, gps, indicators = "act")

  if ("act" %in% names(result)) {
    dt <- result[["act"]]
    # Cluster 1 should be excluded
    expect_false(1 %in% dt$cluster_id)
  }
})


# ---- prep_act_mbg ----

test_that("prep_act_mbg returns single tibble", {
  kr <- .mock_kr_act()
  gps <- .mock_gps()

  dt <- prep_act_mbg(kr, gps, indicator = "act")

  expect_s3_class(dt, "tbl_df")
  expect_true(all(c("cluster_id", "indicator", "samplesize", "x", "y") %in% names(dt)))
})

test_that("prep_act_mbg errors when no data available", {
  kr <- .mock_kr_act()
  kr$ml13e <- NA_real_
  gps <- .mock_gps()

  expect_error(
    prep_act_mbg(kr, gps, indicator = "act"),
    "No data returned"
  )
})


# ---- .merge_kr_pr_febrile() unit tests ----

# Helper: minimal febrile KR data (output of .prepare_act_data())
.mock_kr_fever <- function(n = 5, seed = 1) {
  set.seed(seed)
  data.frame(
    cluster_id = 1:n,
    v001 = 1:n,          # geographic cluster (for PR linkage)
    v002 = rep(1, n),    # household number
    b16_01 = 1:n,        # child line number in household
    h22 = rep(1, n),     # had fever
    ml13e = rep(1, n),   # received ACT
    has_act = rep(1, n), # binary ACT indicator
    age_months = rep(12, n),
    stringsAsFactors = FALSE
  )
}

# Helper: minimal PR data
.mock_pr <- function(n = 5, rdt_values = c(0, 1, 0, 1, 0), include_hml35 = TRUE) {
  pr <- data.frame(
    hv001 = 1:n,   # cluster
    hv002 = rep(1, n), # household
    hvidx = 1:n,   # line number
    stringsAsFactors = FALSE
  )
  if (include_hml35) {
    pr$hml35 <- rdt_values
  }
  pr
}

test_that(".merge_kr_pr_febrile returns NULL when PR lacks hml35", {
  kr_fever <- .mock_kr_fever()
  dhs_pr <- .mock_pr(include_hml35 = FALSE)

  result <- suppressWarnings(
    sntmethods:::.merge_kr_pr_febrile(kr_fever, dhs_pr)
  )
  expect_null(result)
})

test_that(".merge_kr_pr_febrile returns NULL when no children match", {
  kr_fever <- .mock_kr_fever(n = 3)
  # PR data with completely different cluster IDs
  dhs_pr <- data.frame(
    hv001 = 99:101,
    hv002 = rep(1, 3),
    hvidx = rep(1, 3),
    hml35 = c(1, 0, 1),
    stringsAsFactors = FALSE
  )

  result <- suppressWarnings(
    sntmethods:::.merge_kr_pr_febrile(kr_fever, dhs_pr)
  )
  expect_null(result)
})

test_that(".merge_kr_pr_febrile filters hml35 to {0, 1} only", {
  kr_fever <- .mock_kr_fever(n = 5)
  # hml35: mix of valid (0/1) and invalid (2, 8, NA)
  dhs_pr <- .mock_pr(n = 5, rdt_values = c(0, 1, 2, 8, NA))

  result <- sntmethods:::.merge_kr_pr_febrile(kr_fever, dhs_pr)

  # Only children matched to valid RDT (0 or 1) — rows 1 and 2
  if (!is.null(result)) {
    expect_true(all(result$rdt_result %in% c(0, 1)))
  }
})

test_that(".merge_kr_pr_febrile sets has_rdt_pos == 1 only where hml35 == 1", {
  kr_fever <- .mock_kr_fever(n = 3)
  dhs_pr <- .mock_pr(n = 3, rdt_values = c(0, 1, 0))

  result <- sntmethods:::.merge_kr_pr_febrile(kr_fever, dhs_pr)

  expect_false(is.null(result))
  expect_true("has_rdt_pos" %in% names(result))
  expect_true(all(result$has_rdt_pos[result$rdt_result == 1] == 1L))
  expect_true(all(result$has_rdt_pos[result$rdt_result == 0] == 0L))
})

test_that(".merge_kr_pr_febrile preserves has_act column after merge", {
  kr_fever <- .mock_kr_fever(n = 3)
  kr_fever$has_act <- c(1, 0, 1)
  dhs_pr <- .mock_pr(n = 3, rdt_values = c(1, 0, 1))

  result <- sntmethods:::.merge_kr_pr_febrile(kr_fever, dhs_pr)

  expect_false(is.null(result))
  expect_true("has_act" %in% names(result))
  expect_equal(result$has_act, c(1, 0, 1))
})

test_that(".merge_kr_pr_febrile returns NULL when dhs_pr is not a data.frame", {
  kr_fever <- .mock_kr_fever()

  result <- suppressWarnings(
    sntmethods:::.merge_kr_pr_febrile(kr_fever, "not_a_df")
  )
  expect_null(result)
})

test_that("calc_act_mbg with dhs_pr = NULL for febrile_rdt_pos skips that indicator", {
  kr <- .mock_kr_act()
  gps <- .mock_gps()

  result <- suppressWarnings(
    calc_act_mbg(kr, gps, indicators = "febrile_rdt_pos", dhs_pr = NULL)
  )

  expect_false("febrile_rdt_pos" %in% names(result))
})


# ---- Both indicators together ----

test_that("calc_act_mbg computes both indicators together", {
  kr <- .mock_kr_act()
  gps <- .mock_gps()

  result <- calc_act_mbg(kr, gps, indicators = c("act", "act_tested"))

  expect_type(result, "list")
  # At minimum "act" should be present (act_tested depends on data)
  expect_true("act" %in% names(result))
})
