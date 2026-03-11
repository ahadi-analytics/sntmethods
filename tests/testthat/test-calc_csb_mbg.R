# ---- Helper: create mock KR data with h32 CSB variables ----
.mock_kr_csb_mbg <- function(n = 200, n_clusters = 20, seed = 42) {
  set.seed(seed)

  kr <- data.frame(
    v001 = rep(seq_len(n_clusters), length.out = n),
    hw1 = sample(0:59, n, replace = TRUE),
    h22 = sample(c(0, 1), n, replace = TRUE, prob = c(0.6, 0.4)),
    stringsAsFactors = FALSE
  )

  # Add h32 source variables — public
  kr$h32a <- ifelse(kr$h22 == 1,
    sample(c(0, 1), sum(kr$h22 == 1), replace = TRUE, prob = c(0.7, 0.3)), NA)
  kr$h32b <- ifelse(kr$h22 == 1,
    sample(c(0, 1), sum(kr$h22 == 1), replace = TRUE, prob = c(0.8, 0.2)), NA)

  # Private formal
  kr$h32j <- ifelse(kr$h22 == 1,
    sample(c(0, 1), sum(kr$h22 == 1), replace = TRUE, prob = c(0.85, 0.15)), NA)

  # Pharmacy
  kr$h32n <- ifelse(kr$h22 == 1,
    sample(c(0, 1), sum(kr$h22 == 1), replace = TRUE, prob = c(0.9, 0.1)), NA)

  # Private informal
  kr$h32s <- ifelse(kr$h22 == 1,
    sample(c(0, 1), sum(kr$h22 == 1), replace = TRUE, prob = c(0.95, 0.05)), NA)

  kr
}

.mock_gps_csb <- function(n_clusters = 20) {
  data.frame(
    DHSCLUST = seq_len(n_clusters),
    LATNUM = runif(n_clusters, -5, 5),
    LONGNUM = runif(n_clusters, 25, 35),
    stringsAsFactors = FALSE
  )
}


# ---- Input validation ----

test_that("calc_csb_mbg rejects non-dataframe inputs", {
  expect_error(
    calc_csb_mbg("not a df", data.frame()),
    "must be a data.frame"
  )

  expect_error(
    calc_csb_mbg(data.frame(v001 = 1), "not a df"),
    "must be a data.frame"
  )
})

test_that("calc_csb_mbg rejects invalid indicator names", {
  kr <- .mock_kr_csb_mbg()
  gps <- .mock_gps_csb()

  expect_error(
    calc_csb_mbg(kr, gps, indicators = c("public", "bogus")),
    "Invalid indicators"
  )
})


# ---- ACT is no longer a valid indicator ----

test_that("calc_csb_mbg rejects act and act_tested as indicators", {
  kr <- .mock_kr_csb_mbg()
  gps <- .mock_gps_csb()

  expect_error(
    calc_csb_mbg(kr, gps, indicators = "act"),
    "Invalid indicators"
  )

  expect_error(
    calc_csb_mbg(kr, gps, indicators = "act_tested"),
    "Invalid indicators"
  )
})


# ---- Basic indicator computation ----

test_that("calc_csb_mbg computes public indicator", {
  kr <- .mock_kr_csb_mbg()
  gps <- .mock_gps_csb()

  result <- calc_csb_mbg(kr, gps, indicators = "public")

  expect_type(result, "list")
  expect_true("csb_public" %in% names(result))

  dt <- result[["csb_public"]]
  expect_s3_class(dt, "tbl_df")
  expect_true(all(c("cluster_id", "indicator", "samplesize", "x", "y") %in% names(dt)))
  expect_true(all(dt$indicator <= dt$samplesize))
})

test_that("calc_csb_mbg computes private indicator", {
  kr <- .mock_kr_csb_mbg()
  gps <- .mock_gps_csb()

  result <- calc_csb_mbg(kr, gps, indicators = "private")
  expect_true("csb_private" %in% names(result))

  dt <- result[["csb_private"]]
  expect_true(all(dt$indicator <= dt$samplesize))
})

test_that("calc_csb_mbg computes none indicator", {
  kr <- .mock_kr_csb_mbg()
  gps <- .mock_gps_csb()

  result <- calc_csb_mbg(kr, gps, indicators = "none")
  expect_true("csb_none" %in% names(result))
})

test_that("calc_csb_mbg computes trained indicator", {
  kr <- .mock_kr_csb_mbg()
  gps <- .mock_gps_csb()

  result <- calc_csb_mbg(kr, gps, indicators = "trained")
  expect_true("csb_trained" %in% names(result))
})

test_that("calc_csb_mbg computes any indicator", {
  kr <- .mock_kr_csb_mbg()
  gps <- .mock_gps_csb()

  result <- calc_csb_mbg(kr, gps, indicators = "any")
  expect_true("csb_any" %in% names(result))

  dt <- result[["csb_any"]]
  expect_true(all(dt$indicator <= dt$samplesize))
})

test_that("calc_csb_mbg computes multiple indicators together", {
  kr <- .mock_kr_csb_mbg()
  gps <- .mock_gps_csb()

  result <- calc_csb_mbg(kr, gps, indicators = c("public", "private", "none"))

  expect_true("csb_public" %in% names(result))
  expect_true("csb_private" %in% names(result))
  expect_true("csb_none" %in% names(result))
})


# ---- Output format ----

test_that("calc_csb_mbg output has correct column types", {
  kr <- .mock_kr_csb_mbg()
  gps <- .mock_gps_csb()

  result <- calc_csb_mbg(kr, gps, indicators = "public")
  dt <- result[["csb_public"]]

  expect_true(is.numeric(dt$cluster_id) || is.integer(dt$cluster_id))
  expect_true(is.numeric(dt$indicator) || is.integer(dt$indicator))
  expect_true(is.numeric(dt$samplesize) || is.integer(dt$samplesize))
  expect_true(is.numeric(dt$x))
  expect_true(is.numeric(dt$y))
})


# ---- GPS filtering ----

test_that("calc_csb_mbg excludes (0,0) GPS coordinates", {
  kr <- .mock_kr_csb_mbg(n = 100, n_clusters = 10)
  gps <- .mock_gps_csb(n_clusters = 10)

  # Set first cluster to (0,0)
  gps$LATNUM[1] <- 0
  gps$LONGNUM[1] <- 0

  result <- calc_csb_mbg(kr, gps, indicators = "public")
  dt <- result[["csb_public"]]

  # Cluster 1 should be excluded
  expect_false(1 %in% dt$cluster_id)
})


# ---- prep_csb_mbg ----

test_that("prep_csb_mbg returns single tibble", {
  kr <- .mock_kr_csb_mbg()
  gps <- .mock_gps_csb()

  dt <- prep_csb_mbg(kr, gps, indicator = "public")

  expect_s3_class(dt, "tbl_df")
  expect_true(all(c("cluster_id", "indicator", "samplesize", "x", "y") %in% names(dt)))
})


# ---- Granular Long-format indicators ----

test_that("calc_csb_mbg computes pub_nochw indicator", {
  kr <- .mock_kr_csb_mbg()
  gps <- .mock_gps_csb()

  result <- calc_csb_mbg(
    kr, gps, indicators = "pub_nochw"
  )
  expect_true("csb_pub_nochw" %in% names(result))

  dt <- result[["csb_pub_nochw"]]
  expect_s3_class(dt, "tbl_df")
  expect_true(all(dt$indicator <= dt$samplesize))
})

test_that("calc_csb_mbg computes pharmacy indicator", {
  kr <- .mock_kr_csb_mbg()
  gps <- .mock_gps_csb()

  result <- calc_csb_mbg(
    kr, gps, indicators = "pharmacy"
  )
  expect_true("csb_pharmacy" %in% names(result))
})

test_that("calc_csb_mbg computes priv_formal indicator", {
  kr <- .mock_kr_csb_mbg()
  gps <- .mock_gps_csb()

  result <- calc_csb_mbg(
    kr, gps, indicators = "priv_formal"
  )
  expect_true("csb_priv_formal" %in% names(result))
})

test_that("calc_csb_mbg computes priv_informal indicator", {
  kr <- .mock_kr_csb_mbg()
  gps <- .mock_gps_csb()

  result <- calc_csb_mbg(
    kr, gps, indicators = "priv_informal"
  )
  expect_true("csb_priv_informal" %in% names(result))
})

test_that("calc_csb_mbg computes priv_form_pha indicator", {
  kr <- .mock_kr_csb_mbg()
  gps <- .mock_gps_csb()

  result <- calc_csb_mbg(
    kr, gps,
    indicators = "priv_form_pha"
  )
  expect_true(
    "csb_priv_form_pha" %in% names(result)
  )
})

test_that("calc_csb_mbg computes all 11 indicators together", {
  kr <- .mock_kr_csb_mbg()
  gps <- .mock_gps_csb()

  all_inds <- c(
    "any", "public", "pub_nochw", "chw",
    "private", "priv_formal", "pharmacy",
    "priv_informal", "priv_form_pha",
    "trained", "none"
  )
  result <- calc_csb_mbg(kr, gps, indicators = all_inds)

  # All should be present (mock data has public, pharmacy,
  # priv_formal, priv_informal)
  for (ind in c("csb_any", "csb_public", "csb_private",
                "csb_trained", "csb_none",
                "csb_pub_nochw", "csb_pharmacy",
                "csb_priv_formal",
                "csb_priv_informal",
                "csb_priv_form_pha")) {
    expect_true(
      ind %in% names(result),
      info = paste("Missing indicator:", ind)
    )
  }
})

test_that("pub_nochw is subset of public", {
  kr <- .mock_kr_csb_mbg()
  gps <- .mock_gps_csb()

  result <- calc_csb_mbg(
    kr, gps,
    indicators = c("public", "pub_nochw")
  )

  pub <- result[["csb_public"]]
  nochw <- result[["csb_pub_nochw"]]

  # Public includes CHW, so public >= public_nochw
  expect_true(
    sum(pub$indicator) >= sum(nochw$indicator)
  )
})


# ---- any + none consistency ----

test_that("calc_csb_mbg any + none equals total per cluster", {
  kr <- .mock_kr_csb_mbg()
  gps <- .mock_gps_csb()

  result <- calc_csb_mbg(kr, gps, indicators = c("any", "none"))

  any_dt <- result[["csb_any"]]
  none_dt <- result[["csb_none"]]

  # Merge by cluster
  merged <- merge(any_dt, none_dt, by = "cluster_id", suffixes = c("_any", "_none"))

  # any + none should equal samplesize (denominator is the same)
  expect_equal(merged$samplesize_any, merged$samplesize_none)
  expect_equal(
    merged$indicator_any + merged$indicator_none,
    merged$samplesize_any
  )
})
