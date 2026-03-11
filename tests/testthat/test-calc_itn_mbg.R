# ============================================================================
# Tests for calc_itn_mbg() — dictionary-driven ITN MBG indicators
# ============================================================================

# ---- Shared mock data helpers ----

.mock_itn_hr <- function(n_hh = 40, n_clusters = 10, seed = 42) {
  set.seed(seed)
  data.frame(
    hv001 = rep(seq_len(n_clusters), length.out = n_hh),
    hhid = seq_len(n_hh),
    hv013 = sample(2:6, n_hh, replace = TRUE),
    hml10_1 = sample(c(0, 1), n_hh, replace = TRUE, prob = c(0.3, 0.7)),
    hml10_2 = sample(c(0, 1), n_hh, replace = TRUE, prob = c(0.5, 0.5)),
    stringsAsFactors = FALSE
  )
}

.mock_itn_pr <- function(hr, seed = 42) {
  set.seed(seed)
  n_hh <- nrow(hr)
  pr_rows <- list()
  for (i in seq_len(n_hh)) {
    hh_sz <- hr$hv013[i]
    for (j in seq_len(hh_sz)) {
      pr_rows <- c(pr_rows, list(data.frame(
        hv001 = hr$hv001[i],
        hhid = hr$hhid[i],
        hv105 = sample(c(0:70), 1),
        hv104 = sample(1:2, 1),
        hml12 = sample(c(0, 1, 2), 1, prob = c(0.3, 0.5, 0.2)),
        hml18 = sample(c(0, 1), 1, prob = c(0.85, 0.15)),
        stringsAsFactors = FALSE
      )))
    }
  }
  do.call(rbind, pr_rows)
}

.mock_itn_gps <- function(n_clusters = 10) {
  data.frame(
    DHSCLUST = seq_len(n_clusters),
    LATNUM = runif(n_clusters, -5, 5),
    LONGNUM = runif(n_clusters, 25, 35),
    stringsAsFactors = FALSE
  )
}


# ---- Input validation ----

test_that("calc_itn_mbg rejects non-dataframe inputs", {
  expect_error(
    calc_itn_mbg("not a df", data.frame(), data.frame()),
    "must be a data.frame"
  )
  expect_error(
    calc_itn_mbg(data.frame(), "not a df", data.frame()),
    "must be a data.frame"
  )
  expect_error(
    calc_itn_mbg(data.frame(), data.frame(), "not a df"),
    "must be a data.frame"
  )
})

test_that("calc_itn_mbg rejects invalid indicator names", {
  hr <- .mock_itn_hr()
  pr <- .mock_itn_pr(hr)
  gps <- .mock_itn_gps()

  expect_error(
    calc_itn_mbg(hr, pr, gps, indicators = c("access_itn", "bogus")),
    "Invalid indicators"
  )
})


# ---- Dictionary structure ----

test_that(".itn_mbg_dictionary returns all 10 indicators", {
  dict <- sntmethods:::.itn_mbg_dictionary()
  names <- vapply(dict, `[[`, character(1), "name")

  expect_equal(length(dict), 10)
  expect_equal(names, c(
    "with_itn", "enough_itn", "access_itn", "use_itn", "use_itn_chu5",
    "use_itn_preg", "use_itn_5_10", "use_itn_10_20",
    "use_itn_20plus", "use_itn_if_access"
  ))
})

test_that(".itn_mbg_dictionary entries have required fields", {
  dict <- sntmethods:::.itn_mbg_dictionary()

  for (spec in dict) {
    expect_true(all(c("name", "data_source", "outcome", "filter_col", "filter_val") %in% names(spec)))
    expect_true(spec$data_source %in% c("hr", "pr"))
  }
})

test_that(".itn_mbg_dictionary has correct data sources", {
  dict <- sntmethods:::.itn_mbg_dictionary()
  by_name <- stats::setNames(dict, vapply(dict, `[[`, character(1), "name"))

  # Only with_itn and enough_itn use HR; rest use PR

  expect_equal(by_name$with_itn$data_source, "hr")
  expect_equal(by_name$enough_itn$data_source, "hr")
  for (nm in setdiff(names(by_name), c("with_itn", "enough_itn"))) {
    expect_equal(by_name[[nm]]$data_source, "pr",
                 label = paste(nm, "data_source"))
  }
})


# ---- Default indicators (all) ----

test_that("calc_itn_mbg computes all indicators by default", {
  hr <- .mock_itn_hr()
  pr <- .mock_itn_pr(hr)
  gps <- .mock_itn_gps()

  result <- suppressMessages(calc_itn_mbg(hr, pr, gps))

  expect_type(result, "list")
  # At minimum with_itn, access_itn, use_itn should be present
  expect_true("with_itn" %in% names(result))
  expect_true("access_itn" %in% names(result))
  expect_true("use_itn" %in% names(result))
})


# ---- Individual indicators ----

test_that("calc_itn_mbg computes with_itn from HR data", {
  hr <- .mock_itn_hr()
  pr <- .mock_itn_pr(hr)
  gps <- .mock_itn_gps()

  result <- suppressMessages(calc_itn_mbg(hr, pr, gps, indicators = "with_itn"))

  expect_true("with_itn" %in% names(result))
  dt <- result[["with_itn"]]
  expect_s3_class(dt, "tbl_df")
  expect_true(all(c("cluster_id", "indicator", "samplesize", "x", "y") %in% names(dt)))
  expect_true(all(dt$indicator <= dt$samplesize))
})

test_that("calc_itn_mbg computes access_itn as binary indicator", {
  hr <- .mock_itn_hr()
  pr <- .mock_itn_pr(hr)
  gps <- .mock_itn_gps()

  result <- suppressMessages(calc_itn_mbg(hr, pr, gps, indicators = "access_itn"))

  expect_true("access_itn" %in% names(result))
  dt <- result[["access_itn"]]
  expect_true(all(dt$indicator <= dt$samplesize))
})

test_that("calc_itn_mbg computes use_itn from PR data", {
  hr <- .mock_itn_hr()
  pr <- .mock_itn_pr(hr)
  gps <- .mock_itn_gps()

  result <- suppressMessages(calc_itn_mbg(hr, pr, gps, indicators = "use_itn"))

  expect_true("use_itn" %in% names(result))
  dt <- result[["use_itn"]]
  expect_true(all(dt$indicator <= dt$samplesize))
})

test_that("calc_itn_mbg computes use_itn_chu5 for under-5 children", {
  hr <- .mock_itn_hr()
  pr <- .mock_itn_pr(hr)
  gps <- .mock_itn_gps()

  result <- suppressMessages(calc_itn_mbg(hr, pr, gps, indicators = "use_itn_chu5"))

  # May be absent if no under-5 children in mock data, but should not error
  if ("use_itn_chu5" %in% names(result)) {
    dt <- result[["use_itn_chu5"]]
    expect_true(all(dt$indicator <= dt$samplesize))
  }
})

test_that("calc_itn_mbg computes age-group indicators", {
  hr <- .mock_itn_hr(n_hh = 80, n_clusters = 10)
  pr <- .mock_itn_pr(hr, seed = 123)
  gps <- .mock_itn_gps()

  result <- suppressMessages(calc_itn_mbg(
    hr, pr, gps,
    indicators = c("use_itn_5_10", "use_itn_10_20", "use_itn_20plus")
  ))

  # At least one age group should be present with sufficient mock data
  age_inds <- intersect(
    c("use_itn_5_10", "use_itn_10_20", "use_itn_20plus"),
    names(result)
  )
  expect_true(length(age_inds) >= 1)

  for (nm in age_inds) {
    dt <- result[[nm]]
    expect_true(all(dt$indicator <= dt$samplesize))
  }
})

test_that("calc_itn_mbg computes use_itn_preg for pregnant women", {
  hr <- .mock_itn_hr()
  pr <- .mock_itn_pr(hr)
  gps <- .mock_itn_gps()

  # May warn about no pregnant women with small mock data
  result <- suppressMessages(suppressWarnings(
    calc_itn_mbg(hr, pr, gps, indicators = c("use_itn", "use_itn_preg"))
  ))

  # use_itn should always be present
  expect_true("use_itn" %in% names(result))

  # use_itn_preg may be absent if no pregnant women in mock
  if ("use_itn_preg" %in% names(result)) {
    dt <- result[["use_itn_preg"]]
    expect_true(all(dt$indicator <= dt$samplesize))
  }
})

test_that("calc_itn_mbg computes use_itn_if_access for those with access", {
  hr <- .mock_itn_hr()
  pr <- .mock_itn_pr(hr)
  gps <- .mock_itn_gps()

  result <- suppressMessages(calc_itn_mbg(hr, pr, gps, indicators = "use_itn_if_access"))

  if ("use_itn_if_access" %in% names(result)) {
    dt <- result[["use_itn_if_access"]]
    expect_true(all(dt$indicator <= dt$samplesize))
  }
})


# ---- Output format ----

test_that("calc_itn_mbg output has correct column types", {
  hr <- .mock_itn_hr()
  pr <- .mock_itn_pr(hr)
  gps <- .mock_itn_gps()

  result <- suppressMessages(calc_itn_mbg(hr, pr, gps, indicators = "with_itn"))
  dt <- result[["with_itn"]]

  expect_true(is.numeric(dt$cluster_id) || is.integer(dt$cluster_id))
  expect_true(is.numeric(dt$indicator) || is.integer(dt$indicator))
  expect_true(is.numeric(dt$samplesize) || is.integer(dt$samplesize))
  expect_true(is.numeric(dt$x))
  expect_true(is.numeric(dt$y))
})


# ---- GPS filtering ----

test_that("calc_itn_mbg excludes (0,0) GPS coordinates", {
  hr <- .mock_itn_hr()
  pr <- .mock_itn_pr(hr)
  gps <- .mock_itn_gps()

  # Set first cluster to (0,0)
  gps$LATNUM[1] <- 0
  gps$LONGNUM[1] <- 0

  result <- suppressMessages(calc_itn_mbg(hr, pr, gps, indicators = "with_itn"))

  if ("with_itn" %in% names(result)) {
    dt <- result[["with_itn"]]
    expect_false(1 %in% dt$cluster_id)
  }
})


# ---- Subset selection ----

test_that("calc_itn_mbg returns only requested indicators", {
  hr <- .mock_itn_hr()
  pr <- .mock_itn_pr(hr)
  gps <- .mock_itn_gps()

  requested <- c("with_itn", "use_itn")
  result <- suppressMessages(calc_itn_mbg(hr, pr, gps, indicators = requested))

  # No extra indicators beyond what was requested
  expect_true(all(names(result) %in% requested))
})


# ---- Denominator consistency ----

test_that("use_itn_chu5 samplesize <= use_itn samplesize", {
  hr <- .mock_itn_hr(n_hh = 80, n_clusters = 10)
  pr <- .mock_itn_pr(hr, seed = 99)
  gps <- .mock_itn_gps()

  result <- suppressMessages(calc_itn_mbg(
    hr, pr, gps,
    indicators = c("use_itn", "use_itn_chu5")
  ))

  if (all(c("use_itn", "use_itn_chu5") %in% names(result))) {
    all_ss <- sum(result[["use_itn"]]$samplesize)
    u5_ss <- sum(result[["use_itn_chu5"]]$samplesize)
    expect_true(u5_ss <= all_ss)
  }
})

test_that("use_itn_if_access samplesize <= use_itn samplesize", {
  hr <- .mock_itn_hr(n_hh = 80, n_clusters = 10)
  pr <- .mock_itn_pr(hr, seed = 99)
  gps <- .mock_itn_gps()

  result <- suppressMessages(calc_itn_mbg(
    hr, pr, gps,
    indicators = c("use_itn", "use_itn_if_access")
  ))

  if (all(c("use_itn", "use_itn_if_access") %in% names(result))) {
    all_ss <- sum(result[["use_itn"]]$samplesize)
    access_ss <- sum(result[["use_itn_if_access"]]$samplesize)
    expect_true(access_ss <= all_ss)
  }
})


# ---- prep_itn_mbg ----

test_that("prep_itn_mbg returns single tibble", {
  hr <- .mock_itn_hr()
  pr <- .mock_itn_pr(hr)
  gps <- .mock_itn_gps()

  dt <- suppressMessages(prep_itn_mbg(hr, pr, gps, indicator = "access_itn"))

  expect_s3_class(dt, "tbl_df")
  expect_true(all(c("cluster_id", "indicator", "samplesize", "x", "y") %in% names(dt)))
})


# ---- No ITN variables ----

test_that("calc_itn_mbg returns NULL when no ITN variables in HR", {
  hr <- data.frame(
    hv001 = 1:10,
    hhid = 1:10,
    hv013 = rep(3, 10),
    stringsAsFactors = FALSE
  )
  pr <- .mock_itn_pr(hr)
  gps <- .mock_itn_gps()

  result <- suppressMessages(suppressWarnings(
    calc_itn_mbg(hr, pr, gps, indicators = "with_itn")
  ))
  expect_null(result)
})
