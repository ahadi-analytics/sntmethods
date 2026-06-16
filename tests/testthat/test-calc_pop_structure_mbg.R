# ---- Helper: create mock PR (household member) roster ----
.mock_pr_roster <- function() {
  data.frame(
    hv001 = c(1, 1, 1, 1, 2, 2, 2, 2),
    # ages: u5 = {2, 4, 1, 3}; ov5 = {30, 8, 95(=95+)}; 98 = unknown (dropped)
    hv105 = c(2, 4, 30, 98, 1, 8, 95, 3),
    # last-but-one record (95) is not a usual resident -> dropped
    hv102 = c(1, 1, 1, 1, 1, 1, 0, 1),
    # hv025: cluster 1 is urban (1), cluster 2 is rural (2)
    hv025 = c(1, 1, 1, 1, 2, 2, 2, 2),
    stringsAsFactors = FALSE
  )
}

.mock_gps <- function() {
  data.frame(
    DHSCLUST = c(1, 2),
    LATNUM = c(8.1, 8.2),
    LONGNUM = c(-11.5, -11.4),
    stringsAsFactors = FALSE
  )
}


# ---- Input validation ----

test_that("calc_pop_structure_mbg rejects non-dataframe inputs", {
  expect_error(
    calc_pop_structure_mbg("not a df", data.frame()),
    "must be a data.frame"
  )
  expect_error(
    calc_pop_structure_mbg(data.frame(), "not a df"),
    "must be a data.frame"
  )
})

test_that("calc_pop_structure_mbg rejects invalid indicator codes", {
  expect_error(
    calc_pop_structure_mbg(
      .mock_pr_roster(), .mock_gps(),
      indicators = "prop_u7"
    ),
    "Invalid indicators"
  )
})

test_that("calc_pop_structure_mbg errors when age variable is absent", {
  pr <- .mock_pr_roster()
  pr$hv105 <- NULL
  expect_error(
    calc_pop_structure_mbg(pr, .mock_gps()),
    "Age variable"
  )
})


# ---- Core behaviour ----

test_that("calc_pop_structure_mbg returns one tibble per indicator", {
  res <- calc_pop_structure_mbg(.mock_pr_roster(), .mock_gps())

  expect_named(
    res,
    c(
      "prop_u5", "prop_ov5",
      "prop_u5_urban", "prop_u5_rural",
      "prop_ov5_urban", "prop_ov5_rural"
    ),
    ignore.order = TRUE
  )
  for (dt in res) {
    expect_true(all(
      c("cluster_id", "indicator", "samplesize", "x", "y") %in% names(dt)
    ))
  }
})


test_that("urban/rural strata restrict to the matching clusters", {
  res <- calc_pop_structure_mbg(.mock_pr_roster(), .mock_gps())

  # Cluster 1 is urban, cluster 2 is rural (hv025)
  expect_equal(res$prop_u5_urban$cluster_id, 1)
  expect_equal(res$prop_u5_rural$cluster_id, 2)

  # Urban + rural denominators reconstruct the all-residence denominator
  all_n <- sum(res$prop_u5$samplesize)
  strat_n <- sum(res$prop_u5_urban$samplesize) +
    sum(res$prop_u5_rural$samplesize)
  expect_equal(strat_n, all_n)

  # Urban u5 count (cluster 1: ages 2, 4) = 2
  expect_equal(res$prop_u5_urban$indicator, 2)
  expect_equal(res$prop_u5_urban$samplesize, 3)
})


test_that("strata are skipped when hv025 is absent (base indicators remain)", {
  pr <- .mock_pr_roster()
  pr$hv025 <- NULL
  res <- calc_pop_structure_mbg(pr, .mock_gps())
  expect_named(res, c("prop_u5", "prop_ov5"), ignore.order = TRUE)
})

test_that("de jure filtering and unknown-age codes drop the right records", {
  res <- calc_pop_structure_mbg(.mock_pr_roster(), .mock_gps())

  # Cluster 1: ages 2, 4, 30 (98 dropped) -> 3 known-age de jure members
  # Cluster 2: ages 1, 8, 3 (95 dropped, not de jure) -> 3 members
  u5 <- res$prop_u5[order(res$prop_u5$cluster_id), ]
  expect_equal(u5$samplesize, c(3, 3))
  expect_equal(u5$indicator, c(2, 2)) # u5 in cl1 = {2,4}; cl2 = {1,3}
})

test_that("u5 and ov5 numerators sum to the denominator per cluster", {
  res <- calc_pop_structure_mbg(.mock_pr_roster(), .mock_gps())

  u5 <- res$prop_u5[order(res$prop_u5$cluster_id), ]
  ov5 <- res$prop_ov5[order(res$prop_ov5$cluster_id), ]

  expect_equal(u5$samplesize, ov5$samplesize)
  expect_equal(u5$indicator + ov5$indicator, u5$samplesize)
})

test_that("hv105 = 95 is treated as 95+ (over 5), not dropped", {
  pr <- data.frame(
    hv001 = c(1, 1),
    hv105 = c(95, 2),
    hv102 = c(1, 1),
    stringsAsFactors = FALSE
  )
  res <- calc_pop_structure_mbg(pr, .mock_gps())
  expect_equal(res$prop_ov5$samplesize, 2)
  expect_equal(res$prop_ov5$indicator, 1) # the 95+ record counts as ov5
})

test_that("a single requested indicator returns only that element", {
  res <- calc_pop_structure_mbg(
    .mock_pr_roster(), .mock_gps(),
    indicators = "prop_u5"
  )
  expect_named(res, "prop_u5")
})


# ---- Metadata wiring ----

test_that("prop_u5 / prop_ov5 are registered MBG indicators", {
  valid <- .valid_mbg_indicators()
  expect_true(all(c("prop_u5", "prop_ov5", "pop_structure") %in% valid))

  meta <- .mbg_indicator_meta("prop_u5")
  expect_equal(meta$recode, "PR")
  expect_equal(meta$pop_type, "all") # weight by total population, not u5

  label <- .mbg_indicator_label("prop_u5")
  expect_match(label$indicator, "under 5")
})
