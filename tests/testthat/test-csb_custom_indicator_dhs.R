# Tests for the custom_csb_indicator parameter on the survey-weighted
# DHS pipeline (calc_csb_dhs_core / calc_csb_dhs).
#
# Mirrors test-csb_custom_indicator.R, which covers the MBG path.

# ---- Mock data helpers ----

.mock_kr_basic_dhs <- function(seed = 42, n_clusters = 20, per_cluster = 10) {
  set.seed(seed)
  n <- n_clusters * per_cluster

  df <- data.frame(
    v021 = rep(seq_len(n_clusters), each = per_cluster),
    v005 = rep(1000000, n),
    v022 = rep(seq_len(n_clusters / 5), each = per_cluster * 5)[seq_len(n)],
    v024 = rep("REGION1", n),
    hw1  = sample(0:59, n, replace = TRUE),
    h22  = rep(1, n),  # all febrile so denom == n
    b5   = rep(1, n),  # all alive
    stringsAsFactors = FALSE
  )

  # h32 source variables. Mutually exclusive 1's so the partition is clean.
  # Source counts (rows allocated to each):
  #   h32a -> dhis     : 40%
  #   h32j -> nondhis  : 30%
  #   h32n -> nondhis  : 10%
  #   h32x -> untreat  : 20%
  group <- sample(
    c("dhis_a", "nondhis_j", "nondhis_n", "untreat_x"),
    size = n,
    replace = TRUE,
    prob = c(0.4, 0.3, 0.1, 0.2)
  )
  df$h32a <- as.integer(group == "dhis_a")
  df$h32j <- as.integer(group == "nondhis_j")
  df$h32n <- as.integer(group == "nondhis_n")
  df$h32x <- as.integer(group == "untreat_x")
  df$h32y <- 0L  # "no treatment" meta slot
  df$h32z <- 0L  # "any source" meta slot

  attr(df$h32a, "label") <- "Government hospital"
  attr(df$h32j, "label") <- "Private hospital/clinic"
  attr(df$h32n, "label") <- "Pharmacy"
  attr(df$h32x, "label") <- "Other"
  attr(df$h32y, "label") <- "No treatment"
  attr(df$h32z, "label") <- "Medical treatment (any source)"

  df
}

.mock_spec_basic_dhs <- function() {
  list(
    name = "csb_eff",
    dhis_locs = c("Government hospital"),
    nondhis_locs = c("Private hospital/clinic", "Pharmacy"),
    untreat_locs = c("Other")
  )
}


# ---- calc_csb_dhs_core() with custom_csb_indicator ----

test_that("calc_csb_dhs_core emits three derived custom indicators in adm0", {
  skip_if_not_installed("survey")

  kr <- .mock_kr_basic_dhs()
  spec <- .mock_spec_basic_dhs()

  result <- calc_csb_dhs_core(kr, custom_csb_indicator = spec)

  expect_type(result, "list")
  expect_true("adm0" %in% names(result))

  adm0 <- result$adm0
  expect_s3_class(adm0, "tbl_df")

  # Built-in CSB rows still present
  expect_true("csb_any" %in% adm0$indicator_code)

  # Derived custom rows present
  expect_true("csb_eff_dhis" %in% adm0$indicator_code)
  expect_true("csb_eff_nondhis" %in% adm0$indicator_code)
  expect_true("csb_eff_untreat" %in% adm0$indicator_code)
})

test_that("custom CSB triple is a mutually exclusive partition (point sums to 1)", {
  skip_if_not_installed("survey")

  kr <- .mock_kr_basic_dhs()
  spec <- .mock_spec_basic_dhs()

  result <- calc_csb_dhs_core(kr, custom_csb_indicator = spec)
  adm0 <- result$adm0

  pts <- sapply(
    c("csb_eff_dhis", "csb_eff_nondhis", "csb_eff_untreat"),
    function(code) adm0$point[adm0$indicator_code == code][1]
  )

  expect_equal(unname(sum(pts)), 1, tolerance = 1e-8)
})

test_that("custom CSB point estimates approximately match raw proportions", {
  skip_if_not_installed("survey")

  kr <- .mock_kr_basic_dhs()
  spec <- .mock_spec_basic_dhs()

  # Expected raw proportions from the mock data
  raw_dhis    <- mean(kr$h32a == 1)
  raw_nondhis <- mean(kr$h32j == 1 | kr$h32n == 1)
  raw_untreat <- mean(kr$h32x == 1)

  result <- calc_csb_dhs_core(kr, custom_csb_indicator = spec)
  adm0 <- result$adm0

  pt <- function(code) adm0$point[adm0$indicator_code == code][1]

  # Allow a small tolerance to absorb survey weighting / rounding.
  expect_equal(pt("csb_eff_dhis"),    raw_dhis,    tolerance = 0.05)
  expect_equal(pt("csb_eff_nondhis"), raw_nondhis, tolerance = 0.05)
  expect_equal(pt("csb_eff_untreat"), raw_untreat, tolerance = 0.05)
})

test_that("calc_csb_dhs_core without custom_csb_indicator emits no custom rows", {
  skip_if_not_installed("survey")

  kr <- .mock_kr_basic_dhs()

  result <- calc_csb_dhs_core(kr)
  adm0 <- result$adm0

  expect_false(any(grepl("^csb_eff_", adm0$indicator_code)))
})

test_that("calc_csb_dhs_core validates custom_csb_indicator spec", {
  skip_if_not_installed("survey")

  kr <- .mock_kr_basic_dhs()

  bad_spec <- list(
    name = "Bad-Name",
    dhis_locs = "Government hospital",
    nondhis_locs = "Private hospital/clinic",
    untreat_locs = "Other"
  )
  expect_error(
    calc_csb_dhs_core(kr, custom_csb_indicator = bad_spec),
    regexp = "valid identifier"
  )

  collision <- .mock_spec_basic_dhs()
  collision$name <- "csb_any"
  expect_error(
    calc_csb_dhs_core(kr, custom_csb_indicator = collision),
    regexp = "collides with a built-in"
  )
})


# ---- calc_csb_dhs() public wrapper ----

test_that("calc_csb_dhs wrapper forwards custom_csb_indicator to the core", {
  skip_if_not_installed("survey")

  kr <- .mock_kr_basic_dhs()
  spec <- .mock_spec_basic_dhs()

  result <- calc_csb_dhs(kr, custom_csb_indicator = spec)
  adm0 <- result$adm0

  expect_true(all(
    c("csb_eff_dhis", "csb_eff_nondhis", "csb_eff_untreat") %in%
      adm0$indicator_code
  ))
})

test_that("calc_csb_dhs wrapper without custom spec returns only built-in CSB", {
  skip_if_not_installed("survey")

  kr <- .mock_kr_basic_dhs()

  result <- calc_csb_dhs(kr)
  adm0 <- result$adm0

  expect_true("csb_any" %in% adm0$indicator_code)
  expect_false(any(grepl("^csb_eff_", adm0$indicator_code)))
})


# ---- .custom_csb_dhs_conditions() helper shape ----

test_that(".custom_csb_dhs_conditions returns three condition specs", {
  spec <- sntmethods:::.validate_custom_csb_indicator_spec(
    .mock_spec_basic_dhs()
  )
  conds <- sntmethods:::.custom_csb_dhs_conditions(spec)

  expect_length(conds, 3L)

  codes <- vapply(conds, `[[`, character(1), "indicator_code")
  expect_setequal(
    codes,
    c("csb_eff_dhis", "csb_eff_nondhis", "csb_eff_untreat")
  )

  # Each condition must carry the keys consumed by .compute_csb_indicator()
  required_keys <- c(
    "indicator", "indicator_code", "indicator_title",
    "denom_code", "outcome_var", "num_desc", "denom_desc"
  )
  for (cond in conds) {
    expect_true(all(required_keys %in% names(cond)))
  }

  # outcome_var must equal indicator_code (column added by .classify_custom_csb_from_h32)
  for (cond in conds) {
    expect_equal(cond$outcome_var, cond$indicator_code)
  }

  # All three share the same denominator
  denom_codes <- vapply(conds, `[[`, character(1), "denom_code")
  expect_true(all(denom_codes == "feb_u5"))
})


# ---- Var-name spec mode (h32a / h32j / etc.) ----

test_that("validator accepts h32 variable names in *_locs", {
  spec <- list(
    name = "csb_eff",
    dhis_locs    = c("h32a"),
    nondhis_locs = c("h32j", "h32n"),
    untreat_locs = c("h32x")
  )
  validated <- sntmethods:::.validate_custom_csb_indicator_spec(spec)
  expect_equal(attr(validated, "vars_dhis"),    "h32a")
  expect_setequal(attr(validated, "vars_nondhis"), c("h32j", "h32n"))
  expect_equal(attr(validated, "vars_untreat"), "h32x")
  # No labels in this spec -> normalized label vectors are empty
  expect_length(attr(validated, "label_norm_dhis"),    0L)
  expect_length(attr(validated, "label_norm_nondhis"), 0L)
})

test_that("validator detects var-name overlap across buckets", {
  spec <- list(
    name = "csb_eff",
    dhis_locs    = c("h32a", "h32j"),  # h32j also in nondhis -> conflict
    nondhis_locs = c("h32j"),
    untreat_locs = c("h32x")
  )
  expect_error(
    sntmethods:::.validate_custom_csb_indicator_spec(spec),
    regexp = "overlap"
  )
})

test_that("calc_csb_dhs_core works end-to-end with var-name-only spec", {
  skip_if_not_installed("survey")

  kr <- .mock_kr_basic_dhs()
  var_spec <- list(
    name = "csb_eff",
    dhis_locs    = c("h32a"),
    nondhis_locs = c("h32j", "h32n"),
    untreat_locs = c("h32x")
  )

  result <- calc_csb_dhs_core(kr, custom_csb_indicator = var_spec)
  adm0 <- result$adm0

  expect_true(all(
    c("csb_eff_dhis", "csb_eff_nondhis", "csb_eff_untreat") %in%
      adm0$indicator_code
  ))

  # Same partition rule -> three points still sum to 1
  pts <- sapply(
    c("csb_eff_dhis", "csb_eff_nondhis", "csb_eff_untreat"),
    function(code) adm0$point[adm0$indicator_code == code][1]
  )
  expect_equal(unname(sum(pts)), 1, tolerance = 1e-8)
})

test_that("var-name and label specs produce identical results on the same data", {
  skip_if_not_installed("survey")

  kr <- .mock_kr_basic_dhs()

  label_spec <- .mock_spec_basic_dhs()
  var_spec <- list(
    name = "csb_eff",
    dhis_locs    = c("h32a"),
    nondhis_locs = c("h32j", "h32n"),
    untreat_locs = c("h32x")
  )

  res_lab <- calc_csb_dhs_core(kr, custom_csb_indicator = label_spec)$adm0
  res_var <- calc_csb_dhs_core(kr, custom_csb_indicator = var_spec)$adm0

  pick <- function(adm0, code) adm0$point[adm0$indicator_code == code][1]
  for (code in c("csb_eff_dhis", "csb_eff_nondhis", "csb_eff_untreat")) {
    expect_equal(pick(res_var, code), pick(res_lab, code), tolerance = 1e-10)
  }
})

test_that("mixed var-name and label spec resolves both styles", {
  skip_if_not_installed("survey")

  kr <- .mock_kr_basic_dhs()
  mixed_spec <- list(
    name = "csb_eff",
    dhis_locs    = c("h32a"),                              # var name
    nondhis_locs = c("Private hospital/clinic", "h32n"),   # mix
    untreat_locs = c("Other")                              # label
  )

  result <- calc_csb_dhs_core(kr, custom_csb_indicator = mixed_spec)
  adm0 <- result$adm0
  expect_true(all(
    c("csb_eff_dhis", "csb_eff_nondhis", "csb_eff_untreat") %in%
      adm0$indicator_code
  ))

  pts <- sapply(
    c("csb_eff_dhis", "csb_eff_nondhis", "csb_eff_untreat"),
    function(code) adm0$point[adm0$indicator_code == code][1]
  )
  expect_equal(unname(sum(pts)), 1, tolerance = 1e-8)
})

test_that("var-name routing disambiguates duplicate labels (h32e/h32n case)", {
  spec <- list(
    name = "csb_eff",
    dhis_locs    = c("h32e"),     # public CHW
    nondhis_locs = c("h32n"),     # private CHW (same label!)
    untreat_locs = c("h32x")
  )

  # Mock: h32e and h32n share the same haven label
  kr <- data.frame(
    h32e = c(1, 0, 0),
    h32n = c(0, 1, 0),
    h32x = c(0, 0, 1),
    h32y = c(0, 0, 0),
    h32z = c(0, 0, 0),
    stringsAsFactors = FALSE
  )
  attr(kr$h32e, "label") <- "Fever/cough: comm.health wrkr"
  attr(kr$h32n, "label") <- "Fever/cough: comm.health wrkr"
  attr(kr$h32x, "label") <- "Fever/cough: Other"

  classification <- sntmethods:::.build_custom_csb_classification(kr, spec)

  expect_equal(
    classification$csb_custom[classification$variable == "h32e"],
    "dhis"
  )
  expect_equal(
    classification$csb_custom[classification$variable == "h32n"],
    "nondhis"
  )
  expect_equal(
    classification$csb_custom[classification$variable == "h32x"],
    "untreat"
  )
})

test_that("var name takes precedence over a conflicting label match", {
  # h32a is in dhis_locs by name. The Government hospital LABEL is also
  # listed under nondhis_locs. Var-name routing must win, so h32a -> dhis.
  spec <- list(
    name = "csb_eff",
    dhis_locs    = c("h32a"),
    nondhis_locs = c("Government hospital", "h32j"),
    untreat_locs = c("Other")
  )

  kr <- data.frame(
    h32a = c(1, 0, 0),
    h32j = c(0, 1, 0),
    h32x = c(0, 0, 1),
    h32y = c(0, 0, 0),
    h32z = c(0, 0, 0),
    stringsAsFactors = FALSE
  )
  attr(kr$h32a, "label") <- "Government hospital"
  attr(kr$h32j, "label") <- "Private hospital/clinic"
  attr(kr$h32x, "label") <- "Other"

  classification <- sntmethods:::.build_custom_csb_classification(kr, spec)
  expect_equal(
    classification$csb_custom[classification$variable == "h32a"],
    "dhis"
  )
})

test_that("unmapped h32 column with neither var name nor label still errors", {
  spec <- list(
    name = "csb_eff",
    dhis_locs    = c("h32a"),
    nondhis_locs = c("h32j"),
    untreat_locs = c("h32x")
  )

  kr <- data.frame(
    h32a = c(1, 0, 0, 0),
    h32j = c(0, 1, 0, 0),
    h32x = c(0, 0, 1, 0),
    h32q = c(0, 0, 0, 1),  # not classified anywhere
    h32y = c(0, 0, 0, 0),
    h32z = c(0, 0, 0, 0),
    stringsAsFactors = FALSE
  )
  attr(kr$h32a, "label") <- "Government hospital"
  attr(kr$h32j, "label") <- "Private hospital/clinic"
  attr(kr$h32x, "label") <- "Other"
  attr(kr$h32q, "label") <- "Some new place"

  expect_error(
    sntmethods:::.build_custom_csb_classification(kr, spec),
    regexp = "does not classify"
  )
})
