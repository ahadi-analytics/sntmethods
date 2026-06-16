# Tests for calc_wealth_dhs_core(), calculate_dhs_gini(), .add_wealth_quintile()

make_wealth_core_mock <- function(n_hh = 600, seed = 7) {
  set.seed(seed)
  data.frame(
    hv001 = sample(1:50, n_hh, replace = TRUE),
    hv005 = sample(800000:1200000, n_hh, replace = TRUE),
    hv022 = sample(1:4, n_hh, replace = TRUE),
    hv024 = sample(1:3, n_hh, replace = TRUE),
    hv270 = sample(1:5, n_hh, replace = TRUE),
    hv271 = stats::rnorm(n_hh, 0, 1) * 1e5,
    hv012 = sample(3:8, n_hh, replace = TRUE),
    hv000 = "SL7",
    hv007 = 2019,
    stringsAsFactors = FALSE
  )
}

# --- calculate_dhs_gini ----------------------------------------------------

test_that("calculate_dhs_gini returns 0 when all wealth scores are equal", {
  out <- calculate_dhs_gini(
    wealth_scores = rep(1, 50),
    weights       = rep(1, 50),
    population    = rep(5, 50)
  )
  expect_equal(out, 0)
})

test_that("calculate_dhs_gini returns NA for too-few observations", {
  expect_warning(
    out <- calculate_dhs_gini(
      wealth_scores = c(1, 2, 3),
      weights       = c(1, 1, 1),
      population    = c(5, 5, 5)
    ),
    "fewer than 10"
  )
  expect_true(is.na(out))
})

test_that("calculate_dhs_gini returns NA when input is all NA", {
  out <- calculate_dhs_gini(
    wealth_scores = rep(NA_real_, 20),
    weights       = rep(1, 20),
    population    = rep(5, 20)
  )
  expect_true(is.na(out))
})

test_that("calculate_dhs_gini returns a value between 0 and 1 for varied input", {
  set.seed(123)
  out <- calculate_dhs_gini(
    wealth_scores = stats::rnorm(200, 0, 1),
    weights       = rep(1, 200),
    population    = sample(3:10, 200, replace = TRUE)
  )
  expect_true(!is.na(out))
  expect_gte(out, 0)
  expect_lte(out, 1)
})


# --- calc_wealth_dhs_core (no-GPS path) ----------------------------------

test_that("calc_wealth_dhs_core errors on non-data.frame input", {
  expect_error(calc_wealth_dhs_core("nope"), "must be a data.frame")
})

test_that("calc_wealth_dhs_core errors on empty data", {
  expect_error(calc_wealth_dhs_core(data.frame()), "is empty")
})

test_that("calc_wealth_dhs_core errors when survey_vars is missing required keys", {
  dat <- make_wealth_core_mock()
  expect_error(
    calc_wealth_dhs_core(
      dat,
      survey_vars = list(cluster = "hv001")
    ),
    "must include"
  )
})

test_that("calc_wealth_dhs_core errors when mapped columns are missing in data", {
  dat <- make_wealth_core_mock()
  dat$hv270 <- NULL
  expect_error(
    suppressMessages(calc_wealth_dhs_core(dat)),
    "Columns not found"
  )
})

test_that("calc_wealth_dhs_core returns adm-level quintile distributions", {
  dat <- make_wealth_core_mock()

  out <- suppressMessages(calc_wealth_dhs_core(dat))

  expect_s3_class(out, "data.frame")
  expect_true(all(
    c("dhs_prop_poorest", "dhs_prop_poorer", "dhs_prop_middle",
      "dhs_prop_richer", "dhs_prop_richest", "dhs_gini",
      "dhs_n_households", "dhs_weighted_households") %in% names(out)
  ))
  # quintile proportions should sum to approximately 1 within each row
  prop_cols <- c("dhs_prop_poorest", "dhs_prop_poorer", "dhs_prop_middle",
                 "dhs_prop_richer", "dhs_prop_richest")
  row_sums <- rowSums(out[, prop_cols], na.rm = TRUE)
  expect_true(all(row_sums > 0))
  expect_true(all(row_sums <= 1 + 1e-6))
})

test_that("calc_wealth_dhs_core falls back to national-level when no admin column in data", {
  dat <- make_wealth_core_mock()
  dat$hv024 <- NULL  # remove default admin column

  out <- suppressMessages(calc_wealth_dhs_core(dat))

  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 1)
  expect_true("level" %in% names(out) || "dhs_prop_poorest" %in% names(out))
})

test_that("calc_wealth_dhs_core warns on invalid quintile values", {
  dat <- make_wealth_core_mock()
  dat$hv270[1:5] <- 9  # invalid quintile
  expect_warning(
    suppressMessages(calc_wealth_dhs_core(dat)),
    "invalid"
  )
})


# --- .add_wealth_quintile -------------------------------------------------

test_that(".add_wealth_quintile autodetects hv270 / v190", {
  dat_hv <- data.frame(hv270 = c(1, 2, 3, 4, 5), x = 1:5)
  out <- suppressMessages(sntmethods:::.add_wealth_quintile(dat_hv))
  expect_equal(out$wealth_quintile, c(1, 2, 3, 4, 5))

  dat_v <- data.frame(v190 = c(1, 2, 3, 4, 5), x = 1:5)
  out <- suppressMessages(sntmethods:::.add_wealth_quintile(dat_v))
  expect_equal(out$wealth_quintile, c(1, 2, 3, 4, 5))
})

test_that(".add_wealth_quintile errors when neither v190 nor hv270 present", {
  dat <- data.frame(x = 1:5)
  expect_error(
    sntmethods:::.add_wealth_quintile(dat),
    "No wealth quintile"
  )
})

test_that(".add_wealth_quintile errors on missing explicit wealth_var", {
  dat <- data.frame(hv270 = c(1, 2, 3))
  expect_error(
    sntmethods:::.add_wealth_quintile(dat, wealth_var = "doesntexist"),
    "not found"
  )
})

test_that(".add_wealth_quintile drops NA quintile rows", {
  dat <- data.frame(hv270 = c(1, NA, 3, NA, 5), x = 1:5)
  out <- suppressMessages(sntmethods:::.add_wealth_quintile(dat))
  expect_equal(nrow(out), 3)
  expect_equal(out$wealth_quintile, c(1, 3, 5))
})

test_that(".add_wealth_quintile filters to requested quintiles", {
  dat <- data.frame(hv270 = c(1, 2, 3, 4, 5), x = 1:5)
  out <- suppressMessages(
    sntmethods:::.add_wealth_quintile(dat, quintiles = c(1, 5))
  )
  expect_setequal(out$wealth_quintile, c(1, 5))
})

test_that(".add_wealth_quintile aborts when nothing survives quintile filter", {
  dat <- data.frame(hv270 = c(1, 2, 3))
  expect_error(
    suppressMessages(
      sntmethods:::.add_wealth_quintile(dat, quintiles = c(5))
    ),
    "No observations remain"
  )
})
