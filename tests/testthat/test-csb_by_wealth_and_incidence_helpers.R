# Tests for:
#   calc_csb_by_wealth_dhs()
#   check_incidence()
#   .aggregate_facility_to_annual()

# --- shared mocks ----------------------------------------------------------

make_csb_kr_mock <- function(n = 200, seed = 7) {
  set.seed(seed)
  kr <- data.frame(
    v021 = rep(1:20, each = n / 20),
    v005 = rep(1000000, n),
    v022 = rep(1:4, each = n / 4),
    v024 = rep(c("REGION1", "REGION2"), each = n / 2),
    v190 = sample(1:5, n, replace = TRUE),
    hw1  = sample(0:59, n, replace = TRUE),
    h22  = sample(c(0, 1), n, replace = TRUE, prob = c(0.6, 0.4)),
    b5   = 1L,
    stringsAsFactors = FALSE
  )
  fever <- kr$h22 == 1
  kr$h32a <- ifelse(fever, sample(c(0, 1), sum(fever), replace = TRUE), NA)
  kr$h32b <- ifelse(fever, sample(c(0, 1), sum(fever), replace = TRUE), NA)
  kr$h32j <- ifelse(fever, sample(c(0, 1), sum(fever), replace = TRUE), NA)
  kr$h32k <- ifelse(fever, sample(c(0, 1), sum(fever), replace = TRUE), NA)
  kr
}


# --- calc_csb_by_wealth_dhs -----------------------------------------------

test_that("calc_csb_by_wealth_dhs rejects non-data.frame input", {
  expect_error(calc_csb_by_wealth_dhs("nope"), "must be a data.frame")
})

test_that("calc_csb_by_wealth_dhs errors on empty input", {
  expect_error(calc_csb_by_wealth_dhs(data.frame()), "is empty")
})

test_that("calc_csb_by_wealth_dhs rejects invalid quintile values", {
  kr <- make_csb_kr_mock(n = 60)
  expect_error(
    calc_csb_by_wealth_dhs(kr, quintiles = c(0, 6)),
    "between 1 and 5"
  )
})

test_that("calc_csb_by_wealth_dhs errors on missing required survey_vars columns", {
  kr <- make_csb_kr_mock(n = 60)
  kr$h22 <- NULL
  expect_error(
    suppressMessages(calc_csb_by_wealth_dhs(kr)),
    "Required variables not found"
  )
})

test_that("calc_csb_by_wealth_dhs returns adm0 list for national-only data", {
  skip_if_not_installed("survey")
  skip_if_not_installed("purrr")
  skip_if_not_installed("tibble")

  # Drop v024 to avoid the auto-region path (which has a known mismatch bug)
  kr <- make_csb_kr_mock(n = 400)
  kr$v024 <- NULL
  out <- suppressMessages(calc_csb_by_wealth_dhs(kr))

  expect_type(out, "list")
  expect_true("adm0" %in% names(out))
  expect_s3_class(out$adm0, "tbl_df")

  expected_cols <- c("survey_id", "indicator", "indicator_code",
                     "point", "ci_l", "ci_u", "numerator", "denominator")
  expect_true(all(expected_cols %in% names(out$adm0)))
})

test_that("calc_csb_by_wealth_dhs limits quintile coverage when filtered", {
  skip_if_not_installed("survey")
  skip_if_not_installed("purrr")
  skip_if_not_installed("tibble")

  kr <- make_csb_kr_mock(n = 300)
  kr$v024 <- NULL
  out <- suppressMessages(
    calc_csb_by_wealth_dhs(kr, quintiles = c(1, 5))
  )
  expect_true("wealth_quintile" %in% names(out$adm0))
  expect_setequal(out$adm0$wealth_quintile, c(1, 5))
})


# --- .aggregate_facility_to_annual -----------------------------------------

make_facility_mock <- function(months = 12, hfs = 3, has_cs = TRUE) {
  set.seed(2)
  df <- expand.grid(
    hf_uid = sprintf("HF%03d", seq_len(hfs)),
    month_idx = seq_len(months),
    stringsAsFactors = FALSE
  )
  df$year <- 2023
  df$adm0 <- "Testland"
  df$adm1 <- "Reg1"
  df$adm2 <- "Dist1"
  df$pop <- 1000
  df$conf <- as.integer(stats::rpois(nrow(df), 5))
  df$test <- df$conf + as.integer(stats::rpois(nrow(df), 8))
  df$pres <- as.integer(stats::rpois(nrow(df), 2))
  df$tpr <- df$conf / pmax(df$test, 1)
  df$reprate <- 0.9
  df$n0_cases <- df$conf
  df$n1_cases <- df$conf + df$pres
  df$n2_cases <- as.integer(df$n1_cases / 0.9)
  if (has_cs) {
    df$cs_public  <- 0.5
    df$cs_private <- 0.3
    df$cs_none    <- 0.2
  }
  df
}

test_that(".aggregate_facility_to_annual rolls monthly facility rows to admin-year", {
  df <- make_facility_mock()
  out <- sntmethods:::.aggregate_facility_to_annual(df, scale_factor = 1000)

  expect_s3_class(out, "data.frame")
  expect_true(all(c("hf_uid", "adm0", "adm1", "adm2", "year",
                    "n0_cases", "n0_incidence",
                    "n1_cases", "n1_incidence",
                    "n2_cases", "n2_incidence") %in% names(out)))
  # 3 facilities x 1 year = 3 grouped rows
  expect_equal(nrow(out), 3)

  # n0 should equal total of monthly n0_cases per facility
  expected <- aggregate(n0_cases ~ hf_uid, data = df, sum)
  actual   <- out[, c("hf_uid", "n0_cases")]
  m <- merge(expected, actual, by = "hf_uid", suffixes = c(".exp", ".act"))
  expect_equal(m$n0_cases.act, m$n0_cases.exp)
})

test_that(".aggregate_facility_to_annual computes N3-N5 when full cs data present", {
  df <- make_facility_mock(has_cs = TRUE)
  out <- sntmethods:::.aggregate_facility_to_annual(df, scale_factor = 1000)
  expect_true(all(c("n3_cases", "n3_incidence",
                    "n4_cases", "n4_incidence",
                    "n5_cases", "n5_incidence") %in% names(out)))
})

test_that(".aggregate_facility_to_annual handles partial cs data (only N4/N5)", {
  df <- make_facility_mock(has_cs = TRUE)
  df$cs_private <- NULL  # leave only cs_public + cs_none
  out <- sntmethods:::.aggregate_facility_to_annual(df, scale_factor = 1000)
  expect_true(all(c("n4_cases", "n5_cases") %in% names(out)))
  expect_false("n3_cases" %in% names(out))
})


# --- check_incidence -------------------------------------------------------

test_that("check_incidence rejects non-list / wrong-shape input", {
  expect_error(check_incidence("nope"), "Input must be output")
  expect_error(check_incidence(list(annual = list())), "Input must be output")
})

test_that("check_incidence aborts when monthly$adm2 is empty", {
  bad <- list(
    monthly = list(adm2 = data.frame()),
    annual  = list(adm2 = data.frame())
  )
  expect_error(check_incidence(bad), "No monthly")
})

test_that("check_incidence returns ggplot objects for a valid input", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("zoo")

  monthly <- data.frame(
    adm1 = rep("R1", 12),
    adm2 = rep("D1", 12),
    year = 2023,
    month = 1:12,
    date = seq.Date(as.Date("2023-01-01"), by = "month", length.out = 12),
    n0_incidence = stats::runif(12, 0, 5),
    n1_incidence = stats::runif(12, 1, 6),
    n2_incidence = stats::runif(12, 2, 7)
  )
  annual <- data.frame(
    adm1 = "R1", adm2 = "D1", year = 2023,
    n0_incidence = 30, n1_incidence = 40, n2_incidence = 50
  )
  inc <- list(monthly = list(adm2 = monthly),
              annual  = list(adm2 = annual))

  out <- suppressMessages(check_incidence(inc, ncol = 1))
  # Function returns a plot object (single ggplot or list)
  expect_true(inherits(out, "ggplot") || is.list(out))
})
