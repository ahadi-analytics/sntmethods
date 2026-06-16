# Tests for indicator_pfpr.R helpers:
#   calc_pfpr_dhs_core()
#   extract_dhs_metadata()
#   .pfpr_legacy_to_codes()
#   .filter_redundant_mbg_results()
#   .are_mbg_results_identical()
#   .extract_age_group_from_name()

make_pfpr_core_mock <- function(n = 200, seed = 42) {
  set.seed(seed)
  data.frame(
    hv000 = "SL7",
    hv007 = 2019L,
    hv001 = rep(1:20, each = n / 20),
    hv021 = rep(1:20, each = n / 20),
    hv005 = 1000000,
    hv022 = rep(1:4, each = n / 4),
    hv024 = rep(1:2, each = n / 2),
    hc1   = sample(6:59, n, replace = TRUE),
    hv103 = 1L,
    hv042 = 1L,
    hml35 = sample(c(0, 1, NA), n, replace = TRUE, prob = c(0.5, 0.3, 0.2)),
    hml32 = sample(c(0, 1, 6, NA), n, replace = TRUE,
                   prob = c(0.5, 0.2, 0.1, 0.2)),
    stringsAsFactors = FALSE
  )
}


# --- calc_pfpr_dhs_core() --------------------------------------------------

test_that("calc_pfpr_dhs_core rejects non-data.frame input", {
  expect_error(calc_pfpr_dhs_core("nope"), "must be a data.frame")
})

test_that("calc_pfpr_dhs_core errors on empty input", {
  expect_error(calc_pfpr_dhs_core(data.frame()), "is empty")
})

test_that("calc_pfpr_dhs_core errors on missing required survey_vars keys", {
  pr <- make_pfpr_core_mock()
  expect_error(
    calc_pfpr_dhs_core(pr, survey_vars = list(cluster = "hv021")),
    "must include"
  )
})

test_that("calc_pfpr_dhs_core errors when mapped columns absent", {
  pr <- make_pfpr_core_mock()
  pr$hml35 <- NULL
  expect_error(
    suppressMessages(calc_pfpr_dhs_core(pr)),
    "Columns not found"
  )
})

test_that("calc_pfpr_dhs_core returns adm-level tibble with PfPR cols", {
  skip_if_not_installed("survey")
  pr <- make_pfpr_core_mock()

  out <- suppressMessages(calc_pfpr_dhs_core(pr))
  expect_s3_class(out, "tbl_df")
  expect_true(all(c(
    "dhs_pfpr_rdt", "dhs_pfpr_rdt_low", "dhs_pfpr_rdt_upp",
    "dhs_pfpr_mic", "dhs_pfpr_mic_low", "dhs_pfpr_mic_upp",
    "dhs_n_tested_rdt", "dhs_n_pos_rdt",
    "dhs_n_tested_mic", "dhs_n_pos_mic"
  ) %in% names(out)))
  # group_var = adm1 by default
  expect_true("adm1" %in% names(out))
})

test_that("calc_pfpr_dhs_core aborts when no admin available", {
  skip_if_not_installed("survey")
  pr <- make_pfpr_core_mock()
  pr$hv024 <- NULL

  expect_error(
    suppressMessages(calc_pfpr_dhs_core(pr)),
    "no admin"
  )
})


# --- extract_dhs_metadata() ------------------------------------------------

test_that("extract_dhs_metadata handles hv-style PR data", {
  pr <- make_pfpr_core_mock()
  meta <- extract_dhs_metadata(pr)

  expect_equal(meta$country_code, "SL7")
  expect_equal(meta$survey_year, 2019)
  expect_equal(meta$survey_id, "SL7")
  expect_equal(meta$file_type, "PR")
  expect_equal(meta$total_records, nrow(pr))
  expect_true(meta$has_rdt)
  expect_true(meta$has_microscopy)
})

test_that("extract_dhs_metadata handles v-style individual data", {
  ir <- data.frame(v000 = "TG7", v007 = 2017L, hml35 = 0, hml32 = 0)
  meta <- extract_dhs_metadata(ir)

  expect_equal(meta$country_code, "TG7")
  expect_equal(meta$survey_year, 2017)
})

test_that("extract_dhs_metadata falls back when no country/year cols", {
  bare <- data.frame(hv001 = 1:5, x = 1)
  meta <- extract_dhs_metadata(bare)

  expect_true(is.na(meta$country_code))
  expect_true(is.na(meta$survey_year))
  # No malaria module present → default
  expect_equal(meta$survey_type, "DHS")
})

test_that("extract_dhs_metadata detects MIS-style data", {
  mis <- data.frame(hv001 = 1:5, sh418s = 0, sh418p = 1)
  meta <- extract_dhs_metadata(mis)
  expect_equal(meta$survey_type, "MIS")
})

test_that("extract_dhs_metadata exposes admin coverage", {
  pr <- make_pfpr_core_mock()
  meta <- extract_dhs_metadata(pr, survey_vars = list(cluster = "hv021", adm1 = "hv024"))

  expect_true("n_admin1_units" %in% names(meta))
  expect_equal(meta$n_admin1_units, length(unique(pr$hv024)))
})


# --- .pfpr_legacy_to_codes() -----------------------------------------------

test_that(".pfpr_legacy_to_codes returns default codes for default age groups", {
  out <- sntmethods:::.pfpr_legacy_to_codes()
  expect_type(out, "character")
  expect_true(length(out) > 0)
  # default test_type is "both" → rdt and mic codes expected
  expect_true(any(grepl("^pfpr_rdt", out)))
  expect_true(any(grepl("^pfpr_mic", out)))
})

test_that(".pfpr_legacy_to_codes filters by test type", {
  rdt_only <- sntmethods:::.pfpr_legacy_to_codes(test_type = "rdt")
  mic_only <- sntmethods:::.pfpr_legacy_to_codes(test_type = "mic")
  expect_true(all(grepl("^pfpr_rdt", rdt_only)))
  expect_true(all(grepl("^pfpr_mic", mic_only)))
})

test_that(".pfpr_legacy_to_codes warns on 'either' (not in MBG dictionary)", {
  expect_message(
    out <- sntmethods:::.pfpr_legacy_to_codes(test_type = "either"),
    "either"
  )
  expect_length(out, 0)
})

test_that(".pfpr_legacy_to_codes warns on age groups not in MBG dictionary", {
  expect_message(
    sntmethods:::.pfpr_legacy_to_codes(
      test_type = "rdt",
      age_groups = list(bizarre = c(1, 2))
    ),
    "not in MBG"
  )
})


# --- .extract_age_group_from_name() ---------------------------------------

test_that(".extract_age_group_from_name strips standard prefixes", {
  expect_equal(sntmethods:::.extract_age_group_from_name("pfpr_rdt_u5"), "u5")
  expect_equal(sntmethods:::.extract_age_group_from_name("pfpr_mic_2_10"), "2_10")
  expect_equal(sntmethods:::.extract_age_group_from_name("pfpr_combined_u10"), "u10")
  expect_equal(sntmethods:::.extract_age_group_from_name("pfpr_either_5_14"), "5_14")
})


# --- .are_mbg_results_identical() -----------------------------------------

make_cluster_dt <- function(n = 30, seed = 1, indicator = NULL, samplesize = NULL) {
  set.seed(seed)
  data.frame(
    cluster_id = seq_len(n),
    indicator  = indicator %||% stats::rpois(n, lambda = 5),
    samplesize = samplesize %||% stats::rpois(n, lambda = 20) + 1
  )
}

test_that(".are_mbg_results_identical returns TRUE for identical data", {
  dt <- make_cluster_dt()
  expect_true(sntmethods:::.are_mbg_results_identical(dt, dt))
})

test_that(".are_mbg_results_identical returns FALSE for very different sizes", {
  dt1 <- make_cluster_dt(n = 30)
  dt2 <- make_cluster_dt(n = 100)
  expect_false(sntmethods:::.are_mbg_results_identical(dt1, dt2))
})

test_that(".are_mbg_results_identical returns FALSE when total counts diverge >1%", {
  dt1 <- make_cluster_dt(n = 30, seed = 1)
  dt2 <- dt1
  dt2$samplesize <- dt2$samplesize * 2  # double sample size → diverges
  expect_false(sntmethods:::.are_mbg_results_identical(dt1, dt2))
})

test_that(".are_mbg_results_identical returns TRUE when both indicators are zero", {
  dt <- make_cluster_dt(indicator = rep(0, 30))
  expect_true(sntmethods:::.are_mbg_results_identical(dt, dt))
})


# --- .filter_redundant_mbg_results() --------------------------------------

test_that(".filter_redundant_mbg_results no-ops on single-element list", {
  dt <- make_cluster_dt()
  results <- list(pfpr_rdt_u5 = dt)
  out <- sntmethods:::.filter_redundant_mbg_results(
    results,
    age_groups = list(pfpr_rdt_u5 = c(0, 59))
  )
  expect_length(out, 1)
})

test_that(".filter_redundant_mbg_results drops the wider duplicate when two rdt sets are identical", {
  dt <- make_cluster_dt()
  results <- list(
    pfpr_rdt_u5  = dt,
    pfpr_rdt_u10 = dt   # identical → wider range removed
  )
  age_groups <- list(
    pfpr_rdt_u5  = c(0, 59),
    pfpr_rdt_u10 = c(0, 119)
  )
  expect_message(
    out <- sntmethods:::.filter_redundant_mbg_results(results, age_groups),
    "skipped"
  )
  expect_named(out, "pfpr_rdt_u5")
})

test_that(".filter_redundant_mbg_results keeps non-redundant pairs", {
  dt_a <- make_cluster_dt(seed = 1)
  dt_b <- make_cluster_dt(seed = 99, indicator = rep(0, 30))  # very different
  results <- list(pfpr_rdt_u5 = dt_a, pfpr_rdt_u10 = dt_b)
  age_groups <- list(pfpr_rdt_u5 = c(0, 59), pfpr_rdt_u10 = c(0, 119))

  out <- sntmethods:::.filter_redundant_mbg_results(results, age_groups)
  expect_length(out, 2)
})
