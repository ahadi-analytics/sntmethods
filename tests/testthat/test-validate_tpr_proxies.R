# Tests for validate_tpr_proxies()

make_tpr_validation_mock <- function(n_hf = 6, n_months = 24, seed = 42) {
  set.seed(seed)
  dates <- seq.Date(as.Date("2020-01-01"), by = "month", length.out = n_months)
  hfs  <- sprintf("HF%03d", seq_len(n_hf))

  df <- expand.grid(hf_uid = hfs, date = dates, stringsAsFactors = FALSE)
  df$year  <- as.integer(format(df$date, "%Y"))
  df$month <- as.integer(format(df$date, "%m"))
  df$adm0  <- "Testland"
  df$adm1  <- ifelse(df$hf_uid %in% hfs[1:3], "Region1", "Region2")
  df$adm2  <- ifelse(df$hf_uid %in% hfs[1:2], "Dist1",
              ifelse(df$hf_uid %in% hfs[3:4], "Dist2", "Dist3"))
  df$test  <- pmax(1, as.integer(stats::rpois(nrow(df), lambda = 80)))
  df$conf  <- pmin(df$test, as.integer(df$test * stats::runif(nrow(df), 0.1, 0.5)))
  df$tpr   <- df$conf / df$test
  df$tpr_source       <- "raw"
  df$flag_inactive    <- FALSE
  df$flag_conf_gt_test <- FALSE
  df
}

# --- input validation -------------------------------------------------------

test_that("validate_tpr_proxies rejects non-data.frame input", {
  expect_error(validate_tpr_proxies("not a df"), "must be a data.frame")
})

test_that("validate_tpr_proxies reports missing required columns", {
  bad <- data.frame(hf_uid = "HF001", adm1 = "R1")
  expect_error(validate_tpr_proxies(bad), "Missing required columns")
})

test_that("validate_tpr_proxies aborts when too few rows survive filtering", {
  dat <- make_tpr_validation_mock(n_hf = 2, n_months = 2)
  # mark every row inactive so nothing survives the filter
  dat$flag_inactive <- TRUE
  expect_error(validate_tpr_proxies(dat), "Insufficient")
})

# --- happy path -------------------------------------------------------------

test_that("validate_tpr_proxies returns metrics + validation_data + plots", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("scales")

  dat <- make_tpr_validation_mock()

  # silence the cli + print(metrics) noise inside the function
  out <- suppressMessages(validate_tpr_proxies(dat))

  expect_type(out, "list")
  expect_named(out, c("metrics", "validation_data", "plots"), ignore.order = TRUE)
  expect_s3_class(out$metrics, "tbl_df")
  expect_setequal(
    out$metrics$proxy_level,
    c("adm2", "adm1", "prev_year", "rolling", "adm0")
  )
  expect_true(all(c("mae", "rmse", "correlation", "bias") %in% names(out$metrics)))

  expect_s3_class(out$validation_data, "data.frame")
  expect_true("tpr_actual" %in% names(out$validation_data))
  expect_true(all(c("proxy_adm2", "proxy_adm1", "proxy_adm0",
                    "proxy_prev_year", "proxy_roll3",
                    "proxy_adjacent_month") %in% names(out$validation_data)))
})

test_that("validate_tpr_proxies returns no plots when generate_plots = FALSE", {
  dat <- make_tpr_validation_mock(n_hf = 4, n_months = 18)
  out <- suppressMessages(
    validate_tpr_proxies(dat, generate_plots = FALSE)
  )
  expect_length(out$plots, 0)
})

test_that("validate_tpr_proxies produces all four diagnostic plots", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("scales")

  dat <- make_tpr_validation_mock()
  out <- suppressMessages(validate_tpr_proxies(dat))

  expect_setequal(
    names(out$plots),
    c("scatter", "error_dist", "mae_by_tests", "stability_map")
  )
  for (p in out$plots) {
    expect_s3_class(p, "ggplot")
  }
})

test_that("validate_tpr_proxies respects min_facilities for adm-level proxies", {
  # single facility per adm2/adm1 → adm2/adm1 proxies should be NA everywhere
  dat <- make_tpr_validation_mock(n_hf = 6)
  dat$adm2 <- dat$hf_uid  # one facility per adm2
  dat$adm1 <- dat$hf_uid  # one facility per adm1

  out <- suppressMessages(
    validate_tpr_proxies(dat, generate_plots = FALSE)
  )
  expect_true(all(is.na(out$validation_data$proxy_adm2)))
  expect_true(all(is.na(out$validation_data$proxy_adm1)))
})
