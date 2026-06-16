# Tests for trend_analysis.R: normalize_zscore() and run_grouped_stl_trend()

# --- normalize_zscore() ------------------------------------------------------

test_that("normalize_zscore returns z-scores for a well-defined numeric vector", {
  vec <- c(1, 2, 3, 4, 5)
  out <- normalize_zscore(vec)

  expect_type(out, "double")
  expect_length(out, length(vec))
  expect_equal(mean(out), 0)
  expect_equal(stats::sd(out), 1)
})

test_that("normalize_zscore returns NA vector when sd is zero (default behavior)", {
  out <- normalize_zscore(c(5, 5, 5))

  expect_length(out, 3)
  expect_true(all(is.na(out)))
})

test_that("normalize_zscore errors on zero variance when na_on_fail = FALSE", {
  expect_error(
    normalize_zscore(c(5, 5, 5), na_on_fail = FALSE),
    "standard deviation is zero"
  )
})

test_that("normalize_zscore returns NA vector when all values are NA", {
  out <- normalize_zscore(c(NA_real_, NA_real_, NA_real_))

  expect_length(out, 3)
  expect_true(all(is.na(out)))
})

test_that("normalize_zscore errors on all-NA when na_on_fail = FALSE", {
  expect_error(
    normalize_zscore(c(NA_real_, NA_real_), na_on_fail = FALSE),
    "only NA"
  )
})

test_that("normalize_zscore errors on non-numeric input", {
  expect_error(normalize_zscore("not numeric"), "must be numeric")
  expect_error(normalize_zscore(c(TRUE, FALSE)), "must be numeric")
})

test_that("normalize_zscore handles NA values in mixed input", {
  vec <- c(1, 2, 3, NA, 5)
  out <- normalize_zscore(vec)

  expect_length(out, 5)
  expect_true(is.na(out[4]))
  expect_false(any(is.na(out[-4])))
})


# --- run_grouped_stl_trend() ------------------------------------------------

make_trend_mock <- function(n_months = 48, groups = c("A", "B"), seed = 1) {
  set.seed(seed)
  df <- expand.grid(
    adm1 = groups,
    date = seq.Date(as.Date("2018-01-01"), by = "month", length.out = n_months),
    stringsAsFactors = FALSE
  )
  df$cases <- as.numeric(
    100 + as.integer(factor(df$adm1)) * 20 +
      seq_along(df$cases %||% rep(0, nrow(df))) * 0.5 +
      stats::rnorm(nrow(df), 0, 5)
  )
  df$incidence <- df$cases / 1000
  df
}

test_that("run_grouped_stl_trend produces STL components and trend stats", {
  skip_if_not_installed("stlplus")
  skip_if_not_installed("trend")

  dat <- make_trend_mock()
  indicators <- list(list(col = "cases", type = "raw_cases"))

  out <- run_grouped_stl_trend(
    data = dat,
    group_col = "adm1",
    date_col = "date",
    indicators = indicators
  )

  expect_s3_class(out, "data.frame")
  # group + date + 4 STL cols + type + mk_p + sens_slope
  expect_true(all(c("adm1", "date", "type", "mk_p", "sens_slope") %in% names(out)))
  expect_equal(names(out)[1:2], c("adm1", "date"))
  expect_setequal(unique(out$adm1), c("A", "B"))
  expect_setequal(unique(out$type), "raw_cases")
})

test_that("run_grouped_stl_trend handles multiple indicators per group", {
  skip_if_not_installed("stlplus")
  skip_if_not_installed("trend")

  dat <- make_trend_mock()
  indicators <- list(
    list(col = "cases",      type = "raw_cases"),
    list(col = "incidence",  type = "rate")
  )

  out <- run_grouped_stl_trend(
    data = dat,
    group_col = "adm1",
    date_col = "date",
    indicators = indicators
  )

  expect_setequal(unique(out$type), c("raw_cases", "rate"))
  # 2 groups * 2 indicators * 48 months
  expect_equal(nrow(out), 2 * 2 * 48)
})

test_that("run_grouped_stl_trend errors when group columns are missing", {
  dat <- make_trend_mock()
  expect_error(
    run_grouped_stl_trend(
      data = dat,
      group_col = "missing_col",
      date_col = "date",
      indicators = list(list(col = "cases", type = "x"))
    ),
    "Grouping columns not found"
  )
})

test_that("run_grouped_stl_trend errors when date column is missing", {
  dat <- make_trend_mock()
  expect_error(
    run_grouped_stl_trend(
      data = dat,
      group_col = "adm1",
      date_col = "missing_date",
      indicators = list(list(col = "cases", type = "x"))
    ),
    "Date column"
  )
})

test_that("run_grouped_stl_trend errors when indicators is empty or not a list", {
  dat <- make_trend_mock()
  expect_error(
    run_grouped_stl_trend(
      data = dat,
      group_col = "adm1",
      date_col = "date",
      indicators = list()
    ),
    "non-empty"
  )
})

test_that("run_grouped_stl_trend warns and skips when indicator column missing", {
  skip_if_not_installed("stlplus")
  skip_if_not_installed("trend")

  dat <- make_trend_mock()
  indicators <- list(
    list(col = "cases",        type = "ok"),
    list(col = "doesnt_exist", type = "missing")
  )

  expect_warning(
    out <- run_grouped_stl_trend(
      data = dat,
      group_col = "adm1",
      date_col = "date",
      indicators = indicators
    ),
    "not found for group"
  )
  expect_false("missing" %in% out$type)
  expect_true("ok" %in% out$type)
})

test_that("run_grouped_stl_trend warns and skips on non-numeric / all-NA columns", {
  skip_if_not_installed("stlplus")
  skip_if_not_installed("trend")

  dat <- make_trend_mock()
  dat$bad <- NA_real_

  expect_warning(
    out <- run_grouped_stl_trend(
      data = dat,
      group_col = "adm1",
      date_col = "date",
      indicators = list(
        list(col = "cases", type = "ok"),
        list(col = "bad",   type = "bad")
      )
    ),
    "Invalid data"
  )
  expect_setequal(unique(out$type), "ok")
})
