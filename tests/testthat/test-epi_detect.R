test_that("available_detectors() reports the v0.9.0 built-ins", {
  detectors <- available_detectors()
  expect_true(is.character(detectors))
  for (nm in c("threshold", "ears_c1", "ears_c2", "ears_c3",
               "cusum_classical", "farrington", "glrnb",
               "trending", "endemic_channel", "stl_residual",
               "anomalize_stl", "arima",
               "changepoint_pelt", "bayesian_changepoint")) {
    expect_true(nm %in% detectors,
                info = paste("detector missing from registry:", nm))
  }
})

test_that("epi_detect() validates inputs", {
  df <- .epi_sim()

  expect_error(
    epi_detect("not a df", "date", "cases", methods = "threshold"),
    "data frame"
  )

  expect_error(
    epi_detect(df[0, ], "date", "cases", methods = "threshold"),
    "empty"
  )

  expect_error(
    epi_detect(df, "no_such_col", "cases", methods = "threshold"),
    regexp = "no_such_col"
  )
})

test_that("epi_detect() returns the documented contract for threshold", {
  df <- .epi_sim()
  rng <- .epi_ranges(df)

  run <- epi_detect(
    df, "date", "cases",
    methods = "threshold",
    baseline_range = rng$baseline_range,
    target_range = rng$target_range
  )

  expect_s3_class(run, "epi_detection_run")
  expect_named(
    run$predictions,
    c("method", "group_id", "date", "alarm", "score",
      "upper_threshold", "failed", "error_message")
  )
  expect_true(all(c("predictions", "preprocessing", "methods", "call") %in%
                  names(run)))
  expect_equal(unique(run$predictions$method), "threshold")
  expect_equal(nrow(run$predictions), 52L)
})

test_that("epi_detect() runs multiple detectors and isolates failures", {
  df <- .epi_sim()
  rng <- .epi_ranges(df)

  run <- epi_detect(
    df, "date", "cases",
    methods = c("threshold", "endemic_channel"),
    baseline_range = rng$baseline_range,
    target_range = rng$target_range
  )

  expect_setequal(unique(run$predictions$method),
                  c("threshold", "endemic_channel"))
  expect_true(any(run$predictions$alarm, na.rm = TRUE))
})

test_that("epi_detect() S3 methods do not error", {
  df <- .epi_sim()
  rng <- .epi_ranges(df)
  run <- epi_detect(
    df, "date", "cases", methods = "threshold",
    baseline_range = rng$baseline_range,
    target_range = rng$target_range
  )

  expect_output(print(run), "epi_detection")
  expect_s3_class(tibble::as_tibble(run), "tbl_df")
  expect_invisible(summary(run))
})

test_that("register_detector() adds and dispatches a third-party detector", {
  old_detectors <- available_detectors()

  new_constant_detector <- function(value = TRUE) {
    sntmethods:::new_epi_detector(method = "always_alarm",
                                   params = list(value = value))
  }

  fit_method <- function(detector, baseline_data, ...) {
    detector$fitted <- TRUE
    detector
  }
  predict_method <- function(detector, target_data, ...) {
    tibble::tibble(
      date = target_data$date,
      alarm = rep(detector$params$value, nrow(target_data)),
      score = rep(1, nrow(target_data)),
      upper_threshold = rep(NA_real_, nrow(target_data))
    )
  }

  register_detector(
    name = "always_alarm",
    constructor = new_constant_detector,
    fit_method = fit_method,
    predict_method = predict_method
  )

  expect_true("always_alarm" %in% available_detectors())

  df <- .epi_sim()
  rng <- .epi_ranges(df)
  run <- epi_detect(
    df, "date", "cases", methods = "always_alarm",
    baseline_range = rng$baseline_range,
    target_range = rng$target_range
  )
  expect_true(all(run$predictions$alarm))
})
