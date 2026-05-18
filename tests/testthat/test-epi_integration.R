test_that("the full epi_* pipeline composes: detect -> evaluate -> ensemble -> recommend", {
  df <- .epi_sim()
  rng <- .epi_ranges(df)

  run <- epi_detect(
    df, "date", "cases",
    methods = c("threshold", "endemic_channel"),
    baseline_range = rng$baseline_range,
    target_range = rng$target_range,
    label_col = "truth"
  )
  expect_s3_class(run, "epi_detection_run")

  eval <- epi_evaluate(run, labels = "truth", loo_method = TRUE)
  expect_s3_class(eval, "epi_evaluation")

  ens <- epi_ensemble(run, strategy = "weighted_vote", weights = eval)
  expect_s3_class(ens, "epi_ensemble")

  recs <- epi_recommend(
    df, date_col = "date", count_col = "cases",
    evaluation = eval, tie_break = "evidence"
  )
  expect_s3_class(recs, "epi_recommendation")

  # ensemble alarm aligned to target window
  expect_equal(nrow(ens$predictions), 52L)
})

test_that("failure isolation: a broken third-party detector does not crash the run", {
  new_broken <- function() {
    sntmethods:::new_epi_detector(method = "broken", params = list())
  }
  fit_b <- function(detector, baseline_data, ...) {
    detector$fitted <- TRUE
    detector
  }
  predict_b <- function(detector, target_data, ...) {
    stop("simulated detector failure")
  }
  register_detector("broken",
                    constructor = new_broken,
                    fit_method = fit_b,
                    predict_method = predict_b)

  df <- .epi_sim()
  rng <- .epi_ranges(df)

  run <- epi_detect(
    df, "date", "cases",
    methods = c("threshold", "broken"),
    baseline_range = rng$baseline_range,
    target_range = rng$target_range
  )

  broken_rows <- run$predictions[run$predictions$method == "broken", ]
  expect_true(all(broken_rows$failed))
  expect_true(all(is.na(broken_rows$alarm)))
  expect_true(all(!is.na(broken_rows$error_message)))

  # the healthy detector still produced 52 valid rows
  thr_rows <- run$predictions[run$predictions$method == "threshold", ]
  expect_equal(nrow(thr_rows), 52L)
  expect_false(any(thr_rows$failed))
})
