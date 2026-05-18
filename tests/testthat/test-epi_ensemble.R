test_that("epi_ensemble() validates input class", {
  expect_error(
    epi_ensemble("not a run"),
    "epi_detection_run"
  )
})

test_that("epi_ensemble() builds a majority vote ensemble", {
  df <- .epi_sim()
  rng <- .epi_ranges(df)

  run <- epi_detect(
    df, "date", "cases",
    methods = c("threshold", "endemic_channel", "ears_c1"),
    baseline_range = rng$baseline_range,
    target_range = rng$target_range
  )

  ens <- epi_ensemble(run, strategy = "majority_vote")
  expect_s3_class(ens, "epi_ensemble")
  expect_equal(unique(ens$predictions$method), "ensemble")
  expect_equal(nrow(ens$predictions), 52L)
})

test_that("epi_ensemble() supports weighted strategy from epi_evaluation", {
  df <- .epi_sim()
  rng <- .epi_ranges(df)

  run <- epi_detect(
    df, "date", "cases",
    methods = c("threshold", "endemic_channel"),
    baseline_range = rng$baseline_range,
    target_range = rng$target_range,
    label_col = "truth"
  )

  eval <- epi_evaluate(run, truth_col = "truth", loo_method = TRUE)
  ens <- epi_ensemble(run, strategy = "weighted_vote", weights = eval)

  expect_s3_class(ens, "epi_ensemble")
  expect_equal(ens$strategy, "weighted_vote")
  expect_true(is.numeric(ens$weights))
})

test_that("epi_ensemble() score_average strategy returns boolean alarms", {
  df <- .epi_sim()
  rng <- .epi_ranges(df)

  run <- epi_detect(
    df, "date", "cases",
    methods = c("threshold", "endemic_channel"),
    baseline_range = rng$baseline_range,
    target_range = rng$target_range
  )

  ens <- epi_ensemble(run, strategy = "score_average")
  expect_true(is.logical(ens$predictions$alarm))
})
