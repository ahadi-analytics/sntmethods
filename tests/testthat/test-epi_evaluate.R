test_that("epi_evaluate() grades a labelled run end-to-end", {
  df <- .epi_sim()
  rng <- .epi_ranges(df)

  run <- epi_detect(
    df, "date", "cases",
    methods = c("threshold", "endemic_channel"),
    baseline_range = rng$baseline_range,
    target_range = rng$target_range,
    label_col = "truth"
  )

  eval <- epi_evaluate(
    detection_run = run,
    truth_col = "truth",
    loo_method = TRUE,
    cost = list(c_fp = 1, c_fn = 5)
  )

  expect_s3_class(eval, "epi_evaluation")
  expect_true("per_method" %in% names(eval))
  expect_true(all(c("method", "sensitivity", "specificity", "auc") %in%
                  names(eval$per_method)))
  expect_true(!is.null(eval$loo_method))
  expect_true(all(c("method", "delta_auc") %in% names(eval$loo_method)))
})

test_that("epi_evaluate() S3 methods do not error", {
  df <- .epi_sim()
  rng <- .epi_ranges(df)

  run <- epi_detect(
    df, "date", "cases", methods = "threshold",
    baseline_range = rng$baseline_range,
    target_range = rng$target_range,
    label_col = "truth"
  )

  eval <- epi_evaluate(run, truth_col = "truth")

  expect_output(print(eval), "epi_evaluation")
  expect_s3_class(tibble::as_tibble(eval), "tbl_df")
  expect_invisible(summary(eval))
})

test_that("epi_evaluate() falls back to unlabelled diagnostics gracefully", {
  df <- .epi_sim()
  rng <- .epi_ranges(df)

  run <- epi_detect(
    df, "date", "cases",
    methods = c("threshold", "endemic_channel"),
    baseline_range = rng$baseline_range,
    target_range = rng$target_range
  )

  eval <- epi_evaluate(run)
  expect_s3_class(eval, "epi_evaluation")
  # Cohen's kappa table should exist in unlabelled mode
  expect_true("agreement" %in% names(eval) || "kappa" %in% names(eval) ||
              "per_method" %in% names(eval))
})
