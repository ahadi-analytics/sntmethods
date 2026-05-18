test_that("profile_series() returns the documented profile schema", {
  df <- .epi_sim()
  prof <- profile_series(df, date_col = "date", count_col = "cases")
  expect_s3_class(prof, "tbl_df")
  expect_true(all(c("group_id", "n_obs", "mean_count", "median_count",
                    "low_count_fraction", "dispersion",
                    "zero_inflation", "missingness",
                    "seasonality_strength", "trend_strength") %in%
                  names(prof)))
  expect_equal(nrow(prof), 1L)
})

test_that("epi_recommend() returns top-k recommendations with rationale", {
  df <- .epi_sim()
  recs <- epi_recommend(
    df, date_col = "date", count_col = "cases",
    max_recommendations = 3
  )

  expect_s3_class(recs, "epi_recommendation")
  expect_true(all(c("group_id", "rank", "method", "score", "reason") %in%
                  names(recs$recommendations)))
  expect_true(all(recs$recommendations$rank %in% 1:3))
})

test_that("epi_recommend() honours simplicity tie-break ordering", {
  df <- .epi_sim()
  recs <- epi_recommend(
    df, date_col = "date", count_col = "cases",
    tie_break = "simplicity",
    max_recommendations = 5
  )
  # threshold should appear in the top recommendations (simplest baseline)
  expect_true("threshold" %in% recs$recommendations$method)
})

test_that("epi_recommend() falls back gracefully when evidence is missing", {
  df <- .epi_sim()
  expect_warning(
    recs <- epi_recommend(df, date_col = "date", count_col = "cases",
                          tie_break = "evidence"),
    "epi_evaluation"
  )
  expect_s3_class(recs, "epi_recommendation")
})

test_that("epi_recommend() S3 methods do not error", {
  df <- .epi_sim()
  recs <- epi_recommend(df, date_col = "date", count_col = "cases")
  expect_output(print(recs), "epi_recommendation")
  expect_s3_class(tibble::as_tibble(recs), "tbl_df")
  expect_invisible(summary(recs))
})
