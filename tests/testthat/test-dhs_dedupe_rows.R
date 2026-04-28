# Tests for the .dhs_dedupe_rows() helper used inside dhs_read().
#
# Some DHS parquet partitions store each respondent twice -- once with the
# canonical survey_id and once with survey_id = NA. The helper must collapse
# both copies onto the canonical row regardless of which file_type is being
# read.

test_that(".dhs_dedupe_rows() returns input unchanged when file_type is NULL", {
  df <- data.frame(caseid = c("a", "b"), bidx = c(1, 1))
  out <- sntmethods:::.dhs_dedupe_rows(df, file_type = NULL, verbose = FALSE)
  expect_equal(out, df)
})

test_that(".dhs_dedupe_rows() is a no-op for unknown file_type (e.g. GE)", {
  df <- data.frame(x = c(1, 2))
  out <- sntmethods:::.dhs_dedupe_rows(df, file_type = "GE", verbose = FALSE)
  expect_equal(out, df)
})

test_that(".dhs_dedupe_rows() is a no-op when dedupe keys are missing", {
  df <- data.frame(other = c(1, 2))
  out <- sntmethods:::.dhs_dedupe_rows(df, file_type = "KR", verbose = FALSE)
  expect_equal(out, df)
})

test_that(".dhs_dedupe_rows() collapses NA-survey_id duplicates for KR", {
  # Two respondents, each duplicated with survey_id = NA on one copy.
  df <- data.frame(
    survey_id = c("XXKR8C", NA, "XXKR8C", NA),
    caseid    = c("a", "a", "b", "b"),
    bidx      = c(1L, 1L, 1L, 1L),
    payload   = c(10, 10, 20, 20)
  )
  out <- sntmethods:::.dhs_dedupe_rows(df, file_type = "KR", verbose = FALSE)
  expect_equal(nrow(out), 2)
  # Canonical (non-NA) survey_id rows should survive.
  expect_true(all(!is.na(out$survey_id)))
  expect_setequal(out$caseid, c("a", "b"))
})

test_that(".dhs_dedupe_rows() preserves rows when no duplicates exist", {
  df <- data.frame(
    survey_id = c("S1", "S1", "S1"),
    caseid    = c("a", "b", "c"),
    bidx      = c(1L, 1L, 1L)
  )
  out <- sntmethods:::.dhs_dedupe_rows(df, file_type = "KR", verbose = FALSE)
  expect_equal(nrow(out), 3)
})

test_that(".dhs_dedupe_rows() distinguishes children by bidx within a mother", {
  # Same caseid (same mother) with two different children (bidx = 1 vs 2)
  # must NOT be collapsed.
  df <- data.frame(
    survey_id = c("S1", "S1", NA, NA),
    caseid    = c("m1", "m1", "m1", "m1"),
    bidx      = c(1L, 2L, 1L, 2L)
  )
  out <- sntmethods:::.dhs_dedupe_rows(df, file_type = "KR", verbose = FALSE)
  expect_equal(nrow(out), 2)
  expect_setequal(out$bidx, c(1L, 2L))
  expect_true(all(!is.na(out$survey_id)))
})

test_that(".dhs_dedupe_rows() works for HR file_type (hhid only)", {
  df <- data.frame(
    survey_id = c("XXHR8C", NA, "XXHR8C"),
    hhid      = c("h1", "h1", "h2"),
    payload   = c(1, 1, 2)
  )
  out <- sntmethods:::.dhs_dedupe_rows(df, file_type = "HR", verbose = FALSE)
  expect_equal(nrow(out), 2)
  expect_setequal(out$hhid, c("h1", "h2"))
  expect_true(all(!is.na(out$survey_id)))
})

test_that(".dhs_dedupe_rows() works for PR file_type (hhid + hvidx)", {
  df <- data.frame(
    survey_id = c("XXPR8C", NA, "XXPR8C", NA),
    hhid      = c("h1", "h1", "h1", "h1"),
    hvidx     = c(1L, 1L, 2L, 2L)
  )
  out <- sntmethods:::.dhs_dedupe_rows(df, file_type = "PR", verbose = FALSE)
  expect_equal(nrow(out), 2)
  expect_setequal(out$hvidx, c(1L, 2L))
  expect_true(all(!is.na(out$survey_id)))
})

test_that(".dhs_dedupe_rows() emits a CLI warning when verbose and duplicates exist", {
  df <- data.frame(
    survey_id = c("S", NA),
    caseid    = c("a", "a"),
    bidx      = c(1L, 1L)
  )
  expect_message(
    sntmethods:::.dhs_dedupe_rows(df, file_type = "KR", verbose = TRUE),
    regexp = "duplicate row"
  )
})

test_that(".dhs_dedupe_rows() is silent when verbose = FALSE", {
  df <- data.frame(
    survey_id = c("S", NA),
    caseid    = c("a", "a"),
    bidx      = c(1L, 1L)
  )
  expect_silent(
    sntmethods:::.dhs_dedupe_rows(df, file_type = "KR", verbose = FALSE)
  )
})
