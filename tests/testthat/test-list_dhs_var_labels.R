# Tests for list_dhs_var_labels()

.mock_kr_for_labels <- function() {
  df <- data.frame(
    caseid = letters[1:5],
    h32a   = c(1L, 0L, 1L, 0L, 0L),
    h32b   = c(0L, 1L, 0L, 0L, 1L),
    h32e   = c(0L, 0L, 0L, 1L, 0L),
    h32n   = c(0L, 0L, 0L, 0L, 1L),
    h32x   = c(0L, 0L, 0L, 0L, 0L),  # no observed 1
    h32_recoded = c(1L, 1L, 0L, 0L, 1L),  # should NOT match prefix mode
    ml13a  = c(1L, 0L, 0L, 1L, 0L),
    ml13b  = c(0L, 1L, 0L, 0L, 1L),
    stringsAsFactors = FALSE
  )

  attr(df$h32a, "label") <- "Government hospital"
  attr(df$h32b, "label") <- "Government health center"
  # Duplicate labels - this is the real-world h32e/h32n problem
  attr(df$h32e, "label") <- "Fever/cough: comm.health wrkr"
  attr(df$h32n, "label") <- "Fever/cough: comm.health wrkr"
  attr(df$h32x, "label") <- "Other"
  attr(df$h32_recoded, "label") <- "Care-seeking recoded"
  attr(df$ml13a, "label") <- "ANC visit 1: doctor"
  attr(df$ml13b, "label") <- "ANC visit 1: nurse/midwife"

  df
}


test_that("list_dhs_var_labels returns inventory for prefix mode", {
  kr <- .mock_kr_for_labels()
  out <- list_dhs_var_labels(kr, "h32")

  expect_s3_class(out, "tbl_df")
  expect_setequal(out$variable, c("h32a", "h32b", "h32e", "h32n", "h32x"))
  expect_false("h32_recoded" %in% out$variable)  # underscore breaks [a-z0-9]+

  # Labels are pulled correctly
  expect_equal(
    out$label[out$variable == "h32a"],
    "Government hospital"
  )

  # Counts
  expect_equal(out$n_nonmissing[out$variable == "h32a"], 5L)
  expect_equal(out$n_ones[out$variable == "h32a"], 2L)
  expect_equal(out$n_ones[out$variable == "h32x"], 0L)
})


test_that("list_dhs_var_labels flags duplicate labels", {
  kr <- .mock_kr_for_labels()
  out <- list_dhs_var_labels(kr, "h32")

  expect_true("duplicate_label" %in% names(out))
  expect_true(out$duplicate_label[out$variable == "h32e"])
  expect_true(out$duplicate_label[out$variable == "h32n"])
  expect_false(out$duplicate_label[out$variable == "h32a"])
})


test_that("list_dhs_var_labels can drop duplicate_label column", {
  kr <- .mock_kr_for_labels()
  out <- list_dhs_var_labels(kr, "h32", duplicate_label = FALSE)
  expect_false("duplicate_label" %in% names(out))
})


test_that("list_dhs_var_labels supports regex mode", {
  kr <- .mock_kr_for_labels()
  out <- list_dhs_var_labels(kr, "^ml13[a-z]$", regex = TRUE)

  expect_setequal(out$variable, c("ml13a", "ml13b"))
  expect_equal(
    out$label[out$variable == "ml13a"],
    "ANC visit 1: doctor"
  )
})


test_that("list_dhs_var_labels filters to observed via only_observed", {
  kr <- .mock_kr_for_labels()
  out <- list_dhs_var_labels(kr, "h32", only_observed = TRUE)

  # h32x has all zeros and should be dropped
  expect_false("h32x" %in% out$variable)
  expect_setequal(out$variable, c("h32a", "h32b", "h32e", "h32n"))
})


test_that("list_dhs_var_labels handles columns without label attr", {
  df <- data.frame(
    h32a = c(1L, 0L, 1L),
    h32b = c(0L, 1L, 0L)
  )
  # Only h32a has label
  attr(df$h32a, "label") <- "Has label"

  out <- list_dhs_var_labels(df, "h32")
  expect_equal(out$label[out$variable == "h32a"], "Has label")
  expect_true(is.na(out$label[out$variable == "h32b"]))
})


test_that("list_dhs_var_labels returns empty tibble when no match", {
  kr <- .mock_kr_for_labels()
  expect_message(
    out <- list_dhs_var_labels(kr, "doesnotexist"),
    regexp = "No variables"
  )
  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 0L)
  expect_true(all(c("variable", "label", "n_nonmissing", "n_ones") %in%
                    names(out)))
})


test_that("list_dhs_var_labels validates inputs", {
  kr <- .mock_kr_for_labels()

  expect_error(
    list_dhs_var_labels(list(a = 1), "h32"),
    regexp = "must be a data frame"
  )
  expect_error(
    list_dhs_var_labels(kr, ""),
    regexp = "non-empty character"
  )
  expect_error(
    list_dhs_var_labels(kr, c("h32", "ml13")),
    regexp = "non-empty character"
  )
  expect_error(
    list_dhs_var_labels(kr, "h32", regex = NA),
    regexp = "TRUE or FALSE"
  )
  expect_error(
    list_dhs_var_labels(kr, "h32", only_observed = "yes"),
    regexp = "TRUE or FALSE"
  )
})


test_that("prefix mode does not match the prefix itself when no suffix", {
  df <- data.frame(
    h32  = c(1L, 0L, 1L),       # bare prefix - should NOT match
    h32a = c(0L, 1L, 0L)
  )
  out <- list_dhs_var_labels(df, "h32")
  expect_setequal(out$variable, "h32a")
})
