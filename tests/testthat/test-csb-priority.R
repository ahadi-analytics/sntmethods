# Tests for the csb_priority_method parameter in CSB / MBG pipeline.
#
# This parameter lets callers resolve overlapping care-seeking records so
# that each individual is assigned to at most one sector (public vs private
# vs none), making csb_public + csb_private + csb_none sum to exactly 100%.

# ---- Helpers ----

# Build a small, deterministic febrile-child data frame with overlapping
# h32 visits. We then call .classify_csb_from_h32 directly to inspect the
# resulting child-level indicators.
.mock_csb_overlap_df <- function() {
  # 5 children, ALL febrile (filtering already done upstream).
  # Column set covers: public (h32a), CHW (h32b), private formal (h32j),
  # pharmacy (h32n), private informal (h32s).
  #
  #  row 1: public only
  #  row 2: private formal only
  #  row 3: public + private formal  (OVERLAP)
  #  row 4: CHW + pharmacy           (OVERLAP)
  #  row 5: none
  data.frame(
    h32a = c(1, 0, 1, 0, 0),   # public
    h32b = c(0, 0, 0, 1, 0),   # CHW
    h32j = c(0, 1, 1, 0, 0),   # private formal
    h32n = c(0, 0, 0, 1, 0),   # pharmacy
    h32s = c(0, 0, 0, 0, 0),   # private informal
    stringsAsFactors = FALSE
  )
}

.mock_classification <- function() {
  data.frame(
    variable = c("h32a", "h32b", "h32j", "h32n", "h32s"),
    csb = c("public", "chw", "private_formal", "pharmacy",
            "private_informal"),
    stringsAsFactors = FALSE
  )
}


# ---- "all" (default) preserves WHO overlapping behavior ----

test_that("csb_priority_method = 'all' allows overlap (WHO default)", {
  df <- .mock_csb_overlap_df()
  cls <- .mock_classification()

  out <- sntmethods:::.classify_csb_from_h32(
    data = df,
    h32_cols = c("h32a", "h32b", "h32j", "h32n", "h32s"),
    classification = cls,
    csb_priority_method = "all"
  )

  # Rows 3 and 4 are in BOTH public and private under "all" semantics
  expect_equal(out$csb_public,  c(1, 0, 1, 1, 0))
  expect_equal(out$csb_private, c(0, 1, 1, 1, 0))
  expect_equal(out$csb_none,    c(0, 0, 0, 0, 1))

  # Overlap means public + private + none > 1 per child for overlap rows
  overlap_flag <- out$csb_public + out$csb_private + out$csb_none
  expect_true(any(overlap_flag > 1))
})


# ---- "public" priority ----

test_that("csb_priority_method = 'public' classifies overlap as public only", {
  df <- .mock_csb_overlap_df()
  cls <- .mock_classification()

  out <- sntmethods:::.classify_csb_from_h32(
    data = df,
    h32_cols = c("h32a", "h32b", "h32j", "h32n", "h32s"),
    classification = cls,
    csb_priority_method = "public"
  )

  # Row 3 (public + private) becomes public only
  # Row 4 (CHW + pharmacy) becomes public only (CHW counts as public)
  expect_equal(out$csb_public,  c(1, 0, 1, 1, 0))
  expect_equal(out$csb_private, c(0, 1, 0, 0, 0))
  expect_equal(out$csb_none,    c(0, 0, 0, 0, 1))

  # Mutually exclusive: sums to 1 per child
  expect_equal(out$csb_public + out$csb_private + out$csb_none,
               rep(1, nrow(out)))
})


# ---- "private" priority ----

test_that("csb_priority_method = 'private' classifies overlap as private only", {
  df <- .mock_csb_overlap_df()
  cls <- .mock_classification()

  out <- sntmethods:::.classify_csb_from_h32(
    data = df,
    h32_cols = c("h32a", "h32b", "h32j", "h32n", "h32s"),
    classification = cls,
    csb_priority_method = "private"
  )

  # Row 3 (public + private) becomes private only
  # Row 4 (CHW + pharmacy) becomes private only (pharmacy is private)
  expect_equal(out$csb_public,  c(1, 0, 0, 0, 0))
  expect_equal(out$csb_private, c(0, 1, 1, 1, 0))
  expect_equal(out$csb_none,    c(0, 0, 0, 0, 1))

  # Mutually exclusive
  expect_equal(out$csb_public + out$csb_private + out$csb_none,
               rep(1, nrow(out)))
})


# ---- "first" priority (take first recurring h32 source) ----

test_that("csb_priority_method = 'first' keeps the first visited h32 source", {
  df <- .mock_csb_overlap_df()
  cls <- .mock_classification()

  out <- sntmethods:::.classify_csb_from_h32(
    data = df,
    h32_cols = c("h32a", "h32b", "h32j", "h32n", "h32s"),
    classification = cls,
    csb_priority_method = "first"
  )

  # Row 3: first visited is h32a (public), not h32j -> public
  # Row 4: first visited is h32b (chw, counts as public), not h32n -> public
  expect_equal(out$csb_public,  c(1, 0, 1, 1, 0))
  expect_equal(out$csb_private, c(0, 1, 0, 0, 0))
  expect_equal(out$csb_none,    c(0, 0, 0, 0, 1))

  # Mutually exclusive
  expect_equal(out$csb_public + out$csb_private + out$csb_none,
               rep(1, nrow(out)))
})


# ---- Percentages sum to 100% at "cluster" level for non-"all" modes ----

test_that("non-'all' methods make csb_public + csb_private + csb_none = 100%", {
  df <- .mock_csb_overlap_df()
  cls <- .mock_classification()
  cols <- c("h32a", "h32b", "h32j", "h32n", "h32s")

  for (method in c("first", "public", "private")) {
    out <- sntmethods:::.classify_csb_from_h32(
      data = df,
      h32_cols = cols,
      classification = cls,
      csb_priority_method = method
    )
    total_pct <- mean(out$csb_public) + mean(out$csb_private) +
      mean(out$csb_none)
    expect_equal(
      total_pct, 1,
      tolerance = 1e-8,
      info = paste0("Failed for csb_priority_method = '", method, "'")
    )
  }
})


# ---- Parameter validation ----

test_that("csb_priority_method rejects invalid values via match.arg", {
  df <- .mock_csb_overlap_df()
  cls <- .mock_classification()

  expect_error(
    sntmethods:::.classify_csb_from_h32(
      data = df,
      h32_cols = c("h32a", "h32b", "h32j", "h32n", "h32s"),
      classification = cls,
      csb_priority_method = "bogus"
    ),
    "should be one of"
  )
})
