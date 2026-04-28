# Tests for the defensive cleanup added to .prepare_csb_data():
#   * h22 / fever values outside {0, 1, 2} are coerced to NA before the
#     fever subset and before fever-coding auto-detection
#   * rows with NA in cluster_id (and, for survey-design mode, weight or
#     stratum) are dropped with a logged drop count BEFORE svydesign()
#     gets a chance to abort

# ---- mock data helper ----

.mock_kr_for_sanitize <- function(seed = 1) {
  set.seed(seed)
  n_clusters <- 10
  per_cluster <- 5
  n <- n_clusters * per_cluster

  df <- data.frame(
    v021 = rep(seq_len(n_clusters), each = per_cluster),
    v005 = rep(1000000, n),
    v022 = rep(1:2, each = n / 2),
    v024 = rep("REGION1", n),
    hw1  = sample(0:59, n, replace = TRUE),
    h22  = rep(1L, n),
    b5   = rep(1L, n),
    stringsAsFactors = FALSE
  )

  # h32 source variables (mutually exclusive 1's so the partition is clean)
  group <- sample(c("a", "j", "x"), size = n, replace = TRUE,
                  prob = c(0.5, 0.3, 0.2))
  df$h32a <- as.integer(group == "a")
  df$h32j <- as.integer(group == "j")
  df$h32x <- as.integer(group == "x")
  df$h32y <- 0L
  df$h32z <- 0L

  attr(df$h32a, "label") <- "Government hospital"
  attr(df$h32j, "label") <- "Private hospital/clinic"
  attr(df$h32x, "label") <- "Other"
  attr(df$h32y, "label") <- "No treatment"
  attr(df$h32z, "label") <- "Medical treatment (any source)"

  df
}


# ---- h22 sanitization ----

test_that(".prepare_csb_data coerces h22 codes 8/9 to NA", {
  kr <- .mock_kr_for_sanitize()

  # Inject DHS "don't know" / "missing" codes on a few rows
  kr$h22[1:3] <- 8L
  kr$h22[4:5] <- 9L

  prepared <- sntmethods:::.prepare_csb_data(
    dhs_kr = kr,
    survey_vars = list(
      cluster = "v021", weight = "v005", stratum = "v022",
      age = "hw1", fever = "h22", alive = "b5"
    ),
    include_survey_vars = TRUE,
    csb_priority_method = "all"
  )

  # 50 rows total - 5 were 8/9 (coerced to NA, then dropped by fever filter)
  # The remaining 45 rows had h22 = 1 (yes), so they all survive.
  expect_equal(nrow(prepared), 45L)
  # had_fever after sanitization should only contain valid yes-codes (1)
  expect_setequal(unique(prepared$had_fever), 1)
})

test_that("calc_csb_dhs_core succeeds with mixed h22 values including 8/9", {
  skip_if_not_installed("survey")

  kr <- .mock_kr_for_sanitize(seed = 42)
  # Inject 8/9 codes - this previously could leak into the fever subset
  kr$h22[1:5] <- 8L
  kr$h22[6:10] <- 9L

  result <- calc_csb_dhs_core(kr)

  expect_type(result, "list")
  expect_true("adm0" %in% names(result))
  expect_true("csb_any" %in% result$adm0$indicator_code)
})


# ---- NA-in-design-columns drop ----

test_that(".prepare_csb_data drops rows with NA cluster id", {
  kr <- .mock_kr_for_sanitize()
  kr$v021[1:7] <- NA_integer_

  prepared <- sntmethods:::.prepare_csb_data(
    dhs_kr = kr,
    survey_vars = list(
      cluster = "v021", weight = "v005", stratum = "v022",
      age = "hw1", fever = "h22", alive = "b5"
    ),
    include_survey_vars = TRUE,
    csb_priority_method = "all"
  )

  # 50 rows - 7 NA-cluster = 43 surviving febrile children
  expect_equal(nrow(prepared), 43L)
  expect_false(anyNA(prepared$cluster_id))
})

test_that(".prepare_csb_data drops rows with NA weight or stratum (survey mode)", {
  kr <- .mock_kr_for_sanitize()
  kr$v005[1:2] <- NA_real_
  kr$v022[3:4] <- NA_integer_

  prepared <- sntmethods:::.prepare_csb_data(
    dhs_kr = kr,
    survey_vars = list(
      cluster = "v021", weight = "v005", stratum = "v022",
      age = "hw1", fever = "h22", alive = "b5"
    ),
    include_survey_vars = TRUE,
    csb_priority_method = "all"
  )

  # 50 rows - 4 dropped = 46
  expect_equal(nrow(prepared), 46L)
  expect_false(anyNA(prepared$survey_weight))
  expect_false(anyNA(prepared$stratum_id))
})

test_that("calc_csb_dhs_core no longer aborts with 'missing values in id'", {
  skip_if_not_installed("survey")

  # Reproduce the original error scenario: NA cluster IDs scattered through
  # the dataset, plus a few invalid h22 codes. Pre-fix this would crash
  # inside survey::svydesign() with `missing values in 'id'`.
  kr <- .mock_kr_for_sanitize(seed = 7)
  kr$v021[c(2, 11, 22, 33, 44)] <- NA_integer_
  kr$h22[c(5, 15)] <- 9L

  expect_error(
    calc_csb_dhs_core(kr),
    NA  # explicitly: NO error
  )
})

test_that(".prepare_csb_data aborts when ALL rows have NA cluster id", {
  kr <- .mock_kr_for_sanitize()
  kr$v021 <- NA_integer_

  expect_error(
    sntmethods:::.prepare_csb_data(
      dhs_kr = kr,
      survey_vars = list(
        cluster = "v021", weight = "v005", stratum = "v022",
        age = "hw1", fever = "h22", alive = "b5"
      ),
      include_survey_vars = TRUE,
      csb_priority_method = "all"
    ),
    regexp = "All rows have NA in survey-design columns"
  )
})
