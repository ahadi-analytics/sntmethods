test_that("calc_csb_dhs_core validates input data", {
  # Test with non-dataframe input
  expect_error(
    calc_csb_dhs_core("not a dataframe"),
    "must be a data.frame"
  )

  # Test with empty dataframe
  expect_error(
    calc_csb_dhs_core(data.frame()),
    "is empty"
  )

  # Test with missing required columns
  kr_data <- data.frame(
    v021 = 1:10,
    v005 = rep(1000000, 10)
  )

  expect_error(
    calc_csb_dhs_core(
      kr_data,
      survey_vars = list(
        cluster = "v021",
        weight = "v005",
        stratum = "v022",    # Missing
        age = "hw1",         # Missing
        fever = "h22"        # Missing
      )
    ),
    "Required variables not found"
  )
})

test_that("calc_csb_dhs_core errors when no h32 variables found", {
  skip_if_not_installed("survey")

  # Create data without any h32 variables
  kr_data <- data.frame(
    v021 = rep(1:10, each = 10),
    v005 = rep(1000000, 100),
    v022 = rep(1:2, each = 50),
    v024 = rep("REGION1", 100),
    hw1 = sample(0:59, 100, replace = TRUE),
    h22 = sample(c(0, 1), 100, replace = TRUE, prob = c(0.6, 0.4)),
    stringsAsFactors = FALSE
  )

  # Should error when no h32 treatment-seeking variables found
  expect_error(
    calc_csb_dhs_core(kr_data),
    "No h32 treatment-seeking variables found"
  )
})

test_that("calc_csb_dhs_core works with multiple h32 sources", {
  skip_if_not_installed("survey")

  # Create mock KR data with multiple h32 source variables
  set.seed(123)
  n_children <- 200

  kr_data <- data.frame(
    v021 = rep(1:20, each = 10),           # 20 clusters
    v005 = rep(1000000, n_children),       # Standard weight
    v022 = rep(1:4, each = 50),            # 4 strata
    v024 = rep(c("REGION1", "REGION2"), each = 100),  # 2 regions
    hw1 = sample(0:59, n_children, replace = TRUE),   # Age in months
    h22 = sample(c(0, 1), n_children, replace = TRUE, prob = c(0.7, 0.3)),  # 30% had fever
    b5 = rep(1, n_children)  # All children alive
  )

  # Add h32 source variables (DHS standard facility codes)
  # Public sources
  kr_data$h32a <- ifelse(kr_data$h22 == 1,
    sample(c(0, 1), sum(kr_data$h22 == 1), replace = TRUE, prob = c(0.8, 0.2)), NA)
  kr_data$h32b <- ifelse(kr_data$h22 == 1,
    sample(c(0, 1), sum(kr_data$h22 == 1), replace = TRUE, prob = c(0.7, 0.3)), NA)
  # Private sources
  kr_data$h32j <- ifelse(kr_data$h22 == 1,
    sample(c(0, 1), sum(kr_data$h22 == 1), replace = TRUE, prob = c(0.6, 0.4)), NA)
  kr_data$h32k <- ifelse(kr_data$h22 == 1,
    sample(c(0, 1), sum(kr_data$h22 == 1), replace = TRUE, prob = c(0.5, 0.5)), NA)
  # Traditional healer (excluded by default)
  kr_data$h32t <- ifelse(kr_data$h22 == 1,
    sample(c(0, 1), sum(kr_data$h22 == 1), replace = TRUE, prob = c(0.9, 0.1)), NA)

  result <- calc_csb_dhs_core(kr_data)

  # Result is now a named list with adm0
  expect_type(result, "list")
  expect_true("adm0" %in% names(result))

  adm0 <- result$adm0
  expect_s3_class(adm0, "tbl_df")

  # Expected columns in adm0 tab
  expected_cols <- c(
    "survey_id", "iso3", "iso2", "survey_type",
    "survey_year", "adm0", "type", "geo_source",
    "point", "ci_l", "ci_u", "numerator", "denominator",
    "indicator", "indicator_code",
    "numerator_description",
    "denominator_description", "denominator_code"
  )
  expect_true(all(expected_cols %in% names(adm0)))

  # Check key indicators are present
  expect_true("csb_any" %in% adm0$indicator_code)
  expect_true("csb_public" %in% adm0$indicator_code)
  expect_true("csb_private" %in% adm0$indicator_code)
  expect_true("csb_none" %in% adm0$indicator_code)

  # Check that all point estimates are between 0 and 1
  csb_any_row <- adm0[adm0$indicator_code == "csb_any", ]
  csb_pub_row <- adm0[adm0$indicator_code == "csb_public", ]
  csb_prv_row <- adm0[adm0$indicator_code == "csb_private", ]
  csb_none_row <- adm0[adm0$indicator_code == "csb_none", ]

  expect_true(all(adm0$point >= 0 & adm0$point <= 1, na.rm = TRUE))

  # Check that confidence intervals are within bounds
  expect_true(all(csb_any_row$ci_l >= 0, na.rm = TRUE))
  expect_true(all(csb_any_row$ci_u <= 1, na.rm = TRUE))

  # Check that csb_any + csb_none approximately equals 1
  expect_true(
    abs(csb_any_row$point + csb_none_row$point - 1) < 0.01
  )

  # adm1 tab auto-produced via v024 fallback (data has v024)
  expect_false(is.null(result$adm1))
  expect_s3_class(result$adm1, "tbl_df")
})

test_that("calc_csb_dhs_core excludes deceased children (b5 == 0)", {
  skip_if_not_installed("survey")

  set.seed(456)
  n_children <- 100

  # Create data with some deceased children
  kr_data <- data.frame(
    v021 = rep(1:10, each = 10),
    v005 = rep(1000000, n_children),
    v022 = rep(1:2, each = 50),
    v024 = rep("REGION1", n_children),
    hw1 = sample(0:59, n_children, replace = TRUE),
    h22 = sample(c(0, 1), n_children, replace = TRUE, prob = c(0.5, 0.5)),
    b5 = sample(c(0, 1), n_children, replace = TRUE, prob = c(0.2, 0.8))  # 20% deceased
  )

  # Add h32 sources
  kr_data$h32a <- ifelse(kr_data$h22 == 1,
    sample(c(0, 1), sum(kr_data$h22 == 1), replace = TRUE), NA)
  kr_data$h32j <- ifelse(kr_data$h22 == 1,
    sample(c(0, 1), sum(kr_data$h22 == 1), replace = TRUE), NA)

  # Count alive children with fever (alive filter now applied)
  expected_fever <- sum(kr_data$h22 == 1 & kr_data$b5 == 1)

  result <- calc_csb_dhs_core(kr_data)

  # The sample size should only include alive children with fever
  adm0 <- result$adm0
  csb_any_row <- adm0[adm0$indicator_code == "csb_any", ]
  expect_equal(csb_any_row$denominator, expected_fever)
})

test_that("calc_csb_dhs_core handles custom source_config", {
  skip_if_not_installed("survey")

  set.seed(789)
  n_children <- 150

  kr_data <- data.frame(
    v021 = rep(1:15, each = 10),
    v005 = rep(1000000, n_children),
    v022 = rep(1:3, each = 50),
    hw1 = sample(0:59, n_children, replace = TRUE),
    h22 = sample(c(0, 1), n_children, replace = TRUE, prob = c(0.6, 0.4)),
    b5 = rep(1, n_children)
  )

  # Add many h32 sources
  kr_data$h32a <- ifelse(kr_data$h22 == 1, sample(c(0, 1), sum(kr_data$h22 == 1), replace = TRUE), NA)
  kr_data$h32b <- ifelse(kr_data$h22 == 1, sample(c(0, 1), sum(kr_data$h22 == 1), replace = TRUE), NA)
  kr_data$h32c <- ifelse(kr_data$h22 == 1, sample(c(0, 1), sum(kr_data$h22 == 1), replace = TRUE), NA)
  kr_data$h32j <- ifelse(kr_data$h22 == 1, sample(c(0, 1), sum(kr_data$h22 == 1), replace = TRUE), NA)
  kr_data$h32k <- ifelse(kr_data$h22 == 1, sample(c(0, 1), sum(kr_data$h22 == 1), replace = TRUE), NA)
  kr_data$h32t <- ifelse(kr_data$h22 == 1, sample(c(0, 1), sum(kr_data$h22 == 1), replace = TRUE), NA)

  # Test with custom source_config
  result <- calc_csb_dhs_core(
    kr_data,
    survey_vars = list(
      cluster = "v021",
      weight = "v005",
      stratum = "v022",
      age = "hw1",
      fever = "h22",
      alive = "b5"
    ),
    source_config = list(
      public = c("h32a", "h32b"),  # Only use these as public
      private = c("h32j"),          # Only use this as private
      excluded = c("h32t", "h32k")  # Exclude pharmacy and traditional
    )
  )

  expect_type(result, "list")
  expect_true("adm0" %in% names(result))

  adm0 <- result$adm0
  expect_s3_class(adm0, "tbl_df")

  # Check key indicator codes are present
  expect_true("csb_any" %in% adm0$indicator_code)
  expect_true("csb_public" %in% adm0$indicator_code)
  expect_true("csb_private" %in% adm0$indicator_code)
})

test_that("calc_csb_dhs_core handles children without fever correctly", {
  skip_if_not_installed("survey")

  # Create data where no children had fever
  kr_data <- data.frame(
    v021 = rep(1:5, each = 10),
    v005 = rep(1000000, 50),
    v022 = rep(1, 50),
    v024 = rep("REGION1", 50),
    hw1 = sample(0:59, 50, replace = TRUE),
    h22 = rep(0, 50),  # No fever
    b5 = rep(1, 50),
    h32a = rep(NA, 50),
    h32j = rep(NA, 50)
  )

  expect_error(
    calc_csb_dhs_core(kr_data),
    "No children with fever"
  )
})

test_that("calc_csb_dhs wrapper returns same named list structure", {
  skip_if_not_installed("survey")

  # Create mock KR data
  set.seed(456)
  n_children <- 100

  kr_data <- data.frame(
    v000 = rep("SL7", n_children),         # Country code
    v007 = rep(2019, n_children),          # Survey year
    v021 = rep(1:10, each = 10),           # 10 clusters
    v005 = rep(1000000, n_children),       # Weight
    v022 = rep(1:2, each = 50),            # Strata
    v024 = rep("WESTERN", n_children),     # Admin1
    hw1 = sample(0:59, n_children, replace = TRUE),
    h22 = sample(c(0, 1), n_children, replace = TRUE, prob = c(0.6, 0.4)),  # 40% fever
    b5 = rep(1, n_children)
  )

  # Add h32 sources
  kr_data$h32a <- ifelse(kr_data$h22 == 1, sample(c(0, 1), sum(kr_data$h22 == 1), replace = TRUE), NA)
  kr_data$h32b <- ifelse(kr_data$h22 == 1, sample(c(0, 1), sum(kr_data$h22 == 1), replace = TRUE), NA)
  kr_data$h32j <- ifelse(kr_data$h22 == 1, sample(c(0, 1), sum(kr_data$h22 == 1), replace = TRUE), NA)

  result <- calc_csb_dhs(kr_data)

  # Result is now a named list with adm0, same as calc_csb_dhs_core
  expect_type(result, "list")
  expect_true("adm0" %in% names(result))

  adm0 <- result$adm0
  expect_s3_class(adm0, "tbl_df")

  # Expected columns
  expected_cols <- c(
    "survey_id", "iso3", "iso2", "survey_type",
    "survey_year", "adm0", "type", "geo_source",
    "point", "ci_l", "ci_u", "numerator", "denominator",
    "indicator", "indicator_code",
    "numerator_description",
    "denominator_description", "denominator_code"
  )
  expect_true(all(expected_cols %in% names(adm0)))

  # Check key indicator codes are present
  expect_true("csb_any" %in% adm0$indicator_code)
  expect_true("csb_public" %in% adm0$indicator_code)
  expect_true("csb_private" %in% adm0$indicator_code)

  # Check survey metadata is populated from v000/v007
  expect_equal(adm0$survey_id[1], "SL2019DHS")
  expect_equal(adm0$iso2[1], "SL")
  expect_equal(adm0$survey_year[1], 2019)

  # type column should always be survey_weighted
  expect_true(all(adm0$type == "survey_weighted"))

  # geo_source should be "survey" when no GPS data
  expect_true(all(adm0$geo_source == "survey"))
})

test_that("calc_csb_dhs_core produces consistent results", {
  skip_if_not_installed("survey")

  # Set seed for reproducibility
  set.seed(999)

  # Create consistent test data
  kr_data <- data.frame(
    v021 = rep(1:5, each = 20),
    v005 = rep(1000000, 100),
    v022 = rep(1, 100),
    v024 = rep("REGION1", 100),
    hw1 = rep(c(6, 12, 24, 36, 48), 20),  # Fixed ages
    h22 = rep(c(0, 0, 1, 1, 1), 20),      # 60% with fever
    b5 = rep(1, 100),                      # All alive
    h32a = rep(c(NA, NA, 1, 0, 0), 20),   # Public source
    h32b = rep(c(NA, NA, 0, 1, 0), 20),   # Public source
    h32j = rep(c(NA, NA, 0, 1, 0), 20)    # Private source
  )

  result1 <- calc_csb_dhs_core(kr_data)
  result2 <- calc_csb_dhs_core(kr_data)

  adm0_1 <- result1$adm0
  adm0_2 <- result2$adm0

  # Results should be identical for the same input
  csb_any_1 <- adm0_1[adm0_1$indicator_code == "csb_any", ]
  csb_any_2 <- adm0_2[adm0_2$indicator_code == "csb_any", ]
  expect_equal(csb_any_1$point, csb_any_2$point)

  csb_pub_1 <- adm0_1[adm0_1$indicator_code == "csb_public", ]
  csb_pub_2 <- adm0_2[adm0_2$indicator_code == "csb_public", ]
  expect_equal(csb_pub_1$point, csb_pub_2$point)

  csb_prv_1 <- adm0_1[adm0_1$indicator_code == "csb_private", ]
  csb_prv_2 <- adm0_2[adm0_2$indicator_code == "csb_private", ]
  expect_equal(csb_prv_1$point, csb_prv_2$point)

  # Denominator should match
  expect_equal(csb_any_1$denominator, csb_any_2$denominator)
})

test_that("calc_csb_dhs_core calculates overlapping indicators (standard DHS methodology)", {
  skip_if_not_installed("survey")

  # Create data where we can verify the calculation
  # 10 children with fever, each visited different combinations of sources
  # With OVERLAPPING indicators (standard DHS methodology):
  # - A child can be counted in BOTH public AND private if they visited both
  kr_data <- data.frame(
    v021 = 1:10,
    v005 = rep(1000000, 10),
    v022 = rep(1, 10),
    hw1 = rep(24, 10),
    h22 = rep(1, 10),  # All have fever
    b5 = rep(1, 10),   # All alive
    # Child 1-3: visited public only (h32a or h32b)
    # Child 4-6: visited private only (h32j)
    # Child 7-8: visited BOTH public and private
    # Child 9-10: visited none
    h32a = c(1, 0, 0, 0, 0, 0, 1, 0, 0, 0),
    h32b = c(0, 1, 1, 0, 0, 0, 0, 1, 0, 0),
    h32j = c(0, 0, 0, 1, 1, 1, 1, 1, 0, 0)
  )

  result <- calc_csb_dhs_core(kr_data)
  adm0 <- result$adm0

  # Extract point estimates by indicator_code
  csb_public <- adm0[adm0$indicator_code == "csb_public", "point"][[1]]
  csb_private <- adm0[adm0$indicator_code == "csb_private", "point"][[1]]
  csb_none <- adm0[adm0$indicator_code == "csb_none", "point"][[1]]
  csb_any <- adm0[adm0$indicator_code == "csb_any", "point"][[1]]

  # With OVERLAPPING indicators (standard DHS methodology):
  # - Public: children 1,2,3,7,8 = 5/10 = 0.5
  # - Private: children 4,5,6,7,8 = 5/10 = 0.5 (includes children who visited BOTH!)
  # - None: children 9,10 = 2/10 = 0.2
  # - Any: all who visited any = 8/10 = 0.8

  expect_equal(csb_public, 0.5)
  expect_equal(csb_private, 0.5)  # Overlapping - includes children 7,8

  expect_equal(csb_none, 0.2)
  expect_equal(csb_any, 0.8)

  # NOTE: With overlapping indicators, public + private + none does NOT need to sum to 1
  # (public + private can exceed 1 when children visit both sectors)
  expect_equal(csb_public + csb_private + csb_none, 1.2)
})

test_that("overlapping indicators - all visited both sectors", {
  skip_if_not_installed("survey")

  # All children visited BOTH public AND private
  # With overlapping indicators (standard DHS), they are counted in BOTH categories
  kr_data <- data.frame(
    v021 = 1:10,              # 10 clusters
    v005 = rep(1000000, 10),
    v022 = rep(1, 10),
    hw1 = rep(24, 10),
    h22 = rep(1, 10),
    b5 = rep(1, 10),
    h32a = rep(1, 10),  # All visited public
    h32j = rep(1, 10)   # All also visited private
  )

  result <- calc_csb_dhs_core(kr_data)
  adm0 <- result$adm0

  csb_public <- adm0[adm0$indicator_code == "csb_public", "point"][[1]]
  csb_private <- adm0[adm0$indicator_code == "csb_private", "point"][[1]]
  csb_none <- adm0[adm0$indicator_code == "csb_none", "point"][[1]]
  csb_any <- adm0[adm0$indicator_code == "csb_any", "point"][[1]]

  # With overlapping indicators (standard DHS methodology):
  # ALL children are counted in BOTH public AND private
  expect_equal(csb_public, 1)
  expect_equal(csb_private, 1)  # Also 1 - overlapping!
  expect_equal(csb_none, 0)
  expect_equal(csb_any, 1)

  # With overlapping indicators, sum exceeds 1 when children visit both sectors
  expect_equal(csb_public + csb_private + csb_none, 2)
})

test_that("calc_csb_dhs_core with region_var returns adm0 + adm1 tabs", {
  skip_if_not_installed("survey")

  set.seed(321)
  n_children <- 200

  kr_data <- data.frame(
    v021 = rep(1:20, each = 10),
    v005 = rep(1000000, n_children),
    v022 = rep(1:4, each = 50),
    v024 = rep(c("REGION1", "REGION2"), each = 100),
    hw1 = sample(0:59, n_children, replace = TRUE),
    h22 = sample(c(0, 1), n_children, replace = TRUE, prob = c(0.7, 0.3)),
    b5 = rep(1, n_children)
  )

  kr_data$h32a <- ifelse(kr_data$h22 == 1,
    sample(c(0, 1), sum(kr_data$h22 == 1), replace = TRUE, prob = c(0.8, 0.2)), NA)
  kr_data$h32j <- ifelse(kr_data$h22 == 1,
    sample(c(0, 1), sum(kr_data$h22 == 1), replace = TRUE, prob = c(0.6, 0.4)), NA)

  result <- calc_csb_dhs_core(kr_data, region_var = "v024")

  # Should have both adm0 and adm1 tabs
  expect_type(result, "list")
  expect_true(all(c("adm0", "adm1") %in% names(result)))

  adm0 <- result$adm0
  adm1 <- result$adm1

  # adm0: 1 row per indicator (national)
  csb_any_nat <- adm0[adm0$indicator_code == "csb_any", ]
  expect_true(nrow(csb_any_nat) == 1)

  # adm1: 2 rows per indicator (REGION1 + REGION2)
  csb_any_sub <- adm1[adm1$indicator_code == "csb_any", ]
  expect_true(nrow(csb_any_sub) == 2)

  # adm1 column should exist and be UPPERCASE
  expect_true("adm1" %in% names(adm1))
  expect_true(all(adm1$adm1 == toupper(adm1$adm1)))
  expect_true(all(c("REGION1", "REGION2") %in% csb_any_sub$adm1))

  # geo_source should be "survey" for both tabs
  expect_true(all(adm0$geo_source == "survey"))
  expect_true(all(adm1$geo_source == "survey"))

  # adm0 column present in both tabs
  expect_true("adm0" %in% names(adm0))
  expect_true("adm0" %in% names(adm1))

  # All point estimates should be valid
  expect_true(all(adm0$point >= 0, na.rm = TRUE))
  expect_true(all(adm1$point >= 0, na.rm = TRUE))
})

test_that("calc_csb_dhs_core errors when region_var column not found", {
  kr_data <- data.frame(
    v021 = rep(1:5, each = 10),
    v005 = rep(1000000, 50),
    v022 = rep(1, 50),
    hw1 = sample(0:59, 50, replace = TRUE),
    h22 = sample(c(0, 1), 50, replace = TRUE),
    h32a = sample(c(0, 1, NA), 50, replace = TRUE)
  )

  expect_error(
    calc_csb_dhs_core(kr_data, region_var = "nonexistent"),
    "not found in `dhs_kr`"
  )
})

test_that("calc_csb_dhs_core errors when region_var is not a single string", {
  kr_data <- data.frame(
    v021 = rep(1:5, each = 10),
    v005 = rep(1000000, 50),
    v022 = rep(1, 50),
    v024 = rep("REGION1", 50),
    hw1 = sample(0:59, 50, replace = TRUE),
    h22 = sample(c(0, 1), 50, replace = TRUE),
    h32a = sample(c(0, 1, NA), 50, replace = TRUE)
  )

  # Multiple strings
  expect_error(
    calc_csb_dhs_core(kr_data, region_var = c("v024", "v025")),
    "single character string"
  )

  # Not a string
  expect_error(
    calc_csb_dhs_core(kr_data, region_var = 123),
    "single character string"
  )
})

test_that("calc_csb_dhs_core returns all 11 CSB indicators at adm0 level", {
  skip_if_not_installed("survey")

  set.seed(42)
  n_children <- 200

  kr_data <- data.frame(
    v021 = rep(1:20, each = 10),
    v005 = rep(1000000, n_children),
    v022 = rep(1:4, each = 50),
    hw1 = sample(0:59, n_children, replace = TRUE),
    h22 = sample(c(0, 1), n_children, replace = TRUE, prob = c(0.6, 0.4)),
    b5 = rep(1, n_children)
  )

  # Add public, CHW, private formal, pharmacy, private informal sources
  kr_data$h32a <- ifelse(kr_data$h22 == 1, sample(c(0, 1), sum(kr_data$h22 == 1), replace = TRUE, prob = c(0.7, 0.3)), NA)
  kr_data$h32b <- ifelse(kr_data$h22 == 1, sample(c(0, 1), sum(kr_data$h22 == 1), replace = TRUE, prob = c(0.8, 0.2)), NA)
  kr_data$h32na <- ifelse(kr_data$h22 == 1, sample(c(0, 1), sum(kr_data$h22 == 1), replace = TRUE, prob = c(0.9, 0.1)), NA)  # CHW
  kr_data$h32j <- ifelse(kr_data$h22 == 1, sample(c(0, 1), sum(kr_data$h22 == 1), replace = TRUE, prob = c(0.6, 0.4)), NA)  # private formal
  kr_data$h32n <- ifelse(kr_data$h22 == 1, sample(c(0, 1), sum(kr_data$h22 == 1), replace = TRUE, prob = c(0.7, 0.3)), NA)  # pharmacy
  kr_data$h32s <- ifelse(kr_data$h22 == 1, sample(c(0, 1), sum(kr_data$h22 == 1), replace = TRUE, prob = c(0.85, 0.15)), NA)  # private informal

  result <- calc_csb_dhs_core(kr_data)
  adm0 <- result$adm0

  # All 11 CSB indicator codes should be present
  expected_codes <- c(
    "csb_any", "csb_none", "csb_public", "csb_pub_nochw",
    "csb_chw", "csb_private", "csb_priv_formal", "csb_pharmacy",
    "csb_priv_informal", "csb_priv_form_pha", "csb_trained"
  )
  for (code in expected_codes) {
    expect_true(
      code %in% adm0$indicator_code,
      info = paste("Missing indicator_code:", code)
    )
  }

  # Each indicator should have exactly 1 row at adm0
  expect_equal(nrow(adm0), length(expected_codes))

  # counts should always be <= denominator
  expect_true(all(adm0$numerator <= adm0$denominator, na.rm = TRUE))
})

test_that("csb_wmr_dictionary returns all 11 indicators with metadata", {
  dict <- csb_wmr_dictionary()
  expect_s3_class(dict, "tbl_df")
  expect_equal(nrow(dict), 11)
  expect_true(all(
    c("indicator", "indicator_code", "indicator_title",
      "numerator_description",
      "denominator_description",
      "denominator_code") %in% names(dict)
  ))
})

test_that("aggregate_csb_admin works with new output format", {
  skip_if_not_installed("sf")

  # aggregate_csb_admin still expects old-style column names (dhs_csb_any, etc.)
  # Create mock cluster results matching the column names aggregate_csb_admin expects
  cluster_results <- data.frame(
    cluster_id = 1:6,
    lat = c(-8.5, -8.3, -8.7, -8.4, -8.6, -8.2),
    lon = c(-11.2, -11.0, -11.5, -11.3, -11.1, -11.4),
    dhs_csb_any = c(0.652, 0.721, 0.583, 0.705, 0.628, 0.689),
    dhs_csb_public = c(0.255, 0.302, 0.221, 0.287, 0.243, 0.275),
    dhs_csb_private = c(0.452, 0.501, 0.385, 0.483, 0.412, 0.468),
    dhs_csb_none = c(0.348, 0.279, 0.417, 0.295, 0.372, 0.311),
    dhs_n_fever = c(25, 30, 28, 22, 26, 24)
  )

  # Create mock shapefile
  library(sf)
  polygons <- st_sfc(
    st_polygon(list(
      cbind(c(-11.6, -11.6, -11.0, -11.0, -11.6),
            c(-8.8, -8.2, -8.2, -8.8, -8.8))
    )),
    st_polygon(list(
      cbind(c(-11.0, -11.0, -10.4, -10.4, -11.0),
            c(-8.8, -8.2, -8.2, -8.8, -8.8))
    ))
  )

  shapefile <- st_sf(
    adm1 = c("REGION1", "REGION2"),
    adm1_name = c("Region One", "Region Two"),
    geometry = polygons,
    crs = 4326
  )

  # Test aggregation (aggregate_csb_admin still uses old column names internally)
  result <- aggregate_csb_admin(
    cluster_results,
    shapefile,
    admin_level = "adm1",
    weighted = TRUE
  )

  expect_s3_class(result, "sf")
  expect_true("dhs_csb_any" %in% names(result))
  expect_true("dhs_csb_public" %in% names(result))
  expect_true("dhs_csb_private" %in% names(result))
  expect_true("dhs_n_fever" %in% names(result))
  expect_true("adm1" %in% names(result))
  expect_true("adm1_name" %in% names(result))
  expect_equal(nrow(result), 2)  # Two regions

  # Check that aggregated values are reasonable (proportions 0-1)
  expect_true(all(result$dhs_csb_any >= 0 & result$dhs_csb_any <= 1))
  expect_true(all(result$dhs_n_fever > 0))
})
