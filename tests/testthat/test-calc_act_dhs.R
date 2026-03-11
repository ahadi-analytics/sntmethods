test_that("calc_act_dhs returns named list with adm0 tab from h37e fallback", {
  skip_if_not_installed("survey")
  skip_if_not_installed("haven")

  set.seed(42)
  n <- 200

  kr_data <- data.frame(
    v021 = rep(1:20, each = 10),
    v005 = rep(1000000, n),
    v022 = rep(1:4, each = 50),
    hw1 = sample(0:59, n, replace = TRUE),
    h22 = sample(c(0, 1), n, replace = TRUE, prob = c(0.6, 0.4)),
    b5 = rep(1, n),
    stringsAsFactors = FALSE
  )

  # ml13 series: all zeros (placeholders)
  kr_data$ml13a <- haven::labelled(
    rep(0, n), labels = c("No" = 0, "Yes" = 1, "DK" = 8)
  )
  kr_data$ml13e <- haven::labelled(
    rep(0, n), labels = c("No" = 0, "Yes" = 1, "DK" = 8)
  )

  # h37 series: real data among febrile children
  febrile <- kr_data$h22 == 1
  h37a_vals <- rep(NA_real_, n)
  h37e_vals <- rep(NA_real_, n)
  h37a_vals[febrile] <- sample(c(0, 1), sum(febrile), replace = TRUE, prob = c(0.7, 0.3))
  h37e_vals[febrile] <- sample(c(0, 1), sum(febrile), replace = TRUE, prob = c(0.6, 0.4))
  kr_data$h37a <- haven::labelled(h37a_vals, labels = c("No" = 0, "Yes" = 1))
  kr_data$h37e <- haven::labelled(h37e_vals, labels = c("No" = 0, "Yes" = 1))

  # h32 for CSB
  h32a_vals <- rep(NA_real_, n)
  h32a_vals[febrile] <- sample(c(0, 1), sum(febrile), replace = TRUE)
  kr_data$h32a <- haven::labelled(h32a_vals, labels = c("No" = 0, "Yes" = 1))

  result <- calc_act_dhs(kr_data)

  # Should be a named list with adm0

  expect_type(result, "list")
  expect_true("adm0" %in% names(result))

  adm0 <- result$adm0
  expect_s3_class(adm0, "tbl_df")

  # Expected columns in adm0 tab
  expect_true(all(c("survey_id", "iso3", "iso2", "survey_type", "survey_year",
                     "adm0", "type", "geo_source",
                     "point", "ci_l", "ci_u", "counts", "denominator",
                     "indicator", "indicator_code",
                     "numerator_description",
                     "denominator_description") %in% names(adm0)))

  # type column should always be survey_weighted
  expect_true(all(adm0$type == "survey_weighted"))

  # geo_source should be "survey" for adm0
  expect_true(all(adm0$geo_source == "survey"))

  # No adm1 tab when no grouping
  expect_null(result$adm1)

  # ACT_ANTIMALARIAL should have non-zero estimate
  act_am <- adm0[adm0$indicator_code == "act_antimal", ]
  expect_true(nrow(act_am) == 1)
  expect_equal(act_am$indicator, "Act Antimalarial")
  expect_true(act_am$point > 0,
    label = "ACT_ANTIMALARIAL point should be > 0 from h37e fallback")
  expect_true(act_am$counts > 0)

  # counts = numerator, denominator = condition-filtered subgroup
  expect_true(act_am$counts <= act_am$denominator,
    label = "counts (numerator) should be <= denominator")
  expect_true(act_am$denominator > 0)
  # All indicators should satisfy counts <= denominator
  expect_true(all(adm0$counts <= adm0$denominator, na.rm = TRUE))
})


test_that("calc_act_dhs works with plain ml13e data", {
  skip_if_not_installed("survey")

  set.seed(42)
  n <- 200

  kr_data <- data.frame(
    v021 = rep(1:20, each = 10),
    v005 = rep(1000000, n),
    v022 = rep(1:4, each = 50),
    hw1 = sample(0:59, n, replace = TRUE),
    h22 = sample(c(0, 1), n, replace = TRUE, prob = c(0.6, 0.4)),
    b5 = rep(1, n),
    stringsAsFactors = FALSE
  )

  febrile <- kr_data$h22 == 1
  ml13e_vals <- rep(NA_real_, n)
  ml13e_vals[febrile] <- sample(c(0, 1), sum(febrile), replace = TRUE, prob = c(0.6, 0.4))
  kr_data$ml13e <- ml13e_vals

  ml13a_vals <- rep(NA_real_, n)
  ml13a_vals[febrile] <- sample(c(0, 1), sum(febrile), replace = TRUE, prob = c(0.8, 0.2))
  kr_data$ml13a <- ml13a_vals

  result <- calc_act_dhs(kr_data)

  expect_type(result, "list")
  adm0 <- result$adm0
  expect_s3_class(adm0, "tbl_df")

  # Should have ACT_ANTIMALARIAL at minimum
  act_am <- adm0[adm0$indicator_code == "act_antimal", ]
  expect_true(nrow(act_am) == 1)
  expect_true(act_am$point > 0)
  expect_true(act_am$counts > 0)
})


test_that("calc_act_dhs with region_var returns adm0 + adm1 tabs", {
  skip_if_not_installed("survey")

  set.seed(42)
  n <- 200

  kr_data <- data.frame(
    v021 = rep(1:20, each = 10),
    v005 = rep(1000000, n),
    v022 = rep(1:4, each = 50),
    v024 = rep(c("North", "South"), each = 100),
    hw1 = sample(0:59, n, replace = TRUE),
    h22 = sample(c(0, 1), n, replace = TRUE, prob = c(0.6, 0.4)),
    b5 = rep(1, n),
    stringsAsFactors = FALSE
  )

  febrile <- kr_data$h22 == 1
  ml13e_vals <- rep(NA_real_, n)
  ml13e_vals[febrile] <- sample(c(0, 1), sum(febrile), replace = TRUE, prob = c(0.6, 0.4))
  kr_data$ml13e <- ml13e_vals

  ml13a_vals <- rep(NA_real_, n)
  ml13a_vals[febrile] <- sample(c(0, 1), sum(febrile), replace = TRUE, prob = c(0.8, 0.2))
  kr_data$ml13a <- ml13a_vals

  result <- calc_act_dhs(kr_data, region_var = "v024")

  # Should have both adm0 and adm1 tabs
  expect_type(result, "list")
  expect_true(all(c("adm0", "adm1") %in% names(result)))

  adm0 <- result$adm0
  adm1 <- result$adm1

  # adm0: 1 row per indicator (national)
  act_am_nat <- adm0[adm0$indicator_code == "act_antimal", ]
  expect_true(nrow(act_am_nat) == 1)

  # adm1: 2 rows per indicator (North + South)
  act_am_sub <- adm1[adm1$indicator_code == "act_antimal", ]
  expect_true(nrow(act_am_sub) == 2)

  # adm1 column should exist and be UPPERCASE
  expect_true("adm1" %in% names(adm1))
  expect_true(all(adm1$adm1 == toupper(adm1$adm1)))
  expect_true(all(c("NORTH", "SOUTH") %in% act_am_sub$adm1))

  # geo_source should be "survey" for adm1 tab
  expect_true(all(adm1$geo_source == "survey"))

  # adm0 column present in both tabs
  expect_true("adm0" %in% names(adm0))
  expect_true("adm0" %in% names(adm1))

  expect_true(all(adm0$point >= 0, na.rm = TRUE))
  expect_true(all(adm1$point >= 0, na.rm = TRUE))
})


test_that("calc_act_dhs detects ACT from haven labels when ml13e is not ACT", {
  skip_if_not_installed("survey")
  skip_if_not_installed("haven")

  set.seed(42)
  n <- 200

  kr_data <- data.frame(
    v021 = rep(1:20, each = 10),
    v005 = rep(1000000, n),
    v022 = rep(1:4, each = 50),
    hw1 = sample(0:59, n, replace = TRUE),
    h22 = sample(c(0, 1), n, replace = TRUE, prob = c(0.6, 0.4)),
    b5 = rep(1, n),
    stringsAsFactors = FALSE
  )

  febrile <- kr_data$h22 == 1

  # ml13e: a non-ACT drug
  ml13e_vals <- rep(0, n)
  ml13e_vals[sample(which(febrile), 3)] <- 1
  kr_data$ml13e <- haven::labelled(
    ml13e_vals,
    labels = c("No" = 0, "Yes" = 1),
    label = "given antimalarial drugs: quinine"
  )

  # ml13h: the actual ACT variable
  ml13h_vals <- rep(NA_real_, n)
  ml13h_vals[febrile] <- sample(c(0, 1), sum(febrile), replace = TRUE, prob = c(0.6, 0.4))
  kr_data$ml13h <- haven::labelled(
    ml13h_vals,
    labels = c("No" = 0, "Yes" = 1),
    label = "given antimalarial drugs: act"
  )

  kr_data$ml13a <- haven::labelled(
    rep(0, n),
    labels = c("No" = 0, "Yes" = 1),
    label = "given antimalarial drugs: sp/fansidar"
  )

  result <- calc_act_dhs(kr_data)

  act_am <- result$adm0[result$adm0$indicator_code == "act_antimal", ]
  expect_true(act_am$point > 0.1,
    label = "Should reflect label-detected ACT variable, not ml13e")
})


test_that("calc_act_dhs errors with no ACT data at all", {
  skip_if_not_installed("survey")

  set.seed(42)
  n <- 100

  kr_data <- data.frame(
    v021 = rep(1:10, each = 10),
    v005 = rep(1000000, n),
    v022 = rep(1:2, each = 50),
    hw1 = sample(0:59, n, replace = TRUE),
    h22 = sample(c(0, 1), n, replace = TRUE, prob = c(0.6, 0.4)),
    b5 = rep(1, n),
    stringsAsFactors = FALSE
  )

  expect_error(calc_act_dhs(kr_data), "ACT variable")
})


test_that("calc_act_dhs uses composite ACT when multiple ml13 vars have ACT labels", {
  skip_if_not_installed("survey")
  skip_if_not_installed("haven")

  set.seed(42)
  n <- 300

  kr_data <- data.frame(
    v021 = rep(1:30, each = 10),
    v005 = rep(1000000, n),
    v022 = rep(1:6, each = 50),
    hw1 = sample(0:59, n, replace = TRUE),
    h22 = sample(c(0, 1), n, replace = TRUE, prob = c(0.5, 0.5)),
    b5 = rep(1, n),
    stringsAsFactors = FALSE
  )

  febrile <- kr_data$h22 == 1
  n_febrile <- sum(febrile)

  kr_data$ml13a <- haven::labelled(
    {v <- rep(NA_real_, n); v[febrile] <- sample(c(0, 1), n_febrile, replace = TRUE, prob = c(0.85, 0.15)); v},
    labels = c("No" = 0, "Yes" = 1),
    label = "given antimalarial drugs: sp/fansidar"
  )

  kr_data$ml13e <- haven::labelled(
    {v <- rep(NA_real_, n); v[febrile] <- sample(c(0, 1), n_febrile, replace = TRUE, prob = c(0.95, 0.05)); v},
    labels = c("No" = 0, "Yes" = 1),
    label = "Dihydroartemisinin-piperaquine taken for fever"
  )

  kr_data$ml13f <- haven::labelled(
    {v <- rep(NA_real_, n); v[febrile] <- sample(c(0, 1), n_febrile, replace = TRUE, prob = c(0.3, 0.7)); v},
    labels = c("No" = 0, "Yes" = 1),
    label = "Artemether-lumefantrine taken for fever"
  )

  kr_data$ml13g <- haven::labelled(
    {v <- rep(NA_real_, n); v[febrile] <- sample(c(0, 1), n_febrile, replace = TRUE, prob = c(0.9, 0.1)); v},
    labels = c("No" = 0, "Yes" = 1),
    label = "Artesunate-amodiaquine taken for fever"
  )

  result <- calc_act_dhs(kr_data)

  act_am <- result$adm0[result$adm0$indicator_code == "act_antimal", ]
  expect_true(act_am$point > 0.3,
    label = "Composite ACT should capture all ACT formulations")
})


test_that("calc_act_dhs accepts act as character vector in survey_vars", {
  skip_if_not_installed("survey")
  skip_if_not_installed("haven")

  set.seed(42)
  n <- 200

  kr_data <- data.frame(
    v021 = rep(1:20, each = 10),
    v005 = rep(1000000, n),
    v022 = rep(1:4, each = 50),
    hw1 = sample(0:59, n, replace = TRUE),
    h22 = sample(c(0, 1), n, replace = TRUE, prob = c(0.6, 0.4)),
    b5 = rep(1, n),
    stringsAsFactors = FALSE
  )

  febrile <- kr_data$h22 == 1
  n_febrile <- sum(febrile)

  ml13e_vals <- rep(NA_real_, n)
  ml13e_vals[febrile] <- sample(c(0, 1), n_febrile, replace = TRUE, prob = c(0.95, 0.05))
  kr_data$ml13e <- ml13e_vals

  ml13f_vals <- rep(NA_real_, n)
  ml13f_vals[febrile] <- sample(c(0, 1), n_febrile, replace = TRUE, prob = c(0.4, 0.6))
  kr_data$ml13f <- ml13f_vals

  ml13a_vals <- rep(NA_real_, n)
  ml13a_vals[febrile] <- sample(c(0, 1), n_febrile, replace = TRUE, prob = c(0.8, 0.2))
  kr_data$ml13a <- ml13a_vals

  result <- calc_act_dhs(
    kr_data,
    survey_vars = list(
      cluster = "v021", weight = "v005", stratum = "v022",
      age = "hw1", fever = "h22", alive = "b5",
      act = c("ml13e", "ml13f"), test = "ml13a"
    )
  )

  act_am <- result$adm0[result$adm0$indicator_code == "act_antimal", ]
  expect_true(act_am$point > 0.3,
    label = "Vector act should combine both variables")
})


test_that("calc_act_dhs excludes artemisinin monotherapies from composite ACT", {
  skip_if_not_installed("survey")
  skip_if_not_installed("haven")

  set.seed(42)
  n <- 200

  kr_data <- data.frame(
    v021 = rep(1:20, each = 10),
    v005 = rep(1000000, n),
    v022 = rep(1:4, each = 50),
    hw1 = sample(0:59, n, replace = TRUE),
    h22 = sample(c(0, 1), n, replace = TRUE, prob = c(0.6, 0.4)),
    b5 = rep(1, n),
    stringsAsFactors = FALSE
  )

  febrile <- kr_data$h22 == 1
  n_febrile <- sum(febrile)

  ml13e_vals <- rep(NA_real_, n)
  ml13e_vals[febrile] <- sample(c(0, 1), n_febrile, replace = TRUE, prob = c(0.9, 0.1))
  kr_data$ml13e <- haven::labelled(
    ml13e_vals, labels = c("No" = 0, "Yes" = 1),
    label = "Combination with artemisinin taken for fever/cough"
  )

  ml13aa_vals <- rep(NA_real_, n)
  ml13aa_vals[febrile] <- sample(c(0, 1), n_febrile, replace = TRUE, prob = c(0.5, 0.5))
  kr_data$ml13aa <- haven::labelled(
    ml13aa_vals, labels = c("No" = 0, "Yes" = 1),
    label = "Artesunate rectal taken for fever"
  )

  ml13a_vals <- rep(NA_real_, n)
  ml13a_vals[febrile] <- sample(c(0, 1), n_febrile, replace = TRUE, prob = c(0.8, 0.2))
  kr_data$ml13a <- haven::labelled(
    ml13a_vals, labels = c("No" = 0, "Yes" = 1),
    label = "given antimalarial drugs: sp/fansidar"
  )

  result <- calc_act_dhs(kr_data)

  act_am <- result$adm0[result$adm0$indicator_code == "act_antimal", ]
  expect_true(act_am$point < 0.3,
    label = "Artesunate rectal (monotherapy) should be excluded from ACT")
})


test_that("act_wmr_dictionary returns all 10 indicators", {
  dict <- act_wmr_dictionary()
  expect_s3_class(dict, "tbl_df")
  expect_equal(nrow(dict), 10)
  expect_true(all(c("indicator", "indicator_code", "numerator_description",
                     "denominator_description") %in% names(dict)))
})


test_that("calc_act_dhs indicators parameter filters correctly", {
  skip_if_not_installed("survey")

  set.seed(42)
  n <- 200

  kr_data <- data.frame(
    v021 = rep(1:20, each = 10),
    v005 = rep(1000000, n),
    v022 = rep(1:4, each = 50),
    hw1 = sample(0:59, n, replace = TRUE),
    h22 = sample(c(0, 1), n, replace = TRUE, prob = c(0.6, 0.4)),
    b5 = rep(1, n),
    stringsAsFactors = FALSE
  )

  febrile <- kr_data$h22 == 1
  ml13e_vals <- rep(NA_real_, n)
  ml13e_vals[febrile] <- sample(c(0, 1), sum(febrile), replace = TRUE, prob = c(0.6, 0.4))
  kr_data$ml13e <- ml13e_vals

  ml13a_vals <- rep(NA_real_, n)
  ml13a_vals[febrile] <- sample(c(0, 1), sum(febrile), replace = TRUE, prob = c(0.8, 0.2))
  kr_data$ml13a <- ml13a_vals

  result <- calc_act_dhs(kr_data, indicators = "ACT_ANTIMALARIAL")

  adm0 <- result$adm0
  # Should only have Act Antimalarial
  expect_equal(unique(adm0$indicator), "Act Antimalarial")
  expect_equal(unique(adm0$indicator_code), "act_antimal")
  expect_equal(nrow(adm0), 1)  # national only
})


test_that("calc_act_dhs with v000/v007 populates survey metadata", {
  skip_if_not_installed("survey")
  skip_if_not_installed("haven")

  set.seed(42)
  n <- 200

  kr_data <- data.frame(
    v000 = rep("TG7", n),
    v007 = rep(2017L, n),
    v021 = rep(1:20, each = 10),
    v005 = rep(1000000, n),
    v022 = rep(1:4, each = 50),
    hw1 = sample(0:59, n, replace = TRUE),
    h22 = sample(c(0, 1), n, replace = TRUE, prob = c(0.6, 0.4)),
    b5 = rep(1, n),
    stringsAsFactors = FALSE
  )

  febrile <- kr_data$h22 == 1
  ml13e_vals <- rep(NA_real_, n)
  ml13e_vals[febrile] <- sample(c(0, 1), sum(febrile), replace = TRUE, prob = c(0.6, 0.4))
  kr_data$ml13e <- ml13e_vals
  ml13a_vals <- rep(NA_real_, n)
  ml13a_vals[febrile] <- sample(c(0, 1), sum(febrile), replace = TRUE, prob = c(0.8, 0.2))
  kr_data$ml13a <- ml13a_vals

  result <- calc_act_dhs(kr_data)
  adm0 <- result$adm0

  expect_equal(adm0$survey_id[1], "TG2017DHS")
  expect_equal(adm0$iso3[1], "TGO")
  expect_equal(adm0$iso2[1], "TG")
  expect_equal(adm0$survey_type[1], "DHS")
  expect_equal(adm0$survey_year[1], 2017L)
  expect_equal(adm0$adm0[1], "TOGO")
  expect_equal(adm0$geo_source[1], "survey")
})
