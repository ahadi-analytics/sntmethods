# Tests for the user-defined custom_csb_indicator pipeline addition.
#
# Covers:
#   * Spec validation (.validate_custom_csb_indicator_spec)
#   * Label normalization (.normalize_custom_csb_label)
#   * Per-survey classification building (.build_custom_csb_classification)
#   * Child-level classification with priority (.classify_custom_csb_from_h32)
#   * Generalized .normalize_csb_mbg_estimates() with custom sector_names
#   * Dictionary-row helpers (.custom_csb_dictionary_rows)
#   * Threading through .valid_mbg_indicators(), .mbg_indicator_pop_type()


# ---- Helpers ----

.mock_kr_with_labels <- function() {
  # Build a tiny KR-like data frame with haven-style labels on h32 vars.
  # We emulate haven attributes by attaching `label` directly.
  df <- data.frame(
    v001  = c(1, 1, 2, 2, 3),
    hw1   = c(12, 36, 48, 24, 6),
    h22   = c(1, 1, 1, 1, 1),
    h32a  = c(1, 0, 1, 0, 0),  # public hosp -> dhis
    h32j  = c(0, 1, 1, 0, 0),  # private clinic -> nondhis
    h32n  = c(0, 0, 0, 1, 0),  # pharmacy -> nondhis
    h32x  = c(0, 0, 0, 0, 1),  # other -> untreat
    h32y  = c(0, 0, 0, 0, 0),
    h32z  = c(0, 0, 0, 0, 0),
    stringsAsFactors = FALSE
  )
  attr(df$h32a, "label") <- "Government hospital"
  attr(df$h32j, "label") <- "Private hospital/clinic"
  attr(df$h32n, "label") <- "Pharmacy"
  attr(df$h32x, "label") <- "Other"
  attr(df$h32y, "label") <- "No treatment"
  attr(df$h32z, "label") <- "Medical treatment (any source)"
  df
}

.mock_spec <- function() {
  list(
    name = "csb_eff",
    dhis_locs = c("Government hospital"),
    nondhis_locs = c("Private hospital/clinic", "Pharmacy"),
    untreat_locs = c("Other")
  )
}


# ---- .normalize_custom_csb_label() ----

test_that(".normalize_custom_csb_label lowercases, trims, collapses whitespace", {
  expect_equal(
    sntmethods:::.normalize_custom_csb_label(c("  Foo  Bar  ", "BAZ", NA)),
    c("foo bar", "baz", NA_character_)
  )
  expect_equal(sntmethods:::.normalize_custom_csb_label(character(0)), character(0))
  expect_equal(sntmethods:::.normalize_custom_csb_label(NULL), character(0))
})


# ---- .validate_custom_csb_indicator_spec() ----

test_that("validator accepts a well-formed spec", {
  spec <- sntmethods:::.validate_custom_csb_indicator_spec(.mock_spec())
  expect_equal(spec$name, "csb_eff")
  expect_equal(attr(spec, "label_norm_dhis"), "government hospital")
  expect_setequal(
    attr(spec, "label_norm_nondhis"),
    c("private hospital/clinic", "pharmacy")
  )
})

test_that("validator rejects missing fields", {
  bad <- list(name = "csb_eff", dhis_locs = "x", nondhis_locs = "y")
  expect_error(
    sntmethods:::.validate_custom_csb_indicator_spec(bad),
    regexp = "untreat_locs"
  )
})

test_that("validator rejects bad name pattern", {
  bad <- .mock_spec()
  bad$name <- "Bad-Name"
  expect_error(
    sntmethods:::.validate_custom_csb_indicator_spec(bad),
    regexp = "valid identifier"
  )
})

test_that("validator rejects collisions with built-in CSB indicators", {
  bad <- .mock_spec()
  bad$name <- "csb_any"
  expect_error(
    sntmethods:::.validate_custom_csb_indicator_spec(bad),
    regexp = "collides with a built-in"
  )
})

test_that("validator rejects overlapping label lists", {
  bad <- .mock_spec()
  bad$nondhis_locs <- c(bad$nondhis_locs, "Government hospital")
  expect_error(
    sntmethods:::.validate_custom_csb_indicator_spec(bad),
    regexp = "overlap"
  )
})


# ---- .build_custom_csb_classification() ----

test_that("classification builder maps every observed h32 label", {
  spec <- sntmethods:::.validate_custom_csb_indicator_spec(.mock_spec())
  cls <- sntmethods:::.build_custom_csb_classification(
    .mock_kr_with_labels(), spec
  )
  expect_setequal(cls$variable, c("h32a", "h32j", "h32n", "h32x"))
  expect_equal(
    cls$csb_custom[cls$variable == "h32a"],
    "dhis"
  )
  expect_setequal(
    cls$csb_custom[cls$variable %in% c("h32j", "h32n")],
    "nondhis"
  )
  expect_equal(
    cls$csb_custom[cls$variable == "h32x"],
    "untreat"
  )
})

test_that("classification builder skips unmapped labels with info (no abort)", {
  bad <- .mock_spec()
  # Drop "Other" so h32x is unmapped: should NOT abort, should just be skipped
  bad$untreat_locs <- character(0)
  cls <- expect_no_error(
    sntmethods:::.build_custom_csb_classification(.mock_kr_with_labels(), bad)
  )
  # h32x is dropped from the classification (will fall through to residual
  # untreat in .classify_custom_csb_from_h32 if a child only has h32x = 1)
  expect_false("h32x" %in% cls$variable)
  expect_setequal(cls$variable, c("h32a", "h32j", "h32n"))
})

test_that("force_keep_vars retains h32 columns whose label starts with 'NA -'", {
  kr <- .mock_kr_with_labels()
  # Simulate a country-specific placeholder label on h32i
  kr$h32i <- c(1, 0, 0, 0, 0)
  attr(kr$h32i, "label") <- "NA - country specific"

  # Without force_keep, h32i is dropped (starts with NA -)
  obs0 <- sntmethods:::.extract_custom_csb_h32_labels(kr)
  expect_false("h32i" %in% obs0$variable)

  # With force_keep_vars, h32i is retained even with NA- label
  obs1 <- sntmethods:::.extract_custom_csb_h32_labels(kr, force_keep_vars = "h32i")
  expect_true("h32i" %in% obs1$variable)
  i_row <- obs1[obs1$variable == "h32i", , drop = FALSE]
  expect_equal(i_row$raw_label, "")
  expect_true(is.na(i_row$label_norm))
})

test_that(".build_custom_csb_classification routes NA-labeled h32 var by name", {
  kr <- .mock_kr_with_labels()
  # Add an h32i column with a country-specific 'NA -' placeholder label,
  # but route it explicitly via dhis_locs by variable name.
  kr$h32i <- c(1, 0, 0, 0, 0)
  attr(kr$h32i, "label") <- "NA - country specific"

  spec <- .mock_spec()
  spec$dhis_locs <- c(spec$dhis_locs, "h32i")

  cls <- sntmethods:::.build_custom_csb_classification(kr, spec)
  expect_true("h32i" %in% cls$variable)
  expect_equal(cls$csb_custom[cls$variable == "h32i"], "dhis")
})

test_that(".zero_fill_custom_csb_cluster_data with NULL returns empty schema", {
  out <- sntmethods:::.zero_fill_custom_csb_cluster_data(NULL)
  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 0L)
  expect_true(all(c("indicator", "n", "p") %in% names(out)))
})

test_that(".zero_fill_custom_csb_cluster_data zeroes indicator/p in sibling", {
  sib <- tibble::tibble(
    cluster = c(1L, 2L, 3L),
    indicator = c(5L, 7L, 2L),
    n = c(10L, 12L, 8L),
    p = c(0.5, 0.58, 0.25)
  )
  out <- sntmethods:::.zero_fill_custom_csb_cluster_data(sib)
  expect_equal(nrow(out), 3L)
  expect_equal(out$cluster, sib$cluster)
  expect_equal(out$n, sib$n)
  expect_true(all(out$indicator == 0L))
  expect_true(all(out$p == 0))
})

test_that(".zero_fill_custom_csb_adm_estimates produces correct schema", {
  skip_if_not_installed("sf")
  primary_sf <- sf::st_sf(
    adm2 = c("A", "B"),
    adm1 = c("R1", "R1"),
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 0)))),
      sf::st_polygon(list(rbind(c(2, 2), c(3, 2), c(3, 3), c(2, 2))))
    )
  )
  out <- sntmethods:::.zero_fill_custom_csb_adm_estimates(
    "csb_eff_untreat", primary_sf, pop_rast = NULL
  )
  expect_equal(nrow(out), 2L)
  expect_true(all(c("csb_eff_untreat_mean", "csb_eff_untreat_lower",
                    "csb_eff_untreat_upper", "csb_eff_untreat_pop") %in%
                    names(out)))
  expect_true(all(out$csb_eff_untreat_mean == 0))
  expect_true(all(out$csb_eff_untreat_lower == 0))
  expect_true(all(out$csb_eff_untreat_upper == 0))
  expect_true(all(is.na(out$csb_eff_untreat_pop)))
})

test_that(".zero_fill_custom_csb_adm_estimates returns NULL on empty primary_sf", {
  skip_if_not_installed("sf")
  empty_sf <- sf::st_sf(
    adm2 = character(0),
    geometry = sf::st_sfc()
  )
  expect_null(
    sntmethods:::.zero_fill_custom_csb_adm_estimates("csb_eff_dhis", empty_sf)
  )
  expect_null(
    sntmethods:::.zero_fill_custom_csb_adm_estimates("csb_eff_dhis", NULL)
  )
})


# ---- .classify_custom_csb_from_h32() priority rules ----

test_that("classify assigns mutually exclusive partition with dhis priority", {
  classification <- tibble::tibble(
    variable = c("h32a", "h32j", "h32n", "h32x"),
    csb_custom = c("dhis", "nondhis", "nondhis", "untreat")
  )
  df <- data.frame(
    h32a = c(1, 0, 1, 0, 0),  # row 3: dhis + nondhis -> dhis
    h32j = c(0, 1, 1, 0, 0),
    h32n = c(0, 0, 0, 1, 0),
    h32x = c(0, 0, 0, 0, 1)
  )
  out <- sntmethods:::.classify_custom_csb_from_h32(
    data = df,
    h32_cols = c("h32a", "h32j", "h32n", "h32x"),
    classification = classification,
    prefix = "csb_eff"
  )
  expect_equal(out$csb_eff_dhis,    c(1, 0, 1, 0, 0))
  expect_equal(out$csb_eff_nondhis, c(0, 1, 0, 1, 0))
  expect_equal(out$csb_eff_untreat, c(0, 0, 0, 0, 1))
  # Triple sums to exactly 1 per child
  expect_equal(
    out$csb_eff_dhis + out$csb_eff_nondhis + out$csb_eff_untreat,
    rep(1L, nrow(out))
  )
})

test_that("classify treats children with no positive slot as untreat", {
  classification <- tibble::tibble(
    variable = c("h32a", "h32j"),
    csb_custom = c("dhis", "nondhis")
  )
  df <- data.frame(h32a = c(0, 0), h32j = c(0, 0))
  out <- sntmethods:::.classify_custom_csb_from_h32(
    df, c("h32a", "h32j"), classification, "csb_eff"
  )
  expect_equal(out$csb_eff_dhis,    c(0, 0))
  expect_equal(out$csb_eff_nondhis, c(0, 0))
  expect_equal(out$csb_eff_untreat, c(1, 1))
})


# ---- Helpers exposed for the pipeline ----

test_that(".custom_csb_indicator_names returns the three derived codes", {
  spec <- list(name = "csb_eff")
  expect_equal(
    sntmethods:::.custom_csb_indicator_names(spec),
    c("csb_eff_dhis", "csb_eff_nondhis", "csb_eff_untreat")
  )
  expect_equal(sntmethods:::.custom_csb_indicator_names(NULL), character(0))
})

test_that(".valid_mbg_indicators appends derived custom CSB codes", {
  spec <- sntmethods:::.validate_custom_csb_indicator_spec(.mock_spec())
  base <- sntmethods:::.valid_mbg_indicators()
  with_custom <- sntmethods:::.valid_mbg_indicators(custom_csb_indicator = spec)
  expect_true(all(base %in% with_custom))
  expect_true(all(c("csb_eff_dhis", "csb_eff_nondhis", "csb_eff_untreat") %in% with_custom))
})

test_that(".valid_mbg_indicators also accepts the user-supplied meta name", {
  # Users should be able to pass the meta name (e.g. "csb_eff") as a
  # category-level dispatch token to run all three derived sub-indicators
  # in one call. This is what makes the custom partition show up in the
  # final Excel output without enumerating each sub-code.
  spec <- sntmethods:::.validate_custom_csb_indicator_spec(.mock_spec())
  with_custom <- sntmethods:::.valid_mbg_indicators(custom_csb_indicator = spec)
  expect_true("csb_eff" %in% with_custom)
  # Without spec the meta name is not valid
  expect_false("csb_eff" %in% sntmethods:::.valid_mbg_indicators())
})

test_that(".mbg_indicator_meta resolves the custom meta name to KR/u5", {
  spec <- sntmethods:::.validate_custom_csb_indicator_spec(.mock_spec())
  meta <- sntmethods:::.mbg_indicator_meta(
    "csb_eff", custom_csb_indicator = spec
  )
  expect_equal(meta$recode, "KR")
  expect_equal(meta$pop_type, "u5")
  expect_equal(meta$age, "0-59 months")
})

test_that(".mbg_indicator_pop_type returns 'u5' for the custom meta name", {
  spec <- sntmethods:::.validate_custom_csb_indicator_spec(.mock_spec())
  expect_equal(
    sntmethods:::.mbg_indicator_pop_type(
      "csb_eff", custom_csb_indicator = spec
    ),
    "u5"
  )
})

test_that(".mbg_indicator_multiplier accepts and threads custom spec", {
  spec <- sntmethods:::.validate_custom_csb_indicator_spec(.mock_spec())
  # Custom CSB sub-codes use proportion (0-1) base unit -> multiplier 100.
  expect_equal(
    sntmethods:::.mbg_indicator_multiplier(
      "csb_eff_dhis", custom_csb_indicator = spec
    ),
    100
  )
  expect_equal(
    sntmethods:::.mbg_indicator_multiplier(
      "csb_eff", custom_csb_indicator = spec
    ),
    100
  )
  # Backwards-compatible default (no spec) still works for built-ins.
  expect_equal(sntmethods:::.mbg_indicator_multiplier("csb_any"), 100)
  expect_equal(sntmethods:::.mbg_indicator_multiplier("u5mr"), 1000)
})

test_that(".mbg_indicator_pop_type recognises custom CSB codes as 'u5'", {
  spec <- sntmethods:::.validate_custom_csb_indicator_spec(.mock_spec())
  expect_equal(
    sntmethods:::.mbg_indicator_pop_type("csb_eff_dhis", custom_csb_indicator = spec),
    "u5"
  )
  expect_equal(
    sntmethods:::.mbg_indicator_pop_type("csb_eff_untreat", custom_csb_indicator = spec),
    "u5"
  )
  # Without spec the code is unknown -> falls back to "all"
  expect_equal(
    sntmethods:::.mbg_indicator_pop_type("csb_eff_dhis"),
    "all"
  )
})

test_that(".custom_csb_dictionary_rows returns one row per derived code", {
  spec <- sntmethods:::.validate_custom_csb_indicator_spec(.mock_spec())
  out <- sntmethods:::.custom_csb_dictionary_rows(spec)
  expect_equal(nrow(out), 3L)
  expect_setequal(
    out$indicator_code,
    c("csb_eff_dhis", "csb_eff_nondhis", "csb_eff_untreat")
  )
  expect_true(all(nzchar(out$indicator_title)))
  # Denominator now matches the built-in CSB family wording so MBG output
  # is internally consistent.
  expect_true(all(
    out$denominator_description ==
      "Children 0-59 months with fever (h22==1), alive (b5==1)"
  ))
})

test_that(".custom_csb_dictionary_rows returns empty tibble when spec is NULL", {
  out <- sntmethods:::.custom_csb_dictionary_rows(NULL)
  expect_equal(nrow(out), 0L)
  # Schema must match the populated case so dplyr::bind_rows() works
  # without column-mismatch warnings inside .build_final_dataset().
  expect_true(all(c(
    "indicator_code", "indicator", "indicator_title",
    "numerator_code", "numerator_description",
    "denominator_code", "denominator_description",
    "domain", "observation_unit", "dhs_recode", "calc_function",
    "eligibility", "dhs_variables", "notes"
  ) %in% names(out)))
})

test_that(".custom_csb_dictionary_rows embeds user-supplied vars per indicator", {
  # Use the exact spec the user requested in production: full h32* lists
  # for each partition. The dictionary must keep these verbatim so the
  # Excel output is fully traceable back to the user's input.
  spec <- sntmethods:::.validate_custom_csb_indicator_spec(list(
    name = "csb_eff",
    dhis_locs = c("h32a", "h32b", "h32c", "h32d", "h32e", "h32f", "h32i", "h32j"),
    nondhis_locs = c("h32k", "h32l", "h32m", "h32n", "h32r"),
    untreat_locs = c("h32s", "h32t", "h32x")
  ))
  out <- sntmethods:::.custom_csb_dictionary_rows(spec)

  dhis_row    <- out[out$indicator_code == "csb_eff_dhis", ]
  nondhis_row <- out[out$indicator_code == "csb_eff_nondhis", ]
  untreat_row <- out[out$indicator_code == "csb_eff_untreat", ]

  # numerator_description carries the literal user-supplied variable list
  expect_true(grepl(
    "h32a, h32b, h32c, h32d, h32e, h32f, h32i, h32j",
    dhis_row$numerator_description, fixed = TRUE
  ))
  expect_true(grepl(
    "h32k, h32l, h32m, h32n, h32r",
    nondhis_row$numerator_description, fixed = TRUE
  ))
  expect_true(grepl(
    "h32s, h32t, h32x",
    untreat_row$numerator_description, fixed = TRUE
  ))

  # dhs_variables carries the same list for downstream auditing
  expect_true(grepl(
    "h32a, h32b, h32c, h32d, h32e, h32f, h32i, h32j",
    dhis_row$dhs_variables, fixed = TRUE
  ))

  # denominator + numerator_code + domain are populated for every row
  expect_true(all(out$denominator_code == "u5_fever"))
  expect_equal(
    out$numerator_code,
    c("n_csb_eff_dhis", "n_csb_eff_nondhis", "n_csb_eff_untreat")
  )
  expect_true(all(out$domain == "CSB (custom)"))
  expect_true(all(out$dhs_recode == "KR"))
  expect_true(all(out$calc_function == "calc_csb_custom_mbg"))
})

test_that(".custom_csb_dictionary_rows handles empty / missing slots gracefully", {
  # If a user passes an empty vector (e.g. no untreat_locs) the row is
  # still emitted, just with a clear "(none specified)" sentinel so the
  # dictionary remains traceable.
  spec <- list(
    name = "csb_eff2",
    dhis_locs = c("h32a"),
    nondhis_locs = c("h32k"),
    untreat_locs = character(0)
  )
  # Cannot run the validator on this (it requires non-empty), so call
  # the helper directly to verify graceful handling.
  out <- sntmethods:::.custom_csb_dictionary_rows(spec)
  untreat_row <- out[out$indicator_code == "csb_eff2_untreat", ]
  expect_true(grepl("(none specified)", untreat_row$numerator_description, fixed = TRUE))
})


# ---- .normalize_csb_mbg_estimates() generalization ----

test_that(".normalize_csb_mbg_estimates rescales custom partition to 100%", {
  # Two admin units with deliberately misaligned posterior means
  prim <- "adm2"
  build_est <- function(name, mean_vals) {
    tibble::tibble(
      adm2 = c("A", "B"),
      !!paste0(name, "_mean")  := mean_vals,
      !!paste0(name, "_lower") := pmax(mean_vals - 5, 0),
      !!paste0(name, "_upper") := pmin(mean_vals + 5, 100)
    )
  }
  mbg_estimates <- list(
    csb_eff_dhis    = build_est("csb_eff_dhis",    c(40, 50)),
    csb_eff_nondhis = build_est("csb_eff_nondhis", c(30, 30)),
    csb_eff_untreat = build_est("csb_eff_untreat", c(50, 40))
  )
  # Note: rows sum to 120 in admin A and 120 in admin B (deliberately != 100)

  out <- sntmethods:::.normalize_csb_mbg_estimates(
    mbg_estimates = mbg_estimates,
    primary_col = prim,
    sector_names = c("csb_eff_dhis", "csb_eff_nondhis", "csb_eff_untreat"),
    any_name = NULL
  )

  sums <- out$csb_eff_dhis$csb_eff_dhis_mean +
    out$csb_eff_nondhis$csb_eff_nondhis_mean +
    out$csb_eff_untreat$csb_eff_untreat_mean
  expect_equal(sums, c(100, 100), tolerance = 1e-8)

  # All values should still be on [0, 100]
  expect_true(all(out$csb_eff_dhis$csb_eff_dhis_mean >= 0))
  expect_true(all(out$csb_eff_dhis$csb_eff_dhis_mean <= 100))
})

test_that(".normalize_csb_mbg_estimates is a no-op when sectors are missing", {
  prim <- "adm2"
  est <- tibble::tibble(adm2 = c("A"), csb_eff_dhis_mean = c(40))
  mbg_estimates <- list(csb_eff_dhis = est)
  out <- sntmethods:::.normalize_csb_mbg_estimates(
    mbg_estimates,
    primary_col = prim,
    sector_names = c("csb_eff_dhis", "csb_eff_nondhis", "csb_eff_untreat"),
    any_name = NULL
  )
  expect_identical(out, mbg_estimates)
})

test_that(".normalize_csb_mbg_estimates errors on bad sector_names length", {
  expect_error(
    sntmethods:::.normalize_csb_mbg_estimates(
      list(), "adm2", sector_names = c("a", "b")
    ),
    regexp = "length-3"
  )
})


# ---- calc_csb_custom_mbg() end-to-end on mock data ----

test_that("calc_csb_custom_mbg returns three cluster data.tables on mock data", {
  skip_if_not_installed("data.table")

  # Build a slightly larger mock: 4 clusters x 4 children each with the
  # same h32 layout as .mock_kr_with_labels()
  base <- .mock_kr_with_labels()
  kr <- do.call(
    rbind,
    lapply(1:4, function(cl) {
      x <- base
      x$v001 <- cl
      x
    })
  )
  # Re-attach haven labels (rbind drops attributes per column)
  attr(kr$h32a, "label") <- "Government hospital"
  attr(kr$h32j, "label") <- "Private hospital/clinic"
  attr(kr$h32n, "label") <- "Pharmacy"
  attr(kr$h32x, "label") <- "Other"
  attr(kr$h32y, "label") <- "No treatment"
  attr(kr$h32z, "label") <- "Medical treatment (any source)"

  gps <- data.frame(
    DHSCLUST = 1:4,
    LATNUM   = c(0.1, 0.2, 0.3, 0.4),
    LONGNUM  = c(1.1, 1.2, 1.3, 1.4)
  )

  spec <- .mock_spec()
  out <- sntmethods:::calc_csb_custom_mbg(
    dhs_kr = kr,
    gps_data = gps,
    custom_csb_indicator = spec
  )

  expect_setequal(
    names(out),
    c("csb_eff_dhis", "csb_eff_nondhis", "csb_eff_untreat")
  )
  for (nm in names(out)) {
    expect_true(all(c("cluster_id", "indicator", "samplesize", "x", "y") %in%
                      names(out[[nm]])))
    expect_true(nrow(out[[nm]]) > 0)
  }

  # Numerators across the three buckets must sum to denominator per cluster
  per_cluster_sum <- function(name) {
    out[[name]][, c("cluster_id", "indicator")]
  }
  d <- merge(
    merge(per_cluster_sum("csb_eff_dhis"),
          per_cluster_sum("csb_eff_nondhis"),
          by = "cluster_id", suffixes = c("_dhis", "_nondhis")),
    per_cluster_sum("csb_eff_untreat"),
    by = "cluster_id"
  )
  names(d)[ncol(d)] <- "indicator_untreat"
  ss <- out$csb_eff_dhis[, c("cluster_id", "samplesize")]
  d <- merge(d, ss, by = "cluster_id")
  expect_equal(
    d$indicator_dhis + d$indicator_nondhis + d$indicator_untreat,
    d$samplesize
  )
})


# ---- .activate_custom_csb_indicators() -----------------------------------

test_that(".activate_custom_csb_indicators is a no-op when spec is NULL", {
  out <- sntmethods:::.activate_custom_csb_indicators(
    indicators = c("pfpr", "csb"),
    custom_csb_indicator = NULL
  )
  expect_equal(out, c("pfpr", "csb"))
})

test_that(".activate_custom_csb_indicators auto-adds the meta name when absent", {
  spec <- sntmethods:::.validate_custom_csb_indicator_spec(list(
    name = "csb_eff",
    dhis_locs = "h32a",
    nondhis_locs = "h32k",
    untreat_locs = "h32x"
  ))
  expect_message(
    out <- sntmethods:::.activate_custom_csb_indicators(
      indicators = c("pfpr", "csb"),
      custom_csb_indicator = spec
    ),
    regexp = "csb_eff"
  )
  expect_true("csb_eff" %in% out)
  # Existing indicators preserved
  expect_true(all(c("pfpr", "csb") %in% out))
})

test_that(".activate_custom_csb_indicators is a no-op when meta name already listed", {
  spec <- sntmethods:::.validate_custom_csb_indicator_spec(list(
    name = "csb_eff",
    dhis_locs = "h32a",
    nondhis_locs = "h32k",
    untreat_locs = "h32x"
  ))
  expect_silent(
    out <- sntmethods:::.activate_custom_csb_indicators(
      indicators = c("pfpr", "csb_eff"),
      custom_csb_indicator = spec
    )
  )
  expect_equal(out, c("pfpr", "csb_eff"))
})

test_that(".activate_custom_csb_indicators is a no-op when any sub-code already listed", {
  spec <- sntmethods:::.validate_custom_csb_indicator_spec(list(
    name = "csb_eff",
    dhis_locs = "h32a",
    nondhis_locs = "h32k",
    untreat_locs = "h32x"
  ))
  expect_silent(
    out <- sntmethods:::.activate_custom_csb_indicators(
      indicators = c("pfpr", "csb_eff_dhis"),
      custom_csb_indicator = spec
    )
  )
  expect_equal(out, c("pfpr", "csb_eff_dhis"))
})
