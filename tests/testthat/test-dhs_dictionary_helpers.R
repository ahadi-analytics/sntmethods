# ---- .enrich_dhs_dictionary ----

test_that(".enrich_dhs_dictionary overrides label_en and notes", {
  dict <- tibble::tibble(
    variable = c("dhs_fever", "dhs_fever_low", "v024"),
    type = c("numeric", "numeric", "character"),
    label_en = c("auto_label1", "auto_label2", "auto_label3"),
    notes = c("auto_note1", NA_character_, "auto_note3"),
    n = c(100L, 100L, 100L)
  )

  labels <- tibble::tribble(
    ~variable, ~label_en, ~label_fr, ~dhs_variable, ~notes,
    "dhs_fever", "Fever prevalence", "Prevalence de la fievre", "h22", "Step 0 of WMR cascade",
    "dhs_fever_low", "Fever - lower 95% CI", "Fievre - IC inferieur", "h22", "Survey-weighted 95% CI"
  )

  result <- .enrich_dhs_dictionary(dict, labels)

  # label_en overridden for matched rows

  expect_equal(result$label_en[result$variable == "dhs_fever"], "Fever prevalence")
  expect_equal(result$label_en[result$variable == "dhs_fever_low"], "Fever - lower 95% CI")
  # label_en preserved for unmatched rows
  expect_equal(result$label_en[result$variable == "v024"], "auto_label3")

  # label_fr added
  expect_true("label_fr" %in% names(result))
  expect_equal(result$label_fr[result$variable == "dhs_fever"], "Prevalence de la fievre")
  expect_true(is.na(result$label_fr[result$variable == "v024"]))

  # notes overridden where defined
  expect_equal(result$notes[result$variable == "dhs_fever"], "Step 0 of WMR cascade")
  expect_equal(result$notes[result$variable == "dhs_fever_low"], "Survey-weighted 95% CI")
  # notes preserved for unmatched rows
  expect_equal(result$notes[result$variable == "v024"], "auto_note3")

  # dhs_variable added
  expect_true("dhs_variable" %in% names(result))
  expect_equal(result$dhs_variable[result$variable == "dhs_fever"], "h22")
})

test_that(".enrich_dhs_dictionary returns dict unchanged when labels empty", {
  dict <- tibble::tibble(
    variable = c("a", "b"),
    type = c("numeric", "character"),
    label_en = c("A", "B"),
    n = c(10L, 20L)
  )

  result <- .enrich_dhs_dictionary(dict, tibble::tibble())
  expect_equal(names(result), names(dict))
  expect_equal(nrow(result), 2)
})

test_that(".enrich_dhs_dictionary handles missing label columns gracefully", {
  dict <- tibble::tibble(
    variable = c("x", "y"),
    type = c("numeric", "numeric"),
    label_en = c("X", "Y"),
    n = c(5L, 10L)
  )

  # Labels with only label_fr and notes (no label_en override)
  labels <- tibble::tribble(
    ~variable, ~label_fr, ~notes,
    "x", "FR-X", "Note for X"
  )

  result <- .enrich_dhs_dictionary(dict, labels)

  # label_en unchanged (no override attempted)
  expect_equal(result$label_en, c("X", "Y"))
  # label_fr added
  expect_equal(result$label_fr[result$variable == "x"], "FR-X")
  expect_true(is.na(result$label_fr[result$variable == "y"]))
  # notes overridden
  expect_equal(result$notes[result$variable == "x"], "Note for X")
})

test_that(".enrich_dhs_dictionary adds all new DHS columns", {
  dict <- tibble::tibble(
    variable = "dhs_act",
    type = "numeric",
    label_en = "auto",
    n = 50L
  )

  labels <- tibble::tribble(
    ~variable, ~label_en, ~label_fr, ~dhs_variable, ~numerator, ~denominator, ~dhs_numerator_var, ~dhs_denominator_var, ~notes,
    "dhs_act", "ACT coverage", "Couverture CTA", "ml13e", "Children with ACT", "Febrile U5", "ml13e", "h22", "Step 4"
  )

  result <- .enrich_dhs_dictionary(dict, labels)

  expect_true(all(c("label_fr", "dhs_variable", "numerator", "denominator",
                     "dhs_numerator_var", "dhs_denominator_var") %in% names(result)))
  expect_equal(result$numerator, "Children with ACT")
  expect_equal(result$denominator, "Febrile U5")
  expect_equal(result$dhs_numerator_var, "ml13e")
  expect_equal(result$dhs_denominator_var, "h22")
})

test_that(".enrich_dhs_dictionary reorders columns correctly", {
  dict <- tibble::tibble(
    variable = "x",
    type = "numeric",
    label_en = "X",
    n = 10L,
    n_missing = 0L,
    pct_missing = 0,
    n_unique = 5L,
    example_values = "1,2,3",
    min = 1,
    max = 10,
    notes = "orig"
  )

  labels <- tibble::tribble(
    ~variable, ~label_en, ~label_fr, ~dhs_variable, ~notes,
    "x", "New X", "FR X", "hv001", "new note"
  )

  result <- .enrich_dhs_dictionary(dict, labels)

  # Check that label_fr comes after label_en
  idx_en <- which(names(result) == "label_en")
  idx_fr <- which(names(result) == "label_fr")
  expect_true(idx_fr == idx_en + 1)

  # Check that notes is last among structural columns
  idx_notes <- which(names(result) == "notes")
  idx_max <- which(names(result) == "max")
  expect_true(idx_notes > idx_max)
})


# ---- .build_mbg_labels ----

test_that(".build_mbg_labels generates correct rows for known indicators", {
  labels <- .build_mbg_labels("fever")

  expect_s3_class(labels, "tbl_df")
  expect_true(nrow(labels) == 7)  # mean, lower, upper, n_tested, n_pos, n_clusters, raw

  # Check column names
  expect_true(all(c("variable", "label_en", "label_fr", "dhs_variable",
                     "numerator", "denominator", "dhs_numerator_var",
                     "dhs_denominator_var", "notes") %in% names(labels)))

  # Check variable names
  expected_vars <- c(
    "fever_mean", "fever_lower", "fever_upper",
    "n_tested_fever", "n_pos_fever", "n_clusters_fever", "fever_raw"
  )
  expect_equal(labels$variable, expected_vars)

  # Check DHS variable
  expect_true(all(labels$dhs_variable[1:5] == "h22"))
  expect_true(is.na(labels$dhs_variable[6]))  # n_clusters has NA dhs_variable

  # Check numerator/denominator populated only for _mean row
  mean_row <- labels[labels$variable == "fever_mean", ]
  expect_equal(mean_row$numerator, "Children with fever")
  expect_equal(mean_row$denominator, "Alive children 0-59 months")
  expect_equal(mean_row$dhs_numerator_var, "h22")
  expect_equal(mean_row$dhs_denominator_var, "b5, hw1")

  # CI and count rows should have NA numerator/denominator
  lower_row <- labels[labels$variable == "fever_lower", ]
  expect_true(is.na(lower_row$numerator))
  expect_true(is.na(lower_row$denominator))
  expect_true(is.na(lower_row$dhs_numerator_var))
  expect_true(is.na(lower_row$dhs_denominator_var))
})

test_that(".build_mbg_labels handles multiple indicators", {
  labels <- .build_mbg_labels(c("fever", "malaria_dx"))

  expect_equal(nrow(labels), 14)  # 7 per indicator
  expect_true("fever_mean" %in% labels$variable)
  expect_true("malaria_dx_mean" %in% labels$variable)
})

test_that(".build_mbg_labels generates labels for sub-indicators", {
  # Test representative sub-indicators from multi-indicator categories
  sub_inds <- c("act_tested", "csb_any", "anc_4plus", "iptp_3plus",
                "epi_dpt3", "anemia_severe", "itn_use_all", "pfpr_rdt_u5")
  labels <- .build_mbg_labels(sub_inds)

  expect_equal(nrow(labels), 7 * length(sub_inds))

  # All should have proper labels (not just the indicator name)
  expect_equal(
    labels$label_en[labels$variable == "act_tested_mean"],
    "Received ACT (test-positive) - Mean"
  )
  expect_equal(
    labels$label_en[labels$variable == "csb_any_mean"],
    "Care-seeking at any source - Mean"
  )
  expect_equal(
    labels$label_en[labels$variable == "anc_4plus_mean"],
    "At least 4 ANC visits - Mean"
  )
  expect_equal(
    labels$label_en[labels$variable == "epi_dpt3_mean"],
    "DPT dose 3 - Mean"
  )
  expect_equal(
    labels$label_en[labels$variable == "anemia_severe_mean"],
    "Severe anemia prevalence - Mean"
  )
  expect_equal(
    labels$label_fr[labels$variable == "itn_use_all_mean"],
    "Utilisation MII - toute la population - Moyenne"
  )
  expect_equal(
    labels$dhs_variable[labels$variable == "pfpr_rdt_u5_mean"],
    "hml35"
  )
})

test_that(".build_mbg_labels handles unknown indicators gracefully", {
  labels <- .build_mbg_labels("custom_indicator")

  expect_equal(nrow(labels), 7)
  expect_true("custom_indicator_mean" %in% labels$variable)
  # Unknown indicator uses name as label
  expect_true(grepl("custom_indicator", labels$label_en[1]))
})


# ---- .mbg_id_labels ----

test_that(".mbg_id_labels returns expected structure", {
  labels <- .mbg_id_labels()

  expect_s3_class(labels, "tbl_df")
  expect_true(all(c("variable", "label_en", "label_fr", "notes") %in%
                    names(labels)))

  # Check key variables
  expect_true("iso3_code" %in% labels$variable)
  expect_true("adm1" %in% labels$variable)
  expect_true("survey_year" %in% labels$variable)
})
