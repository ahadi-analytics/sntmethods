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
    "dhs_fever", "Fever prevalence", "Prevalence de la fievre", "h22", "Step 0 of case management cascade",
    "dhs_fever_low", "Fever - lower 95% CI", "Fievre - IC inferieur", "h22", "Survey-weighted 95% CI"
  )

  result <- .enrich_dhs_dictionary(dict, labels)

  # label_en overridden for matched rows

  expect_equal(result$label_en[result$variable == "dhs_fever"], "Fever prevalence")
  expect_equal(result$label_en[result$variable == "dhs_fever_low"], "Fever - lower 95% CI")
  # label_en preserved for unmatched rows
  expect_equal(result$label_en[result$variable == "v024"], "auto_label3")

  # notes overridden where defined
  expect_equal(result$notes[result$variable == "dhs_fever"], "Step 0 of case management cascade")
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

  expect_true(all(c("dhs_variable", "numerator", "denominator",
                     "dhs_numerator_var", "dhs_denominator_var") %in% names(result)))
  expect_equal(result$numerator, "Children with ACT")
  expect_equal(result$denominator, "Febrile U5")
  expect_equal(result$dhs_numerator_var, "ml13e")
  expect_equal(result$dhs_denominator_var, "h22")
})

# ---- .mbg_indicator_meta pop_type ----

test_that(".mbg_indicator_meta returns correct pop_type for all indicators", {
  # U5 indicators
  u5_indicators <- c(
    "fever", "csb_any", "csb_public", "csb_private", "csb_none", "csb_trained",
    "malaria_dx", "antimalarial", "antimalarial_public",
    "act", "act_public", "act_tested",
    "febrile_rdt_pos", "febrile_rdt_pos_act",
    "pfpr_rdt", "pfpr_mic", "pfpr_rdt_u5", "pfpr_mic_u5",
    "pfpr_either_u5", "pfpr_combined_u5",
    "use_itn_chu5",
    "severe_anemia", "anemia_any", "anemia_moderate_plus",
    "epi_bcg", "epi_dpt3", "epi_measles1",
    "u5mr", "smc_coverage",
    "eff_cm_any", "eff_cm_public"
  )
  for (ind in u5_indicators) {
    expect_equal(.mbg_indicator_meta(ind)$pop_type, "u5", info = ind)
  }

  # WRA indicators
  wra_indicators <- c(
    "anc_1plus", "anc_3plus", "anc_4plus", "anc_8plus",
    "iptp_1plus", "iptp_2plus", "iptp_3plus", "iptp_4plus",
    "iptp_1only", "iptp_2only", "iptp_3only",
    "use_itn_preg"
  )
  for (ind in wra_indicators) {
    expect_equal(.mbg_indicator_meta(ind)$pop_type, "wra", info = ind)
  }

  # All-population indicators
  all_indicators <- c(
    "enough_itn", "with_itn", "access_itn", "use_itn",
    "use_itn_if_access", "irs_coverage"
  )
  for (ind in all_indicators) {
    expect_equal(.mbg_indicator_meta(ind)$pop_type, "all", info = ind)
  }

  # Age-stratified ITN
  expect_equal(.mbg_indicator_meta("use_itn_5_10")$pop_type, "5_10")
  expect_equal(.mbg_indicator_meta("use_itn_10_20")$pop_type, "10_20")
  expect_equal(.mbg_indicator_meta("use_itn_20plus")$pop_type, "20plus")
})

test_that(".mbg_indicator_meta defaults pop_type to 'all' for unknown indicators", {
  meta <- .mbg_indicator_meta("nonexistent_indicator_xyz")
  expect_equal(meta$pop_type, "all")
})

# ---- .mbg_indicator_pop_type ----

test_that(".mbg_indicator_pop_type returns correct type for individual indicators", {
  expect_equal(.mbg_indicator_pop_type("pfpr_rdt_u5"), "u5")
  expect_equal(.mbg_indicator_pop_type("fever"), "u5")
  expect_equal(.mbg_indicator_pop_type("act"), "u5")
  expect_equal(.mbg_indicator_pop_type("epi_bcg"), "u5")
  expect_equal(.mbg_indicator_pop_type("smc_coverage"), "u5")
  expect_equal(.mbg_indicator_pop_type("use_itn_chu5"), "u5")

  expect_equal(.mbg_indicator_pop_type("use_itn"), "all")
  expect_equal(.mbg_indicator_pop_type("irs_coverage"), "all")
  expect_equal(.mbg_indicator_pop_type("enough_itn"), "all")

  expect_equal(.mbg_indicator_pop_type("use_itn_5_10"), "5_10")
  expect_equal(.mbg_indicator_pop_type("use_itn_10_20"), "10_20")
  expect_equal(.mbg_indicator_pop_type("use_itn_20plus"), "20plus")

  expect_equal(.mbg_indicator_pop_type("iptp_3plus"), "wra")
  expect_equal(.mbg_indicator_pop_type("anc_4plus"), "wra")
  expect_equal(.mbg_indicator_pop_type("use_itn_preg"), "wra")
})

test_that(".mbg_indicator_pop_type returns correct type for category dispatch keys", {
  expect_equal(.mbg_indicator_pop_type("pfpr"), "u5")
  expect_equal(.mbg_indicator_pop_type("itn"), "all")
  expect_equal(.mbg_indicator_pop_type("irs"), "all")
  expect_equal(.mbg_indicator_pop_type("anc"), "wra")
  expect_equal(.mbg_indicator_pop_type("csb"), "u5")
  expect_equal(.mbg_indicator_pop_type("act"), "u5")
  expect_equal(.mbg_indicator_pop_type("anemia"), "u5")
  expect_equal(.mbg_indicator_pop_type("iptp"), "wra")
  expect_equal(.mbg_indicator_pop_type("epi"), "u5")
  expect_equal(.mbg_indicator_pop_type("u5mr"), "u5")
  expect_equal(.mbg_indicator_pop_type("smc"), "u5")
  expect_equal(.mbg_indicator_pop_type("fever"), "u5")
  expect_equal(.mbg_indicator_pop_type("antimalarial"), "u5")
  expect_equal(.mbg_indicator_pop_type("eff_cm"), "u5")
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

  # Check that notes is last among structural columns
  idx_notes <- which(names(result) == "notes")
  idx_max <- which(names(result) == "max")
  expect_true(idx_notes > idx_max)
})
