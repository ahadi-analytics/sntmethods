#' Enrich a sntutils data dictionary with DHS/MBG domain labels
#'
#' Left-joins DHS-specific labels onto a base dictionary produced by
#' \code{sntutils::build_dictionary()}, overriding generic auto-labels
#' with precise DHS indicator names, French translations, and
#' methodological notes.
#'
#' @param dict A data frame produced by \code{sntutils::build_dictionary()}.
#' @param labels A tibble with at least a \code{variable} column and one or more
#'   of: \code{label_en}, \code{label_fr}, \code{dhs_variable}, \code{numerator},
#'   \code{denominator}, \code{dhs_numerator_var}, \code{dhs_denominator_var},
#'   \code{notes}.
#'
#' @return An enriched data frame with the same rows as \code{dict} but with
#'   additional/overridden columns from \code{labels}.
#'
#' @noRd
.enrich_dhs_dictionary <- function(dict, labels) {
  if (!is.data.frame(dict) || !is.data.frame(labels)) {
    return(dict)
  }
  if (!"variable" %in% names(labels) || nrow(labels) == 0) {
    return(dict)
  }

  # Left-join labels onto dict by variable
  labels_joined <- dplyr::left_join(
    dict[, "variable", drop = FALSE],
    labels,
    by = "variable"
  )

  enriched <- dict

  # Override label_en where our DHS labels are defined (not NA)
  if ("label_en" %in% names(labels)) {
    enriched$label_en <- dplyr::coalesce(
      labels_joined$label_en,
      enriched$label_en
    )
  }

  # Add or override label_fr
  if ("label_fr" %in% names(labels)) {
    enriched$label_fr <- labels_joined$label_fr
  }

  # Override notes where our DHS notes are defined (not NA)
  if ("notes" %in% names(labels)) {
    existing_notes <- if ("notes" %in% names(enriched)) {
      enriched$notes
    } else {
      rep(NA_character_, nrow(enriched))
    }
    enriched$notes <- dplyr::coalesce(
      labels_joined$notes,
      existing_notes
    )
  }

  # Add new DHS-specific columns
  new_cols <- c(
    "dhs_variable", "numerator", "denominator",
    "dhs_numerator_var", "dhs_denominator_var",
    "dhs_recode", "indicator_category", "wmr_cascade_step",
    "age_group", "units"
  )
  for (col in new_cols) {
    if (col %in% names(labels)) {
      enriched[[col]] <- labels_joined[[col]]
    }
  }

  # Derive category column from indicator_category + variable name
  if ("indicator_category" %in% names(enriched)) {
    enriched$category <- dplyr::case_when(
      enriched$indicator_category == "ITN" ~ "ITNs",
      enriched$indicator_category == "Malaria" &
        grepl("pfpr", enriched$variable, ignore.case = TRUE) ~ "Parasite rate",
      enriched$indicator_category == "Malaria" ~ "Case management",
      is.na(enriched$indicator_category) &
        grepl(
          "^adm[0-9]|^iso3_code$|^dhs_code$|^survey_year$|^survey_type$",
          enriched$variable
        ) ~ "Key info",
      !is.na(enriched$indicator_category) ~ enriched$indicator_category,
      TRUE ~ NA_character_
    )
  }

  # Validate labels against data
  .validate_dictionary_labels(enriched, labels)

  # Reorder columns: structural first, then DHS-specific, then stats
  desired_order <- c(
    "variable", "type", "label_en", "label_fr",
    "category",
    "dhs_variable", "numerator", "denominator",
    "dhs_numerator_var", "dhs_denominator_var",
    "dhs_recode", "indicator_category", "wmr_cascade_step",
    "age_group", "units",
    "n", "n_missing", "pct_missing", "n_unique",
    "example_values", "min", "max", "notes"
  )
  existing <- intersect(desired_order, names(enriched))
  remaining <- setdiff(names(enriched), desired_order)
  enriched <- enriched[, c(existing, remaining), drop = FALSE]

  tibble::as_tibble(enriched)
}


#' Build MBG-specific labels for indicator columns
#'
#' Programmatically generates label tibbles for MBG output columns
#' based on indicator names. Each indicator produces _mean, _lower,
#' _upper, n_tested_, n_pos_, n_clusters_, and _raw columns.
#'
#' @param indicator_names Character vector of indicator names
#'   (e.g., "fever", "malaria_dx").
#'
#' @return A tibble with columns: variable, label_en, label_fr,
#'   dhs_variable, numerator, denominator, dhs_numerator_var,
#'   dhs_denominator_var, notes.
#'
#' @noRd
.build_mbg_labels <- function(indicator_names) {
  # Map indicator names to readable labels with numerator/denominator metadata
  # Fields: en, fr, dhs_var, num, den, dhs_num, dhs_den
  indicator_labels <- list(
    # WMR Case Management Cascade
    fever = list(
      en = "Fever prevalence", fr = "Prevalence de la fievre",
      dhs_var = "h22",
      num = "Children with fever", den = "Alive children 0-59 months",
      dhs_num = "h22", dhs_den = "b5, hw1"
    ),
    csb_public = list(
      en = "Care-seeking at public facility",
      fr = "Recherche de soins en etablissement public",
      dhs_var = "h32a-c",
      num = "Febrile children taken to public facility",
      den = "Febrile children under 5",
      dhs_num = "h32a-c", dhs_den = "h22"
    ),
    csb_private = list(
      en = "Care-seeking at private facility",
      fr = "Recherche de soins en etablissement prive",
      dhs_var = "h32d-u",
      num = "Febrile children taken to private facility",
      den = "Febrile children under 5",
      dhs_num = "h32d-u", dhs_den = "h22"
    ),
    csb_none = list(
      en = "No care-seeking for fever",
      fr = "Pas de recherche de soins pour fievre",
      dhs_var = "1-any(h32*)",
      num = "Febrile children not taken anywhere",
      den = "Febrile children under 5",
      dhs_num = "1-any(h32*)", dhs_den = "h22"
    ),
    csb_any = list(
      en = "Care-seeking at any source",
      fr = "Recherche de soins toute source",
      dhs_var = "h32a-z",
      num = "Febrile children taken to any source",
      den = "Febrile children under 5",
      dhs_num = "h32a-z", dhs_den = "h22"
    ),
    csb_trained = list(
      en = "Care-seeking from trained provider",
      fr = "Recherche de soins aupres d'un prestataire forme",
      dhs_var = "h32a-u",
      num = "Febrile children taken to trained provider",
      den = "Febrile children under 5",
      dhs_num = "h32a-u", dhs_den = "h22"
    ),
    malaria_dx = list(
      en = "Blood taken for malaria testing",
      fr = "Sang preleve pour test de paludisme",
      dhs_var = "h47/ml1",
      num = "Febrile children with blood taken for testing",
      den = "Febrile children under 5",
      dhs_num = "h47/ml1", dhs_den = "h22"
    ),
    antimalarial = list(
      en = "Received any antimalarial",
      fr = "A recu un antipaludique",
      dhs_var = "ml13a-h (or h37a-h)",
      num = "Febrile children receiving any antimalarial",
      den = "Febrile children under 5",
      dhs_num = "ml13a-h (or h37a-h)", dhs_den = "h22"
    ),
    act = list(
      en = "Received ACT", fr = "A recu un CTA",
      dhs_var = "ml13e (or h37e)",
      num = "Febrile children receiving ACT",
      den = "Febrile children under 5",
      dhs_num = "ml13e (or h37e)", dhs_den = "h22"
    ),
    act_tested = list(
      en = "Received ACT (test-positive)",
      fr = "A recu un CTA (testes positifs)",
      dhs_var = "ml13e (or h37e)",
      num = "Test-positive children receiving ACT",
      den = "Test-positive febrile children",
      dhs_num = "ml13e (or h37e)", dhs_den = "h47/ml1"
    ),
    febrile_rdt_pos = list(
      en = "RDT positivity rate (febrile U5)",
      fr = "Taux de positivite TDR (enfants febriles < 5 ans)",
      dhs_var = "hml35",
      num = "Febrile children under 5 with positive RDT",
      den = "Febrile children under 5 with valid RDT result",
      dhs_num = "hml35", dhs_den = "hml35"
    ),
    febrile_rdt_pos_act = list(
      en = "ACT coverage among febrile RDT+ children",
      fr = "Couverture CTA chez les enfants febriles TDR+",
      dhs_var = "ml13e (or h37e)",
      num = "Febrile RDT-positive children receiving ACT",
      den = "Febrile children under 5 with positive RDT",
      dhs_num = "ml13e (or h37e)", dhs_den = "hml35"
    ),
    # PfPR
    pfpr_rdt = list(
      en = "PfPR by RDT", fr = "TIP par TDR",
      dhs_var = "hml35",
      num = "RDT-positive children", den = "Children tested by RDT",
      dhs_num = "hml35", dhs_den = "hml35"
    ),
    pfpr_mic = list(
      en = "PfPR by microscopy", fr = "TIP par microscopie",
      dhs_var = "hml32",
      num = "Microscopy-positive children",
      den = "Children tested by microscopy",
      dhs_num = "hml32", dhs_den = "hml32"
    ),
    pfpr_rdt_u5 = list(
      en = "PfPR by RDT (under 5)",
      fr = "TIP par TDR (moins de 5 ans)",
      dhs_var = "hml35",
      num = "RDT-positive children under 5",
      den = "Children under 5 tested by RDT",
      dhs_num = "hml35", dhs_den = "hml35"
    ),
    pfpr_mic_u5 = list(
      en = "PfPR by microscopy (under 5)",
      fr = "TIP par microscopie (moins de 5 ans)",
      dhs_var = "hml32",
      num = "Microscopy-positive children under 5",
      den = "Children under 5 tested by microscopy",
      dhs_num = "hml32", dhs_den = "hml32"
    ),
    pfpr_either_u5 = list(
      en = "PfPR by either test (under 5)",
      fr = "TIP par tout test (moins de 5 ans)",
      dhs_var = "hml32/hml35",
      num = "Children under 5 positive by either test",
      den = "Children under 5 tested",
      dhs_num = "hml32/hml35", dhs_den = "hml32/hml35"
    ),
    pfpr_combined_u5 = list(
      en = "PfPR combined (under 5)",
      fr = "TIP combine (moins de 5 ans)",
      dhs_var = "hml32/hml35",
      num = "Children under 5 positive (combined)",
      den = "Children under 5 tested",
      dhs_num = "hml32/hml35", dhs_den = "hml32/hml35"
    ),
    # ITN
    itn_ownership = list(
      en = "ITN household ownership",
      fr = "Possession de MII par menage",
      dhs_var = "hml10_*",
      num = "Households with at least 1 ITN",
      den = "All households",
      dhs_num = "hml10_*", dhs_den = "hv001"
    ),
    itn_access = list(
      en = "ITN population access",
      fr = "Acces de la population aux MII",
      dhs_var = "hml18",
      num = "Population with access to ITN",
      den = "De facto household population",
      dhs_num = "hml18", dhs_den = "hv013"
    ),
    itn_use = list(
      en = "ITN population use",
      fr = "Utilisation des MII par la population",
      dhs_var = "hml12",
      num = "Population sleeping under ITN",
      den = "De facto household population",
      dhs_num = "hml12", dhs_den = "hv013"
    ),
    itn_use_u5 = list(
      en = "ITN use in children under 5",
      fr = "Utilisation des MII chez les enfants de moins de 5 ans",
      dhs_var = "hml12",
      num = "Children under 5 sleeping under ITN",
      den = "Children under 5 in household",
      dhs_num = "hml12", dhs_den = "hv013"
    ),
    itn_use_preg = list(
      en = "ITN use in pregnant women",
      fr = "Utilisation des MII chez les femmes enceintes",
      dhs_var = "hml12",
      num = "Pregnant women sleeping under ITN",
      den = "Pregnant women in household",
      dhs_num = "hml12", dhs_den = "hv013"
    ),
    itn_use_all = list(
      en = "ITN use - all population",
      fr = "Utilisation MII - toute la population",
      dhs_var = "hml12",
      num = "Population sleeping under ITN",
      den = "De facto household population",
      dhs_num = "hml12", dhs_den = "hv013"
    ),
    itn_use_5_10 = list(
      en = "ITN use in children 5-10",
      fr = "Utilisation MII enfants 5-10 ans",
      dhs_var = "hml12",
      num = "Children 5-10 sleeping under ITN",
      den = "Children 5-10 in household",
      dhs_num = "hml12", dhs_den = "hv013"
    ),
    itn_use_10_20 = list(
      en = "ITN use in adolescents 10-20",
      fr = "Utilisation MII adolescents 10-20 ans",
      dhs_var = "hml12",
      num = "Adolescents 10-20 sleeping under ITN",
      den = "Adolescents 10-20 in household",
      dhs_num = "hml12", dhs_den = "hv013"
    ),
    itn_use_20plus = list(
      en = "ITN use in adults 20+",
      fr = "Utilisation MII adultes 20+",
      dhs_var = "hml12",
      num = "Adults 20+ sleeping under ITN",
      den = "Adults 20+ in household",
      dhs_num = "hml12", dhs_den = "hv013"
    ),
    itn_use_pregnant = list(
      en = "ITN use in pregnant women",
      fr = "Utilisation MII femmes enceintes",
      dhs_var = "hml12",
      num = "Pregnant women sleeping under ITN",
      den = "Pregnant women in household",
      dhs_num = "hml12", dhs_den = "hv013"
    ),
    itn_use_if_access = list(
      en = "ITN use given access",
      fr = "Utilisation MII si acces",
      dhs_var = "hml12",
      num = "People with access using ITN",
      den = "People with access to ITN",
      dhs_num = "hml12", dhs_den = "hml18"
    ),
    # Anemia
    severe_anemia = list(
      en = "Severe anemia prevalence",
      fr = "Prevalence de l'anemie severe",
      dhs_var = "hw53/hc56",
      num = "Children with Hb < 8 g/dL",
      den = "Children with Hb measurement",
      dhs_num = "hw53/hc56", dhs_den = "hw53/hc56"
    ),
    anemia_any = list(
      en = "Any anemia prevalence",
      fr = "Prevalence de toute anemie",
      dhs_var = "hw53/hc56",
      num = "Children with Hb < 11 g/dL",
      den = "Children with Hb measurement",
      dhs_num = "hw53/hc56", dhs_den = "hw53/hc56"
    ),
    anemia_moderate_plus = list(
      en = "Moderate or severe anemia",
      fr = "Anemie moderee ou severe",
      dhs_var = "hw53/hc56",
      num = "Children with Hb < 10 g/dL",
      den = "Children with Hb measurement",
      dhs_num = "hw53/hc56", dhs_den = "hw53/hc56"
    ),
    anemia_severe = list(
      en = "Severe anemia prevalence",
      fr = "Prevalence de l'anemie severe",
      dhs_var = "hw53/hc56",
      num = "Children with Hb < 8 g/dL",
      den = "Children with Hb measurement",
      dhs_num = "hw53/hc56", dhs_den = "hw53/hc56"
    ),
    anemia_mild_only = list(
      en = "Mild anemia only",
      fr = "Anemie legere uniquement",
      dhs_var = "hw53/hc56",
      num = "Children with 10 <= Hb < 11 g/dL",
      den = "Children with Hb measurement",
      dhs_num = "hw53/hc56", dhs_den = "hw53/hc56"
    ),
    anemia_moderate_only = list(
      en = "Moderate anemia only",
      fr = "Anemie moderee uniquement",
      dhs_var = "hw53/hc56",
      num = "Children with 8 <= Hb < 10 g/dL",
      den = "Children with Hb measurement",
      dhs_num = "hw53/hc56", dhs_den = "hw53/hc56"
    ),
    anemia_severe_only = list(
      en = "Severe anemia only",
      fr = "Anemie severe uniquement",
      dhs_var = "hw53/hc56",
      num = "Children with Hb < 8 g/dL",
      den = "Children with Hb measurement",
      dhs_num = "hw53/hc56", dhs_den = "hw53/hc56"
    ),
    # ANC
    anc_1plus = list(
      en = "At least 1 ANC visit", fr = "Au moins 1 visite CPN",
      dhs_var = "m14_1",
      num = "Women with >= 1 ANC visit",
      den = "Women with recent birth",
      dhs_num = "m14_1", dhs_den = "v201"
    ),
    anc_3plus = list(
      en = "At least 3 ANC visits", fr = "Au moins 3 visites CPN",
      dhs_var = "m14_1",
      num = "Women with >= 3 ANC visits",
      den = "Women with recent birth",
      dhs_num = "m14_1", dhs_den = "v201"
    ),
    anc_4plus = list(
      en = "At least 4 ANC visits", fr = "Au moins 4 visites CPN",
      dhs_var = "m14_1",
      num = "Women with >= 4 ANC visits",
      den = "Women with recent birth",
      dhs_num = "m14_1", dhs_den = "v201"
    ),
    anc_8plus = list(
      en = "At least 8 ANC visits", fr = "Au moins 8 visites CPN",
      dhs_var = "m14_1",
      num = "Women with >= 8 ANC visits",
      den = "Women with recent birth",
      dhs_num = "m14_1", dhs_den = "v201"
    ),
    # IPTp
    iptp_1plus = list(
      en = "At least 1 dose IPTp-SP", fr = "Au moins 1 dose TPI-SP",
      dhs_var = "m49a_1/ml1_1",
      num = "Women receiving >= 1 SP dose",
      den = "Women with recent birth",
      dhs_num = "m49a_1/ml1_1", dhs_den = "v201"
    ),
    iptp_2plus = list(
      en = "At least 2 doses IPTp-SP", fr = "Au moins 2 doses TPI-SP",
      dhs_var = "m49a_1/ml1_1",
      num = "Women receiving >= 2 SP doses",
      den = "Women with recent birth",
      dhs_num = "m49a_1/ml1_1", dhs_den = "v201"
    ),
    iptp_3plus = list(
      en = "At least 3 doses IPTp-SP", fr = "Au moins 3 doses TPI-SP",
      dhs_var = "m49a_1/ml1_1",
      num = "Women receiving >= 3 SP doses",
      den = "Women with recent birth",
      dhs_num = "m49a_1/ml1_1", dhs_den = "v201"
    ),
    iptp_4plus = list(
      en = "At least 4 doses IPTp-SP", fr = "Au moins 4 doses TPI-SP",
      dhs_var = "ml1_1",
      num = "Women receiving >= 4 SP doses",
      den = "Women with recent birth",
      dhs_num = "ml1_1", dhs_den = "v201"
    ),
    iptp_1only = list(
      en = "Exactly 1 dose IPTp-SP", fr = "Exactement 1 dose TPI-SP",
      dhs_var = "m49a_1/ml1_1",
      num = "Women receiving exactly 1 SP dose",
      den = "Women with recent birth",
      dhs_num = "m49a_1/ml1_1", dhs_den = "v201"
    ),
    iptp_2only = list(
      en = "Exactly 2 doses IPTp-SP", fr = "Exactement 2 doses TPI-SP",
      dhs_var = "m49a_1/ml1_1",
      num = "Women receiving exactly 2 SP doses",
      den = "Women with recent birth",
      dhs_num = "m49a_1/ml1_1", dhs_den = "v201"
    ),
    iptp_3only = list(
      en = "Exactly 3 doses IPTp-SP", fr = "Exactement 3 doses TPI-SP",
      dhs_var = "m49a_1/ml1_1",
      num = "Women receiving exactly 3 SP doses",
      den = "Women with recent birth",
      dhs_num = "m49a_1/ml1_1", dhs_den = "v201"
    ),
    # EPI
    epi_bcg = list(
      en = "BCG vaccination", fr = "Vaccination BCG",
      dhs_var = "h2",
      num = "Children 12-23m with BCG", den = "Children 12-23 months",
      dhs_num = "h2", dhs_den = "hw1"
    ),
    epi_dpt1 = list(
      en = "DPT dose 1", fr = "DTC dose 1",
      dhs_var = "h3",
      num = "Children 12-23m with DPT1", den = "Children 12-23 months",
      dhs_num = "h3", dhs_den = "hw1"
    ),
    epi_dpt2 = list(
      en = "DPT dose 2", fr = "DTC dose 2",
      dhs_var = "h5",
      num = "Children 12-23m with DPT2", den = "Children 12-23 months",
      dhs_num = "h5", dhs_den = "hw1"
    ),
    epi_dpt3 = list(
      en = "DPT dose 3", fr = "DTC dose 3",
      dhs_var = "h7",
      num = "Children 12-23m with DPT3", den = "Children 12-23 months",
      dhs_num = "h7", dhs_den = "hw1"
    ),
    epi_polio1 = list(
      en = "Polio dose 1", fr = "Polio dose 1",
      dhs_var = "h4",
      num = "Children 12-23m with Polio1", den = "Children 12-23 months",
      dhs_num = "h4", dhs_den = "hw1"
    ),
    epi_polio2 = list(
      en = "Polio dose 2", fr = "Polio dose 2",
      dhs_var = "h6",
      num = "Children 12-23m with Polio2", den = "Children 12-23 months",
      dhs_num = "h6", dhs_den = "hw1"
    ),
    epi_polio3 = list(
      en = "Polio dose 3", fr = "Polio dose 3",
      dhs_var = "h8",
      num = "Children 12-23m with Polio3", den = "Children 12-23 months",
      dhs_num = "h8", dhs_den = "hw1"
    ),
    epi_measles1 = list(
      en = "Measles dose 1", fr = "Rougeole dose 1",
      dhs_var = "h9",
      num = "Children 12-23m with Measles1", den = "Children 12-23 months",
      dhs_num = "h9", dhs_den = "hw1"
    ),
    epi_measles2 = list(
      en = "Measles dose 2", fr = "Rougeole dose 2",
      dhs_var = "h9a",
      num = "Children 12-23m with Measles2", den = "Children 12-23 months",
      dhs_num = "h9a", dhs_den = "hw1"
    ),
    epi_vita1 = list(
      en = "Vitamin A dose 1", fr = "Vitamine A dose 1",
      dhs_var = "h33",
      num = "Children 12-23m with Vitamin A dose 1",
      den = "Children 12-23 months",
      dhs_num = "h33", dhs_den = "hw1"
    ),
    epi_vita2 = list(
      en = "Vitamin A dose 2", fr = "Vitamine A dose 2",
      dhs_var = "h33a",
      num = "Children 12-23m with Vitamin A dose 2",
      den = "Children 12-23 months",
      dhs_num = "h33a", dhs_den = "hw1"
    ),
    epi_malaria = list(
      en = "Malaria vaccine (RTS,S/R21)",
      fr = "Vaccin antipaludique (RTS,S/R21)",
      dhs_var = "h62-h65",
      num = "Children 12-23m with malaria vaccine",
      den = "Children 12-23 months",
      dhs_num = "h62-h65", dhs_den = "hw1"
    ),
    epi_fully_vaccinated = list(
      en = "Fully vaccinated (basic)",
      fr = "Completement vaccine (base)",
      dhs_var = "h2-h9",
      num = "Children 12-23m fully vaccinated",
      den = "Children 12-23 months",
      dhs_num = "h2-h9", dhs_den = "hw1"
    ),
    # U5MR
    u5mr = list(
      en = "Under-5 mortality rate",
      fr = "Taux de mortalite des moins de 5 ans",
      dhs_var = "b5, b7, b3",
      num = "Deaths before age 5", den = "Live births",
      dhs_num = "b5, b7", dhs_den = "b3"
    ),
    # IRS / SMC
    irs_coverage = list(
      en = "IRS coverage", fr = "Couverture PID",
      dhs_var = "hv253",
      num = "Households sprayed in last 12 months",
      den = "All households",
      dhs_num = "hv253", dhs_den = "hv001"
    ),
    smc_coverage = list(
      en = "SMC coverage", fr = "Couverture CPS",
      dhs_var = "hml43/ml13g",
      num = "Children receiving SMC", den = "Children under 5",
      dhs_num = "hml43/ml13g", dhs_den = "hw1"
    )
  )

  na_chr <- NA_character_

  rows <- lapply(indicator_names, function(ind) {
    info <- indicator_labels[[ind]]
    if (is.null(info)) {
      # Fallback for unknown indicators
      info <- list(
        en = ind, fr = ind, dhs_var = na_chr,
        num = na_chr, den = na_chr, dhs_num = na_chr, dhs_den = na_chr
      )
    }

    # Use NA for indicators that lack numerator/denominator metadata
    num <- info$num %||% na_chr
    den <- info$den %||% na_chr
    dhs_num <- info$dhs_num %||% na_chr
    dhs_den <- info$dhs_den %||% na_chr

    # Lookup indicator metadata (recode, category, cascade, age, units)
    meta <- .mbg_indicator_meta(ind)
    na_int <- NA_integer_

    tibble::tribble(
      ~variable, ~label_en, ~label_fr, ~dhs_variable,
      ~numerator, ~denominator, ~dhs_numerator_var, ~dhs_denominator_var,
      ~dhs_recode, ~indicator_category, ~wmr_cascade_step, ~age_group, ~units,
      ~notes,

      paste0(ind, "_mean"),
      paste0(info$en, " - Mean"),
      paste0(info$fr, " - Moyenne"),
      info$dhs_var,
      num, den, dhs_num, dhs_den,
      meta$recode, meta$category, meta$cascade, meta$age, meta$base_unit,
      "MBG model-based estimate; population-weighted using WorldPop rasters; x100 for percentage",

      paste0(ind, "_lower"),
      paste0(info$en, " - Lower 95% CI"),
      paste0(info$fr, " - IC 95% inferieur"),
      info$dhs_var,
      na_chr, na_chr, na_chr, na_chr,
      meta$recode, meta$category, meta$cascade, meta$age, meta$base_unit,
      "Lower bound of 95% credible interval from MBG posterior distribution",

      paste0(ind, "_upper"),
      paste0(info$en, " - Upper 95% CI"),
      paste0(info$fr, " - IC 95% superieur"),
      info$dhs_var,
      na_chr, na_chr, na_chr, na_chr,
      meta$recode, meta$category, meta$cascade, meta$age, meta$base_unit,
      "Upper bound of 95% credible interval from MBG posterior distribution",

      paste0("n_tested_", ind),
      paste0("N tested - ", info$en),
      paste0("N testes - ", info$fr),
      info$dhs_var,
      na_chr, na_chr, na_chr, na_chr,
      meta$recode, meta$category, meta$cascade, meta$age, "count",
      "Unweighted cluster-level denominator aggregated to admin area",

      paste0("n_pos_", ind),
      paste0("N positive - ", info$en),
      paste0("N positifs - ", info$fr),
      info$dhs_var,
      na_chr, na_chr, na_chr, na_chr,
      meta$recode, meta$category, meta$cascade, meta$age, "count",
      "Unweighted cluster-level numerator aggregated to admin area",

      paste0("n_clusters_", ind),
      paste0("N clusters - ", info$en),
      paste0("N grappes - ", info$fr),
      na_chr,
      na_chr, na_chr, na_chr, na_chr,
      meta$recode, meta$category, meta$cascade, meta$age, "count",
      "Number of DHS survey clusters with data in admin area",

      paste0(ind, "_raw"),
      paste0(info$en, " - Raw"),
      paste0(info$fr, " - Brut"),
      info$dhs_var,
      na_chr, na_chr, na_chr, na_chr,
      meta$recode, meta$category, meta$cascade, meta$age, meta$base_unit,
      "Unweighted cluster proportion (n_pos / n_tested)"
    )
  })

  dplyr::bind_rows(rows)
}


#' Standard identifier column labels for MBG output
#'
#' @return A tibble with columns matching the full label schema.
#' @noRd
.mbg_id_labels <- function() {
  na_chr <- NA_character_
  na_int <- NA_integer_
  tibble::tribble(
    ~variable,     ~label_en,                                      ~label_fr,                                            ~dhs_variable, ~numerator, ~denominator, ~dhs_numerator_var, ~dhs_denominator_var, ~dhs_recode, ~indicator_category, ~wmr_cascade_step, ~age_group, ~units,    ~notes,
    "iso3_code",   "ISO 3166-1 alpha-3 country code",              "Code pays ISO 3166-1 alpha-3",                       na_chr,        na_chr,     na_chr,       na_chr,             na_chr,               na_chr,      "Key info",          na_int,            na_chr,     na_chr,    na_chr,
    "dhs_code",    "DHS country code",                             "Code pays DHS",                                      na_chr,        na_chr,     na_chr,       na_chr,             na_chr,               na_chr,      "Key info",          na_int,            na_chr,     na_chr,    na_chr,
    "adm0",        "Administrative level 0 (country)",             "Niveau administratif 0 (pays)",                      na_chr,        na_chr,     na_chr,       na_chr,             na_chr,               na_chr,      "Key info",          na_int,            na_chr,     na_chr,    na_chr,
    "adm1",        "Administrative level 1 (province/region)",     "Niveau administratif 1 (province/region)",           na_chr,        na_chr,     na_chr,       na_chr,             na_chr,               na_chr,      "Key info",          na_int,            na_chr,     na_chr,    na_chr,
    "adm2",        "Administrative level 2 (district/zone)",       "Niveau administratif 2 (district/zone de sante)",    na_chr,        na_chr,     na_chr,       na_chr,             na_chr,               na_chr,      "Key info",          na_int,            na_chr,     na_chr,    na_chr,
    "adm3",        "Administrative level 3 (commune/subdistrict)", "Niveau administratif 3 (commune/sous-district)",     na_chr,        na_chr,     na_chr,       na_chr,             na_chr,               na_chr,      "Key info",          na_int,            na_chr,     na_chr,    na_chr,
    "survey_year", "Survey year",                                  "Annee de l'enquete",                                 na_chr,        na_chr,     na_chr,       na_chr,             na_chr,               na_chr,      "Key info",          na_int,            na_chr,     na_chr,    na_chr,
    "survey_type", "Survey type (DHS, MIS, etc.)",                 "Type d'enquete (EDS, EIP, etc.)",                    na_chr,        na_chr,     na_chr,       na_chr,             na_chr,               na_chr,      "Key info",          na_int,            na_chr,     na_chr,    na_chr
  )
}


#' Lookup metadata for MBG indicator names
#'
#' Returns DHS recode type, indicator category, WMR cascade step,
#' target age group, and base units for a given indicator name.
#'
#' @param ind Character scalar indicator name.
#' @return A named list with: recode, category, cascade, age, base_unit.
#' @noRd
.mbg_indicator_meta <- function(ind) {
  na_chr <- NA_character_
  na_int <- NA_integer_

  meta <- list(
    # WMR Cascade — KR module, Malaria category
    fever             = list(recode = "KR", category = "Malaria",       cascade = 0L, age = "0-59 months"),
    csb_public        = list(recode = "KR", category = "Malaria",       cascade = 1L, age = "0-59 months"),
    csb_private       = list(recode = "KR", category = "Malaria",       cascade = 1L, age = "0-59 months"),
    csb_none          = list(recode = "KR", category = "Malaria",       cascade = 1L, age = "0-59 months"),
    csb_any           = list(recode = "KR", category = "Malaria",       cascade = 1L, age = "0-59 months"),
    csb_trained       = list(recode = "KR", category = "Malaria",       cascade = 1L, age = "0-59 months"),
    malaria_dx        = list(recode = "KR", category = "Malaria",       cascade = 2L, age = "0-59 months"),
    antimalarial      = list(recode = "KR", category = "Malaria",       cascade = 3L, age = "0-59 months"),
    act               = list(recode = "KR",    category = "Malaria", cascade = 4L,      age = "0-59 months"),
    act_tested        = list(recode = "KR",    category = "Malaria", cascade = 4L,      age = "0-59 months"),
    febrile_rdt_pos   = list(recode = "KR+PR", category = "Malaria", cascade = 2L,      age = "0-59 months"),
    febrile_rdt_pos_act = list(recode = "KR+PR", category = "Malaria", cascade = 4L,    age = "0-59 months"),
    # PfPR — PR module
    pfpr_rdt          = list(recode = "PR", category = "Malaria",       cascade = na_int, age = "6-59 months"),
    pfpr_mic          = list(recode = "PR", category = "Malaria",       cascade = na_int, age = "6-59 months"),
    pfpr_rdt_u5       = list(recode = "PR", category = "Malaria",       cascade = na_int, age = "6-59 months"),
    pfpr_mic_u5       = list(recode = "PR", category = "Malaria",       cascade = na_int, age = "6-59 months"),
    pfpr_either_u5    = list(recode = "PR", category = "Malaria",       cascade = na_int, age = "6-59 months"),
    pfpr_combined_u5  = list(recode = "PR", category = "Malaria",       cascade = na_int, age = "6-59 months"),
    # ITN — HR/PR module
    itn_ownership     = list(recode = "HR/PR", category = "ITN",        cascade = na_int, age = "all ages"),
    itn_access        = list(recode = "HR/PR", category = "ITN",        cascade = na_int, age = "all ages"),
    itn_use           = list(recode = "HR/PR", category = "ITN",        cascade = na_int, age = "all ages"),
    itn_use_all       = list(recode = "HR/PR", category = "ITN",        cascade = na_int, age = "all ages"),
    itn_use_u5        = list(recode = "HR/PR", category = "ITN",        cascade = na_int, age = "0-59 months"),
    itn_use_preg      = list(recode = "HR/PR", category = "ITN",        cascade = na_int, age = "women 15-49"),
    itn_use_5_10      = list(recode = "HR/PR", category = "ITN",        cascade = na_int, age = "5-10 years"),
    itn_use_10_20     = list(recode = "HR/PR", category = "ITN",        cascade = na_int, age = "10-20 years"),
    itn_use_20plus    = list(recode = "HR/PR", category = "ITN",        cascade = na_int, age = "20+ years"),
    itn_use_pregnant  = list(recode = "HR/PR", category = "ITN",        cascade = na_int, age = "women 15-49"),
    itn_use_if_access = list(recode = "HR/PR", category = "ITN",        cascade = na_int, age = "all ages"),
    # Anemia — PR module
    severe_anemia      = list(recode = "PR", category = "Nutrition",    cascade = na_int, age = "6-59 months"),
    anemia_any         = list(recode = "PR", category = "Nutrition",    cascade = na_int, age = "6-59 months"),
    anemia_moderate_plus = list(recode = "PR", category = "Nutrition",  cascade = na_int, age = "6-59 months"),
    anemia_severe      = list(recode = "PR", category = "Nutrition",    cascade = na_int, age = "6-59 months"),
    anemia_mild_only   = list(recode = "PR", category = "Nutrition",    cascade = na_int, age = "6-59 months"),
    anemia_moderate_only = list(recode = "PR", category = "Nutrition",  cascade = na_int, age = "6-59 months"),
    anemia_severe_only = list(recode = "PR", category = "Nutrition",    cascade = na_int, age = "6-59 months"),
    # ANC — IR module
    anc_1plus         = list(recode = "IR", category = "Maternal health", cascade = na_int, age = "women 15-49"),
    anc_3plus         = list(recode = "IR", category = "Maternal health", cascade = na_int, age = "women 15-49"),
    anc_4plus         = list(recode = "IR", category = "Maternal health", cascade = na_int, age = "women 15-49"),
    anc_8plus         = list(recode = "IR", category = "Maternal health", cascade = na_int, age = "women 15-49"),
    # IPTp — IR module
    iptp_1plus        = list(recode = "IR", category = "Maternal health", cascade = na_int, age = "women 15-49"),
    iptp_2plus        = list(recode = "IR", category = "Maternal health", cascade = na_int, age = "women 15-49"),
    iptp_3plus        = list(recode = "IR", category = "Maternal health", cascade = na_int, age = "women 15-49"),
    iptp_4plus        = list(recode = "IR", category = "Maternal health", cascade = na_int, age = "women 15-49"),
    iptp_1only        = list(recode = "IR", category = "Maternal health", cascade = na_int, age = "women 15-49"),
    iptp_2only        = list(recode = "IR", category = "Maternal health", cascade = na_int, age = "women 15-49"),
    iptp_3only        = list(recode = "IR", category = "Maternal health", cascade = na_int, age = "women 15-49"),
    # EPI — KR module
    epi_bcg           = list(recode = "KR", category = "Immunization",  cascade = na_int, age = "12-23 months"),
    epi_dpt1          = list(recode = "KR", category = "Immunization",  cascade = na_int, age = "12-23 months"),
    epi_dpt2          = list(recode = "KR", category = "Immunization",  cascade = na_int, age = "12-23 months"),
    epi_dpt3          = list(recode = "KR", category = "Immunization",  cascade = na_int, age = "12-23 months"),
    epi_polio1        = list(recode = "KR", category = "Immunization",  cascade = na_int, age = "12-23 months"),
    epi_polio2        = list(recode = "KR", category = "Immunization",  cascade = na_int, age = "12-23 months"),
    epi_polio3        = list(recode = "KR", category = "Immunization",  cascade = na_int, age = "12-23 months"),
    epi_measles1      = list(recode = "KR", category = "Immunization",  cascade = na_int, age = "12-23 months"),
    epi_measles2      = list(recode = "KR", category = "Immunization",  cascade = na_int, age = "12-23 months"),
    epi_vita1         = list(recode = "KR", category = "Immunization",  cascade = na_int, age = "12-23 months"),
    epi_vita2         = list(recode = "KR", category = "Immunization",  cascade = na_int, age = "12-23 months"),
    epi_malaria       = list(recode = "KR", category = "Immunization",  cascade = na_int, age = "12-23 months"),
    epi_fully_vaccinated = list(recode = "KR", category = "Immunization", cascade = na_int, age = "12-23 months"),
    # U5MR — KR module (special unit)
    u5mr              = list(recode = "KR", category = "Mortality",     cascade = na_int, age = "0-59 months",
                             base_unit = "per 1000 live births"),
    # IRS — HR module
    irs_coverage      = list(recode = "HR", category = "IRS",           cascade = na_int, age = "all ages"),
    # SMC — KR module
    smc_coverage      = list(recode = "KR", category = "SMC",           cascade = na_int, age = "0-59 months")
  )

  m <- meta[[ind]]
  if (is.null(m)) {
    m <- list(
      recode = na_chr, category = na_chr, cascade = na_int, age = na_chr
    )
  }
  m$base_unit <- m$base_unit %||% "proportion (0-1)"
  m
}


#' Validate label variables against data dictionary
#'
#' Warns when labels reference variables not present in the data.
#'
#' @param dict Enriched data dictionary.
#' @param labels Labels tibble used for enrichment.
#' @noRd
.validate_dictionary_labels <- function(dict, labels) {
  if (!is.data.frame(dict) || !is.data.frame(labels)) return(invisible(NULL))
  if (!"variable" %in% names(labels) || nrow(labels) == 0) return(invisible(NULL))

  data_vars <- dict$variable
  label_vars <- labels$variable
  orphaned <- setdiff(label_vars, data_vars)

  if (length(orphaned) > 0) {
    cli::cli_warn(c(
      "!" = "{length(orphaned)} label(s) don't match any data column",
      "i" = "Orphaned: {.var {orphaned}}"
    ))
  }
  invisible(NULL)
}
