#' Enrich a sntutils data dictionary with DHS/MBG domain labels
#'
#' Left-joins DHS-specific labels onto a base dictionary produced by
#' \code{sntutils::build_dictionary()}, overriding generic auto-labels
#' with precise DHS indicator names and methodological notes.
#'
#' @param dict A data frame produced by \code{sntutils::build_dictionary()}.
#' @param labels A tibble with at least a \code{variable} column and one or more
#'   of: \code{label_en}, \code{dhs_variable}, \code{numerator},
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
    "dhs_recode", "indicator_category", "cascade_step",
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
    "variable", "type", "label_en",
    "category",
    "dhs_variable", "numerator", "denominator",
    "dhs_numerator_var", "dhs_denominator_var",
    "dhs_recode", "indicator_category", "cascade_step",
    "age_group", "units",
    "n", "n_missing", "pct_missing", "n_unique",
    "example_values", "min", "max", "notes"
  )
  existing <- intersect(desired_order, names(enriched))
  remaining <- setdiff(names(enriched), desired_order)
  enriched <- enriched[, c(existing, remaining), drop = FALSE]

  tibble::as_tibble(enriched)
}


#' Lookup metadata for MBG indicator names
#'
#' Returns DHS recode type, indicator category, cascade step,
#' target age group, and base units for a given indicator name.
#'
#' @param ind Character scalar indicator name.
#' @return A named list with: recode, category, cascade, age, base_unit.
#' @noRd
.mbg_indicator_meta <- function(ind) {
  na_chr <- NA_character_
  na_int <- NA_integer_

  meta <- list(
    # Cascade — KR module, Malaria category
    fever             = list(recode = "KR", category = "Malaria",       cascade = 0L, age = "0-59 months",  pop_type = "u5"),
    csb_public        = list(recode = "KR", category = "Malaria",       cascade = 1L, age = "0-59 months",  pop_type = "u5"),
    csb_private       = list(recode = "KR", category = "Malaria",       cascade = 1L, age = "0-59 months",  pop_type = "u5"),
    csb_none          = list(recode = "KR", category = "Malaria",       cascade = 1L, age = "0-59 months",  pop_type = "u5"),
    csb_any           = list(recode = "KR", category = "Malaria",       cascade = 1L, age = "0-59 months",  pop_type = "u5"),
    csb_trained       = list(recode = "KR", category = "Malaria",       cascade = 1L, age = "0-59 months",  pop_type = "u5"),
    malaria_dx        = list(recode = "KR", category = "Malaria",       cascade = 2L, age = "0-59 months",  pop_type = "u5"),
    antimalarial      = list(recode = "KR", category = "Malaria",       cascade = 3L, age = "0-59 months",  pop_type = "u5"),
    antimalarial_public = list(recode = "KR", category = "Malaria",     cascade = 3L, age = "0-59 months",  pop_type = "u5"),
    act               = list(recode = "KR",    category = "Malaria",    cascade = 4L, age = "0-59 months",  pop_type = "u5"),
    act_public        = list(recode = "KR",    category = "Malaria",    cascade = 4L, age = "0-59 months",  pop_type = "u5"),
    act_tested        = list(recode = "KR",    category = "Malaria",    cascade = 4L, age = "0-59 months",  pop_type = "u5"),
    febrile_rdt_pos   = list(recode = "KR+PR", category = "Malaria",    cascade = 2L, age = "0-59 months",  pop_type = "u5"),
    febrile_rdt_pos_act = list(recode = "KR+PR", category = "Malaria",  cascade = 4L, age = "0-59 months",  pop_type = "u5"),
    # PfPR — PR module
    pfpr_rdt          = list(recode = "PR", category = "Malaria",       cascade = na_int, age = "6-59 months",  pop_type = "u5"),
    pfpr_mic          = list(recode = "PR", category = "Malaria",       cascade = na_int, age = "6-59 months",  pop_type = "u5"),
    pfpr_rdt_u5       = list(recode = "PR", category = "Malaria",       cascade = na_int, age = "6-59 months",  pop_type = "u5"),
    pfpr_mic_u5       = list(recode = "PR", category = "Malaria",       cascade = na_int, age = "6-59 months",  pop_type = "u5"),
    pfpr_either_u5    = list(recode = "PR", category = "Malaria",       cascade = na_int, age = "6-59 months",  pop_type = "u5"),
    pfpr_combined_u5  = list(recode = "PR", category = "Malaria",       cascade = na_int, age = "6-59 months",  pop_type = "u5"),
    # ITN — HR/PR module (aligned with DHS indicator codes)
    enough_itn        = list(recode = "HR/PR", category = "ITN",        cascade = na_int, age = "all ages",     pop_type = "all"),
    with_itn          = list(recode = "HR/PR", category = "ITN",        cascade = na_int, age = "all ages",     pop_type = "all"),
    access_itn        = list(recode = "HR/PR", category = "ITN",        cascade = na_int, age = "all ages",     pop_type = "all"),
    use_itn           = list(recode = "HR/PR", category = "ITN",        cascade = na_int, age = "all ages",     pop_type = "all"),
    use_itn_chu5      = list(recode = "HR/PR", category = "ITN",        cascade = na_int, age = "0-59 months",  pop_type = "u5"),
    use_itn_preg      = list(recode = "HR/PR", category = "ITN",        cascade = na_int, age = "women 15-49",  pop_type = "wra"),
    use_itn_5_10      = list(recode = "HR/PR", category = "ITN",        cascade = na_int, age = "5-10 years",   pop_type = "5_10"),
    use_itn_10_20     = list(recode = "HR/PR", category = "ITN",        cascade = na_int, age = "10-20 years",  pop_type = "10_20"),
    use_itn_20plus    = list(recode = "HR/PR", category = "ITN",        cascade = na_int, age = "20+ years",    pop_type = "20plus"),
    use_itn_if_access = list(recode = "HR/PR", category = "ITN",        cascade = na_int, age = "all ages",     pop_type = "all"),
    # Anemia — PR module
    severe_anemia      = list(recode = "PR", category = "Nutrition",    cascade = na_int, age = "6-59 months",  pop_type = "u5"),
    anemia_any         = list(recode = "PR", category = "Nutrition",    cascade = na_int, age = "6-59 months",  pop_type = "u5"),
    anemia_moderate_plus = list(recode = "PR", category = "Nutrition",  cascade = na_int, age = "6-59 months",  pop_type = "u5"),
    anemia_severe      = list(recode = "PR", category = "Nutrition",    cascade = na_int, age = "6-59 months",  pop_type = "u5"),
    anemia_mild_only   = list(recode = "PR", category = "Nutrition",    cascade = na_int, age = "6-59 months",  pop_type = "u5"),
    anemia_moderate_only = list(recode = "PR", category = "Nutrition",  cascade = na_int, age = "6-59 months",  pop_type = "u5"),
    anemia_severe_only = list(recode = "PR", category = "Nutrition",    cascade = na_int, age = "6-59 months",  pop_type = "u5"),
    # ANC — IR module
    anc_1plus         = list(recode = "IR", category = "Maternal health", cascade = na_int, age = "women 15-49", pop_type = "wra"),
    anc_3plus         = list(recode = "IR", category = "Maternal health", cascade = na_int, age = "women 15-49", pop_type = "wra"),
    anc_4plus         = list(recode = "IR", category = "Maternal health", cascade = na_int, age = "women 15-49", pop_type = "wra"),
    anc_8plus         = list(recode = "IR", category = "Maternal health", cascade = na_int, age = "women 15-49", pop_type = "wra"),
    # IPTp — IR module
    iptp_1plus        = list(recode = "IR", category = "Maternal health", cascade = na_int, age = "women 15-49", pop_type = "wra"),
    iptp_2plus        = list(recode = "IR", category = "Maternal health", cascade = na_int, age = "women 15-49", pop_type = "wra"),
    iptp_3plus        = list(recode = "IR", category = "Maternal health", cascade = na_int, age = "women 15-49", pop_type = "wra"),
    iptp_4plus        = list(recode = "IR", category = "Maternal health", cascade = na_int, age = "women 15-49", pop_type = "wra"),
    iptp_1only        = list(recode = "IR", category = "Maternal health", cascade = na_int, age = "women 15-49", pop_type = "wra"),
    iptp_2only        = list(recode = "IR", category = "Maternal health", cascade = na_int, age = "women 15-49", pop_type = "wra"),
    iptp_3only        = list(recode = "IR", category = "Maternal health", cascade = na_int, age = "women 15-49", pop_type = "wra"),
    # EPI — KR module
    epi_bcg           = list(recode = "KR", category = "Immunization",  cascade = na_int, age = "12-23 months", pop_type = "u5"),
    epi_dpt1          = list(recode = "KR", category = "Immunization",  cascade = na_int, age = "12-23 months", pop_type = "u5"),
    epi_dpt2          = list(recode = "KR", category = "Immunization",  cascade = na_int, age = "12-23 months", pop_type = "u5"),
    epi_dpt3          = list(recode = "KR", category = "Immunization",  cascade = na_int, age = "12-23 months", pop_type = "u5"),
    epi_polio1        = list(recode = "KR", category = "Immunization",  cascade = na_int, age = "12-23 months", pop_type = "u5"),
    epi_polio2        = list(recode = "KR", category = "Immunization",  cascade = na_int, age = "12-23 months", pop_type = "u5"),
    epi_polio3        = list(recode = "KR", category = "Immunization",  cascade = na_int, age = "12-23 months", pop_type = "u5"),
    epi_measles1      = list(recode = "KR", category = "Immunization",  cascade = na_int, age = "12-23 months", pop_type = "u5"),
    epi_measles2      = list(recode = "KR", category = "Immunization",  cascade = na_int, age = "12-23 months", pop_type = "u5"),
    epi_vita1         = list(recode = "KR", category = "Immunization",  cascade = na_int, age = "12-23 months", pop_type = "u5"),
    epi_vita2         = list(recode = "KR", category = "Immunization",  cascade = na_int, age = "12-23 months", pop_type = "u5"),
    epi_malaria       = list(recode = "KR", category = "Immunization",  cascade = na_int, age = "12-23 months", pop_type = "u5"),
    epi_fully_vaccinated = list(recode = "KR", category = "Immunization", cascade = na_int, age = "12-23 months", pop_type = "u5"),
    epi_penta1        = list(recode = "KR", category = "Immunization",  cascade = na_int, age = "12-23 months", pop_type = "u5"),
    epi_penta2        = list(recode = "KR", category = "Immunization",  cascade = na_int, age = "12-23 months", pop_type = "u5"),
    epi_penta3        = list(recode = "KR", category = "Immunization",  cascade = na_int, age = "12-23 months", pop_type = "u5"),
    epi_pneumo1       = list(recode = "KR", category = "Immunization",  cascade = na_int, age = "12-23 months", pop_type = "u5"),
    epi_pneumo2       = list(recode = "KR", category = "Immunization",  cascade = na_int, age = "12-23 months", pop_type = "u5"),
    epi_pneumo3       = list(recode = "KR", category = "Immunization",  cascade = na_int, age = "12-23 months", pop_type = "u5"),
    epi_rota1         = list(recode = "KR", category = "Immunization",  cascade = na_int, age = "12-23 months", pop_type = "u5"),
    epi_rota2         = list(recode = "KR", category = "Immunization",  cascade = na_int, age = "12-23 months", pop_type = "u5"),
    epi_rota3         = list(recode = "KR", category = "Immunization",  cascade = na_int, age = "12-23 months", pop_type = "u5"),
    epi_ipv           = list(recode = "KR", category = "Immunization",  cascade = na_int, age = "12-23 months", pop_type = "u5"),
    epi_hepb0         = list(recode = "KR", category = "Immunization",  cascade = na_int, age = "12-23 months", pop_type = "u5"),
    epi_yellowfever   = list(recode = "KR", category = "Immunization",  cascade = na_int, age = "12-23 months", pop_type = "u5"),
    # U5MR — KR module (special unit)
    u5mr              = list(recode = "KR", category = "Mortality",     cascade = na_int, age = "0-59 months",  pop_type = "u5",
                             base_unit = "per 1000 live births"),
    # IRS — HR module
    irs_coverage      = list(recode = "HR", category = "IRS",           cascade = na_int, age = "all ages",     pop_type = "all"),
    # SMC — KR module
    smc_coverage      = list(recode = "KR", category = "SMC",           cascade = na_int, age = "0-59 months",  pop_type = "u5"),
    # Derived: Effective coverage of case management
    eff_cm_any        = list(recode = "KR", category = "Malaria",       cascade = na_int, age = "0-59 months",  pop_type = "u5"),
    eff_cm_public     = list(recode = "KR", category = "Malaria",       cascade = na_int, age = "0-59 months",  pop_type = "u5")
  )

  m <- meta[[ind]]
  if (is.null(m)) {
    m <- list(
      recode = na_chr, category = na_chr, cascade = na_int, age = na_chr,
      pop_type = "all"
    )
  }
  m$base_unit <- m$base_unit %||% "proportion (0-1)"
  m$pop_type  <- m$pop_type  %||% "all"
  m
}


#' Population Raster Type for an MBG Indicator
#'
#' Looks up which population raster to use for a given indicator code.
#' Returns `"u5"` for child indicators (0-59 months, 6-59 months, 12-23 months),
#' `"wra"` for women of reproductive age (ANC, IPTp), or `"all"` for total
#' population (IRS, household-level ITN). For category-level dispatch keys
#' (e.g., `"itn"`, `"pfpr"`), returns the dominant pop_type for that family.
#'
#' @param ind Character indicator code or category name.
#' @return Character scalar: `"u5"`, `"wra"`, or `"all"`.
#' @noRd
.mbg_indicator_pop_type <- function(ind) {
  # Category-level defaults (when dispatching a whole family)
  category_pop <- c(
    pfpr = "u5", itn = "all", irs = "all", anc = "wra",
    csb = "u5", act = "u5", anemia = "u5", iptp = "wra",
    epi = "u5", u5mr = "u5", smc = "u5", fever = "u5",
    antimalarial = "u5", eff_cm = "u5"
  )

  if (ind %in% names(category_pop)) return(category_pop[[ind]])

  # Individual indicator lookup from meta
  .mbg_indicator_meta(ind)$pop_type
}


#' MBG Indicator Labels and Descriptions
#'
#' Returns human-readable indicator name, numerator description,
#' denominator description, and denominator code for an MBG indicator.
#' Used to build long-format output matching the DHS column structure.
#'
#' @param ind Character indicator code (e.g., "act", "pfpr_rdt").
#' @return Named list with `indicator`, `numerator_description`,
#'   `denominator_description`, `denominator_code`.
#' @noRd
.mbg_indicator_label <- function(ind) {
  meta <- .mbg_indicator_meta(ind)

  # ---- Indicator display name ----
  labels <- list(
    # Fever / care-seeking cascade
    fever             = "Fever prevalence (U5)",
    csb_public        = "Care-seeking: public sector",
    csb_private       = "Care-seeking: private sector",
    csb_none          = "Care-seeking: none",
    csb_any           = "Care-seeking: any provider",
    csb_trained       = "Care-seeking: trained provider",
    malaria_dx        = "Malaria diagnostic testing (U5)",
    antimalarial      = "Antimalarial treatment (U5)",
    antimalarial_public = "Antimalarial treatment: public sector",
    # ACT
    act               = "ACT treatment (U5)",
    act_public        = "ACT treatment: public sector",
    act_tested        = "ACT among test-positive (U5)",
    febrile_rdt_pos   = "Febrile RDT positive (U5)",
    febrile_rdt_pos_act = "ACT among febrile RDT positive (U5)",
    # PfPR
    pfpr_rdt          = "PfPR by RDT",
    pfpr_mic          = "PfPR by microscopy",
    pfpr_rdt_u5       = "PfPR by RDT (U5)",
    pfpr_mic_u5       = "PfPR by microscopy (U5)",
    pfpr_either_u5    = "PfPR by RDT or microscopy (U5)",
    pfpr_combined_u5  = "PfPR combined (U5)",
    # ITN
    enough_itn        = "Sufficient ITNs (1 per 2 people)",
    with_itn          = "Household owns at least 1 ITN",
    access_itn        = "Population with access to ITN",
    use_itn           = "ITN use (slept under ITN)",
    use_itn_chu5      = "ITN use: children under 5",
    use_itn_preg      = "ITN use: pregnant women",
    use_itn_5_10      = "ITN use: ages 5-10",
    use_itn_10_20     = "ITN use: ages 10-20",
    use_itn_20plus    = "ITN use: ages 20+",
    use_itn_if_access = "ITN use given access",
    # Anemia
    severe_anemia      = "Severe anemia (U5)",
    anemia_any         = "Any anemia (U5)",
    anemia_moderate_plus = "Moderate+ anemia (U5)",
    anemia_severe      = "Severe anemia (U5)",
    anemia_mild_only   = "Mild anemia only (U5)",
    anemia_moderate_only = "Moderate anemia only (U5)",
    anemia_severe_only = "Severe anemia only (U5)",
    # ANC
    anc_1plus          = "ANC 1+ visits",
    anc_3plus          = "ANC 3+ visits",
    anc_4plus          = "ANC 4+ visits",
    anc_8plus          = "ANC 8+ visits",
    # IPTp
    iptp_1plus         = "IPTp 1+ doses",
    iptp_2plus         = "IPTp 2+ doses",
    iptp_3plus         = "IPTp 3+ doses",
    iptp_4plus         = "IPTp 4+ doses",
    iptp_1only         = "IPTp 1 dose only",
    iptp_2only         = "IPTp 2 doses only",
    iptp_3only         = "IPTp 3 doses only",
    # IRS
    irs_coverage       = "IRS coverage",
    # SMC
    smc_coverage       = "SMC coverage (U5)",
    # U5MR
    u5mr               = "Under-5 mortality rate",
    # Derived
    eff_cm_any         = "Effective case management (any provider)",
    eff_cm_public      = "Effective case management (public sector)"
  )

  indicator_name <- labels[[ind]] %||% gsub("_", " ", ind)

  # ---- Denominator descriptions from age/population ----
  denom_map <- list(
    "0-59 months"  = list(desc = "Children under 5 years",          code = "u5"),
    "6-59 months"  = list(desc = "Children 6-59 months",            code = "ch_6_59m"),
    "12-23 months" = list(desc = "Children 12-23 months",           code = "ch_12_23m"),
    "women 15-49"  = list(desc = "Women aged 15-49 with live birth in last 2 years", code = "wra"),
    "all ages"     = list(desc = "De facto household population",   code = "hh_pop")
  )

  denom_info <- denom_map[[meta$age]] %||% list(desc = meta$age, code = tolower(gsub("[- ]", "_", meta$age)))

  list(
    indicator                = indicator_name,
    numerator_description    = indicator_name,
    denominator_description  = denom_info$desc,
    denominator_code         = denom_info$code
  )
}


#' Valid MBG Indicator Codes
#'
#' Returns the complete set of valid indicator codes for the MBG pipeline.
#' Built from `.mbg_indicator_meta()` keys (the single source of truth) plus
#' category-level shorthand names used for dispatch.
#'
#' @return Character vector of all valid indicator names.
#' @noRd
.valid_mbg_indicators <- function() {
  # Category-level dispatch keys (run all sub-indicators for that family)
  categories <- c(
    "pfpr", "itn", "irs", "anc", "csb", "act", "anemia", "iptp", "epi",
    "u5mr", "smc", "fever", "antimalarial",
    # Derived (auto-expands to dependencies)
    "eff_cm"
  )

  # Individual indicator codes — aligned with _conditions() indicator_code
  # values and _mbg_dictionary() name values across all calc functions
  individual_codes <- c(
    # PfPR (PR module)
    "pfpr_rdt", "pfpr_mic",
    "pfpr_rdt_u5", "pfpr_rdt_5_10", "pfpr_rdt_u10", "pfpr_rdt_2_10",
    "pfpr_mic_u5", "pfpr_mic_5_10", "pfpr_mic_u10", "pfpr_mic_2_10",
    "pfpr_either_u5", "pfpr_either_5_10", "pfpr_either_u10", "pfpr_either_2_10",
    "pfpr_combined_u5",
    # ITN (HR/PR module)
    "enough_itn", "with_itn", "access_itn", "use_itn",
    "use_itn_chu5", "use_itn_preg", "use_itn_if_access",
    "use_itn_5_10", "use_itn_10_20", "use_itn_20plus",
    # Case management cascade (KR module)
    "fever", "malaria_dx",
    "csb_any", "csb_public", "csb_pub_nochw", "csb_chw",
    "csb_private", "csb_priv_formal", "csb_pharmacy",
    "csb_priv_informal", "csb_priv_form_pha",
    "csb_trained", "csb_none",
    # Antimalarial (KR module)
    "antimalarial", "antimalarial_public",
    # ACT (KR module) — from .act_mbg_dictionary()
    "act", "act_care_seek", "act_antimal", "act_any_tx",
    "act_trained", "act_pub", "act_pub_nochw", "act_chw",
    "act_priv", "act_priv_formal", "act_priv_pharm",
    "act_priv_informal", "act_priv_form_pha",
    "act_public", "act_tested",
    "febrile_rdt_pos", "febrile_rdt_pos_act",
    # Antimalarial sub-indicators from ACT dictionary
    "antimal", "antimal_any_tx", "antimal_trained",
    "antimal_pub", "antimal_pub_nochw", "antimal_chw",
    "antimal_priv", "antimal_formal", "antimal_pharm",
    "antimal_priv_informal", "antimal_form_pharm",
    # Malaria dx among antimalarial recipients
    "mal_dx_am", "mal_dx_pub_am", "mal_dx_pub_nochw_am",
    "mal_dx_chw_am", "mal_dx_priv_am", "mal_dx_priv_formal_am",
    "mal_dx_pharm_am", "mal_dx_priv_informal_am", "mal_dx_priv_form_pha_am",
    # Anemia (PR module)
    "severe_anemia", "anemia_any", "anemia_moderate_plus", "anemia_severe",
    "anemia_mild_only", "anemia_moderate_only", "anemia_severe_only",
    # ANC (IR module)
    "anc_1plus", "anc_2plus", "anc_3plus", "anc_4plus", "anc_8plus",
    # IPTp (IR module)
    "iptp_1plus", "iptp_2plus", "iptp_3plus", "iptp_4plus",
    "iptp_1only", "iptp_2only", "iptp_3only",
    # EPI (KR module)
    "epi_bcg", "epi_dpt1", "epi_dpt2", "epi_dpt3",
    "epi_polio0", "epi_polio1", "epi_polio2", "epi_polio3",
    "epi_measles1", "epi_measles2",
    "epi_vita1", "epi_vita2", "epi_malaria",
    "epi_penta1", "epi_penta2", "epi_penta3",
    "epi_pneumo1", "epi_pneumo2", "epi_pneumo3",
    "epi_rota1", "epi_rota2", "epi_rota3",
    "epi_ipv", "epi_hepb0", "epi_yellowfever",
    "epi_any", "epi_never_vaccinated", "epi_fully_vaccinated",
    # Mortality (BR module)
    "u5mr",
    # IRS (HR module)
    "irs_coverage",
    # SMC (KR module)
    "smc_coverage",
    # Derived
    "eff_cm_any", "eff_cm_public"
  )

  unique(c(categories, individual_codes))
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
