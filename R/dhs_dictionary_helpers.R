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
#' @param custom_csb_indicator Optional validated `custom_csb_indicator`
#'   spec list. When non-NULL, derived custom CSB codes
#'   (`<name>_dhis`, `<name>_nondhis`, `<name>_untreat`) are recognized
#'   with the same KR/U5 metadata as the built-in CSB family.
#' @return A named list with: recode, category, cascade, age, base_unit.
#' @noRd
.mbg_indicator_meta <- function(ind, custom_csb_indicator = NULL) {
  na_chr <- NA_character_
  na_int <- NA_integer_

  meta <- list(
    # Cascade -- KR module, Malaria category
    fever             = list(recode = "KR", category = "Malaria",       cascade = 0L, age = "0-59 months",  pop_type = "u5"),
    csb_public        = list(recode = "KR", category = "Malaria",       cascade = 1L, age = "0-59 months",  pop_type = "u5"),
    csb_private       = list(recode = "KR", category = "Malaria",       cascade = 1L, age = "0-59 months",  pop_type = "u5"),
    csb_none          = list(recode = "KR", category = "Malaria",       cascade = 1L, age = "0-59 months",  pop_type = "u5"),
    csb_any           = list(recode = "KR", category = "Malaria",       cascade = 1L, age = "0-59 months",  pop_type = "u5"),
    csb_trained       = list(recode = "KR", category = "Malaria",       cascade = 1L, age = "0-59 months",  pop_type = "u5"),
    csb_chw           = list(recode = "KR", category = "Malaria",       cascade = 1L, age = "0-59 months",  pop_type = "u5"),
    csb_pharmacy      = list(recode = "KR", category = "Malaria",       cascade = 1L, age = "0-59 months",  pop_type = "u5"),
    csb_priv_formal   = list(recode = "KR", category = "Malaria",       cascade = 1L, age = "0-59 months",  pop_type = "u5"),
    csb_priv_informal = list(recode = "KR", category = "Malaria",       cascade = 1L, age = "0-59 months",  pop_type = "u5"),
    csb_priv_form_pha = list(recode = "KR", category = "Malaria",       cascade = 1L, age = "0-59 months",  pop_type = "u5"),
    csb_pub_nochw     = list(recode = "KR", category = "Malaria",       cascade = 1L, age = "0-59 months",  pop_type = "u5"),
    # Wealth-stratified CSB -- KR module
    csb_q1            = list(recode = "KR", category = "Malaria",       cascade = 1L, age = "0-59 months",  pop_type = "u5"),
    csb_q2            = list(recode = "KR", category = "Malaria",       cascade = 1L, age = "0-59 months",  pop_type = "u5"),
    csb_q3            = list(recode = "KR", category = "Malaria",       cascade = 1L, age = "0-59 months",  pop_type = "u5"),
    csb_q4            = list(recode = "KR", category = "Malaria",       cascade = 1L, age = "0-59 months",  pop_type = "u5"),
    csb_q5            = list(recode = "KR", category = "Malaria",       cascade = 1L, age = "0-59 months",  pop_type = "u5"),
    malaria_dx        = list(recode = "KR", category = "Malaria",       cascade = 2L, age = "0-59 months",  pop_type = "u5"),
    antimalarial      = list(recode = "KR", category = "Malaria",       cascade = 3L, age = "0-59 months",  pop_type = "u5"),
    antimalarial_public = list(recode = "KR", category = "Malaria",     cascade = 3L, age = "0-59 months",  pop_type = "u5"),
    act               = list(recode = "KR",    category = "Malaria",    cascade = 4L, age = "0-59 months",  pop_type = "u5"),
    act_pub           = list(recode = "KR",    category = "Malaria",    cascade = 4L, age = "0-59 months",  pop_type = "u5"),
    act_tested        = list(recode = "KR",    category = "Malaria",    cascade = 4L, age = "0-59 months",  pop_type = "u5"),
    act_care_seek     = list(recode = "KR",    category = "Malaria",    cascade = 4L, age = "0-59 months",  pop_type = "u5"),
    act_am       = list(recode = "KR",    category = "Malaria",    cascade = 4L, age = "0-59 months",  pop_type = "u5"),
    act_any_tx_am        = list(recode = "KR",    category = "Malaria",    cascade = 4L, age = "0-59 months",  pop_type = "u5"),
    act_trained_am       = list(recode = "KR",    category = "Malaria",    cascade = 4L, age = "0-59 months",  pop_type = "u5"),
    act_pub_am        = list(recode = "KR",    category = "Malaria",    cascade = 4L, age = "0-59 months",  pop_type = "u5"),
    act_pub_nochw_am     = list(recode = "KR",    category = "Malaria",    cascade = 4L, age = "0-59 months",  pop_type = "u5"),
    act_chw_am           = list(recode = "KR",    category = "Malaria",    cascade = 4L, age = "0-59 months",  pop_type = "u5"),
    act_priv_am          = list(recode = "KR",    category = "Malaria",    cascade = 4L, age = "0-59 months",  pop_type = "u5"),
    act_priv_formal_am   = list(recode = "KR",    category = "Malaria",    cascade = 4L, age = "0-59 months",  pop_type = "u5"),
    act_priv_pharm_am    = list(recode = "KR",    category = "Malaria",    cascade = 4L, age = "0-59 months",  pop_type = "u5"),
    act_priv_informal_am = list(recode = "KR",    category = "Malaria",    cascade = 4L, age = "0-59 months",  pop_type = "u5"),
    act_priv_form_pha_am = list(recode = "KR",    category = "Malaria",    cascade = 4L, age = "0-59 months",  pop_type = "u5"),
    # Antimalarial sub-indicators (from .act_mbg_dictionary)
    antimal           = list(recode = "KR",    category = "Malaria",    cascade = 3L, age = "0-59 months",  pop_type = "u5"),
    antimal_any_tx    = list(recode = "KR",    category = "Malaria",    cascade = 3L, age = "0-59 months",  pop_type = "u5"),
    antimal_trained   = list(recode = "KR",    category = "Malaria",    cascade = 3L, age = "0-59 months",  pop_type = "u5"),
    antimal_pub       = list(recode = "KR",    category = "Malaria",    cascade = 3L, age = "0-59 months",  pop_type = "u5"),
    antimal_pub_nochw = list(recode = "KR",    category = "Malaria",    cascade = 3L, age = "0-59 months",  pop_type = "u5"),
    antimal_chw       = list(recode = "KR",    category = "Malaria",    cascade = 3L, age = "0-59 months",  pop_type = "u5"),
    antimal_priv      = list(recode = "KR",    category = "Malaria",    cascade = 3L, age = "0-59 months",  pop_type = "u5"),
    antimal_formal    = list(recode = "KR",    category = "Malaria",    cascade = 3L, age = "0-59 months",  pop_type = "u5"),
    antimal_pharm     = list(recode = "KR",    category = "Malaria",    cascade = 3L, age = "0-59 months",  pop_type = "u5"),
    antimal_priv_informal = list(recode = "KR", category = "Malaria",   cascade = 3L, age = "0-59 months",  pop_type = "u5"),
    antimal_form_pharm = list(recode = "KR",   category = "Malaria",    cascade = 3L, age = "0-59 months",  pop_type = "u5"),
    # Malaria diagnostic sub-indicators (from .act_mbg_dictionary)
    mal_dx_am         = list(recode = "KR",    category = "Malaria",    cascade = 2L, age = "0-59 months",  pop_type = "u5"),
    mal_dx_pub_am     = list(recode = "KR",    category = "Malaria",    cascade = 2L, age = "0-59 months",  pop_type = "u5"),
    mal_dx_pub_nochw_am = list(recode = "KR",  category = "Malaria",    cascade = 2L, age = "0-59 months",  pop_type = "u5"),
    mal_dx_chw_am     = list(recode = "KR",    category = "Malaria",    cascade = 2L, age = "0-59 months",  pop_type = "u5"),
    mal_dx_priv_am    = list(recode = "KR",    category = "Malaria",    cascade = 2L, age = "0-59 months",  pop_type = "u5"),
    mal_dx_priv_formal_am = list(recode = "KR", category = "Malaria",   cascade = 2L, age = "0-59 months",  pop_type = "u5"),
    mal_dx_pharm_am   = list(recode = "KR",    category = "Malaria",    cascade = 2L, age = "0-59 months",  pop_type = "u5"),
    mal_dx_priv_informal_am = list(recode = "KR", category = "Malaria", cascade = 2L, age = "0-59 months",  pop_type = "u5"),
    mal_dx_priv_form_pha_am = list(recode = "KR", category = "Malaria", cascade = 2L, age = "0-59 months",  pop_type = "u5"),
    febrile_rdt_pos   = list(recode = "KR+PR", category = "Malaria",    cascade = 2L, age = "0-59 months",  pop_type = "u5"),
    febrile_rdt_pos_act = list(recode = "KR+PR", category = "Malaria",  cascade = 4L, age = "0-59 months",  pop_type = "u5"),
    # PfPR -- PR module
    pfpr_rdt          = list(recode = "PR", category = "Malaria",       cascade = na_int, age = "6-59 months",  pop_type = "u5"),
    pfpr_mic          = list(recode = "PR", category = "Malaria",       cascade = na_int, age = "6-59 months",  pop_type = "u5"),
    pfpr_rdt_u5       = list(recode = "PR", category = "Malaria",       cascade = na_int, age = "6-59 months",  pop_type = "u5"),
    pfpr_mic_u5       = list(recode = "PR", category = "Malaria",       cascade = na_int, age = "6-59 months",  pop_type = "u5"),
    # ITN -- HR/PR module (aligned with DHS indicator codes)
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
    # Anemia -- PR module
    severe_anemia      = list(recode = "PR", category = "Nutrition",    cascade = na_int, age = "6-59 months",  pop_type = "u5"),
    anemia_any         = list(recode = "PR", category = "Nutrition",    cascade = na_int, age = "6-59 months",  pop_type = "u5"),
    anemia_moderate_plus = list(recode = "PR", category = "Nutrition",  cascade = na_int, age = "6-59 months",  pop_type = "u5"),
    anemia_severe      = list(recode = "PR", category = "Nutrition",    cascade = na_int, age = "6-59 months",  pop_type = "u5"),
    anemia_mild_only   = list(recode = "PR", category = "Nutrition",    cascade = na_int, age = "6-59 months",  pop_type = "u5"),
    anemia_moderate_only = list(recode = "PR", category = "Nutrition",  cascade = na_int, age = "6-59 months",  pop_type = "u5"),
    anemia_severe_only = list(recode = "PR", category = "Nutrition",    cascade = na_int, age = "6-59 months",  pop_type = "u5"),
    # Wealth -- HR module
    prop_poorest       = list(recode = "HR", category = "Wealth",       cascade = na_int, age = "all ages",     pop_type = "all"),
    prop_q1            = list(recode = "HR", category = "Wealth",       cascade = na_int, age = "all ages",     pop_type = "all"),
    prop_poorer        = list(recode = "HR", category = "Wealth",       cascade = na_int, age = "all ages",     pop_type = "all"),
    prop_q2            = list(recode = "HR", category = "Wealth",       cascade = na_int, age = "all ages",     pop_type = "all"),
    prop_middle        = list(recode = "HR", category = "Wealth",       cascade = na_int, age = "all ages",     pop_type = "all"),
    prop_q3            = list(recode = "HR", category = "Wealth",       cascade = na_int, age = "all ages",     pop_type = "all"),
    prop_richer        = list(recode = "HR", category = "Wealth",       cascade = na_int, age = "all ages",     pop_type = "all"),
    prop_q4            = list(recode = "HR", category = "Wealth",       cascade = na_int, age = "all ages",     pop_type = "all"),
    prop_richest       = list(recode = "HR", category = "Wealth",       cascade = na_int, age = "all ages",     pop_type = "all"),
    prop_q5            = list(recode = "HR", category = "Wealth",       cascade = na_int, age = "all ages",     pop_type = "all"),
    # ANC -- IR module
    anc_1plus         = list(recode = "IR", category = "Maternal health", cascade = na_int, age = "women 15-49", pop_type = "wra"),
    anc_2plus         = list(recode = "IR", category = "Maternal health", cascade = na_int, age = "women 15-49", pop_type = "wra"),
    anc_3plus         = list(recode = "IR", category = "Maternal health", cascade = na_int, age = "women 15-49", pop_type = "wra"),
    anc_4plus         = list(recode = "IR", category = "Maternal health", cascade = na_int, age = "women 15-49", pop_type = "wra"),
    anc_8plus         = list(recode = "IR", category = "Maternal health", cascade = na_int, age = "women 15-49", pop_type = "wra"),
    # IPTp -- IR module
    iptp_1plus        = list(recode = "IR", category = "Maternal health", cascade = na_int, age = "women 15-49", pop_type = "wra"),
    iptp_2plus        = list(recode = "IR", category = "Maternal health", cascade = na_int, age = "women 15-49", pop_type = "wra"),
    iptp_3plus        = list(recode = "IR", category = "Maternal health", cascade = na_int, age = "women 15-49", pop_type = "wra"),
    iptp_4plus        = list(recode = "IR", category = "Maternal health", cascade = na_int, age = "women 15-49", pop_type = "wra"),
    iptp_1only        = list(recode = "IR", category = "Maternal health", cascade = na_int, age = "women 15-49", pop_type = "wra"),
    iptp_2only        = list(recode = "IR", category = "Maternal health", cascade = na_int, age = "women 15-49", pop_type = "wra"),
    iptp_3only        = list(recode = "IR", category = "Maternal health", cascade = na_int, age = "women 15-49", pop_type = "wra"),
    # EPI -- KR module
    epi_bcg           = list(recode = "KR", category = "Immunization",  cascade = na_int, age = "12-23 months", pop_type = "1_2"),
    epi_dpt1          = list(recode = "KR", category = "Immunization",  cascade = na_int, age = "12-23 months", pop_type = "1_2"),
    epi_dpt2          = list(recode = "KR", category = "Immunization",  cascade = na_int, age = "12-23 months", pop_type = "1_2"),
    epi_dpt3          = list(recode = "KR", category = "Immunization",  cascade = na_int, age = "12-23 months", pop_type = "1_2"),
    epi_polio1        = list(recode = "KR", category = "Immunization",  cascade = na_int, age = "12-23 months", pop_type = "1_2"),
    epi_polio2        = list(recode = "KR", category = "Immunization",  cascade = na_int, age = "12-23 months", pop_type = "1_2"),
    epi_polio3        = list(recode = "KR", category = "Immunization",  cascade = na_int, age = "12-23 months", pop_type = "1_2"),
    epi_measles1      = list(recode = "KR", category = "Immunization",  cascade = na_int, age = "12-23 months", pop_type = "1_2"),
    epi_measles2      = list(recode = "KR", category = "Immunization",  cascade = na_int, age = "12-23 months", pop_type = "1_2"),
    epi_vita1         = list(recode = "KR", category = "Immunization",  cascade = na_int, age = "12-23 months", pop_type = "1_2"),
    epi_vita2         = list(recode = "KR", category = "Immunization",  cascade = na_int, age = "12-23 months", pop_type = "1_2"),
    epi_malaria       = list(recode = "KR", category = "Immunization",  cascade = na_int, age = "12-23 months", pop_type = "1_2"),
    epi_polio0        = list(recode = "KR", category = "Immunization",  cascade = na_int, age = "12-23 months", pop_type = "1_2"),
    epi_any           = list(recode = "KR", category = "Immunization",  cascade = na_int, age = "12-23 months", pop_type = "1_2"),
    epi_never_vaccinated = list(recode = "KR", category = "Immunization", cascade = na_int, age = "12-23 months", pop_type = "1_2"),
    epi_fully_vaccinated = list(recode = "KR", category = "Immunization", cascade = na_int, age = "12-23 months", pop_type = "1_2"),
    epi_penta1        = list(recode = "KR", category = "Immunization",  cascade = na_int, age = "12-23 months", pop_type = "1_2"),
    epi_penta2        = list(recode = "KR", category = "Immunization",  cascade = na_int, age = "12-23 months", pop_type = "1_2"),
    epi_penta3        = list(recode = "KR", category = "Immunization",  cascade = na_int, age = "12-23 months", pop_type = "1_2"),
    epi_pneumo1       = list(recode = "KR", category = "Immunization",  cascade = na_int, age = "12-23 months", pop_type = "1_2"),
    epi_pneumo2       = list(recode = "KR", category = "Immunization",  cascade = na_int, age = "12-23 months", pop_type = "1_2"),
    epi_pneumo3       = list(recode = "KR", category = "Immunization",  cascade = na_int, age = "12-23 months", pop_type = "1_2"),
    epi_rota1         = list(recode = "KR", category = "Immunization",  cascade = na_int, age = "12-23 months", pop_type = "1_2"),
    epi_rota2         = list(recode = "KR", category = "Immunization",  cascade = na_int, age = "12-23 months", pop_type = "1_2"),
    epi_rota3         = list(recode = "KR", category = "Immunization",  cascade = na_int, age = "12-23 months", pop_type = "1_2"),
    epi_ipv           = list(recode = "KR", category = "Immunization",  cascade = na_int, age = "12-23 months", pop_type = "1_2"),
    epi_hepb0         = list(recode = "KR", category = "Immunization",  cascade = na_int, age = "12-23 months", pop_type = "1_2"),
    epi_yellowfever   = list(recode = "KR", category = "Immunization",  cascade = na_int, age = "12-23 months", pop_type = "1_2"),
    # U5MR -- KR module (special unit)
    u5mr              = list(recode = "KR", category = "Mortality",     cascade = na_int, age = "0-59 months",  pop_type = "u5",
                             base_unit = "per 1000 live births"),
    # IRS -- HR module
    irs_coverage      = list(recode = "HR", category = "IRS",           cascade = na_int, age = "all ages",     pop_type = "all"),
    # SMC -- KR module
    smc_coverage      = list(recode = "KR", category = "SMC",           cascade = na_int, age = "0-59 months",  pop_type = "u5"),
    # Derived: Effective coverage of case management
    eff_cm_any        = list(recode = "KR", category = "Malaria",       cascade = na_int, age = "0-59 months",  pop_type = "u5"),
    eff_cm_public     = list(recode = "KR", category = "Malaria",       cascade = na_int, age = "0-59 months",  pop_type = "u5")
  )

  m <- meta[[ind]]

  # Runtime custom CSB indicators (KR / U5, like built-in CSB family).
  # Recognise both the three derived sub-codes (`<name>_dhis`, `_nondhis`,
  # `_untreat`) and the user-supplied meta name itself (e.g. "csb_eff"),
  # which is used as a category-level dispatch key in run_mbg_pipeline().
  if (is.null(m) && !is.null(custom_csb_indicator)) {
    custom_codes <- .custom_csb_indicator_names(custom_csb_indicator)
    custom_meta_name <- custom_csb_indicator$name
    if (ind %in% custom_codes || identical(ind, custom_meta_name)) {
      m <- list(
        recode = "KR", category = "Malaria", cascade = 1L,
        age = "0-59 months", pop_type = "u5"
      )
    }
  }

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
#' @param custom_csb_indicator Optional validated `custom_csb_indicator`
#'   spec list. When non-NULL, derived custom CSB codes resolve to
#'   pop_type `"u5"`.
#' @return Character scalar: `"u5"`, `"wra"`, or `"all"`.
#' @noRd
.mbg_indicator_pop_type <- function(ind, custom_csb_indicator = NULL) {
  # Category-level defaults (when dispatching a whole family)
  category_pop <- c(
    pfpr = "u5", itn = "all", irs = "all", anc = "wra",
    csb = "u5", act = "u5", anemia = "u5", iptp = "wra",
    epi = "1_2", u5mr = "u5", smc = "u5", fever = "u5",
    antimalarial = "u5", eff_cm = "u5"
  )

  if (ind %in% names(category_pop)) return(category_pop[[ind]])

  # Individual indicator lookup from meta
  .mbg_indicator_meta(ind, custom_csb_indicator = custom_csb_indicator)$pop_type
}


#' Unit Multiplier for an MBG Indicator
#'
#' Returns the scaling factor to convert MBG model predictions (0-1 scale)
#' to the indicator's natural units: 1000 for U5MR (per 1,000 live births),
#' 100 for everything else (percentage).
#'
#' @param ind Character indicator code.
#' @param custom_csb_indicator Optional validated `custom_csb_indicator`
#'   spec list. When non-NULL, derived custom CSB codes resolve via the
#'   same lookup as the built-in CSB family.
#' @return Numeric scalar: 1000 or 100.
#' @noRd
.mbg_indicator_multiplier <- function(ind, custom_csb_indicator = NULL) {
  meta <- .mbg_indicator_meta(ind, custom_csb_indicator = custom_csb_indicator)
  if (identical(meta$base_unit, "per 1000 live births")) 1000 else 100
}


#' MBG Indicator Labels and Descriptions
#'
#' Returns human-readable indicator name, numerator description,
#' denominator description, and denominator code for an MBG indicator.
#' Pulls detailed metadata from DHS \code{_conditions()} functions
#' (single source of truth) so MBG output matches DHS output exactly.
#'
#' @param ind Character indicator code (e.g., "act", "pfpr_rdt").
#' @return Named list with `indicator`, `numerator_description`,
#'   `denominator_description`, `denominator_code`.
#' @noRd
.mbg_indicator_label <- function(ind) {
  # Look up detailed metadata from DHS conditions functions
  detail <- .dhs_indicator_lookup()[[ind]]

  if (!is.null(detail)) {
    return(list(
      indicator             = detail$indicator_title %||% detail$indicator,
      numerator_description = detail$num_desc,
      denominator_description = detail$denom_desc,
      denominator_code      = detail$denom_code
    ))
  }

  # Fall back to meta-based generic labels for indicators without
  # a _conditions() entry (e.g. derived / combined indicators)
  meta <- .mbg_indicator_meta(ind)
  indicator_name <- gsub("_", " ", ind)

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


#' Eligibility Notes for an MBG Indicator
#'
#' Returns a brief explanation of the age restriction for an indicator,
#' describing the biological or methodological reason for the eligibility
#' criteria (e.g., maternal antibody protection for PfPR).
#'
#' @param ind Character indicator code.
#' @return Character scalar with eligibility note, or \code{NA_character_}.
#' @noRd
.mbg_eligibility_notes <- function(ind) {
  # Derived indicators get a methodology note
  if (grepl("^eff_cm_", ind)) {
    return("Derived indicator: product of CSB and ACT surfaces")
  }

  meta <- .mbg_indicator_meta(ind)
  # Only provide notes when the age range excludes younger ages,
  # explaining the biological/methodological reason for the restriction.
  # Ranges starting from 0 or covering all ages don't need a note.
  notes_map <- list(
    "6-59 months"  = "Excludes <6 months (residual maternal antibodies)",
    "12-23 months" = "Standard DHS vaccination assessment window"
  )
  notes_map[[meta$age]] %||% NA_character_
}


# Cache environment for DHS indicator detail lookup
.indicator_detail_env <- new.env(parent = emptyenv())

#' Build Indicator Detail Lookup from DHS Conditions
#'
#' Collects metadata from all DHS \code{_conditions()} functions and
#' indexes by \code{indicator_code}. Cached after first call.
#'
#' @return Named list keyed by indicator_code, each entry a conditions list.
#' @noRd
.dhs_indicator_lookup <- function() {
  if (!is.null(.indicator_detail_env$lookup)) {
    return(.indicator_detail_env$lookup)
  }

  # Collect all conditions from DHS calc functions
  cond_fns <- list(
    .act_conditions,
    .csb_conditions,
    .fever_conditions,
    .pfpr_conditions,
    .itn_conditions,
    .anc_conditions,
    .iptp_conditions,
    .epi_conditions,
    .severe_anemia_conditions,
    .antimalarial_conditions,
    .malaria_dx_conditions,
    .irs_conditions,
    .smc_conditions,
    .u5mr_conditions,
    .case_management_conditions
  )

  lookup <- list()
  for (fn in cond_fns) {
    conds <- tryCatch(fn(), error = function(e) list())
    for (cond in conds) {
      code <- cond$indicator_code
      if (!is.null(code) && !code %in% names(lookup)) {
        lookup[[code]] <- cond
      }
    }
  }

  .indicator_detail_env$lookup <- lookup
  lookup
}


#' Valid MBG Indicator Codes
#'
#' Returns the complete set of valid indicator codes for the MBG pipeline.
#' Built from `.mbg_indicator_meta()` keys (the single source of truth) plus
#' category-level shorthand names used for dispatch.
#'
#' @param custom_csb_indicator Optional validated `custom_csb_indicator`
#'   spec list. When non-NULL, the three derived custom CSB codes
#'   (`<name>_dhis`, `<name>_nondhis`, `<name>_untreat`) are appended to
#'   the result.
#' @return Character vector of all valid indicator names.
#' @noRd
.valid_mbg_indicators <- function(custom_csb_indicator = NULL) {
  # Category-level dispatch keys (run all sub-indicators for that family)
  categories <- c(
    "pfpr", "itn", "irs", "anc", "csb", "act", "anemia", "iptp", "epi",
    "u5mr", "smc", "fever", "antimalarial", "wealth",
    # Derived (auto-expands to dependencies)
    "eff_cm"
  )

  # Individual indicator codes -- aligned with _conditions() indicator_code
  # values and _mbg_dictionary() name values across all calc functions
  individual_codes <- c(
    # PfPR (PR module)
    "pfpr_rdt", "pfpr_mic",
    "pfpr_rdt_u5", "pfpr_mic_u5",
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
    # Wealth-stratified CSB (KR module)
    "csb_q1", "csb_q2", "csb_q3", "csb_q4", "csb_q5",
    # Antimalarial (KR module)
    "antimalarial", "antimalarial_public",
    # ACT (KR module) -- from .act_mbg_dictionary()
    "act", "act_care_seek", "act_am", "act_any_tx_am",
    "act_trained_am", "act_pub_am", "act_pub_nochw_am", "act_chw_am",
    "act_priv_am", "act_priv_formal_am", "act_priv_pharm_am",
    "act_priv_informal_am", "act_priv_form_pha_am",
    "act_pub", "act_tested",
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
    # Wealth (HR module)
    "prop_poorest", "prop_q1", "prop_poorer", "prop_q2",
    "prop_middle", "prop_q3", "prop_richer", "prop_q4",
    "prop_richest", "prop_q5",
    # Derived
    "eff_cm_any", "eff_cm_public"
  )

  custom_codes <- if (!is.null(custom_csb_indicator)) {
    .custom_csb_indicator_names(custom_csb_indicator)
  } else {
    character(0)
  }

  # Also accept the user-supplied meta name (e.g. "csb_eff") as a valid
  # category-level dispatch key. This lets callers pass a single token to
  # run all three derived sub-indicators (`<name>_dhis`, `<name>_nondhis`,
  # `<name>_untreat`) instead of enumerating them by hand. The dispatcher
  # in .process_indicator_category() already detects this case via
  # is_custom_csb_meta and runs the full partition.
  custom_meta_name <- if (!is.null(custom_csb_indicator)) {
    custom_csb_indicator$name
  } else {
    character(0)
  }

  unique(c(categories, individual_codes, custom_codes, custom_meta_name))
}


#' Build temporary dictionary rows for runtime custom CSB indicators
#'
#' Builds a tibble carrying full per-indicator metadata for each of the
#' three derived custom CSB codes (`<name>_dhis`, `<name>_nondhis`,
#' `<name>_untreat`). The pipeline binds these rows to the static
#' \code{dhs_dictionary()} before joining indicator metadata onto the
#' final MBG output, so custom indicators are not exposed in the public
#' dictionary API but still get fully labeled in pipeline outputs.
#'
#' Each row embeds the actual user-supplied DHS variable list
#' (e.g. \code{h32a, h32b, h32c, h32d, h32e, h32f, h32i, h32j} for
#' \code{_dhis}) inside both \code{numerator_description} and
#' \code{dhs_variables} so the Excel output is fully traceable: a
#' downstream reader can see exactly which `h32*` codes contributed to
#' each numerator. The denominator description matches the built-in CSB
#' family ("Children 0-59 months with fever, alive (h22==1, b5==1)").
#'
#' @param custom_csb_indicator A validated user spec.
#' @return A tibble with one row per derived custom CSB code, or an empty
#'   tibble (with the expected columns) when `custom_csb_indicator` is NULL.
#' @noRd
.custom_csb_dictionary_rows <- function(custom_csb_indicator) {
  empty <- tibble::tibble(
    indicator_code          = character(0),
    indicator               = character(0),
    indicator_title         = character(0),
    numerator_code          = character(0),
    numerator_description   = character(0),
    denominator_code        = character(0),
    denominator_description = character(0),
    domain                  = character(0),
    observation_unit        = character(0),
    dhs_recode              = character(0),
    calc_function           = character(0),
    eligibility             = character(0),
    dhs_variables           = character(0),
    notes                   = character(0)
  )
  if (is.null(custom_csb_indicator)) return(empty)

  prefix <- custom_csb_indicator$name

  # Pull the actual user-supplied variable lists. These come from the
  # validated spec so they are guaranteed to be character vectors.
  dhis_vars    <- custom_csb_indicator$dhis_locs    %||% character(0)
  nondhis_vars <- custom_csb_indicator$nondhis_locs %||% character(0)
  untreat_vars <- custom_csb_indicator$untreat_locs %||% character(0)

  # If the user passed labels rather than h32 codes, the spec validator
  # accepts both; we keep whatever they passed verbatim so the user can
  # always trace back to their input. Joined with commas for the
  # `dhs_variables` column (matches the built-in dictionary style).
  fmt <- function(v) {
    if (length(v) == 0) "(none specified)" else paste(v, collapse = ", ")
  }
  dhis_str    <- fmt(dhis_vars)
  nondhis_str <- fmt(nondhis_vars)
  untreat_str <- fmt(untreat_vars)

  # Standard CSB-family denominator (matches built-in csb_* indicators).
  denom_desc <- "Children 0-59 months with fever (h22==1), alive (b5==1)"
  denom_code <- "u5_fever"
  elig       <- denom_desc

  ind_codes <- c(
    paste0(prefix, "_dhis"),
    paste0(prefix, "_nondhis"),
    paste0(prefix, "_untreat")
  )

  ind_titles <- c(
    paste0(
      "Custom care seeking (DHIS sources) [", prefix,
      "] among under 5 with fever"
    ),
    paste0(
      "Custom care seeking (non-DHIS sources) [", prefix,
      "] among under 5 with fever"
    ),
    paste0(
      "Custom no/untreated care seeking [", prefix,
      "] among under 5 with fever"
    )
  )

  # Numerator descriptions embed the exact user-supplied variables so a
  # downstream consumer can audit which `h32*` slots fed each numerator.
  num_descs <- c(
    paste0(
      "Children with fever who sought care at user-listed DHIS sources [",
      dhis_str, "]"
    ),
    paste0(
      "Children with fever who sought care at user-listed non-DHIS ",
      "sources [", nondhis_str, "] (and not at any DHIS source)"
    ),
    paste0(
      "Children with fever who did not seek care at any user-listed ",
      "DHIS or non-DHIS source [untreat: ", untreat_str, "]"
    )
  )

  # `dhs_variables` carries the full per-row variable list so the
  # exported dictionary tab in Excel records exactly which DHS variables
  # produced each derived indicator (full traceability).
  dhs_var_strs <- c(
    paste0("h22, b5, hw1; numerator vars: ", dhis_str),
    paste0("h22, b5, hw1; numerator vars: ", nondhis_str),
    paste0("h22, b5, hw1; numerator vars: ", untreat_str)
  )

  tibble::tibble(
    indicator_code          = ind_codes,
    indicator               = ind_titles,
    indicator_title         = ind_titles,
    numerator_code          = paste0("n_", ind_codes),
    numerator_description   = num_descs,
    denominator_code        = rep(denom_code, 3L),
    denominator_description = rep(denom_desc, 3L),
    domain                  = rep("CSB (custom)", 3L),
    observation_unit        = rep("Person", 3L),
    dhs_recode              = rep("KR", 3L),
    calc_function           = rep("calc_csb_custom_mbg", 3L),
    eligibility             = rep(elig, 3L),
    dhs_variables           = dhs_var_strs,
    notes                   = rep(
      paste0(
        "Runtime user-defined CSB partition (custom_csb_indicator); ",
        "numerator may be NA when no respondents reported the listed ",
        "sources for a given admin unit."
      ),
      3L
    )
  )
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
