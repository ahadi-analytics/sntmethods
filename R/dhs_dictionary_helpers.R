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
    csb_chw           = list(recode = "KR", category = "Malaria",       cascade = 1L, age = "0-59 months",  pop_type = "u5"),
    csb_pharmacy      = list(recode = "KR", category = "Malaria",       cascade = 1L, age = "0-59 months",  pop_type = "u5"),
    csb_priv_formal   = list(recode = "KR", category = "Malaria",       cascade = 1L, age = "0-59 months",  pop_type = "u5"),
    csb_priv_informal = list(recode = "KR", category = "Malaria",       cascade = 1L, age = "0-59 months",  pop_type = "u5"),
    csb_priv_form_pha = list(recode = "KR", category = "Malaria",       cascade = 1L, age = "0-59 months",  pop_type = "u5"),
    csb_pub_nochw     = list(recode = "KR", category = "Malaria",       cascade = 1L, age = "0-59 months",  pop_type = "u5"),
    malaria_dx        = list(recode = "KR", category = "Malaria",       cascade = 2L, age = "0-59 months",  pop_type = "u5"),
    antimalarial      = list(recode = "KR", category = "Malaria",       cascade = 3L, age = "0-59 months",  pop_type = "u5"),
    antimalarial_public = list(recode = "KR", category = "Malaria",     cascade = 3L, age = "0-59 months",  pop_type = "u5"),
    act               = list(recode = "KR",    category = "Malaria",    cascade = 4L, age = "0-59 months",  pop_type = "u5"),
    act_public        = list(recode = "KR",    category = "Malaria",    cascade = 4L, age = "0-59 months",  pop_type = "u5"),
    act_tested        = list(recode = "KR",    category = "Malaria",    cascade = 4L, age = "0-59 months",  pop_type = "u5"),
    act_care_seek     = list(recode = "KR",    category = "Malaria",    cascade = 4L, age = "0-59 months",  pop_type = "u5"),
    act_antimal       = list(recode = "KR",    category = "Malaria",    cascade = 4L, age = "0-59 months",  pop_type = "u5"),
    act_any_tx        = list(recode = "KR",    category = "Malaria",    cascade = 4L, age = "0-59 months",  pop_type = "u5"),
    act_trained       = list(recode = "KR",    category = "Malaria",    cascade = 4L, age = "0-59 months",  pop_type = "u5"),
    act_pub           = list(recode = "KR",    category = "Malaria",    cascade = 4L, age = "0-59 months",  pop_type = "u5"),
    act_pub_nochw     = list(recode = "KR",    category = "Malaria",    cascade = 4L, age = "0-59 months",  pop_type = "u5"),
    act_chw           = list(recode = "KR",    category = "Malaria",    cascade = 4L, age = "0-59 months",  pop_type = "u5"),
    act_priv          = list(recode = "KR",    category = "Malaria",    cascade = 4L, age = "0-59 months",  pop_type = "u5"),
    act_priv_formal   = list(recode = "KR",    category = "Malaria",    cascade = 4L, age = "0-59 months",  pop_type = "u5"),
    act_priv_pharm    = list(recode = "KR",    category = "Malaria",    cascade = 4L, age = "0-59 months",  pop_type = "u5"),
    act_priv_informal = list(recode = "KR",    category = "Malaria",    cascade = 4L, age = "0-59 months",  pop_type = "u5"),
    act_priv_form_pha = list(recode = "KR",    category = "Malaria",    cascade = 4L, age = "0-59 months",  pop_type = "u5"),
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
    # PfPR — PR module
    pfpr_rdt          = list(recode = "PR", category = "Malaria",       cascade = na_int, age = "6-59 months",  pop_type = "u5"),
    pfpr_mic          = list(recode = "PR", category = "Malaria",       cascade = na_int, age = "6-59 months",  pop_type = "u5"),
    pfpr_rdt_u5       = list(recode = "PR", category = "Malaria",       cascade = na_int, age = "6-59 months",  pop_type = "u5"),
    pfpr_mic_u5       = list(recode = "PR", category = "Malaria",       cascade = na_int, age = "6-59 months",  pop_type = "u5"),
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
    anc_2plus         = list(recode = "IR", category = "Maternal health", cascade = na_int, age = "women 15-49", pop_type = "wra"),
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
    epi = "1_2", u5mr = "u5", smc = "u5", fever = "u5",
    antimalarial = "u5", eff_cm = "u5"
  )

  if (ind %in% names(category_pop)) return(category_pop[[ind]])

  # Individual indicator lookup from meta
  .mbg_indicator_meta(ind)$pop_type
}


#' Unit Multiplier for an MBG Indicator
#'
#' Returns the scaling factor to convert MBG model predictions (0-1 scale)
#' to the indicator's natural units: 1000 for U5MR (per 1,000 live births),
#' 100 for everything else (percentage).
#'
#' @param ind Character indicator code.
#' @return Numeric scalar: 1000 or 100.
#' @noRd
.mbg_indicator_multiplier <- function(ind) {
  meta <- .mbg_indicator_meta(ind)
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
  meta <- .mbg_indicator_meta(ind)
  notes_map <- list(
    "6-59 months"  = "Excludes <6 months (residual maternal antibodies)",
    "12-23 months" = "Standard DHS vaccination assessment window",
    "0-59 months"  = "All children under 5 years",
    "women 15-49"  = "Women of reproductive age with recent live birth",
    "all ages"     = "All de facto household members",
    "5-10 years"   = "De facto children aged 5-9 years",
    "10-20 years"  = "De facto population aged 10-19 years",
    "20+ years"    = "De facto population aged 20 years and above"
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
