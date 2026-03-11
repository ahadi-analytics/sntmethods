#' Calculate EPI Coverage from DHS Data
#'
#' Estimates vaccination coverage using survey-weighted methods from
#' DHS Children's Recode data.
#'
#' @param dhs_kr DHS Children's Recode (KR) dataset.
#' @param indicators Character vector of vaccines to calculate. Options:
#'   "bcg", "dpt1", "dpt2", "dpt3", "polio0", "polio1", "polio2", "polio3",
#'   "measles1", "measles2", "vita1", "vita2", "malaria",
#'   "any", "never_vaccinated", "fully_vaccinated".
#'   Default: c("bcg", "dpt3", "measles1", "fully_vaccinated").
#' @param age_min_months Minimum age in months (default: 12).
#' @param age_max_months Maximum age in months (default: 23).
#' @param survey_vars Named list mapping DHS variable names.
#' @param region_var Optional column name to use as grouping variable.
#' @param gps_data Optional DHS GPS dataset.
#' @param gps_vars Named list for GPS variables.
#' @param shapefile Optional sf object with administrative boundaries.
#' @param admin_level Character vector of admin columns.
#' @param join_nearest Logical.
#'
#' @return Tibble with EPI estimates. For each vaccine: `dhs_epi_<vaccine>`,
#'   `dhs_epi_<vaccine>_low`, `dhs_epi_<vaccine>_upp`. Plus `dhs_n_epi_eligible`.
#'
#' @seealso [calc_epi_mbg()] for cluster-level MBG inputs
#' @export
calc_epi_dhs_core <- function(
  dhs_kr,
  indicators = c("bcg", "dpt3", "measles1", "fully_vaccinated"),
  age_min_months = 12,
  age_max_months = 23,
  survey_vars = list(
    cluster = "v021",
    weight = "v005",
    stratum = "v022",
    age = "hw1",
    bcg = "h2",
    dpt1 = "h3", dpt2 = "h4", dpt3 = "h5",
    polio0 = "h0",
    polio1 = "h6", polio2 = "h7", polio3 = "h8",
    measles1 = "h9", measles2 = "h9a",
    vita1 = "h33", vita2 = "h33a",
    malaria = "h68",
    penta1 = "h51", penta2 = "h52", penta3 = "h53",
    pneumo1 = "h54", pneumo2 = "h55", pneumo3 = "h56",
    rota1 = "h57", rota2 = "h58", rota3 = "h59",
    ipv = "h60", hepb0 = "h50", yellowfever = "h61",
    any = "h10"
  ),
  region_var = NULL,
  gps_data = NULL,
  gps_vars = list(
    cluster = "DHSCLUST",
    lat = "LATNUM",
    lon = "LONGNUM"
  ),
  shapefile = NULL,
  admin_level = NULL,
  join_nearest = TRUE
) {
  # Prepare data using shared helper
  kr <- .prepare_epi_data(
    dhs_kr = dhs_kr,
    survey_vars = survey_vars,
    age_min_months = age_min_months,
    age_max_months = age_max_months,
    include_survey_vars = TRUE
  )

  # Determine grouping
  class_var <- NULL
  if (!is.null(region_var)) {
    if (!region_var %in% names(dhs_kr)) {
      cli::cli_abort("Column {.var {region_var}} not found in `dhs_kr`.")
    }
    class_var <- region_var
  } else if ("v024" %in% names(kr)) {
    class_var <- "v024"
    cli::cli_alert_info("Using v024 (region) as grouping variable")
  }

  # Set up survey design
  use_strata <- dplyr::n_distinct(kr$stratum_id) > 1
  if (use_strata) {
    survey_options <- options(survey.lonely.psu = "adjust")
    on.exit(options(survey_options), add = TRUE)
    des <- survey::svydesign(
      ids = ~cluster_id, strata = ~stratum_id,
      weights = ~survey_weight, data = kr, nest = TRUE
    )
  } else {
    des <- survey::svydesign(
      ids = ~cluster_id, weights = ~survey_weight,
      data = kr, nest = TRUE
    )
  }

  # Build indicator formula from available vax_ columns
  vax_cols <- paste0("vax_", indicators)
  available_vax <- vax_cols[vax_cols %in% names(kr)]

  if (length(available_vax) == 0) {
    cli::cli_abort("No requested vaccine indicators found in prepared data")
  }

  indicator_formula <- stats::as.formula(
    paste("~", paste(available_vax, collapse = " + "))
  )

  # Calculate
  if (!is.null(class_var)) {
    group_formula <- stats::as.formula(paste("~", class_var))
    epi_results <- tryCatch({
      survey::svyby(
        indicator_formula, by = group_formula, design = des,
        FUN = survey::svymean, vartype = "ci",
        na.rm = TRUE, keep.names = FALSE
      ) |> tibble::as_tibble()
    }, error = function(e) {
      if (grepl("has only one PSU", e$message)) {
        des_ns <- survey::svydesign(
          ids = ~cluster_id, weights = ~survey_weight,
          data = kr, nest = TRUE
        )
        survey::svyby(
          indicator_formula, by = group_formula, design = des_ns,
          FUN = survey::svymean, vartype = "ci",
          na.rm = TRUE, keep.names = FALSE
        ) |> tibble::as_tibble()
      } else stop(e)
    })
  } else {
    epi_means <- survey::svymean(indicator_formula, design = des, na.rm = TRUE)
    epi_ci <- stats::confint(epi_means)
    epi_results <- tibble::tibble(level = "National")
    for (v in available_vax) {
      epi_results[[v]] <- as.numeric(epi_means[v])
      epi_results[[paste0("ci_l.", v)]] <- epi_ci[v, 1]
      epi_results[[paste0("ci_u.", v)]] <- epi_ci[v, 2]
    }
  }

  # Rename vax_ columns to dhs_epi_ format
  for (ind in indicators) {
    vax_col <- paste0("vax_", ind)
    if (vax_col %in% names(epi_results)) {
      epi_results <- epi_results |>
        dplyr::rename(
          !!paste0("dhs_epi_", ind) := !!vax_col,
          !!paste0("dhs_epi_", ind, "_low") := !!paste0("ci_l.", vax_col),
          !!paste0("dhs_epi_", ind, "_upp") := !!paste0("ci_u.", vax_col)
        )
    }
  }

  # Sample sizes
  if (!is.null(class_var)) {
    sample_sizes <- kr |>
      dplyr::group_by(.data[[class_var]]) |>
      dplyr::summarise(dhs_n_epi_eligible = dplyr::n(), .groups = "drop")
    epi_results <- epi_results |>
      dplyr::left_join(sample_sizes, by = class_var)
  } else {
    epi_results$dhs_n_epi_eligible <- nrow(kr)
  }

  # Format
  epi_cols <- names(epi_results)[grepl("^dhs_epi_", names(epi_results))]
  epi_results <- epi_results |>
    dplyr::mutate(
      dplyr::across(dplyr::all_of(epi_cols), ~ round(.x, 2)),
      dplyr::across(dplyr::matches("_low$"), ~ pmax(0, .)),
      dplyr::across(dplyr::matches("_upp$"), ~ pmin(1, .)),
      dhs_n_epi_eligible = as.integer(dhs_n_epi_eligible)
    ) |>
    dplyr::select(-dplyr::any_of(c("level")))

  tibble::as_tibble(epi_results)
}

#' Calculate EPI Coverage from DHS Data (Standardized)
#'
#' Estimates vaccination coverage using survey-weighted methods from
#' DHS Children's Recode data. Returns standardized long-format output as
#' `list(adm0, adm1)`.
#'
#' @param dhs_kr DHS Children's Recode (KR) dataset.
#' @param indicators Character vector of vaccines to calculate, or NULL for all
#'   available. Options include: "bcg", "dpt1", "dpt2", "dpt3", "polio0",
#'   "polio1", "polio2", "polio3", "measles1", "measles2", "vita1", "vita2",
#'   "malaria", "penta1", "penta2", "penta3", "pneumo1", "pneumo2", "pneumo3",
#'   "rota1", "rota2", "rota3", "ipv", "hepb0", "yellowfever",
#'   "any", "never_vaccinated", "fully_vaccinated".
#' @param age_min_months Minimum age in months (default: 12).
#' @param age_max_months Maximum age in months (default: 23).
#' @param survey_vars Named list mapping DHS variable names.
#' @param region_var Optional column name to use as grouping variable.
#' @param ci_method CI method for svyciprop. Default: "logit".
#'
#' @return Named list with `adm0` (national) and optionally `adm1` (regional)
#'   tibbles in standardized long format.
#'
#' @seealso [epi_dictionary()] for indicator definitions,
#'   [calc_epi_dhs_core()] for backward-compatible wide output
#' @export
calc_epi_dhs <- function(
  dhs_kr,
  indicators = NULL,
  age_min_months = 12,
  age_max_months = 23,
  survey_vars = list(
    cluster = "v021",
    weight = "v005",
    stratum = "v022",
    age = "hw1",
    bcg = "h2",
    dpt1 = "h3", dpt2 = "h4", dpt3 = "h5",
    polio0 = "h0",
    polio1 = "h6", polio2 = "h7", polio3 = "h8",
    measles1 = "h9", measles2 = "h9a",
    vita1 = "h33", vita2 = "h33a",
    malaria = "h68",
    penta1 = "h51", penta2 = "h52", penta3 = "h53",
    pneumo1 = "h54", pneumo2 = "h55", pneumo3 = "h56",
    rota1 = "h57", rota2 = "h58", rota3 = "h59",
    ipv = "h60", hepb0 = "h50", yellowfever = "h61",
    any = "h10"
  ),
  region_var = NULL,
  ci_method = "logit"
) {
  # ---- 1. Extract survey metadata ----
  survey_meta <- .extract_survey_meta(dhs_kr)

  # ---- 2. Prepare data ----
  kr <- .prepare_epi_data(
    dhs_kr = dhs_kr,
    survey_vars = survey_vars,
    age_min_months = age_min_months,
    age_max_months = age_max_months,
    include_survey_vars = TRUE
  )

  # ---- 3. Resolve region labels ----
  group_var <- NULL
  geo_src <- NA_character_

  if (!is.null(region_var)) {
    if (!region_var %in% names(dhs_kr)) {
      cli::cli_abort("Column {.var {region_var}} not found in `dhs_kr`.")
    }
    kr$region <- .resolve_region_labels(
      dhs_kr[[region_var]], region_var
    )
    # Map to age-filtered subset if lengths differ
    if (nrow(kr) != nrow(dhs_kr)) {
      resolved_all <- .resolve_region_labels(
        dhs_kr[[region_var]], region_var
      )
      raw_all <- as.character(
        as.vector(haven::zap_labels(dhs_kr[[region_var]]))
      )
      lookup <- stats::setNames(resolved_all, raw_all)
      kr_raw <- as.character(kr[[region_var]])
      kr$region <- unname(lookup[kr_raw])
    }
    group_var <- "region"
    geo_src <- "survey"
  }

  # ---- 4. Build conditions for available vaccines ----
  all_conditions <- .epi_conditions()

  # Detect which vax_ columns are available in prepared data
  available_vax <- names(kr)[grepl("^vax_", names(kr))]
  available_vax_names <- sub("^vax_", "", available_vax)

  # Filter conditions to available vaccines
  conditions <- all_conditions[vapply(
    all_conditions,
    function(cond) cond$outcome_var %in% available_vax,
    logical(1)
  )]

  # Further filter by user-requested indicators if provided

  if (!is.null(indicators)) {
    requested_codes <- paste0("epi_", indicators)
    conditions <- conditions[vapply(
      conditions,
      function(cond) cond$indicator_code %in% requested_codes,
      logical(1)
    )]
  }

  if (length(conditions) == 0) {
    cli::cli_abort("No requested vaccine indicators found in prepared data.")
  }

  # ---- 5. Compute national results ----
  national_results <- purrr::map_dfr(conditions, function(cond) {
    .compute_dhs_indicator_generic(
      data = kr,
      condition = cond,
      group_var = NULL,
      ci_method = ci_method
    )
  })

  # ---- 6. Compute regional results ----
  regional_results <- tibble::tibble()
  if (!is.null(group_var)) {
    regional_results <- purrr::map_dfr(conditions, function(cond) {
      .compute_dhs_indicator_generic(
        data = kr,
        condition = cond,
        group_var = group_var,
        subnational_level = "adm1",
        ci_method = ci_method
      )
    })
    # Keep only regional rows
    regional_results <- regional_results |>
      dplyr::filter(level != "adm0")
  }

  # ---- 7. Assemble output ----
  .assemble_dhs_output(
    national_results = national_results,
    regional_results = regional_results,
    survey_meta = survey_meta,
    geo_source = geo_src,
    admin_col = "adm1"
  )
}


# =============================================================================
# Indicator conditions
# =============================================================================

#' Internal: EPI indicator conditions
#'
#' Defines conditions for all EPI vaccine indicators. Each condition specifies
#' the outcome variable (vax_* column created by .prepare_epi_data()), the
#' indicator code, and metadata descriptions. No filter_expr is needed because
#' the age filtering is done during data preparation.
#'
#' @return List of named lists, each with indicator specification.
#' @noRd
.epi_conditions <- function() {
  denom <- "Children 12-23 months"
  list(
    list(
      indicator       = "EPI_BCG",
      indicator_code  = "epi_bcg",
      indicator_title = "BCG vaccination coverage",
      denom_code      = "epi_eligible",
      filter_expr     = NULL,
      outcome_var     = "vax_bcg",
      num_desc        = "Children 12-23 months vaccinated with BCG",
      denom_desc      = denom
    ),
    list(
      indicator       = "EPI_DPT1",
      indicator_code  = "epi_dpt1",
      indicator_title = "DPT/Penta dose 1 coverage",
      denom_code      = "epi_eligible",
      filter_expr     = NULL,
      outcome_var     = "vax_dpt1",
      num_desc        = "Children 12-23 months with DPT/Penta dose 1",
      denom_desc      = denom
    ),
    list(
      indicator       = "EPI_DPT2",
      indicator_code  = "epi_dpt2",
      indicator_title = "DPT/Penta dose 2 coverage",
      denom_code      = "epi_eligible",
      filter_expr     = NULL,
      outcome_var     = "vax_dpt2",
      num_desc        = "Children 12-23 months with DPT/Penta dose 2",
      denom_desc      = denom
    ),
    list(
      indicator       = "EPI_DPT3",
      indicator_code  = "epi_dpt3",
      indicator_title = "DPT/Penta dose 3 coverage",
      denom_code      = "epi_eligible",
      filter_expr     = NULL,
      outcome_var     = "vax_dpt3",
      num_desc        = "Children 12-23 months with DPT/Penta dose 3",
      denom_desc      = denom
    ),
    list(
      indicator       = "EPI_POLIO0",
      indicator_code  = "epi_polio0",
      indicator_title = "OPV at birth (dose 0) coverage",
      denom_code      = "epi_eligible",
      filter_expr     = NULL,
      outcome_var     = "vax_polio0",
      num_desc        = "Children 12-23 months with OPV birth dose",
      denom_desc      = denom
    ),
    list(
      indicator       = "EPI_POLIO1",
      indicator_code  = "epi_polio1",
      indicator_title = "Polio dose 1 coverage",
      denom_code      = "epi_eligible",
      filter_expr     = NULL,
      outcome_var     = "vax_polio1",
      num_desc        = "Children 12-23 months with Polio dose 1",
      denom_desc      = denom
    ),
    list(
      indicator       = "EPI_POLIO2",
      indicator_code  = "epi_polio2",
      indicator_title = "Polio dose 2 coverage",
      denom_code      = "epi_eligible",
      filter_expr     = NULL,
      outcome_var     = "vax_polio2",
      num_desc        = "Children 12-23 months with Polio dose 2",
      denom_desc      = denom
    ),
    list(
      indicator       = "EPI_POLIO3",
      indicator_code  = "epi_polio3",
      indicator_title = "Polio dose 3 coverage",
      denom_code      = "epi_eligible",
      filter_expr     = NULL,
      outcome_var     = "vax_polio3",
      num_desc        = "Children 12-23 months with Polio dose 3",
      denom_desc      = denom
    ),
    list(
      indicator       = "EPI_MEASLES1",
      indicator_code  = "epi_measles1",
      indicator_title = "Measles dose 1 coverage",
      denom_code      = "epi_eligible",
      filter_expr     = NULL,
      outcome_var     = "vax_measles1",
      num_desc        = "Children 12-23 months with Measles dose 1",
      denom_desc      = denom
    ),
    list(
      indicator       = "EPI_MEASLES2",
      indicator_code  = "epi_measles2",
      indicator_title = "Measles dose 2 coverage",
      denom_code      = "epi_eligible",
      filter_expr     = NULL,
      outcome_var     = "vax_measles2",
      num_desc        = "Children 12-23 months with Measles dose 2",
      denom_desc      = denom
    ),
    list(
      indicator       = "EPI_VITA1",
      indicator_code  = "epi_vita1",
      indicator_title = "Vitamin A dose 1 coverage",
      denom_code      = "epi_eligible",
      filter_expr     = NULL,
      outcome_var     = "vax_vita1",
      num_desc        = "Children 12-23 months with Vitamin A dose 1",
      denom_desc      = denom
    ),
    list(
      indicator       = "EPI_VITA2",
      indicator_code  = "epi_vita2",
      indicator_title = "Vitamin A dose 2 coverage",
      denom_code      = "epi_eligible",
      filter_expr     = NULL,
      outcome_var     = "vax_vita2",
      num_desc        = "Children 12-23 months with Vitamin A dose 2",
      denom_desc      = denom
    ),
    list(
      indicator       = "EPI_MALARIA",
      indicator_code  = "epi_malaria",
      indicator_title = "Malaria vaccine (RTS,S/R21) coverage",
      denom_code      = "epi_eligible",
      filter_expr     = NULL,
      outcome_var     = "vax_malaria",
      num_desc        = "Children 12-23 months with malaria vaccine",
      denom_desc      = denom
    ),
    list(
      indicator       = "EPI_PENTA1",
      indicator_code  = "epi_penta1",
      indicator_title = "Pentavalent dose 1 coverage",
      denom_code      = "epi_eligible",
      filter_expr     = NULL,
      outcome_var     = "vax_penta1",
      num_desc        = "Children 12-23 months with Pentavalent dose 1",
      denom_desc      = denom
    ),
    list(
      indicator       = "EPI_PENTA2",
      indicator_code  = "epi_penta2",
      indicator_title = "Pentavalent dose 2 coverage",
      denom_code      = "epi_eligible",
      filter_expr     = NULL,
      outcome_var     = "vax_penta2",
      num_desc        = "Children 12-23 months with Pentavalent dose 2",
      denom_desc      = denom
    ),
    list(
      indicator       = "EPI_PENTA3",
      indicator_code  = "epi_penta3",
      indicator_title = "Pentavalent dose 3 coverage",
      denom_code      = "epi_eligible",
      filter_expr     = NULL,
      outcome_var     = "vax_penta3",
      num_desc        = "Children 12-23 months with Pentavalent dose 3",
      denom_desc      = denom
    ),
    list(
      indicator       = "EPI_PNEUMO1",
      indicator_code  = "epi_pneumo1",
      indicator_title = "Pneumococcal dose 1 coverage",
      denom_code      = "epi_eligible",
      filter_expr     = NULL,
      outcome_var     = "vax_pneumo1",
      num_desc        = "Children 12-23 months with Pneumococcal dose 1",
      denom_desc      = denom
    ),
    list(
      indicator       = "EPI_PNEUMO2",
      indicator_code  = "epi_pneumo2",
      indicator_title = "Pneumococcal dose 2 coverage",
      denom_code      = "epi_eligible",
      filter_expr     = NULL,
      outcome_var     = "vax_pneumo2",
      num_desc        = "Children 12-23 months with Pneumococcal dose 2",
      denom_desc      = denom
    ),
    list(
      indicator       = "EPI_PNEUMO3",
      indicator_code  = "epi_pneumo3",
      indicator_title = "Pneumococcal dose 3 coverage",
      denom_code      = "epi_eligible",
      filter_expr     = NULL,
      outcome_var     = "vax_pneumo3",
      num_desc        = "Children 12-23 months with Pneumococcal dose 3",
      denom_desc      = denom
    ),
    list(
      indicator       = "EPI_ROTA1",
      indicator_code  = "epi_rota1",
      indicator_title = "Rotavirus dose 1 coverage",
      denom_code      = "epi_eligible",
      filter_expr     = NULL,
      outcome_var     = "vax_rota1",
      num_desc        = "Children 12-23 months with Rotavirus dose 1",
      denom_desc      = denom
    ),
    list(
      indicator       = "EPI_ROTA2",
      indicator_code  = "epi_rota2",
      indicator_title = "Rotavirus dose 2 coverage",
      denom_code      = "epi_eligible",
      filter_expr     = NULL,
      outcome_var     = "vax_rota2",
      num_desc        = "Children 12-23 months with Rotavirus dose 2",
      denom_desc      = denom
    ),
    list(
      indicator       = "EPI_ROTA3",
      indicator_code  = "epi_rota3",
      indicator_title = "Rotavirus dose 3 coverage",
      denom_code      = "epi_eligible",
      filter_expr     = NULL,
      outcome_var     = "vax_rota3",
      num_desc        = "Children 12-23 months with Rotavirus dose 3",
      denom_desc      = denom
    ),
    list(
      indicator       = "EPI_IPV",
      indicator_code  = "epi_ipv",
      indicator_title = "IPV coverage",
      denom_code      = "epi_eligible",
      filter_expr     = NULL,
      outcome_var     = "vax_ipv",
      num_desc        = "Children 12-23 months with IPV",
      denom_desc      = denom
    ),
    list(
      indicator       = "EPI_HEPB0",
      indicator_code  = "epi_hepb0",
      indicator_title = "Hepatitis B birth dose coverage",
      denom_code      = "epi_eligible",
      filter_expr     = NULL,
      outcome_var     = "vax_hepb0",
      num_desc        = "Children 12-23 months with Hepatitis B birth dose",
      denom_desc      = denom
    ),
    list(
      indicator       = "EPI_YELLOWFEVER",
      indicator_code  = "epi_yellowfever",
      indicator_title = "Yellow Fever vaccine coverage",
      denom_code      = "epi_eligible",
      filter_expr     = NULL,
      outcome_var     = "vax_yellowfever",
      num_desc        = "Children 12-23 months with Yellow Fever vaccine",
      denom_desc      = denom
    ),
    list(
      indicator       = "EPI_ANY",
      indicator_code  = "epi_any",
      indicator_title = "Received any vaccination",
      denom_code      = "epi_eligible",
      filter_expr     = NULL,
      outcome_var     = "vax_any",
      num_desc        = "Children 12-23 months with any vaccination",
      denom_desc      = denom
    ),
    list(
      indicator       = "EPI_NEVER_VACCINATED",
      indicator_code  = "epi_never_vaccinated",
      indicator_title = "Never vaccinated (zero dose)",
      denom_code      = "epi_eligible",
      filter_expr     = NULL,
      outcome_var     = "vax_never_vaccinated",
      num_desc        = "Children 12-23 months with no vaccination",
      denom_desc      = denom
    ),
    list(
      indicator       = "EPI_FULLY_VACCINATED",
      indicator_code  = "epi_fully_vaccinated",
      indicator_title = "Fully vaccinated (BCG + DPT3 + Polio3 + Measles1)",
      denom_code      = "epi_eligible",
      filter_expr     = NULL,
      outcome_var     = "vax_fully_vaccinated",
      num_desc        = "Children 12-23 months fully vaccinated",
      denom_desc      = denom
    )
  )
}


# =============================================================================
# Indicator Dictionary
# =============================================================================

#' EPI Indicator Dictionary
#'
#' Returns the dictionary of EPI (Expanded Programme on Immunization)
#' indicators with metadata. Not all indicators will be available in every
#' survey; availability depends on the DHS variables present in the dataset.
#'
#' @return Tibble with columns: indicator, indicator_code, indicator_title,
#'   numerator_description, denominator_description, denominator_code.
#'
#' @examples
#' epi_dictionary()
#'
#' @export
epi_dictionary <- function() {
  conds <- .epi_conditions()
  tibble::tibble(
    indicator               = vapply(conds, `[[`, character(1), "indicator"),
    indicator_code          = vapply(conds, `[[`, character(1), "indicator_code"),
    indicator_title         = vapply(conds, `[[`, character(1), "indicator_title"),
    numerator_description   = vapply(conds, `[[`, character(1), "num_desc"),
    denominator_description = vapply(conds, `[[`, character(1), "denom_desc"),
    denominator_code        = vapply(conds, `[[`, character(1), "denom_code")
  )
}
