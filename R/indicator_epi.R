# epi indicator
#
# Merged from: dhs_calc_epi.R dhs_calc_epi_mbg.R dhs_helpers_epi.R
# Contains the survey-weighted calc, MBG cluster-prep, and indicator-
# specific helpers for this family.

# ---- dhs_calc_epi.R ----

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
#' @keywords internal
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
#' @param gps_data Optional DHS GE (GPS) cluster dataset used to attach
#'   admin-unit labels when `shapefile` is supplied. Default `NULL`.
#' @param gps_vars Named list mapping cluster/lat/lon column names in
#'   `gps_data`. Defaults to the standard DHS GE names
#'   (`DHSCLUST`, `LATNUM`, `LONGNUM`).
#' @param shapefile Optional `sf` polygon dataset whose attributes carry
#'   admin labels for the cluster-to-admin spatial join. When `NULL`
#'   (default) the spatial join step is skipped.
#' @param admin_level Character vector of admin column names in `shapefile`
#'   to retain (e.g. `c("adm1", "adm2")`). Default `NULL` (use all).
#' @param join_nearest Logical. If `TRUE` (default), clusters that fall
#'   outside any polygon are re-assigned to the nearest polygon. If
#'   `FALSE`, unmatched clusters are left as `NA`.
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
  region_var   = NULL,
  gps_data     = NULL,
  gps_vars     = list(cluster = "DHSCLUST", lat = "LATNUM", lon = "LONGNUM"),
  shapefile    = NULL,
  admin_level  = NULL,
  join_nearest = TRUE,
  ci_method    = "logit"
) {
  # Fail fast on missing suggested dependencies
  .check_pkg(
    c("tibble"),
    reason = "for `calc_epi_dhs()`"
  )

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

  # ---- 3. Build conditions for available vaccines ----
  all_conditions <- .epi_conditions()

  # Detect which vax_ columns are available in prepared data
  available_vax <- names(kr)[grepl("^vax_", names(kr))]

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

  # ---- 4. Compute indicators across admin levels ----
  .compute_dhs_indicators_with_admin(
    data               = kr,
    conditions         = conditions,
    dhs_data           = dhs_kr,
    survey_meta        = survey_meta,
    region_var         = region_var,
    default_region_var = "v024",
    gps_data           = gps_data,
    gps_vars           = gps_vars,
    shapefile          = shapefile,
    admin_level        = admin_level,
    join_nearest       = join_nearest,
    ci_method          = ci_method
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
#' @keywords internal
#' @noRd
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


# ---- dhs_calc_epi_mbg.R ----

#' Prepare EPI (Vaccination) Data for MBG Analysis
#'
#' Prepares cluster-level vaccination coverage data for MBG analysis.
#' Calculates coverage for standard EPI vaccines plus malaria vaccine.
#'
#' @details
#' Methodology: \url{https://github.com/ahadi-analytics/sntmethods/blob/master/inst/methods/epi_dhs.yml}
#'
#' @param dhs_kr DHS Children's Recode (KR) dataset.
#' @param gps_data DHS GPS dataset with cluster coordinates.
#' @param indicators Character vector of vaccines to calculate:
#'   \itemize{
#'     \item "bcg": BCG vaccine
#'     \item "dpt1", "dpt2", "dpt3": DPT doses 1-3 (falls back to pentavalent)
#'     \item "polio0": OPV birth dose
#'     \item "polio1", "polio2", "polio3": Polio doses 1-3
#'     \item "measles1", "measles2": Measles doses 1-2
#'     \item "vita1", "vita2": Vitamin A doses 1-2
#'     \item "malaria": Malaria vaccine (RTS,S/R21)
#'     \item "penta1", "penta2", "penta3": Pentavalent doses 1-3
#'     \item "pneumo1", "pneumo2", "pneumo3": Pneumococcal doses 1-3
#'     \item "rota1", "rota2", "rota3": Rotavirus doses 1-3
#'     \item "ipv": Inactivated Polio Vaccine
#'     \item "hepb0": Hepatitis B birth dose
#'     \item "yellowfever": Yellow Fever vaccine
#'     \item "any": Any vaccination (h10 >= 1)
#'     \item "never_vaccinated": Zero-dose (h10 == 0)
#'     \item "fully_vaccinated": Basic fully vaccinated
#'   }
#'   Default: c("bcg", "dpt3", "measles1").
#' @param age_min_months Minimum age in months (default: 12).
#' @param age_max_months Maximum age in months (default: 23).
#' @param survey_vars Named list mapping DHS variable names.
#' @param gps_vars Named list for GPS variable mapping.
#'
#' @return A list of data.tables (one per vaccine).
#'
#' @details
#' Standard EPI target age is 12-23 months (children who have had time to
#' complete basic vaccination schedule). Malaria vaccine (RTS,S) is measured
#' by h68 variable.
#'
#' @examples
#' \dontrun{
#' epi_mbg <- calc_epi_mbg(
#'   dhs_kr = kr_data,
#'   gps_data = gps_data,
#'   indicators = c("bcg", "dpt3", "measles1")
#' )
#' }
#'
#' @export
calc_epi_mbg <- function(
  dhs_kr,
  gps_data,
  indicators = c("bcg", "dpt3", "measles1"),
  age_min_months = 12,
  age_max_months = 23,
  survey_vars = list(
    cluster = "v001",
    age = "hw1",
    bcg = "h2",
    polio0 = "h0",
    dpt1 = "h3", dpt2 = "h4", dpt3 = "h5",
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
  gps_vars = list(
    cluster = "DHSCLUST",
    lat = "LATNUM",
    lon = "LONGNUM"
  )
) {
  # ---- Input validation ----

  valid_indicators <- c(
    "bcg", "dpt1", "dpt2", "dpt3",
    "polio0", "polio1", "polio2", "polio3",
    "measles1", "measles2",
    "vita1", "vita2",
    "malaria",
    "penta1", "penta2", "penta3",
    "pneumo1", "pneumo2", "pneumo3",
    "rota1", "rota2", "rota3",
    "ipv", "hepb0", "yellowfever",
    "any", "never_vaccinated",
    "fully_vaccinated"
  )
  invalid <- setdiff(indicators, valid_indicators)
  if (length(invalid) > 0) {
    cli::cli_abort("Invalid indicators: {.val {invalid}}")
  }

  # ---- Prepare GPS data ----

  gps_clean <- .prepare_gps_data(gps_data, gps_vars)

  # ---- Prepare KR data ----

  kr <- .prepare_epi_data(
    dhs_kr, survey_vars,
    age_min_months = age_min_months,
    age_max_months = age_max_months,
    include_survey_vars = FALSE
  )

  # ---- Calculate vaccine indicators ----

  results <- list()

  for (ind in indicators) {
    if (ind == "fully_vaccinated") {
      # Check that vax_fully_vaccinated was created by the helper
      if (!"vax_fully_vaccinated" %in% names(kr)) {
        cli::cli_alert_warning(
          "Cannot calculate fully_vaccinated - missing required vaccine variables"
        )
        next
      }

      result <- .aggregate_to_mbg_clusters(
        kr, "vax_fully_vaccinated", gps_clean, "epi_fully_vaccinated"
      )
      if (!is.null(result)) results[["epi_fully_vaccinated"]] <- result

    } else {
      # Single vaccine indicator
      vax_col <- paste0("vax_", ind)

      if (!vax_col %in% names(kr)) {
        cli::cli_alert_warning(
          "Vaccine column {.var {vax_col}} for {ind} not available in prepared data"
        )
        next
      }

      result_name <- paste0("epi_", ind)
      result <- .aggregate_to_mbg_clusters(kr, vax_col, gps_clean, result_name)
      if (!is.null(result)) results[[result_name]] <- result
    }
  }

  if (length(results) == 0) return(NULL)

  results
}


#' Prepare Single EPI Indicator for MBG
#'
#' @inheritParams calc_epi_mbg
#' @param vaccine Single vaccine name. Default: "measles1".
#'
#' @return A data.table with columns: cluster_id, indicator, samplesize, x, y
#' @export
prep_epi_mbg <- function(
  dhs_kr,
  gps_data,
  vaccine = "measles1",
  age_min_months = 12,
  age_max_months = 23,
  survey_vars = list(
    cluster = "v001",
    age = "hw1",
    bcg = "h2",
    polio0 = "h0",
    dpt1 = "h3", dpt2 = "h4", dpt3 = "h5",
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
  gps_vars = list(
    cluster = "DHSCLUST",
    lat = "LATNUM",
    lon = "LONGNUM"
  )
) {
  result <- calc_epi_mbg(
    dhs_kr = dhs_kr,
    gps_data = gps_data,
    indicators = vaccine,
    age_min_months = age_min_months,
    age_max_months = age_max_months,
    survey_vars = survey_vars,
    gps_vars = gps_vars
  )

  result[[1]]
}


# ---- dhs_helpers_epi.R ----

#' EPI Vaccine Registry
#'
#' Centralized mapping of vaccine indicators to DHS variables, including
#' fallback variables for vaccines that changed names across DHS eras
#' (e.g., DPT -> Pentavalent).
#'
#' @return Named list. Each element contains: dhs_var, fallback_var (or NULL),
#'   fallback_source (or NULL).
#'
#' @noRd
.epi_vaccine_registry <- function() {
  list(
    # Classic EPI vaccines (all DHS eras)
    bcg      = list(dhs_var = "h2",   fallback_var = NULL),
    polio0   = list(dhs_var = "h0",   fallback_var = NULL),
    dpt1     = list(dhs_var = "h3",   fallback_var = "h51", fallback_source = "pentavalent"),
    dpt2     = list(dhs_var = "h4",   fallback_var = "h52", fallback_source = "pentavalent"),
    dpt3     = list(dhs_var = "h5",   fallback_var = "h53", fallback_source = "pentavalent"),
    polio1   = list(dhs_var = "h6",   fallback_var = NULL),
    polio2   = list(dhs_var = "h7",   fallback_var = NULL),
    polio3   = list(dhs_var = "h8",   fallback_var = NULL),
    measles1 = list(dhs_var = "h9",   fallback_var = NULL),
    measles2 = list(dhs_var = "h9a",  fallback_var = NULL),
    vita1    = list(dhs_var = "h33",  fallback_var = NULL),
    vita2    = list(dhs_var = "h33a", fallback_var = NULL),
    malaria  = list(dhs_var = "h68",  fallback_var = NULL),
    # Newer vaccines (DHS-7+, circa 2015+)
    penta1   = list(dhs_var = "h51",  fallback_var = NULL),
    penta2   = list(dhs_var = "h52",  fallback_var = NULL),
    penta3   = list(dhs_var = "h53",  fallback_var = NULL),
    pneumo1  = list(dhs_var = "h54",  fallback_var = NULL),
    pneumo2  = list(dhs_var = "h55",  fallback_var = NULL),
    pneumo3  = list(dhs_var = "h56",  fallback_var = NULL),
    rota1    = list(dhs_var = "h57",  fallback_var = NULL),
    rota2    = list(dhs_var = "h58",  fallback_var = NULL),
    rota3    = list(dhs_var = "h59",  fallback_var = NULL),
    ipv      = list(dhs_var = "h60",  fallback_var = NULL),
    hepb0    = list(dhs_var = "h50",  fallback_var = NULL),
    yellowfever = list(dhs_var = "h61", fallback_var = NULL),
    # Aggregated indicators
    any      = list(dhs_var = "h10",  fallback_var = NULL)
  )
}


#' Prepare EPI Data for Analysis
#'
#' Shared data cleaning and indicator computation for EPI functions.
#' Used by both calc_epi_dhs_core() and calc_epi_mbg().
#'
#' Applies recall bias correction: prefers `b19` (interview-date-corrected age)
#' over `hw1` (raw age from health card). DHS-7+ surveys provide `b19` to
#' address age heaping at 12 and 24 months, which distorts the 12-23 month
#' eligibility window.
#'
#' @param dhs_kr DHS Children's Recode (KR) dataset.
#' @param survey_vars Named list mapping DHS variable names.
#' @param age_min_months Minimum age in months.
#' @param age_max_months Maximum age in months.
#' @param include_survey_vars Logical. If TRUE, includes survey design columns.
#'
#' @return A data frame of eligible children with columns:
#'   cluster_id, age_months, and binary vaccination columns for each
#'   available vaccine. If include_survey_vars = TRUE, also: survey_weight,
#'   stratum_id.
#'
#' @noRd
.prepare_epi_data <- function(
  dhs_kr,
  survey_vars,
  age_min_months = 12,
  age_max_months = 23,
  include_survey_vars = FALSE
) {
  if (!is.data.frame(dhs_kr)) {
    cli::cli_abort("`dhs_kr` must be a data.frame or tibble")
  }
  if (nrow(dhs_kr) == 0) {
    cli::cli_abort("`dhs_kr` is empty.")
  }

  # Auto-detect age variable if specified one is missing
  # Fallback order: b19 (recall-corrected) -> hc1 (standard KR) -> b8 (current age) -> hw1 (anthropometry)
  if (!survey_vars$age %in% names(dhs_kr)) {
    age_candidates <- c("b19", "hc1", "b8", "hw1")
    available_age <- intersect(age_candidates, names(dhs_kr))

    if (length(available_age) > 0) {
      old_age_var <- survey_vars$age
      survey_vars$age <- available_age[1]
      cli::cli_alert_info(
        "Age variable {.var {old_age_var}} not found; using {.var {survey_vars$age}} instead"
      )
    }
  }

  # Recall bias correction: prefer b19 over hw1
  # b19 = current age computed from interview CMC - birth CMC (DHS-7+)
  # hw1 = age from health card (subject to heaping at 12/24 months)
  age_var <- survey_vars$age
  if (age_var == "hw1" && "b19" %in% names(dhs_kr)) {
    b19_vals <- as.vector(haven::zap_labels(dhs_kr[["b19"]]))
    if (any(!is.na(b19_vals))) {
      age_var <- "b19"
      survey_vars$age <- "b19"
      cli::cli_alert_info(
        "Using {.var b19} (recall-bias-corrected age) instead of {.var hw1}"
      )
    }
  }

  # Build vaccine mapping from survey_vars
  vaccine_mapping <- list(
    bcg = survey_vars$bcg, dpt1 = survey_vars$dpt1,
    dpt2 = survey_vars$dpt2, dpt3 = survey_vars$dpt3,
    polio0 = survey_vars$polio0,
    polio1 = survey_vars$polio1, polio2 = survey_vars$polio2,
    polio3 = survey_vars$polio3,
    measles1 = survey_vars$measles1, measles2 = survey_vars$measles2,
    vita1 = survey_vars$vita1, vita2 = survey_vars$vita2,
    malaria = survey_vars$malaria,
    penta1 = survey_vars$penta1, penta2 = survey_vars$penta2,
    penta3 = survey_vars$penta3,
    pneumo1 = survey_vars$pneumo1, pneumo2 = survey_vars$pneumo2,
    pneumo3 = survey_vars$pneumo3,
    rota1 = survey_vars$rota1, rota2 = survey_vars$rota2,
    rota3 = survey_vars$rota3,
    ipv = survey_vars$ipv, hepb0 = survey_vars$hepb0,
    yellowfever = survey_vars$yellowfever,
    any = survey_vars$any
  )

  # Apply DPT -> Pentavalent fallback when primary variable is missing
  registry <- .epi_vaccine_registry()
  fallback_applied <- character(0)

  for (vax_name in names(vaccine_mapping)) {
    var_name <- vaccine_mapping[[vax_name]]
    if (!is.null(var_name) && !(var_name %in% names(dhs_kr))) {
      reg_entry <- registry[[vax_name]]
      if (!is.null(reg_entry$fallback_var) &&
          reg_entry$fallback_var %in% names(dhs_kr)) {
        vaccine_mapping[[vax_name]] <- reg_entry$fallback_var
        fallback_applied <- c(
          fallback_applied,
          paste0(vax_name, ": ", var_name, " -> ",
                 reg_entry$fallback_var, " (", reg_entry$fallback_source, ")")
        )
      }
    }
  }

  if (length(fallback_applied) > 0) {
    cli::cli_alert_info(
      "Variable fallbacks applied: {paste(fallback_applied, collapse = '; ')}"
    )
  }

  available_vaccines <- sapply(vaccine_mapping, function(v) !is.null(v) && v %in% names(dhs_kr))
  available_vaccine_cols <- unlist(vaccine_mapping[available_vaccines])

  # Check if any vaccines are available
  if (length(available_vaccine_cols) == 0) {
    cli::cli_warn("No vaccine variables found in KR data; EPI indicators not available for this survey")
    return(NULL)
  }

  # Select columns
  select_cols <- unique(c(
    survey_vars$cluster, survey_vars$age,
    available_vaccine_cols
  ))
  if (include_survey_vars) {
    select_cols <- unique(c(select_cols, survey_vars$weight, survey_vars$stratum))
  }
  select_cols <- select_cols[select_cols %in% names(dhs_kr)]

  kr <- dhs_kr |>
    dplyr::select(dplyr::all_of(select_cols)) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), haven::zap_labels)) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), as.vector)) |>
    dplyr::mutate(
      cluster_id = .data[[survey_vars$cluster]],
      age_months = suppressWarnings(as.numeric(as.character(.data[[survey_vars$age]])))
    )

  if (include_survey_vars) {
    kr <- kr |>
      dplyr::mutate(
        survey_weight = .data[[survey_vars$weight]] / 1e6,
        stratum_id = .data[[survey_vars$stratum]]
      )
  }

  # Filter to eligible age range
  kr <- kr |>
    dplyr::filter(
      age_months >= age_min_months,
      age_months <= age_max_months
    )

  if (nrow(kr) == 0) {
    cli::cli_abort(
      "No eligible children found in age range {age_min_months}-{age_max_months} months"
    )
  }

  cli::cli_alert_info(
    "Found {format(nrow(kr), big.mark = ',')} children aged {age_min_months}-{age_max_months} months"
  )

  # Add binary vaccination columns for each available vaccine

  # Ensure vaccine columns are numeric (guards against residual haven labels
  # or character values in older DHS surveys -- same issue as CSB h32 fix)
  for (vax_name in names(available_vaccines)[available_vaccines]) {
    var_name <- vaccine_mapping[[vax_name]]
    kr[[var_name]] <- suppressWarnings(as.numeric(as.character(kr[[var_name]])))
  }

  # Vaccines that need special coding (not the standard 1/2/3 pattern)
  special_vaccines <- c("any")

  # DHS: 1 = vaccination card, 2 = reported by mother, 3 = both
  for (vax_name in names(available_vaccines)[available_vaccines]) {
    if (vax_name %in% special_vaccines) next
    var_name <- vaccine_mapping[[vax_name]]
    col_name <- paste0("vax_", vax_name)
    kr[[col_name]] <- as.integer(!is.na(kr[[var_name]]) & kr[[var_name]] %in% c(1, 2, 3))
  }

  # Special coding: "any" -- h10 >= 1 means child received at least one vaccination
  if ("any" %in% names(available_vaccines)[available_vaccines]) {
    any_var <- vaccine_mapping[["any"]]
    kr$vax_any <- as.integer(!is.na(kr[[any_var]]) & kr[[any_var]] >= 1)
  }

  # Derived indicator: "never_vaccinated" -- inverse of "any" (h10 == 0)
  if ("any" %in% names(available_vaccines)[available_vaccines]) {
    any_var <- vaccine_mapping[["any"]]
    kr$vax_never_vaccinated <- as.integer(!is.na(kr[[any_var]]) & kr[[any_var]] == 0)
  }

  # Add fully_vaccinated if all required vaccines are present
  required_for_fv <- c("bcg", "dpt3", "polio3", "measles1")
  if (all(required_for_fv %in% names(available_vaccines)[available_vaccines])) {
    kr$vax_fully_vaccinated <- as.integer(
      kr$vax_bcg == 1 & kr$vax_dpt3 == 1 &
      kr$vax_polio3 == 1 & kr$vax_measles1 == 1
    )
  }

  kr
}


