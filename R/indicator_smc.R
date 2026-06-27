# smc indicator
#
# Merged from: dhs_calc_smc.R dhs_calc_smc_mbg.R dhs_helpers_smc.R
# Contains the survey-weighted calc, MBG cluster-prep, and indicator-
# specific helpers for this family.

# ---- dhs_calc_smc.R ----

#' Calculate SMC Coverage from DHS Data
#'
#' Estimates Seasonal Malaria Chemoprevention (SMC) coverage among children
#' under 5 using survey-weighted methods.
#'
#' @param dhs_kr DHS Children's Recode (KR) dataset.
#' @param survey_vars Named list mapping DHS variable names.
#' @param region_var Optional column name to use as grouping variable.
#' @param gps_data Optional DHS GPS dataset.
#' @param gps_vars Named list for GPS variables.
#' @param shapefile Optional sf object with administrative boundaries.
#' @param admin_level Character vector of admin columns.
#' @param join_nearest Logical.
#'
#' @return Tibble with SMC estimates including:
#'   dhs_smc, dhs_smc_low, dhs_smc_upp, dhs_n_smc_eligible, dhs_n_smc_received.
#'
#' @seealso [calc_smc_mbg()] for cluster-level MBG inputs
#' @keywords internal
calc_smc_dhs_core <- function(
  dhs_kr,
  survey_vars = list(
    cluster = "v021",
    weight = "v005",
    stratum = "v022",
    age = "hw1",
    smc_primary = "hml43",
    smc_alt = "ml13g"
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
  kr <- .prepare_smc_data(
    dhs_kr = dhs_kr,
    survey_vars = survey_vars,
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

  # Calculate SMC coverage
  if (!is.null(class_var)) {
    group_formula <- stats::as.formula(paste("~", class_var))
    smc_results <- tryCatch({
      survey::svyby(
        ~received_smc, by = group_formula, design = des,
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
          ~received_smc, by = group_formula, design = des_ns,
          FUN = survey::svymean, vartype = "ci",
          na.rm = TRUE, keep.names = FALSE
        ) |> tibble::as_tibble()
      } else stop(e)
    })
    # svyby with single variable produces ci_l/ci_u (no suffix)
    smc_results <- dplyr::rename(smc_results,
      dhs_smc = received_smc, dhs_smc_low = ci_l, dhs_smc_upp = ci_u
    )
  } else {
    smc_mean <- survey::svymean(~received_smc, design = des, na.rm = TRUE)
    smc_ci <- stats::confint(smc_mean)
    smc_results <- tibble::tibble(
      level = "National",
      dhs_smc = as.numeric(smc_mean["received_smc"]),
      dhs_smc_low = smc_ci["received_smc", 1],
      dhs_smc_upp = smc_ci["received_smc", 2]
    )
  }

  # Sample sizes
  if (!is.null(class_var)) {
    sample_sizes <- kr |>
      dplyr::group_by(.data[[class_var]]) |>
      dplyr::summarise(
        dhs_n_smc_eligible = dplyr::n(),
        dhs_n_smc_received = sum(received_smc == 1, na.rm = TRUE),
        .groups = "drop"
      )
    smc_results <- smc_results |>
      dplyr::left_join(sample_sizes, by = class_var)
  } else {
    smc_results$dhs_n_smc_eligible <- nrow(kr)
    smc_results$dhs_n_smc_received <- sum(kr$received_smc == 1, na.rm = TRUE)
  }

  # Format
  smc_results <- smc_results |>
    dplyr::mutate(
      dhs_smc = round(dhs_smc, 2),
      dhs_smc_low = pmax(0, round(dhs_smc_low, 2)),
      dhs_smc_upp = pmin(1, round(dhs_smc_upp, 2)),
      dhs_n_smc_eligible = as.integer(dhs_n_smc_eligible),
      dhs_n_smc_received = as.integer(dhs_n_smc_received)
    ) |>
    dplyr::select(-dplyr::any_of(c("level")))

  tibble::as_tibble(smc_results)
}

#' Calculate SMC Coverage from DHS Data
#'
#' Computes SMC coverage among eligible children from DHS Children's
#' Recode (KR) data. Returns survey-weighted proportions with logit
#' confidence intervals in standardized long format.
#'
#' @inheritParams calc_smc_dhs_core
#' @param ci_method Method for confidence intervals. Default: "logit".
#'
#' @return Named list with:
#'   \describe{
#'     \item{`adm0`}{National-level estimates (always present)}
#'     \item{`adm1`}{Admin-1 estimates (when `region_var` provided)}
#'   }
#'   Each tibble contains standardized columns: survey_id, iso3, iso2,
#'   survey_type, survey_year, adm0, adm1, type, geo_source, point,
#'   ci_l, ci_u, numerator, denominator, indicator, indicator_code,
#'   numerator_description, denominator_description, denominator_code.
#'
#' @examples
#' \dontrun{
#' smc <- calc_smc_dhs(dhs_kr = kr_data, region_var = "v024")
#' smc$adm0
#' smc$adm1
#' }
#'
#' @seealso [smc_dictionary()] for indicator metadata,
#'   [calc_smc_dhs_core()] for legacy wide-format output
#' @export
calc_smc_dhs <- function(
  dhs_kr,
  survey_vars = list(
    cluster = "v021",
    weight = "v005",
    stratum = "v022",
    age = "hw1",
    smc_primary = "hml43",
    smc_alt = "ml13g"
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
    reason = "for `calc_smc_dhs()`"
  )

  # ---- 1. Extract survey metadata ----
  survey_meta <- .extract_survey_meta(dhs_kr)

  # ---- 2. Prepare data ----
  kr <- .prepare_smc_data(
    dhs_kr = dhs_kr,
    survey_vars = survey_vars,
    include_survey_vars = TRUE
  )

  # .prepare_smc_data may return NULL if SMC variable not found
  if (is.null(kr)) {
    cli::cli_abort("SMC data preparation failed; SMC variable not available.")
  }

  # ---- 3. Compute indicators across admin levels ----
  .compute_dhs_indicators_with_admin(
    data               = kr,
    conditions         = .smc_conditions(),
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

#' Internal: SMC indicator conditions
#'
#' @return List of named lists, each with indicator specification.
#' @noRd
.smc_conditions <- function() {

  denom <- "Children 0-59 months with valid SMC response (hml43 or ml13g non-missing)"
  list(
    list(
      indicator       = "SMC",
      indicator_code  = "smc",
      indicator_title = "Proportion of children under 5 who received seasonal malaria chemoprevention",
      denom_code      = "smc_eligible",
      filter_expr     = NULL,
      outcome_var     = "received_smc",
      num_desc        = "Children 0-59 months who received SMC during the malaria season (hml43 == 1 or ml13g == 1)",
      denom_desc      = denom
    ),
    list(
      indicator       = "SMC",
      indicator_code  = "smc_coverage",
      indicator_title = "Proportion of children under 5 who received seasonal malaria chemoprevention",
      denom_code      = "smc_eligible",
      filter_expr     = NULL,
      outcome_var     = "received_smc",
      num_desc        = "Children 0-59 months who received SMC during the malaria season (hml43 == 1 or ml13g == 1)",
      denom_desc      = denom
    )
  )
}


# =============================================================================
# Indicator Dictionary
# =============================================================================

#' SMC Indicator Dictionary
#'
#' Returns the dictionary of SMC indicators with metadata. SMC coverage
#' measures the proportion of children under 5 who received seasonal malaria
#' chemoprevention during the malaria season, derived from DHS Children's
#' Recode (KR) variables hml43 (primary) or ml13g (fallback).
#'
#' @return Tibble with columns: indicator, indicator_code, indicator_title,
#'   numerator_description, denominator_description, denominator_code.
#'
#' @keywords internal
#' @noRd
smc_dictionary <- function() {
  conds <- .smc_conditions()
  tibble::tibble(
    indicator               = vapply(conds, `[[`, character(1), "indicator"),
    indicator_code          = vapply(conds, `[[`, character(1), "indicator_code"),
    indicator_title         = vapply(conds, `[[`, character(1), "indicator_title"),
    numerator_description   = vapply(conds, `[[`, character(1), "num_desc"),
    denominator_description = vapply(conds, `[[`, character(1), "denom_desc"),
    denominator_code        = vapply(conds, `[[`, character(1), "denom_code")
  )
}


# ---- dhs_calc_smc_mbg.R ----

#' Prepare SMC Data for MBG Analysis
#'
#' Prepares cluster-level Seasonal Malaria Chemoprevention (SMC) receipt data
#' for MBG analysis. SMC coverage among children under 5.
#'
#' @details
#' Methodology: \url{https://github.com/ahadi-analytics/sntmethods/blob/master/inst/methods/smc_dhs.yml}
#'
#' @param dhs_kr DHS Children's Recode (KR) dataset.
#' @param gps_data DHS GPS dataset with cluster coordinates.
#' @param survey_vars Named list mapping DHS variable names.
#' @param gps_vars Named list for GPS variable mapping.
#'
#' @return A data.table with columns:
#'   \itemize{
#'     \item cluster_id: Cluster identifier
#'     \item indicator: Number of children who received SMC
#'     \item samplesize: Total number of children in analysis
#'     \item x: Longitude
#'     \item y: Latitude
#'   }
#'
#' @details
#' SMC variable availability varies by survey. Common variables include:
#' \itemize{
#'   \item hml43: SMC in malaria season (DHS-7+)
#'   \item ml13g: Received antimalarial for prevention
#' }
#'
#' This function first checks which SMC-related variables are available
#' and uses the most appropriate one.
#'
#' @examples
#' \dontrun{
#' smc_mbg <- calc_smc_mbg(
#'   dhs_kr = kr_data,
#'   gps_data = gps_data
#' )
#' }
#'
#' @export
calc_smc_mbg <- function(
  dhs_kr,
  gps_data,
  survey_vars = list(
    cluster = "v001",
    age = "hw1",
    smc_primary = "hml43",
    smc_alt = "ml13g"
  ),
  gps_vars = list(
    cluster = "DHSCLUST",
    lat = "LATNUM",
    lon = "LONGNUM"
  )
) {
  # ---- Input validation ----

  if (!is.data.frame(gps_data)) {
    cli::cli_abort("`gps_data` must be a data.frame or tibble")
  }

  # ---- Prepare GPS data ----

  gps_clean <- .prepare_gps_data(gps_data, gps_vars)

  # ---- Prepare KR data ----

  kr <- .prepare_smc_data(dhs_kr, survey_vars, include_survey_vars = FALSE, strict = FALSE)
  if (is.null(kr)) return(NULL)

  # ---- Aggregate to cluster level ----

  result <- .aggregate_to_mbg_clusters(
    kr, "received_smc", gps_clean, "smc_coverage"
  )
  if (is.null(result)) return(NULL)

  result
}


#' Prepare SMC Data for MBG (Alias)
#'
#' @inheritParams calc_smc_mbg
#' @return A data.table with columns: cluster_id, indicator, samplesize, x, y
#' @export
prep_smc_mbg <- calc_smc_mbg


# ---- dhs_helpers_smc.R ----

#' Prepare SMC Data for Analysis
#'
#' Shared data cleaning and indicator computation for SMC functions.
#' Used by both calc_smc_dhs_core() and calc_smc_mbg().
#'
#' @param dhs_kr DHS Children's Recode (KR) dataset.
#' @param survey_vars Named list mapping DHS variable names.
#' @param include_survey_vars Logical. If TRUE, includes survey design columns.
#'
#' @return A data frame of eligible children with columns:
#'   cluster_id, age_months, received_smc (binary 0/1).
#'   If include_survey_vars = TRUE, also: survey_weight, stratum_id.
#'
#' @noRd
.prepare_smc_data <- function(
  dhs_kr,
  survey_vars,
  include_survey_vars = FALSE,
  strict = TRUE
) {
  if (!is.data.frame(dhs_kr)) {
    cli::cli_abort("`dhs_kr` must be a data.frame or tibble")
  }
  if (nrow(dhs_kr) == 0) {
    cli::cli_abort("`dhs_kr` is empty.")
  }

  # Determine which SMC variable to use
  smc_var <- NULL
  if (survey_vars$smc_primary %in% names(dhs_kr)) {
    smc_var <- survey_vars$smc_primary
    cli::cli_alert_info("Using SMC variable: {.var {smc_var}} (primary)")
  } else if (!is.null(survey_vars$smc_alt) && survey_vars$smc_alt %in% names(dhs_kr)) {
    # Guard: if smc_alt is ml13g and other ml13 drug variables exist,
    # ml13g is part of the antimalarial treatment series, not SMC
    ml13_drug_vars <- names(dhs_kr)[grepl("^ml13[a-f]$", names(dhs_kr))]
    if (grepl("^ml13", survey_vars$smc_alt) && length(ml13_drug_vars) > 0) {
      return(NULL)
    } else {
      smc_var <- survey_vars$smc_alt
      cli::cli_alert_info("Using SMC variable: {.var {smc_var}} (alternative)")
    }
  } else if (strict) {
    cli::cli_abort(
      "No SMC variable found; checked {.var {survey_vars$smc_primary}} and {.var {survey_vars$smc_alt}}"
    )
  } else {
    cli::cli_warn(
      "No SMC variable found; checked {.var {survey_vars$smc_primary}} and {.var {survey_vars$smc_alt}}; SMC not available for this survey"
    )
    return(NULL)
  }

  kr <- dhs_kr |>
    dplyr::mutate(dplyr::across(dplyr::everything(), haven::zap_labels)) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), as.vector)) |>
    dplyr::mutate(
      cluster_id = .data[[survey_vars$cluster]],
      age_months = suppressWarnings(as.numeric(as.character(.data[[survey_vars$age]]))),
      smc_receipt = suppressWarnings(as.numeric(as.character(.data[[smc_var]])))
    )

  if (include_survey_vars) {
    kr <- kr |>
      dplyr::mutate(
        survey_weight = .data[[survey_vars$weight]] / 1e6,
        stratum_id = .data[[survey_vars$stratum]]
      )
  }

  # Filter to U5 children with valid SMC responses
  kr <- kr |>
    dplyr::filter(
      age_months >= 0,
      age_months <= 59,
      !is.na(smc_receipt),
      smc_receipt %in% c(0, 1)
    ) |>
    dplyr::mutate(
      received_smc = as.integer(smc_receipt == 1)
    )

  if (nrow(kr) == 0) {
    cli::cli_abort("No eligible children with valid SMC data found")
  }

  cli::cli_alert_info(
    "Found {format(nrow(kr), big.mark = ',')} children with SMC data"
  )

  kr
}


