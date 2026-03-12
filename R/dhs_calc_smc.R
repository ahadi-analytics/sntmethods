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
#' @export
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
#'   survey_type, survey_year, adm0, [adm1], type, geo_source, point,
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
  region_var = NULL,
  ci_method = "logit"
) {
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
    group_var <- "region"
    geo_src <- "survey"
  }

  # ---- 4. Get conditions ----
  conditions <- .smc_conditions()

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

#' Internal: SMC indicator conditions
#'
#' @return List of named lists, each with indicator specification.
#' @noRd
.smc_conditions <- function() {
  list(
    list(
      indicator       = "SMC",
      indicator_code  = "smc",
      indicator_title = "SMC coverage among eligible children",
      denom_code      = "smc_eligible",
      filter_expr     = NULL,
      outcome_var     = "received_smc",
      num_desc        = "Children who received SMC",
      denom_desc      = "SMC-eligible children"
    ),
    list(
      indicator       = "SMC",
      indicator_code  = "smc_coverage",
      indicator_title = "SMC coverage among eligible children",
      denom_code      = "smc_eligible",
      filter_expr     = NULL,
      outcome_var     = "received_smc",
      num_desc        = "Children who received SMC",
      denom_desc      = "SMC-eligible children"
    )
  )
}


# =============================================================================
# Indicator Dictionary
# =============================================================================

#' SMC Indicator Dictionary
#'
#' Returns the dictionary of SMC indicators with metadata.
#'
#' @return Tibble with columns: indicator, indicator_code, indicator_title,
#'   numerator_description, denominator_description, denominator_code.
#'
#' @examples
#' smc_dictionary()
#'
#' @export
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
