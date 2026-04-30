#' Calculate IRS Coverage from DHS Data
#'
#' Estimates Indoor Residual Spraying (IRS) coverage at the household level
#' using survey-weighted methods from DHS Household Records data.
#'
#' @param dhs_hr DHS Household Records (HR) dataset.
#' @param survey_vars Named list mapping DHS variable names. Required keys:
#'   \itemize{
#'     \item `cluster`: Cluster/PSU ID (default: "hv021")
#'     \item `weight`: Survey weight (default: "hv005")
#'     \item `stratum`: Stratum variable (default: "hv022")
#'     \item `irs`: IRS variable (default: "hv253")
#'   }
#' @param region_var Optional column name to use as grouping variable.
#' @param gps_data Optional DHS GPS dataset.
#' @param gps_vars Named list for GPS variables.
#' @param shapefile Optional sf object with administrative boundaries.
#' @param admin_level Character vector of admin columns from shapefile.
#' @param join_nearest Logical; if TRUE, assigns clusters outside polygons
#'   to nearest admin unit.
#'
#' @return Tibble with IRS estimates including:
#'   dhs_irs, dhs_irs_low, dhs_irs_upp, dhs_n_households_irs, dhs_n_sprayed.
#'
#' @seealso [calc_irs_mbg()] for cluster-level MBG inputs
#' @keywords internal
calc_irs_dhs_core <- function(
  dhs_hr,
  survey_vars = list(
    cluster = "hv021",
    weight = "hv005",
    stratum = "hv022",
    irs = "hv253"
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
  hr <- .prepare_irs_data(
    dhs_hr = dhs_hr,
    survey_vars = survey_vars,
    include_survey_vars = TRUE
  )

  # Determine grouping variable
  class_var <- NULL
  if (!is.null(region_var)) {
    if (!region_var %in% names(hr)) {
      cli::cli_abort("Column {.var {region_var}} not found in data.")
    }
    class_var <- region_var
  } else if ("hv024" %in% names(hr)) {
    class_var <- "hv024"
    cli::cli_alert_info("Using hv024 (region) as grouping variable")
  }

  # Set up survey design
  use_strata <- dplyr::n_distinct(hr$stratum_id) > 1
  if (use_strata) {
    survey_options <- options(survey.lonely.psu = "adjust")
    on.exit(options(survey_options), add = TRUE)
    des <- survey::svydesign(
      ids = ~cluster_id, strata = ~stratum_id,
      weights = ~survey_weight, data = hr, nest = TRUE
    )
  } else {
    des <- survey::svydesign(
      ids = ~cluster_id, weights = ~survey_weight,
      data = hr, nest = TRUE
    )
  }

  # Calculate IRS coverage
  if (!is.null(class_var)) {
    group_formula <- stats::as.formula(paste("~", class_var))
    irs_results <- tryCatch({
      survey::svyby(
        ~sprayed, by = group_formula, design = des,
        FUN = survey::svymean, vartype = "ci",
        na.rm = TRUE, keep.names = FALSE
      ) |> tibble::as_tibble()
    }, error = function(e) {
      if (grepl("has only one PSU", e$message)) {
        des_ns <- survey::svydesign(
          ids = ~cluster_id, weights = ~survey_weight,
          data = hr, nest = TRUE
        )
        survey::svyby(
          ~sprayed, by = group_formula, design = des_ns,
          FUN = survey::svymean, vartype = "ci",
          na.rm = TRUE, keep.names = FALSE
        ) |> tibble::as_tibble()
      } else stop(e)
    })
    # svyby with single variable produces ci_l/ci_u (no suffix)
    irs_results <- dplyr::rename(irs_results,
      dhs_irs = sprayed, dhs_irs_low = ci_l, dhs_irs_upp = ci_u
    )
  } else {
    irs_mean <- survey::svymean(~sprayed, design = des, na.rm = TRUE)
    irs_ci <- stats::confint(irs_mean)
    irs_results <- tibble::tibble(
      level = "National",
      dhs_irs = as.numeric(irs_mean["sprayed"]),
      dhs_irs_low = irs_ci["sprayed", 1],
      dhs_irs_upp = irs_ci["sprayed", 2]
    )
  }

  # Sample sizes
  if (!is.null(class_var)) {
    sample_sizes <- hr |>
      dplyr::group_by(.data[[class_var]]) |>
      dplyr::summarise(
        dhs_n_households_irs = dplyr::n(),
        dhs_n_sprayed = sum(sprayed == 1, na.rm = TRUE),
        .groups = "drop"
      )
    irs_results <- irs_results |>
      dplyr::left_join(sample_sizes, by = class_var)
  } else {
    irs_results$dhs_n_households_irs <- nrow(hr)
    irs_results$dhs_n_sprayed <- sum(hr$sprayed == 1, na.rm = TRUE)
  }

  # Format
  irs_results <- irs_results |>
    dplyr::mutate(
      dhs_irs = round(dhs_irs, 2),
      dhs_irs_low = pmax(0, round(dhs_irs_low, 2)),
      dhs_irs_upp = pmin(1, round(dhs_irs_upp, 2)),
      dhs_n_households_irs = as.integer(dhs_n_households_irs),
      dhs_n_sprayed = as.integer(dhs_n_sprayed)
    ) |>
    dplyr::select(-dplyr::any_of(c("level")))

  tibble::as_tibble(irs_results)
}

#' Calculate IRS Coverage from DHS Data
#'
#' Computes IRS coverage among households from DHS Household Records (HR)
#' data. Returns survey-weighted proportions with logit confidence intervals
#' in standardized long format.
#'
#' @inheritParams calc_irs_dhs_core
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
#' irs <- calc_irs_dhs(dhs_hr = hr_data, region_var = "hv024")
#' irs$adm0
#' irs$adm1
#' }
#'
#' @seealso [irs_dictionary()] for indicator metadata,
#'   [calc_irs_dhs_core()] for legacy wide-format output
#' @export
calc_irs_dhs <- function(
  dhs_hr,
  survey_vars = list(
    cluster = "hv021",
    weight = "hv005",
    stratum = "hv022",
    irs = "hv253"
  ),
  region_var = NULL,
  ci_method = "logit"
) {
  # ---- 1. Extract survey metadata (HR data uses hv-prefix) ----
  survey_meta <- .extract_survey_meta_hv(dhs_hr)

  # ---- 2. Prepare data ----
  hr <- .prepare_irs_data(
    dhs_hr = dhs_hr,
    survey_vars = survey_vars,
    include_survey_vars = TRUE
  )

  # ---- 3. Resolve region labels ----
  group_var <- NULL
  geo_src <- NA_character_

  if (!is.null(region_var)) {
    if (!region_var %in% names(dhs_hr)) {
      cli::cli_abort("Column {.var {region_var}} not found in `dhs_hr`.")
    }
    hr$region <- .resolve_region_labels(
      dhs_hr[[region_var]], region_var
    )
    group_var <- "region"
    geo_src <- "survey"
  }

  # ---- 4. Get conditions ----
  conditions <- .irs_conditions()

  # ---- 5. Compute national results ----
  national_results <- purrr::map_dfr(conditions, function(cond) {
    .compute_dhs_indicator_generic(
      data = hr,
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
        data = hr,
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

#' Internal: IRS indicator conditions
#'
#' @return List of named lists, each with indicator specification.
#' @noRd
.irs_conditions <- function() {
  denom <- "All surveyed households with valid IRS response (hv253 non-missing)"
  list(
    list(
      indicator       = "IRS",
      indicator_code  = "irs",
      indicator_title = "Proportion of households sprayed with residual insecticide in last 12 months",
      denom_code      = "all_hh",
      filter_expr     = NULL,
      outcome_var     = "sprayed",
      num_desc        = "Households sprayed with residual insecticide in the 12 months preceding the survey (hv253 == 1)",
      denom_desc      = denom
    ),
    list(
      indicator       = "IRS",
      indicator_code  = "irs_coverage",
      indicator_title = "Proportion of households sprayed with residual insecticide in last 12 months",
      denom_code      = "all_hh",
      filter_expr     = NULL,
      outcome_var     = "sprayed",
      num_desc        = "Households sprayed with residual insecticide in the 12 months preceding the survey (hv253 == 1)",
      denom_desc      = denom
    )
  )
}


# =============================================================================
# Indicator Dictionary
# =============================================================================

#' IRS Indicator Dictionary
#'
#' Returns the dictionary of IRS indicators with metadata. IRS coverage
#' measures the proportion of households sprayed with residual insecticide
#' in the 12 months preceding the survey, derived from the DHS Household
#' Recode (HR) variable hv253.
#'
#' @return Tibble with columns: indicator, indicator_code, indicator_title,
#'   numerator_description, denominator_description, denominator_code.
#'
#' @keywords internal
irs_dictionary <- function() {
  conds <- .irs_conditions()
  tibble::tibble(
    indicator               = vapply(conds, `[[`, character(1), "indicator"),
    indicator_code          = vapply(conds, `[[`, character(1), "indicator_code"),
    indicator_title         = vapply(conds, `[[`, character(1), "indicator_title"),
    numerator_description   = vapply(conds, `[[`, character(1), "num_desc"),
    denominator_description = vapply(conds, `[[`, character(1), "denom_desc"),
    denominator_code        = vapply(conds, `[[`, character(1), "denom_code")
  )
}
