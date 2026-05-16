#' Calculate ANC Coverage from DHS Data
#'
#' Estimates Antenatal Care (ANC) attendance rates using survey-weighted
#' methods from DHS Individual Recode data.
#'
#' @param dhs_ir DHS Individual Recode (IR) dataset.
#' @param survey_vars Named list mapping DHS variable names. Required keys:
#'   \itemize{
#'     \item `cluster`: Cluster/PSU ID (default: "v021")
#'     \item `weight`: Survey weight (default: "v005")
#'     \item `stratum`: Stratum variable (default: "v022")
#'     \item `interview_date`: Interview date CMC (default: "v008")
#'     \item `birth_date`: Birth date CMC (default: "b3_01")
#'     \item `anc_visits`: Number of ANC visits (default: "m14_1")
#'   }
#' @param birth_window_months Maximum months since last birth. Default: 24.
#' @param region_var Optional column name to use as grouping variable.
#' @param gps_data Optional DHS GPS dataset.
#' @param gps_vars Named list for GPS variables.
#' @param shapefile Optional sf object with administrative boundaries.
#' @param admin_level Character vector of admin columns.
#' @param join_nearest Logical; if TRUE, assigns unmatched clusters.
#'
#' @return Tibble with ANC estimates including:
#'   dhs_anc_1plus, dhs_anc_2plus, dhs_anc_3plus, dhs_anc_4plus, dhs_anc_8plus
#'   (each with _low, _upp), dhs_n_recent_births.
#'
#' @seealso [calc_anc_mbg()] for cluster-level MBG inputs
#' @keywords internal
calc_anc_dhs_core <- function(
  dhs_ir,
  survey_vars = list(
    cluster = "v021",
    weight = "v005",
    stratum = "v022",
    interview_date = "v008",
    birth_date = "b3_01",
    anc_visits = "m14_1"
  ),
  birth_window_months = 24,
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
  ir <- .prepare_anc_data(
    dhs_ir = dhs_ir,
    survey_vars = survey_vars,
    birth_window_months = birth_window_months,
    include_survey_vars = TRUE
  )

  # Determine grouping
  class_var <- NULL
  if (!is.null(region_var)) {
    if (!region_var %in% names(dhs_ir)) {
      cli::cli_abort("Column {.var {region_var}} not found in `dhs_ir`.")
    }
    class_var <- region_var
  } else if ("v024" %in% names(ir)) {
    class_var <- "v024"
    cli::cli_alert_info("Using v024 (region) as grouping variable")
  }

  # Set up survey design
  use_strata <- dplyr::n_distinct(ir$stratum_id) > 1
  if (use_strata) {
    survey_options <- options(survey.lonely.psu = "adjust")
    on.exit(options(survey_options), add = TRUE)
    des <- survey::svydesign(
      ids = ~cluster_id, strata = ~stratum_id,
      weights = ~survey_weight, data = ir, nest = TRUE
    )
  } else {
    des <- survey::svydesign(
      ids = ~cluster_id, weights = ~survey_weight,
      data = ir, nest = TRUE
    )
  }

  # Calculate ANC indicators
  indicator_formula <- ~has_anc1 + has_anc2 + has_anc3 + has_anc4 + has_anc8

  if (!is.null(class_var)) {
    group_formula <- stats::as.formula(paste("~", class_var))
    anc_results <- tryCatch({
      survey::svyby(
        indicator_formula, by = group_formula, design = des,
        FUN = survey::svymean, vartype = "ci",
        na.rm = TRUE, keep.names = FALSE
      ) |> tibble::as_tibble()
    }, error = function(e) {
      if (grepl("has only one PSU", e$message)) {
        des_ns <- survey::svydesign(
          ids = ~cluster_id, weights = ~survey_weight,
          data = ir, nest = TRUE
        )
        survey::svyby(
          indicator_formula, by = group_formula, design = des_ns,
          FUN = survey::svymean, vartype = "ci",
          na.rm = TRUE, keep.names = FALSE
        ) |> tibble::as_tibble()
      } else stop(e)
    })
  } else {
    anc_means <- survey::svymean(indicator_formula, design = des, na.rm = TRUE)
    anc_ci <- stats::confint(anc_means)
    anc_results <- tibble::tibble(
      level = "National",
      has_anc1 = as.numeric(anc_means["has_anc1"]),
      `ci_l.has_anc1` = anc_ci["has_anc1", 1],
      `ci_u.has_anc1` = anc_ci["has_anc1", 2],
      has_anc2 = as.numeric(anc_means["has_anc2"]),
      `ci_l.has_anc2` = anc_ci["has_anc2", 1],
      `ci_u.has_anc2` = anc_ci["has_anc2", 2],
      has_anc3 = as.numeric(anc_means["has_anc3"]),
      `ci_l.has_anc3` = anc_ci["has_anc3", 1],
      `ci_u.has_anc3` = anc_ci["has_anc3", 2],
      has_anc4 = as.numeric(anc_means["has_anc4"]),
      `ci_l.has_anc4` = anc_ci["has_anc4", 1],
      `ci_u.has_anc4` = anc_ci["has_anc4", 2],
      has_anc8 = as.numeric(anc_means["has_anc8"]),
      `ci_l.has_anc8` = anc_ci["has_anc8", 1],
      `ci_u.has_anc8` = anc_ci["has_anc8", 2]
    )
  }

  # Rename
  anc_results <- anc_results |>
    dplyr::rename(
      dhs_anc_1plus = has_anc1,
      dhs_anc_1plus_low = `ci_l.has_anc1`,
      dhs_anc_1plus_upp = `ci_u.has_anc1`,
      dhs_anc_2plus = has_anc2,
      dhs_anc_2plus_low = `ci_l.has_anc2`,
      dhs_anc_2plus_upp = `ci_u.has_anc2`,
      dhs_anc_3plus = has_anc3,
      dhs_anc_3plus_low = `ci_l.has_anc3`,
      dhs_anc_3plus_upp = `ci_u.has_anc3`,
      dhs_anc_4plus = has_anc4,
      dhs_anc_4plus_low = `ci_l.has_anc4`,
      dhs_anc_4plus_upp = `ci_u.has_anc4`,
      dhs_anc_8plus = has_anc8,
      dhs_anc_8plus_low = `ci_l.has_anc8`,
      dhs_anc_8plus_upp = `ci_u.has_anc8`
    )

  # Sample sizes
  if (!is.null(class_var)) {
    sample_sizes <- ir |>
      dplyr::group_by(.data[[class_var]]) |>
      dplyr::summarise(
        dhs_n_recent_births = dplyr::n(),
        .groups = "drop"
      )
    anc_results <- anc_results |>
      dplyr::left_join(sample_sizes, by = class_var)
  } else {
    anc_results$dhs_n_recent_births <- nrow(ir)
  }

  # Format
  anc_cols <- names(anc_results)[grepl("^dhs_anc_", names(anc_results))]
  anc_results <- anc_results |>
    dplyr::mutate(
      dplyr::across(dplyr::all_of(anc_cols), ~ round(.x, 2)),
      dplyr::across(dplyr::matches("_low$"), ~ pmax(0, .)),
      dplyr::across(dplyr::matches("_upp$"), ~ pmin(1, .)),
      dhs_n_recent_births = as.integer(dhs_n_recent_births)
    ) |>
    dplyr::select(-dplyr::any_of(c("level")))

  tibble::as_tibble(anc_results)
}

#' Calculate ANC Coverage from DHS Data (standardized long-format output)
#'
#' Computes ANC 1+/2+/3+/4+/8+ coverage indicators nationally and optionally
#' by subnational region, returning the standardized `list(adm0, adm1)` output.
#'
#' @inheritParams calc_anc_dhs_core
#' @param ci_method CI method for svyciprop. Default: "logit".
#'
#' @return Named list with `adm0` tibble and optionally `adm1` tibble in
#'   standardized long format.
#' @export
calc_anc_dhs <- function(
  dhs_ir,
  survey_vars = list(
    cluster = "v021",
    weight = "v005",
    stratum = "v022",
    interview_date = "v008",
    birth_date = "b3_01",
    anc_visits = "m14_1"
  ),
  birth_window_months = 24,
  region_var          = NULL,
  gps_data            = NULL,
  gps_vars            = list(cluster = "DHSCLUST", lat = "LATNUM", lon = "LONGNUM"),
  shapefile           = NULL,
  admin_level         = NULL,
  join_nearest        = TRUE,
  ci_method           = "logit"
) {
  # Fail fast on missing suggested dependencies
  .check_pkg(
    c("tibble"),
    reason = "for `calc_anc_dhs()`"
  )

  # ---- 1. Extract survey metadata (IR data uses v-prefix) ----
  survey_meta <- .extract_survey_meta(dhs_ir)

  # ---- 2. Prepare data via existing helper ----
  ir <- .prepare_anc_data(
    dhs_ir = dhs_ir,
    survey_vars = survey_vars,
    birth_window_months = birth_window_months,
    include_survey_vars = TRUE
  )

  if (is.null(ir) || nrow(ir) == 0) {
    cli::cli_abort("No eligible ANC data after preparation.")
  }

  # ---- 3. Compute indicators across admin levels ----
  .compute_dhs_indicators_with_admin(
    data               = ir,
    conditions         = .anc_conditions(),
    dhs_data           = dhs_ir,
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
# ANC conditions & dictionary
# =============================================================================

#' Internal: ANC indicator conditions
#'
#' Returns a list of indicator specifications for ANC coverage indicators.
#'
#' @return List of named lists, each with: indicator, indicator_code,
#'   indicator_title, denom_code, filter_expr, outcome_var, num_desc,
#'   denom_desc.
#' @noRd
.anc_conditions <- function() {
  denom <- "Women with recent birth (within birth_window_months)"
  list(
    list(
      indicator       = "ANC_1PLUS",
      indicator_code  = "anc_1plus",
      indicator_title = "ANC 1+ visit coverage",
      denom_code      = "recent_births",
      filter_expr     = NULL,
      outcome_var     = "has_anc1",
      num_desc        = "Women with 1+ ANC visits",
      denom_desc      = denom
    ),
    list(
      indicator       = "ANC_2PLUS",
      indicator_code  = "anc_2plus",
      indicator_title = "ANC 2+ visit coverage",
      denom_code      = "recent_births",
      filter_expr     = NULL,
      outcome_var     = "has_anc2",
      num_desc        = "Women with 2+ ANC visits",
      denom_desc      = denom
    ),
    list(
      indicator       = "ANC_3PLUS",
      indicator_code  = "anc_3plus",
      indicator_title = "ANC 3+ visit coverage",
      denom_code      = "recent_births",
      filter_expr     = NULL,
      outcome_var     = "has_anc3",
      num_desc        = "Women with 3+ ANC visits",
      denom_desc      = denom
    ),
    list(
      indicator       = "ANC_4PLUS",
      indicator_code  = "anc_4plus",
      indicator_title = "ANC 4+ visit coverage",
      denom_code      = "recent_births",
      filter_expr     = NULL,
      outcome_var     = "has_anc4",
      num_desc        = "Women with 4+ ANC visits",
      denom_desc      = denom
    ),
    list(
      indicator       = "ANC_8PLUS",
      indicator_code  = "anc_8plus",
      indicator_title = "ANC 8+ visit coverage",
      denom_code      = "recent_births",
      filter_expr     = NULL,
      outcome_var     = "has_anc8",
      num_desc        = "Women with 8+ ANC visits",
      denom_desc      = denom
    )
  )
}


#' ANC Indicator Dictionary
#'
#' Returns a tibble describing all ANC indicators computed by
#' \code{\link{calc_anc_dhs}}.
#'
#' @return Tibble with columns: indicator, indicator_code, indicator_title,
#'   numerator_description, denominator_description, denominator_code.
#' @keywords internal
anc_dictionary <- function() {
  conds <- .anc_conditions()
  tibble::tibble(
    indicator               = vapply(conds, `[[`, character(1), "indicator"),
    indicator_code          = vapply(conds, `[[`, character(1), "indicator_code"),
    indicator_title         = vapply(conds, `[[`, character(1), "indicator_title"),
    numerator_description   = vapply(conds, `[[`, character(1), "num_desc"),
    denominator_description = vapply(conds, `[[`, character(1), "denom_desc"),
    denominator_code        = vapply(conds, `[[`, character(1), "denom_code")
  )
}
