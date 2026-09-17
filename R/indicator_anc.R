# anc indicator
#
# Merged from: dhs_calc_anc.R dhs_calc_anc_mbg.R dhs_helpers_anc.R
# Contains the survey-weighted calc, MBG cluster-prep, and indicator-
# specific helpers for this family.

# ---- dhs_calc_anc.R ----

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
#' @noRd
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


# ---- dhs_calc_anc_mbg.R ----

#' Prepare ANC Data for MBG Analysis
#'
#' Prepares cluster-level Antenatal Care (ANC) attendance data for Model-Based
#' Geostatistics (MBG) analysis. Calculates the proportion of women who had
#' at least N ANC visits during their most recent pregnancy.
#'
#' @details
#' Methodology: \url{https://github.com/ahadi-analytics/sntmethods/blob/master/inst/methods/anc_dhs.yml}
#'
#' @param dhs_ir DHS Individual Recode dataset.
#' @param gps_data DHS GPS dataset with cluster coordinates.
#' @param indicators Character vector of indicators to calculate:
#'   \itemize{
#'     \item "anc_1plus": At least 1 ANC visit
#'     \item "anc_2plus": At least 2 ANC visits
#'     \item "anc_3plus": At least 3 ANC visits
#'     \item "anc_4plus": At least 4 ANC visits
#'     \item "anc_8plus": At least 8 ANC visits (2016 WHO recommendation)
#'   }
#'   Default: c("anc_1plus", "anc_2plus", "anc_3plus", "anc_4plus").
#' @param birth_window_months Number of months to look back for births.
#'   Default: 36 (3 years). Max 60 (5 years).
#' @param survey_vars Named list mapping DHS variable names.
#' @param gps_vars Named list for GPS variable mapping.
#'
#' @return A list of data.tables (one per indicator), each with columns:
#'   \itemize{
#'     \item cluster_id: Cluster identifier
#'     \item indicator: Number of women meeting threshold
#'     \item samplesize: Total number of women with recent births
#'     \item x: Longitude
#'     \item y: Latitude
#'   }
#'
#' @details
#' This function uses data on most recent births within the specified window.
#' ANC visits are measured using m14 (number of antenatal visits).
#'
#' @examples
#' \dontrun{
#' anc_mbg <- calc_anc_mbg(
#'   dhs_ir = ir_data,
#'   gps_data = gps_data,
#'   indicators = c("anc_1plus", "anc_4plus")
#' )
#' }
#'
#' @export
calc_anc_mbg <- function(
  dhs_ir,
  gps_data,
  indicators = c("anc_1plus", "anc_2plus", "anc_3plus", "anc_4plus"),
  birth_window_months = 36,
  survey_vars = list(
    cluster = "v001",
    interview_date = "v008",
    birth_date = "b3_01",
    anc_visits = "m14_1"
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

  valid_indicators <- c("anc_1plus", "anc_2plus", "anc_3plus", "anc_4plus", "anc_8plus")
  invalid <- setdiff(indicators, valid_indicators)
  if (length(invalid) > 0) {
    cli::cli_abort("Invalid indicators: {.val {invalid}}")
  }

  # ---- Prepare GPS data ----

  gps_clean <- .prepare_gps_data(gps_data, gps_vars)

  # ---- Prepare IR data ----

  ir <- .prepare_anc_data(
    dhs_ir, survey_vars, birth_window_months,
    include_survey_vars = FALSE
  )
  if (is.null(ir)) return(NULL)

  # ---- Aggregate to cluster level ----

  indicator_map <- list(
    anc_1plus = "has_anc1",
    anc_2plus = "has_anc2",
    anc_3plus = "has_anc3",
    anc_4plus = "has_anc4",
    anc_8plus = "has_anc8"
  )

  results <- list()

  for (ind in indicators) {
    cluster_dt <- .aggregate_to_mbg_clusters(
      ir, indicator_map[[ind]], gps_clean, ind
    )
    if (!is.null(cluster_dt)) {
      results[[ind]] <- cluster_dt
    }
  }

  if (length(results) == 0) return(NULL)

  results
}


#' Prepare Single ANC Indicator for MBG
#'
#' Simplified function to prepare a single ANC indicator.
#'
#' @inheritParams calc_anc_mbg
#' @param threshold Minimum number of ANC visits (1, 4, or 8). Default: 4.
#'
#' @return A data.table with columns: cluster_id, indicator, samplesize, x, y
#'
#' @export
prep_anc_mbg <- function(
  dhs_ir,
  gps_data,
  threshold = 4,
  birth_window_months = 36,
  survey_vars = list(
    cluster = "v001",
    interview_date = "v008",
    birth_date = "b3_01",
    anc_visits = "m14_1"
  ),
  gps_vars = list(
    cluster = "DHSCLUST",
    lat = "LATNUM",
    lon = "LONGNUM"
  )
) {
  indicator_name <- paste0("anc_", threshold, "plus")

  result <- calc_anc_mbg(
    dhs_ir = dhs_ir,
    gps_data = gps_data,
    indicators = indicator_name,
    birth_window_months = birth_window_months,
    survey_vars = survey_vars,
    gps_vars = gps_vars
  )

  result[[1]]
}


# ---- dhs_helpers_anc.R ----

#' Prepare ANC Data for Analysis
#'
#' Shared data cleaning and indicator computation for ANC functions.
#' Used by both calc_anc_dhs_core() and calc_anc_mbg().
#'
#' @param dhs_ir DHS Individual Recode dataset.
#' @param survey_vars Named list mapping DHS variable names.
#' @param birth_window_months Months to look back for births.
#' @param include_survey_vars Logical. If TRUE, includes survey design columns.
#'
#' @return A data frame of eligible women with columns:
#'   cluster_id, anc_visits, and binary indicators:
#'   has_anc1, has_anc2, has_anc3, has_anc4, has_anc8.
#'   If include_survey_vars = TRUE, also: survey_weight, stratum_id.
#'
#' @noRd
.prepare_anc_data <- function(
  dhs_ir,
  survey_vars,
  birth_window_months = 36,
  include_survey_vars = FALSE
) {
  if (!is.data.frame(dhs_ir)) {
    cli::cli_abort("`dhs_ir` must be a data.frame or tibble")
  }
  if (nrow(dhs_ir) == 0) {
    cli::cli_abort("`dhs_ir` is empty.")
  }

  if (birth_window_months < 1 || birth_window_months > 60) {
    cli::cli_abort("`birth_window_months` must be between 1 and 60")
  }

  # Check required columns
  required_cols <- c(survey_vars$cluster, survey_vars$interview_date,
                     survey_vars$birth_date, survey_vars$anc_visits)
  if (include_survey_vars) {
    required_cols <- c(required_cols, survey_vars$weight, survey_vars$stratum)
  }
  missing_cols <- setdiff(required_cols, names(dhs_ir))
  if (length(missing_cols) > 0) {
    cli::cli_warn(
      "ANC required columns not found: {.var {missing_cols}}; ANC not available for this survey"
    )
    return(NULL)
  }

  ir <- dhs_ir |>
    dplyr::mutate(dplyr::across(dplyr::everything(), haven::zap_labels)) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), as.vector)) |>
    dplyr::mutate(
      cluster_id = .data[[survey_vars$cluster]],
      interview_cmc = .data[[survey_vars$interview_date]],
      birth_cmc = .data[[survey_vars$birth_date]],
      anc_visits = .data[[survey_vars$anc_visits]]
    )

  if (include_survey_vars) {
    ir <- ir |>
      dplyr::mutate(
        survey_weight = .data[[survey_vars$weight]] / 1e6,
        stratum_id = .data[[survey_vars$stratum]]
      )
  }

  # Filter to recent births
  ir <- ir |>
    dplyr::filter(
      !is.na(birth_cmc),
      !is.na(interview_cmc)
    ) |>
    dplyr::mutate(
      months_since_birth = interview_cmc - birth_cmc
    ) |>
    dplyr::filter(
      months_since_birth >= 0,
      months_since_birth <= birth_window_months
    )

  # Filter valid ANC responses (98 = "don't know")
  ir <- ir |>
    dplyr::filter(
      !is.na(anc_visits),
      anc_visits < 98
    )

  if (nrow(ir) == 0) {
    cli::cli_abort("No eligible women with valid ANC data found")
  }

  cli::cli_alert_info(
    "Found {format(nrow(ir), big.mark = ',')} women with births in last {birth_window_months} months"
  )

  # Calculate indicators
  ir <- ir |>
    dplyr::mutate(
      has_anc1 = as.integer(anc_visits >= 1),
      has_anc2 = as.integer(anc_visits >= 2),
      has_anc3 = as.integer(anc_visits >= 3),
      has_anc4 = as.integer(anc_visits >= 4),
      has_anc8 = as.integer(anc_visits >= 8)
    )

  ir
}


