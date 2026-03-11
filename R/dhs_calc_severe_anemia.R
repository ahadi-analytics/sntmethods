#' Calculate Severe Anemia Prevalence from DHS Data (Core Function)
#'
#' Core function that estimates severe anemia prevalence (Hb < 8.0 g/dL) among
#' children aged 6-59 months using standard DHS methodology. This indicator
#' represents clinically significant anemia requiring medical attention.
#'
#' Note: Most users should use `calc_severe_anemia_dhs()` instead, which
#' provides additional spatial aggregation capabilities and data dictionary
#' support.
#'
#' @details
#' Methodology: \url{https://github.com/ahadi-analytics/sntmethods/blob/master/inst/methods/anemia_dhs.yml}
#'
#' @param dhs_pr DHS Person Records dataset in tidy format (data.frame or
#'   tibble).
#' @param survey_vars Named list mapping DHS variable names. Required keys:
#'   \itemize{
#'     \item `cluster`: Cluster ID (default: "hv001")
#'     \item `weight`: Survey weight (default: "hv005", divided by 1,000,000)
#'     \item `stratum`: Explicit stratum variable if available (default: "hv022")
#'     \item `adm1`: First administrative level (default: "hv024")
#'     \item `adm2`: Second administrative level (default: NULL)
#'     \item `age`: Child's age in months (default: "hc1")
#'     \item `hemoglobin`: Raw hemoglobin in tenths of g/dL (default: "hc56")
#'     \item `hemoglobin_adj`: Altitude-adjusted hemoglobin (default: "hw53")
#'     \item `present`: Present in household (1=yes, default: "hv103")
#'     \item `mother`: Mother listed in household (1=yes, default: "hv042")
#'   }
#' @param hb_threshold Hemoglobin threshold in g/dL for severe anemia
#'   (default: 8.0). Children with Hb < threshold are classified as severely
#'   anemic.
#' @param altitude_adjusted Logical. If TRUE (default), uses altitude-adjusted
#'   hemoglobin variable (hw53). If FALSE, uses raw hemoglobin (hc56).
#'   WHO recommends altitude adjustment for surveys in regions above 1000m.
#' @param gps_data Optional DHS GPS dataset. If provided, results are
#'   cluster-level.
#' @param gps_vars Named list for GPS variables (cluster, lat, lon).
#'
#' @return A tibble with severe anemia estimates, confidence intervals, and
#'   sample sizes. Columns depend on whether GPS data is provided (cluster-level
#'   vs admin-level).
#'
#' @details
#' DHS stores hemoglobin values in tenths of g/dL (e.g., 80 = 8.0 g/dL).
#' The function handles this conversion automatically.
#'
#' Severe anemia (Hb < 8.0 g/dL) is clinically significant and typically
#' requires medical intervention. This differs from:
#' \itemize{
#'   \item Any anemia: Hb < 11.0 g/dL
#'   \item Moderate anemia: Hb 7.0-9.9 g/dL
#'   \item Mild anemia: Hb 10.0-10.9 g/dL
#' }
#'
#' \strong{Altitude Adjustment:}
#' The WHO recommends adjusting hemoglobin values for altitude to account
#' for physiological adaptation to lower oxygen at higher elevations. When
#' `altitude_adjusted = TRUE`, the function uses the pre-computed altitude-
#' adjusted variable (hw53) from DHS. This is particularly important for
#' surveys in highland areas.
#'
#' @examples
#' # minimal example (structure only)
#' # anemia <- calc_severe_anemia_dhs_core(
#' #   dhs_pr = pr_data,
#' #   altitude_adjusted = TRUE  # Use altitude-adjusted Hb (default)
#' # )
#' #
#' # # Use raw hemoglobin (no altitude adjustment)
#' # anemia <- calc_severe_anemia_dhs_core(
#' #   dhs_pr = pr_data,
#' #   altitude_adjusted = FALSE
#' # )
#'
#' @export
calc_severe_anemia_dhs_core <- function(
  dhs_pr,
  survey_vars = list(
    cluster = "hv001",
    weight = "hv005",
    stratum = "hv022",
    adm1 = "hv024",
    adm2 = NULL,
    age = "hc1",
    hemoglobin = "hc56",
    hemoglobin_adj = "hw53",
    present = "hv103",
    mother = "hv042"
  ),
  hb_threshold = 8.0,
  altitude_adjusted = TRUE,
  gps_data = NULL,
  gps_vars = list(
    cluster = "DHSCLUST",
    lat = "LATNUM",
    lon = "LONGNUM"
  )
) {
  # ---- 1. Input validation ------------------------------------------------

  if (!is.data.frame(dhs_pr)) {
    cli::cli_abort("dhs_pr must be a data.frame or tibble")
  }
  if (nrow(dhs_pr) == 0) {
    cli::cli_abort("dhs_pr is empty")
  }

  # Required mapping in survey_vars
  needed <- c(
    "cluster",
    "weight",
    "age",
    "hemoglobin",
    "present",
    "mother"
  )

  if (!all(needed %in% names(survey_vars))) {
    missing <- setdiff(needed, names(survey_vars))
    cli::cli_abort("`survey_vars` must include: {missing}")
  }

  # Check that mapped columns exist in data
  mapped_cols <- unlist(survey_vars[needed])
  mapped_cols <- mapped_cols[!is.null(mapped_cols)]
  missing_cols <- setdiff(mapped_cols, names(dhs_pr))

  if (length(missing_cols) > 0) {
    cli::cli_abort(
      "Columns not found in dhs_pr: {missing_cols}. Check survey_vars mapping."
    )
  }

  # Convert threshold to tenths (DHS stores Hb in tenths of g/dL)
  hb_threshold_tenths <- hb_threshold * 10

  # ---- 1b. Select hemoglobin variable based on altitude_adjusted -----------

  if (altitude_adjusted) {
    hb_var <- survey_vars$hemoglobin_adj %||% "hw53"
    if (!hb_var %in% names(dhs_pr)) {
      cli::cli_abort(
        c(
          "Altitude-adjusted hemoglobin variable `{hb_var}` not found in data.",
          "i" = "Set `altitude_adjusted = FALSE` to use raw hemoglobin ({survey_vars$hemoglobin})"
        )
      )
    }
    cli::cli_alert_info("Using altitude-adjusted hemoglobin: {hb_var}")
  } else {
    hb_var <- survey_vars$hemoglobin %||% "hc56"
    if (!hb_var %in% names(dhs_pr)) {
      cli::cli_abort("Raw hemoglobin variable `{hb_var}` not found in data.")
    }
    cli::cli_alert_info("Using raw hemoglobin (not altitude-adjusted): {hb_var}")
  }

  cli::cli_alert_info(
    "Using severe anemia threshold: Hb < {hb_threshold} g/dL ({hb_threshold_tenths} in DHS units)"
  )

  # Admin availability
  has_adm1 <- "adm1" %in% names(survey_vars) &&
    !is.na(survey_vars$adm1) &&
    survey_vars$adm1 %in% names(dhs_pr)

  has_adm2 <- "adm2" %in% names(survey_vars) &&
    !is.null(survey_vars$adm2) &&
    survey_vars$adm2 %in% names(dhs_pr)

  # ---- 2. Prepare base dataset --------------------------------------------

  # Override hemoglobin variable based on altitude_adjusted setting
  helper_survey_vars <- survey_vars
  helper_survey_vars$hemoglobin <- hb_var

  pr <- .prepare_anemia_data(
    dhs_pr = dhs_pr,
    survey_vars = helper_survey_vars,
    age_min = 6,
    age_max = 59,
    include_survey_vars = TRUE
  )

  use_strata <- dplyr::n_distinct(pr$stratum_id) > 1

  # ---- 3. Create severe anemia indicators ---------------------------------
  # The helper already filtered to eligible children with valid Hb and created

  # hemoglobin in g/dL. Now create tested_hb and severe_anemia for survey design.

  pr <- pr |>
    dplyr::mutate(
      tested_hb = 1,  # All rows from helper are eligible with valid Hb
      severe_anemia = as.numeric(hemoglobin < hb_threshold)
    )

  # Additional anemia severity indicators (WHO thresholds, g/dL)
  pr <- pr |>
    dplyr::mutate(
      anemia_any = as.integer(hemoglobin < 11),
      anemia_moderate_plus = as.integer(hemoglobin < 10),
      anemia_mild_only = as.integer(hemoglobin >= 10 & hemoglobin < 11),
      anemia_moderate_only = as.integer(hemoglobin >= hb_threshold & hemoglobin < 10),
      anemia_severe_only = as.integer(hemoglobin < hb_threshold)
    )

  # Filter to tested children (all from helper are already tested)
  pr_tested <- pr

  n_severe <- sum(pr_tested$severe_anemia == 1, na.rm = TRUE)
  cli::cli_alert_info(
    "Found {format(nrow(pr_tested), big.mark = ',')} children with valid Hb measurements; {format(n_severe, big.mark = ',')} ({round(n_severe/nrow(pr_tested)*100, 1)}%) with severe anemia"
  )

  # ---- 5. Survey design ---------------------------------------------------

  # Handle single-PSU strata
  survey_options <- options(survey.lonely.psu = "adjust")
  on.exit(options(survey_options), add = TRUE)

  if (use_strata) {
    des <- survey::svydesign(
      ids = ~cluster_id,
      strata = ~stratum_id,
      weights = ~survey_weight,
      data = pr_tested,
      nest = TRUE
    )
  } else {
    des <- survey::svydesign(
      ids = ~cluster_id,
      weights = ~survey_weight,
      data = pr_tested,
      nest = TRUE
    )
  }

  # ---- 6. Grouping logic --------------------------------------------------

  if (!is.null(gps_data)) {
    group_vars <- "cluster_id"
  } else if (has_adm2) {
    group_vars <- c("adm1", "adm2")
  } else if (has_adm1) {
    group_vars <- "adm1"
  } else {
    cli::cli_abort("No admin fields available for grouping.")
  }

  group_formula <- stats::as.formula(
    paste("~", paste(group_vars, collapse = " + "))
  )

  # ---- 7. Calculate severe anemia prevalence ------------------------------

  anemia_results <- survey::svyby(
    ~severe_anemia,
    by = group_formula,
    design = des,
    FUN = survey::svymean,
    vartype = "ci",
    keep.names = FALSE
  ) |>
    tibble::as_tibble() |>
    dplyr::rename(
      dhs_severe_anemia = severe_anemia,
      dhs_severe_anemia_low = ci_l,
      dhs_severe_anemia_upp = ci_u
    ) |>
    dplyr::mutate(
      dhs_severe_anemia = round(dhs_severe_anemia, 2),
      dhs_severe_anemia_low = pmax(0, round(dhs_severe_anemia_low, 2)),
      dhs_severe_anemia_upp = pmin(1, round(dhs_severe_anemia_upp, 2))
    )

  # ---- 7b. Calculate additional anemia severity indicators ----------------

  additional_anemia_vars <- c(
    "anemia_any", "anemia_moderate_plus",
    "anemia_mild_only", "anemia_moderate_only", "anemia_severe_only"
  )

  for (avar in additional_anemia_vars) {
    afml <- stats::as.formula(paste("~", avar))
    aresult <- survey::svyby(
      afml,
      by = group_formula,
      design = des,
      FUN = survey::svymean,
      vartype = "ci",
      keep.names = FALSE
    ) |>
      tibble::as_tibble() |>
      dplyr::rename(
        !!paste0("dhs_", avar) := !!avar,
        !!paste0("dhs_", avar, "_low") := ci_l,
        !!paste0("dhs_", avar, "_upp") := ci_u
      ) |>
      dplyr::mutate(
        dplyr::across(
          dplyr::starts_with("dhs_"),
          ~ round(.x, 2)
        ),
        dplyr::across(dplyr::matches("_low$"), ~ pmax(0, .)),
        dplyr::across(dplyr::matches("_upp$"), ~ pmin(1, .))
      )

    anemia_results <- anemia_results |>
      dplyr::left_join(aresult, by = group_vars)
  }

  # ---- 8. Calculate sample sizes ------------------------------------------

  denom <- survey::svyby(
    ~ tested_hb + severe_anemia,
    by = group_formula,
    design = des,
    FUN = survey::svytotal,
    keep.names = TRUE
  ) |>
    tibble::as_tibble() |>
    dplyr::rename(
      dhs_n_tested_hb = tested_hb,
      dhs_n_severe_anemia = severe_anemia
    ) |>
    dplyr::mutate(
      dhs_n_tested_hb = as.integer(round(dhs_n_tested_hb)),
      dhs_n_severe_anemia = as.integer(round(dhs_n_severe_anemia))
    )

  # ---- 9. Merge results ---------------------------------------------------

  anemia_final <- anemia_results |>
    dplyr::left_join(
      denom,
      by = group_vars
    )

  # ---- 10. Attach GPS coordinates if provided -----------------------------

  if (!is.null(gps_data)) {
    anemia_final <- join_dhs_coords(
      pr_data = anemia_final,
      gps_data = gps_data,
      pr_vars = list(cluster = "cluster_id"),
      gps_vars = gps_vars
    )
  }

  anemia_final
}

#' Calculate Severe Anemia Prevalence from DHS Data (Standardized)
#'
#' Computes anemia indicators from DHS Person Records (PR) data.
#' Returns survey-weighted proportions with logit confidence intervals
#' in standardized long format.
#'
#' @details
#' Computes six anemia indicators following WHO thresholds. See
#' [severe_anemia_dictionary()] for the full indicator list.
#'
#' @param dhs_pr DHS Person Records dataset (data.frame or tibble).
#' @param survey_vars Named list mapping DHS variable names. Required keys:
#'   \itemize{
#'     \item `cluster`: Cluster ID (default: "hv001")
#'     \item `weight`: Survey weight (default: "hv005", divided by 1,000,000)
#'     \item `stratum`: Stratum variable (default: "hv022")
#'     \item `adm1`: First administrative level (default: "hv024")
#'     \item `age`: Child's age in months (default: "hc1")
#'     \item `hemoglobin`: Raw hemoglobin in tenths of g/dL (default: "hc56")
#'     \item `hemoglobin_adj`: Altitude-adjusted hemoglobin (default: "hw53")
#'     \item `present`: Present in household (default: "hv103")
#'     \item `mother`: Mother listed in household (default: "hv042")
#'   }
#' @param altitude_adjusted Logical. If TRUE (default), uses altitude-adjusted
#'   hemoglobin variable (hw53). If FALSE, uses raw hemoglobin (hc56).
#' @param region_var Optional column name for subnational grouping (e.g.,
#'   "hv024"). If NULL, defaults to survey_vars$adm1.
#' @param ci_method Method for confidence intervals. Default: "logit".
#'
#' @return Named list of tibbles:
#'   \describe{
#'     \item{`adm0`}{National-level estimates (always present)}
#'     \item{`adm1`}{Admin-1 estimates (when `region_var` or adm1 available)}
#'   }
#'   Each tibble contains columns: survey_id, iso3, iso2, survey_type,
#'   survey_year, adm0, [adm1], type, geo_source, point, ci_l, ci_u,
#'   numerator, denominator, indicator, indicator_code,
#'   numerator_description, denominator_description, denominator_code.
#'
#' @examples
#' \dontrun{
#' anemia <- calc_severe_anemia_dhs(dhs_pr = pr_data, region_var = "hv024")
#' anemia$adm0
#' anemia$adm1
#' }
#'
#' @seealso [severe_anemia_dictionary()] for indicator metadata,
#'   [calc_severe_anemia_dhs_core()] for legacy wide-format output
#' @export
calc_severe_anemia_dhs <- function(
  dhs_pr,
  survey_vars = list(
    cluster = "hv001",
    weight = "hv005",
    stratum = "hv022",
    adm1 = "hv024",
    adm2 = NULL,
    age = "hc1",
    hemoglobin = "hc56",
    hemoglobin_adj = "hw53",
    present = "hv103",
    mother = "hv042"
  ),
  altitude_adjusted = TRUE,
  region_var = NULL,
  ci_method = "logit"
) {
  # ---- 1. Extract survey metadata (PR data = hv-prefix) ----
  survey_meta <- .extract_survey_meta_hv(dhs_pr)

  # ---- 2. Select hemoglobin variable based on altitude_adjusted ----
  if (altitude_adjusted) {
    hb_var <- survey_vars$hemoglobin_adj %||% "hw53"
    if (!hb_var %in% names(dhs_pr)) {
      cli::cli_abort(c(
        "Altitude-adjusted hemoglobin variable `{hb_var}` not found in data.",
        "i" = "Set `altitude_adjusted = FALSE` to use raw hemoglobin ({survey_vars$hemoglobin})"
      ))
    }
    cli::cli_alert_info("Using altitude-adjusted hemoglobin: {hb_var}")
  } else {
    hb_var <- survey_vars$hemoglobin %||% "hc56"
    if (!hb_var %in% names(dhs_pr)) {
      cli::cli_abort("Raw hemoglobin variable `{hb_var}` not found in data.")
    }
    cli::cli_alert_info("Using raw hemoglobin (not altitude-adjusted): {hb_var}")
  }

  # ---- 3. Prepare dataset ----
  helper_survey_vars <- survey_vars
  helper_survey_vars$hemoglobin <- hb_var

  pr <- .prepare_anemia_data(
    dhs_pr = dhs_pr,
    survey_vars = helper_survey_vars,
    age_min = 6,
    age_max = 59,
    include_survey_vars = TRUE
  )

  if (is.null(pr) || nrow(pr) == 0) {
    cli::cli_abort("No eligible children with valid Hb measurements found.")
  }

  cli::cli_alert_success(
    "Children 6-59 months with valid Hb: {format(nrow(pr), big.mark = ',')} children"
  )

  # ---- 4. Resolve region labels ----
  group_var <- NULL
  geo_src <- NA_character_

  # Determine which region variable to use
  effective_region_var <- region_var
  if (is.null(effective_region_var)) {
    # Default to survey_vars$adm1 if available
    adm1_var <- survey_vars$adm1 %||% "hv024"
    if (adm1_var %in% names(dhs_pr)) {
      effective_region_var <- adm1_var
    }
  }

  if (!is.null(effective_region_var)) {
    if (!effective_region_var %in% names(dhs_pr)) {
      cli::cli_abort(
        "Column {.var {effective_region_var}} not found in `dhs_pr`."
      )
    }
    # Build lookup from full dataset, then apply to filtered subset
    resolved_all <- .resolve_region_labels(
      dhs_pr[[effective_region_var]], effective_region_var
    )
    raw_all <- as.character(as.vector(
      haven::zap_labels(dhs_pr[[effective_region_var]])
    ))
    lookup <- stats::setNames(resolved_all, raw_all)
    pr_raw <- as.character(pr[[effective_region_var]])
    pr$region <- unname(lookup[pr_raw])
    group_var <- "region"
    geo_src <- "survey"
  }

  # ---- 5. Get conditions ----
  conditions <- .severe_anemia_conditions()

  # ---- 6. Compute national results ----
  national_results <- purrr::map_dfr(conditions, function(cond) {
    .compute_dhs_indicator_generic(
      data = pr,
      condition = cond,
      group_var = NULL,
      ci_method = ci_method
    )
  })

  # ---- 7. Compute regional results ----
  regional_results <- tibble::tibble()
  if (!is.null(group_var)) {
    regional_results <- purrr::map_dfr(conditions, function(cond) {
      .compute_dhs_indicator_generic(
        data = pr,
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

  # ---- 8. Assemble output ----
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

#' Internal: Severe anemia indicator conditions
#'
#' Uses the outcome columns created by `.prepare_anemia_data()`:
#' has_severe, has_any_anemia, has_moderate_plus, has_mild_only,
#' has_moderate_only, has_severe_only.
#'
#' @return List of named lists, each with indicator specification.
#' @noRd
.severe_anemia_conditions <- function() {
  denom <- "Children 6-59 months tested for Hb"
  list(
    list(
      indicator       = "SEVERE_ANEMIA",
      indicator_code  = "severe_anemia",
      indicator_title = "Severe anemia prevalence (Hb < 8 g/dL)",
      denom_code      = "hb_tested_6_59m",
      filter_expr     = NULL,
      outcome_var     = "has_severe",
      num_desc        = "Children with Hb < 8.0 g/dL",
      denom_desc      = denom
    ),
    list(
      indicator       = "ANEMIA_ANY",
      indicator_code  = "anemia_any",
      indicator_title = "Any anemia prevalence (Hb < 11 g/dL)",
      denom_code      = "hb_tested_6_59m",
      filter_expr     = NULL,
      outcome_var     = "has_any_anemia",
      num_desc        = "Children with Hb < 11.0 g/dL",
      denom_desc      = denom
    ),
    list(
      indicator       = "ANEMIA_MODERATE_PLUS",
      indicator_code  = "anemia_moderate_plus",
      indicator_title = "Moderate-plus anemia prevalence (Hb < 10 g/dL)",
      denom_code      = "hb_tested_6_59m",
      filter_expr     = NULL,
      outcome_var     = "has_moderate_plus",
      num_desc        = "Children with Hb < 10.0 g/dL",
      denom_desc      = denom
    ),
    list(
      indicator       = "ANEMIA_MILD_ONLY",
      indicator_code  = "anemia_mild_only",
      indicator_title = "Mild anemia only prevalence (10 <= Hb < 11 g/dL)",
      denom_code      = "hb_tested_6_59m",
      filter_expr     = NULL,
      outcome_var     = "has_mild_only",
      num_desc        = "Children with 10.0 <= Hb < 11.0 g/dL",
      denom_desc      = denom
    ),
    list(
      indicator       = "ANEMIA_MODERATE_ONLY",
      indicator_code  = "anemia_moderate_only",
      indicator_title = "Moderate anemia only prevalence (8 <= Hb < 10 g/dL)",
      denom_code      = "hb_tested_6_59m",
      filter_expr     = NULL,
      outcome_var     = "has_moderate_only",
      num_desc        = "Children with 8.0 <= Hb < 10.0 g/dL",
      denom_desc      = denom
    ),
    list(
      indicator       = "ANEMIA_SEVERE_ONLY",
      indicator_code  = "anemia_severe_only",
      indicator_title = "Severe anemia only prevalence (Hb < 8 g/dL)",
      denom_code      = "hb_tested_6_59m",
      filter_expr     = NULL,
      outcome_var     = "has_severe_only",
      num_desc        = "Children with Hb < 8.0 g/dL",
      denom_desc      = denom
    )
  )
}


#' Severe Anemia Indicator Dictionary
#'
#' Returns the full dictionary of severe anemia indicators with metadata.
#'
#' @return Tibble with columns: indicator, indicator_code, indicator_title,
#'   numerator_description, denominator_description, denominator_code.
#'
#' @examples
#' severe_anemia_dictionary()
#'
#' @export
severe_anemia_dictionary <- function() {
  conds <- .severe_anemia_conditions()
  tibble::tibble(
    indicator               = vapply(conds, `[[`, character(1), "indicator"),
    indicator_code          = vapply(conds, `[[`, character(1), "indicator_code"),
    indicator_title         = vapply(conds, `[[`, character(1), "indicator_title"),
    numerator_description   = vapply(conds, `[[`, character(1), "num_desc"),
    denominator_description = vapply(conds, `[[`, character(1), "denom_desc"),
    denominator_code        = vapply(conds, `[[`, character(1), "denom_code")
  )
}

#' Aggregate Cluster-level Severe Anemia to Administrative Levels
#'
#' Helper function to aggregate cluster-level severe anemia results from
#' `calc_severe_anemia_dhs_core()` to administrative levels using a shapefile.
#' Performs spatial joins and calculates weighted or unweighted averages by
#' administrative unit.
#'
#' @param cluster_results Cluster-level results from `calc_severe_anemia_dhs_core()`
#'   containing dhs_severe_anemia, dhs_n_tested_hb, lat, and lon columns.
#' @param shapefile SF object with administrative boundaries containing columns
#'   named "adm0", "adm1", "adm2", etc.
#' @param admin_level Character vector of admin levels to aggregate to
#'   (e.g., `c("adm1")` or `c("adm1", "adm2")`).
#' @param weighted Logical. If `TRUE` (default), uses sample size weighted
#'   averaging. If `FALSE`, uses simple unweighted mean.
#'
#' @return SF object with aggregated severe anemia by administrative level,
#'   including geometry for mapping.
#'
#' @export
aggregate_severe_anemia_admin <- function(
  cluster_results,
  shapefile,
  admin_level = c("adm1"),
  weighted = TRUE
) {
  if (!inherits(cluster_results, "sf")) {
    cluster_sf <- cluster_results |>
      sf::st_as_sf(
        coords = c("lon", "lat"),
        crs = 4326,
        remove = FALSE
      )
  } else {
    cluster_sf <- cluster_results
  }

  shapefile <- shapefile |>
    sf::st_transform(4326) |>
    sf::st_make_valid()

  joined <- sf::st_join(
    cluster_sf,
    shapefile[, c(admin_level, "geometry")],
    join = sf::st_within,
    left = TRUE
  )

  unmatched <- is.na(joined[[admin_level[1]]])

  if (any(unmatched)) {
    nearest_idx <- sf::st_nearest_feature(
      joined[unmatched, ],
      shapefile
    )

    for (col in admin_level) {
      joined[unmatched, col] <- shapefile[[col]][nearest_idx]
    }
  }

  joined_df <- sf::st_drop_geometry(joined)

  # Identify additional anemia indicator columns if present
  additional_anemia_point <- grep(
    "^dhs_anemia_", names(joined_df), value = TRUE
  )
  additional_anemia_point <- additional_anemia_point[
    !grepl("_(low|upp)$", additional_anemia_point)
  ]

  if (weighted) {
    aggregated <- joined_df |>
      dplyr::group_by(
        dplyr::across(dplyr::all_of(admin_level))
      ) |>
      dplyr::summarise(
        dhs_severe_anemia = stats::weighted.mean(
          dhs_severe_anemia,
          w = dhs_n_tested_hb,
          na.rm = TRUE
        ),
        dplyr::across(
          dplyr::all_of(additional_anemia_point),
          ~ stats::weighted.mean(.x, w = dhs_n_tested_hb, na.rm = TRUE)
        ),
        dhs_n_tested_hb = sum(dhs_n_tested_hb, na.rm = TRUE),
        dhs_n_severe_anemia = sum(dhs_n_severe_anemia, na.rm = TRUE),
        dhs_n_clusters = dplyr::n(),
        .groups = "drop"
      )
  } else {
    aggregated <- joined_df |>
      dplyr::group_by(
        dplyr::across(dplyr::all_of(admin_level))
      ) |>
      dplyr::summarise(
        dhs_severe_anemia = mean(dhs_severe_anemia, na.rm = TRUE),
        dplyr::across(
          dplyr::all_of(additional_anemia_point),
          ~ mean(.x, na.rm = TRUE)
        ),
        dhs_n_tested_hb = sum(dhs_n_tested_hb, na.rm = TRUE),
        dhs_n_severe_anemia = sum(dhs_n_severe_anemia, na.rm = TRUE),
        dhs_n_clusters = dplyr::n(),
        .groups = "drop"
      )
  }

  aggregated <- aggregated |>
    dplyr::mutate(
      dhs_severe_anemia = round(dhs_severe_anemia, 1),
      dplyr::across(
        dplyr::all_of(additional_anemia_point),
        ~ round(.x, 1)
      ),
      dhs_n_tested_hb = as.integer(dhs_n_tested_hb),
      dhs_n_severe_anemia = as.integer(dhs_n_severe_anemia)
    )

  # Detect and preserve admin name columns
  admin_name_cols <- paste0(admin_level, "_name")
  admin_name_cols <- admin_name_cols[admin_name_cols %in% names(shapefile)]
  all_admin_cols <- c(admin_level, admin_name_cols)

  result_with_geometry <- shapefile |>
    dplyr::select(dplyr::all_of(all_admin_cols)) |>
    dplyr::distinct() |>
    dplyr::left_join(
      aggregated,
      by = admin_level
    )

  result_with_geometry
}
