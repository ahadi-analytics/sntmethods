# =============================================================================
# Shared DHS survey-weighted indicator computation helpers
# =============================================================================

#' Compute a single DHS indicator in standardized long format
#'
#' Generic helper used by all DHS indicator functions. Applies optional filtering,
#' creates survey design, computes svyciprop nationally and optionally by region,
#' and returns a standardized tibble with indicator metadata.
#'
#' @param data Prepared dataset with standardized column names:
#'   `cluster_id`, `stratum_id`, `survey_weight`, and the outcome column.
#' @param condition Named list with indicator specification:
#'   \itemize{
#'     \item `indicator_title`: Human-readable indicator name
#'     \item `indicator_code`: Short code (e.g., "fever")
#'     \item `denom_code`: Denominator code
#'     \item `filter_expr`: Quoted filter expression or NULL
#'     \item `outcome_var`: Column name of the binary 0/1 outcome
#'     \item `num_desc`: Numerator description
#'     \item `denom_desc`: Denominator description
#'   }
#' @param group_var Optional grouping variable name for regional estimates.
#' @param subnational_level Admin level name for grouped rows (e.g., "adm1").
#' @param ci_method CI method for svyciprop. Default: "logit".
#'
#' @return Tibble with columns: level, location, point, ci_l, ci_u, numerator,
#'   denominator, indicator, indicator_code, numerator_description,
#'   denominator_description, denominator_code.
#' @noRd
.compute_dhs_indicator_generic <- function(data, condition, group_var = NULL,
                                            subnational_level = NULL,
                                            ci_method = "logit") {

  outcome_var <- condition$outcome_var
  ind_title   <- condition$indicator_title

  # Apply filter
  if (is.null(condition$filter_expr)) {
    filtered <- data
  } else {
    filtered <- tryCatch(
      dplyr::filter(data, !!condition$filter_expr),
      error = function(e) tibble::tibble()
    )
  }

  if (!outcome_var %in% names(filtered)) {
    return(tibble::tibble())
  }

  filtered$.dhs_outcome <- filtered[[outcome_var]]
  filtered <- filtered[!is.na(filtered$.dhs_outcome), ]

  n_denom <- nrow(filtered)
  if (n_denom == 0) return(tibble::tibble())

  # Survey design
  n_clusters <- dplyr::n_distinct(filtered$cluster_id)
  use_strata <- dplyr::n_distinct(filtered$stratum_id) > 1

  # Weighted counts
  n_denom_w <- round(sum(filtered$survey_weight, na.rm = TRUE))
  n_numer_w <- round(sum(
    filtered$survey_weight * (filtered$.dhs_outcome == 1), na.rm = TRUE
  ))

  # Single-cluster guard
  if (n_clusters < 2) {
    point_est <- n_numer_w / n_denom_w
    national <- tibble::tibble(
      level = "adm0", location = "National",
      point = point_est, ci_l = NA_real_, ci_u = NA_real_,
      numerator = n_numer_w, denominator = n_denom_w
    )
    return(
      national |>
        dplyr::mutate(
          indicator               = condition$indicator_title,
          indicator_code          = condition$indicator_code,
          numerator_description   = condition$num_desc,
          denominator_description = condition$denom_desc,
          denominator_code        = condition$denom_code
        )
    )
  }

  svy <- tryCatch({
    if (use_strata) {
      survey::svydesign(
        ids = ~cluster_id, strata = ~stratum_id,
        weights = ~survey_weight, data = filtered, nest = TRUE
      )
    } else {
      survey::svydesign(
        ids = ~cluster_id, weights = ~survey_weight,
        data = filtered, nest = TRUE
      )
    }
  }, error = function(e) {
    survey::svydesign(
      ids = ~cluster_id, weights = ~survey_weight,
      data = filtered, nest = TRUE
    )
  })

  # National estimate
  old_opts <- options(survey.lonely.psu = "certainty")
  on.exit(options(old_opts), add = TRUE)

  national <- tryCatch({
    est <- survey::svyciprop(~.dhs_outcome, svy, method = ci_method,
                              na.rm = TRUE)
    ci  <- stats::confint(est)
    tibble::tibble(
      level = "adm0", location = "National",
      point = as.numeric(est), ci_l = ci[1], ci_u = ci[2],
      numerator = n_numer_w, denominator = n_denom_w
    )
  }, error = function(e) {
    cli::cli_alert_warning("    {ind_title} national: {e$message}")
    tibble::tibble(
      level = "adm0", location = "National", point = NA_real_,
      ci_l = NA_real_, ci_u = NA_real_,
      numerator = n_numer_w, denominator = n_denom_w
    )
  })

  # Regional estimates
  regional <- tibble::tibble()
  sub_level <- subnational_level %||% "adm1"

  if (!is.null(group_var) && group_var %in% names(filtered)) {
    group_formula <- stats::as.formula(paste("~", group_var))

    regional <- tryCatch({
      by_result <- survey::svyby(
        ~.dhs_outcome, by = group_formula, design = svy,
        FUN = survey::svyciprop, vartype = "ci",
        method = ci_method, na.rm = TRUE, keep.names = FALSE
      ) |> tibble::as_tibble()

      region_num <- filtered |>
        dplyr::group_by(.data[[group_var]]) |>
        dplyr::summarise(
          numerator = round(sum(
            survey_weight * (.dhs_outcome == 1), na.rm = TRUE
          )), .groups = "drop"
        )

      region_denom <- filtered |>
        dplyr::group_by(.data[[group_var]]) |>
        dplyr::summarise(
          denominator = round(sum(survey_weight, na.rm = TRUE)),
          .groups = "drop"
        )

      names(by_result)[names(by_result) == "ci_l"] <- "ci_l..dhs_outcome"
      names(by_result)[names(by_result) == "ci_u"] <- "ci_u..dhs_outcome"

      by_result |>
        dplyr::rename(
          location = !!group_var, point = .dhs_outcome,
          ci_l = `ci_l..dhs_outcome`, ci_u = `ci_u..dhs_outcome`
        ) |>
        dplyr::mutate(location = as.character(location)) |>
        dplyr::left_join(
          region_num |>
            dplyr::mutate(
              location = as.character(.data[[group_var]])
            ) |>
            dplyr::select(location, numerator),
          by = "location"
        ) |>
        dplyr::left_join(
          region_denom |>
            dplyr::mutate(
              location = as.character(.data[[group_var]])
            ) |>
            dplyr::select(location, denominator),
          by = "location"
        ) |>
        dplyr::mutate(level = sub_level) |>
        dplyr::select(
          level, location, point, ci_l, ci_u, numerator, denominator
        )
    }, error = function(e) {
      cli::cli_alert_warning("    {ind_title} by group: {e$message}")
      tibble::tibble()
    })
  }

  dplyr::bind_rows(national, regional) |>
    dplyr::mutate(
      indicator               = condition$indicator_title,
      indicator_code          = condition$indicator_code,
      numerator_description   = condition$num_desc,
      denominator_description = condition$denom_desc,
      denominator_code        = condition$denom_code
    )
}


#' Assemble standardized DHS output list
#'
#' Takes national and optional regional results in long format and assembles
#' the standardized `list(adm0 = ..., adm1 = ...)` return structure with
#' survey metadata columns prepended.
#'
#' @param national_results Tibble of national results with level/location columns.
#' @param regional_results Tibble of regional results (or NULL/empty tibble).
#' @param survey_meta Named list from `.extract_survey_meta()` or
#'   `.extract_survey_meta_hv()`.
#' @param geo_source Character. geo_source value for regional rows
#'   (e.g., "survey", "gps"). Default: NA.
#' @param admin_col Character. Column name for the admin level (e.g., "adm1").
#'
#' @return Named list with `adm0` tibble and optionally `adm1` (or other) tibble.
#' @noRd
.assemble_dhs_output <- function(national_results, regional_results = NULL,
                                  survey_meta, geo_source = NA_character_,
                                  admin_col = "adm1") {

  meta_cols <- tibble::tibble(
    survey_id   = survey_meta$survey_id,
    iso3        = survey_meta$iso3,
    iso2        = survey_meta$iso2,
    survey_type = survey_meta$survey_type,
    survey_year = survey_meta$survey_year,
    adm0        = survey_meta$country_upper
  )

  # Assemble adm0
  adm0_data <- national_results |>
    dplyr::filter(level == "adm0") |>
    dplyr::select(-level, -location)

  adm0_tbl <- dplyr::bind_cols(
    meta_cols[rep(1, nrow(adm0_data)), ],
    tibble::tibble(type = "survey_weighted", geo_source = NA_character_),
    adm0_data
  ) |> tibble::as_tibble()

  out <- list(adm0 = adm0_tbl)

  # Assemble subnational
  if (!is.null(regional_results) && nrow(regional_results) > 0) {
    sub_data <- regional_results |>
      dplyr::filter(level != "adm0")

    if (nrow(sub_data) > 0) {
      sub_tbl <- dplyr::bind_cols(
        meta_cols[rep(1, nrow(sub_data)), ],
        sub_data |>
          dplyr::transmute(
            !!admin_col := toupper(location),
            type       = "survey_weighted",
            geo_source = geo_source,
            point, ci_l, ci_u,
            numerator, denominator,
            indicator, indicator_code,
            numerator_description,
            denominator_description, denominator_code
          )
      ) |> tibble::as_tibble()

      out[[admin_col]] <- sub_tbl
    }
  }

  out
}


# =============================================================================
# MBG helpers
# =============================================================================

#' Prepare GPS Data for MBG Analysis
#'
#' Shared GPS cleaning used by all MBG functions. Extracts cluster coordinates,
#' filters invalid values, and deduplicates.
#'
#' @param gps_data DHS GPS dataset with cluster coordinates.
#' @param gps_vars Named list for GPS variable mapping with keys:
#'   cluster, lat, lon.
#'
#' @return A tibble with columns: cluster_id, x (longitude), y (latitude).
#'
#' @noRd
.prepare_gps_data <- function(
  gps_data,
  gps_vars = list(
    cluster = "DHSCLUST",
    lat = "LATNUM",
    lon = "LONGNUM"
  )
) {
  if (!is.data.frame(gps_data)) {
    cli::cli_abort("`gps_data` must be a data.frame or tibble")
  }

  gps_clean <- gps_data |>
    dplyr::transmute(
      cluster_id = .data[[gps_vars$cluster]],
      x = as.numeric(.data[[gps_vars$lon]]),
      y = as.numeric(.data[[gps_vars$lat]])
    ) |>
    dplyr::filter(!is.na(x), !is.na(y), x != 0, y != 0) |>
    dplyr::distinct()

  cli::cli_alert_info(
    "GPS data: {nrow(gps_clean)} clusters with valid coordinates"
  )

  gps_clean
}


#' Aggregate Individual Data to MBG Cluster Counts
#'
#' Shared cluster-level aggregation pattern used by all MBG functions.
#' Groups individual-level data by cluster, counts indicator positives and
#' total sample size, then joins GPS coordinates.
#'
#' @param individual_data Data frame with individual-level records. Must have
#'   columns `cluster_id` and the column named by `indicator_col`.
#' @param indicator_col Character. Name of the binary (0/1) indicator column
#'   to aggregate.
#' @param gps_clean Cleaned GPS data from `.prepare_gps_data()`.
#' @param result_name Character. Name for the result in log messages.
#'
#' @return A data.table with columns: cluster_id, indicator, samplesize, x, y.
#'   Returns NULL if no valid clusters could be created.
#'
#' @noRd
.aggregate_to_mbg_clusters <- function(
  individual_data,
  indicator_col,
  gps_clean,
  result_name = "indicator"
) {
  cluster_data <- individual_data |>
    dplyr::group_by(cluster_id) |>
    dplyr::summarise(
      indicator = sum(.data[[indicator_col]], na.rm = TRUE),
      samplesize = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::inner_join(gps_clean, by = "cluster_id") |>
    dplyr::filter(samplesize > 0)

  if (nrow(cluster_data) == 0) {
    cli::cli_alert_warning("{result_name}: no valid clusters")
    return(NULL)
  }

  cli::cli_alert_success("{result_name}: {nrow(cluster_data)} clusters")

  cluster_data
}


#' Extract Interview Month per Cluster
#'
#' Extracts the median interview month for each DHS cluster from any
#' available recode file (KR > PR > HR > IR preference order).
#'
#' @param survey_data Named list of loaded survey data frames.
#' @param gps_clean Cleaned GPS data from `.prepare_gps_data()`.
#'
#' @return A tibble with columns: cluster_id, interview_month, x, y.
#'   Returns NULL if no interview month data could be extracted.
#'
#' @noRd
.extract_cluster_interview_month <- function(survey_data, gps_clean) {
  recode_vars <- list(
    KR = list(cluster = "v001", month = "v006"),
    PR = list(cluster = "hv001", month = "hv006"),
    HR = list(cluster = "hv001", month = "hv006"),
    IR = list(cluster = "v001", month = "v006")
  )

  for (recode in c("KR", "PR", "HR", "IR")) {
    if (!recode %in% names(survey_data)) next

    df <- survey_data[[recode]]
    vars <- recode_vars[[recode]]

    if (!all(c(vars$cluster, vars$month) %in% names(df))) next

    cluster_months <- df |>
      dplyr::transmute(
        cluster_id = as.integer(haven::zap_labels(.data[[vars$cluster]])),
        interview_month = as.integer(haven::zap_labels(.data[[vars$month]]))
      ) |>
      dplyr::filter(!is.na(interview_month), !is.na(cluster_id)) |>
      dplyr::group_by(cluster_id) |>
      dplyr::summarise(
        interview_month = as.integer(stats::median(interview_month, na.rm = TRUE)),
        .groups = "drop"
      )

    if (nrow(cluster_months) > 0) {
      cluster_months <- cluster_months |>
        dplyr::inner_join(gps_clean, by = "cluster_id")

      cli::cli_alert_info(
        "Interview month extracted from {recode}: {nrow(cluster_months)} clusters"
      )
      return(cluster_months)
    }
  }

  cli::cli_alert_warning("Could not extract interview month from any recode file")
  NULL
}


#' Aggregate Interview Month to Administrative Level
#'
#' Spatially joins cluster-level interview months to admin polygons and
#' computes the median month per administrative unit.
#'
#' @param cluster_months Tibble from `.extract_cluster_interview_month()`.
#' @param admin_sf sf object with administrative boundaries.
#' @param admin_col Column name for admin identifier (e.g., "adm2").
#'
#' @return A tibble with columns: `admin_col` and `median_survey_month`.
#'   Returns NULL if no valid data.
#'
#' @noRd
.aggregate_interview_month_to_admin <- function(
  cluster_months,
  admin_sf,
  admin_col = "adm2"
) {
  if (is.null(cluster_months) || nrow(cluster_months) == 0) return(NULL)

  cluster_sf <- cluster_months |>
    sf::st_as_sf(coords = c("x", "y"), crs = 4326, remove = FALSE)

  admin_crs <- sf::st_crs(admin_sf)
  if (!is.na(admin_crs)) {
    cluster_sf <- sf::st_transform(cluster_sf, admin_crs)
  }

  joined <- sf::st_join(
    cluster_sf,
    admin_sf |> dplyr::select(dplyr::all_of(admin_col)),
    join = sf::st_within,
    left = TRUE
  )

  unmatched <- is.na(joined[[admin_col]])
  if (any(unmatched)) {
    nearest_idx <- sf::st_nearest_feature(joined[unmatched, ], admin_sf)
    joined[[admin_col]][unmatched] <- admin_sf[[admin_col]][nearest_idx]
  }

  result <- sf::st_drop_geometry(joined) |>
    dplyr::filter(!is.na(.data[[admin_col]])) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(admin_col))) |>
    dplyr::summarise(
      median_survey_month = as.integer(
        round(stats::median(interview_month, na.rm = TRUE))
      ),
      .groups = "drop"
    )

  result
}


#' Resolve region variable to human-readable labels
#'
#' Converts a haven-labelled region variable to uppercase character labels.
#' If the result is still numeric (no haven labels), falls back to extracting
#' value labels from the `labels` attribute. Final fallback: "Region_N".
#'
#' @param region_col The region column (possibly haven_labelled).
#' @param region_var_name Name of the variable (for messages).
#' @return Character vector of uppercase region names.
#' @noRd
.resolve_region_labels <- function(region_col, region_var_name = "region") {
  # Try haven::as_factor first
  resolved <- tryCatch({
    as.character(haven::as_factor(region_col)) |> toupper()
  }, error = function(e) {
    as.character(region_col) |> toupper()
  })

  # Check if result is still numeric
  unique_vals <- unique(resolved[!is.na(resolved)])
  is_numeric <- length(unique_vals) > 0 &&
    all(grepl("^\\d+(\\.\\d+)?$", unique_vals))

  if (!is_numeric) return(resolved)

  # Try extracting from value labels attribute
  val_labels <- attr(region_col, "labels")
  if (!is.null(val_labels) && length(val_labels) > 0) {
    lookup <- stats::setNames(
      toupper(names(val_labels)), as.character(val_labels)
    )
    raw_vals <- as.character(as.vector(haven::zap_labels(region_col)))
    new_resolved <- unname(lookup[raw_vals])
    new_resolved[is.na(new_resolved)] <- resolved[is.na(new_resolved)]

    new_unique <- unique(new_resolved[!is.na(new_resolved)])
    if (!all(grepl("^\\d+(\\.\\d+)?$", new_unique))) {
      cli::cli_alert_info(
        "Resolved {.var {region_var_name}} codes to labels"
      )
      return(new_resolved)
    }
  }

  # Final fallback
  cli::cli_alert_warning(
    "{.var {region_var_name}} has numeric values with no labels; using Region_N"
  )
  paste0("REGION_", resolved)
}
