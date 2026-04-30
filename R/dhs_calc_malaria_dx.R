#' Calculate Malaria Diagnostic Testing from DHS Data
#'
#' Estimates the proportion of febrile children under 5 who had blood taken
#' for malaria testing using survey-weighted methods. This is step 2 of the
#' case management cascade.
#'
#' @details
#' Methodology: \url{https://github.com/ahadi-analytics/sntmethods/blob/master/inst/methods/malaria_dx_dhs.yml}
#'
#' @param dhs_kr DHS children's recode (KR) dataset (data.frame or tibble).
#' @param survey_vars Named list mapping DHS variable names. Required keys:
#'   \itemize{
#'     \item `cluster`: Cluster/PSU ID (default: "v021")
#'     \item `weight`: Survey weight (default: "v005")
#'     \item `stratum`: Stratum variable (default: "v022")
#'     \item `age`: Child's age in months (default: "hw1")
#'     \item `fever`: Had fever in last 2 weeks (default: "h22")
#'     \item `malaria_dx`: Blood taken for malaria test (default: "h47")
#'   }
#' @param region_var Optional column name in `dhs_kr` to use as grouping
#'   variable (e.g., "v024" for region).
#' @param gps_data Optional DHS GPS dataset with cluster coordinates.
#' @param gps_vars Named list for GPS variables (cluster, lat, lon).
#' @param shapefile Optional sf object with administrative boundaries.
#' @param admin_level Character vector of admin columns from shapefile
#'   (e.g., c("adm1", "adm2")).
#' @param join_nearest Logical; if TRUE, assigns clusters outside polygons
#'   to nearest admin unit. Default: TRUE.
#'
#' @return Tibble with malaria diagnosis estimates by grouping level, including:
#'   \itemize{
#'     \item Grouping variables (region, admin level, or national)
#'     \item `dhs_malaria_dx`: Proportion tested among febrile children
#'     \item `dhs_malaria_dx_low`, `dhs_malaria_dx_upp`: 95\% confidence interval
#'     \item `dhs_n_febrile`: Number of febrile children (denominator)
#'     \item `dhs_n_tested`: Number who had blood taken
#'   }
#'
#' @details
#' This function measures whether a diagnostic TEST was performed (h47),
#' distinct from PfPR which measures test RESULTS. The denominator is
#' febrile U5 children (h22 == 1).
#'
#' @examples
#' \dontrun{
#' dx_results <- calc_malaria_dx_dhs_core(
#'   dhs_kr = kr_data,
#'   region_var = "v024"
#' )
#' }
#'
#' @seealso [calc_act_dhs()] for ACT treatment (step 4),
#'   [calc_case_management_dhs()] for the full cascade
#' @keywords internal
calc_malaria_dx_dhs_core <- function(
  dhs_kr,
  survey_vars = list(
    cluster = "v021",
    weight = "v005",
    stratum = "v022",
    age = "hw1",
    fever = "h22",
    malaria_dx = "h47"
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
  # ---- 1. Input validation ----

  if (!is.data.frame(dhs_kr)) {
    cli::cli_abort("`dhs_kr` must be a data.frame or tibble.")
  }

  if (nrow(dhs_kr) == 0) {
    cli::cli_abort("`dhs_kr` is empty.")
  }

  # Check required survey variables
  needed <- unlist(survey_vars[c("cluster", "weight", "stratum", "age", "fever")])
  missing_vars <- setdiff(needed, names(dhs_kr))

  if (length(missing_vars) > 0) {
    cli::cli_abort(c(
      "Required variables not found: {.var {missing_vars}}",
      "i" = "Check your survey_vars mapping"
    ))
  }

  # Validate region_var if provided
  if (!is.null(region_var)) {
    if (!is.character(region_var) || length(region_var) != 1) {
      cli::cli_abort("`region_var` must be a single character string.")
    }
    if (!region_var %in% names(dhs_kr)) {
      cli::cli_abort(c(
        "Column {.var {region_var}} not found in `dhs_kr`.",
        "i" = "Available columns: {.var {head(names(dhs_kr), 10)}}..."
      ))
    }
  }

  # ---- 2. Prepare base dataset ----

  kr_fever <- .prepare_malaria_dx_data(
    dhs_kr = dhs_kr,
    survey_vars = survey_vars,
    include_survey_vars = TRUE
  )
  if (is.null(kr_fever)) return(NULL)

  # ---- 3. Spatial join if GPS + shapefile provided ----

  class_var <- NULL

  if (!is.null(region_var)) {
    class_var <- region_var
    cli::cli_alert_info("Using {.var {region_var}} as grouping variable")
  } else if (!is.null(gps_data) && !is.null(shapefile)) {
    cli::cli_alert_info("Joining GPS coordinates and administrative boundaries")

    if (!requireNamespace("sf", quietly = TRUE)) {
      cli::cli_abort("Package 'sf' is required for spatial operations")
    }

    gps_clean <- gps_data |>
      dplyr::select(
        cluster_id = !!gps_vars$cluster,
        lat = !!gps_vars$lat,
        lon = !!gps_vars$lon
      ) |>
      dplyr::distinct()

    kr_fever <- kr_fever |>
      dplyr::left_join(gps_clean, by = "cluster_id")

    clusters_sf <- kr_fever |>
      dplyr::select(cluster_id, lat, lon) |>
      dplyr::distinct() |>
      dplyr::filter(!is.na(lat), !is.na(lon)) |>
      sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)

    shapefile <- shapefile |>
      sf::st_transform(4326) |>
      sf::st_make_valid()

    if (is.null(admin_level)) {
      available_admins <- names(shapefile)[grep("^adm[0-9]+$", names(shapefile))]
      if (length(available_admins) == 0) {
        cli::cli_abort("No admin columns (adm0, adm1, adm2, etc.) found in shapefile")
      }
      admin_level <- available_admins
    }

    cluster_admin <- sf::st_join(
      clusters_sf,
      shapefile[, c(admin_level, "geometry")],
      join = sf::st_within,
      left = TRUE
    )

    if (join_nearest) {
      unmatched <- is.na(cluster_admin[[admin_level[1]]])
      if (any(unmatched)) {
        nearest_idx <- sf::st_nearest_feature(cluster_admin[unmatched, ], shapefile)
        for (col in admin_level) {
          if (col %in% names(shapefile)) {
            cluster_admin[unmatched, col] <- shapefile[[col]][nearest_idx]
          }
        }
      }
    }

    cluster_admin_df <- sf::st_drop_geometry(cluster_admin)
    kr_fever <- kr_fever |>
      dplyr::left_join(cluster_admin_df, by = "cluster_id")

    if (length(admin_level) > 1) {
      kr_fever$admin_class <- apply(
        kr_fever[, admin_level, drop = FALSE], 1, paste, collapse = "_"
      )
      class_var <- "admin_class"
    } else {
      class_var <- admin_level[1]
    }
  } else if ("v024" %in% names(kr_fever)) {
    class_var <- "v024"
    cli::cli_alert_info("Using v024 (region) as grouping variable")
  }

  # ---- 4. Set up survey design ----

  use_strata <- dplyr::n_distinct(kr_fever$stratum_id) > 1

  if (use_strata) {
    survey_options <- options(survey.lonely.psu = "adjust")
    on.exit(options(survey_options), add = TRUE)

    des <- survey::svydesign(
      ids = ~cluster_id,
      strata = ~stratum_id,
      weights = ~survey_weight,
      data = kr_fever,
      nest = TRUE
    )
  } else {
    des <- survey::svydesign(
      ids = ~cluster_id,
      weights = ~survey_weight,
      data = kr_fever,
      nest = TRUE
    )
  }

  # ---- 5. Calculate malaria diagnosis indicators ----

  if (!is.null(class_var)) {
    group_formula <- stats::as.formula(paste("~", class_var))
  } else {
    group_formula <- ~1
  }

  if (!is.null(class_var)) {
    dx_results <- tryCatch({
      survey::svyby(
        ~had_test,
        by = group_formula,
        design = des,
        FUN = survey::svymean,
        vartype = "ci",
        na.rm = TRUE,
        keep.names = FALSE
      ) |>
        tibble::as_tibble()
    }, error = function(e) {
      if (grepl("has only one PSU", e$message)) {
        des_no_strata <- survey::svydesign(
          ids = ~cluster_id, weights = ~survey_weight,
          data = kr_fever, nest = TRUE
        )
        survey::svyby(
          ~had_test, by = group_formula, design = des_no_strata,
          FUN = survey::svymean, vartype = "ci", na.rm = TRUE,
          keep.names = FALSE
        ) |> tibble::as_tibble()
      } else {
        stop(e)
      }
    })
  } else {
    dx_mean <- survey::svymean(~had_test, design = des, na.rm = TRUE)
    dx_ci <- stats::confint(dx_mean)

    dx_results <- tibble::tibble(
      level = "National",
      had_test = as.numeric(dx_mean["had_test"]),
      `ci_l.had_test` = dx_ci["had_test", 1],
      `ci_u.had_test` = dx_ci["had_test", 2]
    )
  }

  # Normalize CI column names (svyby uses ci_l/ci_u for single-variable formulas)
  names(dx_results)[names(dx_results) == "ci_l"] <- "ci_l.had_test"
  names(dx_results)[names(dx_results) == "ci_u"] <- "ci_u.had_test"

  # Rename columns
  dx_results <- dx_results |>
    dplyr::rename(
      dhs_malaria_dx = had_test,
      dhs_malaria_dx_low = `ci_l.had_test`,
      dhs_malaria_dx_upp = `ci_u.had_test`
    )

  # ---- 6. Calculate sample sizes ----

  if (!is.null(class_var)) {
    sample_sizes <- kr_fever |>
      dplyr::group_by(.data[[class_var]]) |>
      dplyr::summarise(
        dhs_n_febrile = dplyr::n(),
        dhs_n_tested = sum(had_test == 1, na.rm = TRUE),
        .groups = "drop"
      )

    dx_results <- dx_results |>
      dplyr::left_join(sample_sizes, by = class_var)
  } else {
    dx_results$dhs_n_febrile <- nrow(kr_fever)
    dx_results$dhs_n_tested <- sum(kr_fever$had_test == 1, na.rm = TRUE)
  }

  # ---- 7. Format results ----

  # Round proportions
  dx_cols <- names(dx_results)[grepl("^dhs_malaria_dx", names(dx_results))]
  dx_results <- dx_results |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(dx_cols[!grepl("^dhs_n_", dx_cols)]),
        ~ round(.x, 2)
      )
    )

  # Clamp CIs to [0, 1]
  dx_results <- dx_results |>
    dplyr::mutate(
      dplyr::across(dplyr::matches("_low$"), ~ pmax(0, .)),
      dplyr::across(dplyr::matches("_upp$"), ~ pmin(1, .))
    )

  # Ensure count columns are integers
  count_cols <- intersect(
    c("dhs_n_febrile", "dhs_n_tested"),
    names(dx_results)
  )
  dx_results <- dx_results |>
    dplyr::mutate(
      dplyr::across(dplyr::all_of(count_cols), ~ as.integer(round(.x)))
    )

  # Split admin_class back if needed
  if (!is.null(class_var) && class_var == "admin_class" &&
      !is.null(admin_level) && length(admin_level) > 1) {
    admin_splits <- stringr::str_split(
      dx_results$admin_class, "_", simplify = TRUE
    )
    for (i in seq_along(admin_level)) {
      dx_results[[admin_level[i]]] <- admin_splits[, i]
    }
  }

  # Remove temporary columns
  dx_results <- dx_results |>
    dplyr::select(-dplyr::any_of(c("admin_class", "level")))

  tibble::as_tibble(dx_results)
}


#' Calculate Malaria Diagnostic Testing from DHS Data (Standardized)
#'
#' Estimates the proportion of febrile children under 5 who had blood taken
#' for malaria testing. Returns standardized long-format output as
#' `list(adm0, adm1)`.
#'
#' @inheritParams calc_malaria_dx_dhs_core
#' @param ci_method CI method for svyciprop. Default: "logit".
#'
#' @return Named list with `adm0` (national) and optionally `adm1` (regional)
#'   tibbles in standardized long format.
#'
#' @seealso [malaria_dx_dictionary()] for indicator definitions,
#'   [calc_malaria_dx_dhs_core()] for backward-compatible wide output
#' @export
calc_malaria_dx_dhs <- function(
  dhs_kr,
  survey_vars = list(
    cluster = "v021",
    weight = "v005",
    stratum = "v022",
    age = "hw1",
    fever = "h22",
    malaria_dx = "h47"
  ),
  region_var   = NULL,
  gps_data     = NULL,
  gps_vars     = list(cluster = "DHSCLUST", lat = "LATNUM", lon = "LONGNUM"),
  shapefile    = NULL,
  admin_level  = NULL,
  join_nearest = TRUE,
  ci_method    = "logit"
) {
  # ---- 1. Extract survey metadata ----
  survey_meta <- .extract_survey_meta(dhs_kr)

  # ---- 2. Prepare data ----
  kr_fever <- .prepare_malaria_dx_data(
    dhs_kr = dhs_kr,
    survey_vars = survey_vars,
    include_survey_vars = TRUE
  )
  if (is.null(kr_fever)) {
    cli::cli_abort("No valid malaria diagnosis data after preparation.")
  }

  # ---- 3. Compute indicators across admin levels ----
  .compute_dhs_indicators_with_admin(
    data               = kr_fever,
    conditions         = .malaria_dx_conditions(),
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

#' Internal: Malaria Dx indicator conditions
#'
#' @return List of named lists, each with indicator specification.
#' @noRd
.malaria_dx_conditions <- function() {
  denom <- "Febrile U5 children (0-59 months)"
  list(
    list(
      indicator       = "MALARIA_DX",
      indicator_code  = "malaria_dx",
      indicator_title = "Blood taken for malaria testing among febrile U5",
      denom_code      = "febrile_u5",
      filter_expr     = NULL,
      outcome_var     = "had_test",
      num_desc        = "Febrile children who had blood taken for testing",
      denom_desc      = denom
    )
  )
}


# =============================================================================
# Indicator Dictionary
# =============================================================================

#' Malaria Diagnostic Testing Indicator Dictionary
#'
#' Returns the dictionary of malaria diagnostic testing indicators with
#' metadata.
#'
#' @return Tibble with columns: indicator, indicator_code, indicator_title,
#'   numerator_description, denominator_description, denominator_code.
#'
#' @keywords internal
malaria_dx_dictionary <- function() {
  conds <- .malaria_dx_conditions()
  tibble::tibble(
    indicator               = vapply(conds, `[[`, character(1), "indicator"),
    indicator_code          = vapply(conds, `[[`, character(1), "indicator_code"),
    indicator_title         = vapply(conds, `[[`, character(1), "indicator_title"),
    numerator_description   = vapply(conds, `[[`, character(1), "num_desc"),
    denominator_description = vapply(conds, `[[`, character(1), "denom_desc"),
    denominator_code        = vapply(conds, `[[`, character(1), "denom_code")
  )
}
