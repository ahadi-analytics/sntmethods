# antimalarial indicator
#
# Merged from: dhs_calc_antimalarial.R dhs_calc_antimalarial_mbg.R dhs_helpers_antimalarial.R 
# Contains the survey-weighted calc, MBG cluster-prep, and indicator-
# specific helpers for this family.

# ---- dhs_calc_antimalarial.R ----

#' Calculate Antimalarial Treatment from DHS Data
#'
#' Estimates the proportion of febrile children under 5 who received any
#' antimalarial drug using survey-weighted methods. This is step 3 of the
#' case management cascade.
#'
#' @details
#' Methodology: \url{https://github.com/ahadi-analytics/sntmethods/blob/master/inst/methods/antimalarial_dhs.yml}
#'
#' @param dhs_kr DHS children's recode (KR) dataset (data.frame or tibble).
#' @param survey_vars Named list mapping DHS variable names. Required keys:
#'   \itemize{
#'     \item `cluster`: Cluster/PSU ID (default: "v021")
#'     \item `weight`: Survey weight (default: "v005")
#'     \item `stratum`: Stratum variable (default: "v022")
#'     \item `age`: Child's age in months (default: "hw1")
#'     \item `fever`: Had fever in last 2 weeks (default: "h22")
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
#' @return Tibble with antimalarial estimates by grouping level, including:
#'   \itemize{
#'     \item Grouping variables (region, admin level, or national)
#'     \item `dhs_antimalarial`: Proportion receiving any antimalarial
#'     \item `dhs_antimalarial_low`, `dhs_antimalarial_upp`: 95\% confidence interval
#'     \item `dhs_n_febrile`: Number of febrile children (denominator)
#'     \item `dhs_n_antimalarial`: Number receiving any antimalarial
#'   }
#'
#' @details
#' Auto-detects the available drug series at runtime: ml13* variables (ml13a
#' through ml13g, ml13aa, etc.) are preferred. If absent, falls back to the
#' h37a-h series used in older DHS surveys (e.g. BFA 2021). received_antimalarial
#' = 1 if ANY detected drug variable equals 1. ACT (ml13e / h37e) is a subset.
#'
#' @examples
#' \dontrun{
#' am_results <- calc_antimalarial_dhs_core(
#'   dhs_kr = kr_data,
#'   region_var = "v024"
#' )
#' }
#'
#' @seealso [calc_act_dhs()] for ACT-specific treatment (step 4),
#'   [calc_case_management_dhs()] for the full cascade
#' @keywords internal
calc_antimalarial_dhs_core <- function(
  dhs_kr,
  survey_vars = list(
    cluster = "v021",
    weight = "v005",
    stratum = "v022",
    age = "hw1",
    fever = "h22",
    alive = "b5"
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

  # Check for antimalarial variables (ml13 preferred, h37 fallback for older surveys)
  # Use standard drug slots (a-h) for presence check -- actual label-based filtering
  # happens downstream in .prepare_antimalarial_data()
  ml13_vars <- grep("^ml13[a-h]$", names(dhs_kr), value = TRUE)
  h37_vars  <- grep("^h37[a-h]$", names(dhs_kr), value = TRUE)
  if (length(ml13_vars) == 0 && length(h37_vars) == 0) {
    cli::cli_abort(c(
      "No antimalarial treatment variables found in data.",
      "i" = "Checked for ml13a/ml13b/... (newer surveys) and h37a-h (older surveys).",
      "i" = "Verify that this survey includes malaria treatment questions."
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

  kr_fever <- .prepare_antimalarial_data(
    dhs_kr = dhs_kr,
    survey_vars = survey_vars,
    include_survey_vars = TRUE
  )

  # ---- 2b. Classify public sector from h32 treatment-seeking variables ----

  kr_fever$antimalarial_public <- tryCatch({
    csb <- .classify_csb_from_h32(kr_fever)
    as.integer(kr_fever$has_antimalarial == 1 & csb$csb_public == 1)
  }, error = function(e) {
    cli::cli_warn("Cannot compute antimalarial_public: {e$message}")
    NA_real_
  })

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

  # ---- 5. Calculate antimalarial indicators ----

  if (!is.null(class_var)) {
    group_formula <- stats::as.formula(paste("~", class_var))
  } else {
    group_formula <- ~1
  }

  if (!is.null(class_var)) {
    am_results <- tryCatch({
      survey::svyby(
        ~has_antimalarial,
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
          ~has_antimalarial, by = group_formula, design = des_no_strata,
          FUN = survey::svymean, vartype = "ci", na.rm = TRUE,
          keep.names = FALSE
        ) |> tibble::as_tibble()
      } else {
        stop(e)
      }
    })
  } else {
    am_mean <- survey::svymean(~has_antimalarial, design = des, na.rm = TRUE)
    am_ci <- stats::confint(am_mean)

    am_results <- tibble::tibble(
      level = "National",
      has_antimalarial = as.numeric(am_mean["has_antimalarial"]),
      `ci_l.has_antimalarial` = am_ci["has_antimalarial", 1],
      `ci_u.has_antimalarial` = am_ci["has_antimalarial", 2]
    )
  }

  # Normalize CI column names (svyby uses ci_l/ci_u for single-variable formulas)
  names(am_results)[names(am_results) == "ci_l"] <- "ci_l.has_antimalarial"
  names(am_results)[names(am_results) == "ci_u"] <- "ci_u.has_antimalarial"

  # Rename columns
  am_results <- am_results |>
    dplyr::rename(
      dhs_antimalarial = has_antimalarial,
      dhs_antimalarial_low = `ci_l.has_antimalarial`,
      dhs_antimalarial_upp = `ci_u.has_antimalarial`
    )

  # ---- 5b. Calculate antimalarial_public indicator ----

  has_am_public <- !all(is.na(kr_fever$antimalarial_public))

  if (has_am_public) {
    if (!is.null(class_var)) {
      am_public_results <- tryCatch({
        survey::svyby(
          ~antimalarial_public,
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
            ~antimalarial_public, by = group_formula, design = des_no_strata,
            FUN = survey::svymean, vartype = "ci", na.rm = TRUE,
            keep.names = FALSE
          ) |> tibble::as_tibble()
        } else {
          stop(e)
        }
      })
    } else {
      am_pub_mean <- survey::svymean(~antimalarial_public, design = des, na.rm = TRUE)
      am_pub_ci <- stats::confint(am_pub_mean)

      am_public_results <- tibble::tibble(
        level = "National",
        antimalarial_public = as.numeric(am_pub_mean["antimalarial_public"]),
        `ci_l.antimalarial_public` = am_pub_ci["antimalarial_public", 1],
        `ci_u.antimalarial_public` = am_pub_ci["antimalarial_public", 2]
      )
    }

    # Normalize CI column names
    names(am_public_results)[names(am_public_results) == "ci_l"] <- "ci_l.antimalarial_public"
    names(am_public_results)[names(am_public_results) == "ci_u"] <- "ci_u.antimalarial_public"

    am_public_results <- am_public_results |>
      dplyr::rename(
        dhs_antimalarial_public = antimalarial_public,
        dhs_antimalarial_public_low = `ci_l.antimalarial_public`,
        dhs_antimalarial_public_upp = `ci_u.antimalarial_public`
      )

    # Join to main results
    if (!is.null(class_var)) {
      am_results <- am_results |>
        dplyr::left_join(am_public_results, by = class_var)
    } else {
      am_results <- am_results |>
        dplyr::bind_cols(
          am_public_results |> dplyr::select(-dplyr::any_of("level"))
        )
    }
  }

  # ---- 6. Calculate sample sizes ----

  if (!is.null(class_var)) {
    sample_sizes <- kr_fever |>
      dplyr::group_by(.data[[class_var]]) |>
      dplyr::summarise(
        dhs_n_febrile = dplyr::n(),
        dhs_n_antimalarial = sum(has_antimalarial == 1, na.rm = TRUE),
        dhs_n_antimalarial_public = sum(antimalarial_public == 1, na.rm = TRUE),
        .groups = "drop"
      )

    am_results <- am_results |>
      dplyr::left_join(sample_sizes, by = class_var)
  } else {
    am_results$dhs_n_febrile <- nrow(kr_fever)
    am_results$dhs_n_antimalarial <- sum(
      kr_fever$has_antimalarial == 1, na.rm = TRUE
    )
    am_results$dhs_n_antimalarial_public <- sum(
      kr_fever$antimalarial_public == 1, na.rm = TRUE
    )
  }

  # ---- 7. Format results ----

  # Round proportions
  am_cols <- names(am_results)[grepl("^dhs_antimalarial", names(am_results))]
  am_results <- am_results |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(am_cols[!grepl("^dhs_n_", am_cols)]),
        ~ round(.x, 2)
      )
    )

  # Clamp CIs to [0, 1]
  am_results <- am_results |>
    dplyr::mutate(
      dplyr::across(dplyr::matches("_low$"), ~ pmax(0, .)),
      dplyr::across(dplyr::matches("_upp$"), ~ pmin(1, .))
    )

  # Ensure count columns are integers
  count_cols <- intersect(
    c("dhs_n_febrile", "dhs_n_antimalarial", "dhs_n_antimalarial_public"),
    names(am_results)
  )
  am_results <- am_results |>
    dplyr::mutate(
      dplyr::across(dplyr::all_of(count_cols), ~ as.integer(round(.x)))
    )

  # Split admin_class back if needed
  if (!is.null(class_var) && class_var == "admin_class" &&
      !is.null(admin_level) && length(admin_level) > 1) {
    admin_splits <- stringr::str_split(
      am_results$admin_class, "_", simplify = TRUE
    )
    for (i in seq_along(admin_level)) {
      am_results[[admin_level[i]]] <- admin_splits[, i]
    }
  }

  # Remove temporary columns
  am_results <- am_results |>
    dplyr::select(-dplyr::any_of(c("admin_class", "level")))

  tibble::as_tibble(am_results)
}


#' Calculate Antimalarial Treatment from DHS Data (Standardized)
#'
#' Computes antimalarial treatment indicators from DHS Children's Recode (KR)
#' data. Returns survey-weighted proportions with logit confidence intervals
#' in standardized long format.
#'
#' @details
#' Computes two indicators:
#' \itemize{
#'   \item Any antimalarial treatment among febrile U5 children
#'   \item Antimalarial from public sector facility among febrile U5 children
#' }
#' See [antimalarial_dictionary()] for the full indicator list.
#'
#' @inheritParams calc_antimalarial_dhs_core
#' @param ci_method Method for confidence intervals. Default: "logit".
#'
#' @return Named list of tibbles:
#'   \describe{
#'     \item{`adm0`}{National-level estimates (always present)}
#'     \item{`adm1`}{Admin-1 estimates (when `region_var` provided)}
#'   }
#'   Each tibble contains columns: survey_id, iso3, iso2, survey_type,
#'   survey_year, adm0, adm1, type, geo_source, point, ci_l, ci_u,
#'   numerator, denominator, indicator, indicator_code,
#'   numerator_description, denominator_description, denominator_code.
#'
#' @examples
#' \dontrun{
#' am <- calc_antimalarial_dhs(dhs_kr = kr_data, region_var = "v024")
#' am$adm0
#' am$adm1
#' }
#'
#' @seealso [antimalarial_dictionary()] for indicator metadata,
#'   [calc_antimalarial_dhs_core()] for legacy wide-format output
#' @export
calc_antimalarial_dhs <- function(
  dhs_kr,
  survey_vars = list(
    cluster = "v021",
    weight = "v005",
    stratum = "v022",
    age = "hw1",
    fever = "h22",
    alive = "b5"
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
    c("stringr", "tibble"),
    reason = "for `calc_antimalarial_dhs()`"
  )

  # ---- 1. Extract survey metadata ----
  survey_meta <- .extract_survey_meta(dhs_kr)

  # ---- 2. Prepare febrile U5 dataset ----
  kr_fever <- .prepare_antimalarial_data(
    dhs_kr = dhs_kr,
    survey_vars = survey_vars,
    include_survey_vars = TRUE
  )

  # ---- 2b. Classify public sector from h32 treatment-seeking variables ----
  kr_fever$antimalarial_public <- tryCatch({
    csb <- .classify_csb_from_h32(kr_fever)
    as.integer(kr_fever$has_antimalarial == 1 & csb$csb_public == 1)
  }, error = function(e) {
    cli::cli_warn("Cannot compute antimalarial_public: {e$message}")
    NA_real_
  })

  cli::cli_alert_success(
    "Under 5 with fever: {format(nrow(kr_fever), big.mark = ',')} children"
  )

  # ---- 3. Compute indicators across admin levels ----
  .compute_dhs_indicators_with_admin(
    data               = kr_fever,
    conditions         = .antimalarial_conditions(),
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

#' Internal: Antimalarial indicator conditions
#'
#' @return List of named lists, each with indicator specification.
#' @noRd
.antimalarial_conditions <- function() {
  denom <- "Under 5 with fever"
  list(
    list(
      indicator       = "ANTIMALARIAL",
      indicator_code  = "antimalarial",
      indicator_title = "Any antimalarial treatment among febrile U5",
      denom_code      = "feb_u5",
      filter_expr     = NULL,
      outcome_var     = "has_antimalarial",
      num_desc        = "Febrile children receiving any antimalarial",
      denom_desc      = denom
    ),
    list(
      indicator       = "ANTIMALARIAL_PUBLIC",
      indicator_code  = "antimalarial_public",
      indicator_title = "Antimalarial from public sector among febrile U5",
      denom_code      = "feb_u5",
      filter_expr     = NULL,
      outcome_var     = "antimalarial_public",
      num_desc        = "Febrile children receiving antimalarial from public sector",
      denom_desc      = denom
    )
  )
}


#' Antimalarial Indicator Dictionary
#'
#' Returns the full dictionary of antimalarial indicators with metadata.
#'
#' @return Tibble with columns: indicator, indicator_code, indicator_title,
#'   numerator_description, denominator_description, denominator_code.
#'
#' @keywords internal
antimalarial_dictionary <- function() {
  conds <- .antimalarial_conditions()
  tibble::tibble(
    indicator               = vapply(conds, `[[`, character(1), "indicator"),
    indicator_code          = vapply(conds, `[[`, character(1), "indicator_code"),
    indicator_title         = vapply(conds, `[[`, character(1), "indicator_title"),
    numerator_description   = vapply(conds, `[[`, character(1), "num_desc"),
    denominator_description = vapply(conds, `[[`, character(1), "denom_desc"),
    denominator_code        = vapply(conds, `[[`, character(1), "denom_code")
  )
}


# ---- dhs_calc_antimalarial_mbg.R ----

#' Prepare Antimalarial Treatment Data for MBG Analysis
#'
#' Prepares cluster-level antimalarial treatment data for MBG analysis.
#' Uses a dictionary-driven approach matching the indicator codes from
#' \code{\link{calc_antimalarial_dhs}}.
#'
#' @details
#' Methodology: \url{https://github.com/ahadi-analytics/sntmethods/blob/master/inst/methods/antimalarial_dhs.yml}
#'
#' All dictionary-based indicators share the same data preparation pipeline:
#' \enumerate{
#'   \item Filter to febrile U5 children (via \code{.prepare_antimalarial_data()})
#'   \item Classify care-seeking sectors if needed (via
#'     \code{.classify_csb_from_h32()})
#'   \item Apply per-indicator filters and aggregate to cluster-level counts
#' }
#'
#' @param dhs_kr DHS Children's Recode (KR) dataset.
#' @param gps_data DHS GPS dataset with cluster coordinates.
#' @param indicators Character vector of indicators to calculate.
#'   See \code{.antimalarial_mbg_dictionary()} for the full list of
#'   standardized indicator codes. Default: \code{"antimalarial"}.
#' @param survey_vars Named list mapping DHS variable names:
#'   \itemize{
#'     \item \code{cluster}: Cluster ID (default: "v001")
#'     \item \code{age}: Child's age in months (default: "hw1")
#'     \item \code{fever}: Fever in last 2 weeks (default: "h22")
#'   }
#' @param gps_vars Named list for GPS variable mapping.
#'
#' @return A named list of data.tables (one per indicator), each with columns:
#'   \itemize{
#'     \item cluster_id: Cluster identifier
#'     \item indicator: Numerator count (children receiving antimalarial)
#'     \item samplesize: Denominator count (febrile U5 children)
#'     \item x: Longitude
#'     \item y: Latitude
#'   }
#'
#' @examples
#' \dontrun{
#' am_mbg <- calc_antimalarial_mbg(
#'   dhs_kr = kr_data,
#'   gps_data = gps_data,
#'   indicators = c("antimalarial", "antimalarial_public")
#' )
#' }
#'
#' @seealso [calc_antimalarial_dhs()] for survey-weighted estimates,
#'   [calc_act_mbg()] for ACT-specific treatment
#' @export
calc_antimalarial_mbg <- function(
  dhs_kr,
  gps_data,
  indicators = "antimalarial",
  survey_vars = list(
    cluster = "v001",
    age = "hw1",
    fever = "h22"
  ),
  gps_vars = list(
    cluster = "DHSCLUST",
    lat = "LATNUM",
    lon = "LONGNUM"
  )
) {
  # ---- Input validation ----

  if (!is.data.frame(dhs_kr)) {
    cli::cli_abort("`dhs_kr` must be a data.frame or tibble")
  }
  if (!is.data.frame(gps_data)) {
    cli::cli_abort("`gps_data` must be a data.frame or tibble")
  }

  # Validate indicators against dictionary
  dict <- .antimalarial_mbg_dictionary()
  dict_names <- vapply(dict, `[[`, character(1), "name")
  invalid <- setdiff(indicators, dict_names)
  if (length(invalid) > 0) {
    cli::cli_abort("Invalid indicators: {.val {invalid}}")
  }

  # ---- Prepare base data using shared helpers ----

  gps_clean <- .prepare_gps_data(gps_data, gps_vars)

  am_data <- tryCatch(
    .prepare_antimalarial_data(
      dhs_kr = dhs_kr,
      survey_vars = survey_vars,
      include_survey_vars = FALSE
    ),
    error = function(e) {
      cli::cli_alert_warning(conditionMessage(e))
      return(NULL)
    }
  )

  if (is.null(am_data)) return(list())

  if (all(is.na(am_data$has_antimalarial))) {
    cli::cli_alert_warning("All antimalarial variables are NA")
    return(list())
  }

  # ---- Determine which dictionary entries are requested ----

  dict_specs <- dict[vapply(dict, function(d) d$name %in% indicators, logical(1))]

  # ---- Conditional CSB enrichment (only when needed) ----

  needs_csb <- any(vapply(
    dict_specs,
    function(s) !is.null(s$csb_filter),
    logical(1)
  ))

  if (needs_csb) {
    am_data <- tryCatch(
      .classify_csb_from_h32(am_data),
      error = function(e) {
        cli::cli_alert_warning(
          "CSB classification failed: {conditionMessage(e)}"
        )
        NULL
      }
    )
    if (is.null(am_data)) {
      # Fall back: re-prepare without CSB, skip CSB-dependent indicators
      am_data <- tryCatch(
        .prepare_antimalarial_data(
          dhs_kr = dhs_kr,
          survey_vars = survey_vars,
          include_survey_vars = FALSE
        ),
        error = function(e) {
          cli::cli_alert_warning(conditionMessage(e))
          return(NULL)
        }
      )
      if (is.null(am_data)) return(list())
      needs_csb <- FALSE
    }
  }

  # ---- Dictionary-driven indicator loop ----

  results <- list()

  for (spec in dict_specs) {
    # Skip CSB-filtered indicators if CSB enrichment failed
    if (!is.null(spec$csb_filter) && !needs_csb) {
      cli::cli_alert_warning(
        "Skipping {.val {spec$name}}: CSB classification not available"
      )
      next
    }

    filtered <- am_data

    # Apply CSB filter if specified
    if (!is.null(spec$csb_filter)) {
      col <- spec$csb_filter
      if (!col %in% names(filtered)) next
      filtered <- filtered[
        !is.na(filtered[[col]]) & filtered[[col]] == 1, ,
        drop = FALSE
      ]
    }

    # Filter to non-NA outcome
    filtered <- filtered[!is.na(filtered[[spec$outcome]]), , drop = FALSE]
    if (nrow(filtered) == 0) {
      cli::cli_alert_warning(
        "No data for {.val {spec$name}} -- skipping"
      )
      next
    }

    # Build binary outcome
    filtered$.binary <- as.integer(filtered[[spec$outcome]] == 1)

    dt <- .aggregate_to_mbg_clusters(
      individual_data = filtered,
      indicator_col = ".binary",
      gps_clean = gps_clean,
      result_name = spec$name
    )

    if (!is.null(dt)) {
      results[[spec$name]] <- dt
    }
  }

  if (length(results) == 0) {
    cli::cli_alert_warning("No valid antimalarial MBG data could be prepared")
  }

  results
}


#' Antimalarial MBG Indicator Dictionary
#'
#' Returns the full set of standardized indicator specifications for
#' cluster-level antimalarial MBG output. Each entry defines the outcome
#' variable and an optional CSB filter column.
#'
#' @return List of named lists with fields:
#'   \code{name}, \code{outcome}, \code{csb_filter}.
#' @noRd
.antimalarial_mbg_dictionary <- function() {
  list(
    list(
      name = "antimalarial",
      outcome = "has_antimalarial",
      csb_filter = NULL
    ),
    list(
      name = "antimalarial_public",
      outcome = "has_antimalarial",
      csb_filter = "csb_public"
    )
  )
}


#' Prepare Single Antimalarial Indicator for MBG
#'
#' Convenience wrapper around [calc_antimalarial_mbg()] to prepare antimalarial
#' treatment data for MBG analysis.
#'
#' @inheritParams calc_antimalarial_mbg
#' @param indicator Single indicator name. Default: "antimalarial".
#'
#' @return A data.table with columns: cluster_id, indicator, samplesize, x, y
#' @export
prep_antimalarial_mbg <- function(
  dhs_kr,
  gps_data,
  indicator = "antimalarial",
  survey_vars = list(
    cluster = "v001",
    age = "hw1",
    fever = "h22"
  ),
  gps_vars = list(
    cluster = "DHSCLUST",
    lat = "LATNUM",
    lon = "LONGNUM"
  )
) {
  result <- calc_antimalarial_mbg(
    dhs_kr = dhs_kr,
    gps_data = gps_data,
    indicators = indicator,
    survey_vars = survey_vars,
    gps_vars = gps_vars
  )

  if (length(result) == 0) {
    cli::cli_abort("No data returned for indicator {.val {indicator}}")
  }

  result[[1]]
}


# ---- dhs_helpers_antimalarial.R ----

#' Prepare Antimalarial Data for Analysis
#'
#' Shared data cleaning and indicator computation for antimalarial functions.
#' Used by both calc_antimalarial_dhs_core() and calc_case_management_dhs().
#'
#' @param dhs_kr DHS Children's Recode (KR) dataset.
#' @param survey_vars Named list mapping DHS variable names.
#' @param include_survey_vars Logical. If TRUE, includes survey design columns.
#'
#' @return A data frame of febrile U5 children with columns:
#'   cluster_id, age_months, received_antimalarial, ml13_vars_found.
#'   If include_survey_vars = TRUE, also: survey_weight, stratum_id.
#'
#' @noRd
.prepare_antimalarial_data <- function(
  dhs_kr,
  survey_vars,
  include_survey_vars = FALSE
) {
  if (!is.data.frame(dhs_kr)) {
    cli::cli_abort("`dhs_kr` must be a data.frame or tibble.")
  }
  if (nrow(dhs_kr) == 0) {
    cli::cli_abort("`dhs_kr` is empty.")
  }

  # Auto-detect age variable if specified one is missing
  # Fallback order: hw1 (anthropometry) -> hc1 (standard KR) -> b8 (current age)
  if (!survey_vars$age %in% names(dhs_kr)) {
    age_candidates <- c("hc1", "b8", "hw1")
    available_age <- intersect(age_candidates, names(dhs_kr))

    if (length(available_age) > 0) {
      old_age_var <- survey_vars$age
      survey_vars$age <- available_age[1]
      cli::cli_alert_info(
        "Age variable {.var {old_age_var}} not found; using {.var {survey_vars$age}} instead"
      )
    }
  }

  # Check required columns
  needed <- c(survey_vars$cluster, survey_vars$age, survey_vars$fever)
  if (include_survey_vars) {
    needed <- c(needed, survey_vars$weight, survey_vars$stratum)
  }
  missing_vars <- setdiff(needed, names(dhs_kr))
  if (length(missing_vars) > 0) {
    cli::cli_abort(c(
      "Required variables not found: {.var {missing_vars}}",
      "i" = "Check your survey_vars mapping"
    ))
  }

  # Auto-detect available antimalarial variables using label-based filtering.
  # Only include variables whose labels contain actual drug names -- excludes
  # non-drug response codes ("Don't know", "Other", "No treatment") that would
  # inflate the antimalarial composite.
  # Prefer ml13* series (drug-specific, newer surveys);
  # fall back to h37* series (older DHS surveys use h37a-h for drug-specific treatment).
  antimalarial_pattern <- paste0(
    "antimalarial|fansidar|chloroquine|amodiaquine|quinine|",
    "artemether|artesunate|dihydroartemis|artemisinin|coartem|",
    "\\bsp\\b|\\bcta\\b|\\bact\\b|mefloquine|piperaquine|lumefantrine"
  )

  # Label-based detection from original dhs_kr (pre-zap)
  .detect_am_labels <- function(candidates) {
    matched <- character(0)
    for (v in candidates) {
      lbl <- attr(dhs_kr[[v]], "label")
      if (is.null(lbl) || !is.character(lbl) ||
          length(lbl) != 1) next
      if (grepl(antimalarial_pattern, lbl, ignore.case = TRUE)) {
        matched <- c(matched, v)
      }
    }
    matched
  }

  ml13_candidates <- grep("^ml13[a-z]+$", names(dhs_kr), value = TRUE)
  h37_candidates  <- grep("^h37[a-z]+$", names(dhs_kr), value = TRUE)

  # Stage 1: label-based detection
  ml13_vars <- .detect_am_labels(ml13_candidates)
  h37_vars  <- .detect_am_labels(h37_candidates)

  # Stage 2: if no labels matched, fall back to standard drug slots (a-h)
  if (length(ml13_vars) == 0 && length(h37_vars) == 0) {
    ml13_vars <- grep("^ml13[a-h]$", names(dhs_kr), value = TRUE)
    h37_vars  <- grep("^h37[a-h]$", names(dhs_kr), value = TRUE)
  }

  use_h37_fallback <- FALSE

  if (length(ml13_vars) > 0) {
    # Check if ml13 series has any positive values (zap labels for safe comparison)
    ml13_has_data <- any(sapply(ml13_vars, function(v) {
      vals <- as.vector(haven::zap_labels(dhs_kr[[v]]))
      any(vals == 1, na.rm = TRUE)
    }))
    if (ml13_has_data) {
      cli::cli_alert_info(
        "Detected {length(ml13_vars)} ml13 antimalarial variables: {paste(ml13_vars, collapse = ', ')}"
      )
    } else if (length(h37_vars) > 0) {
      h37_has_data <- any(sapply(h37_vars, function(v) {
        vals <- as.vector(haven::zap_labels(dhs_kr[[v]]))
        any(vals == 1, na.rm = TRUE)
      }))
      if (h37_has_data) {
        cli::cli_alert_info(
          "ml13* variables have no positive values; using h37* series which has data: {paste(h37_vars, collapse = ', ')}"
        )
        ml13_vars <- character(0)
        use_h37_fallback <- TRUE
      } else {
        cli::cli_alert_info(
          "Detected {length(ml13_vars)} ml13 antimalarial variables (no positive values found)"
        )
      }
    } else {
      cli::cli_alert_info(
        "Detected {length(ml13_vars)} ml13 antimalarial variables (no positive values found)"
      )
    }
  } else if (length(h37_vars) > 0) {
    cli::cli_alert_info(
      "No ml13* variables found; using h37* series as fallback: {paste(h37_vars, collapse = ', ')}"
    )
    use_h37_fallback <- TRUE
  } else {
    cli::cli_abort(c(
      "No antimalarial treatment variables found in data.",
      "i" = "Checked for ml13a/ml13b/... (newer surveys) and h37a-h (older surveys).",
      "i" = "Verify that this survey includes malaria treatment questions."
    ))
  }

  if (length(ml13_vars) == 0 && !use_h37_fallback) {
    cli::cli_abort("No antimalarial treatment variables with data found.")
  }

  # Zap labels
  kr <- dhs_kr |>
    dplyr::mutate(dplyr::across(dplyr::everything(), haven::zap_labels)) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), as.vector))

  # Force indicator columns to numeric (guards against haven character residuals)
  num_cols <- c(
    survey_vars$age, survey_vars$fever, survey_vars$alive,
    if (use_h37_fallback) h37_vars else ml13_vars
  )
  for (col in num_cols) {
    if (!is.null(col) && col %in% names(kr)) {
      kr[[col]] <- suppressWarnings(as.numeric(as.character(kr[[col]])))
    }
  }

  # Build columns
  kr <- kr |>
    dplyr::mutate(
      cluster_id = .data[[survey_vars$cluster]],
      age_months = .data[[survey_vars$age]],
      had_fever = .data[[survey_vars$fever]]
    )

  if (include_survey_vars) {
    kr <- kr |>
      dplyr::mutate(
        survey_weight = .data[[survey_vars$weight]] / 1e6,
        stratum_id = .data[[survey_vars$stratum]]
      )
  }

  # Check alive variable if present
  has_alive <- !is.null(survey_vars$alive) &&
    survey_vars$alive %in% names(dhs_kr)
  if (has_alive) {
    kr <- kr |>
      dplyr::mutate(child_alive = .data[[survey_vars$alive]])
  }

  # Filter to U5 children
  kr_u5 <- kr |>
    dplyr::filter(
      age_months >= 0,
      age_months <= 59
    )

  if (has_alive) {
    kr_u5 <- kr_u5 |>
      dplyr::filter(child_alive == 1)
  }

  # Detect fever coding scheme: Some surveys use 0=No/1=Yes, others use 1=No/2=Yes
  fever_values <- unique(kr_u5$had_fever[!is.na(kr_u5$had_fever)])

  if (length(fever_values) == 0) {
    cli::cli_abort(
      "Fever variable has no valid values. Check that {.var {survey_vars$fever}} exists and contains data."
    )
  }

  # Determine "Yes" value: if values are strictly {1, 2} or {2}, assume 2=Yes
  # Otherwise, assume 1=Yes (standard DHS coding)
  if (all(fever_values %in% c(1, 2)) && 2 %in% fever_values && !0 %in% fever_values) {
    fever_yes_value <- 2
    cli::cli_alert_info(
      "Detected alternative fever coding (1=No, 2=Yes) - using 2 as 'Yes'"
    )
  } else {
    fever_yes_value <- 1
  }

  # Filter to children with fever
  kr_fever <- kr_u5 |>
    dplyr::filter(had_fever == fever_yes_value)

  if (nrow(kr_fever) == 0) {
    n_with_data <- sum(!is.na(kr_u5$had_fever))
    cli::cli_abort(c(
      "No children with fever in the last 2 weeks found.",
      "i" = "Total U5 children: {nrow(kr_u5)}",
      "i" = "Children with fever data: {n_with_data}",
      "i" = "Unique fever values: {paste(sort(fever_values), collapse = ', ')}",
      "i" = "Expected 'Yes' value: {fever_yes_value}"
    ))
  }

  # Create binary antimalarial indicator
  if (use_h37_fallback) {
    # h37* series: 1 if ANY drug variable == 1
    # Each h37x records whether a specific drug was taken for fever/cough
    h37_matrix <- as.matrix(kr_fever[, h37_vars, drop = FALSE])
    h37_matrix[!h37_matrix %in% c(0, 1)] <- NA
    kr_fever$received_antimalarial <- apply(h37_matrix, 1, function(row) {
      if (any(row == 1, na.rm = TRUE)) return(1)
      if (any(is.na(row))) return(NA_real_)
      return(0)
    })
    attr(kr_fever, "ml13_vars_found") <- h37_vars
  } else {
    # ml13* series: 1 if ANY drug variable == 1
    ml13_matrix <- as.matrix(kr_fever[, ml13_vars, drop = FALSE])
    ml13_matrix[!ml13_matrix %in% c(0, 1)] <- NA
    kr_fever$received_antimalarial <- apply(ml13_matrix, 1, function(row) {
      if (any(row == 1, na.rm = TRUE)) return(1)
      if (any(is.na(row))) return(NA_real_)
      return(0)
    })
    attr(kr_fever, "ml13_vars_found") <- ml13_vars
  }

  if (all(is.na(kr_fever$received_antimalarial))) {
    cli::cli_abort("All antimalarial variables are NA for febrile children")
  }

  # Create binary indicator for survey estimation
  kr_fever <- kr_fever |>
    dplyr::mutate(
      has_antimalarial = dplyr::if_else(
        received_antimalarial == 1, 1, 0, missing = NA_real_
      )
    )

  cli::cli_alert_info(
    "Found {format(nrow(kr_fever), big.mark = ',')} febrile children under 5"
  )

  kr_fever
}


