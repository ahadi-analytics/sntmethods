# u5mr indicator
#
# Merged from: dhs_calc_u5mr.R dhs_calc_u5mr_mbg.R 
# Contains the survey-weighted calc, MBG cluster-prep, and indicator-
# specific helpers for this family.

# ---- dhs_calc_u5mr.R ----

#' Calculate Under-5 Mortality Rate (U5MR) from DHS data using DHS.rates
#'
#' core function that estimates under-5 mortality rate (U5MR) using the
#' DHS.rates::chmort() function following standard DHS methodology. when gps
#' and shapefile are provided, joins spatial data to assign admin boundaries
#' to each child record before calculating U5MR at the specified admin level.
#'
#' @details
#' Methodology: \url{https://github.com/ahadi-analytics/sntmethods/blob/master/inst/methods/u5mr_dhs.yml}
#'
#' @param dhs_kr dhs children's recode (KR) dataset in tidy format
#'   (data.frame or tibble).
#' @param survey_vars named list mapping dhs variable names. required keys:
#'   \itemize{
#'     \item `cluster`: cluster id (default: "v021")
#'     \item `weight`: survey weight (default: "v005")
#'     \item `stratum`: stratum variable (default: "v022")
#'     \item `interview_date`: date of interview (default: "v008")
#'     \item `birth_date`: child's birth date (default: "b3")
#'     \item `age_at_death`: age at death in months (default: "b7")
#'   }
#' @param period_years years before survey to calculate rates (default: 5).
#' @param gps_data optional dhs gps dataset with cluster coordinates.
#' @param gps_vars named list for gps variables (cluster, lat, lon).
#' @param shapefile optional sf object with administrative boundaries.
#' @param admin_level character vector of admin columns from shapefile
#'   (for example, c("adm1", "adm2")). if NULL, uses existing admin
#'   variables in data.
#' @param join_nearest logical; if TRUE, assigns clusters outside polygons
#'   to nearest admin unit.
#'
#' @return tibble with U5MR estimates by administrative level, with
#'   confidence intervals and sample sizes.
#'
#' @keywords internal
calc_u5mr_dhs_core <- function(
  dhs_kr,
  survey_vars = list(
    cluster = "v021",
    weight = "v005",
    stratum = "v022",
    interview_date = "v008",
    birth_date = "b3",
    age_at_death = "b7"
  ),
  period_years = 5,
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
  # ---- 1. input validation ---------------------------------------------------

  if (!is.data.frame(dhs_kr)) {
    cli::cli_abort("`dhs_kr` must be a data.frame or tibble.")
  }

  if (nrow(dhs_kr) == 0) {
    cli::cli_abort("`dhs_kr` is empty.")
  }

  if (!requireNamespace("DHS.rates", quietly = TRUE)) {
    cli::cli_abort(
      c(
        "Package 'DHS.rates' is required but not installed.",
        "i" = "Install it with: install.packages('DHS.rates')"
      )
    )
  }

  # check required survey variables
  needed <- unlist(
    survey_vars[
      c(
        "cluster",
        "weight",
        "stratum",
        "interview_date",
        "birth_date"
      )
    ]
  )

  missing_vars <- setdiff(needed, names(dhs_kr))

  if (length(missing_vars) > 0) {
    cli::cli_abort(
      c(
        "Required variables not found: {.var {missing_vars}}",
        "i" = "Check your survey_vars mapping"
      )
    )
  }

  # handle missing age_at_death variable
  age_death_var <- survey_vars$age_at_death %||% "b7"

  if (!age_death_var %in% names(dhs_kr)) {
    cli::cli_warn(
      c(
        "Mortality variable {.var {age_death_var}} not found in KR recode; \\
        U5MR cannot be estimated - skipping.",
        "i" = paste0(
          "U5MR requires the age-at-death variable (standard DHS code ",
          "{.val b7}), which is present in standard DHS recode files but ",
          "typically absent from Malaria Indicator Surveys (MIS)."
        ),
        "*" = paste0(
          "Use a standard DHS KR recode file, or pass the correct variable ",
          "name via {.arg survey_vars$age_at_death}."
        )
      )
    )
    return(NULL)
  }

  # Guard: if age_at_death column exists but is entirely NA, skip
  if (all(is.na(dhs_kr[[age_death_var]]))) {
    cli::cli_warn(
      "{.var {age_death_var}} is entirely NA; U5MR cannot be estimated - skipping"
    )
    return(NULL)
  }

  # ---- 2. join gps and shapefile if provided --------------------------------

  class_var <- NULL

  if (!is.null(gps_data) && !is.null(shapefile)) {
    cli::cli_alert_info(
      "joining GPS coordinates and administrative boundaries"
    )

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

    dhs_kr <- dhs_kr |>
      dplyr::left_join(
        gps_clean,
        by = stats::setNames("cluster_id", survey_vars$cluster)
      )

    clusters_sf <- dhs_kr |>
      dplyr::select(
        !!survey_vars$cluster,
        lat,
        lon
      ) |>
      dplyr::distinct() |>
      dplyr::filter(
        !is.na(lat),
        !is.na(lon)
      ) |>
      sf::st_as_sf(
        coords = c("lon", "lat"),
        crs = 4326
      )

    shapefile <- shapefile |>
      sf::st_transform(4326) |>
      sf::st_make_valid()

    if (is.null(admin_level)) {
      available_admins <- names(shapefile)[
        grep("^adm[0-9]+$", names(shapefile))
      ]

      if (length(available_admins) == 0) {
        cli::cli_abort(
          "no admin columns (adm0, adm1, adm2, etc.) found in shapefile"
        )
      }

      admin_level <- available_admins

      cli::cli_alert_info(
        "using admin levels: {paste(admin_level, collapse = ', ')}"
      )
    }

    missing_cols <- setdiff(admin_level, names(shapefile))

    if (length(missing_cols) > 0) {
      cli::cli_abort(
        "admin columns not found in shapefile: ",
        "{paste(missing_cols, collapse = ', ')}"
      )
    }

    admin_name_cols <- paste0(admin_level, "_name")
    admin_name_cols <- admin_name_cols[
      admin_name_cols %in% names(shapefile)
    ]
    all_admin_cols <- c(admin_level, admin_name_cols)

    cluster_admin <- sf::st_join(
      clusters_sf,
      shapefile[, c(all_admin_cols, "geometry")],
      join = sf::st_within,
      left = TRUE
    )

    if (join_nearest) {
      unmatched <- is.na(cluster_admin[[admin_level[1]]])

      if (any(unmatched)) {
        cli::cli_alert_info(
          "assigning {sum(unmatched)} clusters to nearest admin units"
        )

        nearest_idx <- sf::st_nearest_feature(
          cluster_admin[unmatched, ],
          shapefile
        )

        for (col in all_admin_cols) {
          if (col %in% names(shapefile)) {
            cluster_admin[unmatched, col] <- shapefile[[col]][
              nearest_idx
            ]
          }
        }
      }
    }

    cluster_admin_df <- sf::st_drop_geometry(cluster_admin)

    dhs_kr <- dhs_kr |>
      dplyr::left_join(
        cluster_admin_df,
        by = survey_vars$cluster
      )

    if (length(admin_level) > 1) {
      dhs_kr$admin_class <- apply(
        dhs_kr[, admin_level, drop = FALSE],
        1,
        paste,
        collapse = "_"
      )
      class_var <- "admin_class"
    } else {
      class_var <- admin_level[1]
    }
  } else if (!is.null(shapefile)) {
    cli::cli_alert_warning(
      "shapefile provided without GPS data; using existing admin vars"
    )

    existing_admins <- c("v024", "v025", "sdist")
    found_admin <- existing_admins[
      existing_admins %in% names(dhs_kr)
    ][1]

    if (!is.na(found_admin)) {
      class_var <- found_admin
      cli::cli_alert_info(
        "using {.var {found_admin}} as grouping variable"
      )
    }
  } else if (!is.null(gps_data)) {
    cli::cli_alert_info(
      "GPS provided without shapefile; calculating cluster-level U5MR"
    )
    class_var <- survey_vars$cluster
  } else {
    if ("v024" %in% names(dhs_kr)) {
      class_var <- "v024"
      cli::cli_alert_info(
        "using v024 (region) as grouping variable"
      )
    }
  }

  # ---- 3. calculate U5MR using DHS.rates ------------------------------------

  if (!is.null(class_var)) {
    cli::cli_alert_info(
      "calculating U5MR by {.var {class_var}} using DHS.rates::chmort()"
    )
  } else {
    cli::cli_alert_info(
      "calculating national-level U5MR using DHS.rates::chmort()"
    )
  }

  mort_results <- tryCatch(
    DHS.rates::chmort(
      Data = dhs_kr,
      JK = "Yes",
      Strata = survey_vars$stratum,
      Cluster = survey_vars$cluster,
      Weight = survey_vars$weight,
      Date_of_interview = survey_vars$interview_date,
      Date_of_birth = survey_vars$birth_date,
      Age_at_death = age_death_var,
      Period = period_years * 12,
      Class = class_var
    ),
    error = function(e) {
      cli::cli_abort(
        c(
          "Error in DHS.rates::chmort()",
          "x" = e$message,
          "i" = "Check data structure and survey design"
        )
      )
    }
  )

  # ---- 4. extract and format results ----------------------------------------

  mort_df <- as.data.frame(mort_results)

  if (!is.null(class_var)) {
    if ("Class" %in% names(mort_df)) {
      u5mr_rows <- mort_df[
        grepl("U5MR", rownames(mort_df), ignore.case = TRUE),
        ,
        drop = FALSE
      ]

      results <- u5mr_rows |>
        dplyr::rename(
          !!class_var := Class,
          dhs_u5mr = R
        ) |>
        dplyr::mutate(
          dhs_u5mr_low = if ("LCI" %in% names(u5mr_rows)) {
            LCI
          } else {
            NA_real_
          },
          dhs_u5mr_upp = if ("UCI" %in% names(u5mr_rows)) {
            UCI
          } else {
            NA_real_
          }
        ) |>
        dplyr::select(
          !!class_var,
          dhs_u5mr,
          dhs_u5mr_low,
          dhs_u5mr_upp
        )
    } else {
      u5mr_row <- mort_df[
        grepl("U5MR", rownames(mort_df), ignore.case = TRUE)[1],
        ,
        drop = FALSE
      ]

      results <- tibble::tibble(
        !!class_var := names(u5mr_row),
        dhs_u5mr = as.numeric(u5mr_row[1, ])
      ) |>
        dplyr::mutate(
          dhs_u5mr_low = NA_real_,
          dhs_u5mr_upp = NA_real_
        )
    }

    if (class_var == "admin_class" && length(admin_level) > 1) {
      admin_splits <- stringr::str_split(
        results$admin_class,
        "_",
        simplify = TRUE
      )

      for (i in seq_along(admin_level)) {
        results[[admin_level[i]]] <- admin_splits[, i]
      }
    }

    if (!is.null(shapefile)) {
      admin_name_cols <- paste0(admin_level, "_name")
      admin_name_cols <- admin_name_cols[
        admin_name_cols %in% names(shapefile)
      ]

      if (length(admin_name_cols) > 0) {
        admin_lookup <- sf::st_drop_geometry(shapefile) |>
          dplyr::select(
            dplyr::all_of(c(admin_level, admin_name_cols))
          ) |>
          dplyr::distinct()

        results <- results |>
          dplyr::left_join(
            admin_lookup,
            by = admin_level
          )
      }
    } else {
      admin_name_cols <- character(0)
    }

    sample_sizes <- dhs_kr |>
      dplyr::group_by(
        dplyr::across(
          dplyr::all_of(
            intersect(
              names(dhs_kr),
              c(class_var, admin_level)
            )
          )
        )
      ) |>
      dplyr::summarise(
        dhs_n_births = dplyr::n(),
        dhs_n_deaths = sum(
          !is.na(.data[[age_death_var]]) &
            .data[[age_death_var]] < 60,
          na.rm = TRUE
        ),
        .groups = "drop"
      )

    join_cols <- intersect(
      names(results),
      names(sample_sizes)
    )

    if (length(join_cols) > 0) {
      results <- results |>
        dplyr::left_join(
          sample_sizes,
          by = join_cols
        )
    }

    col_order <- c(
      admin_level,
      admin_name_cols,
      "dhs_n_births",
      "dhs_n_deaths",
      "dhs_u5mr",
      "dhs_u5mr_low",
      "dhs_u5mr_upp"
    )

    col_order <- intersect(col_order, names(results))

    other_cols <- setdiff(
      names(results),
      c(col_order, "admin_class")
    )

    results <- results |>
      dplyr::select(
        dplyr::all_of(c(col_order, other_cols))
      )
  } else {
    u5mr_row <- mort_df[
      grepl("U5MR", rownames(mort_df), ignore.case = TRUE)[1],
      ,
      drop = FALSE
    ]

    results <- tibble::tibble(
      level = "National",
      dhs_u5mr = u5mr_row$R,
      dhs_u5mr_low = if ("LCI" %in% names(u5mr_row)) {
        u5mr_row$LCI
      } else {
        NA_real_
      },
      dhs_u5mr_upp = if ("UCI" %in% names(u5mr_row)) {
        u5mr_row$UCI
      } else {
        NA_real_
      },
      dhs_n_births = nrow(dhs_kr),
      dhs_n_deaths = sum(
        !is.na(dhs_kr[[age_death_var]]) &
          dhs_kr[[age_death_var]] < 60,
        na.rm = TRUE
      )
    )
  }

  tibble::as_tibble(results)
}

# helper for NULL default
`%||%` <- function(x, y) if (is.null(x)) y else x


# =============================================================================
# U5MR indicator conditions and dictionary
# =============================================================================

#' Internal: U5MR indicator conditions
#'
#' Returns list of indicator specifications for childhood mortality rates.
#' U5MR is a rate (per 1000 live births), not a proportion, so it is NOT
#' computed via `.compute_dhs_indicator_generic()`.
#'
#' @return List of named lists.
#' @noRd
.u5mr_conditions <- function() {
  list(
    list(
      indicator       = "U5MR",
      indicator_code  = "u5mr",
      indicator_title = "Under-5 mortality rate",
      outcome_var     = NA_character_,
      filter_expr     = NULL,
      num_desc        = "Deaths under age 5",
      denom_desc      = "Live births (synthetic cohort life table)",
      denom_code      = "live_births"
    )
  )
}


#' U5MR Indicator Dictionary
#'
#' Returns the dictionary of U5MR indicators with metadata.
#'
#' @return Tibble with columns: indicator, indicator_code, indicator_title,
#'   numerator_description, denominator_description, denominator_code.
#'
#' @keywords internal
u5mr_dictionary <- function() {
  conds <- .u5mr_conditions()
  tibble::tibble(
    indicator               = vapply(conds, `[[`, character(1), "indicator"),
    indicator_code          = vapply(conds, `[[`, character(1), "indicator_code"),
    indicator_title         = vapply(conds, `[[`, character(1), "indicator_title"),
    numerator_description   = vapply(conds, `[[`, character(1), "num_desc"),
    denominator_description = vapply(conds, `[[`, character(1), "denom_desc"),
    denominator_code        = vapply(conds, `[[`, character(1), "denom_code")
  )
}


#' Calculate U5MR from DHS Data (Standardized Long Format)
#'
#' Estimates under-5 mortality rate (U5MR) from DHS Children's Recode data
#' using the DHS.rates package. Returns results in standardized long format
#' with `list(adm0, adm1)` structure.
#'
#' U5MR is computed via `DHS.rates::chmort()` (synthetic cohort life table
#' method), NOT via `svyciprop()`, because it is a rate per 1000 live births
#' rather than a proportion.
#'
#' @param dhs_kr DHS Children's Recode (KR) dataset in tidy format.
#' @param survey_vars Named list mapping DHS variable names. See
#'   [calc_u5mr_dhs_core()].
#' @param period_years Years before survey to calculate rates (default: 5).
#' @param region_var Optional column name for subnational grouping
#'   (e.g., "v024"). Auto-falls back to "v024" if no spatial params.
#' @param gps_data Optional DHS GPS dataset with cluster coordinates.
#' @param gps_vars Named list for GPS variables (cluster, lat, lon).
#' @param shapefile Optional sf object with administrative boundaries.
#' @param admin_level Character vector specifying aggregation levels.
#' @param join_nearest Logical; if TRUE, assigns clusters outside all
#'   polygons to nearest administrative unit.
#'
#' @return Named list of tibbles:
#'   \describe{
#'     \item{`adm0`}{National-level estimates (always present)}
#'     \item{`adm1`}{Admin-1 estimates (when region_var or shapefile used)}
#'   }
#'   Each tibble contains columns: survey_id, iso3, iso2, survey_type,
#'   survey_year, adm0, adm1, type, geo_source, point, ci_l, ci_u,
#'   numerator, denominator, indicator, indicator_code,
#'   numerator_description, denominator_description, denominator_code.
#'
#' @seealso [u5mr_dictionary()] for indicator definitions,
#'   [calc_u5mr_dhs_core()] for the legacy wide-format output
#' @export
calc_u5mr_dhs <- function(
  dhs_kr,
  survey_vars = list(
    cluster = "v021",
    weight = "v005",
    stratum = "v022",
    interview_date = "v008",
    birth_date = "b3",
    age_at_death = "b7"
  ),
  period_years = 5,
  region_var   = NULL,
  gps_data     = NULL,
  gps_vars     = list(
    cluster = "DHSCLUST",
    lat     = "LATNUM",
    lon     = "LONGNUM"
  ),
  shapefile    = NULL,
  admin_level  = NULL,
  join_nearest = TRUE
) {
  # Fail fast on missing suggested dependencies
  .check_pkg(
    c("stringr", "tibble"),
    reason = "for `calc_u5mr_dhs()`"
  )

  # ---- 1. Input validation ----

  if (!is.data.frame(dhs_kr)) {
    cli::cli_abort("`dhs_kr` must be a data.frame or tibble.")
  }
  if (nrow(dhs_kr) == 0) {
    cli::cli_abort("`dhs_kr` is empty.")
  }

  # Pre-flight check for age-at-death variable.
  # MIS surveys typically lack b7; rather than aborting and breaking a
  # multi-survey loop, warn and return NULL so the caller can skip cleanly.
  age_death_var <- survey_vars$age_at_death %||% "b7"
  if (!age_death_var %in% names(dhs_kr) ||
      all(is.na(dhs_kr[[age_death_var]]))) {
    cli::cli_warn(
      c(
        "Age-at-death variable {.var {age_death_var}} not available; \\
        U5MR cannot be estimated - skipping.",
        "i" = paste0(
          "U5MR requires the standard DHS variable {.val b7}, which is ",
          "typically present in standard DHS recodes but absent from ",
          "Malaria Indicator Surveys (MIS)."
        )
      )
    )
    return(NULL)
  }

  # ---- 2. Extract survey metadata ----

  survey_meta <- .extract_survey_meta(dhs_kr)

  # ---- 3. Determine region variable ----

  # Auto-fallback to v024 when no spatial parameters provided
  if (is.null(region_var) && is.null(gps_data) && is.null(shapefile)) {
    if ("v024" %in% names(dhs_kr)) {
      region_var <- "v024"
      cli::cli_alert_info(
        "No region_var/GPS/shapefile specified; defaulting to {.var v024} for adm1"
      )
    }
  }

  # ---- 4. Compute U5MR via core function ----

  # Resolve class_var for DHS.rates::chmort()
  class_var_for_core <- NULL
  geo_src <- NA_character_

  if (!is.null(region_var) && region_var %in% names(dhs_kr)) {
    class_var_for_core <- region_var
    geo_src <- "survey"
  }

  # National computation (no Class variable)
  mort_national <- tryCatch(
    .compute_u5mr_chmort(
      dhs_kr      = dhs_kr,
      survey_vars = survey_vars,
      period_years = period_years,
      class_var   = NULL
    ),
    error = function(e) {
      cli::cli_warn("National U5MR computation failed: {e$message}")
      NULL
    }
  )

  # Regional computation (with Class variable)
  mort_regional <- NULL
  if (!is.null(class_var_for_core)) {
    mort_regional <- tryCatch(
      .compute_u5mr_chmort(
        dhs_kr      = dhs_kr,
        survey_vars = survey_vars,
        period_years = period_years,
        class_var   = class_var_for_core
      ),
      error = function(e) {
        cli::cli_warn("Regional U5MR computation failed: {e$message}")
        NULL
      }
    )
  }

  # ---- 5. Condition metadata ----

  cond <- .u5mr_conditions()[[1]]

  age_death_var <- survey_vars$age_at_death %||% "b7"

  # ---- 6. Build national long-format tibble ----

  meta_cols <- tibble::tibble(
    survey_id   = survey_meta$survey_id,
    iso3        = survey_meta$iso3,
    iso2        = survey_meta$iso2,
    survey_type = survey_meta$survey_type,
    survey_year = survey_meta$survey_year,
    adm0        = survey_meta$country_upper
  )

  # National row
  if (!is.null(mort_national)) {
    mort_nat_df <- as.data.frame(mort_national)
    u5mr_idx <- which(grepl("U5MR", rownames(mort_nat_df), ignore.case = TRUE))
    if (length(u5mr_idx) == 0) u5mr_idx <- 1L
    u5mr_row <- mort_nat_df[u5mr_idx[1], , drop = FALSE]

    n_births_nat <- nrow(dhs_kr)
    n_deaths_nat <- sum(
      !is.na(dhs_kr[[age_death_var]]) & dhs_kr[[age_death_var]] < 60,
      na.rm = TRUE
    )

    national_tbl <- dplyr::bind_cols(
      meta_cols,
      tibble::tibble(
        type       = "survey_weighted",
        geo_source = NA_character_,
        point      = round(u5mr_row$R, 1),
        ci_l       = if ("LCI" %in% names(u5mr_row)) round(u5mr_row$LCI, 1) else NA_real_,
        ci_u       = if ("UCI" %in% names(u5mr_row)) round(u5mr_row$UCI, 1) else NA_real_,
        numerator   = as.integer(n_deaths_nat),
        denominator = as.integer(n_births_nat),
        indicator               = cond$indicator_title,
        indicator_code          = cond$indicator_code,
        numerator_description   = cond$num_desc,
        denominator_description = cond$denom_desc,
        denominator_code        = cond$denom_code
      )
    ) |> tibble::as_tibble()
  } else {
    national_tbl <- dplyr::bind_cols(
      meta_cols,
      tibble::tibble(
        type       = "survey_weighted",
        geo_source = NA_character_,
        point      = NA_real_,
        ci_l       = NA_real_,
        ci_u       = NA_real_,
        numerator   = NA_integer_,
        denominator = as.integer(nrow(dhs_kr)),
        indicator               = cond$indicator_title,
        indicator_code          = cond$indicator_code,
        numerator_description   = cond$num_desc,
        denominator_description = cond$denom_desc,
        denominator_code        = cond$denom_code
      )
    ) |> tibble::as_tibble()
  }

  out <- list(adm0 = national_tbl)

  # ---- 7. Build regional long-format tibble ----

  if (!is.null(mort_regional) && !is.null(class_var_for_core)) {
    mort_reg_df <- as.data.frame(mort_regional)

    u5mr_rows <- mort_reg_df[
      grepl("U5MR", rownames(mort_reg_df), ignore.case = TRUE),
      , drop = FALSE
    ]

    if (nrow(u5mr_rows) > 0 && "Class" %in% names(u5mr_rows)) {
      # Resolve region labels
      region_labels <- .resolve_region_labels(
        dhs_kr[[class_var_for_core]], class_var_for_core
      )
      label_lookup <- stats::setNames(
        region_labels,
        as.character(as.vector(haven::zap_labels(dhs_kr[[class_var_for_core]])))
      )
      # Deduplicate
      label_lookup <- label_lookup[!duplicated(names(label_lookup))]

      # Sample sizes by region
      kr_zapped <- dhs_kr |>
        dplyr::mutate(dplyr::across(dplyr::everything(), haven::zap_labels)) |>
        dplyr::mutate(dplyr::across(dplyr::everything(), as.vector))

      sample_by_region <- kr_zapped |>
        dplyr::group_by(.data[[class_var_for_core]]) |>
        dplyr::summarise(
          n_births = dplyr::n(),
          n_deaths = sum(
            !is.na(.data[[age_death_var]]) & .data[[age_death_var]] < 60,
            na.rm = TRUE
          ),
          .groups = "drop"
        )

      regional_rows <- list()
      for (i in seq_len(nrow(u5mr_rows))) {
        class_val <- as.character(u5mr_rows$Class[i])
        region_name <- if (class_val %in% names(label_lookup)) {
          label_lookup[[class_val]]
        } else {
          toupper(class_val)
        }

        # Match sample sizes
        ss_match <- sample_by_region[
          as.character(sample_by_region[[class_var_for_core]]) == class_val,
        ]
        n_b <- if (nrow(ss_match) > 0) ss_match$n_births[1] else NA_integer_
        n_d <- if (nrow(ss_match) > 0) ss_match$n_deaths[1] else NA_integer_

        regional_rows[[i]] <- dplyr::bind_cols(
          meta_cols,
          tibble::tibble(
            adm1       = toupper(region_name),
            type       = "survey_weighted",
            geo_source = geo_src,
            point      = round(u5mr_rows$R[i], 1),
            ci_l       = if ("LCI" %in% names(u5mr_rows)) round(u5mr_rows$LCI[i], 1) else NA_real_,
            ci_u       = if ("UCI" %in% names(u5mr_rows)) round(u5mr_rows$UCI[i], 1) else NA_real_,
            numerator   = as.integer(n_d),
            denominator = as.integer(n_b),
            indicator               = cond$indicator_title,
            indicator_code          = cond$indicator_code,
            numerator_description   = cond$num_desc,
            denominator_description = cond$denom_desc,
            denominator_code        = cond$denom_code
          )
        )
      }

      adm1_tbl <- dplyr::bind_rows(regional_rows) |>
        tibble::as_tibble()

      out[["adm1"]] <- adm1_tbl
    }
  }

  out
}


#' Internal: Compute U5MR via DHS.rates::chmort()
#'
#' Thin wrapper around DHS.rates::chmort() that handles the call and
#' returns the raw result object.
#'
#' @param dhs_kr DHS KR dataset.
#' @param survey_vars Named list of variable mappings.
#' @param period_years Reference period in years.
#' @param class_var Optional class variable for subgroup estimates.
#' @return Raw result from DHS.rates::chmort().
#' @noRd
.compute_u5mr_chmort <- function(dhs_kr, survey_vars, period_years,
                                  class_var = NULL) {
  age_death_var <- survey_vars$age_at_death %||% "b7"

  DHS.rates::chmort(
    Data = dhs_kr,
    JK = "Yes",
    Strata = survey_vars$stratum,
    Cluster = survey_vars$cluster,
    Weight = survey_vars$weight,
    Date_of_interview = survey_vars$interview_date,
    Date_of_birth = survey_vars$birth_date,
    Age_at_death = age_death_var,
    Period = period_years * 12,
    Class = class_var
  )
}


#' Aggregate U5MR to administrative levels
#'
#' helper to aggregate U5MR results to administrative levels using a
#' shapefile. performs spatial joins and calculates weighted averages by
#' administrative unit.
#'
#' @param u5mr_results U5MR results with coordinates.
#' @param shapefile sf object with administrative boundaries.
#' @param admin_level character vector of admin levels to aggregate to.
#' @param weighted logical. if TRUE (default), uses births as weights.
#'
#' @return sf object with aggregated U5MR by administrative level.
#'
#' @keywords internal
#' @noRd
aggregate_u5mr_admin <- function(
  u5mr_results,
  shapefile,
  admin_level = c("adm1"),
  weighted = TRUE
) {
  if (!requireNamespace("sf", quietly = TRUE)) {
    cli::cli_abort("Package 'sf' is required for spatial operations")
  }

  if (!inherits(u5mr_results, "sf")) {
    if (!all(c("lat", "lon") %in% names(u5mr_results))) {
      cli::cli_abort(
        "u5mr_results must have lat and lon columns for spatial join"
      )
    }

    u5mr_sf <- u5mr_results |>
      sf::st_as_sf(
        coords = c("lon", "lat"),
        crs = 4326,
        remove = FALSE
      )
  } else {
    u5mr_sf <- u5mr_results
  }

  shapefile <- shapefile |>
    sf::st_transform(4326) |>
    sf::st_make_valid()

  joined <- sf::st_join(
    u5mr_sf,
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

  if (weighted && "dhs_n_births" %in% names(joined_df)) {
    aggregated <- joined_df |>
      dplyr::group_by(
        dplyr::across(
          dplyr::all_of(admin_level)
        )
      ) |>
      dplyr::summarise(
        dhs_u5mr = stats::weighted.mean(
          dhs_u5mr,
          w = dhs_n_births,
          na.rm = TRUE
        ),
        dhs_n_births = sum(
          dhs_n_births,
          na.rm = TRUE
        ),
        dhs_n_deaths = sum(
          dhs_n_deaths,
          na.rm = TRUE
        ),
        .groups = "drop"
      )
  } else {
    aggregated <- joined_df |>
      dplyr::group_by(
        dplyr::across(
          dplyr::all_of(admin_level)
        )
      ) |>
      dplyr::summarise(
        dhs_u5mr = mean(
          dhs_u5mr,
          na.rm = TRUE
        ),
        dhs_n_births = sum(
          dhs_n_births,
          na.rm = TRUE
        ),
        dhs_n_deaths = sum(
          dhs_n_deaths,
          na.rm = TRUE
        ),
        .groups = "drop"
      )
  }

  aggregated <- aggregated |>
    dplyr::mutate(
      dhs_u5mr = round(
        dhs_u5mr,
        1
      )
    )

  admin_name_cols <- paste0(admin_level, "_name")
  admin_name_cols <- admin_name_cols[
    admin_name_cols %in% names(shapefile)
  ]
  all_admin_cols <- c(admin_level, admin_name_cols)

  result_with_geometry <- shapefile |>
    dplyr::select(
      dplyr::all_of(all_admin_cols)
    ) |>
    dplyr::distinct() |>
    dplyr::left_join(
      aggregated,
      by = admin_level
    )

  result_with_geometry
}


# ---- dhs_calc_u5mr_mbg.R ----

#' Prepare U5MR Data for MBG Analysis
#'
#' Prepares cluster-level Under-5 Mortality Rate (U5MR) data for MBG analysis
#' using `DHS.rates::chmort()` for the mortality calculation. This follows the
#' standard DHS synthetic-cohort life-table methodology with 8 age segments.
#'
#' @details
#' Methodology: \url{https://github.com/ahadi-analytics/sntmethods/blob/master/inst/methods/u5mr_dhs.yml}
#'
#' `DHS.rates::chmort()` computes childhood mortality rates (NNMR, PNNMR, IMR,
#' CMR, U5MR) using the standard DHS synthetic-cohort approach. When called with
#' `Class = "v001"` (cluster ID), it produces per-cluster U5MR estimates. The
#' function uses 8 age segments (0-1, 1-3, 3-6, 6-12, 12-24, 24-36, 36-48,
#' 48-60 months) and applies partial-exposure weighting at period boundaries.
#'
#' Because `chmort()` internally computes a design effect (DEFT) via
#' [survey::svydesign()], and per-cluster subsets contain only a single PSU,
#' this function creates synthetic PSU and strata columns with two pseudo-PSUs
#' per cluster. Uniform weights are applied so that the cluster-level rates are
#' unweighted -- appropriate for MBG, which handles spatial smoothing and
#' uncertainty internally.
#'
#' **Important:** The `indicator` and `samplesize` columns are used by MBG to
#' model the death proportion (indicator/samplesize). The MBG pipeline
#' automatically converts model outputs to "per 1,000" units to match
#' epidemiological standards and the scale of the `u5mr` column.
#'
#' @param dhs_br DHS Birth Recode (BR) dataset. Must contain the standard DHS
#'   variables needed by `DHS.rates::chmort()`: cluster ID (v001), interview
#'   date in CMC (v008), child's date of birth in CMC (b3), and child's age at
#'   death in months (b7).
#' @param gps_data DHS GPS dataset with cluster coordinates.
#' @param period_years Number of years to look back for the mortality reference
#'   period. Default: 5 (standard DHS 5-year window). Passed to
#'   `DHS.rates::chmort()` as `Period = period_years * 12`.
#' @param survey_vars Named list mapping DHS variable names. Keys:
#'   \itemize{
#'     \item cluster: Cluster ID variable (default: "v001")
#'     \item interview_date: Date of interview in CMC (default: "v008")
#'     \item birth_date: Child's date of birth in CMC (default: "b3")
#'     \item age_at_death: Child's age at death in months (default: "b7")
#'   }
#' @param gps_vars Named list for GPS variable mapping.
#'
#' @return A list with one data.table named "u5mr" containing:
#'   \itemize{
#'     \item cluster_id: Cluster identifier
#'     \item indicator: Estimated number of deaths (derived from U5MR and exposure)
#'     \item samplesize: Number of births exposed in the 0-60 month window
#'     \item x: Longitude
#'     \item y: Latitude
#'     \item u5mr: U5MR per 1,000 live births
#'   }
#'   Returns NULL if required variables are missing or data is insufficient.
#'
#' @examples
#' \dontrun{
#' u5mr_mbg <- calc_u5mr_mbg(
#'   dhs_br = br_data,
#'   gps_data = gps_data
#' )
#' # Access combined U5MR
#' u5mr_mbg$u5mr
#' }
#'
#' @seealso [calc_u5mr_dhs()] for survey-weighted estimates
#' @export
calc_u5mr_mbg <- function(
  dhs_br,
  gps_data,
  period_years = 5,
  survey_vars = list(
    cluster        = "v001",
    interview_date = "v008",
    birth_date     = "b3",
    age_at_death   = "b7"
  ),
  gps_vars = list(
    cluster = "DHSCLUST",
    lat     = "LATNUM",
    lon     = "LONGNUM"
  )
) {
  # Fail fast on missing suggested dependencies
  .check_pkg(
    c("data.table"),
    reason = "for `calc_u5mr_mbg()`"
  )

  # ---- Check DHS.rates availability ----

  if (!requireNamespace("DHS.rates", quietly = TRUE)) {
    cli::cli_abort(
      c(
        "Package {.pkg DHS.rates} is required but not installed.",
        "i" = "Install it with: {.code install.packages('DHS.rates')}"
      )
    )
  }

  # ---- Input validation ----

  if (!is.data.frame(dhs_br)) {
    cli::cli_abort("{.arg dhs_br} must be a data.frame or tibble.")
  }

  if (nrow(dhs_br) == 0) {
    cli::cli_abort("{.arg dhs_br} is empty.")
  }

  if (!is.data.frame(gps_data)) {
    cli::cli_abort("{.arg gps_data} must be a data.frame or tibble.")
  }

  # Check required BR columns
  required_vars <- unlist(survey_vars)
  missing_vars <- setdiff(required_vars, names(dhs_br))

  if (length(missing_vars) > 0) {
    cli::cli_warn(
      "U5MR column(s) not found in BR data: {.var {missing_vars}}; U5MR not available for this survey"
    )
    return(NULL)
  }

  # Guard: if age_at_death is entirely NA, U5MR cannot be estimated
  age_death_var <- survey_vars$age_at_death
  if (all(is.na(dhs_br[[age_death_var]]))) {
    cli::cli_warn(
      "{.var {age_death_var}} is entirely NA; U5MR cannot be estimated - skipping"
    )
    return(NULL)
  }

  # ---- Prepare GPS data ----

  gps_clean <- .prepare_gps_data(gps_data, gps_vars)

  # ---- Prepare BR data for chmort() ----

  cluster_var <- survey_vars$cluster
  br_prepped <- .prepare_br_for_chmort(dhs_br, cluster_var)

  cli::cli_alert_info(
    "BR data: {format(nrow(br_prepped), big.mark = ',')} birth records ",
    "across {length(unique(br_prepped[[cluster_var]]))} clusters"
  )

  # ---- Call DHS.rates::chmort() per cluster ----

  cli::cli_alert_info(
    "Calculating cluster-level U5MR using {.fn DHS.rates::chmort} ",
    "(Period = {period_years * 12} months)"
  )

  # chmort() uses cat() for progress messages; suppress with capture.output()
  mort_results <- tryCatch({
      invisible(utils::capture.output({
        mort_raw <- DHS.rates::chmort(
          Data.Name         = br_prepped,
          Strata            = "v022",
          Cluster           = "v021",
          Weight            = "v005",
          Date_of_interview = survey_vars$interview_date,
          Date_of_birth     = survey_vars$birth_date,
          Age_at_death      = survey_vars$age_at_death,
          Period            = period_years * 12,
          Class             = cluster_var
        )
      }))
      mort_raw
    },
    error = function(e) {
      cli::cli_abort(
        c(
          "Error in {.fn DHS.rates::chmort}",
          "x" = e$message,
          "i" = "Check that BR data has valid variables: {.var {unlist(survey_vars)}}"
        )
      )
    }
  )

  # ---- Extract U5MR from chmort output ----

  # chmort() with Class returns a data.frame with columns: Class, R, N, WN
  # Row names follow the pattern: NNMR, PNNMR, IMR, CMR, U5MR for the first

  # class, then NNMR1, PNNMR1, ..., U5MR1 for the second, etc.

  mort_df <- as.data.frame(mort_results)

  if (!"Class" %in% names(mort_df) || !"R" %in% names(mort_df)) {
    cli::cli_abort(
      c(
        "Unexpected output format from {.fn DHS.rates::chmort}.",
        "i" = "Expected columns {.val Class}, {.val R}, {.val N} in output."
      )
    )
  }

  u5mr_rows <- mort_df[grepl("^U5MR", rownames(mort_df)), , drop = FALSE]

  if (nrow(u5mr_rows) == 0) {
    cli::cli_warn("No U5MR rows found in {.fn chmort} output; skipping")
    return(NULL)
  }

  # Build cluster-level results
  # R  = U5MR per 1,000 live births
  # N  = exposure (number of births in the 0-60 month window)
  # WN = weighted exposure (equal to N here since weights are uniform)
  u5mr_data <- data.frame(
    cluster_id = as.integer(as.character(u5mr_rows$Class)),
    u5mr       = as.numeric(u5mr_rows$R),
    samplesize = as.integer(u5mr_rows$N),
    stringsAsFactors = FALSE
  )

  # Drop clusters with NaN/NA U5MR (too few births for a valid estimate)
  na_mask <- is.na(u5mr_data$u5mr) | is.nan(u5mr_data$u5mr)
  if (any(na_mask)) {
    n_na <- sum(na_mask)
    cli::cli_alert_warning(
      "Dropping {n_na} cluster(s) with undefined U5MR (insufficient births)"
    )
    u5mr_data <- u5mr_data[!na_mask, ]
  }

  if (nrow(u5mr_data) == 0) {
    cli::cli_warn("No clusters with valid U5MR estimates; skipping")
    return(NULL)
  }

  # Derive approximate death count from rate and exposure
  # U5MR = deaths/exposed * 1000, so deaths ~ round(U5MR/1000 * exposed)
  u5mr_data$indicator <- as.integer(round(
    u5mr_data$u5mr / 1000 * u5mr_data$samplesize
  ))

  # ---- Join GPS coordinates ----

  u5mr_data <- dplyr::inner_join(u5mr_data, gps_clean, by = "cluster_id")

  if (nrow(u5mr_data) == 0) {
    cli::cli_warn(
      "No clusters matched between mortality results and GPS data; skipping"
    )
    return(NULL)
  }

  # ---- Format final output ----

  u5mr_output <- u5mr_data |>
    dplyr::transmute(
      cluster_id,
      indicator,
      samplesize,
      x,
      y,
      u5mr
    )

  n_clusters   <- nrow(u5mr_output)
  total_deaths <- sum(u5mr_output$indicator, na.rm = TRUE)
  total_exposed <- sum(u5mr_output$samplesize, na.rm = TRUE)
  mean_u5mr    <- round(mean(u5mr_output$u5mr, na.rm = TRUE), 1)

  cli::cli_alert_success(
    "U5MR: {n_clusters} clusters, {total_deaths} total deaths, ",
    "mean U5MR = {mean_u5mr} per 1,000 live births"
  )

  list(
    u5mr = data.table::as.data.table(u5mr_output)
  )
}


#' Prepare BR Data for Cluster-Level chmort()
#'
#' Creates synthetic survey-design columns required by `DHS.rates::chmort()`
#' when computing per-cluster mortality rates. Since `chmort()` internally
#' calls `DEFT()` which requires at least 2 PSUs per stratum -- even when
#' DEFT is not used in the output -- this function creates 2 pseudo-PSUs
#' per cluster and assigns uniform weights.
#'
#' @param dhs_br DHS Birth Recode dataset.
#' @param cluster_var Name of the cluster ID variable (default: "v001").
#'
#' @return A data.frame with synthetic v005, v021, v022 columns suitable
#'   for `chmort(Class = cluster_var)`.
#'
#' @noRd
.prepare_br_for_chmort <- function(dhs_br, cluster_var = "v001") {
  br <- as.data.frame(dhs_br)

  # Strip haven labels to avoid issues with DHS.rates internals
  for (col in names(br)) {
    if (inherits(br[[col]], "haven_labelled")) {
      br[[col]] <- as.vector(br[[col]])
    }
  }

  # Sort by cluster for deterministic PSU assignment
  br <- br[order(br[[cluster_var]]), ]

  # Create 2 pseudo-PSUs per cluster by alternating rows.
  # This satisfies svydesign() which requires >= 2 PSUs per stratum.
  br$v021 <- paste0(
    br[[cluster_var]], "_",
    stats::ave(
      rep(1L, nrow(br)),
      br[[cluster_var]],
      FUN = function(x) ((seq_along(x) - 1L) %% 2L) + 1L
    )
  )

  # Each cluster is its own stratum (since we want per-cluster rates)
  br$v022 <- br[[cluster_var]]

  # Uniform weights: MBG uses unweighted cluster-level rates.
  # DHS.rates divides v005 by 1e6, so v005 = 1e6 gives weight = 1.
  br$v005 <- 1e6

  # Drop clusters with only 1 birth (cannot form 2 pseudo-PSUs)
  cluster_counts <- table(br[[cluster_var]])
  singleton_clusters <- names(cluster_counts)[cluster_counts < 2]

  if (length(singleton_clusters) > 0) {
    cli::cli_alert_warning(
      "Dropping {length(singleton_clusters)} cluster(s) with < 2 births ",
      "(cannot compute mortality rate)"
    )
    br <- br[!br[[cluster_var]] %in% as.integer(singleton_clusters), ]
  }

  br
}


