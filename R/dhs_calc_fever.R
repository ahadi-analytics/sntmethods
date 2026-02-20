#' Calculate Fever Prevalence from DHS Data
#'
#' Estimates fever prevalence among children under 5 using survey-weighted
#' methods. This is step 0 of the WMR case management cascade.
#'
#' @details
#' Methodology: \url{https://github.com/ahadi-analytics/sntmethods/blob/master/inst/methods/fever_dhs.yml}
#'
#' @param dhs_kr DHS children's recode (KR) dataset (data.frame or tibble).
#' @param survey_vars Named list mapping DHS variable names. Required keys:
#'   \itemize{
#'     \item `cluster`: Cluster/PSU ID (default: "v021")
#'     \item `weight`: Survey weight (default: "v005")
#'     \item `stratum`: Stratum variable (default: "v022")
#'     \item `age`: Child's age in months (default: "hw1")
#'     \item `fever`: Had fever in last 2 weeks (default: "h22")
#'     \item `alive`: Child survival status (default: "b5")
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
#' @return Tibble with fever estimates by grouping level, including:
#'   \itemize{
#'     \item Grouping variables (region, admin level, or national)
#'     \item `dhs_fever`: Proportion with fever among U5 children
#'     \item `dhs_fever_low`, `dhs_fever_upp`: 95\% confidence interval
#'     \item `dhs_n_children`: Number of U5 children (denominator)
#'     \item `dhs_n_fever`: Number of febrile children
#'   }
#'
#' @details
#' This function calculates fever prevalence among alive U5 children.
#' The denominator is ALL alive children under 5, not just febrile children.
#' This differs from CSB/ACT which use febrile children as denominator.
#'
#' Fever prevalence is the entry point (step 0) of the WMR case management
#' cascade: Fever -> Sought care -> Tested -> Any antimalarial -> ACT.
#'
#' @examples
#' \dontrun{
#' fever_results <- calc_fever_dhs_core(
#'   dhs_kr = kr_data,
#'   region_var = "v024"
#' )
#' }
#'
#' @seealso [calc_csb_dhs_core()] for care-seeking behavior (step 1),
#'   [calc_case_management_dhs()] for the full cascade
#' @export
calc_fever_dhs_core <- function(
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

  kr_u5 <- .prepare_fever_data(
    dhs_kr = dhs_kr,
    survey_vars = survey_vars,
    include_survey_vars = TRUE
  )

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

    kr_u5 <- kr_u5 |>
      dplyr::left_join(gps_clean, by = "cluster_id")

    clusters_sf <- kr_u5 |>
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
    kr_u5 <- kr_u5 |>
      dplyr::left_join(cluster_admin_df, by = "cluster_id")

    if (length(admin_level) > 1) {
      kr_u5$admin_class <- apply(
        kr_u5[, admin_level, drop = FALSE], 1, paste, collapse = "_"
      )
      class_var <- "admin_class"
    } else {
      class_var <- admin_level[1]
    }
  } else if ("v024" %in% names(kr_u5)) {
    class_var <- "v024"
    cli::cli_alert_info("Using v024 (region) as grouping variable")
  }

  # ---- 4. Set up survey design ----

  use_strata <- dplyr::n_distinct(kr_u5$stratum_id) > 1

  if (use_strata) {
    survey_options <- options(survey.lonely.psu = "certainty")
    on.exit(options(survey_options), add = TRUE)

    des <- survey::svydesign(
      ids = ~cluster_id,
      strata = ~stratum_id,
      weights = ~survey_weight,
      data = kr_u5,
      nest = TRUE
    )
  } else {
    des <- survey::svydesign(
      ids = ~cluster_id,
      weights = ~survey_weight,
      data = kr_u5,
      nest = TRUE
    )
  }

  # ---- 5. Calculate fever prevalence ----

  if (!is.null(class_var)) {
    group_formula <- stats::as.formula(paste("~", class_var))
  } else {
    group_formula <- ~1
  }

  if (!is.null(class_var)) {
    fever_results <- tryCatch({
      survey::svyby(
        ~had_fever,
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
          data = kr_u5, nest = TRUE
        )
        survey::svyby(
          ~had_fever, by = group_formula, design = des_no_strata,
          FUN = survey::svymean, vartype = "ci", na.rm = TRUE,
          keep.names = FALSE
        ) |> tibble::as_tibble()
      } else {
        stop(e)
      }
    })
  } else {
    fever_mean <- survey::svymean(~had_fever, design = des, na.rm = TRUE)
    fever_ci <- stats::confint(fever_mean)

    fever_results <- tibble::tibble(
      level = "National",
      had_fever = as.numeric(fever_mean["had_fever"]),
      `ci_l.had_fever` = fever_ci["had_fever", 1],
      `ci_u.had_fever` = fever_ci["had_fever", 2]
    )
  }

  # Normalize CI column names (svyby uses ci_l/ci_u for single-variable formulas)
  names(fever_results)[names(fever_results) == "ci_l"] <- "ci_l.had_fever"
  names(fever_results)[names(fever_results) == "ci_u"] <- "ci_u.had_fever"

  # Rename columns
  fever_results <- fever_results |>
    dplyr::rename(
      dhs_fever = had_fever,
      dhs_fever_low = `ci_l.had_fever`,
      dhs_fever_upp = `ci_u.had_fever`
    )

  # ---- 6. Calculate sample sizes ----

  if (!is.null(class_var)) {
    sample_sizes <- kr_u5 |>
      dplyr::group_by(.data[[class_var]]) |>
      dplyr::summarise(
        dhs_n_children = dplyr::n(),
        dhs_n_fever = sum(had_fever == 1, na.rm = TRUE),
        .groups = "drop"
      )

    fever_results <- fever_results |>
      dplyr::left_join(sample_sizes, by = class_var)
  } else {
    fever_results$dhs_n_children <- nrow(kr_u5)
    fever_results$dhs_n_fever <- sum(kr_u5$had_fever == 1, na.rm = TRUE)
  }

  # ---- 7. Format results ----

  # Round proportions
  fever_cols <- names(fever_results)[grepl("^dhs_fever", names(fever_results))]
  fever_results <- fever_results |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(fever_cols[!grepl("^dhs_n_", fever_cols)]),
        ~ round(.x, 2)
      )
    )

  # Clamp CIs to [0, 1]
  fever_results <- fever_results |>
    dplyr::mutate(
      dplyr::across(dplyr::matches("_low$"), ~ pmax(0, .)),
      dplyr::across(dplyr::matches("_upp$"), ~ pmin(1, .))
    )

  # Ensure count columns are integers
  count_cols <- intersect(
    c("dhs_n_children", "dhs_n_fever"),
    names(fever_results)
  )
  fever_results <- fever_results |>
    dplyr::mutate(
      dplyr::across(dplyr::all_of(count_cols), ~ as.integer(round(.x)))
    )

  # Split admin_class back if needed
  if (!is.null(class_var) && class_var == "admin_class" &&
      !is.null(admin_level) && length(admin_level) > 1) {
    admin_splits <- stringr::str_split(
      fever_results$admin_class, "_", simplify = TRUE
    )
    for (i in seq_along(admin_level)) {
      fever_results[[admin_level[i]]] <- admin_splits[, i]
    }
  }

  # Remove temporary columns
  fever_results <- fever_results |>
    dplyr::select(-dplyr::any_of(c("admin_class", "level")))

  tibble::as_tibble(fever_results)
}


#' Calculate Fever Prevalence from DHS Data (Wrapper)
#'
#' Convenience wrapper around calc_fever_dhs_core() that also extracts
#' survey metadata and builds a data dictionary.
#'
#' @inheritParams calc_fever_dhs_core
#'
#' @return List with:
#'   \itemize{
#'     \item `data`: Tibble with fever estimates by admin level
#'     \item `dict`: Data dictionary from sntutils::build_dictionary()
#'     \item `metadata`: List with survey metadata
#'   }
#'
#' @examples
#' \dontrun{
#' fever <- calc_fever_dhs(
#'   dhs_kr = kr_data,
#'   region_var = "v024"
#' )
#' fever$data
#' fever$dict
#' }
#'
#' @export
calc_fever_dhs <- function(
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
  # Extract metadata
  metadata <- .extract_dhs_metadata_fever(
    dhs_kr = dhs_kr,
    survey_vars = survey_vars
  )

  # Calculate fever using core function
  fever_data <- calc_fever_dhs_core(
    dhs_kr = dhs_kr,
    survey_vars = survey_vars,
    region_var = region_var,
    gps_data = gps_data,
    gps_vars = gps_vars,
    shapefile = shapefile,
    admin_level = admin_level,
    join_nearest = join_nearest
  )

  list(
    data = fever_data,
    dict = sntutils::build_dictionary(fever_data),
    metadata = metadata
  )
}


#' Extract metadata from DHS KR dataset for fever analysis
#'
#' @param dhs_kr DHS children's recode dataset.
#' @param survey_vars Named list of survey variable mappings.
#'
#' @return List containing survey metadata.
#' @noRd
.extract_dhs_metadata_fever <- function(dhs_kr, survey_vars = NULL) {
  metadata <- list()

  if ("v000" %in% names(dhs_kr)) {
    metadata$country_code <- unique(dhs_kr$v000)[1]
  } else {
    metadata$country_code <- NA_character_
  }

  if ("v007" %in% names(dhs_kr)) {
    metadata$survey_year <- unique(dhs_kr$v007)[1]
  } else {
    metadata$survey_year <- NA_integer_
  }

  metadata$survey_type <- "DHS"
  metadata$file_type <- "KR"
  metadata$total_records <- nrow(dhs_kr)
  metadata$analysis_type <- "Fever Prevalence"
  metadata$methodology <- "WHO World Malaria Report (WMR)"
  metadata$age_group <- "0-59 months"
  metadata$cascade_step <- 0L
  metadata$processed_date <- Sys.Date()

  metadata
}
