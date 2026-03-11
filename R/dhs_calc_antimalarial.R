#' Calculate Antimalarial Treatment from DHS Data
#'
#' Estimates the proportion of febrile children under 5 who received any
#' antimalarial drug using survey-weighted methods. This is step 3 of the
#' WMR case management cascade.
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
#' @export
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
  ml13_vars <- grep("^ml13[a-z]+$", names(dhs_kr), value = TRUE)
  h37_vars  <- grep("^h37[a-z]+$", names(dhs_kr), value = TRUE)
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
    survey_options <- options(survey.lonely.psu = "certainty")
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

  # ---- 6. Calculate sample sizes ----

  if (!is.null(class_var)) {
    sample_sizes <- kr_fever |>
      dplyr::group_by(.data[[class_var]]) |>
      dplyr::summarise(
        dhs_n_febrile = dplyr::n(),
        dhs_n_antimalarial = sum(has_antimalarial == 1, na.rm = TRUE),
        .groups = "drop"
      )

    am_results <- am_results |>
      dplyr::left_join(sample_sizes, by = class_var)
  } else {
    am_results$dhs_n_febrile <- nrow(kr_fever)
    am_results$dhs_n_antimalarial <- sum(
      kr_fever$has_antimalarial == 1, na.rm = TRUE
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
    c("dhs_n_febrile", "dhs_n_antimalarial"),
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


#' Calculate Antimalarial Treatment from DHS Data (Wrapper)
#'
#' Convenience wrapper around calc_antimalarial_dhs_core() that also extracts
#' survey metadata and builds a data dictionary.
#'
#' @inheritParams calc_antimalarial_dhs_core
#'
#' @return List with:
#'   \itemize{
#'     \item `data`: Tibble with antimalarial estimates by admin level
#'     \item `dict`: Data dictionary from sntutils::build_dictionary()
#'     \item `metadata`: List with survey metadata
#'   }
#'
#' @examples
#' \dontrun{
#' am <- calc_antimalarial_dhs(
#'   dhs_kr = kr_data,
#'   region_var = "v024"
#' )
#' am$data
#' }
#'
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
  metadata <- .extract_dhs_metadata_antimalarial(
    dhs_kr = dhs_kr,
    survey_vars = survey_vars
  )

  am_data <- calc_antimalarial_dhs_core(
    dhs_kr = dhs_kr,
    survey_vars = survey_vars,
    region_var = region_var,
    gps_data = gps_data,
    gps_vars = gps_vars,
    shapefile = shapefile,
    admin_level = admin_level,
    join_nearest = join_nearest
  )

  labels <- tibble::tribble(
    ~variable, ~label_en, ~label_fr, ~dhs_variable, ~numerator, ~denominator, ~dhs_numerator_var, ~dhs_denominator_var, ~notes,
    "dhs_antimalarial", "Any antimalarial treatment", "Traitement antipaludique (tout type)", "ml13a-h (or h37a-h)", "Febrile children receiving antimalarial", "Febrile children under 5", "ml13a-h (or h37a-h)", "h22", "Step 3 of WMR cascade; ml13* used when present, h37a-h fallback for older surveys (e.g. BFA 2021)",
    "dhs_antimalarial_low", "Antimalarial - lower 95% CI", "Antipaludique - IC 95% inferieur", "ml13a-h (or h37a-h)", NA_character_, NA_character_, NA_character_, NA_character_, "Survey-weighted 95% CI, clamped to [0,1]",
    "dhs_antimalarial_upp", "Antimalarial - upper 95% CI", "Antipaludique - IC 95% superieur", "ml13a-h (or h37a-h)", NA_character_, NA_character_, NA_character_, NA_character_, "Survey-weighted 95% CI, clamped to [0,1]",
    "dhs_n_febrile", "Number of febrile children (denominator)", "Nombre d'enfants febriles (denominateur)", "h22", NA_character_, NA_character_, NA_character_, NA_character_, "Unweighted count",
    "dhs_n_antimalarial", "Number receiving antimalarial (numerator)", "Nombre recevant un antipaludique (numerateur)", "ml13a-h (or h37a-h)", NA_character_, NA_character_, NA_character_, NA_character_, "Unweighted count"
  )

  dict <- sntutils::build_dictionary(am_data)
  dict <- .enrich_dhs_dictionary(dict, labels)

  list(
    data = am_data,
    dict = dict,
    metadata = metadata
  )
}


#' Extract metadata from DHS KR dataset for antimalarial analysis
#'
#' @param dhs_kr DHS children's recode dataset.
#' @param survey_vars Named list of survey variable mappings.
#'
#' @return List containing survey metadata.
#' @noRd
.extract_dhs_metadata_antimalarial <- function(dhs_kr, survey_vars = NULL) {
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
  metadata$analysis_type <- "Antimalarial Treatment"
  metadata$methodology <- "WHO World Malaria Report (WMR)"
  metadata$age_group <- "0-59 months"
  metadata$condition <- "Fever in last 2 weeks"
  metadata$cascade_step <- 3L
  metadata$processed_date <- Sys.Date()

  # Detect antimalarial variables (ml13 preferred, h37 fallback)
  ml13_vars <- grep("^ml13[a-z]+$", names(dhs_kr), value = TRUE)
  h37_vars  <- grep("^h37[a-z]+$", names(dhs_kr), value = TRUE)
  if (length(ml13_vars) > 0) {
    metadata$antimalarial_vars_detected <- ml13_vars
    metadata$antimalarial_series <- "ml13"
  } else {
    metadata$antimalarial_vars_detected <- h37_vars
    metadata$antimalarial_series <- if (length(h37_vars) > 0) "h37" else "none"
  }
  metadata$n_antimalarial_vars <- length(metadata$antimalarial_vars_detected)

  metadata
}
