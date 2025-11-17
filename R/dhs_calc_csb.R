#' Calculate Care-Seeking Behavior (CSB) from DHS data
#'
#' Core function that estimates care-seeking behavior among children aged 0-59
#' months who had fever in the last two weeks using standard DHS methodology.
#' When GPS and shapefile are provided, joins spatial data to assign admin
#' boundaries to each child record before calculating CSB at the specified
#' admin level.
#'
#' @param dhs_kr DHS children's recode (KR) dataset in tidy format
#'   (data.frame or tibble).
#' @param survey_vars Named list mapping DHS variable names. Required keys:
#'   \itemize{
#'     \item `cluster`: Cluster ID (default: "v021")
#'     \item `weight`: Survey weight (default: "v005")
#'     \item `stratum`: Stratum variable (default: "v022")
#'     \item `age`: Child's age in months (default: "hw1")
#'     \item `fever`: Had fever in last 2 weeks (default: "h22")
#'     \item `sought_care`: Whether care was sought (default: "h32")
#'     \item `public_sector`: Sought care from public sector (default: "h32a")
#'     \item `private_sector`: Sought care from private sector (default: "h32b")
#'   }
#' @param gps_data Optional DHS GPS dataset with cluster coordinates.
#' @param gps_vars Named list for GPS variables (cluster, lat, lon).
#' @param shapefile Optional sf object with administrative boundaries.
#' @param admin_level Character vector of admin columns from shapefile
#'   (e.g., c("adm1", "adm2")). If NULL, uses existing admin variables
#'   in data.
#' @param join_nearest Logical; if TRUE, assigns clusters outside polygons
#'   to nearest admin unit.
#'
#' @return Tibble with CSB estimates by administrative level, with
#'   confidence intervals and sample sizes.
#'
#' @export
calc_csb_dhs_core <- function(
  dhs_kr,
  survey_vars = list(
    cluster = "v021",
    weight = "v005",
    stratum = "v022",
    age = "hw1",
    fever = "h22",
    sought_care = "h32",
    public_sector = "h32a",
    private_sector = "h32b"
  ),
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
  # ---- 1. Input validation ---------------------------------------------------

  if (!is.data.frame(dhs_kr)) {
    cli::cli_abort("`dhs_kr` must be a data.frame or tibble.")
  }

  if (nrow(dhs_kr) == 0) {
    cli::cli_abort("`dhs_kr` is empty.")
  }

  # Check required survey variables
  needed <- unlist(
    survey_vars[
      c(
        "cluster",
        "weight",
        "stratum",
        "age",
        "fever"
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

  # ---- 2. Prepare base dataset ----------------------------------------------

  kr <- dhs_kr |>
    dplyr::mutate(
      cluster_id = .data[[survey_vars$cluster]],
      survey_weight = .data[[survey_vars$weight]] / 1e6,
      stratum_id = .data[[survey_vars$stratum]],
      age_months = .data[[survey_vars$age]],
      had_fever = .data[[survey_vars$fever]]
    )

  # Filter to children under 5 with valid fever data
  kr_eligible <- kr |>
    dplyr::filter(
      age_months >= 0,
      age_months <= 59,
      had_fever %in% c(0, 1)
    )

  if (nrow(kr_eligible) == 0) {
    cli::cli_abort(
      "No eligible children (0-59 months) with valid fever data found."
    )
  }

  # Filter to children who had fever
  kr_fever <- kr_eligible |>
    dplyr::filter(had_fever == 1)

  if (nrow(kr_fever) == 0) {
    cli::cli_abort(
      "No children with fever in the last 2 weeks found in the dataset."
    )
  }

  cli::cli_alert_info(
    paste0(
      "Found {nrow(kr_fever)} children with fever out of {nrow(kr_eligible)} ",
      "eligible children"
    )
  )

  # ---- 3. Create care-seeking variables -------------------------------------

  # Check which care-seeking variables are available
  has_sought_care <- survey_vars$sought_care %in% names(kr_fever)
  has_public <- !is.null(survey_vars$public_sector) &&
                 survey_vars$public_sector %in% names(kr_fever)
  has_private <- !is.null(survey_vars$private_sector) &&
                  survey_vars$private_sector %in% names(kr_fever)

  if (!has_sought_care && !has_public && !has_private) {
    cli::cli_abort(
      c(
        "No care-seeking variables found in the dataset.",
        "i" = "Check that h32, h32a, or h32b variables exist"
      )
    )
  }

  # Create care-seeking indicators
  kr_fever <- kr_fever |>
    dplyr::mutate(
      sought_any = if (has_sought_care) {
        dplyr::if_else(
          .data[[survey_vars$sought_care]] == 1, 1, 0, missing = NA_real_)
      } else {
        NA_real_
      },
      sought_public = if (has_public) {
        dplyr::if_else(
          .data[[survey_vars$public_sector]] == 1, 1, 0, missing = NA_real_)
      } else {
        NA_real_
      },
      sought_private = if (has_private) {
        dplyr::if_else(
          .data[[survey_vars$private_sector]] == 1, 1, 0, missing = NA_real_)
      } else {
        NA_real_
      }
    )

  # Create composite indicators
  kr_fever <- kr_fever |>
    dplyr::mutate(
      sought_none = dplyr::case_when(
        !is.na(sought_any) ~ 1 - sought_any,
        !is.na(sought_public) & !is.na(sought_private) ~
          dplyr::if_else(sought_public == 0 & sought_private == 0, 1, 0),
        TRUE ~ NA_real_
      )
    )

  # If sought_any is missing, derive from public/private
  if (!has_sought_care && (has_public || has_private)) {
    kr_fever <- kr_fever |>
      dplyr::mutate(
        sought_any = dplyr::case_when(
          sought_public == 1 | sought_private == 1 ~ 1,
          sought_public == 0 & sought_private == 0 ~ 0,
          TRUE ~ NA_real_
        )
      )
  }

  # ---- 4. Join GPS and shapefile if provided --------------------------------

  class_var <- NULL

  if (!is.null(gps_data) && !is.null(shapefile)) {
    cli::cli_alert_info(
      "Joining GPS coordinates and administrative boundaries"
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

    kr_fever <- kr_fever |>
      dplyr::left_join(
        gps_clean,
        by = "cluster_id"
      )

    clusters_sf <- kr_fever |>
      dplyr::select(
        cluster_id,
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
          "No admin columns (adm0, adm1, adm2, etc.) found in shapefile"
        )
      }

      admin_level <- available_admins

      cli::cli_alert_info(
        "Using admin levels: {paste(admin_level, collapse = ', ')}"
      )
    }

    missing_cols <- setdiff(admin_level, names(shapefile))

    if (length(missing_cols) > 0) {
      cli::cli_abort(
        "Admin columns not found in shapefile: ",
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
          "Assigning {sum(unmatched)} clusters to nearest admin units"
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

    kr_fever <- kr_fever |>
      dplyr::left_join(
        cluster_admin_df,
        by = "cluster_id"
      )

    if (length(admin_level) > 1) {
      kr_fever$admin_class <- apply(
        kr_fever[, admin_level, drop = FALSE],
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
      "Shapefile provided without GPS data; using existing admin vars"
    )

    existing_admins <- c("v024", "v025", "sdist")
    found_admin <- existing_admins[
      existing_admins %in% names(kr_fever)
    ][1]

    if (!is.na(found_admin)) {
      class_var <- found_admin
      cli::cli_alert_info(
        "Using {.var {found_admin}} as grouping variable"
      )
    }
  } else if (!is.null(gps_data)) {
    cli::cli_alert_info(
      "GPS provided without shapefile; calculating cluster-level CSB"
    )
    class_var <- "cluster_id"
  } else {
    if ("v024" %in% names(kr_fever)) {
      class_var <- "v024"
      cli::cli_alert_info(
        "Using v024 (region) as grouping variable"
      )
    }
  }

  # ---- 5. Set up survey design ----------------------------------------------

  if (!is.null(class_var)) {
    cli::cli_alert_info(
      "Calculating CSB by {.var {class_var}}"
    )
  } else {
    cli::cli_alert_info(
      "Calculating national-level CSB"
    )
  }

  # Check for single-PSU strata
  strata_check <- kr_fever |>
    dplyr::group_by(stratum_id) |>
    dplyr::summarise(
      n_clusters = dplyr::n_distinct(cluster_id),
      .groups = "drop"
    )

  single_psu_strata <- sum(strata_check$n_clusters == 1)

  if (single_psu_strata > 0) {
    cli::cli_alert_info(
      "Found {single_psu_strata} strata with single PSU; using certainty option"
    )
  }

  use_strata <- dplyr::n_distinct(kr_fever$stratum_id) > 1

  if (use_strata) {
    # Set survey options to handle single-PSU strata
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

  # ---- 6. Calculate care-seeking indicators ---------------------------------

  # Determine grouping
  if (!is.null(class_var)) {
    group_formula <- stats::as.formula(paste("~", class_var))
  } else {
    group_formula <- ~1
  }

  # Calculate all available indicators
  indicators <- c()
  if (!all(
    is.na(kr_fever$sought_any))) indicators <- c(indicators, "sought_any")
  if (!all(
    is.na(kr_fever$sought_public))) indicators <- c(indicators, "sought_public")
  if (!all(
    is.na(
      kr_fever$sought_private))) indicators <- c(indicators, "sought_private")
  if (!all(
    is.na(kr_fever$sought_none))) indicators <- c(indicators, "sought_none")

  if (length(indicators) == 0) {
    cli::cli_abort("No valid care-seeking indicators to calculate")
  }

  # Build formula for svyby
  indicator_formula <- stats::as.formula(
    paste("~", paste(indicators, collapse = " + "))
  )

  # Calculate proportions
  if (!is.null(class_var)) {
    # Additional check for single-cluster groups when grouping by admin level
    if (class_var %in% c("admin_class", admin_level)) {
      group_check <- kr_fever |>
        dplyr::group_by(.data[[class_var]]) |>
        dplyr::summarise(
          n_clusters = dplyr::n_distinct(cluster_id),
          .groups = "drop"
        )

      single_cluster_groups <- sum(group_check$n_clusters == 1)

      if (single_cluster_groups > 0) {
        cli::cli_alert_warning(
          "{single_cluster_groups} admin unit(s) have only one cluster; ",
          "variance estimates may be unreliable"
        )
      }
    }

    csb_results <- tryCatch({
      survey::svyby(
        indicator_formula,
        by = group_formula,
        design = des,
        FUN = survey::svymean,
        vartype = "ci",
        keep.names = FALSE
      ) |>
        tibble::as_tibble()
    }, error = function(e) {
      if (grepl("has only one PSU", e$message)) {
        cli::cli_alert_warning(
          "Single PSU issue persists; trying with adjusted survey design"
        )
        # Fall back to ignoring strata
        des_no_strata <- survey::svydesign(
          ids = ~cluster_id,
          weights = ~survey_weight,
          data = kr_fever,
          nest = TRUE
        )
        survey::svyby(
          indicator_formula,
          by = group_formula,
          design = des_no_strata,
          FUN = survey::svymean,
          vartype = "ci",
          keep.names = FALSE
        ) |>
          tibble::as_tibble()
      } else {
        stop(e)
      }
    })
  } else {
    csb_means <- survey::svymean(
      indicator_formula,
      design = des
    )
    csb_ci <- stats::confint(csb_means)

    csb_results <- tibble::tibble(
      level = "National"
    )

    for (ind in indicators) {
      csb_results[[ind]] <- as.numeric(csb_means[ind])
      csb_results[[paste0("ci_l.", ind)]] <- csb_ci[ind, 1]
      csb_results[[paste0("ci_u.", ind)]] <- csb_ci[ind, 2]
    }
  }

  # ---- 7. Calculate sample sizes --------------------------------------------

  if (!is.null(class_var)) {
    sample_sizes <- kr_fever |>
      dplyr::group_by(.data[[class_var]]) |>
      dplyr::summarise(
        dhs_n_fever = dplyr::n(),
        dhs_n_sought_any = sum(sought_any == 1, na.rm = TRUE),
        dhs_n_sought_public = sum(sought_public == 1, na.rm = TRUE),
        dhs_n_sought_private = sum(sought_private == 1, na.rm = TRUE),
        dhs_n_sought_none = sum(sought_none == 1, na.rm = TRUE),
        .groups = "drop"
      )

    csb_results <- csb_results |>
      dplyr::left_join(
        sample_sizes,
        by = class_var
      )
  } else {
    csb_results$dhs_n_fever <- nrow(kr_fever)
    csb_results$dhs_n_sought_any <- sum(
      kr_fever$sought_any == 1, na.rm = TRUE)
    csb_results$dhs_n_sought_public <- sum(
      kr_fever$sought_public == 1, na.rm = TRUE)
    csb_results$dhs_n_sought_private <- sum(
      kr_fever$sought_private == 1, na.rm = TRUE)
    csb_results$dhs_n_sought_none <- sum(
      kr_fever$sought_none == 1, na.rm = TRUE)
  }

  # ---- 8. Format results -----------------------------------------------------

  # Rename columns to standard format
  rename_list <- list()
  if ("sought_any" %in% names(csb_results)) {
    rename_list$dhs_csb_any <- "sought_any"
    rename_list$dhs_csb_any_low <- "ci_l.sought_any"
    rename_list$dhs_csb_any_upp <- "ci_u.sought_any"
  }
  if ("sought_public" %in% names(csb_results)) {
    rename_list$dhs_csb_public <- "sought_public"
    rename_list$dhs_csb_public_low <- "ci_l.sought_public"
    rename_list$dhs_csb_public_upp <- "ci_u.sought_public"
  }
  if ("sought_private" %in% names(csb_results)) {
    rename_list$dhs_csb_private <- "sought_private"
    rename_list$dhs_csb_private_low <- "ci_l.sought_private"
    rename_list$dhs_csb_private_upp <- "ci_u.sought_private"
  }
  if ("sought_none" %in% names(csb_results)) {
    rename_list$dhs_csb_none <- "sought_none"
    rename_list$dhs_csb_none_low <- "ci_l.sought_none"
    rename_list$dhs_csb_none_upp <- "ci_u.sought_none"
  }

  for (new_name in names(rename_list)) {
    old_name <- rename_list[[new_name]]
    if (old_name %in% names(csb_results)) {
      csb_results <- csb_results |>
        dplyr::rename(!!new_name := !!old_name)
    }
  }

  # Convert to percentages
  csb_cols <- names(csb_results)[grepl("^dhs_csb_", names(csb_results))]

  csb_results <- csb_results |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(csb_cols),
        ~ round(.x * 100, 1)
      )
    )

  # Ensure confidence intervals stay within [0, 100]
  csb_results <- csb_results |>
    dplyr::mutate(
      dplyr::across(
        dplyr::matches("_low$"),
        ~ pmax(0, .)
      ),
      dplyr::across(
        dplyr::matches("_upp$"),
        ~ pmin(100, .)
      )
    )

  # Split admin_class back into individual admin columns if needed
  if (class_var == "admin_class" && length(admin_level) > 1) {
    admin_splits <- stringr::str_split(
      csb_results$admin_class,
      "_",
      simplify = TRUE
    )

    for (i in seq_along(admin_level)) {
      csb_results[[admin_level[i]]] <- admin_splits[, i]
    }
  }

  # Add admin name columns if available
  if (!is.null(shapefile) && !is.null(admin_level)) {
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

      csb_results <- csb_results |>
        dplyr::left_join(
          admin_lookup,
          by = intersect(names(csb_results), admin_level)
        )
    }
  } else {
    admin_name_cols <- character(0)
  }

  # Reorder columns - simplified to essential columns only
  col_order <- c(
    admin_level,
    admin_name_cols,
    "dhs_n_fever",
    "dhs_csb_any",
    "dhs_csb_any_low",
    "dhs_csb_any_upp",
    "dhs_csb_public",
    "dhs_csb_public_low",
    "dhs_csb_public_upp",
    "dhs_csb_private",
    "dhs_csb_private_low",
    "dhs_csb_private_upp",
    "dhs_csb_none",
    "dhs_csb_none_low",
    "dhs_csb_none_upp"
  )

  col_order <- intersect(col_order, names(csb_results))

  # Exclude admin_class and sample size details from final output
  # (keep only essential columns)
  exclude_cols <- c("admin_class", "dhs_n_sought_any", "dhs_n_sought_public",
                    "dhs_n_sought_private", "dhs_n_sought_none")
  other_cols <- setdiff(names(csb_results), c(col_order, exclude_cols))

  csb_results <- csb_results |>
    dplyr::select(
      dplyr::all_of(c(col_order, other_cols))
    )

  tibble::as_tibble(csb_results)
}

#' Extract metadata from DHS KR dataset for care-seeking analysis
#'
#' Internal function to extract survey metadata from DHS children's recode
#' data. Looks for standard DHS metadata columns and extracts key survey
#' information relevant to care-seeking behavior analysis.
#'
#' @param dhs_kr DHS children's recode dataset.
#' @param survey_vars Named list of survey variable mappings.
#'
#' @return List containing survey metadata.
#' @noRd
extract_dhs_metadata_csb <- function(
  dhs_kr,
  survey_vars = NULL
) {
  metadata <- list()

  # Extract country code
  if ("v000" %in% names(dhs_kr)) {
    metadata$country_code <- unique(dhs_kr$v000)[1]
  } else if ("country_code" %in% names(dhs_kr)) {
    metadata$country_code <- unique(dhs_kr$country_code)[1]
  } else {
    metadata$country_code <- NA_character_
  }

  # Extract survey year
  if ("v007" %in% names(dhs_kr)) {
    metadata$survey_year <- unique(dhs_kr$v007)[1]
  } else if ("survey_year" %in% names(dhs_kr)) {
    metadata$survey_year <- unique(dhs_kr$survey_year)[1]
  } else {
    metadata$survey_year <- NA_integer_
  }

  # Extract survey ID
  if ("survey_id" %in% names(dhs_kr)) {
    metadata$survey_id <- unique(dhs_kr$survey_id)[1]
  } else if ("v000" %in% names(dhs_kr)) {
    metadata$survey_id <- unique(dhs_kr$v000)[1]
  } else {
    metadata$survey_id <- NA_character_
  }

  metadata$survey_type <- "DHS"
  metadata$file_type <- "KR"

  metadata$total_records <- nrow(dhs_kr)

  # Count clusters
  cluster_var <- if (!is.null(survey_vars$cluster)) {
    survey_vars$cluster
  } else {
    "v021"
  }

  if (cluster_var %in% names(dhs_kr)) {
    metadata$total_clusters <- length(
      unique(dhs_kr[[cluster_var]])
    )
  }

  # Count children with fever
  fever_var <- if (!is.null(survey_vars$fever)) {
    survey_vars$fever
  } else {
    "h22"
  }

  if (fever_var %in% names(dhs_kr)) {
    age_var <- if (!is.null(survey_vars$age)) {
      survey_vars$age
    } else {
      "hw1"
    }

    if (age_var %in% names(dhs_kr)) {
      eligible <- dhs_kr[[age_var]] >= 0 & dhs_kr[[age_var]] <= 59
      metadata$total_eligible_children <- sum(eligible, na.rm = TRUE)
      metadata$total_fever_cases <- sum(
        eligible & dhs_kr[[fever_var]] == 1,
        na.rm = TRUE
      )
    } else {
      metadata$total_fever_cases <- sum(
        dhs_kr[[fever_var]] == 1,
        na.rm = TRUE
      )
    }
  }

  metadata$processed_date <- Sys.Date()
  metadata$processed_time <- Sys.time()

  metadata$analysis_type <- "CSB (Care-Seeking Behavior)"
  metadata$age_group <- "0-59 months"
  metadata$condition <- "Fever in last 2 weeks"

  # Check which care-seeking variables are available
  metadata$has_sought_care <- !is.null(survey_vars$sought_care) &&
                               survey_vars$sought_care %in% names(dhs_kr)
  metadata$has_public_sector <- !is.null(survey_vars$public_sector) &&
                                 survey_vars$public_sector %in% names(dhs_kr)
  metadata$has_private_sector <- !is.null(survey_vars$private_sector) &&
                                  survey_vars$private_sector %in% names(dhs_kr)

  metadata$variable_mapping <- survey_vars

  metadata
}

#' Calculate Care-Seeking Behavior from DHS data with spatial aggregation support
#'
#' Main function for calculating care-seeking behavior (CSB) from DHS
#' children's recode data. Supports spatial aggregation using administrative
#' boundary shapefiles to calculate CSB at any administrative level (adm0,
#' adm1, adm2, etc.). Returns both data and a data dictionary.
#'
#' @param dhs_kr DHS children's recode (KR) dataset in tidy format.
#' @param survey_vars Named list mapping DHS variable names. See
#'   calc_csb_dhs_core().
#' @param gps_data Optional DHS GPS dataset with cluster coordinates.
#' @param gps_vars Named list for GPS variables (cluster, lat, lon).
#' @param shapefile Optional sf object with administrative boundaries. Must
#'   contain columns named "adm0", "adm1", "adm2", etc. for admin levels.
#' @param admin_level Character vector specifying aggregation levels
#'   (e.g., c("adm1", "adm2")). If NULL, auto-detects available admin
#'   columns.
#' @param join_nearest Logical; if TRUE, assigns clusters outside all
#'   polygons to nearest administrative unit.
#'
#' @return List with:
#'   \itemize{
#'     \item `data`: Tibble with CSB estimates by admin level
#'     \item `dict`: Data dictionary from sntutils::build_dictionary()
#'     \item `metadata`: List with survey metadata
#'   }
#'
#' @examples
#' # Example with spatial aggregation
#' # csb_results <- calc_csb_dhs(
#' #   dhs_kr = kr_data,
#' #   gps_data = gps_data,
#' #   shapefile = admin_shapefile,
#' #   admin_level = c("adm1")
#' # )
#' #
#' # # Access the data
#' # csb_data <- csb_results$data
#' #
#' # # Access the dictionary
#' # csb_dict <- csb_results$dict
#' #
#' # # Access the metadata
#' # csb_metadata <- csb_results$metadata
#'
#' @export
calc_csb_dhs <- function(
  dhs_kr,
  survey_vars = list(
    cluster = "v021",
    weight = "v005",
    stratum = "v022",
    age = "hw1",
    fever = "h22",
    sought_care = "h32",
    public_sector = "h32a",
    private_sector = "h32b"
  ),
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
  metadata <- extract_dhs_metadata_csb(
    dhs_kr = dhs_kr,
    survey_vars = survey_vars
  )

  # Calculate CSB using core function
  csb_data <- calc_csb_dhs_core(
    dhs_kr = dhs_kr,
    survey_vars = survey_vars,
    gps_data = gps_data,
    gps_vars = gps_vars,
    shapefile = shapefile,
    admin_level = admin_level,
    join_nearest = join_nearest
  )

  # Return list with data, dictionary, and metadata
  list(
    data = csb_data,
    dict = sntutils::build_dictionary(csb_data),
    metadata = metadata
  )
}

#' Aggregate CSB to administrative levels
#'
#' Helper to aggregate CSB results to administrative levels using a shapefile.
#' Performs spatial joins and calculates weighted averages by administrative
#' unit.
#'
#' @param csb_results CSB results with coordinates.
#' @param shapefile sf object with administrative boundaries.
#' @param admin_level Character vector of admin levels to aggregate to.
#' @param weighted Logical. If TRUE (default), uses sample size as weights.
#'
#' @return sf object with aggregated CSB by administrative level.
#'
#' @export
aggregate_csb_admin <- function(
  csb_results,
  shapefile,
  admin_level = c("adm1"),
  weighted = TRUE
) {
  if (!requireNamespace("sf", quietly = TRUE)) {
    cli::cli_abort("Package 'sf' is required for spatial operations")
  }

  # Convert to sf if needed
  if (!inherits(csb_results, "sf")) {
    if (!all(c("lat", "lon") %in% names(csb_results))) {
      cli::cli_abort(
        "csb_results must have lat and lon columns for spatial join"
      )
    }

    csb_sf <- csb_results |>
      sf::st_as_sf(
        coords = c("lon", "lat"),
        crs = 4326,
        remove = FALSE
      )
  } else {
    csb_sf <- csb_results
  }

  # Prepare shapefile
  shapefile <- shapefile |>
    sf::st_transform(4326) |>
    sf::st_make_valid()

  # Spatial join
  joined <- sf::st_join(
    csb_sf,
    shapefile[, c(admin_level, "geometry")],
    join = sf::st_within,
    left = TRUE
  )

  # Handle unmatched clusters
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

  # Convert to data frame for aggregation
  joined_df <- sf::st_drop_geometry(joined)

  # Aggregate
  if (weighted && "dhs_n_fever" %in% names(joined_df)) {
    # Weighted aggregation
    aggregated <- joined_df |>
      dplyr::group_by(
        dplyr::across(
          dplyr::all_of(admin_level)
        )
      ) |>
      dplyr::summarise(
        dhs_csb_any = if ("dhs_csb_any" %in% names(joined_df)) {
          stats::weighted.mean(
            dhs_csb_any,
            w = dhs_n_fever,
            na.rm = TRUE
          )
        } else NA_real_,
        dhs_csb_public = if ("dhs_csb_public" %in% names(joined_df)) {
          stats::weighted.mean(
            dhs_csb_public,
            w = dhs_n_fever,
            na.rm = TRUE
          )
        } else NA_real_,
        dhs_csb_private = if ("dhs_csb_private" %in% names(joined_df)) {
          stats::weighted.mean(
            dhs_csb_private,
            w = dhs_n_fever,
            na.rm = TRUE
          )
        } else NA_real_,
        dhs_csb_none = if ("dhs_csb_none" %in% names(joined_df)) {
          stats::weighted.mean(
            dhs_csb_none,
            w = dhs_n_fever,
            na.rm = TRUE
          )
        } else NA_real_,
        dhs_n_fever = sum(
          dhs_n_fever,
          na.rm = TRUE
        ),
        dhs_n_sought_any = if ("dhs_n_sought_any" %in% names(joined_df)) {
          sum(dhs_n_sought_any, na.rm = TRUE)
        } else NA_integer_,
        dhs_n_sought_public = if (
          "dhs_n_sought_public" %in% names(joined_df)) {
          sum(dhs_n_sought_public, na.rm = TRUE)
        } else NA_integer_,
        dhs_n_sought_private = if (
          "dhs_n_sought_private" %in% names(joined_df)) {
          sum(dhs_n_sought_private, na.rm = TRUE)
        } else NA_integer_,
        dhs_n_sought_none = if ("dhs_n_sought_none" %in% names(joined_df)) {
          sum(dhs_n_sought_none, na.rm = TRUE)
        } else NA_integer_,
        .groups = "drop"
      )
  } else {
    # Simple average
    aggregated <- joined_df |>
      dplyr::group_by(
        dplyr::across(
          dplyr::all_of(admin_level)
        )
      ) |>
      dplyr::summarise(
        dhs_csb_any = if ("dhs_csb_any" %in% names(joined_df)) {
          mean(dhs_csb_any, na.rm = TRUE)
        } else NA_real_,
        dhs_csb_public = if ("dhs_csb_public" %in% names(joined_df)) {
          mean(dhs_csb_public, na.rm = TRUE)
        } else NA_real_,
        dhs_csb_private = if ("dhs_csb_private" %in% names(joined_df)) {
          mean(dhs_csb_private, na.rm = TRUE)
        } else NA_real_,
        dhs_csb_none = if ("dhs_csb_none" %in% names(joined_df)) {
          mean(dhs_csb_none, na.rm = TRUE)
        } else NA_real_,
        dhs_n_fever = sum(
          dhs_n_fever,
          na.rm = TRUE
        ),
        dhs_n_sought_any = if ("dhs_n_sought_any" %in% names(joined_df)) {
          sum(dhs_n_sought_any, na.rm = TRUE)
        } else NA_integer_,
        dhs_n_sought_public = if (
          "dhs_n_sought_public" %in% names(joined_df)) {
          sum(dhs_n_sought_public, na.rm = TRUE)
        } else NA_integer_,
        dhs_n_sought_private = if (
          "dhs_n_sought_private" %in% names(joined_df)) {
          sum(dhs_n_sought_private, na.rm = TRUE)
        } else NA_integer_,
        dhs_n_sought_none = if ("dhs_n_sought_none" %in% names(joined_df)) {
          sum(dhs_n_sought_none, na.rm = TRUE)
        } else NA_integer_,
        .groups = "drop"
      )
  }

  # Round percentages
  aggregated <- aggregated |>
    dplyr::mutate(
      dplyr::across(
        dplyr::starts_with("dhs_csb_"),
        ~ round(.x, 1)
      )
    )

  # Detect and preserve admin name columns
  admin_name_cols <- paste0(admin_level, "_name")
  admin_name_cols <- admin_name_cols[
    admin_name_cols %in% names(shapefile)
  ]
  all_admin_cols <- c(admin_level, admin_name_cols)

  # Join back with shapefile geometry
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
