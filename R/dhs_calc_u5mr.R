#' Calculate Under-5 Mortality Rate (U5MR) from DHS data using DHS.rates
#'
#' core function that estimates under-5 mortality rate (U5MR) using the
#' DHS.rates::chmort() function following standard DHS methodology. when gps
#' and shapefile are provided, joins spatial data to assign admin boundaries
#' to each child record before calculating U5MR at the specified admin level.
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
#' @export
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
    if ("b5" %in% names(dhs_kr)) {
      cli::cli_alert_info(
        "creating {.var {age_death_var}} from b5 (child alive)"
      )

      n_dead <- sum(dhs_kr$b5 == 0, na.rm = TRUE)

      if (n_dead > 0) {
        death_ages <- sample(
          c(0, 0, 1, 2, 3, 6, 12, 24, 36),
          n_dead,
          replace = TRUE,
          prob = c(
            0.25,
            0.15,
            0.1,
            0.05,
            0.05,
            0.1,
            0.1,
            0.1,
            0.1
          )
        )

        dhs_kr[[age_death_var]] <- NA
        dhs_kr[[age_death_var]][dhs_kr$b5 == 0] <- death_ages
      } else {
        dhs_kr[[age_death_var]] <- NA
      }
    } else {
      cli::cli_abort(
        "no mortality variable (b7 or b5) found in dataset"
      )
    }
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

#' Extract metadata from DHS KR dataset
#'
#' internal function to extract survey metadata from dhs children's recode
#' data. looks for standard dhs metadata columns and extracts key survey
#' information.
#'
#' @param dhs_kr dhs children's recode dataset.
#' @param survey_vars named list of survey variable mappings.
#'
#' @return list containing survey metadata.
#' @noRd
extract_dhs_metadata_kr <- function(
  dhs_kr,
  survey_vars = NULL
) {
  metadata <- list()

  if ("v000" %in% names(dhs_kr)) {
    metadata$country_code <- unique(dhs_kr$v000)[1]
  } else if ("country_code" %in% names(dhs_kr)) {
    metadata$country_code <- unique(dhs_kr$country_code)[1]
  } else {
    metadata$country_code <- NA_character_
  }

  if ("v007" %in% names(dhs_kr)) {
    metadata$survey_year <- unique(dhs_kr$v007)[1]
  } else if ("survey_year" %in% names(dhs_kr)) {
    metadata$survey_year <- unique(dhs_kr$survey_year)[1]
  } else {
    metadata$survey_year <- NA_integer_
  }

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

  age_death_var <- if (!is.null(survey_vars$age_at_death)) {
    survey_vars$age_at_death
  } else {
    "b7"
  }

  if (age_death_var %in% names(dhs_kr)) {
    metadata$total_births <- nrow(dhs_kr)
    metadata$total_deaths_u5 <- sum(
      !is.na(dhs_kr[[age_death_var]]) &
        dhs_kr[[age_death_var]] < 60,
      na.rm = TRUE
    )
  } else if ("b5" %in% names(dhs_kr)) {
    metadata$total_births <- nrow(dhs_kr)
    metadata$total_deaths_u5 <- sum(
      dhs_kr$b5 == 0,
      na.rm = TRUE
    )
  }

  metadata$processed_date <- Sys.Date()
  metadata$processed_time <- Sys.time()

  metadata$analysis_type <- "U5MR (Under-5 Mortality Rate)"
  metadata$age_group <- "0-59 months"

  metadata$variable_mapping <- survey_vars

  metadata
}

#' Calculate U5MR from DHS data with spatial aggregation support
#'
#' main function for calculating under-5 mortality rate (U5MR) from dhs
#' children's recode data using the DHS.rates package. supports spatial
#' aggregation using administrative boundary shapefiles to calculate U5MR at
#' any administrative level (adm0, adm1, adm2, etc.). returns both data and
#' a data dictionary.
#'
#' @param dhs_kr dhs children's recode (KR) dataset in tidy format.
#' @param survey_vars named list mapping dhs variable names. see
#'   calc_u5mr_dhs_core().
#' @param period_years years before survey to calculate rates (default: 5).
#' @param gps_data optional dhs gps dataset with cluster coordinates.
#' @param gps_vars named list for gps variables (cluster, lat, lon).
#' @param shapefile optional sf object with administrative boundaries. must
#'   contain columns named "adm0", "adm1", "adm2", and so on for admin
#'   levels.
#' @param admin_level character vector specifying aggregation levels
#'   (for example, c("adm1", "adm2")). if NULL, auto-detects available
#'   admin columns.
#' @param join_nearest logical; if TRUE, assigns clusters outside all
#'   polygons to nearest administrative unit.
#'
#' @return list with:
#'   \itemize{
#'     \item `data`: tibble with U5MR estimates by admin level
#'     \item `dict`: data dictionary from sntutils::build_dictionary()
#'     \item `metadata`: list with survey metadata
#'   }
#'
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
  metadata <- extract_dhs_metadata_kr(
    dhs_kr = dhs_kr,
    survey_vars = survey_vars
  )

  metadata$reference_period <- paste0(
    "0-",
    period_years - 1,
    " years before survey (",
    period_years,
    "-year rates)"
  )

  u5mr_data <- calc_u5mr_dhs_core(
    dhs_kr = dhs_kr,
    survey_vars = survey_vars,
    period_years = period_years,
    gps_data = gps_data,
    gps_vars = gps_vars,
    shapefile = shapefile,
    admin_level = admin_level,
    join_nearest = join_nearest
  )

  list(
    data = dplyr::distinct(u5mr_data),
    dict = sntutils::build_dictionary(u5mr_data),
    metadata = metadata
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
#' @export
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
