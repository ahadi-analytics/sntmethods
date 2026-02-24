#' Calculate Gini Coefficient Using DHS Brown Formula Methodology
#'
#' Calculates the Gini coefficient for wealth inequality following the DHS
#' methodology. Uses the Brown formula with configurable number of wealth
#' groups (default 100) to provide a standardized measure of inequality.
#'
#' @param wealth_scores Numeric vector of wealth index factor scores (hv271).
#' @param weights Numeric vector of survey sampling weights.
#' @param population Numeric vector of household population sizes (de jure
#'   members).
#' @param n_groups Integer specifying the number of wealth groups for
#'   calculation (default: 100, following DHS methodology).
#'
#' @return Numeric Gini coefficient value between 0 (perfect equality) and 1
#'   (maximum inequality). Returns NA_real_ if insufficient data.
#'
#' @examples
#' # Example with synthetic data
#' wealth_scores <- rnorm(100, mean = 0, sd = 1)
#' weights <- rep(1, 100)
#' population <- sample(3:8, 100, replace = TRUE)
#'
#' gini <- calculate_dhs_gini(wealth_scores, weights, population)
#'
#' @export
calculate_dhs_gini <- function(
  wealth_scores,
  weights,
  population,
  n_groups = 100
) {
  # Remove missing values
  complete_cases <- !is.na(wealth_scores) &
    !is.na(weights) &
    !is.na(population)

  wealth_scores <- wealth_scores[complete_cases]
  weights <- weights[complete_cases]
  population <- population[complete_cases]

  if (length(wealth_scores) == 0) {
    return(NA_real_)
  }

  if (length(wealth_scores) < 10) {
    cli::cli_warn("fewer than 10 observations for gini calculation")
    return(NA_real_)
  }

  # DHS methodology: get min and max for grouping
  min_wealth <- min(wealth_scores)
  max_wealth <- max(wealth_scores)

  if (min_wealth == max_wealth) {
    # Perfect equality - all households have same wealth
    return(0)
  }

  # Create wealth groups
  wealth_group <- cut(
    wealth_scores,
    breaks = seq(min_wealth, max_wealth, length.out = n_groups + 1),
    include.lowest = TRUE,
    labels = FALSE
  )

  # Tally population and wealth for each group
  group_data <- data.frame(
    group = wealth_group,
    weight = weights,
    population = population,
    wealth_score = wealth_scores
  )

  group_summary <- group_data |>
    dplyr::group_by(group) |>
    dplyr::summarise(
      group_pop = sum(weight * population, na.rm = TRUE),
      group_wealth = sum(
        weight * (wealth_score - min_wealth) * population,
        na.rm = TRUE
      ),
      .groups = "drop"
    ) |>
    dplyr::arrange(group)

  # Calculate cumulative proportions
  group_summary <- group_summary |>
    dplyr::mutate(
      cum_pop = cumsum(group_pop),
      cum_wealth = cumsum(group_wealth)
    )

  total_pop <- sum(group_summary$group_pop)
  total_wealth <- sum(group_summary$group_wealth)

  if (total_pop == 0 || total_wealth == 0) {
    return(NA_real_)
  }

  group_summary <- group_summary |>
    dplyr::mutate(
      prop_pop = cum_pop / total_pop,
      prop_wealth = cum_wealth / total_wealth
    )

  # Apply Brown Formula for Gini coefficient
  prop_pop <- c(0, group_summary$prop_pop)
  prop_wealth <- c(0, group_summary$prop_wealth)

  n <- length(prop_pop)
  G <- 0
  for (k in 2:n) {
    G <- G + (prop_pop[k] - prop_pop[k - 1]) *
      (prop_wealth[k] + prop_wealth[k - 1])
  }

  1 - G
}

#' Extract Wealth-Specific Metadata from DHS Household Dataset
#'
#' Internal function to extract survey metadata specific to wealth analysis
#' from DHS Household Records data.
#'
#' @param dhs_hr DHS Household Records dataset
#' @param survey_vars Named list of survey variable mappings
#'
#' @return A list containing survey metadata
#' @noRd
extract_wealth_metadata <- function(dhs_hr, survey_vars = NULL) {
  metadata <- list()

  # Extract country code (hv000)
  if ("hv000" %in% names(dhs_hr)) {
    metadata$country_code <- unique(dhs_hr$hv000)[1]
  } else if ("country_code" %in% names(dhs_hr)) {
    metadata$country_code <- unique(dhs_hr$country_code)[1]
  } else {
    metadata$country_code <- NA_character_
  }

  # Extract survey year (hv007)
  if ("hv007" %in% names(dhs_hr)) {
    metadata$survey_year <- unique(dhs_hr$hv007)[1]
  } else if ("survey_year" %in% names(dhs_hr)) {
    metadata$survey_year <- unique(dhs_hr$survey_year)[1]
  } else {
    metadata$survey_year <- NA_integer_
  }

  # Extract survey ID
  if ("survey_id" %in% names(dhs_hr)) {
    metadata$survey_id <- unique(dhs_hr$survey_id)[1]
  } else if ("hv000" %in% names(dhs_hr)) {
    metadata$survey_id <- unique(dhs_hr$hv000)[1]
  } else {
    metadata$survey_id <- NA_character_
  }

  # Survey type
  metadata$survey_type <- "DHS"
  metadata$file_type <- "HR"

  # Extract total sample sizes
  metadata$total_households <- nrow(dhs_hr)

  cluster_var <- if (!is.null(survey_vars$cluster)) {
    survey_vars$cluster
  } else {
    "hv001"
  }

  if (cluster_var %in% names(dhs_hr)) {
    metadata$total_clusters <- length(unique(dhs_hr[[cluster_var]]))
  }

  # Add processing timestamp
  metadata$processed_date <- Sys.Date()
  metadata$processed_time <- Sys.time()

  # Add analysis type
  metadata$analysis_type <- "Wealth Quintile Distribution and Gini Coefficient"

  # Check which wealth variables are available
  metadata$has_wealth_quintile <- "hv270" %in% names(dhs_hr) ||
    (!is.null(survey_vars$wealth_quintile) &&
      survey_vars$wealth_quintile %in% names(dhs_hr))

  metadata$has_wealth_score <- "hv271" %in% names(dhs_hr) ||
    (!is.null(survey_vars$wealth_score) &&
      survey_vars$wealth_score %in% names(dhs_hr))

  # Add variable mapping info for transparency
  metadata$variable_mapping <- survey_vars

  metadata
}

#' Calculate Core Wealth Quintile Distributions from DHS Data
#'
#' Core function that calculates wealth quintile distributions and Gini
#' coefficients using DHS household data. When GPS data and shapefile are
#' provided, performs spatial joins to aggregate at administrative levels.
#'
#' @param dhs_hr DHS Household Records dataset in tidy format (data.frame or
#'   tibble).
#' @param survey_vars Named list mapping DHS variable names. Required keys:
#'   \itemize{
#'     \item `cluster`: Cluster ID (default: "hv001")
#'     \item `weight`: Survey weight (default: "hv005", divided by 1,000,000)
#'     \item `stratum`: Survey stratum (default: "hv022")
#'     \item `adm1`: First administrative level (default: "hv024")
#'     \item `adm2`: Second administrative level (default: NULL)
#'     \item `wealth_quintile`: Wealth quintile variable (default: "hv270")
#'     \item `wealth_score`: Wealth index factor score (default: "hv271")
#'     \item `hh_members`: De jure household members (default: "hv012")
#'   }
#' @param gps_data Optional DHS GPS dataset. If provided with shapefile,
#'   enables spatial aggregation.
#' @param gps_vars Named list for GPS variables (cluster, lat, lon).
#' @param shapefile Optional sf object with administrative boundaries.
#' @param admin_level Character vector of admin columns in shapefile.
#' @param join_nearest Logical; if TRUE, assigns unmatched clusters to nearest
#'   polygon. Default TRUE.
#'
#' @return A tibble with wealth quintile distributions and Gini coefficients
#'   by administrative unit or cluster.
#'
#' @export
calc_wealth_dhs_core <- function(
  dhs_hr,
  survey_vars = list(
    cluster = "hv001",
    weight = "hv005",
    stratum = "hv022",
    adm1 = "hv024",
    adm2 = NULL,
    wealth_quintile = "hv270",
    wealth_score = "hv271",
    hh_members = "hv012"
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
  # ---- 1. input validation ---------------------------------------------------

  if (!is.data.frame(dhs_hr)) {
    cli::cli_abort("dhs_hr must be a data.frame or tibble")
  }

  if (nrow(dhs_hr) == 0) {
    cli::cli_abort("dhs_hr is empty")
  }

  # Required mapping in survey_vars
  needed <- c(
    "cluster",
    "weight",
    "stratum",
    "wealth_quintile",
    "wealth_score",
    "hh_members"
  )

  if (!all(needed %in% names(survey_vars))) {
    missing <- setdiff(needed, names(survey_vars))
    cli::cli_abort("`survey_vars` must include: {missing}")
  }

  # Check that mapped columns exist in data
  mapped_cols <- unlist(survey_vars[needed])
  mapped_cols <- mapped_cols[!is.null(mapped_cols)]
  missing_cols <- setdiff(mapped_cols, names(dhs_hr))

  if (length(missing_cols) > 0) {
    cli::cli_abort(
      "Columns not found in dhs_hr: {missing_cols}. Check survey_vars mapping."
    )
  }

  # Admin availability
  has_adm1 <- "adm1" %in% names(survey_vars) &&
    !is.na(survey_vars$adm1) &&
    survey_vars$adm1 %in% names(dhs_hr)

  has_adm2 <- "adm2" %in% names(survey_vars) &&
    !is.null(survey_vars$adm2) &&
    survey_vars$adm2 %in% names(dhs_hr)

  # ---- 2. prepare base dataset -----------------------------------------------

  hr <- dhs_hr |>
    dplyr::mutate(
      dplyr::across(
        dplyr::everything(),
        haven::zap_labels
      )
    ) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::everything(),
        ~ as.vector(.x)
      )
    ) |>
    dplyr::mutate(
      adm1 = if (has_adm1) {
        haven::as_factor(.data[[survey_vars$adm1]]) |>
          as.character() |>
          toupper()
      } else {
        NA_character_
      },
      adm2 = if (has_adm2) {
        haven::as_factor(.data[[survey_vars$adm2]]) |>
          as.character() |>
          toupper()
      } else {
        NA_character_
      }
    ) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::everything(),
        haven::zap_labels
      )
    ) |>
    dplyr::mutate(
      cluster_id = .data[[survey_vars$cluster]],
      stratum_id = .data[[survey_vars$stratum]],
      survey_weight = .data[[survey_vars$weight]] / 1e6,
      wealth_quintile = .data[[survey_vars$wealth_quintile]],
      wealth_score = .data[[survey_vars$wealth_score]],
      hh_members = .data[[survey_vars$hh_members]]
    ) |>
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::where(is.factor),
        .fns = as.character
      )
    )

  if (!has_adm2) {
    hr <- hr |>
      dplyr::select(-adm2)
  }

  # Validate wealth quintile values
  valid_quintiles <- hr$wealth_quintile %in% 1:5
  if (sum(!valid_quintiles, na.rm = TRUE) > 0) {
    cli::cli_warn(
      "{sum(!valid_quintiles, na.rm = TRUE)} records have invalid ",
      "wealth quintile values (not 1-5)"
    )
  }

  n_households <- nrow(hr)
  cli::cli_alert_info(
    "Processed {format(n_households, big.mark = ',')} households"
  )

  # ---- 3. determine grouping logic -------------------------------------------

  grouping_vars <- NULL

  if (!is.null(gps_data) && !is.null(shapefile)) {
    # Join admin levels from shapefile via GPS coordinates
    cli::cli_alert_info(
      "Joining GPS coordinates with administrative boundaries"
    )

    if (!requireNamespace("sf", quietly = TRUE)) {
      cli::cli_abort("Package 'sf' is required for spatial operations")
    }

    # Prepare GPS data
    gps_clean <- gps_data |>
      dplyr::select(
        cluster_id = !!gps_vars$cluster,
        lat = !!gps_vars$lat,
        lon = !!gps_vars$lon
      ) |>
      dplyr::distinct()

    # Add GPS to household data
    hr <- hr |>
      dplyr::left_join(gps_clean, by = "cluster_id")

    # Create spatial points from clusters
    clusters_sf <- hr |>
      dplyr::select(cluster_id, lat, lon) |>
      dplyr::distinct() |>
      dplyr::filter(!is.na(lat), !is.na(lon)) |>
      sf::st_as_sf(
        coords = c("lon", "lat"),
        crs = 4326
      )

    # Prepare shapefile
    shapefile <- shapefile |>
      sf::st_transform(4326) |>
      sf::st_make_valid()

    # Determine admin levels from shapefile
    if (is.null(admin_level)) {
      available_admins <- names(shapefile)[
        grepl("^adm[0-9]+$", names(shapefile))
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

    # Check admin columns exist
    missing_cols <- setdiff(admin_level, names(shapefile))
    if (length(missing_cols) > 0) {
      cli::cli_abort(
        "Admin columns not found in shapefile: ",
        "{paste(missing_cols, collapse = ', ')}"
      )
    }

    # Get admin name columns if available
    admin_name_cols <- paste0(admin_level, "_name")
    admin_name_cols <- admin_name_cols[
      admin_name_cols %in% names(shapefile)
    ]
    all_admin_cols <- c(admin_level, admin_name_cols)

    # Spatial join - assign admin levels to each cluster
    cluster_admin <- sf::st_join(
      clusters_sf,
      shapefile[, c(all_admin_cols, "geometry")],
      join = sf::st_within,
      left = TRUE
    )

    # Assign unmatched clusters to nearest admin unit
    if (join_nearest) {
      unmatched <- is.na(cluster_admin[[admin_level[1]]])

      if (any(unmatched)) {
        cli::cli_alert_info(
          "Assigning {format(sum(unmatched), big.mark = ',')} clusters to nearest admin units."
        )

        nearest_idx <- sf::st_nearest_feature(
          cluster_admin[unmatched, ],
          shapefile
        )

        for (col in all_admin_cols) {
          if (col %in% names(shapefile)) {
            cluster_admin[unmatched, col] <- shapefile[[col]][nearest_idx]
          }
        }
      }
    }

    # Convert to dataframe and join back to main dataset
    cluster_admin_df <- sf::st_drop_geometry(cluster_admin) |>
      dplyr::select(cluster_id, dplyr::all_of(all_admin_cols))

    # Remove any existing admin columns that conflict with shapefile columns
    conflicting_cols <- intersect(names(hr), all_admin_cols)

    if (length(conflicting_cols) > 0) {
      hr <- hr |>
        dplyr::select(-dplyr::all_of(conflicting_cols))
    }

    hr <- hr |>
      dplyr::left_join(cluster_admin_df, by = "cluster_id")

    # Use admin levels for grouping
    grouping_vars <- admin_level

    cli::cli_alert_info(
      "Calculating wealth indicators by {paste(admin_level, collapse = ' + ')}"
    )
  } else if (!is.null(gps_data)) {
    # Cluster-level with GPS only (no shapefile)
    gps_clean <- gps_data |>
      dplyr::select(
        cluster_id = !!gps_vars$cluster,
        lat = !!gps_vars$lat,
        lon = !!gps_vars$lon
      ) |>
      dplyr::distinct()

    hr <- hr |>
      dplyr::left_join(gps_clean, by = "cluster_id")

    grouping_vars <- "cluster_id"

    cli::cli_alert_info("Calculating cluster-level wealth indicators")
  } else if (
    "adm2" %in% names(hr) &&
      "adm1" %in% names(hr) &&
      !all(is.na(hr$adm2))
  ) {
    grouping_vars <- c("adm1", "adm2")
    cli::cli_alert_info("Using admin levels: adm1 and adm2")
  } else if ("adm1" %in% names(hr) && !all(is.na(hr$adm1))) {
    grouping_vars <- "adm1"
    cli::cli_alert_info("Using administrative level: adm1")
  } else {
    cli::cli_alert_info("Calculating national-level wealth indicators")
  }

  # ---- 4. calculate wealth quintile distributions ----------------------------

  quintile_labels <- c(
    "dhs_prop_poorest",
    "dhs_prop_poorer",
    "dhs_prop_middle",
    "dhs_prop_richer",
    "dhs_prop_richest"
  )

  if (!is.null(grouping_vars)) {
    wealth_dist <- hr |>
      dplyr::group_by(dplyr::across(dplyr::all_of(grouping_vars))) |>
      dplyr::summarise(
        dhs_n_households = dplyr::n(),
        dhs_weighted_households = sum(survey_weight, na.rm = TRUE),
        dhs_prop_poorest = sum(
          survey_weight * (wealth_quintile == 1),
          na.rm = TRUE
        ) / dhs_weighted_households,
        dhs_prop_poorer = sum(
          survey_weight * (wealth_quintile == 2),
          na.rm = TRUE
        ) / dhs_weighted_households,
        dhs_prop_middle = sum(
          survey_weight * (wealth_quintile == 3),
          na.rm = TRUE
        ) / dhs_weighted_households,
        dhs_prop_richer = sum(
          survey_weight * (wealth_quintile == 4),
          na.rm = TRUE
        ) / dhs_weighted_households,
        dhs_prop_richest = sum(
          survey_weight * (wealth_quintile == 5),
          na.rm = TRUE
        ) / dhs_weighted_households,
        dhs_gini = calculate_dhs_gini(
          wealth_scores = wealth_score,
          weights = survey_weight,
          population = hh_members
        ),
        dhs_gini_sample_size = dplyr::n(),
        .groups = "drop"
      )
  } else {
    wealth_dist <- hr |>
      dplyr::summarise(
        level = "National",
        dhs_n_households = dplyr::n(),
        dhs_weighted_households = sum(survey_weight, na.rm = TRUE),
        dhs_prop_poorest = sum(
          survey_weight * (wealth_quintile == 1),
          na.rm = TRUE
        ) / dhs_weighted_households,
        dhs_prop_poorer = sum(
          survey_weight * (wealth_quintile == 2),
          na.rm = TRUE
        ) / dhs_weighted_households,
        dhs_prop_middle = sum(
          survey_weight * (wealth_quintile == 3),
          na.rm = TRUE
        ) / dhs_weighted_households,
        dhs_prop_richer = sum(
          survey_weight * (wealth_quintile == 4),
          na.rm = TRUE
        ) / dhs_weighted_households,
        dhs_prop_richest = sum(
          survey_weight * (wealth_quintile == 5),
          na.rm = TRUE
        ) / dhs_weighted_households,
        dhs_gini = calculate_dhs_gini(
          wealth_scores = wealth_score,
          weights = survey_weight,
          population = hh_members
        ),
        dhs_gini_sample_size = dplyr::n()
      )
  }

  # Round proportions (keep as 0-1 scale)
  wealth_dist <- wealth_dist |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(quintile_labels),
        ~ round(.x, 2)
      ),
      dhs_gini = round(dhs_gini, 3),
      dhs_gini_reliable = dhs_gini_sample_size >= 25
    )

  # ---- 5. identify dominant quintile -----------------------------------------

  wealth_dist <- wealth_dist |>
    dplyr::rowwise() |>
    dplyr::mutate(
      dhs_dominant_quintile = {
        props <- c(
          dhs_prop_poorest,
          dhs_prop_poorer,
          dhs_prop_middle,
          dhs_prop_richer,
          dhs_prop_richest
        )
        quintile_names <- c("Poorest", "Poorer", "Middle", "Richer", "Richest")
        quintile_names[which.max(props)]
      },
      dhs_dominant_prop = max(
        dhs_prop_poorest,
        dhs_prop_poorer,
        dhs_prop_middle,
        dhs_prop_richer,
        dhs_prop_richest
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      dhs_dominant_quintile = factor(
        dhs_dominant_quintile,
        levels = c("Poorest", "Poorer", "Middle", "Richer", "Richest"),
        ordered = TRUE
      )
    )

  # ---- 6. attach GPS coordinates if cluster-level ----------------------------

  if (
    !is.null(gps_data) &&
      !is.null(grouping_vars) &&
      "cluster_id" %in% grouping_vars
  ) {
    gps_clean <- gps_data |>
      dplyr::select(
        cluster_id = !!gps_vars$cluster,
        lat = !!gps_vars$lat,
        lon = !!gps_vars$lon
      ) |>
      dplyr::distinct()

    wealth_dist <- wealth_dist |>
      dplyr::left_join(gps_clean, by = "cluster_id")

    na_share <- mean(
      is.na(wealth_dist$lat) | is.na(wealth_dist$lon)
    )

    if (is.finite(na_share) && na_share > 0.2) {
      cli::cli_alert_warning(
        "more than {round(100 * na_share, 1)}% of clusters lack coords"
      )
    }
  }

  # ---- 7. format column order ------------------------------------------------

  # Select only key final columns (remove intermediate/redundant columns)
  final_columns <- c(
    # admin levels or cluster
    grouping_vars,
    "level",
    "lat",
    "lon",
    # quintile proportions (percentages)
    "dhs_prop_poorest",
    "dhs_prop_poorer",
    "dhs_prop_middle",
    "dhs_prop_richer",
    "dhs_prop_richest",
    # dominant quintile
    "dhs_dominant_quintile",
    "dhs_dominant_prop",
    # inequality measure
    "dhs_gini",
    # sample counts and reliability
    "dhs_n_households",
    "dhs_weighted_households",
    "dhs_gini_sample_size",
    "dhs_gini_reliable"
  )

  final_columns <- base::intersect(final_columns, names(wealth_dist))

  wealth_dist <- wealth_dist |>
    dplyr::select(dplyr::all_of(final_columns))

  tibble::as_tibble(wealth_dist)
}

#' Calculate Wealth Quintile Distributions from DHS Data
#'
#' Main function for calculating wealth quintile distributions and Gini
#' coefficients from DHS Household Records data. Supports spatial aggregation
#' using administrative boundary shapefiles to calculate wealth indicators at
#' any administrative level. Returns both data and a comprehensive data
#' dictionary.
#'
#' @param dhs_hr DHS Household Records dataset in tidy format.
#' @param survey_vars Named list mapping DHS variable names.
#' @param gps_data Optional DHS GPS dataset with cluster coordinates.
#' @param gps_vars Named list for GPS variables (cluster, lat, lon).
#' @param shapefile Optional sf object with administrative boundaries.
#' @param admin_level Character vector specifying aggregation levels.
#' @param join_nearest Logical; if `TRUE`, assigns clusters outside all
#'   polygons to the nearest administrative unit.
#'
#' @return A list containing three elements:
#'   \itemize{
#'     \item `data`: A tibble with wealth quintile distributions and Gini
#'       coefficients
#'     \item `dict`: A data dictionary created using `sntutils::build_dictionary()`
#'     \item `metadata`: A list containing survey metadata
#'   }
#'
#' @examples
#' # Example with spatial aggregation
#' # wealth_results <- calc_wealth_dhs(
#' #   dhs_hr = hr_data,
#' #   gps_data = gps_data,
#' #   shapefile = admin_shapefile,
#' #   admin_level = c("adm1")
#' # )
#' #
#' # # Access the data
#' # wealth_data <- wealth_results$data
#' #
#' # # Access the dictionary
#' # wealth_dict <- wealth_results$dict
#' #
#' # # Access the metadata
#' # wealth_metadata <- wealth_results$metadata
#'
#' @export
calc_wealth_dhs <- function(
  dhs_hr,
  survey_vars = list(
    cluster = "hv001",
    weight = "hv005",
    stratum = "hv022",
    adm1 = "hv024",
    adm2 = NULL,
    wealth_quintile = "hv270",
    wealth_score = "hv271",
    hh_members = "hv012"
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
  # Extract metadata from DHS data
  metadata <- extract_wealth_metadata(dhs_hr, survey_vars)

  # Validate shapefile if provided
  if (!is.null(shapefile)) {
    if (!requireNamespace("sf", quietly = TRUE)) {
      cli::cli_abort("Package 'sf' required for spatial aggregation.")
    }

    if (!inherits(shapefile, "sf")) {
      cli::cli_abort("`shapefile` must be an sf object.")
    }

    if (is.null(gps_data)) {
      cli::cli_abort(
        "GPS data required for spatial aggregation with shapefile."
      )
    }
  }

  # Compute indicators (core handles spatial join if shapefile provided)
  wealth_results <- calc_wealth_dhs_core(
    dhs_hr = dhs_hr,
    survey_vars = survey_vars,
    gps_data = gps_data,
    gps_vars = gps_vars,
    shapefile = shapefile,
    admin_level = admin_level,
    join_nearest = join_nearest
  )

  # Update metadata with aggregation info
  if (!is.null(shapefile)) {
    if (is.null(admin_level)) {
      admin_level <- names(shapefile)[
        grepl("^adm[0-9]+$", names(shapefile))
      ]
    }
    metadata$aggregation_level <- admin_level
    metadata$spatial_join_method <- if (join_nearest) {
      "st_within with nearest fallback"
    } else {
      "st_within only"
    }
  } else if (!is.null(gps_data)) {
    metadata$aggregation_level <- "cluster"
  } else {
    metadata$aggregation_level <- "national or existing admin"
  }

  labels <- tibble::tribble(
    ~variable, ~label_en, ~label_fr, ~dhs_variable, ~numerator, ~denominator, ~dhs_numerator_var, ~dhs_denominator_var, ~dhs_recode, ~indicator_category, ~wmr_cascade_step, ~age_group, ~units, ~notes,
    "dhs_prop_poorest", "Proportion in poorest quintile", "Proportion dans le quintile le plus pauvre", "hv270", "Households in poorest", "All households", "hv270", "hv001", "HR", "Wealth", NA_integer_, NA_character_, "proportion (0-1)", "HR module; DHS-defined wealth quintiles",
    "dhs_prop_poorer", "Proportion in poorer quintile", "Proportion dans le quintile pauvre", "hv270", "Households in poorer", "All households", "hv270", "hv001", "HR", "Wealth", NA_integer_, NA_character_, "proportion (0-1)", "HR module; DHS-defined wealth quintiles",
    "dhs_prop_middle", "Proportion in middle quintile", "Proportion dans le quintile moyen", "hv270", "Households in middle", "All households", "hv270", "hv001", "HR", "Wealth", NA_integer_, NA_character_, "proportion (0-1)", "HR module; DHS-defined wealth quintiles",
    "dhs_prop_richer", "Proportion in richer quintile", "Proportion dans le quintile riche", "hv270", "Households in richer", "All households", "hv270", "hv001", "HR", "Wealth", NA_integer_, NA_character_, "proportion (0-1)", "HR module; DHS-defined wealth quintiles",
    "dhs_prop_richest", "Proportion in richest quintile", "Proportion dans le quintile le plus riche", "hv270", "Households in richest", "All households", "hv270", "hv001", "HR", "Wealth", NA_integer_, NA_character_, "proportion (0-1)", "HR module; DHS-defined wealth quintiles",
    "dhs_dominant_quintile", "Dominant wealth quintile", "Quintile de richesse dominant", "hv270", NA_character_, NA_character_, NA_character_, NA_character_, "HR", "Wealth", NA_integer_, NA_character_, "categorical", "Most common quintile in the area",
    "dhs_dominant_prop", "Proportion in dominant quintile", "Proportion dans le quintile dominant", "hv270", "Households in dominant quintile", "All households", "hv270", "hv001", "HR", "Wealth", NA_integer_, NA_character_, "proportion (0-1)", "Proportion of households in the dominant quintile",
    "dhs_gini", "Gini coefficient", "Coefficient de Gini", "hv271", NA_character_, NA_character_, NA_character_, NA_character_, "HR", "Wealth", NA_integer_, NA_character_, "coefficient (0-1)", "Computed from hv271 wealth factor scores",
    "dhs_n_households", "Number of households", "Nombre de menages", NA_character_, NA_character_, NA_character_, NA_character_, NA_character_, "HR", "Wealth", NA_integer_, NA_character_, "count", "Unweighted count",
    "dhs_weighted_households", "Weighted number of households", "Nombre pondere de menages", NA_character_, NA_character_, NA_character_, NA_character_, NA_character_, "HR", "Wealth", NA_integer_, NA_character_, "count", "Survey-weighted count",
    "dhs_gini_sample_size", "Sample size for Gini", "Taille d'echantillon pour Gini", NA_character_, NA_character_, NA_character_, NA_character_, NA_character_, "HR", "Wealth", NA_integer_, NA_character_, "count", "Number of households used for Gini calculation",
    "dhs_gini_reliable", "Gini reliability flag", "Indicateur de fiabilite du Gini", NA_character_, NA_character_, NA_character_, NA_character_, NA_character_, "HR", "Wealth", NA_integer_, NA_character_, "boolean", "TRUE if sample size >= 25"
  )

  dict <- sntutils::build_dictionary(wealth_results)
  dict <- .enrich_dhs_dictionary(dict, labels)

  list(
    data = wealth_results,
    dict = dict,
    metadata = metadata
  )
}

#' Aggregate Cluster-level Wealth Data to Administrative Levels
#'
#' Helper function to aggregate cluster-level wealth results to administrative
#' levels using a shapefile. Performs spatial joins and calculates weighted
#' averages by administrative unit.
#'
#' @param cluster_results Cluster-level results from `calc_wealth_dhs_core()`
#'   containing wealth quintile proportions, Gini coefficients, lat, and lon
#'   columns.
#' @param shapefile SF object with administrative boundaries containing columns
#'   named "adm0", "adm1", "adm2", etc.
#' @param admin_level Character vector of admin levels to aggregate to
#'   (e.g., `c("adm1")` or `c("adm1", "adm2")`).
#' @param weighted Logical. If `TRUE` (default), uses household-weighted
#'   averaging. If `FALSE`, uses simple unweighted mean.
#'
#' @return SF object with aggregated wealth indicators by administrative level,
#'   including geometry for mapping.
#'
#' @export
aggregate_wealth_admin <- function(
  cluster_results,
  shapefile,
  admin_level = c("adm1"),
  weighted = TRUE
) {
  if (!requireNamespace("sf", quietly = TRUE)) {
    cli::cli_abort("Package `sf` is required for spatial operations.")
  }

  if (!inherits(cluster_results, "sf")) {
    if (!all(c("lat", "lon") %in% names(cluster_results))) {
      cli::cli_abort(
        "cluster_results must have lat and lon for spatial aggregation."
      )
    }

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

  wealth_cols <- names(joined_df)[
    grepl("^dhs_prop_|^dhs_gini$|^dhs_dominant", names(joined_df))
  ]

  sample_cols <- c(
    "dhs_n_households",
    "dhs_weighted_households",
    "dhs_gini_sample_size"
  )
  sample_cols <- sample_cols[sample_cols %in% names(joined_df)]

  if (weighted && "dhs_weighted_households" %in% names(joined_df)) {
    aggregated <- joined_df |>
      dplyr::group_by(dplyr::across(dplyr::all_of(admin_level))) |>
      dplyr::summarise(
        dhs_prop_poorest = stats::weighted.mean(
          dhs_prop_poorest,
          w = dhs_weighted_households,
          na.rm = TRUE
        ),
        dhs_prop_poorer = stats::weighted.mean(
          dhs_prop_poorer,
          w = dhs_weighted_households,
          na.rm = TRUE
        ),
        dhs_prop_middle = stats::weighted.mean(
          dhs_prop_middle,
          w = dhs_weighted_households,
          na.rm = TRUE
        ),
        dhs_prop_richer = stats::weighted.mean(
          dhs_prop_richer,
          w = dhs_weighted_households,
          na.rm = TRUE
        ),
        dhs_prop_richest = stats::weighted.mean(
          dhs_prop_richest,
          w = dhs_weighted_households,
          na.rm = TRUE
        ),
        dhs_gini = stats::weighted.mean(
          dhs_gini,
          w = dhs_gini_sample_size,
          na.rm = TRUE
        ),
        dhs_n_households = sum(dhs_n_households, na.rm = TRUE),
        dhs_weighted_households = sum(dhs_weighted_households, na.rm = TRUE),
        dhs_gini_sample_size = sum(dhs_gini_sample_size, na.rm = TRUE),
        dhs_n_clusters = dplyr::n(),
        .groups = "drop"
      )
  } else {
    aggregated <- joined_df |>
      dplyr::group_by(dplyr::across(dplyr::all_of(admin_level))) |>
      dplyr::summarise(
        dhs_prop_poorest = mean(dhs_prop_poorest, na.rm = TRUE),
        dhs_prop_poorer = mean(dhs_prop_poorer, na.rm = TRUE),
        dhs_prop_middle = mean(dhs_prop_middle, na.rm = TRUE),
        dhs_prop_richer = mean(dhs_prop_richer, na.rm = TRUE),
        dhs_prop_richest = mean(dhs_prop_richest, na.rm = TRUE),
        dhs_gini = mean(dhs_gini, na.rm = TRUE),
        dhs_n_households = sum(dhs_n_households, na.rm = TRUE),
        dhs_weighted_households = sum(dhs_weighted_households, na.rm = TRUE),
        dhs_gini_sample_size = sum(dhs_gini_sample_size, na.rm = TRUE),
        dhs_n_clusters = dplyr::n(),
        .groups = "drop"
      )
  }

  # Identify dominant quintile
  aggregated <- aggregated |>
    dplyr::rowwise() |>
    dplyr::mutate(
      dhs_dominant_quintile = {
        props <- c(
          dhs_prop_poorest,
          dhs_prop_poorer,
          dhs_prop_middle,
          dhs_prop_richer,
          dhs_prop_richest
        )
        quintile_names <- c("Poorest", "Poorer", "Middle", "Richer", "Richest")
        quintile_names[which.max(props)]
      },
      dhs_dominant_prop = max(
        dhs_prop_poorest,
        dhs_prop_poorer,
        dhs_prop_middle,
        dhs_prop_richer,
        dhs_prop_richest
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      dhs_dominant_quintile = factor(
        dhs_dominant_quintile,
        levels = c("Poorest", "Poorer", "Middle", "Richer", "Richest"),
        ordered = TRUE
      ),
      dhs_gini = round(dhs_gini, 3),
      dhs_gini_reliable = dhs_gini_sample_size >= 25
    )

  admin_name_cols <- paste0(admin_level, "_name")
  admin_name_cols <- admin_name_cols[admin_name_cols %in% names(shapefile)]

  result <- shapefile |>
    dplyr::select(dplyr::all_of(c(admin_level, admin_name_cols))) |>
    dplyr::distinct() |>
    dplyr::left_join(aggregated, by = admin_level)

  result
}
