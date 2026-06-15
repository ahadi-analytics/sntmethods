# wealth indicator
#
# Merged from: dhs_calc_wealth.R dhs_calc_wealth_mbg.R dhs_helpers_wealth_stratified.R 
# Contains the survey-weighted calc, MBG cluster-prep, and indicator-
# specific helpers for this family.

# ---- dhs_calc_wealth.R ----

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
#' @keywords internal
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


# =============================================================================
# Wealth indicator conditions and dictionary
# =============================================================================

#' Internal: Wealth indicator conditions
#'
#' Returns list of indicator specifications for wealth quintile proportions
#' and Gini coefficient. Each quintile is a separate indicator.
#'
#' @return List of named lists.
#' @noRd
.wealth_conditions <- function() {
  list(
    list(
      indicator       = "WEALTH_Q1",
      indicator_code  = "wealth_q1",
      indicator_title = "Proportion in poorest wealth quintile (Q1)",
      outcome_var     = "is_q1",
      filter_expr     = NULL,
      num_desc        = "Households in poorest quintile (Q1)",
      denom_desc      = "All households with valid wealth quintile",
      denom_code      = "hh_wealth"
    ),
    list(
      indicator       = "WEALTH_Q2",
      indicator_code  = "wealth_q2",
      indicator_title = "Proportion in poorer wealth quintile (Q2)",
      outcome_var     = "is_q2",
      filter_expr     = NULL,
      num_desc        = "Households in poorer quintile (Q2)",
      denom_desc      = "All households with valid wealth quintile",
      denom_code      = "hh_wealth"
    ),
    list(
      indicator       = "WEALTH_Q3",
      indicator_code  = "wealth_q3",
      indicator_title = "Proportion in middle wealth quintile (Q3)",
      outcome_var     = "is_q3",
      filter_expr     = NULL,
      num_desc        = "Households in middle quintile (Q3)",
      denom_desc      = "All households with valid wealth quintile",
      denom_code      = "hh_wealth"
    ),
    list(
      indicator       = "WEALTH_Q4",
      indicator_code  = "wealth_q4",
      indicator_title = "Proportion in richer wealth quintile (Q4)",
      outcome_var     = "is_q4",
      filter_expr     = NULL,
      num_desc        = "Households in richer quintile (Q4)",
      denom_desc      = "All households with valid wealth quintile",
      denom_code      = "hh_wealth"
    ),
    list(
      indicator       = "WEALTH_Q5",
      indicator_code  = "wealth_q5",
      indicator_title = "Proportion in richest wealth quintile (Q5)",
      outcome_var     = "is_q5",
      filter_expr     = NULL,
      num_desc        = "Households in richest quintile (Q5)",
      denom_desc      = "All households with valid wealth quintile",
      denom_code      = "hh_wealth"
    ),
    list(
      indicator       = "GINI",
      indicator_code  = "gini",
      indicator_title = "Gini coefficient of wealth inequality",
      outcome_var     = NA_character_,
      filter_expr     = NULL,
      num_desc        = "Wealth inequality (Brown formula)",
      denom_desc      = "All households with valid wealth score",
      denom_code      = "hh_wealth_score"
    )
  )
}


#' Wealth Indicator Dictionary
#'
#' Returns the full dictionary of wealth indicators with metadata.
#'
#' @return Tibble with columns: indicator, indicator_code, indicator_title,
#'   numerator_description, denominator_description, denominator_code.
#'
#' @keywords internal
wealth_dictionary <- function() {
  conds <- .wealth_conditions()
  tibble::tibble(
    indicator               = vapply(conds, `[[`, character(1), "indicator"),
    indicator_code          = vapply(conds, `[[`, character(1), "indicator_code"),
    indicator_title         = vapply(conds, `[[`, character(1), "indicator_title"),
    numerator_description   = vapply(conds, `[[`, character(1), "num_desc"),
    denominator_description = vapply(conds, `[[`, character(1), "denom_desc"),
    denominator_code        = vapply(conds, `[[`, character(1), "denom_code")
  )
}


#' Calculate Wealth Quintile Distributions from DHS Data
#'
#' Computes wealth quintile proportions (Q1-Q5) and Gini coefficient from
#' DHS Household Records data. Returns survey-weighted estimates in
#' standardized long format with `list(adm0, adm1)` structure.
#'
#' @param dhs_hr DHS Household Records dataset in tidy format.
#' @param survey_vars Named list mapping DHS variable names.
#' @param region_var Optional column name for subnational grouping
#'   (e.g., "hv024"). Auto-falls back to "hv024" if no spatial params.
#' @param gps_data Optional DHS GPS dataset with cluster coordinates.
#' @param gps_vars Named list for GPS variables (cluster, lat, lon).
#' @param shapefile Optional sf object with administrative boundaries.
#' @param admin_level Character vector specifying aggregation levels.
#' @param join_nearest Logical; if `TRUE`, assigns clusters outside all
#'   polygons to nearest administrative unit.
#' @param ci_method Method for confidence intervals. Default: "logit".
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
#' @seealso [wealth_dictionary()] for indicator definitions,
#'   [calc_wealth_dhs_core()] for the legacy wide-format output
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
  region_var   = NULL,
  gps_data     = NULL,
  gps_vars     = list(
    cluster = "DHSCLUST",
    lat     = "LATNUM",
    lon     = "LONGNUM"
  ),
  shapefile    = NULL,
  admin_level  = NULL,
  join_nearest = TRUE,
  ci_method    = "logit"
) {
  # Fail fast on missing suggested dependencies
  .check_pkg(
    c("purrr", "tibble"),
    reason = "for `calc_wealth_dhs()`"
  )

  # ---- 1. Input validation ----

  if (!is.data.frame(dhs_hr)) {
    cli::cli_abort("`dhs_hr` must be a data.frame or tibble.")
  }
  if (nrow(dhs_hr) == 0) {
    cli::cli_abort("`dhs_hr` is empty.")
  }

  # Check required survey variables
  needed_cols <- c(survey_vars$cluster, survey_vars$weight,
                   survey_vars$stratum, survey_vars$wealth_quintile)
  missing_cols <- setdiff(needed_cols, names(dhs_hr))
  if (length(missing_cols) > 0) {
    cli::cli_abort(c(
      "Required variables not found: {.var {missing_cols}}",
      "i" = "Check your survey_vars mapping"
    ))
  }

  # ---- 2. Extract survey metadata ----

  survey_meta <- .extract_survey_meta_hv(dhs_hr)

  # ---- 3. Prepare household-level data ----

  hr_zapped <- dhs_hr |>
    dplyr::mutate(dplyr::across(dplyr::everything(), haven::zap_labels)) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), as.vector))

  hr_data <- tibble::tibble(
    cluster_id      = hr_zapped[[survey_vars$cluster]],
    stratum_id      = hr_zapped[[survey_vars$stratum]],
    survey_weight   = hr_zapped[[survey_vars$weight]] / 1e6,
    wealth_quintile = hr_zapped[[survey_vars$wealth_quintile]]
  )

  # Filter to valid quintile values

  hr_data <- hr_data |>
    dplyr::filter(wealth_quintile %in% 1:5)

  # Create binary outcome variables for each quintile
  hr_data <- hr_data |>
    dplyr::mutate(
      is_q1 = as.integer(wealth_quintile == 1),
      is_q2 = as.integer(wealth_quintile == 2),
      is_q3 = as.integer(wealth_quintile == 3),
      is_q4 = as.integer(wealth_quintile == 4),
      is_q5 = as.integer(wealth_quintile == 5)
    )

  # Add wealth score and hh_members for Gini calculation
  wealth_score_var <- survey_vars$wealth_score %||% "hv271"
  hh_members_var <- survey_vars$hh_members %||% "hv012"

  if (wealth_score_var %in% names(hr_zapped)) {
    hr_data$wealth_score <- hr_zapped[[wealth_score_var]]
  }
  if (hh_members_var %in% names(hr_zapped)) {
    hr_data$hh_members <- hr_zapped[[hh_members_var]]
  }

  n_hh <- nrow(hr_data)
  cli::cli_alert_info(
    "Processed {format(n_hh, big.mark = ',')} households with valid wealth quintile"
  )

  # ---- 4. Region grouping ----

  region_hr_var <- survey_vars$adm1 %||% "hv024"
  group_var <- NULL
  geo_src <- NA_character_

  # Auto-fallback to hv024 when no spatial parameters provided
  if (is.null(region_var) && is.null(gps_data) && is.null(shapefile)) {
    if (region_hr_var %in% names(dhs_hr)) {
      region_var <- region_hr_var
      cli::cli_alert_info(
        "No region_var/GPS/shapefile specified; defaulting to {.var {region_hr_var}} for adm1"
      )
    }
  }

  if (!is.null(region_var)) {
    if (!region_var %in% names(dhs_hr)) {
      cli::cli_abort("Column {.var {region_var}} not found in `dhs_hr`.")
    }
    hr_data$region <- .resolve_region_labels(
      dhs_hr[[region_var]], region_var
    )
    # Subset to rows that passed the quintile filter
    # (region was extracted from dhs_hr before filtering, need to align)
    # Re-extract from the same rows
    valid_idx <- which(hr_zapped[[survey_vars$wealth_quintile]] %in% 1:5)
    hr_data$region <- .resolve_region_labels(
      dhs_hr[[region_var]][valid_idx], region_var
    )
    group_var <- "region"
    geo_src <- "survey"
  }

  # ---- 5. Get conditions and compute quintile indicators ----

  conds <- .wealth_conditions()
  # Only compute Q1-Q5 (not Gini) via .compute_dhs_indicator_generic
  q_conds <- conds[seq_len(5)]

  meta_cols <- tibble::tibble(
    survey_id   = survey_meta$survey_id,
    iso3        = survey_meta$iso3,
    iso2        = survey_meta$iso2,
    survey_type = survey_meta$survey_type,
    survey_year = survey_meta$survey_year,
    adm0        = survey_meta$country_upper
  )

  .round_results <- function(tbl) {
    tbl |>
      dplyr::mutate(
        point       = round(point, 3),
        ci_l        = round(pmax(ci_l, 0, na.rm = TRUE), 3),
        ci_u        = round(pmin(ci_u, 1, na.rm = TRUE), 3),
        numerator   = as.integer(numerator),
        denominator = as.integer(denominator)
      )
  }

  # --- Compute Gini nationally ---
  gini_cond <- conds[[6]]
  has_gini_data <- "wealth_score" %in% names(hr_data) &&
    "hh_members" %in% names(hr_data)

  .build_gini_row <- function(data_subset, level_val, location_val) {
    if (!has_gini_data) {
      return(tibble::tibble(
        level = level_val, location = location_val,
        point = NA_real_, ci_l = NA_real_, ci_u = NA_real_,
        numerator = NA_integer_, denominator = as.integer(nrow(data_subset)),
        indicator = gini_cond$indicator_title,
        indicator_code = gini_cond$indicator_code,
        numerator_description = gini_cond$num_desc,
        denominator_description = gini_cond$denom_desc,
        denominator_code = gini_cond$denom_code
      ))
    }
    gini_val <- calculate_dhs_gini(
      wealth_scores = data_subset$wealth_score,
      weights       = data_subset$survey_weight,
      population    = data_subset$hh_members
    )
    tibble::tibble(
      level = level_val, location = location_val,
      point = round(gini_val, 3), ci_l = NA_real_, ci_u = NA_real_,
      numerator = NA_integer_, denominator = as.integer(nrow(data_subset)),
      indicator = gini_cond$indicator_title,
      indicator_code = gini_cond$indicator_code,
      numerator_description = gini_cond$num_desc,
      denominator_description = gini_cond$denom_desc,
      denominator_code = gini_cond$denom_code
    )
  }

  # --- adm0 (national) ---
  national_results <- purrr::map_dfr(q_conds, function(cond) {
    .compute_dhs_indicator_generic(
      data      = hr_data,
      condition = cond,
      group_var = NULL,
      ci_method = ci_method
    )
  })

  # Add Gini national
  gini_national <- .build_gini_row(hr_data, "adm0", "National")
  national_results <- dplyr::bind_rows(national_results, gini_national)

  national_results <- .round_results(national_results)

  adm0_tbl <- dplyr::bind_cols(
    meta_cols[rep(1, nrow(national_results)), ],
    tibble::tibble(type = "survey_weighted", geo_source = NA_character_),
    national_results |> dplyr::select(-level, -location)
  ) |>
    tibble::as_tibble()

  out <- list(adm0 = adm0_tbl)

  # --- adm1 (subnational) ---
  if (!is.null(group_var)) {
    sub_results <- purrr::map_dfr(q_conds, function(cond) {
      .compute_dhs_indicator_generic(
        data              = hr_data,
        condition         = cond,
        group_var         = group_var,
        subnational_level = "adm1",
        ci_method         = ci_method
      )
    })

    # Add Gini by region
    regions <- unique(hr_data[[group_var]])
    regions <- regions[!is.na(regions)]
    gini_regional <- purrr::map_dfr(regions, function(rgn) {
      sub <- hr_data[hr_data[[group_var]] == rgn, ]
      .build_gini_row(sub, "adm1", rgn)
    })

    sub_results <- dplyr::bind_rows(sub_results, gini_regional)

    # Filter to regional rows only
    sub_results <- sub_results |>
      dplyr::filter(level != "adm0")

    if (nrow(sub_results) > 0) {
      sub_results <- .round_results(sub_results)

      sub_tbl <- dplyr::bind_cols(
        meta_cols[rep(1, nrow(sub_results)), ],
        sub_results |>
          dplyr::transmute(
            adm1       = toupper(location),
            type       = "survey_weighted",
            geo_source = geo_src,
            point, ci_l, ci_u,
            numerator, denominator,
            indicator, indicator_code,
            numerator_description,
            denominator_description, denominator_code
          )
      ) |>
        tibble::as_tibble()

      out[["adm1"]] <- sub_tbl
    }
  }

  out
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
#' @keywords internal
#' @noRd
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


# ---- dhs_calc_wealth_mbg.R ----

#' Prepare Wealth Quintile Distribution Data for MBG Analysis
#'
#' Prepares cluster-level wealth quintile distribution data for Model-Based
#' Geostatistics (MBG) analysis. Calculates proportions of households in each
#' wealth quintile, aggregated to cluster level.
#'
#' @details
#' This function prepares wealth distribution indicators for spatial modeling.
#' Unlike the survey-weighted [calc_wealth_dhs()], this uses simple cluster-level
#' counts without survey weights - MBG handles spatial smoothing internally.
#'
#' **Pipeline Integration:** This function IS called by [run_mbg_pipeline()]
#' when you specify `indicators = "wealth"` or individual codes like
#' `"prop_poorest"`.
#'
#' Methodology: Uses DHS wealth quintile variable (hv270 in HR recode) which
#' classifies households into 5 quintiles based on wealth index factor scores.
#'
#' @param dhs_hr DHS Household Records (HR) dataset.
#' @param gps_data DHS GPS dataset with cluster coordinates.
#' @param indicators Character vector of indicators to calculate:
#'   \itemize{
#'     \item "prop_poorest" or "prop_q1": Proportion in poorest quintile (Q1)
#'     \item "prop_poorer" or "prop_q2": Proportion in second quintile (Q2)
#'     \item "prop_middle" or "prop_q3": Proportion in middle quintile (Q3)
#'     \item "prop_richer" or "prop_q4": Proportion in fourth quintile (Q4)
#'     \item "prop_richest" or "prop_q5": Proportion in richest quintile (Q5)
#'   }
#'   Default: c("prop_poorest", "prop_richest") for equity analysis.
#' @param survey_vars Named list mapping DHS variable names:
#'   \itemize{
#'     \item cluster: Cluster ID (default: "hv001")
#'     \item wealth_quintile: Wealth quintile variable (default: "hv270")
#'   }
#' @param gps_vars Named list for GPS variable mapping.
#'
#' @return A named list of data.tables (one per indicator), each with columns:
#'   \itemize{
#'     \item cluster_id: Cluster identifier
#'     \item indicator: Numerator count (households in quintile)
#'     \item samplesize: Denominator count (all households)
#'     \item x: Longitude
#'     \item y: Latitude
#'   }
#'
#' @section Output Structure:
#' For `indicators = c("prop_poorest", "prop_richest")`:
#' \preformatted{
#' list(
#'   prop_poorest = data.table(cluster_id, indicator, samplesize, x, y),
#'   prop_richest = data.table(cluster_id, indicator, samplesize, x, y)
#' )
#' }
#'
#' @examples
#' \dontrun{
#' # Poorest quintile distribution for equity mapping
#' wealth_poorest <- calc_wealth_mbg(
#'   dhs_hr = hr_data,
#'   gps_data = gps_data,
#'   indicators = "prop_poorest"
#' )
#'
#' # Compare poorest vs richest for inequality analysis
#' wealth_inequality <- calc_wealth_mbg(
#'   dhs_hr = hr_data,
#'   gps_data = gps_data,
#'   indicators = c("prop_poorest", "prop_richest")
#' )
#'
#' # Via pipeline
#' results <- run_mbg_pipeline(
#'   country_iso3 = "gin",
#'   indicators = "wealth",
#'   ...
#' )
#' }
#'
#' @seealso
#' * [calc_wealth_dhs()] for survey-weighted wealth estimates with CIs
#' * [run_mbg_pipeline()] for automated pipeline processing
#' @export
calc_wealth_mbg <- function(
  dhs_hr,
  gps_data,
  indicators = c("prop_poorest", "prop_richest"),
  survey_vars = list(
    cluster = "hv001",
    wealth_quintile = "hv270"
  ),
  gps_vars = list(
    cluster = "DHSCLUST",
    lat = "LATNUM",
    lon = "LONGNUM"
  )
) {
  # Fail fast on missing suggested dependencies
  .check_pkg(
    c("tibble"),
    reason = "for `calc_wealth_mbg()`"
  )

  # ---- Input validation ----

  if (!is.data.frame(dhs_hr)) {
    cli::cli_abort("`dhs_hr` must be a data.frame or tibble")
  }
  if (!is.data.frame(gps_data)) {
    cli::cli_abort("`gps_data` must be a data.frame or tibble")
  }

  dict <- .wealth_mbg_dictionary()
  dict_names <- vapply(dict, `[[`, character(1), "name")

  # Default: poorest and richest
  if (is.null(indicators)) {
    indicators <- c("prop_poorest", "prop_richest")
  }

  invalid <- setdiff(indicators, dict_names)
  if (length(invalid) > 0) {
    cli::cli_abort(
      "Invalid indicators: {.val {invalid}}. Valid: {.val {dict_names}}"
    )
  }

  # Filter dictionary to requested indicators
  dict_specs <- dict[vapply(dict, function(d) d$name %in% indicators, logical(1))]

  # ---- Prepare GPS data ----

  gps_clean <- .prepare_gps_data(gps_data, gps_vars)

  # ---- Prepare household data ----

  cluster_var <- survey_vars$cluster
  wealth_var <- survey_vars$wealth_quintile

  if (!cluster_var %in% names(dhs_hr)) {
    cli::cli_abort("Cluster variable {.var {cluster_var}} not found in dhs_hr")
  }
  if (!wealth_var %in% names(dhs_hr)) {
    cli::cli_abort("Wealth variable {.var {wealth_var}} not found in dhs_hr")
  }

  # Extract and zap labels
  hr_clean <- dhs_hr |>
    dplyr::mutate(dplyr::across(dplyr::everything(), haven::zap_labels)) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), as.vector))

  hr_data <- tibble::tibble(
    cluster_id = hr_clean[[cluster_var]],
    wealth_quintile = as.numeric(hr_clean[[wealth_var]])
  )

  # Filter to valid quintiles
  hr_data <- hr_data |>
    dplyr::filter(wealth_quintile %in% 1:5, !is.na(wealth_quintile))

  if (nrow(hr_data) == 0) {
    cli::cli_abort("No valid household data after filtering")
  }

  cli::cli_alert_success(
    "Valid households: {format(nrow(hr_data), big.mark = ',')}"
  )

  # Create binary indicators for each quintile
  hr_data <- hr_data |>
    dplyr::mutate(
      in_q1 = as.integer(wealth_quintile == 1),
      in_q2 = as.integer(wealth_quintile == 2),
      in_q3 = as.integer(wealth_quintile == 3),
      in_q4 = as.integer(wealth_quintile == 4),
      in_q5 = as.integer(wealth_quintile == 5)
    )

  # ---- Dictionary-driven indicator loop ----

  results <- list()

  for (spec in dict_specs) {
    outcome_col <- spec$outcome

    if (!outcome_col %in% names(hr_data)) {
      cli::cli_alert_warning(
        "Outcome {.var {outcome_col}} not found for {.val {spec$name}} - skipping"
      )
      next
    }

    # Drop NAs in outcome
    filtered <- hr_data[!is.na(hr_data[[outcome_col]]), , drop = FALSE]

    if (nrow(filtered) == 0) {
      cli::cli_alert_warning("No data for {.val {spec$name}} - skipping")
      next
    }

    dt <- .aggregate_to_mbg_clusters(filtered, outcome_col, gps_clean, spec$name)
    if (!is.null(dt)) {
      results[[spec$name]] <- dt
    }
  }

  if (length(results) == 0) {
    cli::cli_abort("No valid MBG data could be prepared")
  }

  cli::cli_alert_success(
    "Prepared {length(results)} wealth indicator(s)"
  )

  results
}


# =============================================================================
# Wealth MBG Indicator Dictionary
# =============================================================================

#' Wealth MBG Indicator Dictionary
#'
#' Returns the full set of standardized indicator specifications for
#' cluster-level wealth MBG output. Each entry defines the indicator name
#' and outcome column (binary indicator for each quintile).
#'
#' @return List of named lists with fields: \code{name}, \code{outcome},
#'   \code{quintile}.
#' @noRd
.wealth_mbg_dictionary <- function() {
  list(
    list(name = "prop_poorest", outcome = "in_q1", quintile = 1),
    list(name = "prop_q1",      outcome = "in_q1", quintile = 1),
    list(name = "prop_poorer",  outcome = "in_q2", quintile = 2),
    list(name = "prop_q2",      outcome = "in_q2", quintile = 2),
    list(name = "prop_middle",  outcome = "in_q3", quintile = 3),
    list(name = "prop_q3",      outcome = "in_q3", quintile = 3),
    list(name = "prop_richer",  outcome = "in_q4", quintile = 4),
    list(name = "prop_q4",      outcome = "in_q4", quintile = 4),
    list(name = "prop_richest", outcome = "in_q5", quintile = 5),
    list(name = "prop_q5",      outcome = "in_q5", quintile = 5)
  )
}


#' Prepare Single Wealth Indicator for MBG
#'
#' Convenience wrapper around [calc_wealth_mbg()] to prepare a single
#' wealth quintile distribution indicator.
#'
#' @inheritParams calc_wealth_mbg
#' @param indicator Single indicator name. Default: "prop_poorest".
#'
#' @return Named list with single data.table containing columns:
#'   cluster_id, indicator, samplesize, x, y
#'
#' @examples
#' \dontrun{
#' # Poorest quintile distribution only
#' poorest <- prep_wealth_mbg(
#'   dhs_hr = hr_data,
#'   gps_data = gps_data,
#'   indicator = "prop_poorest"
#' )
#' }
#'
#' @export
prep_wealth_mbg <- function(
  dhs_hr,
  gps_data,
  indicator = "prop_poorest",
  survey_vars = list(
    cluster = "hv001",
    wealth_quintile = "hv270"
  ),
  gps_vars = list(
    cluster = "DHSCLUST",
    lat = "LATNUM",
    lon = "LONGNUM"
  )
) {
  result <- calc_wealth_mbg(
    dhs_hr = dhs_hr,
    gps_data = gps_data,
    indicators = indicator,
    survey_vars = survey_vars,
    gps_vars = gps_vars
  )

  result
}


# ---- dhs_helpers_wealth_stratified.R ----

# =============================================================================
# Shared helpers for wealth-stratified DHS indicators
# =============================================================================

#' Prepare wealth quintile variable for stratification
#'
#' Standardizes wealth quintile extraction from DHS datasets. Handles both
#' household (hv270) and individual (v190) recodes.
#'
#' @param dhs_data DHS dataset (KR, IR, PR, or HR recode).
#' @param wealth_var Name of wealth quintile variable. Default: "v190" for
#'   individual recodes, "hv270" for household recodes.
#' @param quintiles Numeric vector of quintiles to include. Default: 1:5 (all).
#'   Use c(1) for poorest only, c(1,2) for poorest + poorer, etc.
#'
#' @return Input dataset with added `wealth_quintile` column, filtered to
#'   requested quintiles. Rows with NA wealth are removed.
#' @noRd
.add_wealth_quintile <- function(dhs_data, wealth_var = NULL, quintiles = 1:5) {
  # Auto-detect wealth variable if not specified
  if (is.null(wealth_var)) {
    if ("v190" %in% names(dhs_data)) {
      wealth_var <- "v190"
    } else if ("hv270" %in% names(dhs_data)) {
      wealth_var <- "hv270"
    } else {
      cli::cli_abort(c(
        "No wealth quintile variable found",
        "i" = "Expected 'v190' (individual recode) or 'hv270' (household recode)",
        "i" = "Specify wealth_var parameter if using custom variable name"
      ))
    }
  }

  if (!wealth_var %in% names(dhs_data)) {
    cli::cli_abort(
      "Wealth variable {.var {wealth_var}} not found in dataset"
    )
  }

  # Extract and validate wealth quintile
  dhs_data$wealth_quintile <- as.numeric(dhs_data[[wealth_var]])

  # Remove NA wealth
  n_before <- nrow(dhs_data)
  dhs_data <- dhs_data[!is.na(dhs_data$wealth_quintile), , drop = FALSE]
  n_after <- nrow(dhs_data)

  if (n_after < n_before) {
    cli::cli_alert_info(
      "Removed {n_before - n_after} rows with missing wealth quintile"
    )
  }

  # Filter to requested quintiles
  valid_q <- dhs_data$wealth_quintile %in% quintiles
  dhs_data <- dhs_data[valid_q, , drop = FALSE]

  if (nrow(dhs_data) == 0) {
    cli::cli_abort(
      "No observations remain after filtering to quintiles: {quintiles}"
    )
  }

  cli::cli_alert_info(
    "Filtered to {nrow(dhs_data)} observations in quintile(s): {paste(quintiles, collapse = ', ')}"
  )

  dhs_data
}


#' Aggregate to MBG clusters by wealth quintile
#'
#' Extension of `.aggregate_to_mbg_clusters` that produces separate outputs
#' for each wealth quintile.
#'
#' @param individual_data Individual-level data with cluster_id and wealth_quintile.
#' @param indicator_col Name of binary 0/1 indicator column.
#' @param gps_clean GPS data with cluster_id, lat, lon.
#' @param result_name Base name for output (e.g., "csb_public").
#' @param quintiles Numeric vector of quintiles to include. Default: 1:5.
#'
#' @return Named list of data.tables, one per quintile. Each has columns:
#'   cluster_id, indicator, samplesize, x, y. Names are formatted as
#'   "{result_name}_q{quintile}" (e.g., "csb_public_q1", "csb_public_q2").
#' @noRd
.aggregate_to_mbg_clusters_by_wealth <- function(
  individual_data,
  indicator_col,
  gps_clean,
  result_name = "indicator",
  quintiles = 1:5
) {
  if (!"wealth_quintile" %in% names(individual_data)) {
    cli::cli_abort(
      "Data must have 'wealth_quintile' column. Use .add_wealth_quintile() first."
    )
  }

  results <- list()

  for (q in quintiles) {
    subset_data <- individual_data[
      individual_data$wealth_quintile == q,
      ,
      drop = FALSE
    ]

    if (nrow(subset_data) == 0) {
      cli::cli_alert_warning(
        "{result_name} Q{q}: no observations in this quintile"
      )
      next
    }

    cluster_data <- .aggregate_to_mbg_clusters(
      individual_data = subset_data,
      indicator_col = indicator_col,
      gps_clean = gps_clean,
      result_name = paste0(result_name, "_q", q)
    )

    if (!is.null(cluster_data)) {
      results[[paste0(result_name, "_q", q)]] <- cluster_data
    }
  }

  results
}


#' Compute survey-weighted indicator by wealth quintile
#'
#' Extension of `.compute_dhs_indicator_generic` that calculates estimates
#' separately for each wealth quintile.
#'
#' @param data Prepared dataset with wealth_quintile column.
#' @param condition Indicator condition specification (from indicator functions).
#' @param group_var Optional grouping variable (e.g., "region" for adm1).
#' @param subnational_level Admin level name (e.g., "adm1").
#' @param ci_method CI method for svyciprop. Default: "logit".
#' @param quintiles Numeric vector of quintiles to include. Default: 1:5.
#'
#' @return Tibble with additional column `wealth_quintile` indicating which
#'   quintile each estimate applies to. All other columns match
#'   `.compute_dhs_indicator_generic` output.
#' @noRd
.compute_dhs_indicator_by_wealth <- function(
  data,
  condition,
  group_var = NULL,
  subnational_level = NULL,
  ci_method = "logit",
  quintiles = 1:5
) {
  if (!"wealth_quintile" %in% names(data)) {
    cli::cli_abort(
      "Data must have 'wealth_quintile' column. Use .add_wealth_quintile() first."
    )
  }

  results <- purrr::map_dfr(quintiles, function(q) {
    subset_data <- data[data$wealth_quintile == q, , drop = FALSE]

    if (nrow(subset_data) == 0) {
      return(tibble::tibble())
    }

    quintile_result <- .compute_dhs_indicator_generic(
      data = subset_data,
      condition = condition,
      group_var = group_var,
      subnational_level = subnational_level,
      ci_method = ci_method
    )

    if (nrow(quintile_result) > 0) {
      quintile_result$wealth_quintile <- q
    }

    quintile_result
  })

  # Reorder columns to put wealth_quintile early
  if (nrow(results) > 0 && "wealth_quintile" %in% names(results)) {
    col_order <- c(
      "wealth_quintile",
      setdiff(names(results), "wealth_quintile")
    )
    results <- results[, col_order, drop = FALSE]
  }

  results
}


