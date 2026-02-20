#' Calculate Severe Anemia Prevalence from DHS Data (Core Function)
#'
#' Core function that estimates severe anemia prevalence (Hb < 8.0 g/dL) among
#' children aged 6-59 months using standard DHS methodology. This indicator
#' represents clinically significant anemia requiring medical attention.
#'
#' Note: Most users should use `calc_severe_anemia_dhs()` instead, which
#' provides additional spatial aggregation capabilities and data dictionary
#' support.
#'
#' @details
#' Methodology: \url{https://github.com/ahadi-analytics/sntmethods/blob/master/inst/methods/anemia_dhs.yml}
#'
#' @param dhs_pr DHS Person Records dataset in tidy format (data.frame or
#'   tibble).
#' @param survey_vars Named list mapping DHS variable names. Required keys:
#'   \itemize{
#'     \item `cluster`: Cluster ID (default: "hv001")
#'     \item `weight`: Survey weight (default: "hv005", divided by 1,000,000)
#'     \item `stratum`: Explicit stratum variable if available (default: "hv022")
#'     \item `adm1`: First administrative level (default: "hv024")
#'     \item `adm2`: Second administrative level (default: NULL)
#'     \item `age`: Child's age in months (default: "hc1")
#'     \item `hemoglobin`: Raw hemoglobin in tenths of g/dL (default: "hc56")
#'     \item `hemoglobin_adj`: Altitude-adjusted hemoglobin (default: "hw53")
#'     \item `present`: Present in household (1=yes, default: "hv103")
#'     \item `mother`: Mother listed in household (1=yes, default: "hv042")
#'   }
#' @param hb_threshold Hemoglobin threshold in g/dL for severe anemia
#'   (default: 8.0). Children with Hb < threshold are classified as severely
#'   anemic.
#' @param altitude_adjusted Logical. If TRUE (default), uses altitude-adjusted
#'   hemoglobin variable (hw53). If FALSE, uses raw hemoglobin (hc56).
#'   WHO recommends altitude adjustment for surveys in regions above 1000m.
#' @param gps_data Optional DHS GPS dataset. If provided, results are
#'   cluster-level.
#' @param gps_vars Named list for GPS variables (cluster, lat, lon).
#'
#' @return A tibble with severe anemia estimates, confidence intervals, and
#'   sample sizes. Columns depend on whether GPS data is provided (cluster-level
#'   vs admin-level).
#'
#' @details
#' DHS stores hemoglobin values in tenths of g/dL (e.g., 80 = 8.0 g/dL).
#' The function handles this conversion automatically.
#'
#' Severe anemia (Hb < 8.0 g/dL) is clinically significant and typically
#' requires medical intervention. This differs from:
#' \itemize{
#'   \item Any anemia: Hb < 11.0 g/dL
#'   \item Moderate anemia: Hb 7.0-9.9 g/dL
#'   \item Mild anemia: Hb 10.0-10.9 g/dL
#' }
#'
#' \strong{Altitude Adjustment:}
#' The WHO recommends adjusting hemoglobin values for altitude to account
#' for physiological adaptation to lower oxygen at higher elevations. When
#' `altitude_adjusted = TRUE`, the function uses the pre-computed altitude-
#' adjusted variable (hw53) from DHS. This is particularly important for
#' surveys in highland areas.
#'
#' @examples
#' # minimal example (structure only)
#' # anemia <- calc_severe_anemia_dhs_core(
#' #   dhs_pr = pr_data,
#' #   altitude_adjusted = TRUE  # Use altitude-adjusted Hb (default)
#' # )
#' #
#' # # Use raw hemoglobin (no altitude adjustment)
#' # anemia <- calc_severe_anemia_dhs_core(
#' #   dhs_pr = pr_data,
#' #   altitude_adjusted = FALSE
#' # )
#'
#' @export
calc_severe_anemia_dhs_core <- function(
  dhs_pr,
  survey_vars = list(
    cluster = "hv001",
    weight = "hv005",
    stratum = "hv022",
    adm1 = "hv024",
    adm2 = NULL,
    age = "hc1",
    hemoglobin = "hc56",
    hemoglobin_adj = "hw53",
    present = "hv103",
    mother = "hv042"
  ),
  hb_threshold = 8.0,
  altitude_adjusted = TRUE,
  gps_data = NULL,
  gps_vars = list(
    cluster = "DHSCLUST",
    lat = "LATNUM",
    lon = "LONGNUM"
  )
) {
  # ---- 1. Input validation ------------------------------------------------

  if (!is.data.frame(dhs_pr)) {
    cli::cli_abort("dhs_pr must be a data.frame or tibble")
  }
  if (nrow(dhs_pr) == 0) {
    cli::cli_abort("dhs_pr is empty")
  }

  # Required mapping in survey_vars
  needed <- c(
    "cluster",
    "weight",
    "age",
    "hemoglobin",
    "present",
    "mother"
  )

  if (!all(needed %in% names(survey_vars))) {
    missing <- setdiff(needed, names(survey_vars))
    cli::cli_abort("`survey_vars` must include: {missing}")
  }

  # Check that mapped columns exist in data
  mapped_cols <- unlist(survey_vars[needed])
  mapped_cols <- mapped_cols[!is.null(mapped_cols)]
  missing_cols <- setdiff(mapped_cols, names(dhs_pr))

  if (length(missing_cols) > 0) {
    cli::cli_abort(
      "Columns not found in dhs_pr: {missing_cols}. Check survey_vars mapping."
    )
  }

  # Convert threshold to tenths (DHS stores Hb in tenths of g/dL)
  hb_threshold_tenths <- hb_threshold * 10

  # ---- 1b. Select hemoglobin variable based on altitude_adjusted -----------

  if (altitude_adjusted) {
    hb_var <- survey_vars$hemoglobin_adj %||% "hw53"
    if (!hb_var %in% names(dhs_pr)) {
      cli::cli_abort(
        c(
          "Altitude-adjusted hemoglobin variable `{hb_var}` not found in data.",
          "i" = "Set `altitude_adjusted = FALSE` to use raw hemoglobin ({survey_vars$hemoglobin})"
        )
      )
    }
    cli::cli_alert_info("Using altitude-adjusted hemoglobin: {hb_var}")
  } else {
    hb_var <- survey_vars$hemoglobin %||% "hc56"
    if (!hb_var %in% names(dhs_pr)) {
      cli::cli_abort("Raw hemoglobin variable `{hb_var}` not found in data.")
    }
    cli::cli_alert_info("Using raw hemoglobin (not altitude-adjusted): {hb_var}")
  }

  cli::cli_alert_info(
    "Using severe anemia threshold: Hb < {hb_threshold} g/dL ({hb_threshold_tenths} in DHS units)"
  )

  # Admin availability
  has_adm1 <- "adm1" %in% names(survey_vars) &&
    !is.na(survey_vars$adm1) &&
    survey_vars$adm1 %in% names(dhs_pr)

  has_adm2 <- "adm2" %in% names(survey_vars) &&
    !is.null(survey_vars$adm2) &&
    survey_vars$adm2 %in% names(dhs_pr)

  # ---- 2. Prepare base dataset --------------------------------------------

  # Override hemoglobin variable based on altitude_adjusted setting
  helper_survey_vars <- survey_vars
  helper_survey_vars$hemoglobin <- hb_var

  pr <- .prepare_anemia_data(
    dhs_pr = dhs_pr,
    survey_vars = helper_survey_vars,
    age_min = 6,
    age_max = 59,
    include_survey_vars = TRUE
  )

  use_strata <- dplyr::n_distinct(pr$stratum_id) > 1

  # ---- 3. Create severe anemia indicators ---------------------------------
  # The helper already filtered to eligible children with valid Hb and created

  # hemoglobin in g/dL. Now create tested_hb and severe_anemia for survey design.

  pr <- pr |>
    dplyr::mutate(
      tested_hb = 1,  # All rows from helper are eligible with valid Hb
      severe_anemia = as.numeric(hemoglobin < hb_threshold)
    )

  # Filter to tested children (all from helper are already tested)
  pr_tested <- pr

  n_severe <- sum(pr_tested$severe_anemia == 1, na.rm = TRUE)
  cli::cli_alert_info(
    "Found {format(nrow(pr_tested), big.mark = ',')} children with valid Hb measurements; {format(n_severe, big.mark = ',')} ({round(n_severe/nrow(pr_tested)*100, 1)}%) with severe anemia"
  )

  # ---- 5. Survey design ---------------------------------------------------

  # Handle single-PSU strata
  survey_options <- options(survey.lonely.psu = "certainty")
  on.exit(options(survey_options), add = TRUE)

  if (use_strata) {
    des <- survey::svydesign(
      ids = ~cluster_id,
      strata = ~stratum_id,
      weights = ~survey_weight,
      data = pr_tested,
      nest = TRUE
    )
  } else {
    des <- survey::svydesign(
      ids = ~cluster_id,
      weights = ~survey_weight,
      data = pr_tested,
      nest = TRUE
    )
  }

  # ---- 6. Grouping logic --------------------------------------------------

  if (!is.null(gps_data)) {
    group_vars <- "cluster_id"
  } else if (has_adm2) {
    group_vars <- c("adm1", "adm2")
  } else if (has_adm1) {
    group_vars <- "adm1"
  } else {
    cli::cli_abort("No admin fields available for grouping.")
  }

  group_formula <- stats::as.formula(
    paste("~", paste(group_vars, collapse = " + "))
  )

  # ---- 7. Calculate severe anemia prevalence ------------------------------

  anemia_results <- survey::svyby(
    ~severe_anemia,
    by = group_formula,
    design = des,
    FUN = survey::svymean,
    vartype = "ci",
    keep.names = FALSE
  ) |>
    tibble::as_tibble() |>
    dplyr::rename(
      dhs_severe_anemia = severe_anemia,
      dhs_severe_anemia_low = ci_l,
      dhs_severe_anemia_upp = ci_u
    ) |>
    dplyr::mutate(
      dhs_severe_anemia = round(dhs_severe_anemia, 2),
      dhs_severe_anemia_low = pmax(0, round(dhs_severe_anemia_low, 2)),
      dhs_severe_anemia_upp = pmin(1, round(dhs_severe_anemia_upp, 2))
    )

  # ---- 8. Calculate sample sizes ------------------------------------------

  denom <- survey::svyby(
    ~ tested_hb + severe_anemia,
    by = group_formula,
    design = des,
    FUN = survey::svytotal,
    keep.names = TRUE
  ) |>
    tibble::as_tibble() |>
    dplyr::rename(
      dhs_n_tested_hb = tested_hb,
      dhs_n_severe_anemia = severe_anemia
    ) |>
    dplyr::mutate(
      dhs_n_tested_hb = as.integer(round(dhs_n_tested_hb)),
      dhs_n_severe_anemia = as.integer(round(dhs_n_severe_anemia))
    )

  # ---- 9. Merge results ---------------------------------------------------

  anemia_final <- anemia_results |>
    dplyr::left_join(
      denom,
      by = group_vars
    )

  # ---- 10. Attach GPS coordinates if provided -----------------------------

  if (!is.null(gps_data)) {
    anemia_final <- join_dhs_coords(
      pr_data = anemia_final,
      gps_data = gps_data,
      pr_vars = list(cluster = "cluster_id"),
      gps_vars = gps_vars
    )
  }

  anemia_final
}

#' Extract Metadata from DHS Dataset for Anemia Analysis
#'
#' Internal function to extract survey metadata from DHS Person Records data
#' for anemia analysis.
#'
#' @param dhs_pr DHS Person Records dataset
#' @param survey_vars Named list of survey variable mappings
#' @param altitude_adjusted Logical indicating if altitude adjustment is used
#' @param hb_var Name of the hemoglobin variable used
#'
#' @return A list containing survey metadata
#' @noRd
extract_dhs_metadata_anemia <- function(
  dhs_pr,
  survey_vars = NULL,
  altitude_adjusted = TRUE,
  hb_var = NULL
) {
  metadata <- list()

  # Extract country code (v000 or hv000)
  if ("v000" %in% names(dhs_pr)) {
    metadata$country_code <- unique(dhs_pr$v000)[1]
  } else if ("hv000" %in% names(dhs_pr)) {
    metadata$country_code <- unique(dhs_pr$hv000)[1]
  } else if ("country_code" %in% names(dhs_pr)) {
    metadata$country_code <- unique(dhs_pr$country_code)[1]
  } else {
    metadata$country_code <- NA_character_
  }

  # Extract survey year (v007 or hv007)
  if ("v007" %in% names(dhs_pr)) {
    metadata$survey_year <- unique(dhs_pr$v007)[1]
  } else if ("hv007" %in% names(dhs_pr)) {
    metadata$survey_year <- unique(dhs_pr$hv007)[1]
  } else if ("survey_year" %in% names(dhs_pr)) {
    metadata$survey_year <- unique(dhs_pr$survey_year)[1]
  } else {
    metadata$survey_year <- NA_integer_
  }

  # Extract survey ID
  if ("survey_id" %in% names(dhs_pr)) {
    metadata$survey_id <- unique(dhs_pr$survey_id)[1]
  } else if ("v000" %in% names(dhs_pr)) {
    metadata$survey_id <- unique(dhs_pr$v000)[1]
  } else if ("hv000" %in% names(dhs_pr)) {
    metadata$survey_id <- unique(dhs_pr$hv000)[1]
  } else {
    metadata$survey_id <- NA_character_
  }

  metadata$survey_type <- "DHS"
  metadata$file_type <- "PR"

  metadata$total_records <- nrow(dhs_pr)

  # Count clusters
  cluster_var <- if (!is.null(survey_vars$cluster)) {
    survey_vars$cluster
  } else {
    "hv001"
  }

  if (cluster_var %in% names(dhs_pr)) {
    metadata$total_clusters <- length(unique(dhs_pr[[cluster_var]]))
  }

  # Count eligible children and those tested
  age_var <- if (!is.null(survey_vars$age)) survey_vars$age else "hc1"
  hb_var_for_tested <- if (!is.null(survey_vars$hemoglobin)) survey_vars$hemoglobin else "hc56"

  if (age_var %in% names(dhs_pr)) {
    eligible <- dhs_pr[[age_var]] >= 6 & dhs_pr[[age_var]] <= 59
    metadata$total_eligible_children <- sum(eligible, na.rm = TRUE)

    if (hb_var_for_tested %in% names(dhs_pr)) {
      tested <- eligible & !is.na(dhs_pr[[hb_var_for_tested]]) & dhs_pr[[hb_var_for_tested]] > 0
      metadata$total_tested <- sum(tested, na.rm = TRUE)
    }
  }

  metadata$processed_date <- Sys.Date()
  metadata$processed_time <- Sys.time()

  metadata$analysis_type <- "Severe Anemia (Hb < 8.0 g/dL)"
  metadata$age_group <- "6-59 months"
  metadata$indicator <- "Clinically significant anemia"

  # Altitude adjustment info
  metadata$altitude_adjusted <- altitude_adjusted
  if (!is.null(hb_var)) {
    metadata$hemoglobin_variable <- hb_var
  } else if (altitude_adjusted) {
    metadata$hemoglobin_variable <- survey_vars$hemoglobin_adj %||% "hw53"
  } else {
    metadata$hemoglobin_variable <- survey_vars$hemoglobin %||% "hc56"
  }

  # Check if hemoglobin variable is available
  metadata$has_hemoglobin <- metadata$hemoglobin_variable %in% names(dhs_pr)

  metadata$variable_mapping <- survey_vars

  metadata
}

#' Calculate Severe Anemia Prevalence from DHS Data with Spatial Aggregation
#'
#' Main function for calculating severe anemia prevalence (Hb < 8.0 g/dL)
#' from DHS data. Supports spatial aggregation using administrative boundary
#' shapefiles to calculate prevalence at cluster level or aggregate to any
#' administrative level (adm0, adm1, adm2, etc.). Returns both data and a
#' comprehensive data dictionary.
#'
#' @param dhs_pr DHS Person Records dataset in tidy format.
#' @param survey_vars Named list mapping DHS variable names. Required keys:
#'   \itemize{
#'     \item `cluster`: Cluster ID (default: "hv001")
#'     \item `weight`: Survey weight (default: "hv005", divided by 1,000,000)
#'     \item `stratum`: Explicit stratum variable if available (default: "hv022")
#'     \item `adm1`: First administrative level (default: "hv024")
#'     \item `adm2`: Second administrative level (default: NULL)
#'     \item `age`: Child's age in months (default: "hc1")
#'     \item `hemoglobin`: Raw hemoglobin in tenths of g/dL (default: "hc56")
#'     \item `hemoglobin_adj`: Altitude-adjusted hemoglobin (default: "hw53")
#'     \item `present`: Present in household (1=yes, default: "hv103")
#'     \item `mother`: Mother listed in household (1=yes, default: "hv042")
#'   }
#' @param hb_threshold Hemoglobin threshold in g/dL for severe anemia
#'   (default: 8.0). Children with Hb < threshold are classified as severely
#'   anemic.
#' @param altitude_adjusted Logical. If TRUE (default), uses altitude-adjusted
#'   hemoglobin variable (hw53). If FALSE, uses raw hemoglobin (hc56).
#'   WHO recommends altitude adjustment for surveys in regions above 1000m.
#' @param gps_data Optional DHS GPS dataset with cluster coordinates.
#' @param gps_vars Named list for GPS variables (cluster, lat, lon).
#' @param shapefile Optional sf object with administrative boundaries. Must
#'   contain columns named "adm0", "adm1", "adm2", etc. for administrative
#'   levels.
#' @param admin_level Character vector specifying aggregation levels
#'   (e.g., `c("adm1", "adm2")`). If NULL, auto-detects available admin columns.
#' @param join_nearest Logical; if `TRUE`, assigns clusters outside all polygons
#'   to the nearest administrative unit.
#'
#' @return A list containing three elements:
#'   \itemize{
#'     \item `data`: A tibble with severe anemia estimates by administrative
#'       level if shapefile is provided, otherwise cluster-level results
#'     \item `dict`: A data dictionary created using `sntutils::build_dictionary()`
#'       describing all columns in the data
#'     \item `metadata`: A list containing survey metadata
#'   }
#'
#' @details
#' Severe anemia (Hb < 8.0 g/dL) is clinically significant and typically
#' requires medical intervention. This is an important malaria-related indicator
#' as severe malaria often causes severe anemia in young children.
#'
#' DHS stores hemoglobin values in tenths of g/dL (e.g., 80 = 8.0 g/dL).
#' The function handles this conversion automatically.
#'
#' @examples
#' # Example with spatial aggregation
#' # anemia_results <- calc_severe_anemia_dhs(
#' #   dhs_pr = pr_data,
#' #   gps_data = gps_data,
#' #   shapefile = admin_shapefile,
#' #   admin_level = c("adm1")
#' # )
#' #
#' # # Access the data
#' # anemia_data <- anemia_results$data
#' #
#' # # Access the dictionary
#' # anemia_dict <- anemia_results$dict
#' #
#' # # Access the metadata
#' # anemia_metadata <- anemia_results$metadata
#'
#' @export
calc_severe_anemia_dhs <- function(
  dhs_pr,
  survey_vars = list(
    cluster = "hv001",
    weight = "hv005",
    stratum = "hv022",
    adm1 = "hv024",
    adm2 = NULL,
    age = "hc1",
    hemoglobin = "hc56",
    hemoglobin_adj = "hw53",
    present = "hv103",
    mother = "hv042"
  ),
  hb_threshold = 8.0,
  altitude_adjusted = TRUE,
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
  metadata <- extract_dhs_metadata_anemia(
    dhs_pr,
    survey_vars,
    altitude_adjusted = altitude_adjusted
  )

  cluster_results <- calc_severe_anemia_dhs_core(
    dhs_pr = dhs_pr,
    survey_vars = survey_vars,
    hb_threshold = hb_threshold,
    altitude_adjusted = altitude_adjusted,
    gps_data = gps_data,
    gps_vars = gps_vars
  )

  if (is.null(shapefile)) {
    dict <- sntutils::build_dictionary(cluster_results)
    dict <- .enrich_dhs_dictionary(dict, .severe_anemia_labels())
    return(list(
      data = cluster_results,
      dict = dict,
      metadata = metadata
    ))
  }

  if (!requireNamespace("sf", quietly = TRUE)) {
    cli::cli_abort(
      "Package `sf` is required for spatial operations. Please install it."
    )
  }

  if (!inherits(shapefile, "sf")) {
    cli::cli_abort("`shapefile` must be an sf object.")
  }

  cluster_sf <- cluster_results |>
    sf::st_as_sf(
      coords = c("lon", "lat"),
      crs = 4326,
      remove = FALSE
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
        "No admin columns (adm0, adm1, adm2, etc.) found in shapefile."
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
      "Admin columns not found in shapefile: {paste(missing_cols, collapse = ', ')}"
    )
  }

  joined <- sf::st_join(
    cluster_sf,
    shapefile[, c(admin_level, "geometry")],
    join = sf::st_within,
    left = TRUE
  )

  if (join_nearest) {
    unmatched <- is.na(joined[[admin_level[1]]])

    if (any(unmatched)) {
      cli::cli_alert_info(
        "Assigning {format(sum(unmatched), big.mark = ',')} clusters to nearest admin units."
      )

      nearest_idx <- sf::st_nearest_feature(
        joined[unmatched, ],
        shapefile
      )

      for (col in admin_level) {
        joined[unmatched, col] <- shapefile[[col]][nearest_idx]
      }
    }
  }

  if (length(admin_level) == 0) {
    dict <- sntutils::build_dictionary(joined)
    dict <- .enrich_dhs_dictionary(dict, .severe_anemia_labels())
    return(list(
      data = joined,
      dict = dict,
      metadata = metadata
    ))
  }

  joined_df <- sf::st_drop_geometry(joined)

  aggregated <- joined_df |>
    dplyr::group_by(
      dplyr::across(dplyr::all_of(admin_level))
    ) |>
    dplyr::summarise(
      dhs_severe_anemia = stats::weighted.mean(
        dhs_severe_anemia,
        w = dhs_n_tested_hb,
        na.rm = TRUE
      ),
      dhs_n_tested_hb = sum(dhs_n_tested_hb, na.rm = TRUE),
      dhs_n_severe_anemia = sum(dhs_n_severe_anemia, na.rm = TRUE),
      n_clusters = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      dhs_severe_anemia_se = sqrt(
        (dhs_severe_anemia * (1 - dhs_severe_anemia)) / dhs_n_tested_hb
      ),
      dhs_severe_anemia_low = pmax(
        0,
        dhs_severe_anemia - 1.96 * dhs_severe_anemia_se
      ),
      dhs_severe_anemia_upp = pmin(
        1,
        dhs_severe_anemia + 1.96 * dhs_severe_anemia_se
      )
    ) |>
    dplyr::select(-dhs_severe_anemia_se) |>
    dplyr::mutate(
      dhs_severe_anemia = round(dhs_severe_anemia, 2),
      dhs_severe_anemia_low = round(dhs_severe_anemia_low, 2),
      dhs_severe_anemia_upp = round(dhs_severe_anemia_upp, 2),
      dhs_n_tested_hb = as.integer(dhs_n_tested_hb),
      dhs_n_severe_anemia = as.integer(dhs_n_severe_anemia)
    )

  # Detect and preserve admin name columns
  admin_name_cols <- paste0(admin_level, "_name")
  admin_name_cols <- admin_name_cols[admin_name_cols %in% names(shapefile)]
  all_admin_cols <- c(admin_level, admin_name_cols)

  result_with_geometry <- shapefile |>
    dplyr::select(dplyr::all_of(all_admin_cols)) |>
    dplyr::distinct() |>
    dplyr::left_join(
      aggregated,
      by = admin_level
    ) |>
    dplyr::select(-n_clusters) |>
    sf::st_drop_geometry()

  dict <- sntutils::build_dictionary(result_with_geometry)
  dict <- .enrich_dhs_dictionary(dict, .severe_anemia_labels())

  list(
    data = dplyr::distinct(result_with_geometry),
    dict = dict,
    metadata = metadata
  )
}

#' Severe anemia label definitions
#' @noRd
.severe_anemia_labels <- function() {
  tibble::tribble(
    ~variable, ~label_en, ~label_fr, ~dhs_variable, ~numerator, ~denominator, ~dhs_numerator_var, ~dhs_denominator_var, ~dhs_recode, ~indicator_category, ~wmr_cascade_step, ~age_group, ~units, ~notes,
    "dhs_severe_anemia", "Severe anemia prevalence", "Prevalence de l'anemie severe", "hw53 (or hc56)", "Children with Hb < 8.0 g/dL", "Children 6-59 months tested for Hb", "hw53/hc56", "hw53/hc56", "PR", "Nutrition", NA_integer_, "6-59 months", "proportion (0-1)", "PR module; Hb < 8.0 g/dL; altitude-adjusted (hw53) preferred over raw (hc56)",
    "dhs_severe_anemia_low", "Severe anemia - lower 95% CI", "Anemie severe - IC 95% inferieur", "hw53 (or hc56)", NA_character_, NA_character_, NA_character_, NA_character_, "PR", "Nutrition", NA_integer_, "6-59 months", "proportion (0-1)", "Survey-weighted 95% CI, clamped to [0,1]",
    "dhs_severe_anemia_upp", "Severe anemia - upper 95% CI", "Anemie severe - IC 95% superieur", "hw53 (or hc56)", NA_character_, NA_character_, NA_character_, NA_character_, "PR", "Nutrition", NA_integer_, "6-59 months", "proportion (0-1)", "Survey-weighted 95% CI, clamped to [0,1]",
    "dhs_n_tested_hb", "Number tested for hemoglobin (denominator)", "Nombre testes pour l'hemoglobine (denominateur)", "hw53 (or hc56)", NA_character_, NA_character_, NA_character_, NA_character_, "PR", "Nutrition", NA_integer_, "6-59 months", "count", "Unweighted count",
    "dhs_n_severe_anemia", "Number with severe anemia (numerator)", "Nombre avec anemie severe (numerateur)", "hw53 (or hc56)", NA_character_, NA_character_, NA_character_, NA_character_, "PR", "Nutrition", NA_integer_, "6-59 months", "count", "Unweighted count"
  )
}

#' Aggregate Cluster-level Severe Anemia to Administrative Levels
#'
#' Helper function to aggregate cluster-level severe anemia results from
#' `calc_severe_anemia_dhs_core()` to administrative levels using a shapefile.
#' Performs spatial joins and calculates weighted or unweighted averages by
#' administrative unit.
#'
#' @param cluster_results Cluster-level results from `calc_severe_anemia_dhs_core()`
#'   containing dhs_severe_anemia, dhs_n_tested_hb, lat, and lon columns.
#' @param shapefile SF object with administrative boundaries containing columns
#'   named "adm0", "adm1", "adm2", etc.
#' @param admin_level Character vector of admin levels to aggregate to
#'   (e.g., `c("adm1")` or `c("adm1", "adm2")`).
#' @param weighted Logical. If `TRUE` (default), uses sample size weighted
#'   averaging. If `FALSE`, uses simple unweighted mean.
#'
#' @return SF object with aggregated severe anemia by administrative level,
#'   including geometry for mapping.
#'
#' @export
aggregate_severe_anemia_admin <- function(
  cluster_results,
  shapefile,
  admin_level = c("adm1"),
  weighted = TRUE
) {
  if (!inherits(cluster_results, "sf")) {
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

  if (weighted) {
    aggregated <- joined_df |>
      dplyr::group_by(
        dplyr::across(dplyr::all_of(admin_level))
      ) |>
      dplyr::summarise(
        dhs_severe_anemia = stats::weighted.mean(
          dhs_severe_anemia,
          w = dhs_n_tested_hb,
          na.rm = TRUE
        ),
        dhs_n_tested_hb = sum(dhs_n_tested_hb, na.rm = TRUE),
        dhs_n_severe_anemia = sum(dhs_n_severe_anemia, na.rm = TRUE),
        dhs_n_clusters = dplyr::n(),
        .groups = "drop"
      )
  } else {
    aggregated <- joined_df |>
      dplyr::group_by(
        dplyr::across(dplyr::all_of(admin_level))
      ) |>
      dplyr::summarise(
        dhs_severe_anemia = mean(dhs_severe_anemia, na.rm = TRUE),
        dhs_n_tested_hb = sum(dhs_n_tested_hb, na.rm = TRUE),
        dhs_n_severe_anemia = sum(dhs_n_severe_anemia, na.rm = TRUE),
        dhs_n_clusters = dplyr::n(),
        .groups = "drop"
      )
  }

  aggregated <- aggregated |>
    dplyr::mutate(
      dhs_severe_anemia = round(dhs_severe_anemia, 1),
      dhs_n_tested_hb = as.integer(dhs_n_tested_hb),
      dhs_n_severe_anemia = as.integer(dhs_n_severe_anemia)
    )

  # Detect and preserve admin name columns
  admin_name_cols <- paste0(admin_level, "_name")
  admin_name_cols <- admin_name_cols[admin_name_cols %in% names(shapefile)]
  all_admin_cols <- c(admin_level, admin_name_cols)

  result_with_geometry <- shapefile |>
    dplyr::select(dplyr::all_of(all_admin_cols)) |>
    dplyr::distinct() |>
    dplyr::left_join(
      aggregated,
      by = admin_level
    )

  result_with_geometry
}
