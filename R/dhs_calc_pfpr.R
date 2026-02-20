#' Join DHS GPS Coordinates to Person Records Data
#'
#' Safely merges cluster-level GPS coordinates from a DHS Geographic dataset
#' onto Person Records (PR) data. All individuals within the same cluster will
#' receive the same coordinates.
#'
#' @details
#' Methodology: \url{https://github.com/ahadi-analytics/sntmethods/blob/master/inst/methods/pfpr_dhs.yml}
#'
#' @param pr_data DHS Person Records dataset (data.frame or tibble). Can be
#'   raw PR data or a processed subset that already includes `cluster_id`.
#' @param gps_data DHS GPS dataset (data.frame or tibble) containing cluster
#'   coordinates from the Geographic (GE) file.
#' @param pr_vars Named list specifying the cluster variable in PR data. Must
#'   include `cluster` if `cluster_id` column is not already present.
#' @param gps_vars Named list mapping GPS variable names. Must include:
#'   \itemize{
#'     \item `cluster`: Cluster ID variable (default: "DHSCLUST")
#'     \item `lat`: Latitude variable (default: "LATNUM")
#'     \item `lon`: Longitude variable (default: "LONGNUM")
#'   }
#'
#' @return PR dataset with `cluster_id`, `lat`, and `lon` columns added. Records
#'   without matching GPS coordinates will have NA values for lat/lon.
#'
#' @examples
#' pr_data <- tibble::tibble(hv001 = c(1, 1, 2), child = 1:3)
#' gps_data <- tibble::tibble(
#'   DHSCLUST = c(1, 2),
#'   LATNUM   = c(8.1, 7.9),
#'   LONGNUM  = c(-11.0, -10.8)
#' )
#'
#' join_dhs_coords(
#'   pr_data = pr_data,
#'   gps_data = gps_data,
#'   pr_vars = list(cluster = "hv001"),
#'   gps_vars = list(
#'     cluster = "DHSCLUST",
#'     lat = "LATNUM",
#'     lon = "LONGNUM"
#'   )
#' )
#'
#' @export
join_dhs_coords <- function(
  pr_data,
  gps_data,
  pr_vars = list(cluster = "hv001"),
  gps_vars = list(
    cluster = "DHSCLUST",
    lat = "LATNUM",
    lon = "LONGNUM"
  )
) {
  # Check inputs are data frames
  if (!is.data.frame(pr_data)) {
    cli::cli_abort("pr_data must be a data.frame or tibble")
  }
  if (!is.data.frame(gps_data)) {
    cli::cli_abort("gps_data must be a data.frame or tibble")
  }

  # Check for empty data
  if (nrow(pr_data) == 0) {
    cli::cli_abort("pr_data is empty")
  }
  if (nrow(gps_data) == 0) {
    cli::cli_abort("gps_data is empty")
  }

  # Required mapping in gps_vars
  needed_gps <- c("cluster", "lat", "lon")
  if (!all(needed_gps %in% names(gps_vars))) {
    cli::cli_abort("`gps_vars` must include: {needed_gps}.")
  }

  # Check GPS columns exist
  gps_cols <- unlist(gps_vars[needed_gps])
  missing_gps_cols <- setdiff(gps_cols, names(gps_data))
  if (length(missing_gps_cols) > 0) {
    cli::cli_abort(
      "GPS columns not found: {missing_gps_cols}. Check gps_vars mapping."
    )
  }

  # ensure cluster_id exists in pr_data
  if (!"cluster_id" %in% names(pr_data)) {
    if (!"cluster" %in% names(pr_vars)) {
      cli::cli_abort(
        "`pr_vars` must include `cluster` when `cluster_id` is missing."
      )
    }

    if (!pr_vars$cluster %in% names(pr_data)) {
      cli::cli_abort(
        "cluster variable `{pr_vars$cluster}` not found in pr dataset."
      )
    }

    pr_core <- pr_data |>
      dplyr::mutate(
        cluster_id = .data[[pr_vars$cluster]]
      )
  } else {
    pr_core <- pr_data
  }

  pr_core <- pr_core |>
    dplyr::relocate(
      cluster_id,
      .after = dplyr::last_col()
    )

  gps_core <- gps_data |>
    dplyr::transmute(
      cluster_id = .data[[gps_vars$cluster]],
      lat = as.numeric(.data[[gps_vars$lat]]),
      lon = as.numeric(.data[[gps_vars$lon]])
    ) |>
    dplyr::distinct(
      cluster_id,
      .keep_all = TRUE
    )

  out <- pr_core |>
    dplyr::left_join(
      gps_core,
      by = "cluster_id"
    )

  na_share <- mean(is.na(out$lat) | is.na(out$lon))

  if (is.finite(na_share) && na_share > 0.2) {
    cli::cli_alert_warning(
      "more than {round(100 * na_share, 1)}% of pr records lack coords ",
      "after pr to gps join."
    )
  }

  out
}

#' Calculate Core PfPR from DHS Data (Base Function)
#'
#' Core function that estimates PfPR among children aged 6-59 months using
#' standard DHS methodology. Supports both Rapid Diagnostic Test (RDT) and
#' microscopy results. Produces cluster-level estimates when GPS data are
#' provided, or aggregates to administrative levels when GPS data are not
#' available. Survey strata are automatically detected using administrative
#' and urban/rural variables when available.
#'
#' Note: Most users should use `calc_pfpr_dhs()` instead, which provides
#' additional spatial aggregation capabilities and data dictionary support.
#'
#' @param dhs_pr DHS Person Records dataset in tidy format (data.frame or
#'   tibble).
#' @param survey_vars Named list mapping DHS variable names. Required keys:
#'   \itemize{
#'     \item `cluster`: Primary sampling unit (default: "hv021"). Note: Use
#'       hv021 (PSU) for proper survey design, not hv001 (cluster number).
#'     \item `weight`: Survey weight (default: "hv005", divided by 1,000,000)
#'     \item `stratum`: Explicit stratum variable if available (default:
#'       "hv022")
#'     \item `adm1`: First administrative level (default: "hv024")
#'     \item `adm2`: Second administrative level (default: NULL)
#'     \item `age`: Child's age in months (default: "hc1")
#'     \item `present`: Present in household (1=yes, default: "hv103")
#'     \item `mother`: Mother listed in household (1=yes, default: "hv042")
#'     \item `rdt`: RDT result (0=negative, 1=positive, default: "hml35")
#'     \item `mic`: Microscopy result (0=neg, 1=pos, 6=other species,
#'       default: "hml32")
#'   }
#' @param gps_data Optional DHS GPS dataset. If provided, results are
#'   cluster-level.
#' @param gps_vars Named list for GPS variables (cluster, lat, lon).
#'
#' @return A tibble with PfPR estimates, confidence intervals, and sample sizes.
#'   Columns depend on whether GPS data is provided (cluster-level vs
#'   admin-level).
#'
#' @examples
#' # minimal example (structure only)
#' # pfpr <- calc_pfpr_dhs_core(
#' #   dhs_pr = pr_data,
#' #   survey_vars = list(
#' #     cluster = "hv021",
#' #     weight = "hv005",
#' #     stratum = "hv022",
#' #     adm1 = "hv024",
#' #     adm2 = NULL,
#' #     age = "hc1",
#' #     present = "hv103",
#' #     mother = "hv042",
#' #     rdt = "hml35",
#' #     mic = "hml32"
#' #   )
#' # )
#'
#' @export
calc_pfpr_dhs_core <- function(
  dhs_pr,
  survey_vars = list(
    cluster = "hv021",
    weight = "hv005",
    stratum = "hv022",
    adm1 = "hv024",
    adm2 = NULL,
    age = "hc1",
    present = "hv103",
    mother = "hv042",
    rdt = "hml35",
    mic = "hml32"
  ),
  gps_data = NULL,
  gps_vars = list(
    cluster = "DHSCLUST",
    lat = "LATNUM",
    lon = "LONGNUM"
  )
) {
  # Check input data
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
    "present",
    "mother",
    "rdt",
    "mic"
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

  # admin availability
  has_adm1 <- "adm1" %in%
    names(survey_vars) &&
    !is.na(survey_vars$adm1) &&
    survey_vars$adm1 %in% names(dhs_pr)

  has_adm2 <- "adm2" %in%
    names(survey_vars) &&
    !is.null(survey_vars$adm2) &&
    survey_vars$adm2 %in% names(dhs_pr)

  # ---- prepare base dataset -----------------------------------

  pr <- .prepare_pfpr_data(
    dhs_pr = dhs_pr,
    survey_vars = survey_vars,
    age_min = 6,
    age_max = 59,
    include_survey_vars = TRUE
  )

  use_strata <- dplyr::n_distinct(pr$stratum_id) > 1

  pr_rdt <- pr |>
    dplyr::filter(tested_rdt == 1)

  pr_mic <- pr |>
    dplyr::filter(tested_mic == 1)

  # ---- survey design ------------------------------------------

  if (use_strata) {
    des_rdt <- survey::svydesign(
      ids = ~cluster_id,
      strata = ~stratum_id,
      weights = ~survey_weight,
      data = pr_rdt,
      nest = TRUE
    )

    des_mic <- survey::svydesign(
      ids = ~cluster_id,
      strata = ~stratum_id,
      weights = ~survey_weight,
      data = pr_mic,
      nest = TRUE
    )
  } else {
    des_rdt <- survey::svydesign(
      ids = ~cluster_id,
      weights = ~survey_weight,
      data = pr_rdt,
      nest = TRUE
    )

    des_mic <- survey::svydesign(
      ids = ~cluster_id,
      weights = ~survey_weight,
      data = pr_mic,
      nest = TRUE
    )
  }

  # ---- grouping logic -----------------------------------------

  if (!is.null(gps_data)) {
    group_vars <- "cluster_id"
  } else if (has_adm2) {
    group_vars <- c("adm1", "adm2")
  } else if (has_adm1) {
    group_vars <- "adm1"
  } else {
    cli::cli_abort("no admin fields available for grouping.")
  }

  group_formula <- stats::as.formula(
    paste("~", paste(group_vars, collapse = " + "))
  )

  # ---- pfpr (rdt) ---------------------------------------------

  pfpr_rdt <- survey::svyby(
    ~rdt_pos,
    by = group_formula,
    design = des_rdt,
    FUN = survey::svymean,
    vartype = "ci",
    keep.names = FALSE
  ) |>
    tibble::as_tibble() |>
    dplyr::rename(
      dhs_pfpr_rdt = rdt_pos,
      dhs_pfpr_rdt_low = ci_l,
      dhs_pfpr_rdt_upp = ci_u
    ) |>
    dplyr::mutate(
      dhs_pfpr_rdt = round(dhs_pfpr_rdt, 2),
      dhs_pfpr_rdt_low = round(dhs_pfpr_rdt_low, 2),
      dhs_pfpr_rdt_upp = round(dhs_pfpr_rdt_upp, 2)
    )

  # ---- pfpr (mic) ---------------------------------------------

  pfpr_mic <- survey::svyby(
    ~mic_pos,
    by = group_formula,
    design = des_mic,
    FUN = survey::svymean,
    vartype = "ci",
    keep.names = FALSE
  ) |>
    tibble::as_tibble() |>
    dplyr::rename(
      dhs_pfpr_mic = mic_pos,
      dhs_pfpr_mic_low = ci_l,
      dhs_pfpr_mic_upp = ci_u
    ) |>
    dplyr::mutate(
      dhs_pfpr_mic = round(dhs_pfpr_mic, 2),
      dhs_pfpr_mic_low = round(dhs_pfpr_mic_low, 2),
      dhs_pfpr_mic_upp = round(dhs_pfpr_mic_upp, 2)
    )

  # ---- denominators -------------------------------------------

  denom_rdt <- survey::svyby(
    ~ tested_rdt + rdt_pos,
    by = group_formula,
    design = des_rdt,
    FUN = survey::svytotal,
    keep.names = TRUE
  ) |>
    tibble::as_tibble() |>
    dplyr::rename(
      dhs_n_tested_rdt = tested_rdt,
      dhs_n_pos_rdt = rdt_pos
    ) |>
    dplyr::mutate(
      dhs_n_tested_rdt = as.integer(round(dhs_n_tested_rdt)),
      dhs_n_pos_rdt = as.integer(round(dhs_n_pos_rdt))
    )

  denom_mic <- survey::svyby(
    ~ tested_mic + mic_pos,
    by = group_formula,
    design = des_mic,
    FUN = survey::svytotal,
    keep.names = TRUE
  ) |>
    tibble::as_tibble() |>
    dplyr::rename(
      dhs_n_tested_mic = tested_mic,
      dhs_n_pos_mic = mic_pos
    ) |>
    dplyr::mutate(
      dhs_n_tested_mic = as.integer(round(dhs_n_tested_mic)),
      dhs_n_pos_mic = as.integer(round(dhs_n_pos_mic))
    )

  # ---- merge results ------------------------------------------

  pfpr_final <- pfpr_rdt |>
    dplyr::left_join(
      pfpr_mic,
      by = group_vars
    ) |>
    dplyr::left_join(
      denom_rdt,
      by = group_vars
    ) |>
    dplyr::left_join(
      denom_mic,
      by = group_vars
    )

  # ---- attach gps coordinates ---------------------------------

  if (!is.null(gps_data)) {
    pfpr_final <- join_dhs_coords(
      pr_data = pfpr_final,
      gps_data = gps_data,
      pr_vars = list(cluster = "cluster_id"),
      gps_vars = gps_vars
    )
  }

  pfpr_final
}

#' Extract Metadata from DHS Dataset
#'
#' Internal function to extract survey metadata from DHS Person Records data.
#' Looks for standard DHS metadata columns and extracts key survey information.
#'
#' @param dhs_pr DHS Person Records dataset
#' @param survey_vars Named list of survey variable mappings
#'
#' @return A list containing survey metadata
#' @noRd
extract_dhs_metadata <- function(dhs_pr, survey_vars = NULL) {
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

  # Extract survey ID/phase (often in v000 or derived from it)
  if ("survey_id" %in% names(dhs_pr)) {
    metadata$survey_id <- unique(dhs_pr$survey_id)[1]
  } else if ("v000" %in% names(dhs_pr)) {
    # Survey ID is often embedded in v000 (e.g., "SL7" for Sierra Leone DHS 7)
    metadata$survey_id <- unique(dhs_pr$v000)[1]
  } else if ("hv000" %in% names(dhs_pr)) {
    metadata$survey_id <- unique(dhs_pr$hv000)[1]
  } else {
    metadata$survey_id <- NA_character_
  }

  # Extract survey type
  if ("survey_type" %in% names(dhs_pr)) {
    metadata$survey_type <- unique(dhs_pr$survey_type)[1]
  } else {
    # Infer survey type from available variables
    if (any(c("hml35", "hml32") %in% names(dhs_pr))) {
      metadata$survey_type <- "DHS"  # Standard DHS with malaria module
    } else if (any(c("sh418s", "sh418p") %in% names(dhs_pr))) {
      metadata$survey_type <- "MIS"  # Malaria Indicator Survey
    } else {
      metadata$survey_type <- "DHS"  # Default to DHS
    }
  }

  # Add file type
  metadata$file_type <- "PR"  # Person Records

  # Extract total sample sizes
  metadata$total_records <- nrow(dhs_pr)
  cluster_var <- if(!is.null(survey_vars$cluster)) survey_vars$cluster else "hv001"
  metadata$total_households <- length(unique(dhs_pr[[cluster_var]]))

  # Extract geographic coverage if admin variables are present
  if (!is.null(survey_vars$adm1) && survey_vars$adm1 %in% names(dhs_pr)) {
    admin1_values <- unique(dhs_pr[[survey_vars$adm1]])
    metadata$n_admin1_units <- length(admin1_values)
    metadata$admin1_coverage <- paste(sort(admin1_values), collapse = ", ")
  }

  # Add processing timestamp
  metadata$processed_date <- Sys.Date()
  metadata$processed_time <- Sys.time()

  # Add analysis type
  metadata$analysis_type <- "PfPR (Plasmodium falciparum Parasite Rate)"
  metadata$age_group <- "6-59 months"

  # Check which test types are available
  metadata$has_rdt <- "hml35" %in% names(dhs_pr) ||
    (!is.null(survey_vars$rdt) && survey_vars$rdt %in% names(dhs_pr))
  metadata$has_microscopy <- "hml32" %in% names(dhs_pr) ||
    (!is.null(survey_vars$mic) && survey_vars$mic %in% names(dhs_pr))

  # Add variable mapping info for transparency
  metadata$variable_mapping <- survey_vars

  return(metadata)
}

#' Calculate PfPR from DHS Data with Spatial Aggregation Support
#'
#' Main function for calculating Plasmodium falciparum Parasite Rate (PfPR)
#' from DHS data. Supports spatial aggregation using administrative boundary
#' shapefiles to calculate PfPR at cluster level or aggregate to any
#' administrative level (adm0, adm1, adm2, etc.). Returns both data and a
#' comprehensive data dictionary.
#'
#' @param dhs_pr DHS Person Records dataset in tidy format.
#' @param survey_vars Named list mapping DHS variable names. Required keys:
#'   \itemize{
#'     \item `cluster`: Primary sampling unit (default: "hv021"). Note: Use
#'       hv021 (PSU) for proper survey design, not hv001 (cluster number).
#'     \item `weight`: Survey weight (default: "hv005", divided by 1,000,000)
#'     \item `stratum`: Explicit stratum variable if available (default: "hv022")
#'     \item `adm1`: First administrative level (default: "hv024")
#'     \item `adm2`: Second administrative level (default: NULL)
#'     \item `age`: Child's age in months (default: "hc1")
#'     \item `present`: Present in household (1=yes, default: "hv103")
#'     \item `mother`: Mother listed in household (1=yes, default: "hv042")
#'     \item `rdt`: RDT result (0=negative, 1=positive, default: "hml35")
#'     \item `mic`: Microscopy result (0=neg, 1=pos, 6=other species,
#'       default: "hml32")
#'   }
#' @param gps_data Optional DHS GPS dataset with cluster coordinates.
#' @param gps_vars Named list for GPS variables (cluster, lat, lon).
#' @param shapefile Optional sf object with administrative boundaries. Must
#'   contain columns named "adm0", "adm1", "adm2", etc. for administrative
#'   levels.
#' @param admin_level Character vector specifying aggregation levels
#'   (e.g., `c("adm1", "adm2")`). If NULL, auto-detects available admin columns.
#' @param join_nearest Logical; if `TRUE`, assigns clusters outside all polygons
#'   to the nearest administrative unit. Useful for displaced DHS coordinates.
#'
#' @return A list containing three elements:
#'   \itemize{
#'     \item `data`: A tibble with PfPR estimates by administrative level if
#'       shapefile is provided, otherwise cluster-level results
#'     \item `dict`: A data dictionary created using `sntutils::build_dictionary()`
#'       describing all columns in the data
#'     \item `metadata`: A list containing survey metadata including:
#'       \itemize{
#'         \item `country_code`: DHS country code
#'         \item `survey_year`: Year of survey
#'         \item `survey_id`: DHS survey identifier
#'         \item `survey_type`: Type of survey (DHS, MIS, etc.)
#'         \item `file_type`: Type of DHS file (PR)
#'         \item `total_records`: Total number of records processed
#'         \item `total_households`: Number of unique households/clusters
#'         \item `analysis_type`: Type of analysis performed
#'         \item `age_group`: Age group analyzed
#'         \item `has_rdt`: Whether RDT data is available
#'         \item `has_microscopy`: Whether microscopy data is available
#'         \item `processed_date`: Date of processing
#'         \item Additional geographic and administrative information
#'       }
#'   }
#'
#' @examples
#' # Example with spatial aggregation
#' # pfpr_results <- calc_pfpr_dhs(
#' #   dhs_pr = pr_data,
#' #   gps_data = gps_data,
#' #   shapefile = admin_shapefile,
#' #   admin_level = c("adm1")
#' # )
#' #
#' # # Access the data
#' # pfpr_data <- pfpr_results$data
#' #
#' # # Access the dictionary
#' # pfpr_dict <- pfpr_results$dict
#' #
#' # # Access the metadata
#' # pfpr_metadata <- pfpr_results$metadata
#' # print(pfpr_metadata$country_code)
#' # print(pfpr_metadata$survey_year)
#'
#' @export
calc_pfpr_dhs <- function(
  dhs_pr,
  survey_vars = list(
    cluster = "hv021",
    weight = "hv005",
    stratum = "hv022",
    adm1 = "hv024",
    adm2 = NULL,
    age = "hc1",
    present = "hv103",
    mother = "hv042",
    rdt = "hml35",
    mic = "hml32"
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
  metadata <- extract_dhs_metadata(dhs_pr, survey_vars)

  cluster_results <- calc_pfpr_dhs_core(
    dhs_pr = dhs_pr,
    survey_vars = survey_vars,
    gps_data = gps_data,
    gps_vars = gps_vars
  )

  if (is.null(shapefile)) {
    dict <- sntutils::build_dictionary(cluster_results)
    dict <- .enrich_dhs_dictionary(dict, .pfpr_labels())
    return(list(
      data = cluster_results,
      dict = dict,
      metadata = metadata
    ))
  }

  if (!requireNamespace("sf", quietly = TRUE)) {
    cli::cli_abort(
      "package `sf` is required for spatial operations. please install it."
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
        "no admin columns (adm0, adm1, adm2, etc.) found in shapefile."
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
    dict <- .enrich_dhs_dictionary(dict, .pfpr_labels())
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
      dhs_pfpr_rdt = stats::weighted.mean(
        dhs_pfpr_rdt,
        w = dhs_n_tested_rdt,
        na.rm = TRUE
      ),
      dhs_pfpr_mic = stats::weighted.mean(
        dhs_pfpr_mic,
        w = dhs_n_tested_mic,
        na.rm = TRUE
      ),
      dhs_n_tested_rdt = sum(dhs_n_tested_rdt, na.rm = TRUE),
      dhs_n_pos_rdt = sum(dhs_n_pos_rdt, na.rm = TRUE),
      dhs_n_tested_mic = sum(dhs_n_tested_mic, na.rm = TRUE),
      dhs_n_pos_mic = sum(dhs_n_pos_mic, na.rm = TRUE),
      n_clusters = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      dhs_pfpr_rdt_se = sqrt(
        (dhs_pfpr_rdt / 100 * (1 - dhs_pfpr_rdt / 100)) / dhs_n_tested_rdt
      ) *
        100,
      dhs_pfpr_rdt_low = pmax(
        0,
        dhs_pfpr_rdt - 1.96 * dhs_pfpr_rdt_se
      ),
      dhs_pfpr_rdt_upp = pmin(
        100,
        dhs_pfpr_rdt + 1.96 * dhs_pfpr_rdt_se
      ),
      dhs_pfpr_mic_se = sqrt(
        (dhs_pfpr_mic / 100 * (1 - dhs_pfpr_mic / 100)) / dhs_n_tested_mic
      ) *
        100,
      dhs_pfpr_mic_low = pmax(
        0,
        dhs_pfpr_mic - 1.96 * dhs_pfpr_mic_se
      ),
      dhs_pfpr_mic_upp = pmin(
        100,
        dhs_pfpr_mic + 1.96 * dhs_pfpr_mic_se
      )
    ) |>
    dplyr::select(
      -dhs_pfpr_rdt_se,
      -dhs_pfpr_mic_se
    ) |>
    dplyr::mutate(
      dhs_pfpr_rdt = round(dhs_pfpr_rdt, 2),
      dhs_pfpr_rdt_low = round(dhs_pfpr_rdt_low, 2),
      dhs_pfpr_rdt_upp = round(dhs_pfpr_rdt_upp, 2),
      dhs_pfpr_mic = round(dhs_pfpr_mic, 2),
      dhs_pfpr_mic_low = round(dhs_pfpr_mic_low, 2),
      dhs_pfpr_mic_upp = round(dhs_pfpr_mic_upp, 2),
      dhs_n_tested_rdt = as.integer(dhs_n_tested_rdt),
      dhs_n_pos_rdt = as.integer(dhs_n_pos_rdt),
      dhs_n_tested_mic = as.integer(dhs_n_tested_mic),
      dhs_n_pos_mic = as.integer(dhs_n_pos_mic)
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
  dict <- .enrich_dhs_dictionary(dict, .pfpr_labels())

  list(
    data = dplyr::distinct(result_with_geometry),
    dict = dict,
    metadata = metadata
  )
}

#' PfPR label definitions
#' @noRd
.pfpr_labels <- function() {
  tibble::tribble(
    ~variable, ~label_en, ~label_fr, ~dhs_variable, ~numerator, ~denominator, ~dhs_numerator_var, ~dhs_denominator_var, ~dhs_recode, ~indicator_category, ~wmr_cascade_step, ~age_group, ~units, ~notes,
    "dhs_pfpr_rdt", "PfPR by RDT", "TIP par TDR", "hml35", "RDT-positive children", "Children 6-59 months tested by RDT", "hml35", "hml35", "PR", "Malaria", NA_integer_, "6-59 months", "proportion (0-1)", "PR module; children 6-59 months; measures test RESULT not test performed",
    "dhs_pfpr_rdt_low", "PfPR RDT - lower 95% CI", "TIP TDR - IC 95% inferieur", "hml35", NA_character_, NA_character_, NA_character_, NA_character_, "PR", "Malaria", NA_integer_, "6-59 months", "proportion (0-1)", "Survey-weighted 95% CI, clamped to [0,1]",
    "dhs_pfpr_rdt_upp", "PfPR RDT - upper 95% CI", "TIP TDR - IC 95% superieur", "hml35", NA_character_, NA_character_, NA_character_, NA_character_, "PR", "Malaria", NA_integer_, "6-59 months", "proportion (0-1)", "Survey-weighted 95% CI, clamped to [0,1]",
    "dhs_pfpr_mic", "PfPR by microscopy", "TIP par microscopie", "hml32", "Microscopy-positive children", "Children 6-59 months tested by microscopy", "hml32", "hml32", "PR", "Malaria", NA_integer_, "6-59 months", "proportion (0-1)", "PR module; children 6-59 months; measures test RESULT not test performed",
    "dhs_pfpr_mic_low", "PfPR microscopy - lower 95% CI", "TIP microscopie - IC 95% inferieur", "hml32", NA_character_, NA_character_, NA_character_, NA_character_, "PR", "Malaria", NA_integer_, "6-59 months", "proportion (0-1)", "Survey-weighted 95% CI, clamped to [0,1]",
    "dhs_pfpr_mic_upp", "PfPR microscopy - upper 95% CI", "TIP microscopie - IC 95% superieur", "hml32", NA_character_, NA_character_, NA_character_, NA_character_, "PR", "Malaria", NA_integer_, "6-59 months", "proportion (0-1)", "Survey-weighted 95% CI, clamped to [0,1]",
    "dhs_n_tested_rdt", "Number tested by RDT (denominator)", "Nombre testes par TDR (denominateur)", "hml35", NA_character_, NA_character_, NA_character_, NA_character_, "PR", "Malaria", NA_integer_, "6-59 months", "count", "Unweighted count",
    "dhs_n_pos_rdt", "Number RDT-positive (numerator)", "Nombre positifs au TDR (numerateur)", "hml35", NA_character_, NA_character_, NA_character_, NA_character_, "PR", "Malaria", NA_integer_, "6-59 months", "count", "Unweighted count",
    "dhs_n_tested_mic", "Number tested by microscopy (denominator)", "Nombre testes par microscopie (denominateur)", "hml32", NA_character_, NA_character_, NA_character_, NA_character_, "PR", "Malaria", NA_integer_, "6-59 months", "count", "Unweighted count",
    "dhs_n_pos_mic", "Number microscopy-positive (numerator)", "Nombre positifs a la microscopie (numerateur)", "hml32", NA_character_, NA_character_, NA_character_, NA_character_, "PR", "Malaria", NA_integer_, "6-59 months", "count", "Unweighted count"
  )
}

#' Aggregate Cluster-level PfPR to Administrative Levels
#'
#' Helper function to aggregate cluster-level PfPR results from
#'   `calc_pfpr_dhs_core()`
#' to administrative levels using a shapefile. Performs spatial joins and
#' calculates weighted or unweighted averages by administrative unit.
#'
#' @param cluster_results Cluster-level results from `calc_pfpr_dhs_core()`
#'   containing pfpr_rdt, pfpr_mic, n_tested_rdt, n_tested_mic, lat, and lon
#'   columns.
#' @param shapefile SF object with administrative boundaries containing columns
#'   named "adm0", "adm1", "adm2", etc.
#' @param admin_level Character vector of admin levels to aggregate to
#'   (e.g., `c("adm1")` or `c("adm1", "adm2")`).
#' @param weighted Logical. If `TRUE` (default), uses sample size weighted
#'   averaging. If `FALSE`, uses simple unweighted mean.
#'
#' @return SF object with aggregated PfPR by administrative level, including
#'   geometry for mapping.
#'
#' @export
aggregate_pfpr_admin <- function(
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
        dhs_pfpr_rdt = stats::weighted.mean(
          dhs_pfpr_rdt,
          w = dhs_n_tested_rdt,
          na.rm = TRUE
        ),
        dhs_pfpr_mic = stats::weighted.mean(
          dhs_pfpr_mic,
          w = dhs_n_tested_mic,
          na.rm = TRUE
        ),
        dhs_n_tested_rdt = sum(dhs_n_tested_rdt, na.rm = TRUE),
        dhs_n_pos_rdt = sum(dhs_n_pos_rdt, na.rm = TRUE),
        dhs_n_tested_mic = sum(dhs_n_tested_mic, na.rm = TRUE),
        dhs_n_pos_mic = sum(dhs_n_pos_mic, na.rm = TRUE),
        dhs_n_clusters = dplyr::n(),
        .groups = "drop"
      )
  } else {
    aggregated <- joined_df |>
      dplyr::group_by(
        dplyr::across(dplyr::all_of(admin_level))
      ) |>
      dplyr::summarise(
        dhs_pfpr_rdt = mean(dhs_pfpr_rdt, na.rm = TRUE),
        dhs_pfpr_mic = mean(dhs_pfpr_mic, na.rm = TRUE),
        dhs_n_tested_rdt = sum(dhs_n_tested_rdt, na.rm = TRUE),
        dhs_n_pos_rdt = sum(dhs_n_pos_rdt, na.rm = TRUE),
        dhs_n_tested_mic = sum(dhs_n_tested_mic, na.rm = TRUE),
        dhs_n_pos_mic = sum(dhs_n_pos_mic, na.rm = TRUE),
        dhs_n_clusters = dplyr::n(),
        .groups = "drop"
      )
  }

  aggregated <- aggregated |>
    dplyr::mutate(
      dhs_pfpr_rdt = round(dhs_pfpr_rdt, 2),
      dhs_pfpr_mic = round(dhs_pfpr_mic, 2),
      dhs_n_tested_rdt = as.integer(dhs_n_tested_rdt),
      dhs_n_pos_rdt = as.integer(dhs_n_pos_rdt),
      dhs_n_tested_mic = as.integer(dhs_n_tested_mic),
      dhs_n_pos_mic = as.integer(dhs_n_pos_mic)
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
