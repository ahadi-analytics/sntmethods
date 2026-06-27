# pfpr indicator
#
# Merged from: dhs_calc_pfpr.R dhs_calc_pfpr_mbg.R dhs_helpers_pfpr.R
# Contains the survey-weighted calc, MBG cluster-prep, and indicator-
# specific helpers for this family.

# ---- dhs_calc_pfpr.R ----

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
#' @keywords internal
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
  if (is.null(pr)) return(NULL)

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

#' Calculate PfPR Indicators from DHS Data
#'
#' Computes PfPR (Plasmodium falciparum Parasite Rate) indicators from DHS
#' Person Records (PR) data. Returns survey-weighted proportions with logit
#' confidence intervals in standardized long format.
#'
#' @details
#' Computes PfPR for children aged 6-59 months using RDT (hml35) and/or
#' microscopy (hml32) results. Follows the same output pattern as
#' \code{\link{calc_act_dhs}} and \code{\link{calc_itn_dhs}}.
#'
#' @param dhs_pr DHS Person Records (PR) dataset (data.frame or tibble).
#' @param survey_vars Named list mapping DHS variable names. Required keys:
#'   \itemize{
#'     \item \code{cluster}: PSU ID (default: "hv021")
#'     \item \code{weight}: Survey weight (default: "hv005")
#'     \item \code{stratum}: Stratum variable (default: "hv022")
#'     \item \code{adm1}: Admin-1 variable (default: "hv024")
#'     \item \code{age}: Child's age in months (default: "hc1")
#'     \item \code{present}: Present in household (default: "hv103")
#'     \item \code{mother}: Mother listed in household (default: "hv042")
#'     \item \code{rdt}: RDT result (default: "hml35")
#'     \item \code{mic}: Microscopy result (default: "hml32")
#'   }
#' @param region_var Optional column name for subnational grouping
#'   (e.g., "hv024").
#' @param gps_data Optional DHS GE dataset with cluster coordinates.
#' @param gps_vars Named list for GE variables: cluster, lat, lon.
#' @param shapefile Optional sf object with administrative boundaries.
#' @param admin_level Character vector of admin columns from shapefile.
#' @param join_nearest Logical; if TRUE, assigns unmatched clusters to nearest
#'   admin unit. Default: TRUE.
#' @param indicators Character vector of indicator names to compute. If NULL
#'   (default), computes all indicators from \code{\link{pfpr_dictionary}}.
#' @param ci_method Method for confidence intervals. Default: "logit".
#'
#' @return Named list of tibbles, one per admin level:
#'   \describe{
#'     \item{\code{adm0}}{National-level estimates (always present)}
#'     \item{\code{adm1}}{Admin-1 estimates (when region_var or shapefile)}
#'     \item{\code{adm2}}{Admin-2 estimates (when shapefile with adm2)}
#'   }
#'   Each tibble has standard columns: survey_id, iso3, iso2,
#'   survey_type, survey_year, adm0, type, geo_source, point, ci_l, ci_u,
#'   numerator, denominator, indicator, indicator_code,
#'   numerator_description, denominator_description, denominator_code.
#'
#' @examples
#' \dontrun{
#' result <- calc_pfpr_dhs(dhs_pr = pr_data)
#' result$adm0  # national PfPR estimates
#'
#' # With subnational
#' result <- calc_pfpr_dhs(dhs_pr = pr_data, region_var = "hv024")
#' result$adm1  # regional PfPR
#' }
#'
#' @seealso \code{\link{pfpr_dictionary}} for indicator definitions
#' @export
calc_pfpr_dhs <- function(
  dhs_pr,
  survey_vars = list(
    cluster = "hv021",
    weight  = "hv005",
    stratum = "hv022",
    adm1    = "hv024",
    adm2    = NULL,
    age     = "hc1",
    present = "hv103",
    mother  = "hv042",
    rdt     = "hml35",
    mic     = "hml32"
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
  indicators   = NULL,
  ci_method    = "logit"
) {
  # Fail fast on missing suggested dependencies
  .check_pkg(
    c("purrr", "stringr", "tibble"),
    reason = "for `calc_pfpr_dhs()`"
  )

  # ---- 1. Input validation ----

  if (!is.data.frame(dhs_pr)) {
    cli::cli_abort("`dhs_pr` must be a data.frame or tibble.")
  }
  if (nrow(dhs_pr) == 0) {
    cli::cli_abort("`dhs_pr` is empty.")
  }

  # ---- 1b. Extract survey metadata ----
  survey_meta <- .extract_survey_meta_hv(dhs_pr)

  # ---- 2. Prepare PfPR data ----
  # Use wide age range (0-119 months) so all age-group variants are available.
  # Individual indicators apply their own age filters via .pfpr_conditions().

  pr <- .prepare_pfpr_data(
    dhs_pr           = dhs_pr,
    survey_vars      = survey_vars,
    age_min          = 0,
    age_max          = 119,
    include_survey_vars = TRUE
  )
  if (is.null(pr)) {
    cli::cli_abort("No valid PfPR data after preparation.")
  }

  has_rdt <- any(pr$tested_rdt == 1, na.rm = TRUE)
  has_mic <- any(pr$tested_mic == 1, na.rm = TRUE)

  n_rdt <- sum(pr$tested_rdt == 1, na.rm = TRUE)
  n_mic <- sum(pr$tested_mic == 1, na.rm = TRUE)
  cli::cli_alert_info(
    "Eligible children (0-119 months): {n_rdt} tested by RDT, {n_mic} by microscopy"
  )

  # ---- 3. Region grouping or spatial join ----

  admin_hierarchy <- list()
  geo_src <- NA_character_

  # Auto-fallback to hv024
  region_hr_var <- survey_vars$adm1 %||% "hv024"
  if (is.null(region_var) && is.null(gps_data) && is.null(shapefile)) {
    if (region_hr_var %in% names(dhs_pr)) {
      region_var <- region_hr_var
      cli::cli_alert_info(
        "No region_var/GPS/shapefile specified; defaulting to \\
        {.var {region_hr_var}} for adm1"
      )
    }
  }

  if (!is.null(region_var)) {
    # Resolve region labels
    pr$region <- .resolve_region_labels(
      dhs_pr[[region_var]], region_var
    )
    # Map to febrile subset if lengths differ
    if (nrow(pr) != nrow(dhs_pr)) {
      # pr is a subset -- need lookup approach
      resolved_all <- .resolve_region_labels(
        dhs_pr[[region_var]], region_var
      )
      raw_all <- as.character(
        as.vector(haven::zap_labels(dhs_pr[[region_var]]))
      )
      lookup <- stats::setNames(resolved_all, raw_all)
      pr_raw <- as.character(pr[[region_var]])
      pr$region <- unname(lookup[pr_raw])
    }

    admin_hierarchy <- list(
      list(group_var = "region", level_name = "adm1")
    )
    geo_src <- "survey"
    cli::cli_alert_info(
      "Grouping by {.var {region_var}}: \\
      {paste(unique(pr$region), collapse = ', ')}"
    )

  } else if (!is.null(gps_data) && !is.null(shapefile)) {
    pr <- .spatial_join_ge(
      kr_fever     = pr,
      gps_data     = gps_data,
      gps_vars     = gps_vars,
      shapefile    = shapefile,
      admin_level  = admin_level,
      join_nearest = join_nearest
    )
    geo_src <- "gps"
    admin_lvls <- attr(pr, "admin_levels") %||% character(0)
    # Drop adm0 from hierarchy: meta_cols$adm0 already carries the country.
    admin_lvls <- setdiff(admin_lvls, "adm0")
    if ("adm0" %in% names(pr)) {
      pr <- dplyr::select(pr, -dplyr::any_of("adm0"))
    }
    for (lvl in admin_lvls) {
      admin_hierarchy <- c(admin_hierarchy, list(
        list(group_var = lvl, level_name = lvl)
      ))
    }
  }

  # ---- 4. Get indicator conditions and filter ----

  dict <- .pfpr_conditions()

  # Skip indicators without data
  if (!has_rdt) {
    dict <- dict[vapply(
      dict, function(d) d$outcome_var != "rdt_pos", logical(1)
    )]
  }
  if (!has_mic) {
    dict <- dict[vapply(
      dict, function(d) d$outcome_var != "mic_pos", logical(1)
    )]
  }

  if (!is.null(indicators)) {
    valid <- vapply(dict, function(d) d$indicator, character(1))
    bad <- setdiff(indicators, valid)
    if (length(bad) > 0) {
      cli::cli_warn(
        "Unknown indicators ignored: {paste(bad, collapse = ', ')}"
      )
    }
    dict <- dict[vapply(
      dict, function(d) d$indicator %in% indicators, logical(1)
    )]
  }

  if (length(dict) == 0) {
    cli::cli_abort("No valid indicators to compute.")
  }

  # ---- 5. Compute each indicator ----

  options(survey.lonely.psu = "adjust")

  if (is.na(geo_src)) {
    geo_src <- if (!is.null(gps_data) && !is.null(shapefile)) {
      "gps"
    } else {
      "survey"
    }
  }

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

  # --- adm0 (national) ---
  national_results <- purrr::map_dfr(dict, function(cond) {
    .compute_pfpr_indicator(
      data      = pr,
      condition = cond,
      group_var = NULL,
      ci_method = ci_method
    )
  })

  national_results <- .round_results(national_results)

  adm0_tbl <- dplyr::bind_cols(
    meta_cols[rep(1, nrow(national_results)), ],
    tibble::tibble(type = "survey_weighted", geo_source = geo_src),
    national_results |> dplyr::select(-level, -location)
  ) |>
    tibble::as_tibble()

  out <- list(adm0 = adm0_tbl)

  # --- subnational tabs ---
  all_level_names <- vapply(
    admin_hierarchy, `[[`, character(1), "level_name"
  )

  for (i in seq_along(admin_hierarchy)) {
    ah <- admin_hierarchy[[i]]
    grp <- ah$group_var
    lvl_name <- ah$level_name

    sub_results <- purrr::map_dfr(dict, function(cond) {
      .compute_pfpr_indicator(
        data              = pr,
        condition         = cond,
        group_var         = grp,
        subnational_level = lvl_name,
        ci_method         = ci_method
      )
    })

    sub_results <- sub_results |>
      dplyr::filter(level != "adm0")

    if (nrow(sub_results) == 0) next

    sub_results <- .round_results(sub_results)
    sub_results <- sub_results |>
      dplyr::mutate(!!lvl_name := toupper(location))

    # Parent admin columns
    parent_levels <- all_level_names[seq_len(i - 1)]
    parent_cols_in_data <- intersect(parent_levels, names(pr))
    if (length(parent_cols_in_data) > 0 && grp %in% names(pr)) {
      parent_lookup <- pr |>
        dplyr::select(dplyr::all_of(c(grp, parent_cols_in_data))) |>
        dplyr::mutate(
          dplyr::across(dplyr::everything(), ~toupper(as.character(.)))
        ) |>
        dplyr::distinct()
      sub_results <- sub_results |>
        dplyr::left_join(
          parent_lookup, by = stats::setNames(grp, lvl_name)
        )
    }

    admin_cols <- c(
      intersect(parent_cols_in_data, names(sub_results)), lvl_name
    )

    sub_tbl <- dplyr::bind_cols(
      meta_cols[rep(1, nrow(sub_results)), ],
      sub_results |>
        dplyr::select(
          dplyr::all_of(admin_cols), point, ci_l, ci_u,
          numerator, denominator,
          indicator, indicator_code,
          numerator_description,
          denominator_description, denominator_code
        )
    ) |>
      dplyr::mutate(
        type       = "survey_weighted",
        geo_source = geo_src,
        .after     = dplyr::all_of(lvl_name)
      ) |>
      tibble::as_tibble()

    out[[lvl_name]] <- sub_tbl
  }

  out
}


# =============================================================================
# PfPR Indicator Dictionary
# =============================================================================

#' PfPR Indicator Dictionary
#'
#' Returns the dictionary of PfPR indicators.
#'
#' @return Tibble with columns: indicator, indicator_code, indicator_title,
#'   outcome_var, numerator_description, denominator_description,
#'   denominator_code, data_level.
#'
#' @keywords internal
#' @noRd
pfpr_dictionary <- function() {
  conds <- .pfpr_conditions()
  tibble::tibble(
    indicator = vapply(conds, `[[`, character(1), "indicator"),
    indicator_code = vapply(conds, `[[`, character(1), "indicator_code"),
    indicator_title = vapply(conds, `[[`, character(1), "indicator_title"),
    outcome_var = vapply(conds, `[[`, character(1), "outcome_var"),
    numerator_description = vapply(conds, `[[`, character(1), "num_desc"),
    denominator_description = vapply(conds, `[[`, character(1), "denom_desc"),
    denominator_code = vapply(conds, `[[`, character(1), "denom_code"),
    data_level = "adm0"
  )
}


#' Internal: PfPR indicator conditions
#'
#' Each condition specifies a filter expression applied to the prepared PR data.
#' The `age` column (child's age in months, from hc1) is available for age-group
#' subsetting. Age-group naming follows the MBG convention:
#'   - (default): 6-59 months
#'   - u5:  0-59 months
#'   - 5_10: 60-119 months
#'   - u10: 0-119 months
#'   - 2_10: 24-119 months
#'
#' @noRd
.pfpr_conditions <- function() {
  list(

    # ---- RDT: 6-59 months (default / original) ----
    list(
      indicator       = "PFPR_RDT",
      indicator_code  = "pfpr_rdt",
      indicator_title = "PfPR by RDT (6-59 months)",
      denom_code      = "tested_rdt_6_59mo",
      filter_expr     = quote(tested_rdt == 1 & age >= 6 & age <= 59),
      outcome_var     = "rdt_pos",
      num_desc        = "Children 6-59 months positive by RDT",
      denom_desc      = "Children 6-59 months tested by RDT"
    ),

    # ---- MIC: 6-59 months (default / original) ----
    list(
      indicator       = "PFPR_MIC",
      indicator_code  = "pfpr_mic",
      indicator_title = "PfPR by microscopy (6-59 months)",
      denom_code      = "tested_mic_6_59mo",
      filter_expr     = quote(tested_mic == 1 & age >= 6 & age <= 59),
      outcome_var     = "mic_pos",
      num_desc        = "Children 6-59 months positive by microscopy",
      denom_desc      = "Children 6-59 months tested by microscopy"
    ),

    # ---- RDT: under 5 (0-59 months) ----
    list(
      indicator       = "PFPR_RDT_U5",
      indicator_code  = "pfpr_rdt_u5",
      indicator_title = "PfPR by RDT (0-59 months)",
      denom_code      = "tested_rdt_u5",
      filter_expr     = quote(tested_rdt == 1 & age >= 0 & age <= 59),
      outcome_var     = "rdt_pos",
      num_desc        = "Children 0-59 months positive by RDT",
      denom_desc      = "Children 0-59 months tested by RDT"
    ),

    # ---- RDT: 5-10 years (60-119 months) ----
    list(
      indicator       = "PFPR_RDT_5_10",
      indicator_code  = "pfpr_rdt_5_10",
      indicator_title = "PfPR by RDT (60-119 months)",
      denom_code      = "tested_rdt_5_10",
      filter_expr     = quote(tested_rdt == 1 & age >= 60 & age <= 119),
      outcome_var     = "rdt_pos",
      num_desc        = "Children 60-119 months positive by RDT",
      denom_desc      = "Children 60-119 months tested by RDT"
    ),

    # ---- RDT: under 10 (0-119 months) ----
    list(
      indicator       = "PFPR_RDT_U10",
      indicator_code  = "pfpr_rdt_u10",
      indicator_title = "PfPR by RDT (0-119 months)",
      denom_code      = "tested_rdt_u10",
      filter_expr     = quote(tested_rdt == 1 & age >= 0 & age <= 119),
      outcome_var     = "rdt_pos",
      num_desc        = "Children 0-119 months positive by RDT",
      denom_desc      = "Children 0-119 months tested by RDT"
    ),

    # ---- RDT: 2-10 years (24-119 months) ----
    list(
      indicator       = "PFPR_RDT_2_10",
      indicator_code  = "pfpr_rdt_2_10",
      indicator_title = "PfPR by RDT (24-119 months)",
      denom_code      = "tested_rdt_2_10",
      filter_expr     = quote(tested_rdt == 1 & age >= 24 & age <= 119),
      outcome_var     = "rdt_pos",
      num_desc        = "Children 24-119 months positive by RDT",
      denom_desc      = "Children 24-119 months tested by RDT"
    ),

    # ---- MIC: under 5 (0-59 months) ----
    list(
      indicator       = "PFPR_MIC_U5",
      indicator_code  = "pfpr_mic_u5",
      indicator_title = "PfPR by microscopy (0-59 months)",
      denom_code      = "tested_mic_u5",
      filter_expr     = quote(tested_mic == 1 & age >= 0 & age <= 59),
      outcome_var     = "mic_pos",
      num_desc        = "Children 0-59 months positive by microscopy",
      denom_desc      = "Children 0-59 months tested by microscopy"
    ),

    # ---- MIC: 5-10 years (60-119 months) ----
    list(
      indicator       = "PFPR_MIC_5_10",
      indicator_code  = "pfpr_mic_5_10",
      indicator_title = "PfPR by microscopy (60-119 months)",
      denom_code      = "tested_mic_5_10",
      filter_expr     = quote(tested_mic == 1 & age >= 60 & age <= 119),
      outcome_var     = "mic_pos",
      num_desc        = "Children 60-119 months positive by microscopy",
      denom_desc      = "Children 60-119 months tested by microscopy"
    ),

    # ---- MIC: under 10 (0-119 months) ----
    list(
      indicator       = "PFPR_MIC_U10",
      indicator_code  = "pfpr_mic_u10",
      indicator_title = "PfPR by microscopy (0-119 months)",
      denom_code      = "tested_mic_u10",
      filter_expr     = quote(tested_mic == 1 & age >= 0 & age <= 119),
      outcome_var     = "mic_pos",
      num_desc        = "Children 0-119 months positive by microscopy",
      denom_desc      = "Children 0-119 months tested by microscopy"
    ),

    # ---- MIC: 2-10 years (24-119 months) ----
    list(
      indicator       = "PFPR_MIC_2_10",
      indicator_code  = "pfpr_mic_2_10",
      indicator_title = "PfPR by microscopy (24-119 months)",
      denom_code      = "tested_mic_2_10",
      filter_expr     = quote(tested_mic == 1 & age >= 24 & age <= 119),
      outcome_var     = "mic_pos",
      num_desc        = "Children 24-119 months positive by microscopy",
      denom_desc      = "Children 24-119 months tested by microscopy"
    )
  )
}


# =============================================================================
# PfPR computation helper
# =============================================================================

#' Compute a single PfPR indicator
#'
#' @param data Prepared PfPR dataset with test result columns.
#' @param condition List with indicator spec (filter_expr, outcome_var, etc.).
#' @param group_var Optional grouping variable for regional estimates.
#' @param subnational_level Admin level name for grouped rows.
#' @param ci_method CI method for svyciprop.
#' @return Tibble with level, location, point, ci_l, ci_u, numerator,
#'   denominator, indicator metadata.
#' @noRd
.compute_pfpr_indicator <- function(data, condition, group_var = NULL,
                                     subnational_level = NULL,
                                     ci_method = "logit") {

  outcome_var <- condition$outcome_var
  ind_title <- condition$indicator_title

  # Apply filter
  if (is.null(condition$filter_expr)) {
    filtered <- data
  } else {
    filtered <- tryCatch(
      dplyr::filter(data, !!condition$filter_expr),
      error = function(e) tibble::tibble()
    )
  }

  if (!outcome_var %in% names(filtered)) {
    return(tibble::tibble())
  }

  filtered$.dhs_outcome <- filtered[[outcome_var]]
  filtered <- filtered[!is.na(filtered$.dhs_outcome), ]

  n_denom <- nrow(filtered)
  if (n_denom == 0) return(tibble::tibble())

  # Survey design
  n_clusters <- dplyr::n_distinct(filtered$cluster_id)
  use_strata <- dplyr::n_distinct(filtered$stratum_id) > 1

  # Weighted counts
  n_denom_w <- round(sum(filtered$survey_weight, na.rm = TRUE))
  n_numer_w <- round(sum(
    filtered$survey_weight * (filtered$.dhs_outcome == 1), na.rm = TRUE
  ))

  # Single-cluster guard
  if (n_clusters < 2) {
    point_est <- n_numer_w / n_denom_w
    national <- tibble::tibble(
      level = "adm0", location = "National",
      point = point_est, ci_l = NA_real_, ci_u = NA_real_,
      numerator = n_numer_w, denominator = n_denom_w
    )
    return(
      national |>
        dplyr::mutate(
          indicator               = condition$indicator_title,
          indicator_code          = condition$indicator_code,
          numerator_description   = condition$num_desc,
          denominator_description = condition$denom_desc,
          denominator_code        = condition$denom_code
        )
    )
  }

  svy <- tryCatch({
    if (use_strata) {
      survey::svydesign(
        ids = ~cluster_id, strata = ~stratum_id,
        weights = ~survey_weight, data = filtered, nest = TRUE
      )
    } else {
      survey::svydesign(
        ids = ~cluster_id, weights = ~survey_weight,
        data = filtered, nest = TRUE
      )
    }
  }, error = function(e) {
    survey::svydesign(
      ids = ~cluster_id, weights = ~survey_weight,
      data = filtered, nest = TRUE
    )
  })

  # National estimate
  national <- tryCatch({
    est <- survey::svyciprop(~.dhs_outcome, svy, method = ci_method,
                              na.rm = TRUE)
    ci  <- stats::confint(est)
    tibble::tibble(
      level = "adm0", location = "National",
      point = as.numeric(est), ci_l = ci[1], ci_u = ci[2],
      numerator = n_numer_w, denominator = n_denom_w
    )
  }, error = function(e) {
    cli::cli_alert_warning("    {ind_title} national: {e$message}")
    tibble::tibble(
      level = "adm0", location = "National", point = NA_real_,
      ci_l = NA_real_, ci_u = NA_real_,
      numerator = n_numer_w, denominator = n_denom_w
    )
  })

  # Regional estimates
  regional <- tibble::tibble()
  sub_level <- subnational_level %||% "adm1"

  if (!is.null(group_var) && group_var %in% names(filtered)) {
    group_formula <- stats::as.formula(paste("~", group_var))

    regional <- tryCatch({
      by_result <- survey::svyby(
        ~.dhs_outcome, by = group_formula, design = svy,
        FUN = survey::svyciprop, vartype = "ci",
        method = ci_method, na.rm = TRUE, keep.names = FALSE
      ) |> tibble::as_tibble()

      region_num <- filtered |>
        dplyr::group_by(.data[[group_var]]) |>
        dplyr::summarise(
          numerator = round(sum(
            survey_weight * (.dhs_outcome == 1), na.rm = TRUE
          )), .groups = "drop"
        )

      region_denom <- filtered |>
        dplyr::group_by(.data[[group_var]]) |>
        dplyr::summarise(
          denominator = round(sum(survey_weight, na.rm = TRUE)),
          .groups = "drop"
        )

      names(by_result)[names(by_result) == "ci_l"] <- "ci_l..dhs_outcome"
      names(by_result)[names(by_result) == "ci_u"] <- "ci_u..dhs_outcome"

      by_result |>
        dplyr::rename(
          location = !!group_var, point = .dhs_outcome,
          ci_l = `ci_l..dhs_outcome`, ci_u = `ci_u..dhs_outcome`
        ) |>
        dplyr::mutate(location = as.character(location)) |>
        dplyr::left_join(
          region_num |>
            dplyr::mutate(
              location = as.character(.data[[group_var]])
            ) |>
            dplyr::select(location, numerator),
          by = "location"
        ) |>
        dplyr::left_join(
          region_denom |>
            dplyr::mutate(
              location = as.character(.data[[group_var]])
            ) |>
            dplyr::select(location, denominator),
          by = "location"
        ) |>
        dplyr::mutate(level = sub_level) |>
        dplyr::select(
          level, location, point, ci_l, ci_u, numerator, denominator
        )
    }, error = function(e) {
      cli::cli_alert_warning("    {ind_title} by group: {e$message}")
      tibble::tibble()
    })
  }

  dplyr::bind_rows(national, regional) |>
    dplyr::mutate(
      indicator               = condition$indicator_title,
      indicator_code          = condition$indicator_code,
      numerator_description   = condition$num_desc,
      denominator_description = condition$denom_desc,
      denominator_code        = condition$denom_code
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
#' @keywords internal
#' @noRd
aggregate_pfpr_admin <- function(
  cluster_results,
  shapefile,
  admin_level = c("adm1"),
  weighted = TRUE
) {
  # sf is used unconditionally below
  .check_spatial_pkg("sf", "aggregate_pfpr_admin")

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


# ---- dhs_calc_pfpr_mbg.R ----

#' Prepare PfPR Data for MBG Analysis
#'
#' Prepares cluster-level malaria parasite prevalence data for Model-Based
#' Geostatistics (MBG) analysis. Uses a dictionary-driven approach matching
#' the indicator codes from \code{\link{calc_pfpr_dhs}}.
#'
#' @details
#' All dictionary-based indicators share the same data preparation pipeline
#' via \code{.prepare_pfpr_data()}, the same shared helper used by the
#' survey-weighted \code{calc_pfpr_dhs()} function. Positivity definitions
#' are identical:
#' \itemize{
#'   \item RDT positive: \code{rdt_res == 1} (hml35 == 1)
#'   \item Microscopy positive: \code{mic_res == 1} (hml32 == 1, Pf only)
#'   \item Either: positive on RDT OR microscopy
#' }
#'
#' Unlike the survey-weighted function, this uses simple cluster-level counts
#' because MBG handles spatial smoothing and uncertainty internally.
#'
#' Methodology: \url{https://github.com/ahadi-analytics/sntmethods/blob/master/inst/methods/pfpr_dhs.yml}
#'
#' @param dhs_pr DHS Person Records dataset (data.frame or tibble).
#' @param gps_data DHS GPS dataset with cluster coordinates.
#' @param indicators Character vector of indicator codes to calculate.
#'   Available codes (\code{<test>} = \code{rdt} or \code{mic}):
#'   \itemize{
#'     \item \code{"pfpr_<test>"}: 6-59 months (standard DHS PfPR)
#'     \item \code{"pfpr_<test>_u5"}: 0-59 months
#'     \item \code{"pfpr_<test>_5_10"}: 60-119 months
#'     \item \code{"pfpr_<test>_u10"}: 0-119 months
#'     \item \code{"pfpr_<test>_2_10"}: 24-119 months (PfPR2-10 reference)
#'   }
#'   Default: all indicators from the dictionary.
#' @param test_type \strong{Deprecated}. Character. Use \code{indicators}
#'   instead. When provided, translated to indicator codes for backward
#'   compatibility. One of \code{"rdt"}, \code{"mic"}, \code{"both"},
#'   or \code{"either"}.
#' @param age_groups \strong{Deprecated}. Named list of age ranges. Use
#'   \code{indicators} instead. When provided alongside \code{test_type},
#'   translated to indicator codes.
#' @param survey_vars Named list mapping DHS variable names. Required keys:
#'   \itemize{
#'     \item cluster: Cluster ID (default: "hv001")
#'     \item age: Age in months (default: "hc1")
#'     \item present: Present in household (default: "hv103")
#'     \item mother: Mother listed in household (default: "hv042")
#'     \item rdt: RDT result variable (default: "hml35")
#'     \item mic: Microscopy result variable (default: "hml32")
#'   }
#' @param gps_vars Named list for GPS variable mapping.
#'
#' @return A named list of data.tables (one per indicator), each with columns:
#'   \itemize{
#'     \item cluster_id: Cluster identifier
#'     \item indicator: Number of positive tests (numerator for MBG)
#'     \item samplesize: Number of children tested (denominator for MBG)
#'     \item x: Longitude
#'     \item y: Latitude
#'   }
#'
#' @examples
#' \dontrun{
#' # New-style: specify exact indicator codes
#' pfpr_mbg <- calc_pfpr_mbg(
#'   dhs_pr = pr_data,
#'   gps_data = gps_data,
#'   indicators = c("pfpr_rdt_u5", "pfpr_mic_u5")
#' )
#'
#' # Legacy style (still works, with deprecation warning)
#' pfpr_mbg <- calc_pfpr_mbg(
#'   dhs_pr = pr_data,
#'   gps_data = gps_data,
#'   test_type = "rdt",
#'   age_groups = list(u5 = c(6, 59))
#' )
#' }
#'
#' @seealso [calc_pfpr_dhs()] for survey-weighted estimates
#' @export
calc_pfpr_mbg <- function(
  dhs_pr,
  gps_data,
  indicators = NULL,
  test_type = NULL,
  age_groups = NULL,
  survey_vars = list(
    cluster = "hv001",
    age = "hc1",
    present = "hv103",
    mother = "hv042",
    rdt = "hml35",
    mic = "hml32"
  ),
  gps_vars = list(
    cluster = "DHSCLUST",
    lat = "LATNUM",
    lon = "LONGNUM"
  )
) {
  # ---- Input validation ----

  if (!is.data.frame(dhs_pr)) {
    cli::cli_abort("`dhs_pr` must be a data.frame or tibble")
  }
  if (!is.data.frame(gps_data)) {
    cli::cli_abort("`gps_data` must be a data.frame or tibble")
  }

  # ---- Resolve indicators ----

  dict <- .pfpr_mbg_dictionary()
  dict_names <- vapply(dict, `[[`, character(1), "name")

  if (!is.null(test_type) || !is.null(age_groups)) {
    # Legacy backward-compat: translate test_type + age_groups to codes.
    # Suppress the deprecation warning when called internally from
    # prep_pfpr_mbg(), which exposes the legacy form as a documented API.
    suppress_warn <- isTRUE(getOption("sntmethods.pfpr_mbg.suppress_legacy_warn"))
    if (!suppress_warn) {
      cli::cli_alert_warning(
        "{.arg test_type}/{.arg age_groups} are deprecated; use {.arg indicators} with specific codes instead"
      )
    }
    indicators <- .pfpr_legacy_to_codes(test_type, age_groups)
  }

  if (is.null(indicators)) {
    # Default: all dictionary indicators
    indicators <- dict_names
  }

  invalid <- setdiff(indicators, dict_names)
  if (length(invalid) > 0) {
    cli::cli_abort(
      "Invalid indicators: {.val {invalid}}. Valid codes: {.val {dict_names}}"
    )
  }

  # ---- Prepare GPS data ----

  gps_clean <- .prepare_gps_data(gps_data, gps_vars)

  # ---- Prepare PR data ----
  # Use very wide age range; age filtering happens per indicator in the loop

  pr <- .prepare_pfpr_data(
    dhs_pr, survey_vars,
    age_min = 0, age_max = 999,
    include_survey_vars = FALSE
  )

  if (is.null(pr)) return(NULL)

  # ---- Dictionary-driven indicator loop ----

  # Filter dictionary to requested indicators
  dict_specs <- dict[vapply(dict, function(d) d$name %in% indicators, logical(1))]

  results <- list()

  for (spec in dict_specs) {
    # Age filter + eligibility (present, mother)
    age_data <- pr[
      pr$present == 1 &
      pr$mother == 1 &
      pr$age >= spec$age_min &
      pr$age <= spec$age_max, ,
      drop = FALSE
    ]

    if (nrow(age_data) == 0) {
      cli::cli_alert_warning(
        "{spec$name}: no eligible children in {spec$age_min}-{spec$age_max} month range"
      )
      next
    }

    # Test-specific filtering and positivity
    if (spec$test_type == "either") {
      # Either: tested on at least one test (RDT or microscopy)
      age_data <- age_data[
        age_data$rdt_res %in% c(0, 1) | age_data$mic_res %in% c(0, 1, 6), ,
        drop = FALSE
      ]
      if (nrow(age_data) == 0) next
      age_data$positive <- as.integer(
        (!is.na(age_data$rdt_res) & age_data$rdt_res == 1L) |
        (!is.na(age_data$mic_res) & age_data$mic_res == 1L)
      )
    } else {
      # RDT or microscopy: filter to valid test results
      test_col <- spec$test_col
      valid_values <- spec$valid_values
      pos_value <- spec$pos_value

      if (!test_col %in% names(age_data)) next
      age_data <- age_data[
        age_data[[test_col]] %in% valid_values, ,
        drop = FALSE
      ]
      if (nrow(age_data) == 0) {
        cli::cli_alert_warning(
          "{spec$name}: no tested individuals - skipping"
        )
        next
      }
      age_data$positive <- as.integer(age_data[[test_col]] == pos_value)
    }

    dt <- .aggregate_to_mbg_clusters(
      individual_data = age_data,
      indicator_col = "positive",
      gps_clean = gps_clean,
      result_name = spec$name
    )
    if (!is.null(dt)) {
      results[[spec$name]] <- dt
    }
  }

  # ---- Filter redundant age groups ----

  # Build age_groups list from dictionary for redundancy check
  age_groups_from_dict <- stats::setNames(
    lapply(dict_specs, function(d) c(d$age_min, d$age_max)),
    vapply(dict_specs, `[[`, character(1), "name")
  )
  results <- .filter_redundant_mbg_results(results, age_groups_from_dict)

  if (length(results) == 0) {
    cli::cli_warn(
      "No valid PfPR data could be prepared; {.var {survey_vars$rdt}} and {.var {survey_vars$mic}} may be absent from this survey"
    )
    return(NULL)
  }

  results
}


# =============================================================================
# PfPR MBG Indicator Dictionary
# =============================================================================

#' PfPR MBG Indicator Dictionary
#'
#' Returns the full set of standardized indicator specifications for
#' cluster-level MBG output. Generates all test type x age group combinations.
#' Mirrors the DHS \code{.pfpr_conditions()} definitions.
#'
#' @details
#' Test types:
#' \itemize{
#'   \item \code{rdt}: RDT positive (\code{rdt_res == 1}, i.e., hml35 == 1)
#'   \item \code{mic}: Microscopy positive (\code{mic_res == 1}, i.e.,
#'     hml32 == 1, Pf only)
#'   \item \code{either}: Positive on RDT OR microscopy
#' }
#'
#' Age groups:
#' \itemize{
#'   \item (unsuffixed): 6-59 months (standard DHS PfPR)
#'   \item \code{u5}: 0-59 months
#'   \item \code{5_10}: 60-119 months
#'   \item \code{u10}: 0-119 months
#'   \item \code{2_10}: 24-119 months (standard PfPR reference range)
#' }
#'
#' @return List of named lists, each with fields: \code{name},
#'   \code{test_type}, \code{test_col}, \code{pos_value},
#'   \code{valid_values}, \code{age_min}, \code{age_max}.
#'
#' @noRd
.pfpr_mbg_dictionary <- function() {
  # Canonical age windows (months), matching .pfpr_conditions() in
  # dhs_calc_pfpr.R so MBG and survey-weighted indicators stay aligned.
  age_windows <- list(
    list(suffix = "",      age_min = 6,  age_max = 59),  # standard 6-59
    list(suffix = "u5",    age_min = 0,  age_max = 59),  # 0-59 (DHS u5)
    list(suffix = "5_10",  age_min = 60, age_max = 119), # 60-119
    list(suffix = "u10",   age_min = 0,  age_max = 119), # 0-119
    list(suffix = "2_10",  age_min = 24, age_max = 119)  # 24-119 (PfPR2-10)
  )

  test_specs <- list(
    rdt = list(test_col = "rdt_res", pos_value = 1, valid_values = c(0, 1)),
    mic = list(test_col = "mic_res", pos_value = 1, valid_values = c(0, 1, 6))
  )

  dict <- list()
  for (tt in names(test_specs)) {
    ts <- test_specs[[tt]]
    for (aw in age_windows) {
      name <- if (nzchar(aw$suffix)) {
        paste0("pfpr_", tt, "_", aw$suffix)
      } else {
        paste0("pfpr_", tt)
      }
      dict[[length(dict) + 1L]] <- list(
        name         = name,
        test_type    = tt,
        test_col     = ts$test_col,
        pos_value    = ts$pos_value,
        valid_values = ts$valid_values,
        age_min      = aw$age_min,
        age_max      = aw$age_max
      )
    }
  }

  dict
}


# =============================================================================
# Legacy backward-compatibility translator
# =============================================================================

#' Translate Legacy test_type + age_groups to Indicator Codes
#'
#' Converts the old-style \code{test_type} and \code{age_groups} parameters
#' into the new indicator code format used by the dictionary.
#'
#' @param test_type Character: "rdt", "mic", "both", or "either".
#' @param age_groups Named list of age ranges. If NULL, uses default ages.
#'
#' @return Character vector of indicator codes.
#'
#' @noRd
.pfpr_legacy_to_codes <- function(test_type = NULL, age_groups = NULL) {
  test_type <- test_type %||% "both"
  test_type <- match.arg(test_type, c("rdt", "mic", "both", "either"))

  # Build a lookup of (test_type, age_min, age_max) -> registered code from
  # the canonical dictionary, so legacy translation can only emit codes that
  # actually exist. This prevents silent fabrication of unregistered names
  # like `pfpr_mic_24_119`.
  dict <- .pfpr_mbg_dictionary()
  dict_keys <- vapply(dict, function(d) {
    paste(d$test_type, d$age_min, d$age_max, sep = "_")
  }, character(1))
  dict_names <- vapply(dict, `[[`, character(1), "name")
  dict_lookup <- stats::setNames(dict_names, dict_keys)

  # Default age groups mirror the canonical .pfpr_conditions() ranges
  if (is.null(age_groups)) {
    age_groups <- list(
      u5     = c(0, 59),
      `5_10` = c(60, 119),
      u10    = c(0, 119),
      `2_10` = c(24, 119)
    )
  }

  # Map test_type to test prefixes (drop "either" - not in MBG dictionary)
  test_prefixes <- switch(test_type,
    rdt    = "rdt",
    mic    = "mic",
    either = character(0),
    both   = c("rdt", "mic")
  )

  if (test_type == "either") {
    cli::cli_alert_warning(
      "{.val either} is not supported by the MBG dictionary; ignoring."
    )
  }

  codes <- character(0)
  unmapped <- character(0)

  # Include the unsuffixed standard code (6-59 months) when the user's age
  # set includes that range, so callers asking for the default ranges still
  # get the canonical pfpr_rdt / pfpr_mic indicators.
  has_default_6_59 <- any(vapply(age_groups, function(x) {
    length(x) == 2 && x[1] == 6 && x[2] == 59
  }, logical(1)))

  for (tp in test_prefixes) {
    if (has_default_6_59) {
      key <- paste(tp, 6, 59, sep = "_")
      if (!is.na(dict_lookup[key])) codes <- c(codes, unname(dict_lookup[key]))
    }
    for (ag in age_groups) {
      if (length(ag) != 2) next
      key <- paste(tp, ag[1], ag[2], sep = "_")
      if (!is.na(dict_lookup[key])) {
        codes <- c(codes, unname(dict_lookup[key]))
      } else {
        unmapped <- c(unmapped, paste0("pfpr_", tp, " ", ag[1], "-", ag[2]))
      }
    }
  }

  if (length(unmapped) > 0) {
    cli::cli_alert_warning(
      "Skipping legacy combinations not in MBG dictionary: {.val {unique(unmapped)}}"
    )
  }

  unique(codes)
}


# =============================================================================
# Redundancy detection helpers
# =============================================================================

#' Filter Redundant MBG Results
#'
#' Detects and removes redundant age groups from MBG results. Two results are
#' considered redundant if they have identical cluster data (same cluster_ids
#' with same indicator and samplesize values). When redundant pairs are found,
#' the more specific age group (narrower age range) is kept.
#'
#' @param results Named list of data.tables from calc_pfpr_mbg
#' @param age_groups Named list of age ranges used to generate results
#'
#' @return Filtered list with redundant results removed
#'
#' @keywords internal
#' @noRd
.filter_redundant_mbg_results <- function(results, age_groups) {
  if (length(results) <= 1) {
    return(results)
  }

  result_names <- names(results)
  to_remove <- character(0)

  # Group results by test type (rdt, mic, or either)
  # Match both suffixed (pfpr_rdt_u5) and unsuffixed (pfpr_rdt) variants
  rdt_results <- result_names[grepl("_rdt(_|$)", result_names)]
  mic_results <- result_names[grepl("_mic(_|$)", result_names)]
  either_results <- result_names[grepl("_either(_|$)", result_names)]

  # Check for redundancy within each test type
  for (test_results in list(rdt_results, mic_results, either_results)) {
    if (length(test_results) <= 1) next

    # Pairwise comparison
    for (i in seq_len(length(test_results) - 1)) {
      for (j in (i + 1):length(test_results)) {
        name_i <- test_results[i]
        name_j <- test_results[j]

        # Skip if already marked for removal
        if (name_i %in% to_remove || name_j %in% to_remove) next

        dt_i <- results[[name_i]]
        dt_j <- results[[name_j]]

        # Check if identical
        if (.are_mbg_results_identical(dt_i, dt_j)) {
          # Determine which to keep (narrower age range)
          range_i <- age_groups[[name_i]]
          range_j <- age_groups[[name_j]]

          # Handle case where age group not found in age_groups
          if (is.null(range_i) || is.null(range_j)) next

          span_i <- range_i[2] - range_i[1]
          span_j <- range_j[2] - range_j[1]

          if (span_i <= span_j) {
            # Keep i (narrower), remove j
            to_remove <- c(to_remove, name_j)
            cli::cli_alert_warning(
              "'{name_j}' skipped - identical to '{name_i}' ",
              "(no data in {range_j[1]}-{range_j[2]} month range outside {range_i[1]}-{range_i[2]})"
            )
          } else {
            # Keep j (narrower), remove i
            to_remove <- c(to_remove, name_i)
            cli::cli_alert_warning(
              "'{name_i}' skipped - identical to '{name_j}' ",
              "(no data in {range_i[1]}-{range_i[2]} month range outside {range_j[1]}-{range_j[2]})"
            )
          }
        }
      }
    }
  }

  # Remove redundant results
  if (length(to_remove) > 0) {
    results <- results[!names(results) %in% to_remove]
  }

  results
}


#' Check if Two MBG Results are Practically Identical
#'
#' Compares two data.tables to determine if they have essentially the same
#' cluster data. Uses tolerance-based comparison to catch cases where small
#' differences exist but the results are practically redundant.
#'
#' @param dt1 First data.table
#' @param dt2 Second data.table
#' @param tol Tolerance for considering results identical. Default 0.99 means
#'   results are considered identical if correlation > 0.99 and total counts
#'   differ by less than 1%.
#'
#' @return TRUE if results are practically identical, FALSE otherwise
#'
#' @keywords internal
#' @noRd
.are_mbg_results_identical <- function(dt1, dt2, tol = 0.99) {
  # Must have same or nearly same number of rows (within 5%)
  n1 <- nrow(dt1)
  n2 <- nrow(dt2)
  if (abs(n1 - n2) / max(n1, n2) > 0.05) {
    return(FALSE)
  }

  # Find common clusters
  common_clusters <- intersect(dt1$cluster_id, dt2$cluster_id)

  # Must have substantial overlap (at least 95% of smaller set)
  min_clusters <- min(n1, n2)
  if (length(common_clusters) / min_clusters < 0.95) {
    return(FALSE)
  }

  # Compare on common clusters
  dt1_common <- dt1[dt1$cluster_id %in% common_clusters, ]
  dt2_common <- dt2[dt2$cluster_id %in% common_clusters, ]

  # Sort by cluster_id
  dt1_sorted <- dt1_common[order(dt1_common$cluster_id), ]
  dt2_sorted <- dt2_common[order(dt2_common$cluster_id), ]

  # Check if total samplesize is nearly identical (within 1%)
  total_ss1 <- sum(dt1_sorted$samplesize)
  total_ss2 <- sum(dt2_sorted$samplesize)
  ss_diff <- abs(total_ss1 - total_ss2) / max(total_ss1, total_ss2)

  if (ss_diff > 0.01) {
    return(FALSE)
  }

  # Check if total indicator is nearly identical (within 1%)
  total_ind1 <- sum(dt1_sorted$indicator)
  total_ind2 <- sum(dt2_sorted$indicator)

  # Handle case where both are zero
  if (total_ind1 == 0 && total_ind2 == 0) {
    return(TRUE)
  }

  ind_diff <- abs(total_ind1 - total_ind2) / max(total_ind1, total_ind2, 1)

  if (ind_diff > 0.01) {
    return(FALSE)
  }

  # Check correlation of proportions (if enough variation)
  prop1 <- dt1_sorted$indicator / dt1_sorted$samplesize
  prop2 <- dt2_sorted$indicator / dt2_sorted$samplesize

  # If no variation in either, they're identical if means are close
  if (stats::sd(prop1) < 0.001 || stats::sd(prop2) < 0.001) {
    return(abs(mean(prop1) - mean(prop2)) < 0.01)
  }

  # High correlation indicates redundancy
  correlation <- stats::cor(prop1, prop2, use = "complete.obs")

  !is.na(correlation) && correlation > tol
}


#' Extract Age Group Name from Result Name
#'
#' Extracts the age group portion from a result name like "pfpr_rdt_u5".
#'
#' @param result_name Result name string
#'
#' @return Age group name (e.g., "u5", "2_10")
#'
#' @keywords internal
#' @noRd
.extract_age_group_from_name <- function(result_name) {
  # Pattern: pfpr_{test}_{age_group}
  # Remove "pfpr_rdt_", "pfpr_mic_", "pfpr_either_", or "pfpr_combined_" prefix
  sub("^pfpr_(rdt|mic|either|combined)_", "", result_name)
}


# =============================================================================
# Convenience wrapper
# =============================================================================

#' Prepare Single PfPR Indicator for MBG
#'
#' Simplified function to prepare a single PfPR indicator for MBG. Returns
#' a single data.table rather than a list.
#'
#' @inheritParams calc_pfpr_mbg
#' @param indicator Single indicator code (e.g., \code{"pfpr_rdt_u5"}).
#'   Also accepts legacy \code{test_type} values (\code{"rdt"}, \code{"mic"})
#'   combined with \code{age_min}/\code{age_max}.
#' @param age_min Minimum age in months (inclusive). Default: 6.
#'   Only used when \code{indicator} is a legacy test_type name.
#' @param age_max Maximum age in months (inclusive). Default: 59.
#'   Only used when \code{indicator} is a legacy test_type name.
#'
#' @return A tibble with columns: cluster_id, indicator, samplesize, x, y
#'
#' @export
prep_pfpr_mbg <- function(
  dhs_pr,
  gps_data,
  indicator = "pfpr_rdt_u5",
  age_min = 6,
  age_max = 59,
  survey_vars = list(
    cluster = "hv001",
    age = "hc1",
    present = "hv103",
    mother = "hv042",
    rdt = "hml35",
    mic = "hml32"
  ),
  gps_vars = list(
    cluster = "DHSCLUST",
    lat = "LATNUM",
    lon = "LONGNUM"
  )
) {
  # Check if indicator is a dictionary code or a legacy test_type name
  dict <- .pfpr_mbg_dictionary()
  dict_names <- vapply(dict, `[[`, character(1), "name")

  if (indicator %in% dict_names) {
    # New-style: indicator is already a valid code
    result <- calc_pfpr_mbg(
      dhs_pr = dhs_pr,
      gps_data = gps_data,
      indicators = indicator,
      survey_vars = survey_vars,
      gps_vars = gps_vars
    )
  } else if (indicator %in% c("rdt", "mic")) {
    # Legacy style: translate test_type + age range to indicator code.
    # `prep_pfpr_mbg()` documents this form as supported, so suppress the
    # deprecation warning that calc_pfpr_mbg() would otherwise emit.
    age_label <- paste0(age_min, "_", age_max)
    old_opt <- options(sntmethods.pfpr_mbg.suppress_legacy_warn = TRUE)
    on.exit(options(old_opt), add = TRUE)
    result <- calc_pfpr_mbg(
      dhs_pr = dhs_pr,
      gps_data = gps_data,
      test_type = indicator,
      age_groups = stats::setNames(list(c(age_min, age_max)), age_label),
      survey_vars = survey_vars,
      gps_vars = gps_vars
    )
  } else {
    cli::cli_abort(
      "Invalid indicator: {.val {indicator}}. Use a dictionary code (e.g., {.val pfpr_rdt_u5}) or legacy name ({.val rdt}, {.val mic})"
    )
  }

  if (is.null(result) || length(result) == 0) {
    cli::cli_abort("No data returned for indicator {.val {indicator}}")
  }

  # Return the single result
  result[[1]]
}


# ---- dhs_helpers_pfpr.R ----

#' Prepare PfPR Data for Analysis
#'
#' Shared data cleaning and indicator computation for PfPR functions.
#' Used by both calc_pfpr_dhs_core() and calc_pfpr_mbg().
#'
#' @param dhs_pr DHS Person Records dataset.
#' @param survey_vars Named list mapping DHS variable names.
#' @param age_min Minimum age in months (default: 6).
#' @param age_max Maximum age in months (default: 59).
#' @param include_survey_vars Logical. If TRUE, includes survey design columns.
#'
#' @return A data frame of eligible children with columns:
#'   cluster_id, age, rdt_res, mic_res, tested_rdt, tested_mic, rdt_pos, mic_pos.
#'   If include_survey_vars = TRUE, also: survey_weight, stratum_id, adm1, (adm2).
#'
#' @noRd
.prepare_pfpr_data <- function(
  dhs_pr,
  survey_vars,
  age_min = 6,
  age_max = 59,
  include_survey_vars = FALSE
) {
  if (!is.data.frame(dhs_pr)) {
    cli::cli_abort("`dhs_pr` must be a data.frame or tibble")
  }
  if (nrow(dhs_pr) == 0) {
    cli::cli_abort("`dhs_pr` is empty")
  }

  # Check required columns (mother/hv042 is optional -- absent in some MIS surveys)
  has_mother_col <- !is.null(survey_vars$mother) && survey_vars$mother %in% names(dhs_pr)
  needed <- c(survey_vars$cluster, survey_vars$age, survey_vars$present)
  missing_cols <- setdiff(needed, names(dhs_pr))
  if (length(missing_cols) > 0) {
    cli::cli_abort("Columns not found in dhs_pr: {.var {missing_cols}}")
  }

  if (!has_mother_col) {
    cli::cli_alert_warning(
      "Column {.var {survey_vars$mother}} not found in dhs_pr; ",
      "skipping mother-listed-in-household filter (common in MIS surveys)"
    )
  }

  # Zap labels
  pr <- dhs_pr |>
    dplyr::mutate(dplyr::across(dplyr::everything(), haven::zap_labels)) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), as.vector))

  # Build core columns (force numeric to guard against haven character residuals)
  pr <- pr |>
    dplyr::mutate(
      cluster_id = .data[[survey_vars$cluster]],
      age = suppressWarnings(as.numeric(as.character(.data[[survey_vars$age]]))),
      present = suppressWarnings(as.numeric(as.character(.data[[survey_vars$present]]))),
      mother = if (has_mother_col) suppressWarnings(as.numeric(as.character(.data[[survey_vars$mother]]))) else 1L,
      rdt_res = if (survey_vars$rdt %in% names(dhs_pr)) suppressWarnings(as.numeric(as.character(.data[[survey_vars$rdt]]))) else NA_real_,
      mic_res = if (survey_vars$mic %in% names(dhs_pr)) suppressWarnings(as.numeric(as.character(.data[[survey_vars$mic]]))) else NA_real_
    )

  if (include_survey_vars) {
    pr <- pr |>
      dplyr::mutate(
        survey_weight = .data[[survey_vars$weight]] / 1e6
      )

    # Handle admin columns
    has_adm1 <- !is.null(survey_vars$adm1) && survey_vars$adm1 %in% names(dhs_pr)
    has_adm2 <- !is.null(survey_vars$adm2) && survey_vars$adm2 %in% names(dhs_pr)

    pr <- pr |>
      dplyr::mutate(
        adm1 = if (has_adm1) {
          haven::as_factor(.data[[survey_vars$adm1]]) |> as.character() |> toupper()
        } else NA_character_,
        adm2 = if (has_adm2) {
          haven::as_factor(.data[[survey_vars$adm2]]) |> as.character() |> toupper()
        } else NA_character_
      )

    # Zap labels again after as_factor
    pr <- pr |>
      dplyr::mutate(dplyr::across(dplyr::everything(), haven::zap_labels))

    # Build stratum
    strata_fields <- character(0)
    if (!is.null(survey_vars$stratum) && survey_vars$stratum %in% names(dhs_pr)) {
      strata_fields <- survey_vars$stratum
    } else {
      if (has_adm1) strata_fields <- c(strata_fields, survey_vars$adm1)
      if ("hv025" %in% names(dhs_pr)) strata_fields <- c(strata_fields, "hv025")
      if (length(strata_fields) == 0 && "hv022" %in% names(dhs_pr)) {
        strata_fields <- "hv022"
      }
    }
    pr <- pr |>
      dplyr::mutate(
        stratum_id = interaction(!!!rlang::syms(strata_fields), drop = TRUE)
      )

    if (!has_adm2) {
      pr <- pr |> dplyr::select(-adm2)
    }
  }

  # Guard: if both rdt_res and mic_res are entirely NA, skip rather than crash
  rdt_vals <- pr$rdt_res
  mic_vals <- pr$mic_res
  if (all(is.na(rdt_vals)) && all(is.na(mic_vals))) {
    cli::cli_warn(
      "Both {.var {survey_vars$rdt}} and {.var {survey_vars$mic}} are entirely NA; skipping pfpr - no valid malaria test data"
    )
    return(NULL)
  }

  # Create test flags
  pr <- pr |>
    dplyr::mutate(
      tested_rdt = as.numeric(dplyr::if_else(
        present == 1 & mother == 1 & age >= age_min & age <= age_max &
          rdt_res %in% c(0, 1),
        1, 0, missing = NA_real_
      )),
      tested_mic = as.numeric(dplyr::if_else(
        present == 1 & mother == 1 & age >= age_min & age <= age_max &
          mic_res %in% c(0, 1, 6),
        1, 0, missing = NA_real_
      )),
      rdt_pos = as.numeric(dplyr::case_when(
        present == 1 & mother == 1 & age >= age_min & age <= age_max &
          rdt_res == 1 ~ 1,
        present == 1 & mother == 1 & age >= age_min & age <= age_max &
          rdt_res == 0 ~ 0,
        TRUE ~ NA_real_
      )),
      mic_pos = as.numeric(dplyr::case_when(
        present == 1 & mother == 1 & age >= age_min & age <= age_max &
          mic_res == 1 ~ 1,
        present == 1 & mother == 1 & age >= age_min & age <= age_max &
          mic_res %in% c(0, 6) ~ 0,
        TRUE ~ NA_real_
      ))
    )

  pr
}


