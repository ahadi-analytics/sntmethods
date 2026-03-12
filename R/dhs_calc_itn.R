#' Calculate ITN Indicators from DHS Data
#'
#' Computes the full set of ITN indicators
#' from DHS Household Records (HR) and Person Records (PR) data. Returns
#' survey-weighted proportions with logit confidence intervals in long
#' format.
#'
#' @details
#' Computes up to 42+ ITN indicators following WHO methodology, organised in
#' 6 categories: ENOUGH_ITN (household sufficient nets), WITH_ITN (household
#' ownership), ACCESS_ITN (population access), USE_ITN_CHU5 (under 5 use),
#' USE_ITN_PREGNANT (pregnant women use), and USE_ITN (general population use).
#' Each category includes 7 subgroup splits: overall, LOW_WEALTH, HIGH_WEALTH,
#' NON_LOW_WEALTH, NON_HIGH_WEALTH, RURAL, URBAN.
#'
#' When `age_breaks` and `age_labels` are provided, additional USE_ITN_AGE_*
#' indicators are computed for each age group.
#'
#' @param dhs_hr DHS Household Records dataset (HR).
#' @param dhs_pr DHS Person Records dataset (PR).
#' @param survey_vars Named list mapping DHS variable names. Required keys:
#'   \itemize{
#'     \item `cluster`: Cluster ID (default: "hv001")
#'     \item `weight`: Survey weight (default: "hv005")
#'     \item `stratum`: Stratum variable (default: "hv022")
#'     \item `hhid`: Household ID (default: "hhid")
#'     \item `hhsize`: Household size (default: "hv013")
#'     \item `age`: Age in years (default: "hv105")
#'     \item `sex`: Sex (default: "hv104")
#'     \item `pregnant`: Pregnancy status (default: "hml18")
#'     \item `itn_use`: Slept under ITN last night (default: "hml12")
#'     \item `itn_prefix`: Prefix for ITN net variables (default: "hml10_")
#'     \item `wealth`: Wealth index quintile (default: "hv270")
#'     \item `residence`: Urban/rural (default: "hv025")
#'   }
#' @param region_var Optional column name for subnational grouping
#'   (e.g., "hv024"). Auto-falls back to "hv024" if no spatial params.
#' @param gps_data Optional DHS GPS dataset with cluster coordinates.
#' @param gps_vars Named list for GE variables: cluster, lat, lon.
#' @param shapefile Optional sf object with administrative boundaries.
#' @param admin_level Character vector of admin columns from shapefile.
#' @param join_nearest Logical; assign unmatched clusters to nearest polygon.
#' @param indicators Character vector of indicator names to compute. If NULL
#'   (default), computes all indicators from [itn_dictionary()].
#' @param age_breaks Optional numeric vector of age group boundaries
#'   (e.g., c(0, 5, 15, Inf)). Creates additional USE_ITN_AGE_* indicators.
#' @param age_labels Optional character vector of labels for age groups
#'   (e.g., c("U5", "5_14", "OV15")). Must have length = length(age_breaks) - 1.
#' @param ci_method Method for confidence intervals. Default: "logit".
#'
#' @return Named list of tibbles, one per admin level:
#'   \describe{
#'     \item{`adm0`}{National-level estimates (always present)}
#'     \item{`adm1`}{Admin-1 estimates (when `region_var` or shapefile used)}
#'     \item{`adm2`}{Admin-2 estimates (when shapefile with adm2 used)}
#'   }
#'   Each tibble contains columns: survey_id, iso3, iso2, survey_type,
#'   survey_year, adm0, [adm1], [adm2], type, geo_source, point, ci_l, ci_u,
#'   numerator, denominator, indicator, indicator_code,
#'   numerator_description, denominator_description, denominator_code.
#'
#' @seealso [itn_dictionary()] for indicator definitions,
#'   [calc_itn_dhs_core()] for the legacy wide-format output
#' @export
calc_itn_dhs <- function(
  dhs_hr,
  dhs_pr,
  survey_vars = list(
    cluster   = "hv001",
    weight    = "hv005",
    stratum   = "hv022",
    hhid      = "hhid",
    hhsize    = "hv013",
    age       = "hv105",
    sex       = "hv104",
    pregnant  = "hml18",
    itn_use   = "hml12",
    itn_prefix = "hml10_",
    itn_treated_prefix = "hml7_",
    wealth    = "hv270",
    residence = "hv025"
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
  age_breaks   = NULL,
  age_labels   = NULL,
  ci_method    = "logit"
) {

  # ---- 1. Input validation ----

  if (!is.data.frame(dhs_hr)) cli::cli_abort("`dhs_hr` must be a data.frame or tibble.")
  if (!is.data.frame(dhs_pr)) cli::cli_abort("`dhs_pr` must be a data.frame or tibble.")
  if (nrow(dhs_hr) == 0) cli::cli_abort("`dhs_hr` is empty.")
  if (nrow(dhs_pr) == 0) cli::cli_abort("`dhs_pr` is empty.")

  # Check required HR variables
  needed_hr <- c(survey_vars$cluster, survey_vars$weight, survey_vars$stratum,
                 survey_vars$hhid, survey_vars$hhsize)
  missing_hr <- setdiff(needed_hr, names(dhs_hr))
  if (length(missing_hr) > 0) {
    cli::cli_abort(c(
      "Required HR variables not found: {.var {missing_hr}}",
      "i" = "Check your survey_vars mapping"
    ))
  }

  # Check required PR variables
  needed_pr <- c(survey_vars$cluster, survey_vars$weight, survey_vars$stratum,
                 survey_vars$hhid, survey_vars$age, survey_vars$sex,
                 survey_vars$itn_use)
  missing_pr <- setdiff(needed_pr, names(dhs_pr))
  if (length(missing_pr) > 0) {
    cli::cli_abort(c(
      "Required PR variables not found: {.var {missing_pr}}",
      "i" = "Check your survey_vars mapping"
    ))
  }

  # Validate age stratification if provided
  if (!is.null(age_breaks)) {
    if (is.null(age_labels) || length(age_labels) != length(age_breaks) - 1) {
      cli::cli_abort(
        "`age_labels` must have length = length(age_breaks) - 1"
      )
    }
  }

  # ---- 1b. Extract survey metadata ----
  survey_meta <- .extract_survey_meta_hv(dhs_hr)

  # ---- 2. Prepare household data ----

  household_data <- .prepare_itn_household_data(
    dhs_hr = dhs_hr,
    survey_vars = survey_vars,
    include_survey_vars = TRUE
  )

  if (is.null(household_data)) {
    cli::cli_warn("ITN indicators skipped: no ITN variables found in HR data")
    return(NULL)
  }

  # Add sufficient nets indicator
  household_data <- household_data |>
    dplyr::mutate(
      hh_sufficient_nets = as.integer(n_itns >= (hh_size / 2))
    )

  # Add wealth and residence from HR (zapped)
  hr_zapped <- dhs_hr |>
    dplyr::mutate(dplyr::across(dplyr::everything(), haven::zap_labels)) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), as.vector))

  wealth_var <- survey_vars$wealth %||% "hv270"
  residence_var <- survey_vars$residence %||% "hv025"

  if (wealth_var %in% names(hr_zapped)) {
    household_data$wealth <- hr_zapped[[wealth_var]]
    household_data <- household_data |>
      dplyr::mutate(
        low_wealth  = as.integer(wealth == 1),
        high_wealth = as.integer(wealth == 5)
      )
  } else {
    cli::cli_alert_warning("Wealth variable {.var {wealth_var}} not found in HR data")
    household_data$low_wealth <- NA_integer_
    household_data$high_wealth <- NA_integer_
  }

  if (residence_var %in% names(hr_zapped)) {
    household_data$residence <- hr_zapped[[residence_var]]
    household_data <- household_data |>
      dplyr::mutate(
        is_rural = as.integer(residence == 2),
        is_urban = as.integer(residence == 1)
      )
  } else {
    cli::cli_alert_warning("Residence variable {.var {residence_var}} not found in HR data")
    household_data$is_rural <- NA_integer_
    household_data$is_urban <- NA_integer_
  }

  # Add region variable for grouping
  region_hr_var <- survey_vars$adm1 %||% "hv024"
  if (region_hr_var %in% names(dhs_hr)) {
    household_data$adm1_raw <- .resolve_region_labels(
      dhs_hr[[region_hr_var]], region_hr_var
    )
  }

  n_hh <- nrow(household_data)
  n_with_itn <- sum(household_data$has_itn)
  cli::cli_alert_info(
    "Processed {format(n_hh, big.mark = ',')} households: {format(n_with_itn, big.mark = ',')} with >=1 ITN"
  )

  # ---- 3. Prepare person data ----

  person_data <- .prepare_itn_person_data(
    dhs_pr = dhs_pr,
    hr_data = household_data |>
      dplyr::select(cluster_id, hhid, hh_size, n_itns, potential_users),
    survey_vars = survey_vars,
    include_survey_vars = TRUE
  )

  # Add derived columns
  pr_zapped <- dhs_pr |>
    dplyr::mutate(dplyr::across(dplyr::everything(), haven::zap_labels)) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), as.vector))

  person_data <- person_data |>
    dplyr::mutate(
      is_under5 = as.integer(age < 5),
      is_pregnant_woman = as.integer(sex == 2 & is_pregnant == 1),
      itn_access_ratio = dplyr::if_else(
        !is.na(potential_users) & !is.na(hh_size) & hh_size > 0,
        pmin(potential_users / hh_size, 1),
        NA_real_
      )
    )

  # Add wealth and residence from PR data
  if (wealth_var %in% names(pr_zapped)) {
    person_data$wealth <- pr_zapped[[wealth_var]]
    person_data <- person_data |>
      dplyr::mutate(
        low_wealth  = as.integer(wealth == 1),
        high_wealth = as.integer(wealth == 5)
      )
  } else {
    person_data$low_wealth <- NA_integer_
    person_data$high_wealth <- NA_integer_
  }

  if (residence_var %in% names(pr_zapped)) {
    person_data$residence <- pr_zapped[[residence_var]]
    person_data <- person_data |>
      dplyr::mutate(
        is_rural = as.integer(residence == 2),
        is_urban = as.integer(residence == 1)
      )
  } else {
    person_data$is_rural <- NA_integer_
    person_data$is_urban <- NA_integer_
  }

  # Add region from PR
  if (region_hr_var %in% names(dhs_pr)) {
    person_data$adm1_raw <- .resolve_region_labels(
      dhs_pr[[region_hr_var]], region_hr_var
    )
  }

  n_persons <- nrow(person_data)
  n_used_itn <- sum(person_data$itn_used)
  cli::cli_alert_info(
    "Processed {format(n_persons, big.mark = ',')} individuals: {format(n_used_itn, big.mark = ',')} used ITN"
  )

  # ---- 4. Region grouping or spatial join ----

  admin_hierarchy <- list()
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
    # Region labels already applied above via adm1_raw (from HR/PR)
    # For household data, group_var = "adm1_raw"
    # For person data, group_var = "adm1_raw"
    admin_hierarchy <- list(list(group_var = "adm1_raw", level_name = "adm1"))
    geo_src <- "survey"

    regions_hh <- unique(household_data$adm1_raw)
    cli::cli_alert_info(
      "Grouping by {.var {region_var}}: {paste(regions_hh, collapse = ', ')}"
    )

  } else if (!is.null(gps_data) && !is.null(shapefile)) {
    # Spatial join for household data
    household_data <- .spatial_join_itn(
      data = household_data,
      gps_data = gps_data,
      gps_vars = gps_vars,
      shapefile = shapefile,
      admin_level = admin_level,
      join_nearest = join_nearest
    )
    # Spatial join for person data (same clusters)
    person_data <- .spatial_join_itn(
      data = person_data,
      gps_data = gps_data,
      gps_vars = gps_vars,
      shapefile = shapefile,
      admin_level = admin_level,
      join_nearest = join_nearest
    )
    geo_src <- "gps"

    admin_lvls <- attr(household_data, "admin_levels") %||% character(0)
    for (lvl in admin_lvls) {
      admin_hierarchy <- c(admin_hierarchy, list(
        list(group_var = lvl, level_name = lvl)
      ))
    }
  }

  # ---- 5. Get indicator dictionary and filter ----

  dict <- .itn_conditions()

  # Add age-group indicators if requested
  if (!is.null(age_breaks) && !is.null(age_labels)) {
    age_dict <- .itn_age_conditions(age_breaks, age_labels)
    dict <- c(dict, age_dict)
  }

  if (!is.null(indicators)) {
    valid <- vapply(dict, function(d) d$indicator, character(1))
    bad <- setdiff(indicators, valid)
    if (length(bad) > 0) {
      cli::cli_warn("Unknown indicators ignored: {paste(bad, collapse = ', ')}")
    }
    dict <- dict[vapply(
      dict, function(d) d$indicator %in% indicators, logical(1)
    )]
  }

  if (length(dict) == 0) {
    cli::cli_abort("No valid indicators to compute.")
  }

  # ---- 6. Compute each indicator ----

  options(survey.lonely.psu = "adjust")

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
    .compute_itn_indicator(
      hh_data     = household_data,
      pr_data     = person_data,
      condition   = cond,
      group_var   = NULL,
      ci_method   = ci_method
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
  all_level_names <- vapply(admin_hierarchy, `[[`, character(1), "level_name")

  for (i in seq_along(admin_hierarchy)) {
    ah <- admin_hierarchy[[i]]
    grp <- ah$group_var
    lvl_name <- ah$level_name

    sub_results <- purrr::map_dfr(dict, function(cond) {
      .compute_itn_indicator(
        hh_data     = household_data,
        pr_data     = person_data,
        condition   = cond,
        group_var   = grp,
        subnational_level = lvl_name,
        ci_method   = ci_method
      )
    })

    # Filter to regional rows only
    sub_results <- sub_results |>
      dplyr::filter(level != "adm0")

    if (nrow(sub_results) == 0) next

    sub_results <- .round_results(sub_results)

    # Add the current admin level column
    sub_results <- sub_results |>
      dplyr::mutate(!!lvl_name := toupper(location))

    # Add parent admin columns
    parent_levels <- all_level_names[seq_len(i - 1)]
    # Use household_data for parent lookups (it has all admin columns)
    parent_cols_in_data <- intersect(parent_levels, names(household_data))
    if (length(parent_cols_in_data) > 0 && grp %in% names(household_data)) {
      parent_lookup <- household_data |>
        dplyr::select(dplyr::all_of(c(grp, parent_cols_in_data))) |>
        dplyr::mutate(dplyr::across(dplyr::everything(), ~toupper(as.character(.)))) |>
        dplyr::distinct()
      sub_results <- sub_results |>
        dplyr::left_join(parent_lookup, by = stats::setNames(grp, lvl_name))
    }

    # Select columns in proper order
    admin_cols <- c(parent_cols_in_data, lvl_name)
    admin_cols <- intersect(admin_cols, names(sub_results))

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
# Survey metadata extraction for HR/PR data
# =============================================================================

#' Extract survey metadata from HR/PR data
#'
#' Like .extract_survey_meta() but checks hv000/hv007 (PR/HR variables)
#' in addition to v000/v007 (KR variables).
#'
#' @param data DHS dataset (HR or PR).
#' @return Named list: survey_id, iso3, iso2, country_upper, survey_type, survey_year.
#' @noRd
.extract_survey_meta_hv <- function(data) {
  # Try v000/v007 first (KR data), then hv000/hv007 (HR/PR data)
  v000 <- NA_character_
  v007 <- NA_integer_

  for (vname in c("v000", "hv000")) {
    if (vname %in% names(data) && is.na(v000)) {
      v000_raw <- unique(as.character(haven::zap_labels(data[[vname]])))
      v000 <- v000_raw[!is.na(v000_raw)][1]
    }
  }
  for (vname in c("v007", "hv007")) {
    if (vname %in% names(data) && is.na(v007)) {
      v007_raw <- unique(as.integer(haven::zap_labels(data[[vname]])))
      v007 <- v007_raw[!is.na(v007_raw)][1]
    }
  }

  iso2 <- if (!is.na(v000)) toupper(substr(v000, 1, 2)) else NA_character_

  iso3 <- NA_character_
  country_upper <- NA_character_
  if (!is.na(iso2)) {
    iso3 <- tryCatch(
      countrycode::countrycode(iso2, origin = "iso2c", destination = "iso3c"),
      warning = function(w) NA_character_
    )
    if (!is.na(iso3)) {
      country_name <- tryCatch(
        countrycode::countrycode(iso3, origin = "iso3c", destination = "country.name"),
        warning = function(w) NA_character_
      )
      country_upper <- if (!is.na(country_name)) toupper(country_name) else NA_character_
    }
  }

  # Detect survey type
  survey_type <- NA_character_
  survey_year <- if (!is.na(v007)) as.integer(v007) else NA_integer_

  # 1. Check survey_type column
  if ("survey_type" %in% names(data)) {
    st <- unique(as.character(haven::zap_labels(data$survey_type)))
    st <- st[!is.na(st) & nchar(st) > 0][1]
    if (!is.na(st)) survey_type <- toupper(st)
  }

  # 2. Check surveyid column
  if (is.na(survey_type) && "surveyid" %in% names(data)) {
    sid <- unique(as.character(haven::zap_labels(data$surveyid)))
    sid <- sid[!is.na(sid)][1]
    if (!is.na(sid)) {
      if (grepl("MIS", sid, ignore.case = TRUE)) survey_type <- "MIS"
      else if (grepl("AIS", sid, ignore.case = TRUE)) survey_type <- "AIS"
      else if (grepl("DHS", sid, ignore.case = TRUE)) survey_type <- "DHS"
    }
  }

  # 3. Fall back to v000/hv000 suffix
  if (is.na(survey_type)) {
    survey_type <- "DHS"
    if (!is.na(v000) && nchar(v000) >= 3) {
      suffix <- substr(v000, 3, nchar(v000))
      if (grepl("[Ii]", suffix)) survey_type <- "MIS"
      else if (grepl("[Aa]", suffix)) survey_type <- "AIS"
    }
  }

  survey_id <- if (!is.na(iso2) && !is.na(survey_year)) {
    paste0(iso2, survey_year, survey_type)
  } else {
    NA_character_
  }

  list(
    survey_id     = survey_id,
    iso3          = iso3,
    iso2          = iso2,
    country_upper = country_upper,
    survey_type   = survey_type,
    survey_year   = survey_year
  )
}


# =============================================================================
# Spatial join helper for ITN data (works with both HH and PR data)
# =============================================================================

#' Spatial join for ITN data
#'
#' @param data Data frame with cluster_id column.
#' @param gps_data DHS GE dataset.
#' @param gps_vars Named list: cluster, lat, lon.
#' @param shapefile sf object with admin boundaries.
#' @param admin_level Character vector of admin columns.
#' @param join_nearest Logical; nearest-feature fallback.
#'
#' @return data with admin columns added. Attribute "admin_levels" set.
#' @noRd
.spatial_join_itn <- function(data, gps_data, gps_vars, shapefile,
                               admin_level = NULL, join_nearest = TRUE) {

  if (!requireNamespace("sf", quietly = TRUE)) {
    cli::cli_abort("Package {.pkg sf} is required for spatial operations.")
  }

  gps_clean <- gps_data |>
    dplyr::mutate(dplyr::across(dplyr::everything(), haven::zap_labels)) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), as.vector)) |>
    dplyr::select(
      cluster_id = !!gps_vars$cluster,
      lat        = !!gps_vars$lat,
      lon        = !!gps_vars$lon
    ) |>
    dplyr::distinct()

  data <- data |>
    dplyr::left_join(gps_clean, by = "cluster_id")

  clusters_sf <- data |>
    dplyr::select(cluster_id, lat, lon) |>
    dplyr::distinct() |>
    dplyr::filter(!is.na(lat), !is.na(lon)) |>
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)

  shapefile <- shapefile |>
    sf::st_transform(4326) |>
    sf::st_make_valid()

  available_admins <- sort(
    names(shapefile)[grep("^adm[0-9]+$", names(shapefile))]
  )
  if (length(available_admins) == 0) {
    cli::cli_abort("No admin columns (adm0, adm1, ...) found in shapefile.")
  }

  # Expand admin_level to include all intermediate levels
  if (!is.null(admin_level) && length(admin_level) == 1) {
    target_num <- as.integer(sub("^adm", "", admin_level))
    all_cols <- available_admins[
      as.integer(sub("^adm", "", available_admins)) >= 1 &
        as.integer(sub("^adm", "", available_admins)) <= target_num
    ]
    admin_level <- if (length(all_cols) > 0) all_cols else admin_level
  } else if (is.null(admin_level)) {
    admin_level <- available_admins[available_admins != "adm0"]
    if (length(admin_level) == 0) admin_level <- available_admins
  }

  cli::cli_alert_info("Spatial join: using admin levels {paste(admin_level, collapse = ', ')}")

  # Spatial join
  join_result <- sf::st_join(
    clusters_sf,
    shapefile[, c(admin_level, "geometry")],
    join = sf::st_within,
    left = TRUE
  )

  # Nearest fallback
  if (join_nearest) {
    unmatched <- is.na(join_result[[admin_level[1]]])
    if (any(unmatched)) {
      cli::cli_alert_info(
        "Assigning {sum(unmatched)} unmatched clusters to nearest polygons"
      )
      nearest_idx <- sf::st_nearest_feature(
        join_result[unmatched, ], shapefile
      )
      for (col in admin_level) {
        join_result[unmatched, col] <- shapefile[[col]][nearest_idx]
      }
    }
  }

  # Join back to data
  cluster_admin <- sf::st_drop_geometry(join_result) |>
    dplyr::select(cluster_id, dplyr::all_of(admin_level)) |>
    dplyr::mutate(dplyr::across(
      dplyr::all_of(admin_level), ~toupper(as.character(.))
    ))

  # Remove conflicting columns
  conflicting <- intersect(names(data), admin_level)
  if (length(conflicting) > 0) {
    data <- data |> dplyr::select(-dplyr::all_of(conflicting))
  }

  data <- data |>
    dplyr::left_join(cluster_admin, by = "cluster_id")

  attr(data, "admin_levels") <- admin_level
  data
}


# =============================================================================
# Core ITN indicator computation
# =============================================================================

#' Compute a single ITN indicator
#'
#' @param hh_data Prepared household data.
#' @param pr_data Prepared person data.
#' @param condition List with: indicator, indicator_code, indicator_title,
#'   data_level ("household"/"person"), outcome_var, filter_expr,
#'   num_desc, denom_desc, denom_code, est_method ("ciprop"/"mean").
#' @param group_var Optional grouping variable.
#' @param subnational_level Admin level name for grouped rows.
#' @param ci_method CI method. Default: "logit".
#'
#' @return Tibble with level, location, point, ci_l, ci_u, numerator,
#'   denominator, indicator, indicator_code, numerator_description,
#'   denominator_description, denominator_code.
#' @noRd
.compute_itn_indicator <- function(hh_data, pr_data, condition,
                                    group_var = NULL,
                                    subnational_level = NULL,
                                    ci_method = "logit") {

  data_level <- condition$data_level  # "household" or "person"
  outcome_var <- condition$outcome_var
  est_method <- condition$est_method %||% "ciprop"
  ind_title <- condition$indicator_title

  # Select the right dataset
  data <- if (data_level == "household") hh_data else pr_data

  # Apply population filter
  if (!is.null(condition$filter_expr)) {
    filtered <- tryCatch(
      dplyr::filter(data, !!condition$filter_expr),
      error = function(e) tibble::tibble()
    )
  } else {
    filtered <- data
  }

  # Check outcome variable exists
  if (!outcome_var %in% names(filtered)) {
    return(tibble::tibble())
  }

  # Drop rows with NA outcome
  filtered <- filtered[!is.na(filtered[[outcome_var]]), ]
  n_denom <- nrow(filtered)
  if (n_denom == 0) return(tibble::tibble())

  # Set outcome as "itn_outcome" for survey estimation
  filtered$itn_outcome <- filtered[[outcome_var]]

  # Weighted counts
  n_denom_w <- round(sum(filtered$survey_weight, na.rm = TRUE))
  n_numer_w <- round(sum(
    filtered$survey_weight * filtered$itn_outcome, na.rm = TRUE
  ))

  # --- Survey design ---
  n_clusters <- dplyr::n_distinct(filtered$cluster_id)
  use_strata <- dplyr::n_distinct(filtered$stratum_id) > 1

  # If only 1 cluster total, can't estimate variance — return point only
  if (n_clusters < 2) {
    point_est <- sum(filtered$survey_weight * filtered$itn_outcome, na.rm = TRUE) /
      sum(filtered$survey_weight, na.rm = TRUE)
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
    # Fallback: drop strata if design fails
    survey::svydesign(
      ids = ~cluster_id, weights = ~survey_weight,
      data = filtered, nest = TRUE
    )
  })

  # --- National estimate ---
  national <- tryCatch({
    if (est_method == "mean") {
      # For access ratio (continuous 0-1)
      est_val <- survey::svymean(~itn_outcome, svy, na.rm = TRUE)
      ci <- stats::confint(est_val)
      tibble::tibble(
        level = "adm0", location = "National",
        point = as.numeric(est_val),
        ci_l = ci[1], ci_u = ci[2],
        numerator = n_numer_w, denominator = n_denom_w
      )
    } else {
      est <- survey::svyciprop(~itn_outcome, svy, method = ci_method,
                                na.rm = TRUE)
      ci <- stats::confint(est)
      tibble::tibble(
        level = "adm0", location = "National",
        point = as.numeric(est),
        ci_l = ci[1], ci_u = ci[2],
        numerator = n_numer_w, denominator = n_denom_w
      )
    }
  }, error = function(e) {
    cli::cli_alert_warning("    {ind_title} national: {e$message}")
    tibble::tibble(
      level = "adm0", location = "National", point = NA_real_,
      ci_l = NA_real_, ci_u = NA_real_,
      numerator = n_numer_w, denominator = n_denom_w
    )
  })

  # --- Regional estimates ---
  regional <- tibble::tibble()
  sub_level <- subnational_level %||% "adm1"

  if (!is.null(group_var) && group_var %in% names(filtered)) {
    group_formula <- stats::as.formula(paste("~", group_var))

    regional <- tryCatch({
      if (est_method == "mean") {
        by_result <- survey::svyby(
          ~itn_outcome, by = group_formula, design = svy,
          FUN = survey::svymean, vartype = "ci",
          na.rm = TRUE, keep.names = FALSE
        ) |> tibble::as_tibble()
      } else {
        by_result <- survey::svyby(
          ~itn_outcome, by = group_formula, design = svy,
          FUN = survey::svyciprop, vartype = "ci",
          method = ci_method, na.rm = TRUE, keep.names = FALSE
        ) |> tibble::as_tibble()
      }

      # Weighted numerator per group
      region_num <- filtered |>
        dplyr::group_by(.data[[group_var]]) |>
        dplyr::summarise(
          numerator = round(sum(
            survey_weight * itn_outcome, na.rm = TRUE
          )),
          .groups = "drop"
        )

      # Weighted denominator per group
      region_denom <- filtered |>
        dplyr::group_by(.data[[group_var]]) |>
        dplyr::summarise(
          denominator = round(sum(survey_weight, na.rm = TRUE)),
          .groups = "drop"
        )

      # Normalize CI column names
      names(by_result)[names(by_result) == "ci_l"] <- "ci_l.itn_outcome"
      names(by_result)[names(by_result) == "ci_u"] <- "ci_u.itn_outcome"

      by_result |>
        dplyr::rename(
          location = !!group_var,
          point    = itn_outcome,
          ci_l     = `ci_l.itn_outcome`,
          ci_u     = `ci_u.itn_outcome`
        ) |>
        dplyr::mutate(location = as.character(location)) |>
        dplyr::left_join(
          region_num |>
            dplyr::mutate(location = as.character(.data[[group_var]])) |>
            dplyr::select(location, numerator),
          by = "location"
        ) |>
        dplyr::left_join(
          region_denom |>
            dplyr::mutate(location = as.character(.data[[group_var]])) |>
            dplyr::select(location, denominator),
          by = "location"
        ) |>
        dplyr::mutate(level = sub_level) |>
        dplyr::select(level, location, point, ci_l, ci_u, numerator,
                       denominator)

    }, error = function(e) {
      cli::cli_alert_warning("    {ind_title} by group: {e$message}")
      tibble::tibble()
    })
  }

  # --- Combine ---
  dplyr::bind_rows(national, regional) |>
    dplyr::mutate(
      indicator               = condition$indicator_title,
      indicator_code          = condition$indicator_code,
      numerator_description   = condition$num_desc,
      denominator_description = condition$denom_desc,
      denominator_code        = condition$denom_code
    )
}


# =============================================================================
# ITN Indicator Dictionary
# =============================================================================

#' ITN Indicator Dictionary
#'
#' Returns the full dictionary of ITN indicators with metadata.
#'
#' @return Tibble with columns: indicator, indicator_code, indicator_title,
#'   data_level, numerator_description, denominator_description,
#'   denominator_code.
#'
#' @examples
#' itn_dictionary()
#'
#' @export
itn_dictionary <- function() {
  conds <- .itn_conditions()
  tibble::tibble(
    indicator = vapply(conds, `[[`, character(1), "indicator"),
    indicator_code = vapply(conds, `[[`, character(1), "indicator_code"),
    indicator_title = vapply(conds, `[[`, character(1), "indicator_title"),
    data_level = vapply(conds, `[[`, character(1), "data_level"),
    numerator_description = vapply(conds, `[[`, character(1), "num_desc"),
    denominator_description = vapply(conds, `[[`, character(1), "denom_desc"),
    denominator_code = vapply(conds, `[[`, character(1), "denom_code")
  )
}


#' Internal: ITN indicator conditions
#'
#' Returns list of 42 indicator specifications for ITN indicators.
#' 6 categories x 7 subgroups each.
#'
#' @return List of named lists.
#' @noRd
.itn_conditions <- function() {

  # Helper: generate 7 subgroup variants for a base indicator
  .expand_subgroups <- function(base_indicator, base_code, base_title,
                                 data_level, outcome_var, base_filter,
                                 num_desc, base_denom_desc, base_denom_code,
                                 est_method = "ciprop") {

    subgroups <- list(
      list(suffix = "",              code_suffix = "",
           filter = base_filter,
           denom_suffix = "",
           denom_desc = base_denom_desc),
      list(suffix = "_LOW_WEALTH",   code_suffix = "_low_wealth",
           filter = if (is.null(base_filter)) quote(low_wealth == 1)
                    else substitute(f & low_wealth == 1, list(f = base_filter)),
           denom_suffix = " low wealth",
           denom_desc = paste0(base_denom_desc, " low wealth")),
      list(suffix = "_HIGH_WEALTH",  code_suffix = "_high_wealth",
           filter = if (is.null(base_filter)) quote(high_wealth == 1)
                    else substitute(f & high_wealth == 1, list(f = base_filter)),
           denom_suffix = " high wealth",
           denom_desc = paste0(base_denom_desc, " high wealth")),
      list(suffix = "_NON_LOW_WEALTH", code_suffix = "_non_low_wealth",
           filter = if (is.null(base_filter)) quote(low_wealth == 0)
                    else substitute(f & low_wealth == 0, list(f = base_filter)),
           denom_suffix = " non low wealth",
           denom_desc = paste0(base_denom_desc, " non low wealth")),
      list(suffix = "_NON_HIGH_WEALTH", code_suffix = "_non_high_wealth",
           filter = if (is.null(base_filter)) quote(high_wealth == 0)
                    else substitute(f & high_wealth == 0, list(f = base_filter)),
           denom_suffix = " non high wealth",
           denom_desc = paste0(base_denom_desc, " non high wealth")),
      list(suffix = "_RURAL",        code_suffix = "_rural",
           filter = if (is.null(base_filter)) quote(is_rural == 1)
                    else substitute(f & is_rural == 1, list(f = base_filter)),
           denom_suffix = " rural",
           denom_desc = paste0(base_denom_desc, " rural")),
      list(suffix = "_URBAN",        code_suffix = "_urban",
           filter = if (is.null(base_filter)) quote(is_urban == 1)
                    else substitute(f & is_urban == 1, list(f = base_filter)),
           denom_suffix = " urban",
           denom_desc = paste0(base_denom_desc, " urban"))
    )

    lapply(subgroups, function(sg) {
      list(
        indicator       = paste0(base_indicator, sg$suffix),
        indicator_code  = paste0(base_code, sg$code_suffix),
        indicator_title = if (nchar(sg$suffix) == 0) base_title
                          else paste0(base_title, " (", gsub("_", " ", tolower(substring(sg$suffix, 2))), ")"),
        data_level      = data_level,
        outcome_var     = outcome_var,
        filter_expr     = sg$filter,
        est_method      = est_method,
        num_desc        = num_desc,
        denom_desc      = sg$denom_desc,
        denom_code      = paste0(base_denom_code, sg$code_suffix)
      )
    })
  }

  c(
    # --- ENOUGH_ITN: Household has enough ITN for every 2 people ---
    .expand_subgroups(
      base_indicator  = "ENOUGH_ITN",
      base_code       = "enough_itn",
      base_title      = "Households with sufficient ITNs (1 per 2 people)",
      data_level      = "household",
      outcome_var     = "hh_sufficient_nets",
      base_filter     = NULL,
      num_desc        = "Households with at least one ITN for every two people",
      base_denom_desc = "De facto households surveyed",
      base_denom_code = "hh"
    ),

    # --- WITH_ITN: Household has at least one ITN ---
    .expand_subgroups(
      base_indicator  = "WITH_ITN",
      base_code       = "with_itn",
      base_title      = "Households with at least one ITN",
      data_level      = "household",
      outcome_var     = "has_itn",
      base_filter     = NULL,
      num_desc        = "Households owning at least one ITN",
      base_denom_desc = "De facto households surveyed",
      base_denom_code = "hh"
    ),

    # --- ACCESS_ITN: Population access to ITN ---
    .expand_subgroups(
      base_indicator  = "ACCESS_ITN",
      base_code       = "access_itn",
      base_title      = "Population with access to an ITN",
      data_level      = "person",
      outcome_var     = "itn_access_ratio",
      base_filter     = NULL,
      num_desc        = "De facto population with access to an ITN within their household",
      base_denom_desc = "De facto household population",
      base_denom_code = "pop",
      est_method      = "mean"
    ),

    # --- USE_ITN_CHU5: Under 5 ITN use ---
    .expand_subgroups(
      base_indicator  = "USE_ITN_CHU5",
      base_code       = "use_itn_chu5",
      base_title      = "Children under 5 who slept under an ITN",
      data_level      = "person",
      outcome_var     = "itn_used",
      base_filter     = quote(is_under5 == 1),
      num_desc        = "Children under 5 who slept under an ITN the previous night",
      base_denom_desc = "De facto children under 5",
      base_denom_code = "pop_u5"
    ),

    # --- USE_ITN_PREGNANT: Pregnant women ITN use ---
    .expand_subgroups(
      base_indicator  = "USE_ITN_PREGNANT",
      base_code       = "use_itn_preg",
      base_title      = "Pregnant women who slept under an ITN",
      data_level      = "person",
      outcome_var     = "itn_used",
      base_filter     = quote(is_pregnant_woman == 1),
      num_desc        = "Pregnant women who slept under an ITN the previous night",
      base_denom_desc = "De facto pregnant women",
      base_denom_code = "pop_preg"
    ),

    # --- USE_ITN: General population ITN use ---
    .expand_subgroups(
      base_indicator  = "USE_ITN",
      base_code       = "use_itn",
      base_title      = "Population that slept under an ITN",
      data_level      = "person",
      outcome_var     = "itn_used",
      base_filter     = NULL,
      num_desc        = "De facto population that slept under an ITN the previous night",
      base_denom_desc = "De facto household population",
      base_denom_code = "pop"
    ),

    # --- USE_ITN_IF_ACCESS: ITN use given access ---
    list(list(
      indicator       = "USE_ITN_IF_ACCESS",
      indicator_code  = "use_itn_if_access",
      indicator_title = "ITN use among population with access",
      data_level      = "person",
      outcome_var     = "itn_used",
      filter_expr     = quote(itn_access_ratio > 0),
      est_method      = "ciprop",
      num_desc        = "Population with ITN access who slept under an ITN the previous night",
      denom_desc      = "De facto population with access to an ITN",
      denom_code      = "pop_access"
    )),

    # --- USE_ITN_5_10: ITN use among 5-9 year olds ---
    list(list(
      indicator       = "USE_ITN_5_10",
      indicator_code  = "use_itn_5_10",
      indicator_title = "Children 5-9 who slept under an ITN",
      data_level      = "person",
      outcome_var     = "itn_used",
      filter_expr     = quote(age >= 5 & age < 10),
      est_method      = "ciprop",
      num_desc        = "Children aged 5-9 who slept under an ITN the previous night",
      denom_desc      = "De facto children aged 5-9 years",
      denom_code      = "pop_5_10"
    )),

    # --- USE_ITN_10_20: ITN use among 10-19 year olds ---
    list(list(
      indicator       = "USE_ITN_10_20",
      indicator_code  = "use_itn_10_20",
      indicator_title = "Population 10-19 who slept under an ITN",
      data_level      = "person",
      outcome_var     = "itn_used",
      filter_expr     = quote(age >= 10 & age < 20),
      est_method      = "ciprop",
      num_desc        = "Population aged 10-19 who slept under an ITN the previous night",
      denom_desc      = "De facto population aged 10-19 years",
      denom_code      = "pop_10_20"
    )),

    # --- USE_ITN_20PLUS: ITN use among 20+ year olds ---
    list(list(
      indicator       = "USE_ITN_20PLUS",
      indicator_code  = "use_itn_20plus",
      indicator_title = "Adults 20+ who slept under an ITN",
      data_level      = "person",
      outcome_var     = "itn_used",
      filter_expr     = quote(age >= 20),
      est_method      = "ciprop",
      num_desc        = "Adults aged 20+ who slept under an ITN the previous night",
      denom_desc      = "De facto population aged 20+ years",
      denom_code      = "pop_20plus"
    ))
  )
}


#' Generate age-group ITN use conditions
#'
#' @param age_breaks Numeric vector of age boundaries.
#' @param age_labels Character vector of age group labels.
#' @return List of condition specs for USE_ITN_AGE_* indicators.
#' @noRd
.itn_age_conditions <- function(age_breaks, age_labels) {
  conditions <- list()

  for (i in seq_along(age_labels)) {
    age_min <- age_breaks[i]
    age_max <- age_breaks[i + 1]
    label <- toupper(age_labels[i])
    code_label <- tolower(age_labels[i])

    # Build filter expression for age range
    if (is.infinite(age_max)) {
      filter_expr <- substitute(age >= amin, list(amin = age_min))
    } else {
      filter_expr <- substitute(
        age >= amin & age < amax,
        list(amin = age_min, amax = age_max)
      )
    }

    # Age label for description
    if (is.infinite(age_max)) {
      age_desc <- paste0(age_min, "+ years")
    } else {
      age_desc <- paste0(age_min, "-", age_max - 1, " years")
    }

    conditions <- c(conditions, list(list(
      indicator       = paste0("USE_ITN_AGE_", label),
      indicator_code  = paste0("use_itn_age_", code_label),
      indicator_title = paste0("Use ITN (", age_desc, ")"),
      data_level      = "person",
      outcome_var     = "itn_used",
      filter_expr     = filter_expr,
      est_method      = "ciprop",
      num_desc        = paste0("Use ITN (", age_desc, ")"),
      denom_desc      = paste0("Population aged ", age_desc),
      denom_code      = paste0("pop_age_", code_label)
    )))
  }

  conditions
}
