#' Calculate Core ITN Coverage and Use from DHS Data
#'
#' Core function that estimates ITN ownership, access, and usage indicators
#' from DHS Household Records (HR) and Person Records (PR) data. Implements
#' standard DHS methodology for calculating ITN coverage indicators. When GPS
#' data is provided, produces cluster-level results. Otherwise uses existing
#' administrative variables in the data.
#'
#' @param dhs_hr DHS Household Records dataset (HR) in tidy format
#'   (data.frame or tibble).
#' @param dhs_pr DHS Person Records dataset (PR) in tidy format
#'   (data.frame or tibble).
#' @param survey_vars Named list mapping DHS variable names. Required keys:
#'   \itemize{
#'     \item `cluster`: Cluster ID (default: "hv001")
#'     \item `weight`: Survey weight (default: "hv005")
#'     \item `stratum`: Stratum variable (default: "hv022")
#'     \item `hhid`: Household ID (default: "hhid")
#'     \item `adm1`: First administrative level (default: "hv024")
#'     \item `adm2`: Second administrative level (default: NULL)
#'     \item `hhsize`: Household size (default: "hv013")
#'     \item `age`: Age in years (default: "hv105")
#'     \item `sex`: Sex (default: "hv104")
#'     \item `pregnant`: Pregnancy status (default: "hml18")
#'     \item `itn_use`: Slept under ITN last night (default: "hml12")
#'     \item `itn_prefix`: Prefix for ITN variables in HR (default: "hml10_")
#'   }
#' @param gps_data Optional DHS GPS dataset. If provided, results are
#'   cluster-level with coordinates.
#' @param gps_vars Named list for GPS variables (cluster, lat, lon).
#' @param shapefile Optional sf object with administrative boundaries for
#'   spatial aggregation.
#' @param admin_level Character vector of admin columns in shapefile (e.g.,
#'   c("adm1", "adm2")). Auto-detected if NULL.
#' @param join_nearest Logical; if TRUE, assigns unmatched clusters to nearest
#'   polygon. Default TRUE.
#'
#' @return Tibble with ITN indicators by cluster (if GPS provided) or by
#'   existing administrative levels, including ownership, access, usage,
#'   and confidence intervals.
#'
#' @export
calc_itn_dhs_core <- function(
  dhs_hr,
  dhs_pr,
  survey_vars = list(
    cluster = "hv001",
    weight = "hv005",
    stratum = "hv022",
    hhid = "hhid",
    adm1 = "hv024",
    adm2 = NULL,
    hhsize = "hv013",
    age = "hv105",
    sex = "hv104",
    pregnant = "hml18",
    itn_use = "hml12",
    itn_prefix = "hml10_"
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
    cli::cli_abort("`dhs_hr` must be a data.frame or tibble.")
  }

  if (!is.data.frame(dhs_pr)) {
    cli::cli_abort("`dhs_pr` must be a data.frame or tibble.")
  }

  if (nrow(dhs_hr) == 0) {
    cli::cli_abort("`dhs_hr` is empty.")
  }

  if (nrow(dhs_pr) == 0) {
    cli::cli_abort("`dhs_pr` is empty.")
  }

  # required hr variables
  required_hr_vars <- c(
    survey_vars$cluster,
    survey_vars$weight,
    survey_vars$stratum,
    survey_vars$hhid,
    survey_vars$hhsize
  )

  if (!is.null(survey_vars$adm1)) {
    required_hr_vars <- c(required_hr_vars, survey_vars$adm1)
  }

  missing_hr_vars <- setdiff(required_hr_vars, names(dhs_hr))

  if (length(missing_hr_vars) > 0) {
    cli::cli_abort(
      c(
        "Required variables not found in HR data: {.var {missing_hr_vars}}",
        "i" = "Check your survey_vars mapping"
      )
    )
  }

  # required pr variables
  required_pr_vars <- c(
    survey_vars$cluster,
    survey_vars$weight,
    survey_vars$stratum,
    survey_vars$hhid,
    survey_vars$age,
    survey_vars$sex,
    survey_vars$itn_use
  )

  missing_pr_vars <- setdiff(required_pr_vars, names(dhs_pr))

  if (length(missing_pr_vars) > 0) {
    cli::cli_abort(
      c(
        "Required variables not found in PR data: {.var {missing_pr_vars}}",
        "i" = "Check your survey_vars mapping"
      )
    )
  }

  # itn net variables in hr
  itn_variable_names <- names(dhs_hr)[
    grepl(
      paste0("^", survey_vars$itn_prefix),
      names(dhs_hr)
    )
  ]

  if (length(itn_variable_names) == 0) {
    cli::cli_abort(
      c(
        "No ITN variables found with prefix {.var {survey_vars$itn_prefix}}",
        "i" = "Check that ITN net variables exist in HR data"
      )
    )
  }

  n_itn_vars <- length(itn_variable_names)
  cli::cli_alert_info(
    "Found {format(n_itn_vars, big.mark = ',')} ITN net variables in HR data"
  )

  # ---- 2. process hr data (household level) ---------------------------------

  # create core household dataset with survey fields and itn counts
  household_core_data <- dhs_hr |>
    dplyr::mutate(
      cluster_id = .data[[survey_vars$cluster]],
      survey_weight = .data[[survey_vars$weight]] / 1e6,
      stratum_id = .data[[survey_vars$stratum]],
      hhid = .data[[survey_vars$hhid]],
      hh_size = .data[[survey_vars$hhsize]],
      num_itns = dhs_hr |>
        dplyr::select(dplyr::all_of(itn_variable_names)) |>
        dplyr::mutate(
          dplyr::across(
            dplyr::everything(),
            ~ ifelse(. == 1, 1, 0)
          )
        ) |>
        base::rowSums(na.rm = TRUE)
    )

  # add admin variables if available
  if (
    !is.null(survey_vars$adm1) &&
      survey_vars$adm1 %in% names(dhs_hr)
  ) {
    household_core_data <- household_core_data |>
      dplyr::mutate(
        adm1 = haven::as_factor(.data[[survey_vars$adm1]]) |>
          base::as.character() |>
          toupper()
      )
  }

  if (
    !is.null(survey_vars$adm2) &&
      survey_vars$adm2 %in% names(dhs_hr)
  ) {
    household_core_data <- household_core_data |>
      dplyr::mutate(
        adm2 = haven::as_factor(.data[[survey_vars$adm2]]) |>
          base::as.character() |>
          toupper()
      )
  }

  # household-level itn indicators
  household_core_data <- household_core_data |>
    dplyr::mutate(
      hh_has_itn = base::as.integer(num_itns > 0),
      hh_sufficient_nets = base::as.integer(num_itns >= (hh_size / 2)),
      potential_users = base::pmin(num_itns * 2, hh_size)
    )

  n_households <- nrow(household_core_data)
  n_with_itn <- sum(household_core_data$hh_has_itn)
  cli::cli_alert_info(
    paste0(
      "Processed ", format(n_households, big.mark = ","),
      " households: ", format(n_with_itn, big.mark = ","),
      " with >=1 ITN"
    )
  )

  # ---- 3. process pr data (individual level) --------------------------------

  # core person-level dataset with survey fields and itn use
  person_core_data <- dhs_pr |>
    dplyr::mutate(
      cluster_id = .data[[survey_vars$cluster]],
      survey_weight = .data[[survey_vars$weight]] / 1e6,
      stratum_id = .data[[survey_vars$stratum]],
      hhid = .data[[survey_vars$hhid]],
      age = .data[[survey_vars$age]],
      sex = .data[[survey_vars$sex]],
      itn_used = dplyr::if_else(
        .data[[survey_vars$itn_use]] %in% c(1, 2),
        1,
        0,
        missing = 0
      )
    )

  # pregnancy status where available
  if (
    !is.null(survey_vars$pregnant) &&
      survey_vars$pregnant %in% names(dhs_pr)
  ) {
    person_core_data <- person_core_data |>
      dplyr::mutate(
        is_pregnant = dplyr::if_else(
          .data[[survey_vars$pregnant]] == 1,
          1,
          0,
          missing = 0
        )
      )
  } else {
    person_core_data <- person_core_data |>
      dplyr::mutate(is_pregnant = 0)
  }

  # age groups and special flags
  person_core_data <- person_core_data |>
    dplyr::mutate(
      age_group = dplyr::case_when(
        age < 5 ~ "Under 5",
        age >= 5 & age < 15 ~ "5-14 years",
        age >= 15 ~ "15+ years",
        TRUE ~ "Unknown"
      ),
      is_under5 = base::as.integer(age < 5),
      is_pregnant_woman = base::as.integer(
        sex == 2 & is_pregnant == 1
      )
    )

  # ---- 4. merge household and person data -----------------------------------

  # household itn info for each person
  household_itn_info <- household_core_data |>
    dplyr::select(
      hhid,
      hh_size,
      num_itns,
      hh_has_itn,
      hh_sufficient_nets,
      potential_users
    )

  person_merged_data <- person_core_data |>
    dplyr::left_join(household_itn_info, by = "hhid")

  # check join success
  join_success <- sum(!is.na(person_merged_data$hh_size))
  join_rate <- round(join_success / nrow(person_merged_data) * 100, 1)

  if (join_rate < 100) {
    cli::cli_alert_warning(
      paste0(
        "Household join: ", join_rate,
        "% of persons matched to households"
      )
    )
  }

  # individual access to itn
  # Create random values outside of mutate for proper evaluation
  n_persons <- nrow(person_merged_data)
  random_vals <- stats::runif(n_persons)

  person_merged_data <- person_merged_data |>
    dplyr::mutate(
      # Calculate access ratio - handle NA and zero hh_size
      itn_access_ratio = dplyr::if_else(
        !is.na(potential_users) & !is.na(hh_size) & hh_size > 0,
        pmin(potential_users / hh_size, 1),
        NA_real_,
        missing = NA_real_
      ),
      # Use pre-generated random values for access allocation
      itn_access = dplyr::if_else(
        itn_access_ratio >= random_vals,
        1L,
        0L,
        missing = 0L
      ),
      itn_use_if_access = dplyr::if_else(
        itn_access == 1 & itn_used == 1,
        1L,
        0L,
        missing = 0L
      )
    )

  # add admin variables if not present
  if (
    "adm1" %in%
      names(household_core_data) &&
      !"adm1" %in% names(person_merged_data)
  ) {
    admin_info <- household_core_data |>
      dplyr::select(
        hhid,
        dplyr::any_of(c("adm1", "adm2"))
      ) |>
      dplyr::distinct()

    person_merged_data <- person_merged_data |>
      dplyr::left_join(admin_info, by = "hhid")
  }

  n_individuals <- nrow(person_merged_data)
  n_used_itn <- sum(person_merged_data$itn_used)
  cli::cli_alert_info(
    paste0(
      "Processed ", format(n_individuals, big.mark = ","),
      " individuals: ", format(n_used_itn, big.mark = ","),
      " used ITN last night"
    )
  )

  # ---- 5. determine grouping logic ------------------------------------------

  grouping_vars <- NULL

  if (!is.null(gps_data) && !is.null(shapefile)) {
    # join admin levels from shapefile via GPS coordinates
    cli::cli_alert_info(
      "Joining GPS coordinates with administrative boundaries"
    )

    if (!requireNamespace("sf", quietly = TRUE)) {
      cli::cli_abort("Package 'sf' is required for spatial operations")
    }

    # prepare GPS data
    gps_clean <- gps_data |>
      dplyr::select(
        cluster_id = !!gps_vars$cluster,
        lat = !!gps_vars$lat,
        lon = !!gps_vars$lon
      ) |>
      dplyr::distinct()

    # add GPS to household data
    household_core_data <- household_core_data |>
      dplyr::left_join(gps_clean, by = "cluster_id")

    person_merged_data <- person_merged_data |>
      dplyr::left_join(gps_clean, by = "cluster_id")

    # create spatial points from clusters
    clusters_sf <- household_core_data |>
      dplyr::select(cluster_id, lat, lon) |>
      dplyr::distinct() |>
      dplyr::filter(!is.na(lat), !is.na(lon)) |>
      sf::st_as_sf(
        coords = c("lon", "lat"),
        crs = 4326
      )

    # prepare shapefile
    shapefile <- shapefile |>
      sf::st_transform(4326) |>
      sf::st_make_valid()

    # determine admin levels from shapefile
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
        paste0(
          "Using admin levels: ",
          paste(admin_level, collapse = ", ")
        )
      )
    }

    # check admin columns exist
    missing_cols <- setdiff(admin_level, names(shapefile))
    if (length(missing_cols) > 0) {
      cli::cli_abort(
        paste0(
          "Admin columns not found in shapefile: ",
          paste(missing_cols, collapse = ", ")
        )
      )
    }

    # get admin name columns if available
    admin_name_cols <- paste0(admin_level, "_name")
    admin_name_cols <- admin_name_cols[
      admin_name_cols %in% names(shapefile)
    ]
    all_admin_cols <- c(admin_level, admin_name_cols)

    # spatial join - assign admin levels to each cluster
    cluster_admin <- sf::st_join(
      clusters_sf,
      shapefile[, c(all_admin_cols, "geometry")],
      join = sf::st_within,
      left = TRUE
    )

    # assign unmatched clusters to nearest admin unit
    if (join_nearest) {
      unmatched <- is.na(cluster_admin[[admin_level[1]]])

      if (any(unmatched)) {
        n_unmatched <- format(sum(unmatched), big.mark = ",")
        cli::cli_alert_info(
          paste0("Assigning ", n_unmatched, " clusters to nearest polygons")
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

    # convert to dataframe and join back to main datasets
    cluster_admin_df <- sf::st_drop_geometry(cluster_admin) |>
      dplyr::select(cluster_id, dplyr::all_of(all_admin_cols))

    # remove any existing admin columns that conflict with shapefile columns
    conflicting_cols <- intersect(
      names(household_core_data),
      all_admin_cols
    )

    if (length(conflicting_cols) > 0) {
      household_core_data <- household_core_data |>
        dplyr::select(-dplyr::all_of(conflicting_cols))

      person_merged_data <- person_merged_data |>
        dplyr::select(-dplyr::all_of(conflicting_cols))
    }

    household_core_data <- household_core_data |>
      dplyr::left_join(cluster_admin_df, by = "cluster_id")

    person_merged_data <- person_merged_data |>
      dplyr::left_join(cluster_admin_df, by = "cluster_id")

    # use admin levels for grouping
    grouping_vars <- admin_level

    cli::cli_alert_info(
      paste0(
        "Calculating ITN indicators by ",
        paste(admin_level, collapse = " + ")
      )
    )
  } else if (!is.null(gps_data)) {
    # cluster-level with gps only (no shapefile)
    gps_clean <- gps_data |>
      dplyr::select(
        cluster_id = !!gps_vars$cluster,
        lat = !!gps_vars$lat,
        lon = !!gps_vars$lon
      ) |>
      dplyr::distinct()

    household_core_data <- household_core_data |>
      dplyr::left_join(gps_clean, by = "cluster_id")

    person_merged_data <- person_merged_data |>
      dplyr::left_join(gps_clean, by = "cluster_id")

    grouping_vars <- "cluster_id"

    cli::cli_alert_info(
      "Calculating cluster-level ITN indicators"
    )
  } else if (
    "adm2" %in%
      names(household_core_data) &&
      "adm1" %in% names(household_core_data)
  ) {
    grouping_vars <- c("adm1", "adm2")

    cli::cli_alert_info(
      "Using admin levels: adm1 and adm2"
    )
  } else if ("adm1" %in% names(household_core_data)) {
    grouping_vars <- "adm1"

    cli::cli_alert_info(
      "Using administrative level: adm1"
    )
  } else {
    cli::cli_alert_info(
      "Calculating national-level ITN indicators"
    )
  }

  # ---- 6. set up survey design ----------------------------------------------

  # always set lonely.psu option - subsets may create single PSU strata
  survey_options <- base::options(
    survey.lonely.psu = "certainty"
  )

  base::on.exit(
    base::options(survey_options),
    add = TRUE
  )

  strata_household_summary <- household_core_data |>
    dplyr::group_by(stratum_id) |>
    dplyr::summarise(
      n_clusters = dplyr::n_distinct(cluster_id),
      .groups = "drop"
    )

  single_psu_strata_count <- base::sum(
    strata_household_summary$n_clusters == 1
  )

  if (single_psu_strata_count > 0) {
    n_single <- format(single_psu_strata_count, big.mark = ",")
    cli::cli_alert_info(
      paste0("Found ", n_single, " strata with single PSU; ",
             "using certainty option")
    )
  }

  use_strata <- dplyr::n_distinct(household_core_data$stratum_id) > 1

  if (use_strata) {
    design_household <- survey::svydesign(
      ids = ~cluster_id,
      strata = ~stratum_id,
      weights = ~survey_weight,
      data = household_core_data,
      nest = TRUE
    )
  } else {
    design_household <- survey::svydesign(
      ids = ~cluster_id,
      weights = ~survey_weight,
      data = household_core_data,
      nest = TRUE
    )
  }

  if (use_strata) {
    design_person <- survey::svydesign(
      ids = ~cluster_id,
      strata = ~stratum_id,
      weights = ~survey_weight,
      data = person_merged_data,
      nest = TRUE
    )
  } else {
    design_person <- survey::svydesign(
      ids = ~cluster_id,
      weights = ~survey_weight,
      data = person_merged_data,
      nest = TRUE
    )
  }

  # ---- 7. calculate itn indicators ------------------------------------------

  if (!is.null(grouping_vars)) {
    grouping_formula <- stats::as.formula(
      paste("~", paste(grouping_vars, collapse = " + "))
    )
  } else {
    grouping_formula <- ~1
  }

  # 7a. household ownership (>=1 itn)
  if (!is.null(grouping_vars)) {
    household_ownership <- survey::svyby(
      ~hh_has_itn,
      by = grouping_formula,
      design = design_household,
      FUN = survey::svymean,
      vartype = "ci",
      keep.names = FALSE
    ) |>
      tibble::as_tibble() |>
      dplyr::rename(
        ci_l.hh_has_itn = ci_l,
        ci_u.hh_has_itn = ci_u
      )
  } else {
    household_ownership_mean <- survey::svymean(
      ~hh_has_itn,
      design = design_household
    )

    household_ownership_ci <- stats::confint(
      household_ownership_mean
    )

    household_ownership <- tibble::tibble(
      level = "National",
      hh_has_itn = base::as.numeric(
        household_ownership_mean
      ),
      ci_l.hh_has_itn = household_ownership_ci[1, 1],
      ci_u.hh_has_itn = household_ownership_ci[1, 2]
    )
  }

  # 7b. household with sufficient nets
  if (!is.null(grouping_vars)) {
    household_sufficient <- survey::svyby(
      ~hh_sufficient_nets,
      by = grouping_formula,
      design = design_household,
      FUN = survey::svymean,
      vartype = "ci",
      keep.names = FALSE
    ) |>
      tibble::as_tibble() |>
      dplyr::rename(
        ci_l.hh_sufficient_nets = ci_l,
        ci_u.hh_sufficient_nets = ci_u
      )
  } else {
    household_sufficient_mean <- survey::svymean(
      ~hh_sufficient_nets,
      design = design_household
    )

    household_sufficient_ci <- stats::confint(
      household_sufficient_mean
    )

    household_sufficient <- tibble::tibble(
      level = "National",
      hh_sufficient_nets = base::as.numeric(
        household_sufficient_mean
      ),
      ci_l.hh_sufficient_nets = household_sufficient_ci[1, 1],
      ci_u.hh_sufficient_nets = household_sufficient_ci[1, 2]
    )
  }

  # 7c. population with access to itn
  if (!is.null(grouping_vars)) {
    population_access <- survey::svyby(
      ~itn_access_ratio,
      by = grouping_formula,
      design = design_person,
      FUN = survey::svymean,
      vartype = "ci",
      keep.names = FALSE,
      na.rm = TRUE
    ) |>
      tibble::as_tibble() |>
      dplyr::rename(
        ci_l.itn_access_ratio = ci_l,
        ci_u.itn_access_ratio = ci_u
      )
  } else {
    population_access_mean <- survey::svymean(
      ~itn_access_ratio,
      design = design_person,
      na.rm = TRUE
    )

    population_access_ci <- stats::confint(
      population_access_mean
    )

    population_access <- tibble::tibble(
      level = "National",
      itn_access_ratio = base::as.numeric(
        population_access_mean
      ),
      ci_l.itn_access_ratio = population_access_ci[1, 1],
      ci_u.itn_access_ratio = population_access_ci[1, 2]
    )
  }

  # 7d. population that used itn last night
  if (!is.null(grouping_vars)) {
    population_use <- survey::svyby(
      ~itn_used,
      by = grouping_formula,
      design = design_person,
      FUN = survey::svymean,
      vartype = "ci",
      keep.names = FALSE
    ) |>
      tibble::as_tibble() |>
      dplyr::rename(
        ci_l.itn_used = ci_l,
        ci_u.itn_used = ci_u
      )
  } else {
    population_use_mean <- survey::svymean(
      ~itn_used,
      design = design_person
    )

    population_use_ci <- stats::confint(
      population_use_mean
    )

    population_use <- tibble::tibble(
      level = "National",
      itn_used = base::as.numeric(
        population_use_mean
      ),
      ci_l.itn_used = population_use_ci[1, 1],
      ci_u.itn_used = population_use_ci[1, 2]
    )
  }

  # 7e. children under 5 who used itn
  person_under5_data <- person_merged_data |>
    dplyr::filter(is_under5 == 1)

  if (nrow(person_under5_data) > 0) {
    if (use_strata) {
      design_u5 <- survey::svydesign(
        ids = ~cluster_id,
        strata = ~stratum_id,
        weights = ~survey_weight,
        data = person_under5_data,
        nest = TRUE
      )
    } else {
      design_u5 <- survey::svydesign(
        ids = ~cluster_id,
        weights = ~survey_weight,
        data = person_under5_data,
        nest = TRUE
      )
    }

    if (!is.null(grouping_vars)) {
      u5_use <- survey::svyby(
        ~itn_used,
        by = grouping_formula,
        design = design_u5,
        FUN = survey::svymean,
        vartype = "ci",
        keep.names = FALSE
      ) |>
        tibble::as_tibble() |>
        dplyr::rename(
          itn_used_u5 = itn_used,
          ci_l.itn_used_u5 = ci_l,
          ci_u.itn_used_u5 = ci_u
        )
    } else {
      u5_use_mean <- survey::svymean(
        ~itn_used,
        design = design_u5
      )

      u5_use_ci <- stats::confint(u5_use_mean)

      u5_use <- tibble::tibble(
        level = "National",
        itn_used_u5 = base::as.numeric(
          u5_use_mean
        ),
        ci_l.itn_used_u5 = u5_use_ci[1, 1],
        ci_u.itn_used_u5 = u5_use_ci[1, 2]
      )
    }
  } else {
    u5_use <- NULL
  }

  # 7f. pregnant women who used itn
  person_pregnant_data <- person_merged_data |>
    dplyr::filter(is_pregnant_woman == 1)

  if (nrow(person_pregnant_data) > 0) {
    if (use_strata) {
      design_pregnant <- survey::svydesign(
        ids = ~cluster_id,
        strata = ~stratum_id,
        weights = ~survey_weight,
        data = person_pregnant_data,
        nest = TRUE
      )
    } else {
      design_pregnant <- survey::svydesign(
        ids = ~cluster_id,
        weights = ~survey_weight,
        data = person_pregnant_data,
        nest = TRUE
      )
    }

    if (!is.null(grouping_vars)) {
      preg_use <- tryCatch(
        {
          survey::svyby(
            ~itn_used,
            by = grouping_formula,
            design = design_pregnant,
            FUN = survey::svymean,
            vartype = "ci",
            keep.names = FALSE
          ) |>
            tibble::as_tibble() |>
            dplyr::rename(
              itn_used_preg = itn_used,
              ci_l.itn_used_preg = ci_l,
              ci_u.itn_used_preg = ci_u
            )
        },
        error = function(e) {
          cli::cli_alert_warning(
            paste0(
              "Could not calculate ITN use for pregnant women: ",
              conditionMessage(e)
            )
          )
          NULL
        }
      )
    } else {
      preg_use_mean <- survey::svymean(
        ~itn_used,
        design = design_pregnant
      )

      preg_use_ci <- stats::confint(preg_use_mean)

      preg_use <- tibble::tibble(
        level = "National",
        itn_used_preg = base::as.numeric(
          preg_use_mean
        ),
        ci_l.itn_used_preg = preg_use_ci[1, 1],
        ci_u.itn_used_preg = preg_use_ci[1, 2]
      )
    }
  } else {
    preg_use <- NULL
  }

  # ---- 8. calculate sample sizes --------------------------------------------

  if (!is.null(grouping_vars)) {
    household_samples <- household_core_data |>
      dplyr::group_by(
        dplyr::across(dplyr::all_of(grouping_vars))
      ) |>
      dplyr::summarise(
        dhs_n_households = dplyr::n(),
        dhs_n_hh_with_itn = base::sum(
          hh_has_itn,
          na.rm = TRUE
        ),
        dhs_n_hh_sufficient = base::sum(
          hh_sufficient_nets,
          na.rm = TRUE
        ),
        .groups = "drop"
      )

    individual_samples <- person_merged_data |>
      dplyr::group_by(
        dplyr::across(dplyr::all_of(grouping_vars))
      ) |>
      dplyr::summarise(
        dhs_n_individuals = dplyr::n(),
        dhs_n_with_access = base::sum(
          itn_access,
          na.rm = TRUE
        ),
        dhs_n_used_itn = base::sum(
          itn_used,
          na.rm = TRUE
        ),
        dhs_n_under5 = base::sum(
          is_under5,
          na.rm = TRUE
        ),
        dhs_n_under5_used = base::sum(
          is_under5 * itn_used,
          na.rm = TRUE
        ),
        dhs_n_pregnant = base::sum(
          is_pregnant_woman,
          na.rm = TRUE
        ),
        dhs_n_pregnant_used = base::sum(
          is_pregnant_woman * itn_used,
          na.rm = TRUE
        ),
        .groups = "drop"
      )
  } else {
    household_samples <- tibble::tibble(
      level = "National",
      dhs_n_households = nrow(household_core_data),
      dhs_n_hh_with_itn = base::sum(
        household_core_data$hh_has_itn,
        na.rm = TRUE
      ),
      dhs_n_hh_sufficient = base::sum(
        household_core_data$hh_sufficient_nets,
        na.rm = TRUE
      )
    )

    individual_samples <- tibble::tibble(
      level = "National",
      dhs_n_individuals = nrow(person_merged_data),
      dhs_n_with_access = base::sum(
        person_merged_data$itn_access,
        na.rm = TRUE
      ),
      dhs_n_used_itn = base::sum(
        person_merged_data$itn_used,
        na.rm = TRUE
      ),
      dhs_n_under5 = base::sum(
        person_merged_data$is_under5,
        na.rm = TRUE
      ),
      dhs_n_under5_used = base::sum(
        person_merged_data$is_under5 * person_merged_data$itn_used,
        na.rm = TRUE
      ),
      dhs_n_pregnant = base::sum(
        person_merged_data$is_pregnant_woman,
        na.rm = TRUE
      ),
      dhs_n_pregnant_used = base::sum(
        person_merged_data$is_pregnant_woman * person_merged_data$itn_used,
        na.rm = TRUE
      )
    )
  }

  # ---- 9. combine results ----------------------------------------------------

  if (!is.null(grouping_vars)) {
    itn_results <- household_ownership |>
      dplyr::left_join(
        household_sufficient,
        by = grouping_vars
      ) |>
      dplyr::left_join(
        population_access,
        by = grouping_vars
      ) |>
      dplyr::left_join(
        population_use,
        by = grouping_vars
      )

    if (!is.null(u5_use)) {
      itn_results <- itn_results |>
        dplyr::left_join(u5_use, by = grouping_vars)
    }

    if (!is.null(preg_use)) {
      itn_results <- itn_results |>
        dplyr::left_join(preg_use, by = grouping_vars)
    }

    itn_results <- itn_results |>
      dplyr::left_join(
        household_samples,
        by = grouping_vars
      ) |>
      dplyr::left_join(
        individual_samples,
        by = grouping_vars
      )
  } else {
    itn_results <- household_ownership |>
      dplyr::bind_cols(
        household_sufficient |>
          dplyr::select(-level)
      ) |>
      dplyr::bind_cols(
        population_access |>
          dplyr::select(-level)
      ) |>
      dplyr::bind_cols(
        population_use |>
          dplyr::select(-level)
      )

    if (!is.null(u5_use)) {
      itn_results <- itn_results |>
        dplyr::bind_cols(
          u5_use |>
            dplyr::select(-level)
        )
    }

    if (!is.null(preg_use)) {
      itn_results <- itn_results |>
        dplyr::bind_cols(
          preg_use |>
            dplyr::select(-level)
        )
    }

    itn_results <- itn_results |>
      dplyr::bind_cols(
        household_samples |>
          dplyr::select(-level)
      ) |>
      dplyr::bind_cols(
        individual_samples |>
          dplyr::select(-level)
      )
  }

  # ---- 10. format results ----------------------------------------------------

  rename_map <- list(
    dhs_itn_ownership = "hh_has_itn",
    dhs_itn_ownership_low = "ci_l.hh_has_itn",
    dhs_itn_ownership_upp = "ci_u.hh_has_itn",
    dhs_itn_sufficient = "hh_sufficient_nets",
    dhs_itn_sufficient_low = "ci_l.hh_sufficient_nets",
    dhs_itn_sufficient_upp = "ci_u.hh_sufficient_nets",
    dhs_itn_access = "itn_access_ratio",
    dhs_itn_access_low = "ci_l.itn_access_ratio",
    dhs_itn_access_upp = "ci_u.itn_access_ratio",
    dhs_itn_use = "itn_used",
    dhs_itn_use_low = "ci_l.itn_used",
    dhs_itn_use_upp = "ci_u.itn_used"
  )

  for (new_name in names(rename_map)) {
    old_name <- rename_map[[new_name]]

    if (old_name %in% names(itn_results)) {
      itn_results <- itn_results |>
        dplyr::rename(!!new_name := !!old_name)
    }
  }

  if ("itn_used_u5" %in% names(itn_results)) {
    itn_results <- itn_results |>
      dplyr::rename(
        dhs_itn_use_u5 = itn_used_u5,
        dhs_itn_use_u5_low = ci_l.itn_used_u5,
        dhs_itn_use_u5_upp = ci_u.itn_used_u5
      )
  }

  if ("itn_used_preg" %in% names(itn_results)) {
    itn_results <- itn_results |>
      dplyr::rename(
        dhs_itn_use_preg = itn_used_preg,
        dhs_itn_use_preg_low = ci_l.itn_used_preg,
        dhs_itn_use_preg_upp = ci_u.itn_used_preg
      )
  }

  itn_indicator_cols <- names(itn_results)[
    grepl("^dhs_itn_", names(itn_results))
  ]

  itn_results <- itn_results |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(itn_indicator_cols),
        ~ round(.x * 100, 1)
      )
    )

  itn_results <- itn_results |>
    dplyr::mutate(
      dplyr::across(
        dplyr::matches("_low$"),
        ~ base::pmax(0, .)
      ),
      dplyr::across(
        dplyr::matches("_upp$"),
        ~ base::pmin(100, .)
      )
    )

  # ---- 11. attach gps coordinates if provided -------------------------------

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

    itn_results <- itn_results |>
      dplyr::left_join(gps_clean, by = "cluster_id")
  }

  column_order <- c(
    # admin levels
    grouping_vars,
    "lat",
    "lon",
    # main percentage indicators
    "dhs_itn_ownership",
    "dhs_itn_sufficient",
    "dhs_itn_access",
    "dhs_itn_use",
    "dhs_itn_use_u5",
    "dhs_itn_use_preg",
    # sample sizes
    "dhs_n_households",
    "dhs_n_individuals",
    "dhs_n_under5",
    "dhs_n_pregnant",
    "dhs_n_hh_with_itn",
    "dhs_n_hh_sufficient",
    "dhs_n_with_access",
    "dhs_n_used_itn",
    "dhs_n_under5_used",
    "dhs_n_pregnant_used",
    # confidence intervals
    "dhs_itn_ownership_low",
    "dhs_itn_ownership_upp",
    "dhs_itn_sufficient_low",
    "dhs_itn_sufficient_upp",
    "dhs_itn_access_low",
    "dhs_itn_access_upp",
    "dhs_itn_use_low",
    "dhs_itn_use_upp",
    "dhs_itn_use_u5_low",
    "dhs_itn_use_u5_upp",
    "dhs_itn_use_preg_low",
    "dhs_itn_use_preg_upp"
  )

  column_order <- base::intersect(
    column_order,
    names(itn_results)
  )

  other_columns <- base::setdiff(
    names(itn_results),
    column_order
  )

  itn_results <- itn_results |>
    dplyr::select(
      dplyr::all_of(
        c(column_order, other_columns)
      )
    )

  tibble::as_tibble(itn_results)
}

#' Extract metadata from DHS datasets for ITN analysis
#'
#' Internal function to extract survey metadata from DHS Household Records
#' and Person Records data. Looks for standard DHS metadata columns and
#' extracts key survey information relevant to ITN coverage analysis.
#'
#' @param dhs_hr DHS Household Records dataset.
#' @param dhs_pr DHS Person Records dataset.
#' @param survey_vars Named list of survey variable mappings.
#'
#' @return List containing survey metadata.
#' @noRd
extract_dhs_metadata_itn <- function(
  dhs_hr,
  dhs_pr,
  survey_vars = NULL
) {
  metadata <- list()

  # extract country code
  if ("v000" %in% names(dhs_pr)) {
    metadata$country_code <- unique(dhs_pr$v000)[1]
  } else if ("hv000" %in% names(dhs_pr)) {
    metadata$country_code <- unique(dhs_pr$hv000)[1]
  } else if ("hv000" %in% names(dhs_hr)) {
    metadata$country_code <- unique(dhs_hr$hv000)[1]
  } else if ("country_code" %in% names(dhs_pr)) {
    metadata$country_code <- unique(dhs_pr$country_code)[1]
  } else {
    metadata$country_code <- NA_character_
  }

  # extract survey year
  if ("v007" %in% names(dhs_pr)) {
    metadata$survey_year <- unique(dhs_pr$v007)[1]
  } else if ("hv007" %in% names(dhs_pr)) {
    metadata$survey_year <- unique(dhs_pr$hv007)[1]
  } else if ("hv007" %in% names(dhs_hr)) {
    metadata$survey_year <- unique(dhs_hr$hv007)[1]
  } else if ("survey_year" %in% names(dhs_pr)) {
    metadata$survey_year <- unique(dhs_pr$survey_year)[1]
  } else {
    metadata$survey_year <- NA_integer_
  }

  # extract survey id
  if ("survey_id" %in% names(dhs_pr)) {
    metadata$survey_id <- unique(dhs_pr$survey_id)[1]
  } else if ("v000" %in% names(dhs_pr)) {
    metadata$survey_id <- unique(dhs_pr$v000)[1]
  } else if ("hv000" %in% names(dhs_pr)) {
    metadata$survey_id <- unique(dhs_pr$hv000)[1]
  } else if ("hv000" %in% names(dhs_hr)) {
    metadata$survey_id <- unique(dhs_hr$hv000)[1]
  } else {
    metadata$survey_id <- NA_character_
  }

  metadata$survey_type <- "DHS"
  metadata$file_type <- "HR+PR"

  metadata$total_households <- nrow(dhs_hr)
  metadata$total_individuals <- nrow(dhs_pr)

  # number of clusters
  cluster_var <- if (!is.null(survey_vars$cluster)) {
    survey_vars$cluster
  } else {
    "hv001"
  }

  if (cluster_var %in% names(dhs_hr)) {
    metadata$total_clusters <- length(unique(dhs_hr[[cluster_var]]))
  }

  # number of itn variables
  itn_prefix <- if (!is.null(survey_vars$itn_prefix)) {
    survey_vars$itn_prefix
  } else {
    "hml10_"
  }

  itn_cols <- names(dhs_hr)[
    grepl(paste0("^", itn_prefix), names(dhs_hr))
  ]

  metadata$n_itn_variables <- length(itn_cols)

  metadata$has_itn_ownership <- metadata$n_itn_variables > 0
  metadata$has_itn_use <- !is.null(survey_vars$itn_use) &&
    survey_vars$itn_use %in% names(dhs_pr)

  metadata$has_pregnancy_data <- !is.null(survey_vars$pregnant) &&
    survey_vars$pregnant %in% names(dhs_pr)

  metadata$processed_date <- Sys.Date()
  metadata$processed_time <- Sys.time()

  metadata$analysis_type <- "ITN Coverage and Use"

  metadata$indicators <- c(
    "Household ownership",
    "Household sufficiency",
    "Population access",
    "Population use",
    "Under-5 use",
    if (metadata$has_pregnancy_data) "Pregnant women use" else NULL
  )

  metadata$variable_mapping <- survey_vars

  metadata
}

#' Calculate ITN Coverage and Use from DHS Data with Spatial Aggregation Support
#'
#' Main function for calculating ITN ownership, access, and usage indicators
#' from DHS Household Records (HR) and Person Records (PR) data. When a
#' shapefile is provided, calculations are performed directly at the
#' administrative level for better statistical precision.
#'
#' @param dhs_hr DHS Household Records dataset (HR) in tidy format.
#' @param dhs_pr DHS Person Records dataset (PR) in tidy format.
#' @param survey_vars Named list mapping DHS variable names.
#' @param gps_data Optional DHS GPS dataset for spatial joins.
#' @param gps_vars Named list for GPS variables (cluster, lat, lon).
#' @param shapefile Optional sf object with administrative boundaries.
#' @param admin_level Character vector of admin columns in shapefile.
#' @param join_nearest Logical; assign unmatched clusters to nearest polygon.
#'
#' @return List containing:
#'   \itemize{
#'     \item data: Tibble with ITN indicators
#'     \item dict: Data dictionary
#'     \item metadata: Survey metadata
#'   }
#'
#' @export
calc_itn_dhs <- function(
  dhs_hr,
  dhs_pr,
  survey_vars = list(
    cluster = "hv001",
    weight = "hv005",
    stratum = "hv022",
    hhid = "hhid",
    adm1 = "hv024",
    adm2 = NULL,
    hhsize = "hv013",
    age = "hv105",
    sex = "hv104",
    pregnant = "hml18",
    itn_use = "hml12",
    itn_prefix = "hml10_"
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
  # extract metadata
  metadata <- extract_dhs_metadata_itn(
    dhs_hr = dhs_hr,
    dhs_pr = dhs_pr,
    survey_vars = survey_vars
  )

  # validate shapefile if provided
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

  # compute indicators (core handles spatial join if shapefile provided)
  itn_results <- calc_itn_dhs_core(
    dhs_hr = dhs_hr,
    dhs_pr = dhs_pr,
    survey_vars = survey_vars,
    gps_data = gps_data,
    gps_vars = gps_vars,
    shapefile = shapefile,
    admin_level = admin_level,
    join_nearest = join_nearest
  )

  # update metadata with aggregation info
  if (!is.null(shapefile)) {
    # admin_level may have been auto-detected in core function
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

  list(
    data = itn_results,
    dict = sntutils::build_dictionary(itn_results),
    metadata = metadata
  )
}

#' Aggregate ITN indicators to administrative levels
#'
#' Helper function to aggregate ITN results to administrative levels using a
#' shapefile. Performs spatial joins and calculates weighted averages by
#' administrative unit.
#'
#' @param itn_results ITN results from calc_itn_dhs_core with lat/lon columns.
#' @param shapefile sf object with administrative boundaries.
#' @param admin_level Character vector of admin columns (default: "adm1").
#' @param weighted Logical; use weighted means by household count (default:
#'   TRUE).
#'
#' @return sf object with aggregated ITN indicators by administrative level.
#'
#' @export
aggregate_itn_admin <- function(
  itn_results,
  shapefile,
  admin_level = c("adm1"),
  weighted = TRUE
) {
  if (!requireNamespace("sf", quietly = TRUE)) {
    cli::cli_abort("Package `sf` is required for spatial operations.")
  }

  # convert to sf if needed
  if (!inherits(itn_results, "sf")) {
    if (!all(c("lat", "lon") %in% names(itn_results))) {
      cli::cli_abort(
        "itn_results must have lat and lon for spatial aggregation."
      )
    }

    itn_sf <- itn_results |>
      sf::st_as_sf(
        coords = c("lon", "lat"),
        crs = 4326,
        remove = FALSE
      )
  } else {
    itn_sf <- itn_results
  }

  shapefile <- shapefile |>
    sf::st_transform(4326) |>
    sf::st_make_valid()

  # spatial join
  joined <- sf::st_join(
    itn_sf,
    shapefile[, c(admin_level, "geometry")],
    join = sf::st_within,
    left = TRUE
  )

  # fix unmatched
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

  itn_cols <- names(joined_df)[grepl("^dhs_itn_", names(joined_df))]
  sample_cols <- names(joined_df)[grepl("^dhs_n_", names(joined_df))]

  # aggregate
  if (weighted && "dhs_n_households" %in% names(joined_df)) {
    aggregated <- joined_df |>
      dplyr::group_by(dplyr::across(dplyr::all_of(admin_level))) |>
      dplyr::summarise(
        dplyr::across(
          dplyr::all_of(itn_cols),
          ~ stats::weighted.mean(., w = dhs_n_households, na.rm = TRUE)
        ),
        dplyr::across(
          dplyr::all_of(sample_cols),
          ~ sum(., na.rm = TRUE)
        ),
        .groups = "drop"
      )
  } else {
    aggregated <- joined_df |>
      dplyr::group_by(dplyr::across(dplyr::all_of(admin_level))) |>
      dplyr::summarise(
        dplyr::across(
          dplyr::all_of(itn_cols),
          ~ mean(., na.rm = TRUE)
        ),
        dplyr::across(
          dplyr::all_of(sample_cols),
          ~ sum(., na.rm = TRUE)
        ),
        .groups = "drop"
      )
  }

  aggregated <- aggregated |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(itn_cols),
        ~ round(., 1)
      )
    )

  admin_name_cols <- paste0(admin_level, "_name")
  admin_name_cols <- admin_name_cols[
    admin_name_cols %in% names(shapefile)
  ]

  result <- shapefile |>
    dplyr::select(dplyr::all_of(c(admin_level, admin_name_cols))) |>
    dplyr::distinct() |>
    dplyr::left_join(aggregated, by = admin_level)

  result
}
