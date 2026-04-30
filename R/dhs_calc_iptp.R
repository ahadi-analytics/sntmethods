#' Calculate Core IPTp Coverage from DHS Data
#'
#' Core function that estimates IPTp (Intermittent Preventive Treatment in
#' pregnancy) coverage indicators from DHS Individual Recode (IR) data.
#' Implements standard DHS methodology for calculating IPTp indicators among
#' women with a recent birth. When GPS data is provided, produces cluster-level
#' results. Otherwise uses existing administrative variables in the data.
#'
#' @details
#' Methodology: \url{https://github.com/ahadi-analytics/sntmethods/blob/master/inst/methods/iptp_dhs.yml}
#'
#' @param dhs_ir DHS Individual Recode dataset (IR) in tidy format
#'   (data.frame or tibble).
#' @param survey_vars Named list mapping DHS variable names. Required keys:
#'   \itemize{
#'     \item `cluster`: Cluster ID (default: "v001")
#'     \item `weight`: Survey weight (default: "v005")
#'     \item `stratum`: Stratum variable (default: "v022")
#'     \item `adm1`: First administrative level (default: "v024")
#'     \item `adm2`: Second administrative level (default: NULL)
#'     \item `interview_cmc`: Interview date in CMC (default: "v008")
#'     \item `birth_cmc`: Birth date of most recent child (default: "b3_01")
#'     \item `birth_age_months`: Age of most recent child in months
#'       (default: "b19_01")
#'     \item `sp_taken`: Whether took SP/Fansidar during pregnancy, 1=yes
#'       (default: "m49a_1")
#'     \item `sp_doses`: Number of SP/Fansidar doses (default: "ml1_1")
#'   }
#' @param birth_window_months Maximum age of most recent birth in months.
#'   Default 24 (DHS standard). Women with births older than this are excluded.
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
#' @return Tibble with IPTp indicators by cluster (if GPS provided) or by
#'   existing administrative levels, including IPTp 1+, 2+, 3+ coverage
#'   and confidence intervals.
#'
#' @keywords internal
calc_iptp_dhs_core <- function(
  dhs_ir,
  survey_vars = list(
    cluster = "v001",
    weight = "v005",
    stratum = "v022",
    adm1 = "v024",
    adm2 = NULL,
    interview_cmc = "v008",
    birth_cmc = "b3_01",
    birth_age_months = "b19_01",
    sp_taken = "m49a_1",
    sp_doses = "ml1_1"
  ),
  birth_window_months = 24,

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

  if (!is.data.frame(dhs_ir)) {
    cli::cli_abort("`dhs_ir` must be a data.frame or tibble.")
  }

  if (nrow(dhs_ir) == 0) {
    cli::cli_abort("`dhs_ir` is empty.")
  }

  # required ir variables
  required_ir_vars <- c(
    survey_vars$cluster,
    survey_vars$weight,
    survey_vars$stratum,
    survey_vars$interview_cmc,
    survey_vars$sp_taken,
    survey_vars$sp_doses
  )

  # birth_cmc is required if birth_age_months is not available
  if (!is.null(survey_vars$adm1)) {
    required_ir_vars <- c(required_ir_vars, survey_vars$adm1)
  }

  missing_ir_vars <- setdiff(required_ir_vars, names(dhs_ir))

  if (length(missing_ir_vars) > 0) {
    cli::cli_abort(
      c(
        "Required variables not found in IR data: {.var {missing_ir_vars}}",
        "i" = "Check your survey_vars mapping"
      )
    )
  }

  # ---- 2. Prepare base dataset -----------------------------------------------

  ir_eligible <- .prepare_iptp_data(
    dhs_ir = dhs_ir,
    survey_vars = survey_vars,
    birth_window_months = birth_window_months,
    include_survey_vars = TRUE
  )

  # Rename helper indicators to match downstream column names
  ir_eligible <- ir_eligible |>
    dplyr::mutate(
      iptp_1plus = as.integer(has_1plus),
      iptp_2plus = as.integer(has_2plus),
      iptp_3plus = as.integer(has_3plus),
      iptp_4plus = as.integer(has_4plus),
      iptp_1only = as.integer(has_1only),
      iptp_2only = as.integer(has_2only),
      iptp_3only = as.integer(has_3only)
    )

  # Add adm2 if available and not already present
  if (
    !is.null(survey_vars$adm2) &&
      survey_vars$adm2 %in% names(dhs_ir) &&
      !"adm2" %in% names(ir_eligible)
  ) {
    # Zap labels and add adm2 from original data
    adm2_vals <- haven::zap_labels(dhs_ir[[survey_vars$adm2]])
    # The helper already filtered rows, so we need to match by row
    # Instead, re-derive from the original data columns preserved by helper
    ir_eligible <- ir_eligible |>
      dplyr::mutate(
        adm2 = haven::as_factor(.data[[survey_vars$adm2]]) |>
          base::as.character() |>
          toupper()
      )
  }

  n_eligible <- nrow(ir_eligible)
  n_iptp1 <- sum(ir_eligible$iptp_1plus == 1, na.rm = TRUE)
  cli::cli_alert_info(
    paste0(
      "IPTp coverage: ", format(n_iptp1, big.mark = ","),
      " (", round(n_iptp1 / n_eligible * 100, 1), "%) received 1+ doses"
    )
  )

  # ---- 5. determine grouping logic -------------------------------------------

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

    # add GPS to data
    ir_eligible <- ir_eligible |>
      dplyr::left_join(gps_clean, by = "cluster_id")

    # create spatial points from clusters
    clusters_sf <- ir_eligible |>
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

    # convert to dataframe and join back to main dataset
    cluster_admin_df <- sf::st_drop_geometry(cluster_admin) |>
      dplyr::select(cluster_id, dplyr::all_of(all_admin_cols))

    # remove any existing admin columns that conflict with shapefile columns
    conflicting_cols <- intersect(
      names(ir_eligible),
      all_admin_cols
    )

    if (length(conflicting_cols) > 0) {
      ir_eligible <- ir_eligible |>
        dplyr::select(-dplyr::all_of(conflicting_cols))
    }

    ir_eligible <- ir_eligible |>
      dplyr::left_join(cluster_admin_df, by = "cluster_id")

    # use admin levels for grouping
    grouping_vars <- admin_level

    cli::cli_alert_info(
      paste0(
        "Calculating IPTp indicators by ",
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

    ir_eligible <- ir_eligible |>
      dplyr::left_join(gps_clean, by = "cluster_id")

    grouping_vars <- "cluster_id"

    cli::cli_alert_info(
      "Calculating cluster-level IPTp indicators"
    )
  } else if (
    "adm2" %in% names(ir_eligible) &&
      "adm1" %in% names(ir_eligible)
  ) {
    grouping_vars <- c("adm1", "adm2")

    cli::cli_alert_info(
      "Using admin levels: adm1 and adm2"
    )
  } else if ("adm1" %in% names(ir_eligible)) {
    grouping_vars <- "adm1"

    cli::cli_alert_info(
      "Using administrative level: adm1"
    )
  } else {
    cli::cli_alert_info(
      "Calculating national-level IPTp indicators"
    )
  }

  # ---- 6. set up survey design -----------------------------------------------

  # always set lonely.psu option - subsets may create single PSU strata
  survey_options <- base::options(
    survey.lonely.psu = "adjust"
  )

  base::on.exit(
    base::options(survey_options),
    add = TRUE
  )

  strata_summary <- ir_eligible |>
    dplyr::group_by(stratum_id) |>
    dplyr::summarise(
      n_clusters = dplyr::n_distinct(cluster_id),
      .groups = "drop"
    )

  single_psu_strata_count <- base::sum(
    strata_summary$n_clusters == 1
  )

  if (single_psu_strata_count > 0) {
    n_single <- format(single_psu_strata_count, big.mark = ",")
    cli::cli_alert_info(
      paste0("Found ", n_single, " strata with single PSU; ",
             "using certainty option")
    )
  }

  use_strata <- dplyr::n_distinct(ir_eligible$stratum_id) > 1

  if (use_strata) {
    design <- survey::svydesign(
      ids = ~cluster_id,
      strata = ~stratum_id,
      weights = ~survey_weight,
      data = ir_eligible,
      nest = TRUE
    )
  } else {
    design <- survey::svydesign(
      ids = ~cluster_id,
      weights = ~survey_weight,
      data = ir_eligible,
      nest = TRUE
    )
  }

  # ---- 7. calculate iptp indicators ------------------------------------------

  if (!is.null(grouping_vars)) {
    grouping_formula <- stats::as.formula(
      paste("~", paste(grouping_vars, collapse = " + "))
    )
  } else {
    grouping_formula <- ~1
  }

  # 7a. IPTp 1+ coverage
  if (!is.null(grouping_vars)) {
    iptp1_results <- survey::svyby(
      ~iptp_1plus,
      by = grouping_formula,
      design = design,
      FUN = survey::svymean,
      vartype = "ci",
      keep.names = FALSE,
      na.rm = TRUE
    ) |>
      tibble::as_tibble() |>
      dplyr::rename(
        ci_l.iptp_1plus = ci_l,
        ci_u.iptp_1plus = ci_u
      )
  } else {
    iptp1_mean <- survey::svymean(
      ~iptp_1plus,
      design = design,
      na.rm = TRUE
    )

    iptp1_ci <- stats::confint(iptp1_mean)

    iptp1_results <- tibble::tibble(
      level = "National",
      iptp_1plus = base::as.numeric(iptp1_mean),
      ci_l.iptp_1plus = iptp1_ci[1, 1],
      ci_u.iptp_1plus = iptp1_ci[1, 2]
    )
  }

  # 7b. IPTp 2+ coverage
  if (!is.null(grouping_vars)) {
    iptp2_results <- survey::svyby(
      ~iptp_2plus,
      by = grouping_formula,
      design = design,
      FUN = survey::svymean,
      vartype = "ci",
      keep.names = FALSE,
      na.rm = TRUE
    ) |>
      tibble::as_tibble() |>
      dplyr::rename(
        ci_l.iptp_2plus = ci_l,
        ci_u.iptp_2plus = ci_u
      )
  } else {
    iptp2_mean <- survey::svymean(
      ~iptp_2plus,
      design = design,
      na.rm = TRUE
    )

    iptp2_ci <- stats::confint(iptp2_mean)

    iptp2_results <- tibble::tibble(
      level = "National",
      iptp_2plus = base::as.numeric(iptp2_mean),
      ci_l.iptp_2plus = iptp2_ci[1, 1],
      ci_u.iptp_2plus = iptp2_ci[1, 2]
    )
  }

  # 7c. IPTp 3+ coverage
  if (!is.null(grouping_vars)) {
    iptp3_results <- survey::svyby(
      ~iptp_3plus,
      by = grouping_formula,
      design = design,
      FUN = survey::svymean,
      vartype = "ci",
      keep.names = FALSE,
      na.rm = TRUE
    ) |>
      tibble::as_tibble() |>
      dplyr::rename(
        ci_l.iptp_3plus = ci_l,
        ci_u.iptp_3plus = ci_u
      )
  } else {
    iptp3_mean <- survey::svymean(
      ~iptp_3plus,
      design = design,
      na.rm = TRUE
    )

    iptp3_ci <- stats::confint(iptp3_mean)

    iptp3_results <- tibble::tibble(
      level = "National",
      iptp_3plus = base::as.numeric(iptp3_mean),
      ci_l.iptp_3plus = iptp3_ci[1, 1],
      ci_u.iptp_3plus = iptp3_ci[1, 2]
    )
  }

  # 7d. IPTp 4+ coverage
  if (!is.null(grouping_vars)) {
    iptp4_results <- survey::svyby(
      ~iptp_4plus,
      by = grouping_formula,
      design = design,
      FUN = survey::svymean,
      vartype = "ci",
      keep.names = FALSE,
      na.rm = TRUE
    ) |>
      tibble::as_tibble() |>
      dplyr::rename(
        ci_l.iptp_4plus = ci_l,
        ci_u.iptp_4plus = ci_u
      )
  } else {
    iptp4_mean <- survey::svymean(
      ~iptp_4plus,
      design = design,
      na.rm = TRUE
    )

    iptp4_ci <- stats::confint(iptp4_mean)

    iptp4_results <- tibble::tibble(
      level = "National",
      iptp_4plus = base::as.numeric(iptp4_mean),
      ci_l.iptp_4plus = iptp4_ci[1, 1],
      ci_u.iptp_4plus = iptp4_ci[1, 2]
    )
  }

  # 7e. IPTp exactly 1 dose
  if (!is.null(grouping_vars)) {
    iptp1only_results <- survey::svyby(
      ~iptp_1only,
      by = grouping_formula,
      design = design,
      FUN = survey::svymean,
      vartype = "ci",
      keep.names = FALSE,
      na.rm = TRUE
    ) |>
      tibble::as_tibble() |>
      dplyr::rename(
        ci_l.iptp_1only = ci_l,
        ci_u.iptp_1only = ci_u
      )
  } else {
    iptp1only_mean <- survey::svymean(
      ~iptp_1only,
      design = design,
      na.rm = TRUE
    )

    iptp1only_ci <- stats::confint(iptp1only_mean)

    iptp1only_results <- tibble::tibble(
      level = "National",
      iptp_1only = base::as.numeric(iptp1only_mean),
      ci_l.iptp_1only = iptp1only_ci[1, 1],
      ci_u.iptp_1only = iptp1only_ci[1, 2]
    )
  }

  # 7f. IPTp exactly 2 doses
  if (!is.null(grouping_vars)) {
    iptp2only_results <- survey::svyby(
      ~iptp_2only,
      by = grouping_formula,
      design = design,
      FUN = survey::svymean,
      vartype = "ci",
      keep.names = FALSE,
      na.rm = TRUE
    ) |>
      tibble::as_tibble() |>
      dplyr::rename(
        ci_l.iptp_2only = ci_l,
        ci_u.iptp_2only = ci_u
      )
  } else {
    iptp2only_mean <- survey::svymean(
      ~iptp_2only,
      design = design,
      na.rm = TRUE
    )

    iptp2only_ci <- stats::confint(iptp2only_mean)

    iptp2only_results <- tibble::tibble(
      level = "National",
      iptp_2only = base::as.numeric(iptp2only_mean),
      ci_l.iptp_2only = iptp2only_ci[1, 1],
      ci_u.iptp_2only = iptp2only_ci[1, 2]
    )
  }

  # 7g. IPTp exactly 3 doses
  if (!is.null(grouping_vars)) {
    iptp3only_results <- survey::svyby(
      ~iptp_3only,
      by = grouping_formula,
      design = design,
      FUN = survey::svymean,
      vartype = "ci",
      keep.names = FALSE,
      na.rm = TRUE
    ) |>
      tibble::as_tibble() |>
      dplyr::rename(
        ci_l.iptp_3only = ci_l,
        ci_u.iptp_3only = ci_u
      )
  } else {
    iptp3only_mean <- survey::svymean(
      ~iptp_3only,
      design = design,
      na.rm = TRUE
    )

    iptp3only_ci <- stats::confint(iptp3only_mean)

    iptp3only_results <- tibble::tibble(
      level = "National",
      iptp_3only = base::as.numeric(iptp3only_mean),
      ci_l.iptp_3only = iptp3only_ci[1, 1],
      ci_u.iptp_3only = iptp3only_ci[1, 2]
    )
  }

  # ---- 8. calculate sample sizes ---------------------------------------------

  if (!is.null(grouping_vars)) {
    sample_sizes <- ir_eligible |>
      dplyr::group_by(
        dplyr::across(dplyr::all_of(grouping_vars))
      ) |>
      dplyr::summarise(
        dhs_n_women = dplyr::n(),
        dhs_n_iptp_1plus = base::sum(
          iptp_1plus == 1,
          na.rm = TRUE
        ),
        dhs_n_iptp_2plus = base::sum(
          iptp_2plus == 1,
          na.rm = TRUE
        ),
        dhs_n_iptp_3plus = base::sum(
          iptp_3plus == 1,
          na.rm = TRUE
        ),
        dhs_n_iptp_4plus = base::sum(
          iptp_4plus == 1,
          na.rm = TRUE
        ),
        dhs_n_iptp_1only = base::sum(
          iptp_1only == 1,
          na.rm = TRUE
        ),
        dhs_n_iptp_2only = base::sum(
          iptp_2only == 1,
          na.rm = TRUE
        ),
        dhs_n_iptp_3only = base::sum(
          iptp_3only == 1,
          na.rm = TRUE
        ),
        .groups = "drop"
      )
  } else {
    sample_sizes <- tibble::tibble(
      level = "National",
      dhs_n_women = nrow(ir_eligible),
      dhs_n_iptp_1plus = base::sum(
        ir_eligible$iptp_1plus == 1,
        na.rm = TRUE
      ),
      dhs_n_iptp_2plus = base::sum(
        ir_eligible$iptp_2plus == 1,
        na.rm = TRUE
      ),
      dhs_n_iptp_3plus = base::sum(
        ir_eligible$iptp_3plus == 1,
        na.rm = TRUE
      ),
      dhs_n_iptp_4plus = base::sum(
        ir_eligible$iptp_4plus == 1,
        na.rm = TRUE
      ),
      dhs_n_iptp_1only = base::sum(
        ir_eligible$iptp_1only == 1,
        na.rm = TRUE
      ),
      dhs_n_iptp_2only = base::sum(
        ir_eligible$iptp_2only == 1,
        na.rm = TRUE
      ),
      dhs_n_iptp_3only = base::sum(
        ir_eligible$iptp_3only == 1,
        na.rm = TRUE
      )
    )
  }

  # ---- 9. combine results ----------------------------------------------------

  if (!is.null(grouping_vars)) {
    iptp_results <- iptp1_results |>
      dplyr::left_join(
        iptp2_results,
        by = grouping_vars
      ) |>
      dplyr::left_join(
        iptp3_results,
        by = grouping_vars
      ) |>
      dplyr::left_join(
        iptp4_results,
        by = grouping_vars
      ) |>
      dplyr::left_join(
        iptp1only_results,
        by = grouping_vars
      ) |>
      dplyr::left_join(
        iptp2only_results,
        by = grouping_vars
      ) |>
      dplyr::left_join(
        iptp3only_results,
        by = grouping_vars
      ) |>
      dplyr::left_join(
        sample_sizes,
        by = grouping_vars
      )
  } else {
    iptp_results <- iptp1_results |>
      dplyr::bind_cols(
        iptp2_results |>
          dplyr::select(-level)
      ) |>
      dplyr::bind_cols(
        iptp3_results |>
          dplyr::select(-level)
      ) |>
      dplyr::bind_cols(
        iptp4_results |>
          dplyr::select(-level)
      ) |>
      dplyr::bind_cols(
        iptp1only_results |>
          dplyr::select(-level)
      ) |>
      dplyr::bind_cols(
        iptp2only_results |>
          dplyr::select(-level)
      ) |>
      dplyr::bind_cols(
        iptp3only_results |>
          dplyr::select(-level)
      ) |>
      dplyr::bind_cols(
        sample_sizes |>
          dplyr::select(-level)
      )
  }

  # ---- 10. format results ----------------------------------------------------

  rename_map <- list(
    dhs_iptp_1 = "iptp_1plus",
    dhs_iptp_1_low = "ci_l.iptp_1plus",
    dhs_iptp_1_upp = "ci_u.iptp_1plus",
    dhs_iptp_2 = "iptp_2plus",
    dhs_iptp_2_low = "ci_l.iptp_2plus",
    dhs_iptp_2_upp = "ci_u.iptp_2plus",
    dhs_iptp_3 = "iptp_3plus",
    dhs_iptp_3_low = "ci_l.iptp_3plus",
    dhs_iptp_3_upp = "ci_u.iptp_3plus",
    dhs_iptp_4 = "iptp_4plus",
    dhs_iptp_4_low = "ci_l.iptp_4plus",
    dhs_iptp_4_upp = "ci_u.iptp_4plus",
    dhs_iptp_1only = "iptp_1only",
    dhs_iptp_1only_low = "ci_l.iptp_1only",
    dhs_iptp_1only_upp = "ci_u.iptp_1only",
    dhs_iptp_2only = "iptp_2only",
    dhs_iptp_2only_low = "ci_l.iptp_2only",
    dhs_iptp_2only_upp = "ci_u.iptp_2only",
    dhs_iptp_3only = "iptp_3only",
    dhs_iptp_3only_low = "ci_l.iptp_3only",
    dhs_iptp_3only_upp = "ci_u.iptp_3only"
  )

  for (new_name in names(rename_map)) {
    old_name <- rename_map[[new_name]]

    if (old_name %in% names(iptp_results)) {
      iptp_results <- iptp_results |>
        dplyr::rename(!!new_name := !!old_name)
    }
  }

  iptp_indicator_cols <- names(iptp_results)[
    grepl("^dhs_iptp_", names(iptp_results))
  ]

  iptp_results <- iptp_results |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(iptp_indicator_cols),
        ~ round(.x, 2)
      )
    )

  iptp_results <- iptp_results |>
    dplyr::mutate(
      dplyr::across(
        dplyr::matches("_low$"),
        ~ base::pmax(0, .)
      ),
      dplyr::across(
        dplyr::matches("_upp$"),
        ~ base::pmin(1, .)
      )
    )

  # ---- 11. attach gps coordinates if provided --------------------------------

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

    iptp_results <- iptp_results |>
      dplyr::left_join(gps_clean, by = "cluster_id")
  }

  column_order <- c(
    # admin levels
    grouping_vars,
    "lat",
    "lon",
    # main percentage indicators (cumulative)
    "dhs_iptp_1",
    "dhs_iptp_2",
    "dhs_iptp_3",
    "dhs_iptp_4",
    # main percentage indicators (exact dose)
    "dhs_iptp_1only",
    "dhs_iptp_2only",
    "dhs_iptp_3only",
    # sample sizes
    "dhs_n_women",
    "dhs_n_iptp_1plus",
    "dhs_n_iptp_2plus",
    "dhs_n_iptp_3plus",
    "dhs_n_iptp_4plus",
    "dhs_n_iptp_1only",
    "dhs_n_iptp_2only",
    "dhs_n_iptp_3only",
    # confidence intervals (cumulative)
    "dhs_iptp_1_low",
    "dhs_iptp_1_upp",
    "dhs_iptp_2_low",
    "dhs_iptp_2_upp",
    "dhs_iptp_3_low",
    "dhs_iptp_3_upp",
    "dhs_iptp_4_low",
    "dhs_iptp_4_upp",
    # confidence intervals (exact dose)
    "dhs_iptp_1only_low",
    "dhs_iptp_1only_upp",
    "dhs_iptp_2only_low",
    "dhs_iptp_2only_upp",
    "dhs_iptp_3only_low",
    "dhs_iptp_3only_upp"
  )

  column_order <- base::intersect(
    column_order,
    names(iptp_results)
  )

  other_columns <- base::setdiff(
    names(iptp_results),
    column_order
  )

  iptp_results <- iptp_results |>
    dplyr::select(
      dplyr::all_of(
        c(column_order, other_columns)
      )
    )

  tibble::as_tibble(iptp_results)
}

#' Calculate IPTp Coverage from DHS Data (standardized long-format output)
#'
#' Computes IPTp 1+/2+/3+/4+ and exact-dose 1/2/3 coverage indicators
#' nationally and optionally by subnational region, returning the standardized
#' `list(adm0, adm1)` output.
#'
#' @param dhs_ir DHS Individual Recode dataset (IR) in tidy format.
#' @param survey_vars Named list mapping DHS variable names.
#' @param birth_window_months Maximum age of most recent birth in months.
#'   Default 24 (DHS standard).
#' @param region_var Optional column name (character string) in `dhs_ir` to use
#'   as the subnational grouping variable (e.g., `"v024"` for region).
#' @param ci_method CI method for svyciprop. Default: "logit".
#'
#' @return Named list with `adm0` tibble and optionally `adm1` tibble in
#'   standardized long format.
#'
#' @examples
#' \dontrun{
#' # Basic usage
#' result <- calc_iptp_dhs(dhs_ir = ir_data)
#'
#' # With subnational estimates
#' result <- calc_iptp_dhs(
#'   dhs_ir = ir_data,
#'   region_var = "v024"
#' )
#' }
#'
#' @export
calc_iptp_dhs <- function(
  dhs_ir,
  survey_vars = list(
    cluster = "v001",
    weight = "v005",
    stratum = "v022",
    adm1 = "v024",
    adm2 = NULL,
    interview_cmc = "v008",
    birth_cmc = "b3_01",
    birth_age_months = "b19_01",
    sp_taken = "m49a_1",
    sp_doses = "ml1_1"
  ),
  birth_window_months = 24,
  region_var          = NULL,
  gps_data            = NULL,
  gps_vars            = list(cluster = "DHSCLUST", lat = "LATNUM", lon = "LONGNUM"),
  shapefile           = NULL,
  admin_level         = NULL,
  join_nearest        = TRUE,
  ci_method           = "logit"
) {

  # ---- 1. Extract survey metadata (IR data uses v-prefix) ----
  survey_meta <- .extract_survey_meta(dhs_ir)

  # ---- 2. Prepare data via existing helper ----
  ir <- .prepare_iptp_data(
    dhs_ir = dhs_ir,
    survey_vars = survey_vars,
    birth_window_months = birth_window_months,
    include_survey_vars = TRUE
  )

  if (is.null(ir) || nrow(ir) == 0) {
    cli::cli_abort("No eligible IPTp data after preparation.")
  }

  # ---- 3. Create binary outcome variables for generic helper ----
  ir <- ir |>
    dplyr::mutate(
      has_iptp_1plus = as.integer(has_1plus),
      has_iptp_2plus = as.integer(has_2plus),
      has_iptp_3plus = as.integer(has_3plus),
      has_iptp_4plus = as.integer(has_4plus),
      has_iptp_1only = as.integer(has_1only),
      has_iptp_2only = as.integer(has_2only),
      has_iptp_3only = as.integer(has_3only)
    )

  # ---- 4. Compute indicators across admin levels ----
  .compute_dhs_indicators_with_admin(
    data               = ir,
    conditions         = .iptp_conditions(),
    dhs_data           = dhs_ir,
    survey_meta        = survey_meta,
    region_var         = region_var,
    default_region_var = "v024",
    gps_data           = gps_data,
    gps_vars           = gps_vars,
    shapefile          = shapefile,
    admin_level        = admin_level,
    join_nearest       = join_nearest,
    ci_method          = ci_method
  )
}


# =============================================================================
# IPTp conditions & dictionary
# =============================================================================

#' Internal: IPTp indicator conditions
#'
#' Returns a list of indicator specifications for IPTp coverage indicators.
#' Covers 4 cumulative (1+, 2+, 3+, 4+) and 3 exact-dose (1, 2, 3)
#' indicators.
#'
#' @return List of named lists, each with: indicator, indicator_code,
#'   indicator_title, denom_code, filter_expr, outcome_var, num_desc,
#'   denom_desc.
#' @noRd
.iptp_conditions <- function() {
  denom <- "Women with recent birth (within birth_window_months)"
  list(
    list(
      indicator       = "IPTP_1PLUS",
      indicator_code  = "iptp_1plus",
      indicator_title = "IPTp 1+ dose coverage",
      denom_code      = "recent_births",
      filter_expr     = NULL,
      outcome_var     = "has_iptp_1plus",
      num_desc        = "Women receiving 1+ SP dose during pregnancy",
      denom_desc      = denom
    ),
    list(
      indicator       = "IPTP_2PLUS",
      indicator_code  = "iptp_2plus",
      indicator_title = "IPTp 2+ dose coverage",
      denom_code      = "recent_births",
      filter_expr     = NULL,
      outcome_var     = "has_iptp_2plus",
      num_desc        = "Women receiving 2+ SP doses during pregnancy",
      denom_desc      = denom
    ),
    list(
      indicator       = "IPTP_3PLUS",
      indicator_code  = "iptp_3plus",
      indicator_title = "IPTp 3+ dose coverage",
      denom_code      = "recent_births",
      filter_expr     = NULL,
      outcome_var     = "has_iptp_3plus",
      num_desc        = "Women receiving 3+ SP doses during pregnancy",
      denom_desc      = denom
    ),
    list(
      indicator       = "IPTP_4PLUS",
      indicator_code  = "iptp_4plus",
      indicator_title = "IPTp 4+ dose coverage",
      denom_code      = "recent_births",
      filter_expr     = NULL,
      outcome_var     = "has_iptp_4plus",
      num_desc        = "Women receiving 4+ SP doses during pregnancy",
      denom_desc      = denom
    ),
    list(
      indicator       = "IPTP_1ONLY",
      indicator_code  = "iptp_1only",
      indicator_title = "IPTp exactly 1 dose coverage",
      denom_code      = "recent_births",
      filter_expr     = NULL,
      outcome_var     = "has_iptp_1only",
      num_desc        = "Women receiving exactly 1 SP dose during pregnancy",
      denom_desc      = denom
    ),
    list(
      indicator       = "IPTP_2ONLY",
      indicator_code  = "iptp_2only",
      indicator_title = "IPTp exactly 2 doses coverage",
      denom_code      = "recent_births",
      filter_expr     = NULL,
      outcome_var     = "has_iptp_2only",
      num_desc        = "Women receiving exactly 2 SP doses during pregnancy",
      denom_desc      = denom
    ),
    list(
      indicator       = "IPTP_3ONLY",
      indicator_code  = "iptp_3only",
      indicator_title = "IPTp exactly 3 doses coverage",
      denom_code      = "recent_births",
      filter_expr     = NULL,
      outcome_var     = "has_iptp_3only",
      num_desc        = "Women receiving exactly 3 SP doses during pregnancy",
      denom_desc      = denom
    )
  )
}


#' IPTp Indicator Dictionary
#'
#' Returns a tibble describing all IPTp indicators computed by
#' \code{\link{calc_iptp_dhs}}.
#'
#' @return Tibble with columns: indicator, indicator_code, indicator_title,
#'   numerator_description, denominator_description, denominator_code.
#' @keywords internal
iptp_dictionary <- function() {
  conds <- .iptp_conditions()
  tibble::tibble(
    indicator               = vapply(conds, `[[`, character(1), "indicator"),
    indicator_code          = vapply(conds, `[[`, character(1), "indicator_code"),
    indicator_title         = vapply(conds, `[[`, character(1), "indicator_title"),
    numerator_description   = vapply(conds, `[[`, character(1), "num_desc"),
    denominator_description = vapply(conds, `[[`, character(1), "denom_desc"),
    denominator_code        = vapply(conds, `[[`, character(1), "denom_code")
  )
}

#' Aggregate IPTp indicators to administrative levels
#'
#' Helper function to aggregate IPTp results to administrative levels using a
#' shapefile. Performs spatial joins and calculates weighted averages by
#' administrative unit.
#'
#' @param iptp_results IPTp results from calc_iptp_dhs_core with lat/lon columns.
#' @param shapefile sf object with administrative boundaries.
#' @param admin_level Character vector of admin columns (default: "adm1").
#' @param weighted Logical; use weighted means by sample count (default: TRUE).
#'
#' @return sf object with aggregated IPTp indicators by administrative level.
#'
#' @keywords internal
#' @noRd
aggregate_iptp_admin <- function(
  iptp_results,
  shapefile,
  admin_level = c("adm1"),
  weighted = TRUE
) {
  if (!requireNamespace("sf", quietly = TRUE)) {
    cli::cli_abort("Package `sf` is required for spatial operations.")
  }

  # convert to sf if needed
  if (!inherits(iptp_results, "sf")) {
    if (!all(c("lat", "lon") %in% names(iptp_results))) {
      cli::cli_abort(
        "iptp_results must have lat and lon for spatial aggregation."
      )
    }

    iptp_sf <- iptp_results |>
      sf::st_as_sf(
        coords = c("lon", "lat"),
        crs = 4326,
        remove = FALSE
      )
  } else {
    iptp_sf <- iptp_results
  }

  shapefile <- shapefile |>
    sf::st_transform(4326) |>
    sf::st_make_valid()

  # spatial join
  joined <- sf::st_join(
    iptp_sf,
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

  iptp_cols <- names(joined_df)[grepl("^dhs_iptp_", names(joined_df))]
  sample_cols <- names(joined_df)[grepl("^dhs_n_", names(joined_df))]

  # aggregate
  if (weighted && "dhs_n_women" %in% names(joined_df)) {
    aggregated <- joined_df |>
      dplyr::group_by(dplyr::across(dplyr::all_of(admin_level))) |>
      dplyr::summarise(
        dplyr::across(
          dplyr::all_of(iptp_cols),
          ~ stats::weighted.mean(., w = dhs_n_women, na.rm = TRUE)
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
          dplyr::all_of(iptp_cols),
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
        dplyr::all_of(iptp_cols),
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
