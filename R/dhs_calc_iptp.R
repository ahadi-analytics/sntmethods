#' Calculate Core IPTp Coverage from DHS Data
#'
#' Core function that estimates IPTp (Intermittent Preventive Treatment in
#' pregnancy) coverage indicators from DHS Individual Recode (IR) data.
#' Implements standard DHS methodology for calculating IPTp indicators among
#' women with a recent birth. When GPS data is provided, produces cluster-level
#' results. Otherwise uses existing administrative variables in the data.
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
#' @export
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

  # ---- 2. calculate birth age ------------------------------------------------

  # Check if b19_01 (age in months) is available, otherwise calculate from CMC
  birth_age_var <- survey_vars$birth_age_months
  birth_cmc_var <- survey_vars$birth_cmc
  interview_cmc_var <- survey_vars$interview_cmc

  has_birth_age <- birth_age_var %in% names(dhs_ir) &&
    !all(is.na(dhs_ir[[birth_age_var]]))

  has_birth_cmc <- birth_cmc_var %in% names(dhs_ir) &&
    !all(is.na(dhs_ir[[birth_cmc_var]]))

  if (!has_birth_age && !has_birth_cmc) {
    cli::cli_abort(
      c(
        "Cannot determine birth age.",
        "i" = paste0(
          "Need either {.var {birth_age_var}} or {.var {birth_cmc_var}}"
        )
      )
    )
  }

  # Create core dataset
  ir_core <- dhs_ir |>
    dplyr::mutate(
      cluster_id = .data[[survey_vars$cluster]],
      survey_weight = .data[[survey_vars$weight]] / 1e6,
      stratum_id = .data[[survey_vars$stratum]],
      sp_taken = .data[[survey_vars$sp_taken]],
      sp_doses = .data[[survey_vars$sp_doses]]
    )

  # Calculate birth age using b19_01 if available, else v008 - b3_01
  if (has_birth_age) {
    ir_core <- ir_core |>
      dplyr::mutate(birth_age = .data[[birth_age_var]])
    cli::cli_alert_info(
      "Using {.var {birth_age_var}} for birth age"
    )
  } else {
    ir_core <- ir_core |>
      dplyr::mutate(
        birth_age = .data[[interview_cmc_var]] - .data[[birth_cmc_var]]
      )
    cli::cli_alert_info(
      "Calculating birth age from {.var {interview_cmc_var}} - {.var {birth_cmc_var}}"
    )
  }

  # add admin variables if available
  if (
    !is.null(survey_vars$adm1) &&
      survey_vars$adm1 %in% names(dhs_ir)
  ) {
    ir_core <- ir_core |>
      dplyr::mutate(
        adm1 = haven::as_factor(.data[[survey_vars$adm1]]) |>
          base::as.character() |>
          toupper()
      )
  }

  if (
    !is.null(survey_vars$adm2) &&
      survey_vars$adm2 %in% names(dhs_ir)
  ) {
    ir_core <- ir_core |>
      dplyr::mutate(
        adm2 = haven::as_factor(.data[[survey_vars$adm2]]) |>
          base::as.character() |>
          toupper()
      )
  }

  # ---- 3. filter to eligible population --------------------------------------

  n_total <- nrow(ir_core)

  ir_eligible <- ir_core |>
    dplyr::filter(
      !is.na(birth_age),
      birth_age >= 0,
      birth_age < birth_window_months
    )

  n_eligible <- nrow(ir_eligible)

  cli::cli_alert_info(
    paste0(
      "Filtered to ", format(n_eligible, big.mark = ","),
      " women with birth < ", birth_window_months,
      " months (from ", format(n_total, big.mark = ","), " total)"
    )
  )

  if (n_eligible == 0) {
    cli::cli_abort(
      c(
        "No eligible women found with births < {birth_window_months} months.",
        "i" = "Check your birth_window_months parameter or data"
      )
    )
  }

  # ---- 4. create iptp indicators (DHS methodology) ---------------------------

  # IPTp indicators following official DHS code:
  # - IPTp 1+: m49a_1 == 1 (took any SP)

  # - IPTp 2+: m49a_1 == 1 AND ml1_1 >= 2 AND ml1_1 <= 97
  # - IPTp 3+: m49a_1 == 1 AND ml1_1 >= 3 AND ml1_1 <= 97
  # Note: values > 97 are typically "don't know" (98) or missing

  ir_eligible <- ir_eligible |>
    dplyr::mutate(
      # IPTp 1+: took any SP/Fansidar
      iptp_1plus = dplyr::case_when(
        sp_taken == 1 ~ 1L,
        sp_taken != 1 ~ 0L,
        TRUE ~ NA_integer_
      ),
      # IPTp 2+: took SP AND doses >= 2 (valid range)
      iptp_2plus = dplyr::case_when(
        sp_taken == 1 & sp_doses >= 2 & sp_doses <= 97 ~ 1L,
        !(sp_taken == 1 & sp_doses >= 2 & sp_doses <= 97) ~ 0L,
        TRUE ~ NA_integer_
      ),
      # IPTp 3+: took SP AND doses >= 3 (valid range)
      iptp_3plus = dplyr::case_when(
        sp_taken == 1 & sp_doses >= 3 & sp_doses <= 97 ~ 1L,
        !(sp_taken == 1 & sp_doses >= 3 & sp_doses <= 97) ~ 0L,
        TRUE ~ NA_integer_
      )
    )

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
    survey.lonely.psu = "certainty"
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
    dhs_iptp_3_upp = "ci_u.iptp_3plus"
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
        ~ round(.x * 100, 1)
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
        ~ base::pmin(100, .)
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
    # main percentage indicators
    "dhs_iptp_1",
    "dhs_iptp_2",
    "dhs_iptp_3",
    # sample sizes
    "dhs_n_women",
    "dhs_n_iptp_1plus",
    "dhs_n_iptp_2plus",
    "dhs_n_iptp_3plus",
    # confidence intervals
    "dhs_iptp_1_low",
    "dhs_iptp_1_upp",
    "dhs_iptp_2_low",
    "dhs_iptp_2_upp",
    "dhs_iptp_3_low",
    "dhs_iptp_3_upp"
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

#' Extract metadata from DHS datasets for IPTp analysis
#'
#' Internal function to extract survey metadata from DHS Individual Recode
#' data. Looks for standard DHS metadata columns and extracts key survey
#' information relevant to IPTp coverage analysis.
#'
#' @param dhs_ir DHS Individual Recode dataset.
#' @param survey_vars Named list of survey variable mappings.
#' @param birth_window_months Birth window used for filtering.
#'
#' @return List containing survey metadata.
#' @noRd
extract_dhs_metadata_iptp <- function(
  dhs_ir,
  survey_vars = NULL,
  birth_window_months = 24
) {
  metadata <- list()

  # extract country code
  if ("v000" %in% names(dhs_ir)) {
    metadata$country_code <- unique(dhs_ir$v000)[1]
  } else {
    metadata$country_code <- NA_character_
  }

  # extract survey year
  if ("v007" %in% names(dhs_ir)) {
    metadata$survey_year <- unique(dhs_ir$v007)[1]
  } else {
    metadata$survey_year <- NA_integer_
  }

  # extract survey id
  if ("v000" %in% names(dhs_ir)) {
    metadata$survey_id <- unique(dhs_ir$v000)[1]
  } else {
    metadata$survey_id <- NA_character_
  }

  metadata$survey_type <- "DHS"
  metadata$file_type <- "IR"

  metadata$total_records <- nrow(dhs_ir)

  # number of clusters
  cluster_var <- if (!is.null(survey_vars$cluster)) {
    survey_vars$cluster
  } else {
    "v001"
  }

  if (cluster_var %in% names(dhs_ir)) {
    metadata$total_clusters <- length(unique(dhs_ir[[cluster_var]]))
  }

  metadata$birth_window_months <- birth_window_months

  metadata$has_sp_taken <- !is.null(survey_vars$sp_taken) &&
    survey_vars$sp_taken %in% names(dhs_ir)
  metadata$has_sp_doses <- !is.null(survey_vars$sp_doses) &&
    survey_vars$sp_doses %in% names(dhs_ir)

  metadata$processed_date <- Sys.Date()
  metadata$processed_time <- Sys.time()

  metadata$analysis_type <- "IPTp Coverage"

  metadata$indicators <- c(
    "IPTp 1+ (one or more doses)",
    "IPTp 2+ (two or more doses)",
    "IPTp 3+ (three or more doses)"
  )

  metadata$variable_mapping <- survey_vars

  metadata
}

#' Calculate IPTp Coverage from DHS Data with Spatial Aggregation Support
#'
#' Main function for calculating IPTp (Intermittent Preventive Treatment in
#' pregnancy) coverage indicators from DHS Individual Recode (IR) data. When a
#' shapefile is provided, calculations are performed directly at the
#' administrative level for better statistical precision.
#'
#' @param dhs_ir DHS Individual Recode dataset (IR) in tidy format.
#' @param survey_vars Named list mapping DHS variable names.
#' @param birth_window_months Maximum age of most recent birth in months.
#'   Default 24 (DHS standard).
#' @param gps_data Optional DHS GPS dataset for spatial joins.
#' @param gps_vars Named list for GPS variables (cluster, lat, lon).
#' @param shapefile Optional sf object with administrative boundaries.
#' @param admin_level Character vector of admin columns in shapefile.
#' @param join_nearest Logical; assign unmatched clusters to nearest polygon.
#'
#' @return List containing:
#'   \itemize{
#'     \item data: Tibble with IPTp indicators
#'     \item dict: Data dictionary
#'     \item metadata: Survey metadata
#'   }
#'
#' @examples
#' \dontrun{
#' # Basic usage
#' result <- calc_iptp_dhs(dhs_ir = ir_data)
#'
#' # With spatial aggregation
#' result <- calc_iptp_dhs(
#'   dhs_ir = ir_data,
#'   gps_data = gps_data,
#'   shapefile = admin_boundaries,
#'   admin_level = "adm1"
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
  metadata <- extract_dhs_metadata_iptp(
    dhs_ir = dhs_ir,
    survey_vars = survey_vars,
    birth_window_months = birth_window_months
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
  iptp_results <- calc_iptp_dhs_core(
    dhs_ir = dhs_ir,
    survey_vars = survey_vars,
    birth_window_months = birth_window_months,
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
    data = iptp_results,
    dict = sntutils::build_dictionary(iptp_results),
    metadata = metadata
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
#' @export
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
