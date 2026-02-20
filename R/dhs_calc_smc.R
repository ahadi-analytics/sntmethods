#' Calculate SMC Coverage from DHS Data
#'
#' Estimates Seasonal Malaria Chemoprevention (SMC) coverage among children
#' under 5 using survey-weighted methods.
#'
#' @param dhs_kr DHS Children's Recode (KR) dataset.
#' @param survey_vars Named list mapping DHS variable names.
#' @param region_var Optional column name to use as grouping variable.
#' @param gps_data Optional DHS GPS dataset.
#' @param gps_vars Named list for GPS variables.
#' @param shapefile Optional sf object with administrative boundaries.
#' @param admin_level Character vector of admin columns.
#' @param join_nearest Logical.
#'
#' @return Tibble with SMC estimates including:
#'   dhs_smc, dhs_smc_low, dhs_smc_upp, dhs_n_smc_eligible, dhs_n_smc_received.
#'
#' @seealso [calc_smc_mbg()] for cluster-level MBG inputs
#' @export
calc_smc_dhs_core <- function(
  dhs_kr,
  survey_vars = list(
    cluster = "v021",
    weight = "v005",
    stratum = "v022",
    age = "hw1",
    smc_primary = "hml43",
    smc_alt = "ml13g"
  ),
  region_var = NULL,
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
  # Prepare data using shared helper
  kr <- .prepare_smc_data(
    dhs_kr = dhs_kr,
    survey_vars = survey_vars,
    include_survey_vars = TRUE
  )

  # Determine grouping
  class_var <- NULL
  if (!is.null(region_var)) {
    if (!region_var %in% names(dhs_kr)) {
      cli::cli_abort("Column {.var {region_var}} not found in `dhs_kr`.")
    }
    class_var <- region_var
  } else if ("v024" %in% names(kr)) {
    class_var <- "v024"
    cli::cli_alert_info("Using v024 (region) as grouping variable")
  }

  # Set up survey design
  use_strata <- dplyr::n_distinct(kr$stratum_id) > 1
  if (use_strata) {
    survey_options <- options(survey.lonely.psu = "certainty")
    on.exit(options(survey_options), add = TRUE)
    des <- survey::svydesign(
      ids = ~cluster_id, strata = ~stratum_id,
      weights = ~survey_weight, data = kr, nest = TRUE
    )
  } else {
    des <- survey::svydesign(
      ids = ~cluster_id, weights = ~survey_weight,
      data = kr, nest = TRUE
    )
  }

  # Calculate SMC coverage
  if (!is.null(class_var)) {
    group_formula <- stats::as.formula(paste("~", class_var))
    smc_results <- tryCatch({
      survey::svyby(
        ~received_smc, by = group_formula, design = des,
        FUN = survey::svymean, vartype = "ci",
        na.rm = TRUE, keep.names = FALSE
      ) |> tibble::as_tibble()
    }, error = function(e) {
      if (grepl("has only one PSU", e$message)) {
        des_ns <- survey::svydesign(
          ids = ~cluster_id, weights = ~survey_weight,
          data = kr, nest = TRUE
        )
        survey::svyby(
          ~received_smc, by = group_formula, design = des_ns,
          FUN = survey::svymean, vartype = "ci",
          na.rm = TRUE, keep.names = FALSE
        ) |> tibble::as_tibble()
      } else stop(e)
    })
    # svyby with single variable produces ci_l/ci_u (no suffix)
    smc_results <- dplyr::rename(smc_results,
      dhs_smc = received_smc, dhs_smc_low = ci_l, dhs_smc_upp = ci_u
    )
  } else {
    smc_mean <- survey::svymean(~received_smc, design = des, na.rm = TRUE)
    smc_ci <- stats::confint(smc_mean)
    smc_results <- tibble::tibble(
      level = "National",
      dhs_smc = as.numeric(smc_mean["received_smc"]),
      dhs_smc_low = smc_ci["received_smc", 1],
      dhs_smc_upp = smc_ci["received_smc", 2]
    )
  }

  # Sample sizes
  if (!is.null(class_var)) {
    sample_sizes <- kr |>
      dplyr::group_by(.data[[class_var]]) |>
      dplyr::summarise(
        dhs_n_smc_eligible = dplyr::n(),
        dhs_n_smc_received = sum(received_smc == 1, na.rm = TRUE),
        .groups = "drop"
      )
    smc_results <- smc_results |>
      dplyr::left_join(sample_sizes, by = class_var)
  } else {
    smc_results$dhs_n_smc_eligible <- nrow(kr)
    smc_results$dhs_n_smc_received <- sum(kr$received_smc == 1, na.rm = TRUE)
  }

  # Format
  smc_results <- smc_results |>
    dplyr::mutate(
      dhs_smc = round(dhs_smc, 2),
      dhs_smc_low = pmax(0, round(dhs_smc_low, 2)),
      dhs_smc_upp = pmin(1, round(dhs_smc_upp, 2)),
      dhs_n_smc_eligible = as.integer(dhs_n_smc_eligible),
      dhs_n_smc_received = as.integer(dhs_n_smc_received)
    ) |>
    dplyr::select(-dplyr::any_of(c("level")))

  tibble::as_tibble(smc_results)
}

#' Calculate SMC Coverage from DHS Data (with metadata)
#'
#' @inheritParams calc_smc_dhs_core
#' @return List with data, dict, and metadata.
#' @export
calc_smc_dhs <- function(
  dhs_kr,
  survey_vars = list(
    cluster = "v021",
    weight = "v005",
    stratum = "v022",
    age = "hw1",
    smc_primary = "hml43",
    smc_alt = "ml13g"
  ),
  region_var = NULL,
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
  smc_data <- calc_smc_dhs_core(
    dhs_kr = dhs_kr,
    survey_vars = survey_vars,
    region_var = region_var,
    gps_data = gps_data,
    gps_vars = gps_vars,
    shapefile = shapefile,
    admin_level = admin_level,
    join_nearest = join_nearest
  )

  list(
    data = smc_data,
    dict = sntutils::build_dictionary(smc_data),
    metadata = list(
      analysis_type = "SMC (Seasonal Malaria Chemoprevention)",
      file_type = "KR",
      processed_date = Sys.Date()
    )
  )
}
