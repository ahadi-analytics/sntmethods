#' Prepare EPI Data for Analysis
#'
#' Shared data cleaning and indicator computation for EPI functions.
#' Used by both calc_epi_dhs_core() and calc_epi_mbg().
#'
#' @param dhs_kr DHS Children's Recode (KR) dataset.
#' @param survey_vars Named list mapping DHS variable names.
#' @param age_min_months Minimum age in months.
#' @param age_max_months Maximum age in months.
#' @param include_survey_vars Logical. If TRUE, includes survey design columns.
#'
#' @return A data frame of eligible children with columns:
#'   cluster_id, age_months, and binary vaccination columns for each
#'   available vaccine. If include_survey_vars = TRUE, also: survey_weight,
#'   stratum_id.
#'
#' @noRd
.prepare_epi_data <- function(
  dhs_kr,
  survey_vars,
  age_min_months = 12,
  age_max_months = 23,
  include_survey_vars = FALSE
) {
  if (!is.data.frame(dhs_kr)) {
    cli::cli_abort("`dhs_kr` must be a data.frame or tibble")
  }
  if (nrow(dhs_kr) == 0) {
    cli::cli_abort("`dhs_kr` is empty.")
  }

  # Build vaccine mapping
  vaccine_mapping <- list(
    bcg = survey_vars$bcg, dpt1 = survey_vars$dpt1,
    dpt2 = survey_vars$dpt2, dpt3 = survey_vars$dpt3,
    polio1 = survey_vars$polio1, polio2 = survey_vars$polio2,
    polio3 = survey_vars$polio3,
    measles1 = survey_vars$measles1, measles2 = survey_vars$measles2,
    vita1 = survey_vars$vita1, vita2 = survey_vars$vita2,
    malaria = survey_vars$malaria
  )

  available_vaccines <- sapply(vaccine_mapping, function(v) !is.null(v) && v %in% names(dhs_kr))
  available_vaccine_cols <- unlist(vaccine_mapping[available_vaccines])

  # Select columns
  select_cols <- unique(c(
    survey_vars$cluster, survey_vars$age,
    available_vaccine_cols
  ))
  if (include_survey_vars) {
    select_cols <- unique(c(select_cols, survey_vars$weight, survey_vars$stratum))
  }
  select_cols <- select_cols[select_cols %in% names(dhs_kr)]

  kr <- dhs_kr |>
    dplyr::select(dplyr::all_of(select_cols)) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), haven::zap_labels)) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), as.vector)) |>
    dplyr::mutate(
      cluster_id = .data[[survey_vars$cluster]],
      age_months = .data[[survey_vars$age]]
    )

  if (include_survey_vars) {
    kr <- kr |>
      dplyr::mutate(
        survey_weight = .data[[survey_vars$weight]] / 1e6,
        stratum_id = .data[[survey_vars$stratum]]
      )
  }

  # Filter to eligible age range
  kr <- kr |>
    dplyr::filter(
      age_months >= age_min_months,
      age_months <= age_max_months
    )

  if (nrow(kr) == 0) {
    cli::cli_abort(
      "No eligible children found in age range {age_min_months}-{age_max_months} months"
    )
  }

  cli::cli_alert_info(
    "Found {format(nrow(kr), big.mark = ',')} children aged {age_min_months}-{age_max_months} months"
  )

  # Add binary vaccination columns for each available vaccine
  # DHS: 1 = vaccination card, 2 = reported by mother, 3 = both
  for (vax_name in names(available_vaccines)[available_vaccines]) {
    var_name <- vaccine_mapping[[vax_name]]
    col_name <- paste0("vax_", vax_name)
    kr[[col_name]] <- as.integer(!is.na(kr[[var_name]]) & kr[[var_name]] %in% c(1, 2, 3))
  }

  # Add fully_vaccinated if all required vaccines are present
  required_for_fv <- c("bcg", "dpt3", "polio3", "measles1")
  if (all(required_for_fv %in% names(available_vaccines)[available_vaccines])) {
    kr$vax_fully_vaccinated <- as.integer(
      kr$vax_bcg == 1 & kr$vax_dpt3 == 1 &
      kr$vax_polio3 == 1 & kr$vax_measles1 == 1
    )
  }

  kr
}
