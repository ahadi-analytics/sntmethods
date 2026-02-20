#' Prepare IRS Data for Analysis
#'
#' Shared data cleaning and indicator computation for IRS functions.
#' Used by both calc_irs_dhs_core() and calc_irs_mbg().
#'
#' @param dhs_hr DHS Household Records dataset.
#' @param survey_vars Named list mapping DHS variable names. Must include
#'   cluster and irs. Optionally: weight, stratum for DHS.
#' @param include_survey_vars Logical. If TRUE, includes survey design columns.
#'
#' @return A data frame of households with columns:
#'   cluster_id, sprayed (binary 0/1).
#'   If include_survey_vars = TRUE, also: survey_weight, stratum_id.
#'
#' @noRd
.prepare_irs_data <- function(
  dhs_hr,
  survey_vars,
  include_survey_vars = FALSE
) {
  if (!is.data.frame(dhs_hr)) {
    cli::cli_abort("`dhs_hr` must be a data.frame or tibble")
  }
  if (nrow(dhs_hr) == 0) {
    cli::cli_abort("`dhs_hr` is empty.")
  }

  if (!survey_vars$irs %in% names(dhs_hr)) {
    cli::cli_abort(c(
      "IRS variable {.var {survey_vars$irs}} not found in HR data",
      "i" = "IRS coverage data may not be available for this survey"
    ))
  }

  hr <- dhs_hr |>
    dplyr::mutate(dplyr::across(dplyr::everything(), haven::zap_labels)) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), as.vector)) |>
    dplyr::mutate(
      cluster_id = .data[[survey_vars$cluster]],
      irs_sprayed = .data[[survey_vars$irs]]
    )

  if (include_survey_vars) {
    hr <- hr |>
      dplyr::mutate(
        survey_weight = .data[[survey_vars$weight]] / 1e6,
        stratum_id = .data[[survey_vars$stratum]]
      )
  }

  hr <- hr |>
    dplyr::filter(!is.na(irs_sprayed)) |>
    dplyr::mutate(
      sprayed = as.integer(irs_sprayed == 1)
    )

  if (nrow(hr) == 0) {
    cli::cli_abort("No households with valid IRS data found")
  }

  cli::cli_alert_info(
    "Found {format(nrow(hr), big.mark = ',')} households with valid IRS data"
  )

  hr
}
