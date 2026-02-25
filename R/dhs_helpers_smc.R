#' Prepare SMC Data for Analysis
#'
#' Shared data cleaning and indicator computation for SMC functions.
#' Used by both calc_smc_dhs_core() and calc_smc_mbg().
#'
#' @param dhs_kr DHS Children's Recode (KR) dataset.
#' @param survey_vars Named list mapping DHS variable names.
#' @param include_survey_vars Logical. If TRUE, includes survey design columns.
#'
#' @return A data frame of eligible children with columns:
#'   cluster_id, age_months, received_smc (binary 0/1).
#'   If include_survey_vars = TRUE, also: survey_weight, stratum_id.
#'
#' @noRd
.prepare_smc_data <- function(
  dhs_kr,
  survey_vars,
  include_survey_vars = FALSE,
  strict = TRUE
) {
  if (!is.data.frame(dhs_kr)) {
    cli::cli_abort("`dhs_kr` must be a data.frame or tibble")
  }
  if (nrow(dhs_kr) == 0) {
    cli::cli_abort("`dhs_kr` is empty.")
  }

  # Determine which SMC variable to use
  smc_var <- NULL
  if (survey_vars$smc_primary %in% names(dhs_kr)) {
    smc_var <- survey_vars$smc_primary
    cli::cli_alert_info("Using SMC variable: {.var {smc_var}} (primary)")
  } else if (!is.null(survey_vars$smc_alt) && survey_vars$smc_alt %in% names(dhs_kr)) {
    smc_var <- survey_vars$smc_alt
    cli::cli_alert_info("Using SMC variable: {.var {smc_var}} (alternative)")
  } else if (strict) {
    cli::cli_abort(
      "No SMC variable found; checked {.var {survey_vars$smc_primary}} and {.var {survey_vars$smc_alt}}"
    )
  } else {
    cli::cli_warn(
      "No SMC variable found; checked {.var {survey_vars$smc_primary}} and {.var {survey_vars$smc_alt}}; SMC not available for this survey"
    )
    return(NULL)
  }

  kr <- dhs_kr |>
    dplyr::mutate(dplyr::across(dplyr::everything(), haven::zap_labels)) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), as.vector)) |>
    dplyr::mutate(
      cluster_id = .data[[survey_vars$cluster]],
      age_months = .data[[survey_vars$age]],
      smc_receipt = .data[[smc_var]]
    )

  if (include_survey_vars) {
    kr <- kr |>
      dplyr::mutate(
        survey_weight = .data[[survey_vars$weight]] / 1e6,
        stratum_id = .data[[survey_vars$stratum]]
      )
  }

  # Filter to U5 children with valid SMC responses
  kr <- kr |>
    dplyr::filter(
      age_months >= 0,
      age_months <= 59,
      !is.na(smc_receipt),
      smc_receipt %in% c(0, 1)
    ) |>
    dplyr::mutate(
      received_smc = as.integer(smc_receipt == 1)
    )

  if (nrow(kr) == 0) {
    cli::cli_abort("No eligible children with valid SMC data found")
  }

  cli::cli_alert_info(
    "Found {format(nrow(kr), big.mark = ',')} children with SMC data"
  )

  kr
}
