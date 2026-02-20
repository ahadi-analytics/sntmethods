#' Prepare ANC Data for Analysis
#'
#' Shared data cleaning and indicator computation for ANC functions.
#' Used by both calc_anc_dhs_core() and calc_anc_mbg().
#'
#' @param dhs_ir DHS Individual Recode dataset.
#' @param survey_vars Named list mapping DHS variable names.
#' @param birth_window_months Months to look back for births.
#' @param include_survey_vars Logical. If TRUE, includes survey design columns.
#'
#' @return A data frame of eligible women with columns:
#'   cluster_id, anc_visits, and binary indicators:
#'   has_anc1, has_anc4, has_anc8.
#'   If include_survey_vars = TRUE, also: survey_weight, stratum_id.
#'
#' @noRd
.prepare_anc_data <- function(
  dhs_ir,
  survey_vars,
  birth_window_months = 36,
  include_survey_vars = FALSE
) {
  if (!is.data.frame(dhs_ir)) {
    cli::cli_abort("`dhs_ir` must be a data.frame or tibble")
  }
  if (nrow(dhs_ir) == 0) {
    cli::cli_abort("`dhs_ir` is empty.")
  }

  if (birth_window_months < 1 || birth_window_months > 60) {
    cli::cli_abort("`birth_window_months` must be between 1 and 60")
  }

  # Check required columns
  required_cols <- c(survey_vars$cluster, survey_vars$interview_date,
                     survey_vars$birth_date, survey_vars$anc_visits)
  if (include_survey_vars) {
    required_cols <- c(required_cols, survey_vars$weight, survey_vars$stratum)
  }
  missing_cols <- setdiff(required_cols, names(dhs_ir))
  if (length(missing_cols) > 0) {
    cli::cli_abort("Required columns not found: {.var {missing_cols}}")
  }

  ir <- dhs_ir |>
    dplyr::mutate(dplyr::across(dplyr::everything(), haven::zap_labels)) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), as.vector)) |>
    dplyr::mutate(
      cluster_id = .data[[survey_vars$cluster]],
      interview_cmc = .data[[survey_vars$interview_date]],
      birth_cmc = .data[[survey_vars$birth_date]],
      anc_visits = .data[[survey_vars$anc_visits]]
    )

  if (include_survey_vars) {
    ir <- ir |>
      dplyr::mutate(
        survey_weight = .data[[survey_vars$weight]] / 1e6,
        stratum_id = .data[[survey_vars$stratum]]
      )
  }

  # Filter to recent births
  ir <- ir |>
    dplyr::filter(
      !is.na(birth_cmc),
      !is.na(interview_cmc)
    ) |>
    dplyr::mutate(
      months_since_birth = interview_cmc - birth_cmc
    ) |>
    dplyr::filter(
      months_since_birth >= 0,
      months_since_birth <= birth_window_months
    )

  # Filter valid ANC responses (98 = "don't know")
  ir <- ir |>
    dplyr::filter(
      !is.na(anc_visits),
      anc_visits < 98
    )

  if (nrow(ir) == 0) {
    cli::cli_abort("No eligible women with valid ANC data found")
  }

  cli::cli_alert_info(
    "Found {format(nrow(ir), big.mark = ',')} women with births in last {birth_window_months} months"
  )

  # Calculate indicators
  ir <- ir |>
    dplyr::mutate(
      has_anc1 = as.integer(anc_visits >= 1),
      has_anc4 = as.integer(anc_visits >= 4),
      has_anc8 = as.integer(anc_visits >= 8)
    )

  ir
}
