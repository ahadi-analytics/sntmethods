#' Prepare Antimalarial Data for Analysis
#'
#' Shared data cleaning and indicator computation for antimalarial functions.
#' Used by both calc_antimalarial_dhs_core() and calc_case_management_dhs().
#'
#' @param dhs_kr DHS Children's Recode (KR) dataset.
#' @param survey_vars Named list mapping DHS variable names.
#' @param include_survey_vars Logical. If TRUE, includes survey design columns.
#'
#' @return A data frame of febrile U5 children with columns:
#'   cluster_id, age_months, received_antimalarial, ml13_vars_found.
#'   If include_survey_vars = TRUE, also: survey_weight, stratum_id.
#'
#' @noRd
.prepare_antimalarial_data <- function(
  dhs_kr,
  survey_vars,
  include_survey_vars = FALSE
) {
  if (!is.data.frame(dhs_kr)) {
    cli::cli_abort("`dhs_kr` must be a data.frame or tibble.")
  }
  if (nrow(dhs_kr) == 0) {
    cli::cli_abort("`dhs_kr` is empty.")
  }

  # Check required columns
  needed <- c(survey_vars$cluster, survey_vars$age, survey_vars$fever)
  if (include_survey_vars) {
    needed <- c(needed, survey_vars$weight, survey_vars$stratum)
  }
  missing_vars <- setdiff(needed, names(dhs_kr))
  if (length(missing_vars) > 0) {
    cli::cli_abort(c(
      "Required variables not found: {.var {missing_vars}}",
      "i" = "Check your survey_vars mapping"
    ))
  }

  # Auto-detect available ml13* variables
  ml13_vars <- grep("^ml13[a-z]*$", names(dhs_kr), value = TRUE)
  if (length(ml13_vars) == 0) {
    cli::cli_abort(c(
      "No ml13 antimalarial variables found in data.",
      "i" = "Expected variables like ml13a, ml13b, ml13c, ml13d, ml13e, etc."
    ))
  }

  cli::cli_alert_info(
    "Detected {length(ml13_vars)} ml13 antimalarial variables: {paste(ml13_vars, collapse = ', ')}"
  )

  # Zap labels
  kr <- dhs_kr |>
    dplyr::mutate(dplyr::across(dplyr::everything(), haven::zap_labels)) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), as.vector))

  # Build columns
  kr <- kr |>
    dplyr::mutate(
      cluster_id = .data[[survey_vars$cluster]],
      age_months = .data[[survey_vars$age]],
      had_fever = .data[[survey_vars$fever]]
    )

  if (include_survey_vars) {
    kr <- kr |>
      dplyr::mutate(
        survey_weight = .data[[survey_vars$weight]] / 1e6,
        stratum_id = .data[[survey_vars$stratum]]
      )
  }

  # Filter to febrile U5 children
  kr_fever <- kr |>
    dplyr::filter(
      age_months >= 0,
      age_months <= 59,
      had_fever == 1
    )

  if (nrow(kr_fever) == 0) {
    cli::cli_abort("No children with fever in the last 2 weeks found.")
  }

  # Create binary antimalarial indicator: 1 if ANY ml13 variable == 1
  ml13_matrix <- as.matrix(kr_fever[, ml13_vars, drop = FALSE])
  kr_fever$received_antimalarial <- apply(ml13_matrix, 1, function(row) {
    if (all(is.na(row))) return(NA_real_)
    if (any(row == 1, na.rm = TRUE)) return(1)
    return(0)
  })

  if (all(is.na(kr_fever$received_antimalarial))) {
    cli::cli_abort("All ml13 antimalarial variables are NA for febrile children")
  }

  # Create binary indicator for survey estimation
  kr_fever <- kr_fever |>
    dplyr::mutate(
      has_antimalarial = dplyr::if_else(
        received_antimalarial == 1, 1, 0, missing = NA_real_
      )
    )

  # Also flag which ml13 vars were found (for metadata)
  attr(kr_fever, "ml13_vars_found") <- ml13_vars

  cli::cli_alert_info(
    "Found {format(nrow(kr_fever), big.mark = ',')} febrile children under 5"
  )

  kr_fever
}
