#' Prepare ACT Data for Analysis
#'
#' Shared data cleaning and indicator computation for ACT functions.
#' Used by both calc_act_dhs() and calc_act_mbg().
#'
#' @param dhs_kr DHS Children's Recode (KR) dataset.
#' @param survey_vars Named list mapping DHS variable names.
#' @param include_survey_vars Logical. If TRUE, includes survey design columns.
#'
#' @return A data frame of febrile children with columns:
#'   cluster_id, age_months, received_act, test_positive, has_act.
#'   If include_survey_vars = TRUE, also: survey_weight, stratum_id.
#'
#' @noRd
.prepare_act_data <- function(
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

  # Detect ACT variable: prefer ml13e (newer surveys), fall back to h37e (older surveys).
  # h37e = "Combination with artemisinin taken for fever/cough" in older DHS surveys.
  act_var <- survey_vars$act
  if (!act_var %in% names(dhs_kr)) {
    if ("h37e" %in% names(dhs_kr)) {
      cli::cli_alert_info(
        "ACT variable {.var {act_var}} not found; using {.var h37e} (artemisinin combination for fever/cough)"
      )
      act_var <- "h37e"
    } else {
      cli::cli_abort(
        "ACT variable {.var {act_var}} not found in data (also tried {.var h37e})"
      )
    }
  }
  has_test_var <- survey_vars$test %in% names(dhs_kr)

  # Zap labels
  kr <- dhs_kr |>
    dplyr::mutate(dplyr::across(dplyr::everything(), haven::zap_labels)) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), as.vector))

  # Build columns
  kr <- kr |>
    dplyr::mutate(
      cluster_id = .data[[survey_vars$cluster]],
      age_months = .data[[survey_vars$age]],
      had_fever = .data[[survey_vars$fever]],
      received_act = .data[[act_var]]
    )

  if (has_test_var) {
    kr$test_positive <- kr[[survey_vars$test]]
  } else {
    kr$test_positive <- NA_real_
  }

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

  if (all(is.na(kr_fever$received_act))) {
    cli::cli_abort("ACT variable {.var {act_var}} is all NA for febrile children")
  }

  # Create binary ACT indicator
  kr_fever <- kr_fever |>
    dplyr::mutate(
      has_act = dplyr::if_else(received_act == 1, 1, 0, missing = NA_real_)
    )

  cli::cli_alert_info(
    "Found {format(nrow(kr_fever), big.mark = ',')} febrile children under 5"
  )

  kr_fever
}
