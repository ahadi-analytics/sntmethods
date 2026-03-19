#' Prepare Fever Data for Analysis
#'
#' Shared data cleaning and indicator computation for fever functions.
#' Used by both calc_fever_dhs_core() and calc_case_management_dhs().
#'
#' @param dhs_kr DHS Children's Recode (KR) dataset.
#' @param survey_vars Named list mapping DHS variable names.
#' @param include_survey_vars Logical. If TRUE, includes survey design columns.
#'
#' @return A data frame of alive U5 children with columns:
#'   cluster_id, age_months, had_fever.
#'   If include_survey_vars = TRUE, also: survey_weight, stratum_id.
#'
#' @noRd
.prepare_fever_data <- function(
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

  # Zap labels
  kr <- dhs_kr |>
    dplyr::mutate(dplyr::across(dplyr::everything(), haven::zap_labels)) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), as.vector))

  # Build columns (force numeric to guard against haven character residuals)
  kr <- kr |>
    dplyr::mutate(
      cluster_id = .data[[survey_vars$cluster]],
      age_months = suppressWarnings(as.numeric(as.character(.data[[survey_vars$age]]))),
      fever_raw = suppressWarnings(as.numeric(as.character(.data[[survey_vars$fever]])))
    )

  # Check alive variable if present
  has_alive <- !is.null(survey_vars$alive) &&
    survey_vars$alive %in% names(dhs_kr)

  if (has_alive) {
    kr <- kr |>
      dplyr::mutate(
        child_alive = suppressWarnings(as.numeric(as.character(.data[[survey_vars$alive]])))
      )
  }

  if (include_survey_vars) {
    kr <- kr |>
      dplyr::mutate(
        survey_weight = .data[[survey_vars$weight]] / 1e6,
        stratum_id = .data[[survey_vars$stratum]]
      )
  }

  # Filter to alive U5 children
  kr_u5 <- kr |>
    dplyr::filter(
      age_months >= 0,
      age_months <= 59
    )

  # Filter to alive children if variable present

  if (has_alive) {
    kr_u5 <- kr_u5 |>
      dplyr::filter(child_alive == 1)
  }

  if (nrow(kr_u5) == 0) {
    cli::cli_abort("No alive children under 5 found in data.")
  }

  # Check that fever variable has valid data
  if (all(is.na(kr_u5$fever_raw))) {
    cli::cli_abort(
      "Fever variable {.var {survey_vars$fever}} is all NA for U5 children"
    )
  }

  # Detect fever coding scheme: Some surveys use 0=No/1=Yes, others use 1=No/2=Yes
  fever_values <- unique(kr_u5$fever_raw[!is.na(kr_u5$fever_raw)])

  # Determine "Yes" value: if values are strictly {1, 2} or {2}, assume 2=Yes
  # Otherwise, assume 1=Yes (standard DHS coding)
  if (all(fever_values %in% c(1, 2)) && 2 %in% fever_values && !0 %in% fever_values) {
    fever_yes_value <- 2
    cli::cli_alert_info(
      "Detected alternative fever coding (1=No, 2=Yes) - using 2 as 'Yes'"
    )
  } else {
    fever_yes_value <- 1
  }

  # Create binary fever indicator
  kr_u5 <- kr_u5 |>
    dplyr::mutate(
      had_fever = dplyr::if_else(fever_raw == fever_yes_value, 1, 0, missing = NA_real_)
    )

  cli::cli_alert_info(
    "Found {format(nrow(kr_u5), big.mark = ',')} alive children under 5"
  )

  kr_u5
}
