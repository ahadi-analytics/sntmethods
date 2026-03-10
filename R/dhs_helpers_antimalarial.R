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

  # Auto-detect available antimalarial variables.
  # Prefer ml13* series (drug-specific, newer surveys);
  # fall back to h37* series (older DHS surveys use h37a-h for drug-specific treatment).
  # NOTE: ml1 is NOT a safe fallback — in some surveys (e.g. BFA 2021) ml1 = "Times took
  #       Fansidar during pregnancy" (IPTp), not child fever treatment.
  ml13_vars <- grep("^ml13[a-z]*$", names(dhs_kr), value = TRUE)
  h37_vars <- grep("^h37[a-h]$", names(dhs_kr), value = TRUE)
  use_h37_fallback <- FALSE

  if (length(ml13_vars) > 0) {
    # Check if ml13 series has any positive values
    ml13_has_data <- any(
      sapply(ml13_vars, function(v) any(dhs_kr[[v]] == 1, na.rm = TRUE))
    )
    if (ml13_has_data) {
      cli::cli_alert_info(
        "Detected {length(ml13_vars)} ml13 antimalarial variables: {paste(ml13_vars, collapse = ', ')}"
      )
    } else if (length(h37_vars) > 0) {
      h37_has_data <- any(
        sapply(h37_vars, function(v) any(dhs_kr[[v]] == 1, na.rm = TRUE))
      )
      if (h37_has_data) {
        cli::cli_alert_info(
          "ml13* variables have no positive values; using h37* series which has data: {paste(h37_vars, collapse = ', ')}"
        )
        ml13_vars <- character(0)
        use_h37_fallback <- TRUE
      } else {
        cli::cli_alert_info(
          "Detected {length(ml13_vars)} ml13 antimalarial variables (no positive values found)"
        )
      }
    } else {
      cli::cli_alert_info(
        "Detected {length(ml13_vars)} ml13 antimalarial variables (no positive values found)"
      )
    }
  } else if (length(h37_vars) > 0) {
    cli::cli_alert_info(
      "No ml13* variables found; using h37* series as fallback: {paste(h37_vars, collapse = ', ')}"
    )
    use_h37_fallback <- TRUE
  } else {
    cli::cli_abort(c(
      "No antimalarial treatment variables found in data.",
      "i" = "Checked for ml13a/ml13b/... (newer surveys) and h37a-h (older surveys).",
      "i" = "Verify that this survey includes malaria treatment questions."
    ))
  }

  if (length(ml13_vars) == 0 && !use_h37_fallback) {
    cli::cli_abort("No antimalarial treatment variables with data found.")
  }

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

  # Check alive variable if present
  has_alive <- !is.null(survey_vars$alive) &&
    survey_vars$alive %in% names(dhs_kr)
  if (has_alive) {
    kr <- kr |>
      dplyr::mutate(child_alive = .data[[survey_vars$alive]])
  }

  # Filter to febrile U5 children
  kr_fever <- kr |>
    dplyr::filter(
      age_months >= 0,
      age_months <= 59,
      had_fever == 1
    )

  if (has_alive) {
    kr_fever <- kr_fever |>
      dplyr::filter(child_alive == 1)
  }

  if (nrow(kr_fever) == 0) {
    cli::cli_abort("No children with fever in the last 2 weeks found.")
  }

  # Create binary antimalarial indicator
  if (use_h37_fallback) {
    # h37* series: 1 if ANY drug variable == 1
    # Each h37x records whether a specific drug was taken for fever/cough
    h37_matrix <- as.matrix(kr_fever[, h37_vars, drop = FALSE])
    h37_matrix[!h37_matrix %in% c(0, 1)] <- NA
    kr_fever$received_antimalarial <- apply(h37_matrix, 1, function(row) {
      if (all(is.na(row))) return(NA_real_)
      if (any(row == 1, na.rm = TRUE)) return(1)
      return(0)
    })
    attr(kr_fever, "ml13_vars_found") <- h37_vars
  } else {
    # ml13* series: 1 if ANY drug variable == 1
    ml13_matrix <- as.matrix(kr_fever[, ml13_vars, drop = FALSE])
    ml13_matrix[!ml13_matrix %in% c(0, 1)] <- NA
    kr_fever$received_antimalarial <- apply(ml13_matrix, 1, function(row) {
      if (all(is.na(row))) return(NA_real_)
      if (any(row == 1, na.rm = TRUE)) return(1)
      return(0)
    })
    attr(kr_fever, "ml13_vars_found") <- ml13_vars
  }

  if (all(is.na(kr_fever$received_antimalarial))) {
    cli::cli_abort("All antimalarial variables are NA for febrile children")
  }

  # Create binary indicator for survey estimation
  kr_fever <- kr_fever |>
    dplyr::mutate(
      has_antimalarial = dplyr::if_else(
        received_antimalarial == 1, 1, 0, missing = NA_real_
      )
    )

  cli::cli_alert_info(
    "Found {format(nrow(kr_fever), big.mark = ',')} febrile children under 5"
  )

  kr_fever
}
