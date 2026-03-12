#' Prepare IPTp Data for Analysis
#'
#' Shared data cleaning and indicator computation for IPTp functions.
#' Used by both calc_iptp_dhs_core() and calc_iptp_mbg().
#'
#' @param dhs_ir DHS Individual Recode dataset.
#' @param survey_vars Named list mapping DHS variable names.
#' @param birth_window_months Months to look back for births.
#' @param include_survey_vars Logical. If TRUE, includes survey design columns.
#'
#' @return A data frame of eligible women with columns:
#'   cluster_id, sp_doses, and binary indicators:
#'   has_1plus, has_2plus, has_3plus, has_1only, has_2only, has_3only.
#'   If include_survey_vars = TRUE, also: survey_weight, stratum_id.
#'
#' @noRd
.prepare_iptp_data <- function(
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


  # Check SP variable -- prefer dose count (ml1_1) over binary (m49a_1)
  sp_var <- survey_vars$sp_doses %||% survey_vars$sp_taken
  if (!sp_var %in% names(dhs_ir)) {
    # Fallback: try sp_taken if sp_doses column missing
    sp_fallback <- survey_vars$sp_taken
    if (!is.null(sp_fallback) && sp_fallback != sp_var &&
        sp_fallback %in% names(dhs_ir)) {
      cli::cli_warn(
        "IPTp dose variable {.var {sp_var}} not found; falling back to binary {.var {sp_fallback}} (only IPTp 1+ will be meaningful)"
      )
      sp_var <- sp_fallback
    } else {
      cli::cli_warn(
        "IPTp variable {.var {sp_var}} not found; IPTp not available for this survey"
      )
      return(NULL)
    }
  }

  # Zap labels
  ir <- dhs_ir |>
    dplyr::mutate(dplyr::across(dplyr::everything(), haven::zap_labels)) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), as.vector))

  # Build core columns (force numeric to guard against haven character residuals)
  ir <- ir |>
    dplyr::mutate(
      cluster_id = .data[[survey_vars$cluster]],
      interview_cmc = suppressWarnings(as.numeric(as.character(
        .data[[survey_vars$interview_date %||% survey_vars$interview_cmc]]
      ))),
      birth_cmc = suppressWarnings(as.numeric(as.character(
        .data[[survey_vars$birth_date %||% survey_vars$birth_cmc]]
      ))),
      sp_doses = suppressWarnings(as.numeric(as.character(.data[[sp_var]])))
    )

  if (include_survey_vars) {
    ir <- ir |>
      dplyr::mutate(
        survey_weight = .data[[survey_vars$weight]] / 1e6,
        stratum_id = .data[[survey_vars$stratum]]
      )

    has_adm1 <- !is.null(survey_vars$adm1) && survey_vars$adm1 %in% names(dhs_ir)
    if (has_adm1) {
      ir <- ir |>
        dplyr::mutate(
          adm1 = haven::as_factor(.data[[survey_vars$adm1]]) |>
            as.character() |> toupper()
        )
    }
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

  # Filter valid SP responses (0-7 are valid dose counts)
  ir <- ir |>
    dplyr::filter(
      !is.na(sp_doses),
      sp_doses <= 7
    )

  if (nrow(ir) == 0) {
    cli::cli_abort("No eligible women with valid IPTp data found")
  }

  cli::cli_alert_info(
    "Found {format(nrow(ir), big.mark = ',')} women with births in last {birth_window_months} months"
  )

  # Calculate IPTp indicators
  ir <- ir |>
    dplyr::mutate(
      has_1plus = as.integer(sp_doses >= 1),
      has_2plus = as.integer(sp_doses >= 2),
      has_3plus = as.integer(sp_doses >= 3),
      has_4plus = as.integer(sp_doses >= 4),
      has_1only = as.integer(sp_doses == 1),
      has_2only = as.integer(sp_doses == 2),
      has_3only = as.integer(sp_doses == 3)
    )

  ir
}
