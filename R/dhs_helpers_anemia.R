#' Prepare Anemia Data for Analysis
#'
#' Shared data cleaning and indicator computation for anemia functions.
#' Used by both calc_severe_anemia_dhs_core() and calc_anemia_mbg().
#'
#' @param dhs_pr DHS Person Records dataset.
#' @param survey_vars Named list mapping DHS variable names.
#' @param age_min Minimum age in months (default: 6).
#' @param age_max Maximum age in months (default: 59).
#' @param include_survey_vars Logical. If TRUE, includes survey design columns.
#'
#' @return A data frame of eligible children with columns:
#'   cluster_id, age, hemoglobin (g/dL), and binary indicators:
#'   has_any_anemia, has_moderate_plus, has_severe,
#'   has_mild_only, has_moderate_only, has_severe_only.
#'   If include_survey_vars = TRUE, also: survey_weight, stratum_id, adm1.
#'
#' @noRd
.prepare_anemia_data <- function(
  dhs_pr,
  survey_vars,
  age_min = 6,
  age_max = 59,
  include_survey_vars = FALSE
) {
  if (!is.data.frame(dhs_pr)) {
    cli::cli_abort("`dhs_pr` must be a data.frame or tibble")
  }
  if (nrow(dhs_pr) == 0) {
    cli::cli_abort("`dhs_pr` is empty")
  }

  # Check hemoglobin variable
  if (!survey_vars$hemoglobin %in% names(dhs_pr)) {
    cli::cli_warn(
      "Hemoglobin variable {.var {survey_vars$hemoglobin}} not found; anemia not available for this survey"
    )
    return(NULL)
  }

  # Zap labels
  pr <- dhs_pr |>
    dplyr::mutate(dplyr::across(dplyr::everything(), haven::zap_labels)) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), as.vector))

  # Check whether mother/hv042 is present (optional — absent in some MIS surveys)
  has_mother_col <- !is.null(survey_vars$mother) && survey_vars$mother %in% names(dhs_pr)
  if (!has_mother_col) {
    cli::cli_alert_warning(
      "Column {.var {survey_vars$mother}} not found in dhs_pr; ",
      "skipping mother-listed-in-household filter (common in MIS surveys)"
    )
  }

  # Build core columns
  pr <- pr |>
    dplyr::mutate(
      cluster_id = .data[[survey_vars$cluster]],
      age = .data[[survey_vars$age]],
      present = .data[[survey_vars$present]],
      mother = if (has_mother_col) .data[[survey_vars$mother]] else 1L,
      hb_raw = .data[[survey_vars$hemoglobin]]
    )

  if (include_survey_vars) {
    pr <- pr |>
      dplyr::mutate(
        survey_weight = .data[[survey_vars$weight]] / 1e6,
        stratum_id = .data[[survey_vars$stratum]]
      )

    has_adm1 <- !is.null(survey_vars$adm1) && survey_vars$adm1 %in% names(dhs_pr)
    has_adm2 <- !is.null(survey_vars$adm2) && survey_vars$adm2 %in% names(dhs_pr)

    if (has_adm1) {
      pr <- pr |>
        dplyr::mutate(
          adm1 = haven::as_factor(.data[[survey_vars$adm1]]) |>
            as.character() |> toupper()
        )
    }
    if (has_adm2) {
      pr <- pr |>
        dplyr::mutate(
          adm2 = haven::as_factor(.data[[survey_vars$adm2]]) |>
            as.character() |> toupper()
        )
    }
  }

  # Filter to eligible children with valid hemoglobin
  pr <- pr |>
    dplyr::filter(
      present == 1,
      mother == 1,
      age >= age_min,
      age <= age_max,
      !is.na(hb_raw),
      hb_raw < 900
    ) |>
    dplyr::mutate(
      hemoglobin = hb_raw / 10
    )

  if (nrow(pr) == 0) {
    cli::cli_warn(
      "No valid hemoglobin values in {.var {survey_vars$hemoglobin}}; skipping anemia"
    )
    return(NULL)
  }

  cli::cli_alert_info(
    "Found {format(nrow(pr), big.mark = ',')} children with Hb measurements"
  )

  # Define thresholds (g/dL)
  threshold_any <- 11
  threshold_moderate <- 10
  threshold_severe <- 8

  pr <- pr |>
    dplyr::mutate(
      has_any_anemia = as.integer(hemoglobin < threshold_any),
      has_moderate_plus = as.integer(hemoglobin < threshold_moderate),
      has_severe = as.integer(hemoglobin < threshold_severe),
      has_mild_only = as.integer(
        hemoglobin >= threshold_moderate & hemoglobin < threshold_any
      ),
      has_moderate_only = as.integer(
        hemoglobin >= threshold_severe & hemoglobin < threshold_moderate
      ),
      has_severe_only = as.integer(hemoglobin < threshold_severe)
    )

  pr
}
