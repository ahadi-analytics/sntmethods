#' Prepare PfPR Data for Analysis
#'
#' Shared data cleaning and indicator computation for PfPR functions.
#' Used by both calc_pfpr_dhs_core() and calc_pfpr_mbg().
#'
#' @param dhs_pr DHS Person Records dataset.
#' @param survey_vars Named list mapping DHS variable names.
#' @param age_min Minimum age in months (default: 6).
#' @param age_max Maximum age in months (default: 59).
#' @param include_survey_vars Logical. If TRUE, includes survey design columns.
#'
#' @return A data frame of eligible children with columns:
#'   cluster_id, age, rdt_res, mic_res, tested_rdt, tested_mic, rdt_pos, mic_pos.
#'   If include_survey_vars = TRUE, also: survey_weight, stratum_id, adm1, (adm2).
#'
#' @noRd
.prepare_pfpr_data <- function(
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

  # Check required columns
  needed <- c(survey_vars$cluster, survey_vars$age,
               survey_vars$present, survey_vars$mother)
  missing_cols <- setdiff(needed, names(dhs_pr))
  if (length(missing_cols) > 0) {
    cli::cli_abort("Columns not found in dhs_pr: {.var {missing_cols}}")
  }

  # Zap labels
  pr <- dhs_pr |>
    dplyr::mutate(dplyr::across(dplyr::everything(), haven::zap_labels)) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), as.vector))

  # Build core columns
  pr <- pr |>
    dplyr::mutate(
      cluster_id = .data[[survey_vars$cluster]],
      age = .data[[survey_vars$age]],
      present = .data[[survey_vars$present]],
      mother = .data[[survey_vars$mother]],
      rdt_res = if (survey_vars$rdt %in% names(dhs_pr)) .data[[survey_vars$rdt]] else NA_real_,
      mic_res = if (survey_vars$mic %in% names(dhs_pr)) .data[[survey_vars$mic]] else NA_real_
    )

  if (include_survey_vars) {
    pr <- pr |>
      dplyr::mutate(
        survey_weight = .data[[survey_vars$weight]] / 1e6
      )

    # Handle admin columns
    has_adm1 <- !is.null(survey_vars$adm1) && survey_vars$adm1 %in% names(dhs_pr)
    has_adm2 <- !is.null(survey_vars$adm2) && survey_vars$adm2 %in% names(dhs_pr)

    pr <- pr |>
      dplyr::mutate(
        adm1 = if (has_adm1) {
          haven::as_factor(.data[[survey_vars$adm1]]) |> as.character() |> toupper()
        } else NA_character_,
        adm2 = if (has_adm2) {
          haven::as_factor(.data[[survey_vars$adm2]]) |> as.character() |> toupper()
        } else NA_character_
      )

    # Zap labels again after as_factor
    pr <- pr |>
      dplyr::mutate(dplyr::across(dplyr::everything(), haven::zap_labels))

    # Build stratum
    strata_fields <- character(0)
    if (!is.null(survey_vars$stratum) && survey_vars$stratum %in% names(dhs_pr)) {
      strata_fields <- survey_vars$stratum
    } else {
      if (has_adm1) strata_fields <- c(strata_fields, survey_vars$adm1)
      if ("hv025" %in% names(dhs_pr)) strata_fields <- c(strata_fields, "hv025")
      if (length(strata_fields) == 0 && "hv022" %in% names(dhs_pr)) {
        strata_fields <- "hv022"
      }
    }
    pr <- pr |>
      dplyr::mutate(
        stratum_id = interaction(!!!rlang::syms(strata_fields), drop = TRUE)
      )

    if (!has_adm2) {
      pr <- pr |> dplyr::select(-adm2)
    }
  }

  # Create test flags
  pr <- pr |>
    dplyr::mutate(
      tested_rdt = as.numeric(dplyr::if_else(
        present == 1 & mother == 1 & age >= age_min & age <= age_max &
          rdt_res %in% c(0, 1),
        1, 0, missing = NA_real_
      )),
      tested_mic = as.numeric(dplyr::if_else(
        present == 1 & mother == 1 & age >= age_min & age <= age_max &
          mic_res %in% c(0, 1, 6),
        1, 0, missing = NA_real_
      )),
      rdt_pos = as.numeric(dplyr::case_when(
        present == 1 & mother == 1 & age >= age_min & age <= age_max &
          rdt_res == 1 ~ 1,
        present == 1 & mother == 1 & age >= age_min & age <= age_max &
          rdt_res == 0 ~ 0,
        TRUE ~ NA_real_
      )),
      mic_pos = as.numeric(dplyr::case_when(
        present == 1 & mother == 1 & age >= age_min & age <= age_max &
          mic_res == 1 ~ 1,
        present == 1 & mother == 1 & age >= age_min & age <= age_max &
          mic_res %in% c(0, 6) ~ 0,
        TRUE ~ NA_real_
      ))
    )

  pr
}
