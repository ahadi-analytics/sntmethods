#' Prepare ITN Household Data for Analysis
#'
#' Shared household-level ITN data preparation.
#' Used by both calc_itn_dhs_core() and calc_itn_mbg().
#'
#' @param dhs_hr DHS Household Records dataset.
#' @param survey_vars Named list mapping DHS variable names.
#' @param include_survey_vars Logical. If TRUE, includes survey design columns.
#'
#' @return A data frame of households with columns:
#'   cluster_id, hhid, hh_size, n_itns, has_itn, potential_users.
#'   If include_survey_vars = TRUE, also: survey_weight, stratum_id.
#'
#' @noRd
.prepare_itn_household_data <- function(
  dhs_hr,
  survey_vars,
  include_survey_vars = FALSE
) {
  if (!is.data.frame(dhs_hr)) {
    cli::cli_abort("`dhs_hr` must be a data.frame or tibble")
  }

  # Find ITN variables
  itn_vars <- names(dhs_hr)[grepl(paste0("^", survey_vars$itn_prefix), names(dhs_hr))]
  if (length(itn_vars) == 0) {
    cli::cli_abort("No ITN variables found with prefix {.var {survey_vars$itn_prefix}}")
  }
  cli::cli_alert_info("Found {length(itn_vars)} ITN variables")

  hr <- dhs_hr |>
    dplyr::mutate(dplyr::across(dplyr::everything(), haven::zap_labels)) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), as.vector)) |>
    dplyr::mutate(
      n_itns = dplyr::pick(dplyr::all_of(itn_vars)) |>
        dplyr::mutate(dplyr::across(dplyr::everything(), ~ dplyr::if_else(. == 1, 1L, 0L))) |>
        rowSums(na.rm = TRUE)
    ) |>
    dplyr::transmute(
      cluster_id = .data[[survey_vars$cluster]],
      hhid = .data[[survey_vars$hhid]],
      hh_size = .data[[survey_vars$hhsize]],
      n_itns = n_itns,
      has_itn = as.integer(n_itns >= 1),
      potential_users = pmin(n_itns * 2, hh_size)
    )

  if (include_survey_vars && !is.null(survey_vars$weight)) {
    hr <- hr |>
      dplyr::mutate(
        survey_weight = dhs_hr[[survey_vars$weight]] / 1e6,
        stratum_id = dhs_hr[[survey_vars$stratum]]
      )
  }

  hr
}


#' Prepare ITN Person Data for Analysis
#'
#' Shared person-level ITN data preparation including deterministic access
#' assignment. Used by both calc_itn_dhs_core() and calc_itn_mbg().
#'
#' @param dhs_pr DHS Person Records dataset.
#' @param hr_data Prepared household data from .prepare_itn_household_data().
#' @param survey_vars Named list mapping DHS variable names.
#' @param include_survey_vars Logical. If TRUE, includes survey design columns.
#'
#' @return A data frame with individual-level ITN data including:
#'   cluster_id, hhid, age, sex, itn_used, is_pregnant, has_access.
#'
#' @noRd
.prepare_itn_person_data <- function(
  dhs_pr,
  hr_data,
  survey_vars,
  include_survey_vars = FALSE
) {
  if (!is.data.frame(dhs_pr)) {
    cli::cli_abort("`dhs_pr` must be a data.frame or tibble")
  }

  pr <- dhs_pr |>
    dplyr::mutate(dplyr::across(dplyr::everything(), haven::zap_labels)) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), as.vector)) |>
    dplyr::transmute(
      cluster_id = .data[[survey_vars$cluster]],
      hhid = .data[[survey_vars$hhid]],
      age = .data[[survey_vars$age]],
      sex = .data[[survey_vars$sex]],
      itn_used = dplyr::if_else(
        .data[[survey_vars$itn_use]] == 1L,
        1L, 0L, missing = 0L
      ),
      is_pregnant = if (survey_vars$pregnant %in% names(dhs_pr)) {
        dplyr::if_else(.data[[survey_vars$pregnant]] == 1, 1L, 0L, missing = 0L)
      } else {
        0L
      }
    )

  if (include_survey_vars && !is.null(survey_vars$weight)) {
    pr <- pr |>
      dplyr::mutate(
        survey_weight = dhs_pr[[survey_vars$weight]] / 1e6,
        stratum_id = dhs_pr[[survey_vars$stratum]]
      )
  }

  # Merge household ITN info
  pr <- pr |>
    dplyr::left_join(
      hr_data |> dplyr::select(cluster_id, hhid, hh_size, n_itns, potential_users),
      by = c("cluster_id", "hhid")
    )

  # Deterministic access assignment (standard DHS methodology)
  pr <- pr |>
    dplyr::arrange(cluster_id, hhid, dplyr::desc(itn_used)) |>
    dplyr::group_by(cluster_id, hhid) |>
    dplyr::mutate(
      person_index = dplyr::row_number(),
      has_access = dplyr::if_else(
        !is.na(potential_users) & !is.na(hh_size) &
          person_index <= pmin(potential_users, hh_size),
        1L, 0L
      )
    ) |>
    dplyr::ungroup()

  pr
}
