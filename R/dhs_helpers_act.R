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


#' Merge Febrile KR Children with PR RDT Results
#'
#' Links febrile U5 children from the KR file to their RDT results in the
#' PR (Person Recode) file. The merge uses cluster number (v001), household
#' number (v002), and child line number (b16_01) as the linkage key.
#'
#' @param kr_fever Febrile U5 data prepared by .prepare_act_data().
#' @param dhs_pr DHS Person Recode (PR) dataset containing hml35 (RDT result).
#' @param kr_cluster_var Column in kr_fever for geographic cluster number
#'   (default: "v001"). Used for PR linkage; distinct from survey design PSU.
#' @param kr_hh_var KR column for household number (default: "v002").
#' @param kr_line_var KR column for the child's line number in the household
#'   (default: "b16_01"). Links to PR hvidx.
#'
#' @return A data frame of febrile children matched to valid RDT results,
#'   containing all kr_fever columns plus: rdt_result (0/1) and
#'   has_rdt_pos (integer 0/1). Returns NULL if hml35 is absent, link
#'   variables are missing, or no children can be matched.
#'
#' @noRd
.merge_kr_pr_febrile <- function(
  kr_fever,
  dhs_pr,
  kr_cluster_var = "v001",
  kr_hh_var = "v002",
  kr_line_var = "b16_01"
) {
  if (!is.data.frame(dhs_pr)) {
    cli::cli_alert_warning(
      "`dhs_pr` must be a data.frame - skipping febrile RDT indicators"
    )
    return(NULL)
  }

  rdt_var <- "hml35"
  if (!rdt_var %in% names(dhs_pr)) {
    cli::cli_alert_warning(
      "RDT variable {.var {rdt_var}} not found in PR data - skipping febrile RDT indicators"
    )
    return(NULL)
  }

  missing_link <- setdiff(c(kr_cluster_var, kr_hh_var, kr_line_var), names(kr_fever))
  if (length(missing_link) > 0) {
    cli::cli_alert_warning(
      "KR link variables {.var {missing_link}} not found - skipping febrile RDT indicators"
    )
    return(NULL)
  }

  # Zap labels on PR
  pr <- dhs_pr |>
    dplyr::mutate(dplyr::across(dplyr::everything(), haven::zap_labels)) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), as.vector))

  # Subset PR: link vars + RDT result (valid tests only: 0 = negative, 1 = positive)
  pr_link <- pr |>
    dplyr::select(
      pr_cluster = hv001,
      pr_hh      = hv002,
      pr_line    = hvidx,
      rdt_result = !!rdt_var
    ) |>
    dplyr::filter(rdt_result %in% c(0, 1))

  if (nrow(pr_link) == 0) {
    cli::cli_alert_warning(
      "No valid RDT results (0/1) found in PR data - skipping febrile RDT indicators"
    )
    return(NULL)
  }

  # Build join key: KR column names → PR column names
  join_key <- stats::setNames(
    c("pr_cluster", "pr_hh", "pr_line"),
    c(kr_cluster_var, kr_hh_var, kr_line_var)
  )

  merged <- dplyr::inner_join(kr_fever, pr_link, by = join_key)

  n_total   <- nrow(kr_fever)
  n_matched <- nrow(merged)

  if (n_matched == 0) {
    cli::cli_alert_warning(
      "No febrile children matched to RDT results in PR data - skipping febrile RDT indicators"
    )
    return(NULL)
  }

  cli::cli_alert_info(
    "Matched {format(n_matched, big.mark = ',')} of {format(n_total, big.mark = ',')} febrile children to RDT results"
  )

  merged |>
    dplyr::mutate(has_rdt_pos = as.integer(rdt_result == 1))
}
