#' Prepare CSB Data for Analysis
#'
#' Shared data cleaning and indicator computation for CSB functions.
#' Used by both calc_csb_dhs_core() and calc_csb_mbg().
#'
#' @param dhs_kr DHS Children's Recode (KR) dataset.
#' @param survey_vars Named list mapping DHS variable names. Must include:
#'   cluster, age, fever. Optionally: weight, stratum.
#' @param csb_classification Data frame with variable and csb columns.
#'   If NULL, uses default WMR classification.
#' @param include_survey_vars Logical. If TRUE, includes survey_weight and
#'   stratum_id columns for DHS survey design. If FALSE, omits them (for MBG).
#'
#' @return A data frame of febrile children with columns:
#'   cluster_id, age_months, and binary indicators:
#'   csb_public, csb_private, csb_trained, csb_any, csb_none.
#'   If include_survey_vars = TRUE, also: survey_weight, stratum_id.
#'   Plus any additional columns from the original KR data needed downstream.
#'
#' @noRd
.prepare_csb_data <- function(
  dhs_kr,
  survey_vars,
  csb_classification = NULL,
  include_survey_vars = FALSE
) {
  # Input validation
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

  # Default classification
  if (is.null(csb_classification)) {
    csb_classification <- .default_csb_classification()
  }

  # Auto-detect h32 variables
  available_h32 <- grep("^h32[a-z0-9]+$", names(dhs_kr), value = TRUE)
  if (length(available_h32) == 0) {
    cli::cli_abort("No h32 treatment-seeking variables found in data.")
  }

  # Warn if any detected h32 variables are not in the classification table
  expected_h32 <- csb_classification$variable
  unexpected_h32 <- setdiff(available_h32, expected_h32)
  if (length(unexpected_h32) > 0) {
    cli::cli_warn(
      "Detected h32 variables not in standard classification: {paste(unexpected_h32, collapse = ', ')}. These may be country-specific non-standard slots. Check that the default classification is appropriate or supply a custom csb_classification."
    )
  }

  # Filter classification to available variables
  csb_classification <- csb_classification |>
    dplyr::filter(variable %in% available_h32)

  h32_cols <- intersect(csb_classification$variable, names(dhs_kr))

  # Zap labels and build base dataset
  kr <- dhs_kr |>
    dplyr::mutate(dplyr::across(dplyr::everything(), haven::zap_labels)) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), as.vector))

  # Build selection
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

  # Filter to U5 children with fever
  kr_fever <- kr |>
    dplyr::filter(
      age_months >= 0,
      age_months <= 59,
      had_fever == 1
    )

  if (nrow(kr_fever) == 0) {
    cli::cli_abort("No children with fever in the last 2 weeks found.")
  }

  cli::cli_alert_info(
    "Found {format(nrow(kr_fever), big.mark = ',')} children under 5 with fever"
  )

  # Apply care-seeking classification from h32 variables
  kr_fever <- .classify_csb_from_h32(kr_fever, h32_cols, csb_classification)

  # For DHS, rename to match expected downstream names
  if (include_survey_vars) {
    kr_fever <- kr_fever |>
      dplyr::mutate(
        csb_any_treatment = csb_any,
        csb_no_treatment = csb_none,
        csb_trained_provider = csb_trained
      )
  }

  kr_fever
}


#' Classify Care-Seeking from h32 Variables
#'
#' Applies the h32 treatment-seeking classification to a data frame that
#' already contains h32 columns. Creates binary indicators for care-seeking
#' categories: csb_public, csb_private, csb_any, csb_none, csb_trained.
#'
#' This is the core classification logic used by \code{.prepare_csb_data()}
#' and also reused by ACT/antimalarial MBG functions to create public
#' care-seeking subsets.
#'
#' @param data Data frame containing h32 columns (already filtered to the
#'   target population, e.g. febrile U5).
#' @param h32_cols Character vector of h32 column names present in data.
#'   If NULL, auto-detects from column names.
#' @param csb_classification Data frame with variable and csb columns.
#'   If NULL, uses default WMR classification.
#'
#' @return The input data frame with added columns: .row_id, has_public,
#'   has_chw, has_private_formal, has_private_informal, has_pharmacy,
#'   csb_public, csb_private, csb_private_formal_pha, csb_any, csb_none,
#'   csb_trained.
#'
#' @noRd
.classify_csb_from_h32 <- function(data, h32_cols = NULL,
                                    csb_classification = NULL) {
  # Default classification
  if (is.null(csb_classification)) {
    csb_classification <- .default_csb_classification()
  }

  # Auto-detect h32 columns if not provided
  if (is.null(h32_cols)) {
    available_h32 <- grep("^h32[a-z0-9]+$", names(data), value = TRUE)
    if (length(available_h32) == 0) {
      cli::cli_abort("No h32 treatment-seeking variables found in data.")
    }
    csb_classification <- csb_classification |>
      dplyr::filter(variable %in% available_h32)
    h32_cols <- intersect(csb_classification$variable, names(data))
  }

  if (length(h32_cols) == 0) {
    cli::cli_abort("No h32 treatment-seeking variables found in data.")
  }

  data <- data |>
    dplyr::mutate(.row_id = dplyr::row_number())

  # Convert h32 to binary and reshape
  kr_long <- data |>
    dplyr::select(.row_id, dplyr::all_of(h32_cols)) |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(h32_cols),
      names_to = "variable",
      values_to = "visited"
    ) |>
    dplyr::left_join(
      csb_classification |> dplyr::select(variable, csb),
      by = "variable"
    ) |>
    dplyr::filter(visited == 1)

  # Aggregate to base categories per child
  if (nrow(kr_long) > 0) {
    base_cats <- kr_long |>
      dplyr::group_by(.row_id, csb) |>
      dplyr::summarise(visited = 1L, .groups = "drop") |>
      tidyr::pivot_wider(
        names_from = csb,
        values_from = visited,
        values_fill = 0L,
        names_prefix = "has_"
      )
    data <- data |>
      dplyr::left_join(base_cats, by = ".row_id")
  }

  # Ensure all base categories exist
  for (col in c("has_public", "has_chw", "has_private_formal",
                 "has_private_informal", "has_pharmacy")) {
    if (!col %in% names(data)) {
      data[[col]] <- 0L
    }
    data[[col]] <- tidyr::replace_na(data[[col]], 0L)
  }

  # Create derived indicators
  data |>
    dplyr::mutate(
      csb_public = as.numeric(has_public == 1 | has_chw == 1),
      csb_private = as.numeric(
        has_private_formal == 1 | has_private_informal == 1 | has_pharmacy == 1
      ),
      csb_private_formal_pha = as.numeric(
        has_private_formal == 1 | has_pharmacy == 1
      ),
      csb_any = as.numeric(csb_public == 1 | csb_private == 1),
      csb_none = as.numeric(csb_public == 0 & csb_private == 0),
      csb_trained = as.numeric(csb_public == 1 | csb_private_formal_pha == 1)
    )
}
