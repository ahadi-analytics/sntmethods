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

  if (has_alive) {
    kr_fever <- kr_fever |>
      dplyr::filter(child_alive == 1)
  }

  if (nrow(kr_fever) == 0) {
    cli::cli_abort("No children with fever in the last 2 weeks found.")
  }

  cli::cli_alert_info(
    "Found {format(nrow(kr_fever), big.mark = ',')} children under 5 with fever"
  )

  # Apply care-seeking classification from h32 variables
  kr_fever <- .classify_csb_from_h32(kr_fever, h32_cols, csb_classification)

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
      # Composite sectors
      csb_public = as.numeric(
        has_public == 1 | has_chw == 1
      ),
      csb_private = as.numeric(
        has_private_formal == 1 |
          has_private_informal == 1 |
          has_pharmacy == 1
      ),
      csb_private_formal_pha = as.numeric(
        has_private_formal == 1 | has_pharmacy == 1
      ),
      csb_any = as.numeric(
        csb_public == 1 | csb_private == 1
      ),
      csb_none = as.numeric(
        csb_public == 0 & csb_private == 0
      ),
      csb_trained = as.numeric(
        csb_public == 1 | csb_private_formal_pha == 1
      ),
      # Granular sectors (WMR indicator alignment)
      csb_public_nochw = as.numeric(has_public == 1),
      csb_chw = as.numeric(has_chw == 1),
      csb_private_formal_ind = as.numeric(
        has_private_formal == 1
      ),
      csb_pharmacy = as.numeric(has_pharmacy == 1),
      csb_private_informal = as.numeric(
        has_private_informal == 1
      ),
      # Aliases for downstream naming consistency
      csb_any_treatment = csb_any,
      csb_trained_provider = csb_trained,
      csb_no_treatment = csb_none
    )
}


#' Detect CSB classification from haven variable labels
#'
#' Scans h32 variables for haven labels and classifies each into a CSB
#' category (public, chw, private_formal, pharmacy, private_informal)
#' based on label content. This is the same approach used for ACT detection
#' in `.detect_act_vars()` — label-based, not slot-based.
#'
#' DHS variable slots change between survey versions (DHS-7 vs DHS-8), so
#' hardcoded slot-to-category mappings break. Label detection works across
#' all versions.
#'
#' @param dhs_kr Original DHS dataset with haven labels intact (pre-zap).
#' @param fallback_classification Data frame with variable and csb columns.
#'   Used as fallback for variables whose labels don't match any pattern.
#'   If NULL, uses `.default_csb_classification()`.
#'
#' @return Data frame with columns: variable, csb — one row per classified
#'   h32 variable found in the data. Excludes meta variables (h32y, h32z)
#'   and unclassified/NA-prefix variables.
#' @noRd
.detect_csb_from_labels <- function(dhs_kr,
                                     fallback_classification = NULL) {

  available_h32 <- grep("^h32[a-z0-9]+$", names(dhs_kr), value = TRUE)
  if (length(available_h32) == 0) return(data.frame(
    variable = character(0), csb = character(0), stringsAsFactors = FALSE
  ))

  # Exclude meta variables (no treatment / medical treatment flags)
  meta_vars <- c("h32y", "h32z")
  available_h32 <- setdiff(available_h32, meta_vars)

  if (is.null(fallback_classification)) {
    fallback_classification <- .default_csb_classification()
  }

  # Label patterns — ordered from most specific to most general.
  # Same philosophy as .detect_act_vars(): scan haven labels, not slot letters.
  #
  # CHW / community health worker / fieldworker
  chw_pattern <- paste0(
    "community.?health.?worker|\\bchw\\b|field.?worker|",
    "community.oriented.resource|",
    "agent.communautaire|relais.communautaire"
  )
  # Pharmacy / drug shop / chemist / PPMV
  pharm_pattern <- paste0(
    "\\bpharmac|drug.?shop|drug.?store|\\bchemist|\\bppmv\\b|",
    "patent.medicine"
  )
  # NGO sector (treated as public for WMR)
  ngo_pattern <- "\\bngo\\b|non.governmental|faith.based"
  # Public sector (government / dispensary / health center / gov mobile clinic)
  public_pattern <- paste0(
    "government|\\bgov\\b|public.sector|other.public|dispensar|",
    "health.center|health.centre|health.post|\\bmch\\b"
  )
  # Private formal (hospital / clinic / doctor / private mobile)
  priv_formal_pattern <- paste0(
    "private.hospital|private.clinic|private.doctor|",
    "private.mobile|other.private"
  )
  # Private informal (shop / market / traditional / itinerant / drug seller)
  priv_informal_pattern <- paste0(
    "\\bshop\\b|\\bmarket\\b|traditional|\\bitinerant\\b|",
    "drug.seller"
  )

  result <- data.frame(
    variable = character(0), csb = character(0),
    stringsAsFactors = FALSE
  )

  for (v in available_h32) {
    lbl <- attr(dhs_kr[[v]], "label")

    # Skip NA-prefixed labels (unused DHS country-specific slots)
    if (!is.null(lbl) && is.character(lbl) && length(lbl) == 1 &&
        grepl("^NA\\s*-", lbl)) {
      next
    }

    csb_cat <- NA_character_

    if (!is.null(lbl) && is.character(lbl) && length(lbl) == 1) {
      # Match most specific first
      if (grepl(chw_pattern, lbl, ignore.case = TRUE)) {
        csb_cat <- "chw"
      } else if (grepl(pharm_pattern, lbl, ignore.case = TRUE)) {
        csb_cat <- "pharmacy"
      } else if (grepl(ngo_pattern, lbl, ignore.case = TRUE)) {
        # NGO treated as public sector for WMR purposes
        csb_cat <- "public"
      } else if (grepl(public_pattern, lbl, ignore.case = TRUE)) {
        csb_cat <- "public"
      } else if (grepl(priv_formal_pattern, lbl, ignore.case = TRUE)) {
        csb_cat <- "private_formal"
      } else if (grepl(priv_informal_pattern, lbl, ignore.case = TRUE,
                        perl = TRUE)) {
        csb_cat <- "private_informal"
      }
    }

    # Fallback to hardcoded classification if label didn't match
    if (is.na(csb_cat)) {
      fb_row <- fallback_classification[
        fallback_classification$variable == v, , drop = FALSE
      ]
      if (nrow(fb_row) == 1) {
        csb_cat <- fb_row$csb
      }
    }

    # Skip if still unclassified (unusual variables like h32x "other")
    if (is.na(csb_cat)) next

    result <- rbind(result, data.frame(
      variable = v, csb = csb_cat, stringsAsFactors = FALSE
    ))
  }

  if (nrow(result) > 0) {
    # Log the classification
    cats <- table(result$csb)
    cat_str <- paste(
      names(cats), cats, sep = ": ", collapse = ", "
    )
    cli::cli_alert_info(
      "CSB classification from labels ({nrow(result)} vars): {cat_str}"
    )
  }

  result
}
