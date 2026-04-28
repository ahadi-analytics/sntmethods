#' Prepare CSB Data for Analysis
#'
#' Shared data cleaning and indicator computation for CSB functions.
#' Used by both calc_csb_dhs_core() and calc_csb_mbg().
#'
#' @param dhs_kr DHS Children's Recode (KR) dataset.
#' @param survey_vars Named list mapping DHS variable names. Must include:
#'   cluster, age, fever. Optionally: weight, stratum.
#' @param include_survey_vars Logical. If TRUE, includes survey_weight and
#'   stratum_id columns for DHS survey design. If FALSE, omits them (for MBG).
#' @param csb_priority_method Character, one of "all" (default), "first",
#'   "public", or "private". Controls how overlapping care-seeking is
#'   handled so each individual is assigned to at most one sector:
#'   \itemize{
#'     \item "all": Keep WHO methodology (overlaps allowed; a child can be
#'       in both csb_public and csb_private).
#'     \item "first": Keep the first recurring h32 source visited per child
#'       (based on h32 alphabetical order: h32a, h32b, ..., h32x). Each
#'       child is assigned to exactly one category.
#'     \item "public": Public priority - if any public/CHW care, classify
#'       as public; else private if any private; else none.
#'     \item "private": Private priority - if any private care, classify
#'       as private; else public if any public; else none.
#'   }
#'   Non-"all" options make csb_public + csb_private + csb_none sum to 100%.
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
  include_survey_vars = FALSE,
  csb_priority_method = c("all", "first", "public", "private")
) {
  csb_priority_method <- match.arg(csb_priority_method)
  # Input validation
  if (!is.data.frame(dhs_kr)) {
    cli::cli_abort("`dhs_kr` must be a data.frame or tibble.")
  }
  if (nrow(dhs_kr) == 0) {
    cli::cli_abort("`dhs_kr` is empty.")
  }

  # Auto-detect age variable if specified one is missing
  # Fallback order: hw1 (anthropometry) -> hc1 (standard KR) -> b8 (current age)
  if (!survey_vars$age %in% names(dhs_kr)) {
    age_candidates <- c("hc1", "b8", "hw1")
    available_age <- intersect(age_candidates, names(dhs_kr))

    if (length(available_age) > 0) {
      old_age_var <- survey_vars$age
      survey_vars$age <- available_age[1]
      cli::cli_alert_info(
        "Age variable {.var {old_age_var}} not found; using {.var {survey_vars$age}} instead"
      )
    }
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

  # Auto-detect h32 variables (before label zapping)
  available_h32 <- grep("^h32[a-z0-9]+$", names(dhs_kr), value = TRUE)

  # Classification: detect from haven labels first, fall back to the
  # hardcoded default mapping. Label detection correctly classifies CHW
  # and pharmacy slots across DHS-7 and DHS-8 survey versions.
  classification <- .detect_csb_from_labels(dhs_kr)
  if (nrow(classification) == 0) {
    classification <- .default_csb_classification()
  }
  if (length(available_h32) == 0) {
    cli::cli_warn("No h32 treatment-seeking variables found in data.")
    return(NULL)
  }

  # Warn if any detected h32 variables are not in the classification table
  expected_h32 <- classification$variable
  unexpected_h32 <- setdiff(available_h32, expected_h32)
  if (length(unexpected_h32) > 0) {
    cli::cli_warn(
      "Detected h32 variables not in standard classification: {paste(unexpected_h32, collapse = ', ')}. These may be country-specific non-standard slots."
    )
  }

  # Filter classification to available variables
  classification <- classification |>
    dplyr::filter(variable %in% available_h32)

  h32_cols <- intersect(classification$variable, names(dhs_kr))

  # Zap labels and build base dataset
  kr <- dhs_kr |>
    dplyr::mutate(dplyr::across(dplyr::everything(), haven::zap_labels)) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), as.vector))

  # Build selection (force numeric to guard against haven character residuals)
  # Diagnostic: check fever values before coercion
  fever_raw <- kr[[survey_vars$fever]]
  unique_fever_raw <- unique(fever_raw[!is.na(fever_raw)])

  kr <- kr |>
    dplyr::mutate(
      cluster_id = .data[[survey_vars$cluster]],
      age_months = suppressWarnings(as.numeric(as.character(.data[[survey_vars$age]]))),
      had_fever = suppressWarnings(as.numeric(as.character(.data[[survey_vars$fever]])))
    )

  # Diagnostic: check fever values after coercion
  unique_fever_coerced <- unique(kr$had_fever[!is.na(kr$had_fever)])

  if (length(unique_fever_coerced) > 0) {
    cli::cli_alert_info(
      "Fever variable ({.var {survey_vars$fever}}) unique values: {paste(sort(unique_fever_coerced), collapse = ', ')}"
    )
  } else {
    cli::cli_warn(
      "Fever variable ({.var {survey_vars$fever}}) has no non-NA values after coercion. Raw values: {paste(head(unique_fever_raw, 10), collapse = ', ')}"
    )
  }

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
      dplyr::mutate(
        child_alive = suppressWarnings(as.numeric(as.character(.data[[survey_vars$alive]])))
      )
  }

  # Filter to U5 children
  kr_u5 <- kr |>
    dplyr::filter(
      age_months >= 0,
      age_months <= 59
    )

  if (has_alive) {
    kr_u5 <- kr_u5 |>
      dplyr::filter(child_alive == 1)
  }

  cli::cli_alert_info(
    "Total children under 5 (alive): {format(nrow(kr_u5), big.mark = ',')}"
  )

  # Detect fever coding scheme: Some surveys use 0=No/1=Yes, others use 1=No/2=Yes
  # Standardize: if unique values are {1, 2}, assume 2=Yes; otherwise assume 1=Yes
  fever_values <- unique(kr_u5$had_fever[!is.na(kr_u5$had_fever)])

  if (length(fever_values) == 0) {
    cli::cli_abort(
      "Fever variable has no valid values. Check that {.var {survey_vars$fever}} exists and contains data."
    )
  }

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

  # Filter to children with fever
  kr_fever <- kr_u5 |>
    dplyr::filter(had_fever == fever_yes_value)

  if (nrow(kr_fever) == 0) {
    n_with_data <- sum(!is.na(kr_u5$had_fever))
    cli::cli_abort(c(
      "No children with fever in the last 2 weeks found.",
      "i" = "Total U5 children: {nrow(kr_u5)}",
      "i" = "Children with fever data: {n_with_data}",
      "i" = "Unique fever values: {paste(sort(fever_values), collapse = ', ')}",
      "i" = "Expected 'Yes' value: {fever_yes_value}"
    ))
  }

  cli::cli_alert_info(
    "Found {format(nrow(kr_fever), big.mark = ',')} children under 5 with fever"
  )

  # Apply care-seeking classification from h32 variables
  kr_fever <- .classify_csb_from_h32(
    kr_fever,
    h32_cols,
    classification = classification,
    csb_priority_method = csb_priority_method
  )

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
#' @param classification Data frame with variable and csb columns mapping
#'   h32 slots to sector labels. If NULL, uses the package default
#'   (see \code{.default_csb_classification()}).
#' @param csb_priority_method Character, one of "all" (default), "first",
#'   "public", or "private". Controls overlap handling. See
#'   \code{.prepare_csb_data()} for details. With non-"all" values, each
#'   child is assigned to at most one of csb_public / csb_private so that
#'   csb_public + csb_private + csb_none sums to 100%.
#'
#' @return The input data frame with added columns: .row_id, has_public,
#'   has_chw, has_private_formal, has_private_informal, has_pharmacy,
#'   csb_public, csb_private, csb_private_formal_pha, csb_any, csb_none,
#'   csb_trained.
#'
#' @noRd
.classify_csb_from_h32 <- function(data, h32_cols = NULL,
                                    classification = NULL,
                                    csb_priority_method = c("all", "first",
                                                             "public",
                                                             "private")) {
  csb_priority_method <- match.arg(csb_priority_method)
  # Default classification
  if (is.null(classification)) {
    classification <- .default_csb_classification()
  }

  # Auto-detect h32 columns if not provided
  if (is.null(h32_cols)) {
    available_h32 <- grep("^h32[a-z0-9]+$", names(data), value = TRUE)
    if (length(available_h32) == 0) {
      cli::cli_abort("No h32 treatment-seeking variables found in data.")
    }
    classification <- classification |>
      dplyr::filter(variable %in% available_h32)
    h32_cols <- intersect(classification$variable, names(data))
  }

  if (length(h32_cols) == 0) {
    cli::cli_abort("No h32 treatment-seeking variables found in data.")
  }

  # Preserve existing .row_id if present (from upstream like .prepare_act_data)
  # Use internal .csb_row_id for pivot logic to avoid overwriting
  has_original_row_id <- ".row_id" %in% names(data)
  if (has_original_row_id) {
    data$.original_row_id <- data$.row_id
  }

  data <- data |>
    dplyr::mutate(.csb_row_id = dplyr::row_number())

  # Ensure h32 columns are numeric (guards against residual haven labels
  # or character values in older DHS surveys like BDI 2012)
  for (col in h32_cols) {
    data[[col]] <- suppressWarnings(as.numeric(as.character(data[[col]])))
  }

  # Convert h32 to binary and reshape
  kr_long <- data |>
    dplyr::select(.csb_row_id, dplyr::all_of(h32_cols)) |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(h32_cols),
      names_to = "variable",
      values_to = "visited"
    ) |>
    dplyr::left_join(
      classification |> dplyr::select(variable, csb),
      by = "variable"
    ) |>
    dplyr::filter(!is.na(visited) & visited == 1)

  # Optional: keep only the FIRST recurring h32 source visited per child
  # (ordered alphabetically: h32a, h32b, ..., h32x). This makes the resulting
  # sector assignment mutually exclusive so csb_public + csb_private +
  # csb_none sums to exactly 100% at the cluster level.
  #
  # We sort alphabetically (rather than using the classification order) so
  # behavior is deterministic. The default classification groups h32 slots
  # by sector, which would otherwise change the meaning of "first".
  if (csb_priority_method == "first" && nrow(kr_long) > 0) {
    h32_sorted <- sort(h32_cols)
    h32_order <- data.frame(
      variable = h32_sorted,
      .h32_order = seq_along(h32_sorted),
      stringsAsFactors = FALSE
    )
    kr_long <- kr_long |>
      dplyr::left_join(h32_order, by = "variable") |>
      dplyr::group_by(.csb_row_id) |>
      dplyr::arrange(.h32_order, .by_group = TRUE) |>
      dplyr::slice_head(n = 1) |>
      dplyr::ungroup() |>
      dplyr::select(-.h32_order)
  }

  # Aggregate to base categories per child
  if (nrow(kr_long) > 0) {
    base_cats <- kr_long |>
      dplyr::group_by(.csb_row_id, csb) |>
      dplyr::summarise(visited = 1L, .groups = "drop") |>
      tidyr::pivot_wider(
        names_from = csb,
        values_from = visited,
        values_fill = 0L,
        names_prefix = "has_"
      )
    data <- data |>
      dplyr::left_join(base_cats, by = ".csb_row_id")
  }

  # Ensure all base categories exist
  for (col in c("has_public", "has_chw", "has_private_formal",
                 "has_private_informal", "has_pharmacy")) {
    if (!col %in% names(data)) {
      data[[col]] <- 0L
    }
    data[[col]] <- tidyr::replace_na(data[[col]], 0L)
  }

  # Sector-priority resolution for overlapping care-seeking.
  # When csb_priority_method is "public" or "private", zero out the
  # non-priority sector for children who sought both. This ensures each
  # child is classified into exactly one of public / private so that
  # csb_public + csb_private + csb_none sums to 100% at the cluster level.
  if (csb_priority_method == "public") {
    .has_public_any <- data$has_public == 1 | data$has_chw == 1
    data$has_private_formal[.has_public_any] <- 0L
    data$has_private_informal[.has_public_any] <- 0L
    data$has_pharmacy[.has_public_any] <- 0L
  } else if (csb_priority_method == "private") {
    .has_private_any <- data$has_private_formal == 1 |
      data$has_private_informal == 1 |
      data$has_pharmacy == 1
    data$has_public[.has_private_any] <- 0L
    data$has_chw[.has_private_any] <- 0L
  }

  # Create derived indicators
  result <- data |>
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
      # Granular sectors (indicator alignment)
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

  # Restore original .row_id if it was present, remove temporary columns
  if (has_original_row_id) {
    result$.row_id <- result$.original_row_id
    result$.original_row_id <- NULL
  }
  result$.csb_row_id <- NULL

  result
}


#' Detect CSB classification from haven variable labels
#'
#' Scans h32 variables for haven labels and classifies each into a CSB
#' category (public, chw, private_formal, pharmacy, private_informal)
#' based on label content. This is the same approach used for ACT detection
#' in `.detect_act_vars()` -- label-based, not slot-based.
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
#' @return Data frame with columns: variable, csb -- one row per classified
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

  # Label patterns -- ordered from most specific to most general.
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
  # NGO sector (treated as public )
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
        # NGO treated as public sector for analysis purposes
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


# ---------------------------------------------------------------------------
# Custom CSB indicator helpers (runtime-scoped)
#
# These helpers support the optional `custom_csb_indicator` argument of
# `run_mbg_pipeline()`, which lets a user define a single mutually exclusive
# care-seeking partition (`<name>_dhis`, `<name>_nondhis`, `<name>_untreat`)
# from three lists of treatment-source labels. The helpers are intentionally
# kept separate from `.classify_csb_from_h32()` so the built-in CSB pipeline
# remains untouched.
# ---------------------------------------------------------------------------

#' Built-in CSB indicator codes that custom names must not collide with.
#' @noRd
.builtin_csb_indicator_codes <- function() {
  c(
    "csb_any", "csb_public", "csb_pub_nochw", "csb_chw",
    "csb_private", "csb_priv_formal", "csb_pharmacy",
    "csb_priv_informal", "csb_priv_form_pha",
    "csb_trained", "csb_none",
    "csb_q1", "csb_q2", "csb_q3", "csb_q4", "csb_q5"
  )
}


#' Derived custom CSB sub-indicator names
#'
#' @param custom_csb_indicator Validated user spec list (must contain `name`).
#' @return Character vector `c("<name>_dhis", "<name>_nondhis", "<name>_untreat")`.
#' @noRd
.custom_csb_indicator_names <- function(custom_csb_indicator) {
  if (is.null(custom_csb_indicator)) return(character(0))
  paste0(
    custom_csb_indicator$name,
    c("_dhis", "_nondhis", "_untreat")
  )
}


#' Normalize a CSB label string for matching
#'
#' Lowercases, strips leading/trailing whitespace, and collapses runs of
#' internal whitespace to a single space. Used so user-supplied labels and
#' DHS haven labels match consistently across surveys with minor formatting
#' differences (extra spaces, mixed case).
#'
#' `NA` values are returned as-is so the helper can be vectorized over
#' character vectors that may contain `NA_character_`.
#'
#' @param x Character vector.
#' @return Character vector of the same length, lowercased and trimmed.
#' @noRd
.normalize_custom_csb_label <- function(x) {
  if (is.null(x)) return(character(0))
  if (length(x) == 0) return(character(0))
  out <- ifelse(is.na(x), NA_character_, tolower(trimws(as.character(x))))
  # Collapse runs of internal whitespace to a single space (only for non-NA)
  out <- ifelse(
    is.na(out), NA_character_, gsub("\\s+", " ", out, perl = TRUE)
  )
  out
}


#' Validate the structure of a `custom_csb_indicator` spec
#'
#' Performs all checks that can be done without a DHS dataset: presence of
#' the four required fields, type checks, name pattern, name collisions,
#' and pairwise-disjointness of the three label lists after normalization.
#' Per-survey label coverage is checked separately by
#' `.build_custom_csb_classification()`.
#'
#' @param custom_csb_indicator A list with `name`, `dhis_locs`,
#'   `nondhis_locs`, `untreat_locs`.
#' @param other_indicators Optional character vector of other indicator codes
#'   in the current pipeline run. Used to flag derived-name collisions.
#' @return Invisibly returns the spec with normalized label vectors attached
#'   as attributes (`label_norm_dhis`, etc.) for downstream reuse.
#' @noRd
.validate_custom_csb_indicator_spec <- function(
  custom_csb_indicator,
  other_indicators = character(0)
) {
  spec <- custom_csb_indicator
  if (!is.list(spec) || is.data.frame(spec)) {
    cli::cli_abort(
      "{.arg custom_csb_indicator} must be a named list."
    )
  }

  required <- c("name", "dhis_locs", "nondhis_locs", "untreat_locs")
  missing_fields <- setdiff(required, names(spec))
  if (length(missing_fields) > 0) {
    cli::cli_abort(c(
      "{.arg custom_csb_indicator} is missing required field{?s}: {.val {missing_fields}}",
      "i" = "Required fields: {.val {required}}"
    ))
  }

  # name -- single non-empty string, regex constrained, no collision
  nm <- spec$name
  if (!is.character(nm) || length(nm) != 1L || is.na(nm) || !nzchar(nm)) {
    cli::cli_abort(
      "{.field custom_csb_indicator$name} must be a single non-empty character."
    )
  }
  if (!grepl("^csb_[a-z0-9_]+$", nm)) {
    cli::cli_abort(c(
      "{.field custom_csb_indicator$name} = {.val {nm}} is not a valid identifier.",
      "i" = "Must match pattern {.code ^csb_[a-z0-9_]+$} (lowercase letters, digits, underscores)."
    ))
  }
  builtin <- .builtin_csb_indicator_codes()
  if (nm %in% builtin) {
    cli::cli_abort(c(
      "{.field custom_csb_indicator$name} = {.val {nm}} collides with a built-in CSB indicator.",
      "i" = "Pick a different prefix (e.g. {.val csb_eff})."
    ))
  }
  derived <- paste0(nm, c("_dhis", "_nondhis", "_untreat"))
  collide_derived <- intersect(derived, builtin)
  if (length(collide_derived) > 0) {
    cli::cli_abort(c(
      "Derived custom CSB names collide with built-in indicators: {.val {collide_derived}}",
      "i" = "Pick a different prefix for {.field custom_csb_indicator$name}."
    ))
  }
  collide_other <- intersect(derived, other_indicators)
  if (length(collide_other) > 0) {
    cli::cli_abort(c(
      "Derived custom CSB names collide with user-requested indicators: {.val {collide_other}}",
      "i" = "Pick a different prefix for {.field custom_csb_indicator$name} or remove the conflicting indicators."
    ))
  }

  # label vectors -- character, NA only allowed in untreat_locs
  for (slot in c("dhis_locs", "nondhis_locs", "untreat_locs")) {
    val <- spec[[slot]]
    if (!is.character(val)) {
      cli::cli_abort(
        "{.field custom_csb_indicator${slot}} must be a character vector."
      )
    }
  }
  if (anyNA(spec$dhis_locs)) {
    cli::cli_abort(
      "{.field custom_csb_indicator$dhis_locs} must not contain NA values."
    )
  }
  if (anyNA(spec$nondhis_locs)) {
    cli::cli_abort(
      "{.field custom_csb_indicator$nondhis_locs} must not contain NA values."
    )
  }

  # Normalize and check pairwise disjointness (NAs in untreat are tolerated
  # but ignored here since they cannot conflict with non-NA labels).
  norm_dhis    <- unique(.normalize_custom_csb_label(spec$dhis_locs))
  norm_nondhis <- unique(.normalize_custom_csb_label(spec$nondhis_locs))
  norm_untreat <- unique(.normalize_custom_csb_label(spec$untreat_locs))
  norm_untreat_nona <- norm_untreat[!is.na(norm_untreat)]

  pairs <- list(
    list("dhis_locs", "nondhis_locs", intersect(norm_dhis, norm_nondhis)),
    list("dhis_locs", "untreat_locs", intersect(norm_dhis, norm_untreat_nona)),
    list("nondhis_locs", "untreat_locs",
         intersect(norm_nondhis, norm_untreat_nona))
  )
  overlaps <- Filter(function(p) length(p[[3]]) > 0, pairs)
  if (length(overlaps) > 0) {
    msgs <- vapply(overlaps, function(p) {
      sprintf("%s & %s: %s", p[[1]], p[[2]],
              paste(p[[3]], collapse = ", "))
    }, character(1))
    cli::cli_abort(c(
      "{.field custom_csb_indicator} label lists overlap (must be disjoint after normalization):",
      stats::setNames(msgs, rep("x", length(msgs)))
    ))
  }

  attr(spec, "label_norm_dhis")    <- norm_dhis
  attr(spec, "label_norm_nondhis") <- norm_nondhis
  attr(spec, "label_norm_untreat") <- norm_untreat
  invisible(spec)
}


#' Extract usable h32 variable labels from a DHS KR dataset
#'
#' Reads haven labels from each `^h32[a-z0-9]+$` variable in `dhs_kr`,
#' skipping `h32y`/`h32z` (no-treatment / medical-treatment flags) and any
#' slot whose label is `NA - ...` (DHS country-specific placeholder for
#' unused slots). Labels must be read **before** any zap/coercion.
#'
#' @param dhs_kr DHS Children's Recode with haven labels intact.
#' @return Tibble with columns `variable`, `raw_label`, `label_norm`. Rows
#'   are returned only for slots that have a usable scalar character label.
#' @noRd
.extract_custom_csb_h32_labels <- function(dhs_kr) {
  if (!is.data.frame(dhs_kr)) {
    cli::cli_abort("`dhs_kr` must be a data.frame or tibble.")
  }
  available_h32 <- grep("^h32[a-z0-9]+$", names(dhs_kr), value = TRUE)
  meta_vars <- c("h32y", "h32z")
  available_h32 <- setdiff(available_h32, meta_vars)
  if (length(available_h32) == 0) {
    return(tibble::tibble(
      variable = character(0),
      raw_label = character(0),
      label_norm = character(0)
    ))
  }

  rows <- list()
  for (v in available_h32) {
    lbl <- attr(dhs_kr[[v]], "label")
    if (is.null(lbl) || !is.character(lbl) || length(lbl) != 1L) next
    if (is.na(lbl) || !nzchar(lbl)) next
    if (grepl("^NA\\s*-", lbl)) next
    rows[[length(rows) + 1L]] <- tibble::tibble(
      variable = v,
      raw_label = lbl,
      label_norm = .normalize_custom_csb_label(lbl)
    )
  }

  if (length(rows) == 0) {
    return(tibble::tibble(
      variable = character(0),
      raw_label = character(0),
      label_norm = character(0)
    ))
  }
  dplyr::bind_rows(rows)
}


#' Build a slot-to-bucket lookup for a custom CSB spec against one survey
#'
#' Maps each usable `h32*` variable in `dhs_kr` to one of `dhis`, `nondhis`,
#' or `untreat` based on the user-supplied label lists. Validates that every
#' observed label is mapped exactly once. User-supplied labels that are not
#' present in the current survey are tolerated (the spec is treated as a
#' superset across surveys).
#'
#' @param dhs_kr DHS Children's Recode with haven labels intact.
#' @param custom_csb_indicator Validated user spec.
#' @return Tibble with columns `variable`, `csb_custom` (one of "dhis",
#'   "nondhis", "untreat"), `raw_label`, `label_norm`.
#' @noRd
.build_custom_csb_classification <- function(dhs_kr, custom_csb_indicator) {
  spec <- .validate_custom_csb_indicator_spec(custom_csb_indicator)

  norm_dhis    <- attr(spec, "label_norm_dhis")
  norm_nondhis <- attr(spec, "label_norm_nondhis")
  norm_untreat <- attr(spec, "label_norm_untreat")
  norm_untreat_nona <- norm_untreat[!is.na(norm_untreat)]

  observed <- .extract_custom_csb_h32_labels(dhs_kr)
  if (nrow(observed) == 0) {
    cli::cli_warn(
      "No usable h32 labels found in {.arg dhs_kr}; custom CSB indicator will be empty."
    )
    return(tibble::tibble(
      variable = character(0),
      csb_custom = character(0),
      raw_label = character(0),
      label_norm = character(0)
    ))
  }

  observed$csb_custom <- dplyr::case_when(
    observed$label_norm %in% norm_dhis ~ "dhis",
    observed$label_norm %in% norm_nondhis ~ "nondhis",
    observed$label_norm %in% norm_untreat_nona ~ "untreat",
    TRUE ~ NA_character_
  )

  unmapped <- observed[is.na(observed$csb_custom), , drop = FALSE]
  if (nrow(unmapped) > 0) {
    msgs <- sprintf(
      "%s: %s",
      unmapped$variable,
      unmapped$raw_label
    )
    cli::cli_abort(c(
      "{.arg custom_csb_indicator} does not classify {nrow(unmapped)} observed h32 label{?s}:",
      stats::setNames(msgs, rep("x", length(msgs))),
      "i" = "Add each label to one of {.field dhis_locs}, {.field nondhis_locs}, or {.field untreat_locs}."
    ))
  }

  # Informational: list extra user labels not used by this survey
  used_norm <- observed$label_norm
  extra_dhis    <- setdiff(norm_dhis, used_norm)
  extra_nondhis <- setdiff(norm_nondhis, used_norm)
  extra_untreat <- setdiff(norm_untreat_nona, used_norm)
  n_extra <- length(extra_dhis) + length(extra_nondhis) + length(extra_untreat)
  if (n_extra > 0) {
    cli::cli_alert_info(
      "Custom CSB: {n_extra} user-supplied label{?s} not present in this survey (ignored)."
    )
  }

  observed[, c("variable", "csb_custom", "raw_label", "label_norm")]
}


#' Classify febrile-U5 children into a custom CSB partition
#'
#' Adds three mutually exclusive 0/1 columns to `data` named
#' `<prefix>_dhis`, `<prefix>_nondhis`, `<prefix>_untreat`. Children with
#' no positive `h32*` slot are classified as `untreat`. When a child reports
#' positive sources spanning multiple buckets, priority is
#' `dhis > nondhis > untreat` so the cluster numerators sum to the
#' denominator by construction.
#'
#' @param data Data frame already filtered to the target population.
#' @param h32_cols Character vector of h32 column names present in `data`.
#' @param classification Tibble from `.build_custom_csb_classification()`
#'   with columns `variable`, `csb_custom`.
#' @param prefix User-supplied indicator prefix (`custom_csb_indicator$name`).
#' @return The input data with three new 0/1 columns.
#' @noRd
.classify_custom_csb_from_h32 <- function(
  data,
  h32_cols,
  classification,
  prefix
) {
  col_dhis    <- paste0(prefix, "_dhis")
  col_nondhis <- paste0(prefix, "_nondhis")
  col_untreat <- paste0(prefix, "_untreat")

  if (!is.data.frame(data) || nrow(data) == 0) {
    data[[col_dhis]] <- integer(0)
    data[[col_nondhis]] <- integer(0)
    data[[col_untreat]] <- integer(0)
    return(data)
  }

  # Restrict h32 columns to those present in BOTH the data and the
  # classification (a column may be missing from classification if its
  # label was empty / "NA - ..." / not detectable; treat it as absent).
  h32_cols <- intersect(intersect(h32_cols, names(data)),
                        classification$variable)

  if (length(h32_cols) == 0) {
    # No usable slots -> everyone is untreat
    data[[col_dhis]] <- 0L
    data[[col_nondhis]] <- 0L
    data[[col_untreat]] <- 1L
    return(data)
  }

  # Coerce h32 columns to numeric (defensive; .prepare_csb_data already
  # zaps labels but this function may be called directly in tests).
  for (col in h32_cols) {
    data[[col]] <- suppressWarnings(as.numeric(as.character(data[[col]])))
  }

  data$.csb_custom_row_id <- dplyr::row_number(data)

  long <- data |>
    dplyr::select(.csb_custom_row_id, dplyr::all_of(h32_cols)) |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(h32_cols),
      names_to = "variable",
      values_to = "visited"
    ) |>
    dplyr::filter(!is.na(visited) & visited == 1) |>
    dplyr::left_join(
      classification[, c("variable", "csb_custom")],
      by = "variable"
    )

  per_child <- if (nrow(long) > 0) {
    long |>
      dplyr::group_by(.csb_custom_row_id) |>
      dplyr::summarise(
        had_dhis    = as.integer(any(csb_custom == "dhis")),
        had_nondhis = as.integer(any(csb_custom == "nondhis")),
        had_untreat = as.integer(any(csb_custom == "untreat")),
        .groups = "drop"
      )
  } else {
    tibble::tibble(
      .csb_custom_row_id = integer(0),
      had_dhis = integer(0),
      had_nondhis = integer(0),
      had_untreat = integer(0)
    )
  }

  data <- data |>
    dplyr::left_join(per_child, by = ".csb_custom_row_id") |>
    dplyr::mutate(
      had_dhis    = tidyr::replace_na(had_dhis, 0L),
      had_nondhis = tidyr::replace_na(had_nondhis, 0L),
      had_untreat = tidyr::replace_na(had_untreat, 0L)
    )

  # Mutually exclusive assignment with priority dhis > nondhis > untreat.
  # Children with no positive slot fall into _untreat by construction.
  data[[col_dhis]] <- as.integer(data$had_dhis == 1L)
  data[[col_nondhis]] <- as.integer(
    data$had_dhis == 0L & data$had_nondhis == 1L
  )
  data[[col_untreat]] <- as.integer(
    data$had_dhis == 0L & data$had_nondhis == 0L
  )

  data$.csb_custom_row_id <- NULL
  data$had_dhis <- NULL
  data$had_nondhis <- NULL
  data$had_untreat <- NULL
  data
}
