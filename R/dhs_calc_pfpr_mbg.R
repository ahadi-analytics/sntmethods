#' Prepare PfPR Data for MBG Analysis
#'
#' Prepares cluster-level malaria parasite prevalence data for Model-Based
#' Geostatistics (MBG) analysis. Uses a dictionary-driven approach matching
#' the indicator codes from \code{\link{calc_pfpr_dhs}}.
#'
#' @details
#' All dictionary-based indicators share the same data preparation pipeline
#' via \code{.prepare_pfpr_data()}, the same shared helper used by the
#' survey-weighted \code{calc_pfpr_dhs()} function. Positivity definitions
#' are identical:
#' \itemize{
#'   \item RDT positive: \code{rdt_res == 1} (hml35 == 1)
#'   \item Microscopy positive: \code{mic_res == 1} (hml32 == 1, Pf only)
#'   \item Either: positive on RDT OR microscopy
#' }
#'
#' Unlike the survey-weighted function, this uses simple cluster-level counts
#' because MBG handles spatial smoothing and uncertainty internally.
#'
#' Methodology: \url{https://github.com/ahadi-analytics/sntmethods/blob/master/inst/methods/pfpr_dhs.yml}
#'
#' @param dhs_pr DHS Person Records dataset (data.frame or tibble).
#' @param gps_data DHS GPS dataset with cluster coordinates.
#' @param indicators Character vector of indicator codes to calculate.
#'   Available codes: \code{"pfpr_rdt"}, \code{"pfpr_mic"} (6-59 months),
#'   \code{"pfpr_rdt_u5"}, \code{"pfpr_mic_u5"} (0-59 months).
#'   Default: all indicators from the dictionary.
#' @param test_type \strong{Deprecated}. Character. Use \code{indicators}
#'   instead. When provided, translated to indicator codes for backward
#'   compatibility. One of \code{"rdt"}, \code{"mic"}, \code{"both"},
#'   or \code{"either"}.
#' @param age_groups \strong{Deprecated}. Named list of age ranges. Use
#'   \code{indicators} instead. When provided alongside \code{test_type},
#'   translated to indicator codes.
#' @param survey_vars Named list mapping DHS variable names. Required keys:
#'   \itemize{
#'     \item cluster: Cluster ID (default: "hv001")
#'     \item age: Age in months (default: "hc1")
#'     \item present: Present in household (default: "hv103")
#'     \item mother: Mother listed in household (default: "hv042")
#'     \item rdt: RDT result variable (default: "hml35")
#'     \item mic: Microscopy result variable (default: "hml32")
#'   }
#' @param gps_vars Named list for GPS variable mapping.
#'
#' @return A named list of data.tables (one per indicator), each with columns:
#'   \itemize{
#'     \item cluster_id: Cluster identifier
#'     \item indicator: Number of positive tests (numerator for MBG)
#'     \item samplesize: Number of children tested (denominator for MBG)
#'     \item x: Longitude
#'     \item y: Latitude
#'   }
#'
#' @examples
#' \dontrun{
#' # New-style: specify exact indicator codes
#' pfpr_mbg <- calc_pfpr_mbg(
#'   dhs_pr = pr_data,
#'   gps_data = gps_data,
#'   indicators = c("pfpr_rdt_u5", "pfpr_mic_u5")
#' )
#'
#' # Legacy style (still works, with deprecation warning)
#' pfpr_mbg <- calc_pfpr_mbg(
#'   dhs_pr = pr_data,
#'   gps_data = gps_data,
#'   test_type = "rdt",
#'   age_groups = list(u5 = c(6, 59))
#' )
#' }
#'
#' @seealso [calc_pfpr_dhs()] for survey-weighted estimates
#' @export
calc_pfpr_mbg <- function(
  dhs_pr,
  gps_data,
  indicators = NULL,
  test_type = NULL,
  age_groups = NULL,
  survey_vars = list(
    cluster = "hv001",
    age = "hc1",
    present = "hv103",
    mother = "hv042",
    rdt = "hml35",
    mic = "hml32"
  ),
  gps_vars = list(
    cluster = "DHSCLUST",
    lat = "LATNUM",
    lon = "LONGNUM"
  )
) {
  # ---- Input validation ----

  if (!is.data.frame(dhs_pr)) {
    cli::cli_abort("`dhs_pr` must be a data.frame or tibble")
  }
  if (!is.data.frame(gps_data)) {
    cli::cli_abort("`gps_data` must be a data.frame or tibble")
  }

  # ---- Resolve indicators ----

  dict <- .pfpr_mbg_dictionary()
  dict_names <- vapply(dict, `[[`, character(1), "name")

  if (!is.null(test_type) || !is.null(age_groups)) {
    # Legacy backward-compat: translate test_type + age_groups to codes
    cli::cli_alert_warning(
      "{.arg test_type}/{.arg age_groups} are deprecated; use {.arg indicators} with specific codes instead"
    )
    indicators <- .pfpr_legacy_to_codes(test_type, age_groups)
  }

  if (is.null(indicators)) {
    # Default: all dictionary indicators
    indicators <- dict_names
  }

  invalid <- setdiff(indicators, dict_names)
  if (length(invalid) > 0) {
    cli::cli_abort(
      "Invalid indicators: {.val {invalid}}. Valid codes: {.val {dict_names}}"
    )
  }

  # ---- Prepare GPS data ----

  gps_clean <- .prepare_gps_data(gps_data, gps_vars)

  # ---- Prepare PR data ----
  # Use very wide age range; age filtering happens per indicator in the loop

  pr <- .prepare_pfpr_data(
    dhs_pr, survey_vars,
    age_min = 0, age_max = 999,
    include_survey_vars = FALSE
  )

  if (is.null(pr)) return(NULL)

  # ---- Dictionary-driven indicator loop ----

  # Filter dictionary to requested indicators
  dict_specs <- dict[vapply(dict, function(d) d$name %in% indicators, logical(1))]

  results <- list()

  for (spec in dict_specs) {
    # Age filter + eligibility (present, mother)
    age_data <- pr[
      pr$present == 1 &
      pr$mother == 1 &
      pr$age >= spec$age_min &
      pr$age <= spec$age_max, ,
      drop = FALSE
    ]

    if (nrow(age_data) == 0) {
      cli::cli_alert_warning(
        "{spec$name}: no eligible children in {spec$age_min}-{spec$age_max} month range"
      )
      next
    }

    # Test-specific filtering and positivity
    if (spec$test_type == "either") {
      # Either: tested on at least one test (RDT or microscopy)
      age_data <- age_data[
        age_data$rdt_res %in% c(0, 1) | age_data$mic_res %in% c(0, 1, 6), ,
        drop = FALSE
      ]
      if (nrow(age_data) == 0) next
      age_data$positive <- as.integer(
        (!is.na(age_data$rdt_res) & age_data$rdt_res == 1L) |
        (!is.na(age_data$mic_res) & age_data$mic_res == 1L)
      )
    } else {
      # RDT or microscopy: filter to valid test results
      test_col <- spec$test_col
      valid_values <- spec$valid_values
      pos_value <- spec$pos_value

      if (!test_col %in% names(age_data)) next
      age_data <- age_data[
        age_data[[test_col]] %in% valid_values, ,
        drop = FALSE
      ]
      if (nrow(age_data) == 0) {
        cli::cli_alert_warning(
          "{spec$name}: no tested individuals - skipping"
        )
        next
      }
      age_data$positive <- as.integer(age_data[[test_col]] == pos_value)
    }

    dt <- .aggregate_to_mbg_clusters(
      individual_data = age_data,
      indicator_col = "positive",
      gps_clean = gps_clean,
      result_name = spec$name
    )
    if (!is.null(dt)) {
      results[[spec$name]] <- dt
    }
  }

  # ---- Filter redundant age groups ----

  # Build age_groups list from dictionary for redundancy check
  age_groups_from_dict <- stats::setNames(
    lapply(dict_specs, function(d) c(d$age_min, d$age_max)),
    vapply(dict_specs, `[[`, character(1), "name")
  )
  results <- .filter_redundant_mbg_results(results, age_groups_from_dict)

  if (length(results) == 0) {
    cli::cli_warn(
      "No valid PfPR data could be prepared; {.var {survey_vars$rdt}} and {.var {survey_vars$mic}} may be absent from this survey"
    )
    return(NULL)
  }

  results
}


# =============================================================================
# PfPR MBG Indicator Dictionary
# =============================================================================

#' PfPR MBG Indicator Dictionary
#'
#' Returns the full set of standardized indicator specifications for
#' cluster-level MBG output. Generates all test type x age group combinations.
#' Mirrors the DHS \code{.pfpr_conditions()} definitions.
#'
#' @details
#' Test types:
#' \itemize{
#'   \item \code{rdt}: RDT positive (\code{rdt_res == 1}, i.e., hml35 == 1)
#'   \item \code{mic}: Microscopy positive (\code{mic_res == 1}, i.e.,
#'     hml32 == 1, Pf only)
#'   \item \code{either}: Positive on RDT OR microscopy
#' }
#'
#' Age groups:
#' \itemize{
#'   \item \code{u5}: 6-59 months
#'   \item \code{5_10}: 60-120 months
#'   \item \code{u10}: 6-119 months
#'   \item \code{2_10}: 24-119 months (standard PfPR reference range)
#' }
#'
#' @return List of named lists, each with fields: \code{name},
#'   \code{test_type}, \code{test_col}, \code{pos_value},
#'   \code{valid_values}, \code{age_min}, \code{age_max}.
#'
#' @noRd
.pfpr_mbg_dictionary <- function() {
  dict <- list(
    # Standard PfPR (6-59 months, matching DHS pfpr_rdt / pfpr_mic)
    list(name = "pfpr_rdt", test_type = "rdt", test_col = "rdt_res",
         pos_value = 1, valid_values = c(0, 1), age_min = 6, age_max = 59),
    list(name = "pfpr_mic", test_type = "mic", test_col = "mic_res",
         pos_value = 1, valid_values = c(0, 1, 6), age_min = 6, age_max = 59),
    # U5 variants (0-59 months)
    list(name = "pfpr_rdt_u5", test_type = "rdt", test_col = "rdt_res",
         pos_value = 1, valid_values = c(0, 1), age_min = 0, age_max = 59),
    list(name = "pfpr_mic_u5", test_type = "mic", test_col = "mic_res",
         pos_value = 1, valid_values = c(0, 1, 6), age_min = 0, age_max = 59)
  )

  dict
}


# =============================================================================
# Legacy backward-compatibility translator
# =============================================================================

#' Translate Legacy test_type + age_groups to Indicator Codes
#'
#' Converts the old-style \code{test_type} and \code{age_groups} parameters
#' into the new indicator code format used by the dictionary.
#'
#' @param test_type Character: "rdt", "mic", "both", or "either".
#' @param age_groups Named list of age ranges. If NULL, uses default ages.
#'
#' @return Character vector of indicator codes.
#'
#' @noRd
.pfpr_legacy_to_codes <- function(test_type = NULL, age_groups = NULL) {
  test_type <- test_type %||% "both"
  test_type <- match.arg(test_type, c("rdt", "mic", "both", "either"))

  if (is.null(age_groups)) {
    age_groups <- list(
      u5 = c(6, 59),
      `5_10` = c(60, 120),
      u10 = c(6, 119),
      `2_10` = c(24, 119)
    )
  }

  # Map test_type to test prefixes
  test_prefixes <- switch(test_type,
    rdt    = "rdt",
    mic    = "mic",
    either = "either",
    both   = c("rdt", "mic", "either")
  )

  age_names <- names(age_groups)

  codes <- character(0)

  # Include standard (unsuffixed) indicators for rdt/mic when using
  # default age groups that include the 2-10 range (24-119 months)
  has_default_2_10 <- any(vapply(age_groups, function(x) {
    length(x) == 2 && x[1] == 24 && x[2] == 119
  }, logical(1)))

  for (tp in test_prefixes) {
    # Add standard code (e.g., pfpr_rdt, pfpr_mic) when 2-10 range is present
    if (has_default_2_10 && tp %in% c("rdt", "mic")) {
      codes <- c(codes, paste0("pfpr_", tp))
    }
    for (an in age_names) {
      codes <- c(codes, paste0("pfpr_", tp, "_", an))
    }
  }

  codes
}


# =============================================================================
# Redundancy detection helpers
# =============================================================================

#' Filter Redundant MBG Results
#'
#' Detects and removes redundant age groups from MBG results. Two results are
#' considered redundant if they have identical cluster data (same cluster_ids
#' with same indicator and samplesize values). When redundant pairs are found,
#' the more specific age group (narrower age range) is kept.
#'
#' @param results Named list of data.tables from calc_pfpr_mbg
#' @param age_groups Named list of age ranges used to generate results
#'
#' @return Filtered list with redundant results removed
#'
#' @keywords internal
#' @noRd
.filter_redundant_mbg_results <- function(results, age_groups) {
  if (length(results) <= 1) {
    return(results)
  }

  result_names <- names(results)
  to_remove <- character(0)

  # Group results by test type (rdt, mic, or either)
  # Match both suffixed (pfpr_rdt_u5) and unsuffixed (pfpr_rdt) variants
  rdt_results <- result_names[grepl("_rdt(_|$)", result_names)]
  mic_results <- result_names[grepl("_mic(_|$)", result_names)]
  either_results <- result_names[grepl("_either(_|$)", result_names)]

  # Check for redundancy within each test type
  for (test_results in list(rdt_results, mic_results, either_results)) {
    if (length(test_results) <= 1) next

    # Pairwise comparison
    for (i in seq_len(length(test_results) - 1)) {
      for (j in (i + 1):length(test_results)) {
        name_i <- test_results[i]
        name_j <- test_results[j]

        # Skip if already marked for removal
        if (name_i %in% to_remove || name_j %in% to_remove) next

        dt_i <- results[[name_i]]
        dt_j <- results[[name_j]]

        # Check if identical
        if (.are_mbg_results_identical(dt_i, dt_j)) {
          # Determine which to keep (narrower age range)
          range_i <- age_groups[[name_i]]
          range_j <- age_groups[[name_j]]

          # Handle case where age group not found in age_groups
          if (is.null(range_i) || is.null(range_j)) next

          span_i <- range_i[2] - range_i[1]
          span_j <- range_j[2] - range_j[1]

          if (span_i <= span_j) {
            # Keep i (narrower), remove j
            to_remove <- c(to_remove, name_j)
            cli::cli_alert_warning(
              "'{name_j}' skipped - identical to '{name_i}' ",
              "(no data in {range_j[1]}-{range_j[2]} month range outside {range_i[1]}-{range_i[2]})"
            )
          } else {
            # Keep j (narrower), remove i
            to_remove <- c(to_remove, name_i)
            cli::cli_alert_warning(
              "'{name_i}' skipped - identical to '{name_j}' ",
              "(no data in {range_i[1]}-{range_i[2]} month range outside {range_j[1]}-{range_j[2]})"
            )
          }
        }
      }
    }
  }

  # Remove redundant results
  if (length(to_remove) > 0) {
    results <- results[!names(results) %in% to_remove]
  }

  results
}


#' Check if Two MBG Results are Practically Identical
#'
#' Compares two data.tables to determine if they have essentially the same
#' cluster data. Uses tolerance-based comparison to catch cases where small
#' differences exist but the results are practically redundant.
#'
#' @param dt1 First data.table
#' @param dt2 Second data.table
#' @param tol Tolerance for considering results identical. Default 0.99 means
#'   results are considered identical if correlation > 0.99 and total counts
#'   differ by less than 1%.
#'
#' @return TRUE if results are practically identical, FALSE otherwise
#'
#' @keywords internal
#' @noRd
.are_mbg_results_identical <- function(dt1, dt2, tol = 0.99) {
  # Must have same or nearly same number of rows (within 5%)
  n1 <- nrow(dt1)
  n2 <- nrow(dt2)
  if (abs(n1 - n2) / max(n1, n2) > 0.05) {
    return(FALSE)
  }

  # Find common clusters
  common_clusters <- intersect(dt1$cluster_id, dt2$cluster_id)

  # Must have substantial overlap (at least 95% of smaller set)
  min_clusters <- min(n1, n2)
  if (length(common_clusters) / min_clusters < 0.95) {
    return(FALSE)
  }

  # Compare on common clusters
  dt1_common <- dt1[dt1$cluster_id %in% common_clusters, ]
  dt2_common <- dt2[dt2$cluster_id %in% common_clusters, ]

  # Sort by cluster_id
  dt1_sorted <- dt1_common[order(dt1_common$cluster_id), ]
  dt2_sorted <- dt2_common[order(dt2_common$cluster_id), ]

  # Check if total samplesize is nearly identical (within 1%)
  total_ss1 <- sum(dt1_sorted$samplesize)
  total_ss2 <- sum(dt2_sorted$samplesize)
  ss_diff <- abs(total_ss1 - total_ss2) / max(total_ss1, total_ss2)

  if (ss_diff > 0.01) {
    return(FALSE)
  }

  # Check if total indicator is nearly identical (within 1%)
  total_ind1 <- sum(dt1_sorted$indicator)
  total_ind2 <- sum(dt2_sorted$indicator)

  # Handle case where both are zero
  if (total_ind1 == 0 && total_ind2 == 0) {
    return(TRUE)
  }

  ind_diff <- abs(total_ind1 - total_ind2) / max(total_ind1, total_ind2, 1)

  if (ind_diff > 0.01) {
    return(FALSE)
  }

  # Check correlation of proportions (if enough variation)
  prop1 <- dt1_sorted$indicator / dt1_sorted$samplesize
  prop2 <- dt2_sorted$indicator / dt2_sorted$samplesize

  # If no variation in either, they're identical if means are close
  if (stats::sd(prop1) < 0.001 || stats::sd(prop2) < 0.001) {
    return(abs(mean(prop1) - mean(prop2)) < 0.01)
  }

  # High correlation indicates redundancy
  correlation <- stats::cor(prop1, prop2, use = "complete.obs")

  !is.na(correlation) && correlation > tol
}


#' Extract Age Group Name from Result Name
#'
#' Extracts the age group portion from a result name like "pfpr_rdt_u5".
#'
#' @param result_name Result name string
#'
#' @return Age group name (e.g., "u5", "2_10")
#'
#' @keywords internal
#' @noRd
.extract_age_group_from_name <- function(result_name) {
  # Pattern: pfpr_{test}_{age_group}
  # Remove "pfpr_rdt_", "pfpr_mic_", "pfpr_either_", or "pfpr_combined_" prefix
  sub("^pfpr_(rdt|mic|either|combined)_", "", result_name)
}


# =============================================================================
# Convenience wrapper
# =============================================================================

#' Prepare Single PfPR Indicator for MBG
#'
#' Simplified function to prepare a single PfPR indicator for MBG. Returns
#' a single data.table rather than a list.
#'
#' @inheritParams calc_pfpr_mbg
#' @param indicator Single indicator code (e.g., \code{"pfpr_rdt_u5"}).
#'   Also accepts legacy \code{test_type} values (\code{"rdt"}, \code{"mic"})
#'   combined with \code{age_min}/\code{age_max}.
#' @param age_min Minimum age in months (inclusive). Default: 6.
#'   Only used when \code{indicator} is a legacy test_type name.
#' @param age_max Maximum age in months (inclusive). Default: 59.
#'   Only used when \code{indicator} is a legacy test_type name.
#'
#' @return A tibble with columns: cluster_id, indicator, samplesize, x, y
#'
#' @export
prep_pfpr_mbg <- function(
  dhs_pr,
  gps_data,
  indicator = "pfpr_rdt_u5",
  age_min = 6,
  age_max = 59,
  survey_vars = list(
    cluster = "hv001",
    age = "hc1",
    present = "hv103",
    mother = "hv042",
    rdt = "hml35",
    mic = "hml32"
  ),
  gps_vars = list(
    cluster = "DHSCLUST",
    lat = "LATNUM",
    lon = "LONGNUM"
  )
) {
  # Check if indicator is a dictionary code or a legacy test_type name
  dict <- .pfpr_mbg_dictionary()
  dict_names <- vapply(dict, `[[`, character(1), "name")

  if (indicator %in% dict_names) {
    # New-style: indicator is already a valid code
    result <- calc_pfpr_mbg(
      dhs_pr = dhs_pr,
      gps_data = gps_data,
      indicators = indicator,
      survey_vars = survey_vars,
      gps_vars = gps_vars
    )
  } else if (indicator %in% c("rdt", "mic")) {
    # Legacy style: translate test_type + age range to indicator code
    age_label <- paste0(age_min, "_", age_max)
    result <- calc_pfpr_mbg(
      dhs_pr = dhs_pr,
      gps_data = gps_data,
      test_type = indicator,
      age_groups = stats::setNames(list(c(age_min, age_max)), age_label),
      survey_vars = survey_vars,
      gps_vars = gps_vars
    )
  } else {
    cli::cli_abort(
      "Invalid indicator: {.val {indicator}}. Use a dictionary code (e.g., {.val pfpr_rdt_u5}) or legacy name ({.val rdt}, {.val mic})"
    )
  }

  if (is.null(result) || length(result) == 0) {
    cli::cli_abort("No data returned for indicator {.val {indicator}}")
  }

  # Return the single result
  result[[1]]
}
