#' Prepare PfPR Data for MBG Analysis
#'
#' Prepares cluster-level malaria parasite prevalence data for Model-Based
#' Geostatistics (MBG) analysis. Aggregates individual test results to cluster
#' counts WITHOUT survey weights - MBG handles spatial smoothing internally.
#'
#' @param dhs_pr DHS Person Records dataset (data.frame or tibble).
#' @param gps_data DHS GPS dataset with cluster coordinates.
#' @param test_type Character. Type of test: "rdt", "mic", "both" (default),
#'   or "either". When "both", produces separate RDT, microscopy, and either
#'   tables plus a combined table per age group. When "either", produces a
#'   single table per age group where positive means positive on either test.
#' @param age_groups Named list of age ranges (in months) to calculate. Each
#'   element should be a length-2 vector c(min, max). Default includes:
#'   \itemize{
#'     \item u5: c(6, 59) - Under 5 years
#'     \item 5_10: c(60, 120) - 5-10 years
#'     \item u10: c(6, 119) - Under 10 years
#'     \item 2_10: c(24, 119) - 2-10 years (standard PfPR reference)
#'   }
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
#' @return A list of data.tables (one per age group + test type combination),
#'   each with columns:
#'   \itemize{
#'     \item cluster_id: Cluster identifier
#'     \item indicator: Number of positive tests (numerator for MBG)
#'     \item samplesize: Number of children tested (denominator for MBG)
#'     \item x: Longitude
#'     \item y: Latitude
#'   }
#'   When \code{test_type = "both"}, additional \code{pfpr_combined_*} tables
#'   are included with columns: cluster_id, latitude, longitude, n_tested,
#'   n_positive_mic, prop_raw_mic, n_positive_rdt, prop_raw_rdt,
#'   n_positive_either, prop_raw_either.
#'   Use [save_mbg_cluster_data()] to save with additional columns (n_positive,
#'   n_tested, prop_raw) or [plot_mbg_clusters()] to visualize.
#'
#' @details
#' This function prepares data for MBG spatial modeling. Unlike the survey-
#' weighted `calc_pfpr_dhs()` function, this uses simple cluster-level counts
#' because MBG handles spatial smoothing and uncertainty internally.
#'
#' The output format matches the expected input for `mbg::MbgModelRunner`:
#' `data.table(cluster_id, indicator, samplesize, x, y)`
#'
#' @examples
#' \dontrun{
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
  test_type = "both",
  age_groups = list(
    u5 = c(6, 59),
    `5_10` = c(60, 120),
    u10 = c(6, 119),
    `2_10` = c(24, 119)
  ),
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
  # Check for required spatial packages
  .check_spatial_pkg("mbg", "calc_pfpr_mbg")

  # ---- Input validation ----

  if (!is.data.frame(dhs_pr)) {
    cli::cli_abort("`dhs_pr` must be a data.frame or tibble")
  }

  if (!is.data.frame(gps_data)) {
    cli::cli_abort("`gps_data` must be a data.frame or tibble")
  }

  if (nrow(dhs_pr) == 0) {
    cli::cli_abort("`dhs_pr` is empty")
  }

  if (nrow(gps_data) == 0) {
    cli::cli_abort("`gps_data` is empty")
  }

  test_type <- match.arg(test_type, c("rdt", "mic", "both", "either"))

  # Check required columns
  required_cols <- c(
    survey_vars$cluster,
    survey_vars$age,
    survey_vars$present,
    survey_vars$mother
  )

  if (test_type %in% c("rdt", "both", "either")) {
    required_cols <- c(required_cols, survey_vars$rdt)
  }

  if (test_type %in% c("mic", "both", "either")) {
    required_cols <- c(required_cols, survey_vars$mic)
  }

  missing_cols <- setdiff(required_cols, names(dhs_pr))

  if (length(missing_cols) > 0) {
    cli::cli_abort("Required columns not found: {.var {missing_cols}}")
  }

  # Check GPS columns
  gps_cols <- c(gps_vars$cluster, gps_vars$lat, gps_vars$lon)
  missing_gps <- setdiff(gps_cols, names(gps_data))

  if (length(missing_gps) > 0) {
    cli::cli_abort("GPS columns not found: {.var {missing_gps}}")
  }

  # ---- Prepare GPS data ----

  gps_clean <- gps_data |>
    dplyr::transmute(
      cluster_id = .data[[gps_vars$cluster]],
      x = as.numeric(.data[[gps_vars$lon]]),
      y = as.numeric(.data[[gps_vars$lat]])
    ) |>
    dplyr::filter(
      !is.na(x),
      !is.na(y),
      x != 0,
      y != 0
    ) |>
    dplyr::distinct()

  cli::cli_alert_info(
    "GPS data: {nrow(gps_clean)} clusters with valid coordinates"
  )

  # ---- Prepare PR data ----

  pr <- dhs_pr |>
    dplyr::mutate(
      dplyr::across(dplyr::everything(), haven::zap_labels)
    ) |>
    dplyr::mutate(
      dplyr::across(dplyr::everything(), as.vector)
    ) |>
    dplyr::transmute(
      cluster_id = .data[[survey_vars$cluster]],
      age = .data[[survey_vars$age]],
      present = .data[[survey_vars$present]],
      mother = .data[[survey_vars$mother]],
      rdt_res = if (survey_vars$rdt %in% names(dhs_pr)) {
        .data[[survey_vars$rdt]]
      } else {
        NA_real_
      },
      mic_res = if (survey_vars$mic %in% names(dhs_pr)) {
        .data[[survey_vars$mic]]
      } else {
        NA_real_
      }
    )

  # ---- Process each age group and test type ----

  results <- list()

  for (age_name in names(age_groups)) {
    age_range <- age_groups[[age_name]]
    age_min <- age_range[1]
    age_max <- age_range[2]

    # Filter to eligible children for this age group
    pr_age <- pr |>
      dplyr::filter(
        present == 1,
        mother == 1,
        age >= age_min,
        age <= age_max
      )

    if (nrow(pr_age) == 0) {
      cli::cli_alert_warning(
        "No eligible children for age group {age_name} ({age_min}-{age_max} months)"
      )
      next
    }

    cli::cli_alert_info(
      "Age group {age_name}: {format(nrow(pr_age), big.mark = ',')} eligible children"
    )

    # RDT results
    if (test_type %in% c("rdt", "both")) {
      pr_rdt <- pr_age |>
        dplyr::filter(rdt_res %in% c(0, 1)) |>
        dplyr::mutate(
          positive = as.integer(rdt_res == 1)
        )

      if (nrow(pr_rdt) > 0) {
        result_name <- paste0("pfpr_rdt_", age_name)

        rdt_cluster <- pr_rdt |>
          dplyr::group_by(cluster_id) |>
          dplyr::summarise(
            indicator = sum(positive, na.rm = TRUE),
            samplesize = dplyr::n(),
            .groups = "drop"
          ) |>
          dplyr::inner_join(gps_clean, by = "cluster_id") |>
          dplyr::filter(samplesize > 0)

        if (nrow(rdt_cluster) > 0) {
          results[[result_name]] <- data.table::as.data.table(rdt_cluster)

          cli::cli_alert_success(
            "{result_name}: {nrow(rdt_cluster)} clusters, ",
            "{sum(rdt_cluster$indicator)} positive / {sum(rdt_cluster$samplesize)} tested"
          )
        }
      }
    }

    # Microscopy results
    if (test_type %in% c("mic", "both")) {
      # mic_res: 0=neg, 1=Pf positive, 6=other species
      pr_mic <- pr_age |>
        dplyr::filter(mic_res %in% c(0, 1, 6)) |>
        dplyr::mutate(
          positive = as.integer(mic_res == 1)  # Only Pf counts as positive
        )

      if (nrow(pr_mic) > 0) {
        result_name <- paste0("pfpr_mic_", age_name)

        mic_cluster <- pr_mic |>
          dplyr::group_by(cluster_id) |>
          dplyr::summarise(
            indicator = sum(positive, na.rm = TRUE),
            samplesize = dplyr::n(),
            .groups = "drop"
          ) |>
          dplyr::inner_join(gps_clean, by = "cluster_id") |>
          dplyr::filter(samplesize > 0)

        if (nrow(mic_cluster) > 0) {
          results[[result_name]] <- data.table::as.data.table(mic_cluster)

          cli::cli_alert_success(
            "{result_name}: {nrow(mic_cluster)} clusters, ",
            "{sum(mic_cluster$indicator)} positive / {sum(mic_cluster$samplesize)} tested"
          )
        }
      }
    }

    # Either test results (positive on either RDT or microscopy)
    if (test_type %in% c("both", "either")) {
      pr_either <- pr_age |>
        dplyr::filter(rdt_res %in% c(0, 1) | mic_res %in% c(0, 1, 6)) |>
        dplyr::mutate(
          positive = as.integer(
            dplyr::coalesce(rdt_res == 1L, FALSE) |
              dplyr::coalesce(mic_res == 1L, FALSE)
          )
        )

      if (nrow(pr_either) > 0) {
        result_name <- paste0("pfpr_either_", age_name)

        either_cluster <- pr_either |>
          dplyr::group_by(cluster_id) |>
          dplyr::summarise(
            indicator = sum(positive, na.rm = TRUE),
            samplesize = dplyr::n(),
            .groups = "drop"
          ) |>
          dplyr::inner_join(gps_clean, by = "cluster_id") |>
          dplyr::filter(samplesize > 0)

        if (nrow(either_cluster) > 0) {
          results[[result_name]] <- data.table::as.data.table(either_cluster)

          cli::cli_alert_success(
            "{result_name}: {nrow(either_cluster)} clusters, ",
            "{sum(either_cluster$indicator)} positive / {sum(either_cluster$samplesize)} tested"
          )
        }
      }
    }

    # Combined table (all test types joined at cluster level)
    if (test_type == "both") {
      rdt_name <- paste0("pfpr_rdt_", age_name)
      mic_name <- paste0("pfpr_mic_", age_name)
      either_name <- paste0("pfpr_either_", age_name)

      if (either_name %in% names(results)) {
        combined_name <- paste0("pfpr_combined_", age_name)

        # Start from either result (correct n_tested denominator)
        combined <- data.table::copy(results[[either_name]])
        data.table::setnames(
          combined,
          c("indicator", "samplesize"),
          c("n_positive_either", "n_tested")
        )
        combined[, prop_raw_either := n_positive_either / n_tested]

        # Rename coordinates to readable names
        data.table::setnames(combined, c("x", "y"), c("longitude", "latitude"))

        # Left join RDT counts
        if (rdt_name %in% names(results)) {
          rdt_dt <- results[[rdt_name]][
            , .(cluster_id, n_positive_rdt = indicator)
          ]
          combined <- merge(combined, rdt_dt, by = "cluster_id", all.x = TRUE)
          combined[is.na(n_positive_rdt), n_positive_rdt := 0L]
          combined[, prop_raw_rdt := n_positive_rdt / n_tested]
        }

        # Left join microscopy counts
        if (mic_name %in% names(results)) {
          mic_dt <- results[[mic_name]][
            , .(cluster_id, n_positive_mic = indicator)
          ]
          combined <- merge(combined, mic_dt, by = "cluster_id", all.x = TRUE)
          combined[is.na(n_positive_mic), n_positive_mic := 0L]
          combined[, prop_raw_mic := n_positive_mic / n_tested]
        }

        # Reorder columns
        col_order <- intersect(
          c("cluster_id", "latitude", "longitude", "n_tested",
            "n_positive_mic", "prop_raw_mic",
            "n_positive_rdt", "prop_raw_rdt",
            "n_positive_either", "prop_raw_either"),
          names(combined)
        )
        data.table::setcolorder(combined, col_order)

        results[[combined_name]] <- combined

        cli::cli_alert_success(
          "{combined_name}: {nrow(combined)} clusters with combined test data"
        )
      }
    }
  }

  # Filter out redundant age groups (e.g., u10 identical to u5 when no 5-10 data)
  results <- .filter_redundant_mbg_results(results, age_groups)

  if (length(results) == 0) {
    cli::cli_abort("No valid MBG data could be prepared from the input data")
  }

  results
}


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
  rdt_results <- result_names[grepl("_rdt_", result_names)]
  mic_results <- result_names[grepl("_mic_", result_names)]
  either_results <- result_names[grepl("_either_", result_names)]

  # Check for redundancy within each test type (skip combined — handled below)
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
          age_i <- .extract_age_group_from_name(name_i)
          age_j <- .extract_age_group_from_name(name_j)

          range_i <- age_groups[[age_i]]
          range_j <- age_groups[[age_j]]

          # Handle case where age group not found in age_groups
          if (is.null(range_i) || is.null(range_j)) next

          span_i <- range_i[2] - range_i[1]
          span_j <- range_j[2] - range_j[1]

          if (span_i <= span_j) {
            # Keep i (narrower), remove j
            to_remove <- c(to_remove, name_j)
            cli::cli_alert_warning(
              "Age group '{age_j}' skipped - identical to '{age_i}' ",
              "(no data in {range_j[1]}-{range_j[2]} month range outside {range_i[1]}-{range_i[2]})"
            )
          } else {
            # Keep j (narrower), remove i
            to_remove <- c(to_remove, name_i)
            cli::cli_alert_warning(
              "Age group '{age_i}' skipped - identical to '{age_j}' ",
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

  # Remove orphaned combined tables (whose either counterpart was removed)
  combined_names <- names(results)[grepl("^pfpr_combined_", names(results))]
  for (cn in combined_names) {
    corresponding_either <- sub("^pfpr_combined_", "pfpr_either_", cn)
    if (!corresponding_either %in% names(results)) {
      results[[cn]] <- NULL
    }
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


#' Prepare Single PfPR Indicator for MBG
#'
#' Simplified function to prepare a single PfPR indicator for MBG. Returns
#' a single data.table rather than a list.
#'
#' @inheritParams calc_pfpr_mbg
#' @param age_min Minimum age in months (inclusive). Default: 6.
#' @param age_max Maximum age in months (inclusive). Default: 59.
#'
#' @return A data.table with columns: cluster_id, indicator, samplesize, x, y
#'
#' @export
prep_pfpr_mbg <- function(
  dhs_pr,
  gps_data,
  test_type = "rdt",
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
  test_type <- match.arg(test_type, c("rdt", "mic"))

  age_label <- paste0(age_min, "_", age_max)

  result <- calc_pfpr_mbg(
    dhs_pr = dhs_pr,
    gps_data = gps_data,
    test_type = test_type,
    age_groups = stats::setNames(list(c(age_min, age_max)), age_label),
    survey_vars = survey_vars,
    gps_vars = gps_vars
  )

  # Return the single result
  result[[1]]
}
