# =============================================================================
# Shared helpers for wealth-stratified DHS indicators
# =============================================================================

#' Prepare wealth quintile variable for stratification
#'
#' Standardizes wealth quintile extraction from DHS datasets. Handles both
#' household (hv270) and individual (v190) recodes.
#'
#' @param dhs_data DHS dataset (KR, IR, PR, or HR recode).
#' @param wealth_var Name of wealth quintile variable. Default: "v190" for
#'   individual recodes, "hv270" for household recodes.
#' @param quintiles Numeric vector of quintiles to include. Default: 1:5 (all).
#'   Use c(1) for poorest only, c(1,2) for poorest + poorer, etc.
#'
#' @return Input dataset with added `wealth_quintile` column, filtered to
#'   requested quintiles. Rows with NA wealth are removed.
#' @noRd
.add_wealth_quintile <- function(dhs_data, wealth_var = NULL, quintiles = 1:5) {
  # Auto-detect wealth variable if not specified
  if (is.null(wealth_var)) {
    if ("v190" %in% names(dhs_data)) {
      wealth_var <- "v190"
    } else if ("hv270" %in% names(dhs_data)) {
      wealth_var <- "hv270"
    } else {
      cli::cli_abort(c(
        "No wealth quintile variable found",
        "i" = "Expected 'v190' (individual recode) or 'hv270' (household recode)",
        "i" = "Specify wealth_var parameter if using custom variable name"
      ))
    }
  }

  if (!wealth_var %in% names(dhs_data)) {
    cli::cli_abort(
      "Wealth variable {.var {wealth_var}} not found in dataset"
    )
  }

  # Extract and validate wealth quintile
  dhs_data$wealth_quintile <- as.numeric(dhs_data[[wealth_var]])

  # Remove NA wealth
  n_before <- nrow(dhs_data)
  dhs_data <- dhs_data[!is.na(dhs_data$wealth_quintile), , drop = FALSE]
  n_after <- nrow(dhs_data)

  if (n_after < n_before) {
    cli::cli_alert_info(
      "Removed {n_before - n_after} rows with missing wealth quintile"
    )
  }

  # Filter to requested quintiles
  valid_q <- dhs_data$wealth_quintile %in% quintiles
  dhs_data <- dhs_data[valid_q, , drop = FALSE]

  if (nrow(dhs_data) == 0) {
    cli::cli_abort(
      "No observations remain after filtering to quintiles: {quintiles}"
    )
  }

  cli::cli_alert_info(
    "Filtered to {nrow(dhs_data)} observations in quintile(s): {paste(quintiles, collapse = ', ')}"
  )

  dhs_data
}


#' Aggregate to MBG clusters by wealth quintile
#'
#' Extension of `.aggregate_to_mbg_clusters` that produces separate outputs
#' for each wealth quintile.
#'
#' @param individual_data Individual-level data with cluster_id and wealth_quintile.
#' @param indicator_col Name of binary 0/1 indicator column.
#' @param gps_clean GPS data with cluster_id, lat, lon.
#' @param result_name Base name for output (e.g., "csb_public").
#' @param quintiles Numeric vector of quintiles to include. Default: 1:5.
#'
#' @return Named list of data.tables, one per quintile. Each has columns:
#'   cluster_id, indicator, samplesize, x, y. Names are formatted as
#'   "{result_name}_q{quintile}" (e.g., "csb_public_q1", "csb_public_q2").
#' @noRd
.aggregate_to_mbg_clusters_by_wealth <- function(
  individual_data,
  indicator_col,
  gps_clean,
  result_name = "indicator",
  quintiles = 1:5
) {
  if (!"wealth_quintile" %in% names(individual_data)) {
    cli::cli_abort(
      "Data must have 'wealth_quintile' column. Use .add_wealth_quintile() first."
    )
  }

  results <- list()

  for (q in quintiles) {
    subset_data <- individual_data[
      individual_data$wealth_quintile == q,
      ,
      drop = FALSE
    ]

    if (nrow(subset_data) == 0) {
      cli::cli_alert_warning(
        "{result_name} Q{q}: no observations in this quintile"
      )
      next
    }

    cluster_data <- .aggregate_to_mbg_clusters(
      individual_data = subset_data,
      indicator_col = indicator_col,
      gps_clean = gps_clean,
      result_name = paste0(result_name, "_q", q)
    )

    if (!is.null(cluster_data)) {
      results[[paste0(result_name, "_q", q)]] <- cluster_data
    }
  }

  results
}


#' Compute survey-weighted indicator by wealth quintile
#'
#' Extension of `.compute_dhs_indicator_generic` that calculates estimates
#' separately for each wealth quintile.
#'
#' @param data Prepared dataset with wealth_quintile column.
#' @param condition Indicator condition specification (from indicator functions).
#' @param group_var Optional grouping variable (e.g., "region" for adm1).
#' @param subnational_level Admin level name (e.g., "adm1").
#' @param ci_method CI method for svyciprop. Default: "logit".
#' @param quintiles Numeric vector of quintiles to include. Default: 1:5.
#'
#' @return Tibble with additional column `wealth_quintile` indicating which
#'   quintile each estimate applies to. All other columns match
#'   `.compute_dhs_indicator_generic` output.
#' @noRd
.compute_dhs_indicator_by_wealth <- function(
  data,
  condition,
  group_var = NULL,
  subnational_level = NULL,
  ci_method = "logit",
  quintiles = 1:5
) {
  if (!"wealth_quintile" %in% names(data)) {
    cli::cli_abort(
      "Data must have 'wealth_quintile' column. Use .add_wealth_quintile() first."
    )
  }

  results <- purrr::map_dfr(quintiles, function(q) {
    subset_data <- data[data$wealth_quintile == q, , drop = FALSE]

    if (nrow(subset_data) == 0) {
      return(tibble::tibble())
    }

    quintile_result <- .compute_dhs_indicator_generic(
      data = subset_data,
      condition = condition,
      group_var = group_var,
      subnational_level = subnational_level,
      ci_method = ci_method
    )

    if (nrow(quintile_result) > 0) {
      quintile_result$wealth_quintile <- q
    }

    quintile_result
  })

  # Reorder columns to put wealth_quintile early
  if (nrow(results) > 0 && "wealth_quintile" %in% names(results)) {
    col_order <- c(
      "wealth_quintile",
      setdiff(names(results), "wealth_quintile")
    )
    results <- results[, col_order, drop = FALSE]
  }

  results
}
