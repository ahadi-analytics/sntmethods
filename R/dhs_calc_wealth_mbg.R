#' Prepare Wealth Quintile Distribution Data for MBG Analysis
#'
#' Prepares cluster-level wealth quintile distribution data for Model-Based
#' Geostatistics (MBG) analysis. Calculates proportions of households in each
#' wealth quintile, aggregated to cluster level.
#'
#' @details
#' This function prepares wealth distribution indicators for spatial modeling.
#' Unlike the survey-weighted [calc_wealth_dhs()], this uses simple cluster-level
#' counts without survey weights - MBG handles spatial smoothing internally.
#'
#' **Pipeline Integration:** This function IS called by [run_mbg_pipeline()]
#' when you specify `indicators = "wealth"` or individual codes like
#' `"prop_poorest"`.
#'
#' Methodology: Uses DHS wealth quintile variable (hv270 in HR recode) which
#' classifies households into 5 quintiles based on wealth index factor scores.
#'
#' @param dhs_hr DHS Household Records (HR) dataset.
#' @param gps_data DHS GPS dataset with cluster coordinates.
#' @param indicators Character vector of indicators to calculate:
#'   \itemize{
#'     \item "prop_poorest" or "prop_q1": Proportion in poorest quintile (Q1)
#'     \item "prop_poorer" or "prop_q2": Proportion in second quintile (Q2)
#'     \item "prop_middle" or "prop_q3": Proportion in middle quintile (Q3)
#'     \item "prop_richer" or "prop_q4": Proportion in fourth quintile (Q4)
#'     \item "prop_richest" or "prop_q5": Proportion in richest quintile (Q5)
#'   }
#'   Default: c("prop_poorest", "prop_richest") for equity analysis.
#' @param survey_vars Named list mapping DHS variable names:
#'   \itemize{
#'     \item cluster: Cluster ID (default: "hv001")
#'     \item wealth_quintile: Wealth quintile variable (default: "hv270")
#'   }
#' @param gps_vars Named list for GPS variable mapping.
#'
#' @return A named list of data.tables (one per indicator), each with columns:
#'   \itemize{
#'     \item cluster_id: Cluster identifier
#'     \item indicator: Numerator count (households in quintile)
#'     \item samplesize: Denominator count (all households)
#'     \item x: Longitude
#'     \item y: Latitude
#'   }
#'
#' @section Output Structure:
#' For `indicators = c("prop_poorest", "prop_richest")`:
#' \preformatted{
#' list(
#'   prop_poorest = data.table(cluster_id, indicator, samplesize, x, y),
#'   prop_richest = data.table(cluster_id, indicator, samplesize, x, y)
#' )
#' }
#'
#' @examples
#' \dontrun{
#' # Poorest quintile distribution for equity mapping
#' wealth_poorest <- calc_wealth_mbg(
#'   dhs_hr = hr_data,
#'   gps_data = gps_data,
#'   indicators = "prop_poorest"
#' )
#'
#' # Compare poorest vs richest for inequality analysis
#' wealth_inequality <- calc_wealth_mbg(
#'   dhs_hr = hr_data,
#'   gps_data = gps_data,
#'   indicators = c("prop_poorest", "prop_richest")
#' )
#'
#' # Via pipeline
#' results <- run_mbg_pipeline(
#'   country_iso3 = "gin",
#'   indicators = "wealth",
#'   ...
#' )
#' }
#'
#' @seealso
#' * [calc_wealth_dhs()] for survey-weighted wealth estimates with CIs
#' * [run_mbg_pipeline()] for automated pipeline processing
#' @export
calc_wealth_mbg <- function(
  dhs_hr,
  gps_data,
  indicators = c("prop_poorest", "prop_richest"),
  survey_vars = list(
    cluster = "hv001",
    wealth_quintile = "hv270"
  ),
  gps_vars = list(
    cluster = "DHSCLUST",
    lat = "LATNUM",
    lon = "LONGNUM"
  )
) {
  # ---- Input validation ----

  if (!is.data.frame(dhs_hr)) {
    cli::cli_abort("`dhs_hr` must be a data.frame or tibble")
  }
  if (!is.data.frame(gps_data)) {
    cli::cli_abort("`gps_data` must be a data.frame or tibble")
  }

  dict <- .wealth_mbg_dictionary()
  dict_names <- vapply(dict, `[[`, character(1), "name")

  # Default: poorest and richest
  if (is.null(indicators)) {
    indicators <- c("prop_poorest", "prop_richest")
  }

  invalid <- setdiff(indicators, dict_names)
  if (length(invalid) > 0) {
    cli::cli_abort(
      "Invalid indicators: {.val {invalid}}. Valid: {.val {dict_names}}"
    )
  }

  # Filter dictionary to requested indicators
  dict_specs <- dict[vapply(dict, function(d) d$name %in% indicators, logical(1))]

  # ---- Prepare GPS data ----

  gps_clean <- .prepare_gps_data(gps_data, gps_vars)

  # ---- Prepare household data ----

  cluster_var <- survey_vars$cluster
  wealth_var <- survey_vars$wealth_quintile

  if (!cluster_var %in% names(dhs_hr)) {
    cli::cli_abort("Cluster variable {.var {cluster_var}} not found in dhs_hr")
  }
  if (!wealth_var %in% names(dhs_hr)) {
    cli::cli_abort("Wealth variable {.var {wealth_var}} not found in dhs_hr")
  }

  # Extract and zap labels
  hr_clean <- dhs_hr |>
    dplyr::mutate(dplyr::across(dplyr::everything(), haven::zap_labels)) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), as.vector))

  hr_data <- tibble::tibble(
    cluster_id = hr_clean[[cluster_var]],
    wealth_quintile = as.numeric(hr_clean[[wealth_var]])
  )

  # Filter to valid quintiles
  hr_data <- hr_data |>
    dplyr::filter(wealth_quintile %in% 1:5, !is.na(wealth_quintile))

  if (nrow(hr_data) == 0) {
    cli::cli_abort("No valid household data after filtering")
  }

  cli::cli_alert_success(
    "Valid households: {format(nrow(hr_data), big.mark = ',')}"
  )

  # Create binary indicators for each quintile
  hr_data <- hr_data |>
    dplyr::mutate(
      in_q1 = as.integer(wealth_quintile == 1),
      in_q2 = as.integer(wealth_quintile == 2),
      in_q3 = as.integer(wealth_quintile == 3),
      in_q4 = as.integer(wealth_quintile == 4),
      in_q5 = as.integer(wealth_quintile == 5)
    )

  # ---- Dictionary-driven indicator loop ----

  results <- list()

  for (spec in dict_specs) {
    outcome_col <- spec$outcome

    if (!outcome_col %in% names(hr_data)) {
      cli::cli_alert_warning(
        "Outcome {.var {outcome_col}} not found for {.val {spec$name}} - skipping"
      )
      next
    }

    # Drop NAs in outcome
    filtered <- hr_data[!is.na(hr_data[[outcome_col]]), , drop = FALSE]

    if (nrow(filtered) == 0) {
      cli::cli_alert_warning("No data for {.val {spec$name}} - skipping")
      next
    }

    dt <- .aggregate_to_mbg_clusters(filtered, outcome_col, gps_clean, spec$name)
    if (!is.null(dt)) {
      results[[spec$name]] <- dt
    }
  }

  if (length(results) == 0) {
    cli::cli_abort("No valid MBG data could be prepared")
  }

  cli::cli_alert_success(
    "Prepared {length(results)} wealth indicator(s)"
  )

  results
}


# =============================================================================
# Wealth MBG Indicator Dictionary
# =============================================================================

#' Wealth MBG Indicator Dictionary
#'
#' Returns the full set of standardized indicator specifications for
#' cluster-level wealth MBG output. Each entry defines the indicator name
#' and outcome column (binary indicator for each quintile).
#'
#' @return List of named lists with fields: \code{name}, \code{outcome},
#'   \code{quintile}.
#' @noRd
.wealth_mbg_dictionary <- function() {
  list(
    list(name = "prop_poorest", outcome = "in_q1", quintile = 1),
    list(name = "prop_q1",      outcome = "in_q1", quintile = 1),
    list(name = "prop_poorer",  outcome = "in_q2", quintile = 2),
    list(name = "prop_q2",      outcome = "in_q2", quintile = 2),
    list(name = "prop_middle",  outcome = "in_q3", quintile = 3),
    list(name = "prop_q3",      outcome = "in_q3", quintile = 3),
    list(name = "prop_richer",  outcome = "in_q4", quintile = 4),
    list(name = "prop_q4",      outcome = "in_q4", quintile = 4),
    list(name = "prop_richest", outcome = "in_q5", quintile = 5),
    list(name = "prop_q5",      outcome = "in_q5", quintile = 5)
  )
}


#' Prepare Single Wealth Indicator for MBG
#'
#' Convenience wrapper around [calc_wealth_mbg()] to prepare a single
#' wealth quintile distribution indicator.
#'
#' @inheritParams calc_wealth_mbg
#' @param indicator Single indicator name. Default: "prop_poorest".
#'
#' @return Named list with single data.table containing columns:
#'   cluster_id, indicator, samplesize, x, y
#'
#' @examples
#' \dontrun{
#' # Poorest quintile distribution only
#' poorest <- prep_wealth_mbg(
#'   dhs_hr = hr_data,
#'   gps_data = gps_data,
#'   indicator = "prop_poorest"
#' )
#' }
#'
#' @export
prep_wealth_mbg <- function(
  dhs_hr,
  gps_data,
  indicator = "prop_poorest",
  survey_vars = list(
    cluster = "hv001",
    wealth_quintile = "hv270"
  ),
  gps_vars = list(
    cluster = "DHSCLUST",
    lat = "LATNUM",
    lon = "LONGNUM"
  )
) {
  result <- calc_wealth_mbg(
    dhs_hr = dhs_hr,
    gps_data = gps_data,
    indicators = indicator,
    survey_vars = survey_vars,
    gps_vars = gps_vars
  )

  result
}
