#' Prepare IRS Data for MBG Analysis
#'
#' Prepares cluster-level Indoor Residual Spraying (IRS) coverage data for
#' Model-Based Geostatistics (MBG) analysis. Calculates the proportion of
#' households sprayed in the last 12 months.
#'
#' @details
#' Methodology: \url{https://github.com/ahadi-analytics/sntmethods/blob/master/inst/methods/irs_dhs.yml}
#'
#' @param dhs_hr DHS Household Records dataset.
#' @param gps_data DHS GPS dataset with cluster coordinates.
#' @param survey_vars Named list mapping DHS variable names.
#' @param gps_vars Named list for GPS variable mapping.
#'
#' @return A data.table with columns:
#'   \itemize{
#'     \item cluster_id: Cluster identifier
#'     \item indicator: Number of households sprayed
#'     \item samplesize: Total number of households
#'     \item x: Longitude
#'     \item y: Latitude
#'   }
#'
#' @details
#' IRS coverage is measured using variable hv253 (household sprayed in last
#' 12 months). This is a household-level indicator.
#'
#' @examples
#' \dontrun{
#' irs_mbg <- calc_irs_mbg(
#'   dhs_hr = hr_data,
#'   gps_data = gps_data
#' )
#' }
#'
#' @export
calc_irs_mbg <- function(
  dhs_hr,
  gps_data,
  survey_vars = list(
    cluster = "hv001",
    irs = "hv253"
  ),
  gps_vars = list(
    cluster = "DHSCLUST",
    lat = "LATNUM",
    lon = "LONGNUM"
  )
) {
  # ---- Input validation ----

  if (!is.data.frame(gps_data)) {
    cli::cli_abort("`gps_data` must be a data.frame or tibble")
  }

  # ---- Prepare GPS data ----

  gps_clean <- .prepare_gps_data(gps_data, gps_vars)

  # ---- Prepare HR data ----

  hr <- .prepare_irs_data(dhs_hr, survey_vars, include_survey_vars = FALSE)

  # ---- Aggregate to cluster level ----

  result <- .aggregate_to_mbg_clusters(hr, "sprayed", gps_clean, "irs_coverage")

  if (is.null(result)) {
    cli::cli_abort("No valid MBG data could be prepared for IRS coverage")
  }

  result
}


#' Prepare IRS Data for MBG (Alias)
#'
#' Alias for calc_irs_mbg for consistent naming.
#'
#' @inheritParams calc_irs_mbg
#' @return A data.table with columns: cluster_id, indicator, samplesize, x, y
#' @export
prep_irs_mbg <- calc_irs_mbg
