#' Prepare SMC Data for MBG Analysis
#'
#' Prepares cluster-level Seasonal Malaria Chemoprevention (SMC) receipt data
#' for MBG analysis. SMC coverage among children under 5.
#'
#' @details
#' Methodology: \url{https://github.com/ahadi-analytics/sntmethods/blob/master/inst/methods/smc_dhs.yml}
#'
#' @param dhs_kr DHS Children's Recode (KR) dataset.
#' @param gps_data DHS GPS dataset with cluster coordinates.
#' @param survey_vars Named list mapping DHS variable names.
#' @param gps_vars Named list for GPS variable mapping.
#'
#' @return A data.table with columns:
#'   \itemize{
#'     \item cluster_id: Cluster identifier
#'     \item indicator: Number of children who received SMC
#'     \item samplesize: Total number of children in analysis
#'     \item x: Longitude
#'     \item y: Latitude
#'   }
#'
#' @details
#' SMC variable availability varies by survey. Common variables include:
#' \itemize{
#'   \item hml43: SMC in malaria season (DHS-7+)
#'   \item ml13g: Received antimalarial for prevention
#' }
#'
#' This function first checks which SMC-related variables are available
#' and uses the most appropriate one.
#'
#' @examples
#' \dontrun{
#' smc_mbg <- calc_smc_mbg(
#'   dhs_kr = kr_data,
#'   gps_data = gps_data
#' )
#' }
#'
#' @export
calc_smc_mbg <- function(
  dhs_kr,
  gps_data,
  survey_vars = list(
    cluster = "v001",
    age = "hw1",
    smc_primary = "hml43",
    smc_alt = "ml13g"
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

  # ---- Prepare KR data ----

  kr <- .prepare_smc_data(dhs_kr, survey_vars, include_survey_vars = FALSE, strict = FALSE)
  if (is.null(kr)) return(NULL)

  # ---- Aggregate to cluster level ----

  result <- .aggregate_to_mbg_clusters(
    kr, "received_smc", gps_clean, "smc_coverage"
  )
  if (is.null(result)) return(NULL)

  result
}


#' Prepare SMC Data for MBG (Alias)
#'
#' @inheritParams calc_smc_mbg
#' @return A data.table with columns: cluster_id, indicator, samplesize, x, y
#' @export
prep_smc_mbg <- calc_smc_mbg
