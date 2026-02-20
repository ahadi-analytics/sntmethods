#' Prepare Care-Seeking Behavior Data for MBG Analysis
#'
#' Prepares cluster-level care-seeking behavior data for MBG analysis.
#' Calculates proportions of febrile children who sought care at various
#' source types.
#'
#' @details
#' Methodology: \url{https://github.com/ahadi-analytics/sntmethods/blob/master/inst/methods/csb_dhs.yml}
#'
#' @param dhs_kr DHS Children's Recode (KR) dataset.
#' @param gps_data DHS GPS dataset with cluster coordinates.
#' @param indicators Character vector of indicators to calculate:
#'   \itemize{
#'     \item "any": Sought care anywhere
#'     \item "public": Sought care at public facility
#'     \item "private": Sought care at private facility
#'     \item "trained": Sought care from trained provider
#'     \item "none": Did not seek care
#'   }
#'   Default: c("any", "public", "private", "none").
#' @param csb_classification Data frame with h32 variable to category mapping.
#'   Must have columns `variable` and `csb`. If NULL, uses default WMR classification.
#' @param survey_vars Named list mapping DHS variable names.
#' @param gps_vars Named list for GPS variable mapping.
#'
#' @return A list of data.tables (one per indicator), each with columns:
#'   \itemize{
#'     \item cluster_id: Cluster identifier
#'     \item indicator: Numerator count
#'     \item samplesize: Denominator count
#'     \item x: Longitude
#'     \item y: Latitude
#'   }
#'
#' @details
#' This function uses KR data on children under 5 who had fever in the last
#' 2 weeks. Care-seeking is determined using h32 variables.
#'
#' Note: Care-seeking indicators (except "none") are NOT mutually exclusive.
#' A child can appear in both "public" and "private" if they visited both.
#'
#' @examples
#' \dontrun{
#' csb_mbg <- calc_csb_mbg(
#'   dhs_kr = kr_data,
#'   gps_data = gps_data,
#'   indicators = c("public", "none")
#' )
#' }
#'
#' @seealso [calc_csb_dhs()] for survey-weighted estimates
#' @export
calc_csb_mbg <- function(
  dhs_kr,
  gps_data,
  indicators = c("any", "public", "private", "none"),
  csb_classification = NULL,
  survey_vars = list(
    cluster = "v001",
    age = "hw1",
    fever = "h22"
  ),
  gps_vars = list(
    cluster = "DHSCLUST",
    lat = "LATNUM",
    lon = "LONGNUM"
  )
) {
  # ---- Input validation ----

  if (!is.data.frame(dhs_kr)) {
    cli::cli_abort("`dhs_kr` must be a data.frame or tibble")
  }
  if (!is.data.frame(gps_data)) {
    cli::cli_abort("`gps_data` must be a data.frame or tibble")
  }

  valid_indicators <- c("any", "public", "private", "trained", "none")
  invalid <- setdiff(indicators, valid_indicators)
  if (length(invalid) > 0) {
    cli::cli_abort("Invalid indicators: {.val {invalid}}")
  }

  # ---- Prepare data using shared helpers ----

  gps_clean <- .prepare_gps_data(gps_data, gps_vars)

  kr_fever <- .prepare_csb_data(
    dhs_kr = dhs_kr,
    survey_vars = survey_vars,
    csb_classification = csb_classification,
    include_survey_vars = FALSE
  )

  # ---- Calculate cluster-level indicators ----

  indicator_map <- list(
    any = "csb_any",
    public = "csb_public",
    private = "csb_private",
    trained = "csb_trained",
    none = "csb_none"
  )

  result_names <- list(
    any = "csb_any",
    public = "csb_public",
    private = "csb_private",
    trained = "csb_trained",
    none = "csb_none"
  )

  results <- list()

  for (ind in indicators) {
    indicator_col <- indicator_map[[ind]]
    result_name <- result_names[[ind]]

    dt <- .aggregate_to_mbg_clusters(
      individual_data = kr_fever,
      indicator_col = indicator_col,
      gps_clean = gps_clean,
      result_name = result_name
    )

    if (!is.null(dt)) {
      results[[result_name]] <- dt
    }
  }

  if (length(results) == 0) {
    cli::cli_abort("No valid MBG data could be prepared")
  }

  results
}


#' Prepare Single CSB Indicator for MBG
#'
#' @inheritParams calc_csb_mbg
#' @param indicator Single indicator name. Default: "public".
#'
#' @return A data.table with columns: cluster_id, indicator, samplesize, x, y
#' @export
prep_csb_mbg <- function(
  dhs_kr,
  gps_data,
  indicator = "public",
  csb_classification = NULL,
  survey_vars = list(
    cluster = "v001",
    age = "hw1",
    fever = "h22"
  ),
  gps_vars = list(
    cluster = "DHSCLUST",
    lat = "LATNUM",
    lon = "LONGNUM"
  )
) {
  result <- calc_csb_mbg(
    dhs_kr = dhs_kr,
    gps_data = gps_data,
    indicators = indicator,
    csb_classification = csb_classification,
    survey_vars = survey_vars,
    gps_vars = gps_vars
  )

  result[[1]]
}
