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
#' @param indicators Character vector of indicators to
#'   calculate. standardized sector breakdown:
#'   \itemize{
#'     \item "any": Sought care anywhere (public or private)
#'     \item "public": Public sector including CHW
#'     \item "pub_nochw": Public sector excluding CHW
#'     \item "chw": Community health worker only
#'     \item "private": Any private sector
#'     \item "priv_formal": Private formal sector only
#'     \item "pharmacy": Pharmacy / drug shop only
#'     \item "priv_informal": Private informal only
#'     \item "priv_form_pha": Private formal or pharmacy
#'     \item "trained": Trained provider (public + formal +
#'       pharmacy)
#'     \item "none": Did not seek care
#'   }
#'   Default: c("any", "public", "private", "none").
#' @param csb_classification Data frame with h32 variable to category mapping.
#'   Must have columns `variable` and `csb`. If NULL, uses default classification.
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

  # Accept both prefixed ("csb_public") and short ("public") forms
  indicators <- sub("^csb_", "", indicators)

  valid_indicators <- c(
    "any", "public", "pub_nochw", "chw",
    "private", "priv_formal", "pharmacy",
    "priv_informal", "priv_form_pha",
    "trained", "none"
  )
  invalid <- setdiff(indicators, valid_indicators)
  if (length(invalid) > 0) {
    cli::cli_abort(
      "Invalid indicators: {.val {invalid}}"
    )
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

  # Maps short indicator name -> CSB column in data
  indicator_map <- list(
    any = "csb_any",
    public = "csb_public",
    pub_nochw = "csb_public_nochw",
    chw = "csb_chw",
    private = "csb_private",
    priv_formal = "csb_private_formal_ind",
    pharmacy = "csb_pharmacy",
    priv_informal = "csb_private_informal",
    priv_form_pha = "csb_private_formal_pha",
    trained = "csb_trained",
    none = "csb_none"
  )

  # Maps short indicator name -> output key name
  result_names <- list(
    any = "csb_any",
    public = "csb_public",
    pub_nochw = "csb_pub_nochw",
    chw = "csb_chw",
    private = "csb_private",
    priv_formal = "csb_priv_formal",
    pharmacy = "csb_pharmacy",
    priv_informal = "csb_priv_informal",
    priv_form_pha = "csb_priv_form_pha",
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
