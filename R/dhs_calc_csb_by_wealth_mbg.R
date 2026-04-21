#' Prepare Care-Seeking Behavior Data by Wealth Quintile for MBG Analysis
#'
#' Prepares cluster-level care-seeking behavior data stratified by wealth
#' quintile for MBG analysis. Calculates proportions of febrile children who
#' sought care, separately for each wealth quintile.
#'
#' @details
#' This function extends [calc_csb_mbg()] to provide wealth-stratified estimates.
#' Each quintile produces separate MBG-ready outputs with cluster-level
#' numerators and denominators.
#'
#' **Important:** This is a standalone utility function for specialized wealth
#' stratification analysis. It is NOT called by [run_mbg_pipeline()]. Use this
#' function directly when you need wealth-disaggregated indicators for custom
#' MBG modeling or equity analysis.
#'
#' Methodology: \url{https://github.com/ahadi-analytics/sntmethods/blob/master/inst/methods/csb_dhs.yml}
#'
#' @param dhs_kr DHS Children's Recode (KR) dataset.
#' @param gps_data DHS GPS dataset with cluster coordinates.
#' @param indicators Character vector of indicators to calculate:
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
#'     \item "trained": Trained provider (public + formal + pharmacy)
#'     \item "none": Did not seek care
#'   }
#'   Default: c("any", "public", "private", "none").
#' @param quintiles Numeric vector of wealth quintiles to include. Default: 1:5
#'   (all quintiles). Use c(1) for poorest only, c(1,2) for poorest + poorer, etc.
#' @param wealth_var Name of wealth quintile variable in dhs_kr. Default: "v190".
#' @param csb_classification Data frame with h32 variable to category mapping.
#'   Must have columns `variable` and `csb`. If NULL, uses default classification.
#' @param csb_priority_method Character, one of "all" (default), "first",
#'   "public", or "private". Controls how overlapping care-seeking records
#'   are resolved so each individual is assigned to at most one sector.
#'   See [calc_csb_mbg()] for details. With non-"all" values, csb_public +
#'   csb_private + csb_none sums to 100% within each quintile.
#' @param survey_vars Named list mapping DHS variable names.
#' @param gps_vars Named list for GPS variable mapping.
#'
#' @return A nested list structure: first level keys are quintiles (e.g., "q1",
#'   "q2"), second level keys are indicators (e.g., "csb_public_q1"). Each
#'   leaf is a data.table with columns:
#'   \itemize{
#'     \item cluster_id: Cluster identifier
#'     \item indicator: Numerator count (children who sought care)
#'     \item samplesize: Denominator count (all febrile children in quintile)
#'     \item x: Longitude
#'     \item y: Latitude
#'   }
#'
#' @section Output Structure:
#' The function returns a list where each indicator-quintile combination gets
#' its own data.table. For example, with indicators = c("public", "private")
#' and quintiles = c(1, 5):
#' \preformatted{
#' list(
#'   csb_public_q1 = data.table(...),
#'   csb_public_q5 = data.table(...),
#'   csb_private_q1 = data.table(...),
#'   csb_private_q5 = data.table(...)
#' )
#' }
#'
#' @examples
#' \dontrun{
#' # Care-seeking by wealth for poorest quintile only
#' csb_poorest <- calc_csb_by_wealth_mbg(
#'   dhs_kr = kr_data,
#'   gps_data = gps_data,
#'   indicators = c("public", "private"),
#'   quintiles = 1
#' )
#'
#' # Compare poorest vs richest quintiles
#' csb_comparison <- calc_csb_by_wealth_mbg(
#'   dhs_kr = kr_data,
#'   gps_data = gps_data,
#'   indicators = c("any", "public", "none"),
#'   quintiles = c(1, 5)
#' )
#' }
#'
#' @seealso
#' * [calc_csb_mbg()] for non-stratified care-seeking MBG data
#' * [calc_csb_by_wealth_dhs()] for wealth-stratified survey-weighted estimates
#' * [calc_csb_dhs()] for standard survey-weighted estimates
#' @export
calc_csb_by_wealth_mbg <- function(
  dhs_kr,
  gps_data,
  indicators = c("any", "public", "private", "none"),
  quintiles = 1:5,
  wealth_var = "v190",
  csb_classification = NULL,
  csb_priority_method = c("all", "first", "public", "private"),
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
  csb_priority_method <- match.arg(csb_priority_method)

  # ---- Input validation ----

  if (!is.data.frame(dhs_kr)) {
    cli::cli_abort("`dhs_kr` must be a data.frame or tibble")
  }
  if (!is.data.frame(gps_data)) {
    cli::cli_abort("`gps_data` must be a data.frame or tibble")
  }

  # Validate quintiles
  if (!all(quintiles %in% 1:5)) {
    cli::cli_abort("`quintiles` must be numeric values between 1 and 5")
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
    include_survey_vars = FALSE,
    csb_priority_method = csb_priority_method
  )

  # Add wealth quintile and filter
  kr_fever <- .add_wealth_quintile(
    dhs_data = kr_fever,
    wealth_var = wealth_var,
    quintiles = quintiles
  )

  # ---- Calculate cluster-level indicators by wealth quintile ----

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

  # Maps short indicator name -> output key base name
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

    # Aggregate by wealth quintile
    wealth_results <- .aggregate_to_mbg_clusters_by_wealth(
      individual_data = kr_fever,
      indicator_col = indicator_col,
      gps_clean = gps_clean,
      result_name = result_name,
      quintiles = quintiles
    )

    # Merge into main results list
    results <- c(results, wealth_results)
  }

  if (length(results) == 0) {
    cli::cli_abort("No valid MBG data could be prepared")
  }

  cli::cli_alert_success(
    "Prepared {length(results)} indicator-quintile combinations"
  )

  results
}


#' Prepare Single CSB Indicator by Wealth Quintile for MBG
#'
#' Convenience wrapper around [calc_csb_by_wealth_mbg()] to prepare a single
#' care-seeking indicator stratified by wealth quintile.
#'
#' @inheritParams calc_csb_by_wealth_mbg
#' @param indicator Single indicator name. Default: "public".
#'
#' @return Named list of data.tables, one per quintile, each with columns:
#'   cluster_id, indicator, samplesize, x, y
#'
#' @examples
#' \dontrun{
#' # Public care-seeking for poorest quintile only
#' csb_pub_q1 <- prep_csb_by_wealth_mbg(
#'   dhs_kr = kr_data,
#'   gps_data = gps_data,
#'   indicator = "public",
#'   quintiles = 1
#' )
#' }
#'
#' @export
prep_csb_by_wealth_mbg <- function(
  dhs_kr,
  gps_data,
  indicator = "public",
  quintiles = 1:5,
  wealth_var = "v190",
  csb_classification = NULL,
  csb_priority_method = c("all", "first", "public", "private"),
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
  csb_priority_method <- match.arg(csb_priority_method)

  result <- calc_csb_by_wealth_mbg(
    dhs_kr = dhs_kr,
    gps_data = gps_data,
    indicators = indicator,
    quintiles = quintiles,
    wealth_var = wealth_var,
    csb_classification = csb_classification,
    csb_priority_method = csb_priority_method,
    survey_vars = survey_vars,
    gps_vars = gps_vars
  )

  result
}
