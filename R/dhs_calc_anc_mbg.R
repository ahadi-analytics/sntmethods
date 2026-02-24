#' Prepare ANC Data for MBG Analysis
#'
#' Prepares cluster-level Antenatal Care (ANC) attendance data for Model-Based
#' Geostatistics (MBG) analysis. Calculates the proportion of women who had
#' at least N ANC visits during their most recent pregnancy.
#'
#' @details
#' Methodology: \url{https://github.com/ahadi-analytics/sntmethods/blob/master/inst/methods/anc_dhs.yml}
#'
#' @param dhs_ir DHS Individual Recode dataset.
#' @param gps_data DHS GPS dataset with cluster coordinates.
#' @param indicators Character vector of indicators to calculate:
#'   \itemize{
#'     \item "anc_1plus": At least 1 ANC visit
#'     \item "anc_3plus": At least 3 ANC visits
#'     \item "anc_4plus": At least 4 ANC visits
#'     \item "anc_8plus": At least 8 ANC visits (2016 WHO recommendation)
#'   }
#'   Default: c("anc_1plus", "anc_3plus", "anc_4plus").
#' @param birth_window_months Number of months to look back for births.
#'   Default: 36 (3 years). Max 60 (5 years).
#' @param survey_vars Named list mapping DHS variable names.
#' @param gps_vars Named list for GPS variable mapping.
#'
#' @return A list of data.tables (one per indicator), each with columns:
#'   \itemize{
#'     \item cluster_id: Cluster identifier
#'     \item indicator: Number of women meeting threshold
#'     \item samplesize: Total number of women with recent births
#'     \item x: Longitude
#'     \item y: Latitude
#'   }
#'
#' @details
#' This function uses data on most recent births within the specified window.
#' ANC visits are measured using m14 (number of antenatal visits).
#'
#' @examples
#' \dontrun{
#' anc_mbg <- calc_anc_mbg(
#'   dhs_ir = ir_data,
#'   gps_data = gps_data,
#'   indicators = c("anc_1plus", "anc_4plus")
#' )
#' }
#'
#' @export
calc_anc_mbg <- function(
  dhs_ir,
  gps_data,
  indicators = c("anc_1plus", "anc_3plus", "anc_4plus"),
  birth_window_months = 36,
  survey_vars = list(
    cluster = "v001",
    interview_date = "v008",
    birth_date = "b3_01",
    anc_visits = "m14_1"
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

  valid_indicators <- c("anc_1plus", "anc_3plus", "anc_4plus", "anc_8plus")
  invalid <- setdiff(indicators, valid_indicators)
  if (length(invalid) > 0) {
    cli::cli_abort("Invalid indicators: {.val {invalid}}")
  }

  # ---- Prepare GPS data ----

  gps_clean <- .prepare_gps_data(gps_data, gps_vars)

  # ---- Prepare IR data ----

  ir <- .prepare_anc_data(
    dhs_ir, survey_vars, birth_window_months,
    include_survey_vars = FALSE
  )

  # ---- Aggregate to cluster level ----

  indicator_map <- list(
    anc_1plus = "has_anc1",
    anc_3plus = "has_anc3",
    anc_4plus = "has_anc4",
    anc_8plus = "has_anc8"
  )

  results <- list()

  for (ind in indicators) {
    cluster_dt <- .aggregate_to_mbg_clusters(
      ir, indicator_map[[ind]], gps_clean, ind
    )
    if (!is.null(cluster_dt)) {
      results[[ind]] <- cluster_dt
    }
  }

  if (length(results) == 0) {
    cli::cli_abort("No valid MBG data could be prepared")
  }

  results
}


#' Prepare Single ANC Indicator for MBG
#'
#' Simplified function to prepare a single ANC indicator.
#'
#' @inheritParams calc_anc_mbg
#' @param threshold Minimum number of ANC visits (1, 4, or 8). Default: 4.
#'
#' @return A data.table with columns: cluster_id, indicator, samplesize, x, y
#'
#' @export
prep_anc_mbg <- function(
  dhs_ir,
  gps_data,
  threshold = 4,
  birth_window_months = 36,
  survey_vars = list(
    cluster = "v001",
    interview_date = "v008",
    birth_date = "b3_01",
    anc_visits = "m14_1"
  ),
  gps_vars = list(
    cluster = "DHSCLUST",
    lat = "LATNUM",
    lon = "LONGNUM"
  )
) {
  indicator_name <- paste0("anc_", threshold, "plus")

  result <- calc_anc_mbg(
    dhs_ir = dhs_ir,
    gps_data = gps_data,
    indicators = indicator_name,
    birth_window_months = birth_window_months,
    survey_vars = survey_vars,
    gps_vars = gps_vars
  )

  result[[1]]
}
