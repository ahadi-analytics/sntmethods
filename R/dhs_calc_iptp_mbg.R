#' Prepare IPTp Data for MBG Analysis
#'
#' Prepares cluster-level Intermittent Preventive Treatment in pregnancy (IPTp)
#' data for MBG analysis. Calculates both cumulative (1+, 2+, 3+) and
#' exclusive (exactly 1, exactly 2, exactly 3) dose categories.
#'
#' @details
#' Methodology: \url{https://github.com/ahadi-analytics/sntmethods/blob/master/inst/methods/iptp_dhs.yml}
#'
#' @param dhs_ir DHS Individual Recode dataset.
#' @param gps_data DHS GPS dataset with cluster coordinates.
#' @param indicators Character vector of indicators to calculate:
#'   \itemize{
#'     \item Cumulative:
#'     \itemize{
#'       \item "iptp_1plus": At least 1 dose
#'       \item "iptp_2plus": At least 2 doses
#'       \item "iptp_3plus": At least 3 doses (WHO recommendation)
#'       \item "iptp_4plus": At least 4 doses (requires ml1_1 as sp_doses)
#'     }
#'     \item Exclusive:
#'     \itemize{
#'       \item "iptp_1only": Exactly 1 dose
#'       \item "iptp_2only": Exactly 2 doses
#'       \item "iptp_3only": Exactly 3 doses
#'     }
#'   }
#'   Default: c("iptp_1plus", "iptp_2plus", "iptp_3plus").
#' @param birth_window_months Months to look back for births. Default: 36.
#' @param survey_vars Named list mapping DHS variable names.
#' @param gps_vars Named list for GPS variable mapping.
#'
#' @return A list of data.tables (one per indicator).
#'
#' @details
#' No ANC attendance restriction is applied; the denominator is all women
#' with a birth in the analysis window and a valid SP response
#' (`sp_doses <= 7`). The default `survey_vars$sp_doses = "ml1_1"` is
#' the dose count variable (0-7). If `ml1_1` is not available, the helper
#' falls back to `sp_taken` (`"m49a_1"`, binary 0/1), in which case only
#' IPTp 1+ will produce meaningful results.
#'
#' @examples
#' \dontrun{
#' iptp_mbg <- calc_iptp_mbg(
#'   dhs_ir = ir_data,
#'   gps_data = gps_data,
#'   indicators = c("iptp_2plus", "iptp_3plus")
#' )
#' }
#'
#' @seealso [calc_iptp_dhs()] for survey-weighted estimates
#' @export
calc_iptp_mbg <- function(
  dhs_ir,
  gps_data,
  indicators = c("iptp_1plus", "iptp_2plus", "iptp_3plus"),
  birth_window_months = 36,
  survey_vars = list(
    cluster = "v001",
    interview_date = "v008",
    birth_date = "b3_01",
    sp_doses = "ml1_1",
    sp_taken = "m49a_1"
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

  valid_indicators <- c(
    "iptp_1plus", "iptp_2plus", "iptp_3plus", "iptp_4plus",
    "iptp_1only", "iptp_2only", "iptp_3only"
  )
  invalid <- setdiff(indicators, valid_indicators)
  if (length(invalid) > 0) {
    cli::cli_abort("Invalid indicators: {.val {invalid}}")
  }

  # ---- Prepare GPS data ----

  gps_clean <- .prepare_gps_data(gps_data, gps_vars)

  # ---- Prepare IR data ----

  ir <- .prepare_iptp_data(
    dhs_ir, survey_vars, birth_window_months,
    include_survey_vars = FALSE
  )
  if (is.null(ir)) return(NULL)

  # ---- Aggregate to cluster level ----

  indicator_map <- list(
    iptp_1plus = "has_1plus",
    iptp_2plus = "has_2plus",
    iptp_3plus = "has_3plus",
    iptp_4plus = "has_4plus",
    iptp_1only = "has_1only",
    iptp_2only = "has_2only",
    iptp_3only = "has_3only"
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

  if (length(results) == 0) return(NULL)

  results
}


#' Prepare Single IPTp Indicator for MBG
#'
#' @inheritParams calc_iptp_mbg
#' @param doses Minimum doses for cumulative indicator (1, 2, or 3).
#'
#' @return A data.table with columns: cluster_id, indicator, samplesize, x, y
#' @export
prep_iptp_mbg <- function(
  dhs_ir,
  gps_data,
  doses = 3,
  birth_window_months = 36,
  survey_vars = list(
    cluster = "v001",
    interview_date = "v008",
    birth_date = "b3_01",
    sp_doses = "ml1_1",
    sp_taken = "m49a_1"
  ),
  gps_vars = list(
    cluster = "DHSCLUST",
    lat = "LATNUM",
    lon = "LONGNUM"
  )
) {
  indicator_name <- paste0("iptp_", doses, "plus")

  result <- calc_iptp_mbg(
    dhs_ir = dhs_ir,
    gps_data = gps_data,
    indicators = indicator_name,
    birth_window_months = birth_window_months,
    survey_vars = survey_vars,
    gps_vars = gps_vars
  )

  result[[1]]
}
