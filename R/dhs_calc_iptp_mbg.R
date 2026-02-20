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
#'       \item "1plus": At least 1 dose
#'       \item "2plus": At least 2 doses
#'       \item "3plus": At least 3 doses (WHO recommendation)
#'     }
#'     \item Exclusive:
#'     \itemize{
#'       \item "1only": Exactly 1 dose
#'       \item "2only": Exactly 2 doses
#'       \item "3only": Exactly 3 doses
#'     }
#'   }
#'   Default: c("1plus", "2plus", "3plus").
#' @param birth_window_months Months to look back for births. Default: 36.
#' @param survey_vars Named list mapping DHS variable names.
#' @param gps_vars Named list for GPS variable mapping.
#'
#' @return A list of data.tables (one per indicator).
#'
#' @details
#' IPTp coverage is measured using m49a (SP/Fansidar during pregnancy).
#' The denominator is women with a live birth in the specified window
#' who attended ANC.
#'
#' @examples
#' \dontrun{
#' iptp_mbg <- calc_iptp_mbg(
#'   dhs_ir = ir_data,
#'   gps_data = gps_data,
#'   indicators = c("2plus", "3plus")
#' )
#' }
#'
#' @seealso [calc_iptp_dhs()] for survey-weighted estimates
#' @export
calc_iptp_mbg <- function(
  dhs_ir,
  gps_data,
  indicators = c("1plus", "2plus", "3plus"),
  birth_window_months = 36,
  survey_vars = list(
    cluster = "v001",
    interview_date = "v008",
    birth_date = "b3_01",
    sp_doses = "m49a_1"
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

  valid_indicators <- c("1plus", "2plus", "3plus", "1only", "2only", "3only")
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

  # ---- Aggregate to cluster level ----

  indicator_map <- list(
    `1plus` = "has_1plus",
    `2plus` = "has_2plus",
    `3plus` = "has_3plus",
    `1only` = "has_1only",
    `2only` = "has_2only",
    `3only` = "has_3only"
  )

  result_names <- list(
    `1plus` = "iptp_1plus",
    `2plus` = "iptp_2plus",
    `3plus` = "iptp_3plus",
    `1only` = "iptp_1only",
    `2only` = "iptp_2only",
    `3only` = "iptp_3only"
  )

  results <- list()

  for (ind in indicators) {
    cluster_dt <- .aggregate_to_mbg_clusters(
      ir, indicator_map[[ind]], gps_clean, result_names[[ind]]
    )
    if (!is.null(cluster_dt)) {
      results[[result_names[[ind]]]] <- cluster_dt
    }
  }

  if (length(results) == 0) {
    cli::cli_abort("No valid MBG data could be prepared")
  }

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
    sp_doses = "m49a_1"
  ),
  gps_vars = list(
    cluster = "DHSCLUST",
    lat = "LATNUM",
    lon = "LONGNUM"
  )
) {
  indicator_name <- paste0(doses, "plus")

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
