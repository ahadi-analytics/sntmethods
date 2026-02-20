#' Prepare Fever Prevalence Data for MBG Analysis
#'
#' Prepares cluster-level fever prevalence data for MBG analysis.
#' Calculates counts of alive children under 5 who had fever in the
#' last 2 weeks, at each survey cluster.
#'
#' @details
#' Methodology: \url{https://github.com/ahadi-analytics/sntmethods/blob/master/inst/methods/fever_dhs.yml}
#'
#' @param dhs_kr DHS Children's Recode (KR) dataset.
#' @param gps_data DHS GPS dataset with cluster coordinates.
#' @param indicators Character vector of indicators to calculate:
#'   \itemize{
#'     \item "fever": Fever prevalence among alive U5 children
#'   }
#'   Default: "fever".
#' @param survey_vars Named list mapping DHS variable names:
#'   \itemize{
#'     \item `cluster`: Cluster ID (default: "v001")
#'     \item `age`: Child's age in months (default: "hw1")
#'     \item `fever`: Fever in last 2 weeks (default: "h22")
#'     \item `alive`: Child survival status (default: "b5")
#'   }
#' @param gps_vars Named list for GPS variable mapping.
#'
#' @return A named list of data.tables (one per indicator), each with columns:
#'   \itemize{
#'     \item cluster_id: Cluster identifier
#'     \item indicator: Numerator count (children with fever)
#'     \item samplesize: Denominator count (all alive U5 children)
#'     \item x: Longitude
#'     \item y: Latitude
#'   }
#'
#' @examples
#' \dontrun{
#' fever_mbg <- calc_fever_mbg(
#'   dhs_kr = kr_data,
#'   gps_data = gps_data
#' )
#' }
#'
#' @seealso [calc_fever_dhs_core()] for survey-weighted estimates,
#'   [calc_csb_mbg()] for care-seeking behavior
#' @export
calc_fever_mbg <- function(
  dhs_kr,
  gps_data,
  indicators = "fever",
  survey_vars = list(
    cluster = "v001",
    age = "hw1",
    fever = "h22",
    alive = "b5"
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

  valid_indicators <- "fever"
  invalid <- setdiff(indicators, valid_indicators)
  if (length(invalid) > 0) {
    cli::cli_abort("Invalid indicators: {.val {invalid}}")
  }

  # ---- Prepare data using shared helpers ----

  gps_clean <- .prepare_gps_data(gps_data, gps_vars)

  kr_u5 <- tryCatch(
    .prepare_fever_data(
      dhs_kr = dhs_kr,
      survey_vars = survey_vars,
      include_survey_vars = FALSE
    ),
    error = function(e) {
      cli::cli_alert_warning(conditionMessage(e))
      return(NULL)
    }
  )

  if (is.null(kr_u5)) return(list())

  if (all(is.na(kr_u5$had_fever))) {
    cli::cli_alert_warning(
      "Fever variable {.var {survey_vars$fever}} is all NA - no fever data available"
    )
    return(list())
  }

  # ---- Calculate cluster-level indicators ----

  results <- list()

  if ("fever" %in% indicators) {
    fever_data <- kr_u5 |>
      dplyr::filter(!is.na(had_fever)) |>
      dplyr::mutate(fever_binary = as.integer(had_fever == 1))

    dt <- .aggregate_to_mbg_clusters(
      individual_data = fever_data,
      indicator_col = "fever_binary",
      gps_clean = gps_clean,
      result_name = "fever"
    )

    if (!is.null(dt)) {
      results[["fever"]] <- dt
    }
  }

  if (length(results) == 0) {
    cli::cli_alert_warning("No valid fever MBG data could be prepared")
  }

  results
}


#' Prepare Single Fever Indicator for MBG
#'
#' Convenience wrapper around [calc_fever_mbg()] to prepare fever
#' prevalence data for MBG analysis.
#'
#' @inheritParams calc_fever_mbg
#' @param indicator Single indicator name. Default: "fever".
#'
#' @return A data.table with columns: cluster_id, indicator, samplesize, x, y
#' @export
prep_fever_mbg <- function(
  dhs_kr,
  gps_data,
  indicator = "fever",
  survey_vars = list(
    cluster = "v001",
    age = "hw1",
    fever = "h22",
    alive = "b5"
  ),
  gps_vars = list(
    cluster = "DHSCLUST",
    lat = "LATNUM",
    lon = "LONGNUM"
  )
) {
  result <- calc_fever_mbg(
    dhs_kr = dhs_kr,
    gps_data = gps_data,
    indicators = indicator,
    survey_vars = survey_vars,
    gps_vars = gps_vars
  )

  if (length(result) == 0) {
    cli::cli_abort("No data returned for indicator {.val {indicator}}")
  }

  result[[1]]
}
