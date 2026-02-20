#' Prepare Malaria Diagnostic Testing Data for MBG Analysis
#'
#' Prepares cluster-level malaria diagnostic testing data for MBG analysis.
#' Calculates counts of febrile children under 5 who had blood taken for
#' malaria testing, at each survey cluster.
#'
#' @details
#' Methodology: \url{https://github.com/ahadi-analytics/sntmethods/blob/master/inst/methods/malaria_dx_dhs.yml}
#'
#' @param dhs_kr DHS Children's Recode (KR) dataset.
#' @param gps_data DHS GPS dataset with cluster coordinates.
#' @param indicators Character vector of indicators to calculate:
#'   \itemize{
#'     \item "malaria_dx": Blood taken for malaria test among febrile U5
#'   }
#'   Default: "malaria_dx".
#' @param survey_vars Named list mapping DHS variable names:
#'   \itemize{
#'     \item `cluster`: Cluster ID (default: "v001")
#'     \item `age`: Child's age in months (default: "hw1")
#'     \item `fever`: Fever in last 2 weeks (default: "h22")
#'     \item `malaria_dx`: Blood taken for malaria test (default: "h47")
#'   }
#' @param gps_vars Named list for GPS variable mapping.
#'
#' @return A named list of data.tables (one per indicator), each with columns:
#'   \itemize{
#'     \item cluster_id: Cluster identifier
#'     \item indicator: Numerator count (children tested)
#'     \item samplesize: Denominator count (febrile U5 children)
#'     \item x: Longitude
#'     \item y: Latitude
#'   }
#'
#' @examples
#' \dontrun{
#' dx_mbg <- calc_malaria_dx_mbg(
#'   dhs_kr = kr_data,
#'   gps_data = gps_data
#' )
#' }
#'
#' @seealso [calc_malaria_dx_dhs_core()] for survey-weighted estimates,
#'   [calc_act_mbg()] for ACT treatment
#' @export
calc_malaria_dx_mbg <- function(
  dhs_kr,
  gps_data,
  indicators = "malaria_dx",
  survey_vars = list(
    cluster = "v001",
    age = "hw1",
    fever = "h22",
    malaria_dx = "h47"
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

  valid_indicators <- "malaria_dx"
  invalid <- setdiff(indicators, valid_indicators)
  if (length(invalid) > 0) {
    cli::cli_abort("Invalid indicators: {.val {invalid}}")
  }

  # ---- Prepare data using shared helpers ----

  gps_clean <- .prepare_gps_data(gps_data, gps_vars)

  kr_fever <- tryCatch(
    .prepare_malaria_dx_data(
      dhs_kr = dhs_kr,
      survey_vars = survey_vars,
      include_survey_vars = FALSE
    ),
    error = function(e) {
      cli::cli_alert_warning(conditionMessage(e))
      return(NULL)
    }
  )

  if (is.null(kr_fever)) return(list())

  if (all(is.na(kr_fever$had_test))) {
    cli::cli_alert_warning(
      "Malaria diagnosis variable {.var {survey_vars$malaria_dx}} is all NA"
    )
    return(list())
  }

  # ---- Calculate cluster-level indicators ----

  results <- list()

  if ("malaria_dx" %in% indicators) {
    dx_data <- kr_fever |>
      dplyr::filter(!is.na(had_test)) |>
      dplyr::mutate(dx_binary = as.integer(had_test == 1))

    dt <- .aggregate_to_mbg_clusters(
      individual_data = dx_data,
      indicator_col = "dx_binary",
      gps_clean = gps_clean,
      result_name = "malaria_dx"
    )

    if (!is.null(dt)) {
      results[["malaria_dx"]] <- dt
    }
  }

  if (length(results) == 0) {
    cli::cli_alert_warning("No valid malaria_dx MBG data could be prepared")
  }

  results
}


#' Prepare Single Malaria Dx Indicator for MBG
#'
#' Convenience wrapper around [calc_malaria_dx_mbg()] to prepare a single
#' malaria diagnostic testing indicator for MBG analysis.
#'
#' @inheritParams calc_malaria_dx_mbg
#' @param indicator Single indicator name. Default: "malaria_dx".
#'
#' @return A data.table with columns: cluster_id, indicator, samplesize, x, y
#' @export
prep_malaria_dx_mbg <- function(
  dhs_kr,
  gps_data,
  indicator = "malaria_dx",
  survey_vars = list(
    cluster = "v001",
    age = "hw1",
    fever = "h22",
    malaria_dx = "h47"
  ),
  gps_vars = list(
    cluster = "DHSCLUST",
    lat = "LATNUM",
    lon = "LONGNUM"
  )
) {
  result <- calc_malaria_dx_mbg(
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
