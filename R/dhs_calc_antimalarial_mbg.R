#' Prepare Antimalarial Treatment Data for MBG Analysis
#'
#' Prepares cluster-level antimalarial treatment data for MBG analysis.
#' Calculates counts of febrile children under 5 who received any antimalarial
#' drug, at each survey cluster.
#'
#' @details
#' Methodology: \url{https://github.com/ahadi-analytics/sntmethods/blob/master/inst/methods/antimalarial_dhs.yml}
#'
#' @param dhs_kr DHS Children's Recode (KR) dataset.
#' @param gps_data DHS GPS dataset with cluster coordinates.
#' @param indicators Character vector of indicators to calculate:
#'   \itemize{
#'     \item "antimalarial": Received any antimalarial among febrile U5
#'   }
#'   Default: "antimalarial".
#' @param survey_vars Named list mapping DHS variable names:
#'   \itemize{
#'     \item `cluster`: Cluster ID (default: "v001")
#'     \item `age`: Child's age in months (default: "hw1")
#'     \item `fever`: Fever in last 2 weeks (default: "h22")
#'   }
#' @param gps_vars Named list for GPS variable mapping.
#'
#' @return A named list of data.tables (one per indicator), each with columns:
#'   \itemize{
#'     \item cluster_id: Cluster identifier
#'     \item indicator: Numerator count (children receiving antimalarial)
#'     \item samplesize: Denominator count (febrile U5 children)
#'     \item x: Longitude
#'     \item y: Latitude
#'   }
#'
#' @examples
#' \dontrun{
#' am_mbg <- calc_antimalarial_mbg(
#'   dhs_kr = kr_data,
#'   gps_data = gps_data
#' )
#' }
#'
#' @seealso [calc_antimalarial_dhs_core()] for survey-weighted estimates,
#'   [calc_act_mbg()] for ACT-specific treatment
#' @export
calc_antimalarial_mbg <- function(
  dhs_kr,
  gps_data,
  indicators = "antimalarial",
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

  valid_indicators <- "antimalarial"
  invalid <- setdiff(indicators, valid_indicators)
  if (length(invalid) > 0) {
    cli::cli_abort("Invalid indicators: {.val {invalid}}")
  }

  # ---- Prepare data using shared helpers ----

  gps_clean <- .prepare_gps_data(gps_data, gps_vars)

  kr_fever <- tryCatch(
    .prepare_antimalarial_data(
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

  if (all(is.na(kr_fever$has_antimalarial))) {
    cli::cli_alert_warning("All antimalarial variables are NA")
    return(list())
  }

  # ---- Calculate cluster-level indicators ----

  results <- list()

  if ("antimalarial" %in% indicators) {
    am_data <- kr_fever |>
      dplyr::filter(!is.na(has_antimalarial)) |>
      dplyr::mutate(am_binary = as.integer(has_antimalarial == 1))

    dt <- .aggregate_to_mbg_clusters(
      individual_data = am_data,
      indicator_col = "am_binary",
      gps_clean = gps_clean,
      result_name = "antimalarial"
    )

    if (!is.null(dt)) {
      results[["antimalarial"]] <- dt
    }
  }

  if (length(results) == 0) {
    cli::cli_alert_warning("No valid antimalarial MBG data could be prepared")
  }

  results
}


#' Prepare Single Antimalarial Indicator for MBG
#'
#' Convenience wrapper around [calc_antimalarial_mbg()] to prepare antimalarial
#' treatment data for MBG analysis.
#'
#' @inheritParams calc_antimalarial_mbg
#' @param indicator Single indicator name. Default: "antimalarial".
#'
#' @return A data.table with columns: cluster_id, indicator, samplesize, x, y
#' @export
prep_antimalarial_mbg <- function(
  dhs_kr,
  gps_data,
  indicator = "antimalarial",
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
  result <- calc_antimalarial_mbg(
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
