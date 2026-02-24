#' Prepare ACT Treatment Data for MBG Analysis
#'
#' Prepares cluster-level ACT (Artemisinin-based Combination Therapy) treatment
#' data for MBG analysis. Calculates counts of febrile children under 5 who
#' received ACT treatment.
#'
#' @details
#' Methodology: \url{https://github.com/ahadi-analytics/sntmethods/blob/master/inst/methods/act_dhs.yml}
#'
#' @param dhs_kr DHS Children's Recode (KR) dataset.
#' @param gps_data DHS GPS dataset with cluster coordinates.
#' @param dhs_pr Optional DHS Person Recode (PR) dataset. Required for
#'   "febrile_rdt_pos" and "febrile_rdt_pos_act" indicators (provides hml35).
#' @param indicators Character vector of indicators to calculate:
#'   \itemize{
#'     \item "act": Received ACT among febrile children under 5
#'     \item "act_tested": Received ACT among children who tested positive
#'       (RDT or microscopy)
#'     \item "febrile_rdt_pos": RDT positivity rate among febrile U5 children
#'       (requires dhs_pr)
#'     \item "febrile_rdt_pos_act": ACT coverage among febrile RDT-positive
#'       children (requires dhs_pr)
#'   }
#'   Default: c("act", "act_tested").
#' @param survey_vars Named list mapping DHS variable names:
#'   \itemize{
#'     \item `cluster`: Cluster ID (default: "v001")
#'     \item `age`: Child's age in months (default: "hw1")
#'     \item `fever`: Fever in last 2 weeks (default: "h22")
#'     \item `act`: Received ACT (default: "ml13e")
#'     \item `test`: Filter variable for act_tested denominator (default: "ml13a").
#'       NOTE: ml13a is chloroquine in standard DHS; verify meaning per survey.
#'   }
#' @param gps_vars Named list for GPS variable mapping.
#'
#' @return A named list of data.tables (one per indicator), each with columns:
#'   \itemize{
#'     \item cluster_id: Cluster identifier
#'     \item indicator: Numerator count
#'     \item samplesize: Denominator count
#'     \item x: Longitude
#'     \item y: Latitude
#'   }
#'
#' @examples
#' \dontrun{
#' act_mbg <- calc_act_mbg(
#'   dhs_kr = kr_data,
#'   gps_data = gps_data,
#'   indicators = c("act", "act_tested")
#' )
#' }
#'
#' @seealso [calc_act_dhs()] for survey-weighted estimates,
#'   [calc_csb_mbg()] for care-seeking behavior
#' @export
calc_act_mbg <- function(
  dhs_kr,
  gps_data,
  dhs_pr = NULL,
  indicators = c("act", "act_tested"),
  survey_vars = list(
    cluster = "v001",
    age = "hw1",
    fever = "h22",
    act = "ml13e",
    test = "ml13a"
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

  valid_indicators <- c("act", "act_tested", "febrile_rdt_pos", "febrile_rdt_pos_act")
  invalid <- setdiff(indicators, valid_indicators)
  if (length(invalid) > 0) {
    cli::cli_abort("Invalid indicators: {.val {invalid}}")
  }

  pr_required <- intersect(indicators, c("febrile_rdt_pos", "febrile_rdt_pos_act"))
  if (length(pr_required) > 0 && is.null(dhs_pr)) {
    cli::cli_alert_warning(
      "Indicators {.val {pr_required}} require `dhs_pr` - these will be skipped"
    )
  }

  # ---- Prepare data using shared helpers ----

  gps_clean <- .prepare_gps_data(gps_data, gps_vars)

  kr_fever <- tryCatch(
    .prepare_act_data(
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

  # Check if ACT data is all NA
  if (all(is.na(kr_fever$received_act))) {
    cli::cli_alert_warning(
      "ACT variable {.var {survey_vars$act}} is all NA - no ACT data available"
    )
    return(list())
  }

  # ---- Calculate cluster-level indicators ----

  results <- list()
  has_test_var <- survey_vars$test %in% names(dhs_kr)

  if ("act" %in% indicators) {
    act_data <- kr_fever |>
      dplyr::filter(!is.na(received_act)) |>
      dplyr::mutate(act_binary = as.integer(received_act == 1))

    dt <- .aggregate_to_mbg_clusters(
      individual_data = act_data,
      indicator_col = "act_binary",
      gps_clean = gps_clean,
      result_name = "act"
    )

    if (!is.null(dt)) {
      results[["act"]] <- dt
    }
  }

  if ("act_tested" %in% indicators) {
    if (!has_test_var) {
      cli::cli_alert_warning(
        "Test variable {.var {survey_vars$test}} not found - skipping act_tested"
      )
    } else if (all(is.na(kr_fever$test_positive))) {
      cli::cli_alert_warning(
        "Test variable {.var {survey_vars$test}} is all NA - skipping act_tested"
      )
    } else {
      tested_data <- kr_fever |>
        dplyr::filter(test_positive == 1, !is.na(received_act)) |>
        dplyr::mutate(act_binary = as.integer(received_act == 1))

      dt <- .aggregate_to_mbg_clusters(
        individual_data = tested_data,
        indicator_col = "act_binary",
        gps_clean = gps_clean,
        result_name = "act_tested"
      )

      if (!is.null(dt)) {
        results[["act_tested"]] <- dt
      }
    }
  }

  # ---- Febrile RDT indicators (require dhs_pr) ----

  if (!is.null(dhs_pr) && length(pr_required) > 0) {
    kr_merged <- .merge_kr_pr_febrile(kr_fever = kr_fever, dhs_pr = dhs_pr)

    if (!is.null(kr_merged)) {
      if ("febrile_rdt_pos" %in% indicators) {
        dt <- .aggregate_to_mbg_clusters(
          individual_data = kr_merged,
          indicator_col   = "has_rdt_pos",
          gps_clean       = gps_clean,
          result_name     = "febrile_rdt_pos"
        )
        if (!is.null(dt)) results[["febrile_rdt_pos"]] <- dt
      }

      if ("febrile_rdt_pos_act" %in% indicators) {
        rdt_pos_data <- kr_merged |>
          dplyr::filter(has_rdt_pos == 1, !is.na(has_act))

        if (nrow(rdt_pos_data) > 0) {
          dt <- .aggregate_to_mbg_clusters(
            individual_data = rdt_pos_data,
            indicator_col   = "has_act",
            gps_clean       = gps_clean,
            result_name     = "febrile_rdt_pos_act"
          )
          if (!is.null(dt)) results[["febrile_rdt_pos_act"]] <- dt
        } else {
          cli::cli_alert_warning(
            "No RDT-positive febrile children found for febrile_rdt_pos_act"
          )
        }
      }
    }
  }

  if (length(results) == 0) {
    cli::cli_alert_warning("No valid ACT MBG data could be prepared")
  }

  results
}


#' Prepare Single ACT Indicator for MBG
#'
#' Convenience wrapper around [calc_act_mbg()] to prepare a single ACT
#' indicator for MBG analysis.
#'
#' @inheritParams calc_act_mbg
#' @param indicator Single indicator name. Default: "act".
#'
#' @return A data.table with columns: cluster_id, indicator, samplesize, x, y
#' @export
prep_act_mbg <- function(
  dhs_kr,
  gps_data,
  indicator = "act",
  survey_vars = list(
    cluster = "v001",
    age = "hw1",
    fever = "h22",
    act = "ml13e",
    test = "ml13a"
  ),
  gps_vars = list(
    cluster = "DHSCLUST",
    lat = "LATNUM",
    lon = "LONGNUM"
  )
) {
  result <- calc_act_mbg(
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
