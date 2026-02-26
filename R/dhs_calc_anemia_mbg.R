#' Prepare Anemia Data for MBG Analysis
#'
#' Prepares cluster-level anemia prevalence data for MBG analysis.
#' Supports multiple severity thresholds (mild, moderate, severe) and
#' both cumulative and exclusive categories.
#'
#' @details
#' Methodology: \url{https://github.com/ahadi-analytics/sntmethods/blob/master/inst/methods/anemia_dhs.yml}
#'
#' @param dhs_pr DHS Person Records dataset.
#' @param gps_data DHS GPS dataset with cluster coordinates.
#' @param indicators Character vector of indicators to calculate:
#'   \itemize{
#'     \item Cumulative (child has hemoglobin below threshold):
#'     \itemize{
#'       \item "any": Any anemia (Hb < 11 g/dL)
#'       \item "moderate_plus": Moderate or severe (Hb < 10 g/dL)
#'       \item "severe": Severe only (Hb < 8 g/dL)
#'     }
#'     \item Exclusive (child is in exactly this category):
#'     \itemize{
#'       \item "mild_only": Mild only (10 <= Hb < 11)
#'       \item "moderate_only": Moderate only (8 <= Hb < 10)
#'       \item "severe_only": Same as severe (Hb < 8)
#'     }
#'   }
#'   Default: c("any", "moderate_plus", "severe").
#' @param age_min Minimum age in months (default: 6).
#' @param age_max Maximum age in months (default: 59).
#' @param survey_vars Named list mapping DHS variable names.
#' @param gps_vars Named list for GPS variable mapping.
#'
#' @return A list of data.tables (one per indicator), each with columns:
#'   \itemize{
#'     \item cluster_id: Cluster identifier
#'     \item indicator: Number with anemia at that threshold
#'     \item samplesize: Total number of children tested
#'     \item x: Longitude
#'     \item y: Latitude
#'   }
#'
#' @details
#' Anemia thresholds follow WHO definitions for children 6-59 months:
#' \itemize{
#'   \item Mild: 10.0-10.9 g/dL
#'   \item Moderate: 7.0-9.9 g/dL
#'   \item Severe: < 7.0 g/dL
#' }
#'
#' Note: DHS uses slightly different cutoffs. This function uses:
#' \itemize{
#'   \item Any anemia: < 11 g/dL
#'   \item Moderate+: < 10 g/dL
#'   \item Severe: < 8 g/dL
#' }
#'
#' Hemoglobin values in DHS are altitude-adjusted and stored in g/dL * 10.
#'
#' @examples
#' \dontrun{
#' anemia_mbg <- calc_anemia_mbg(
#'   dhs_pr = pr_data,
#'   gps_data = gps_data,
#'   indicators = c("any", "severe")
#' )
#' }
#'
#' @seealso [calc_severe_anemia_dhs()] for survey-weighted estimates
#' @export
calc_anemia_mbg <- function(
  dhs_pr,
  gps_data,
  indicators = c("any", "moderate_plus", "severe"),
  age_min = 6,
  age_max = 59,
  survey_vars = list(
    cluster = "hv001",
    age = "hc1",
    present = "hv103",
    mother = "hv042",
    hemoglobin = "hc56"
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
    "any", "moderate_plus", "severe",
    "mild_only", "moderate_only", "severe_only"
  )
  invalid <- setdiff(indicators, valid_indicators)
  if (length(invalid) > 0) {
    cli::cli_abort("Invalid indicators: {.val {invalid}}")
  }

  # ---- Prepare GPS data ----

  gps_clean <- .prepare_gps_data(gps_data, gps_vars)

  # ---- hc56 -> hw53 fallback ----
  if ("hc56" %in% names(dhs_pr) && all(is.na(dhs_pr[["hc56"]]))) {
    if ("hw53" %in% names(dhs_pr)) {
      cli::cli_warn("hc56 is entirely NA; falling back to hw53 for haemoglobin")
      survey_vars$hemoglobin <- "hw53"
    } else {
      cli::cli_warn("hc56 and hw53 are both absent/NA; skipping anemia")
      return(NULL)
    }
  }

  # ---- Prepare PR data ----

  pr <- .prepare_anemia_data(
    dhs_pr, survey_vars, age_min, age_max,
    include_survey_vars = FALSE
  )
  if (is.null(pr)) return(NULL)

  # ---- Aggregate to cluster level ----

  indicator_map <- list(
    any = "has_any_anemia",
    moderate_plus = "has_moderate_plus",
    severe = "has_severe",
    mild_only = "has_mild_only",
    moderate_only = "has_moderate_only",
    severe_only = "has_severe_only"
  )

  result_names <- list(
    any = "anemia_any",
    moderate_plus = "anemia_moderate_plus",
    severe = "anemia_severe",
    mild_only = "anemia_mild_only",
    moderate_only = "anemia_moderate_only",
    severe_only = "anemia_severe_only"
  )

  results <- list()

  for (ind in indicators) {
    cluster_dt <- .aggregate_to_mbg_clusters(
      pr, indicator_map[[ind]], gps_clean, result_names[[ind]]
    )
    if (!is.null(cluster_dt)) {
      results[[result_names[[ind]]]] <- cluster_dt
    }
  }

  if (length(results) == 0) return(NULL)

  results
}


#' Prepare Single Anemia Indicator for MBG
#'
#' @inheritParams calc_anemia_mbg
#' @param indicator Single indicator name. Default: "any".
#'
#' @return A data.table with columns: cluster_id, indicator, samplesize, x, y
#' @export
prep_anemia_mbg <- function(
  dhs_pr,
  gps_data,
  indicator = "any",
  age_min = 6,
  age_max = 59,
  survey_vars = list(
    cluster = "hv001",
    age = "hc1",
    present = "hv103",
    mother = "hv042",
    hemoglobin = "hc56"
  ),
  gps_vars = list(
    cluster = "DHSCLUST",
    lat = "LATNUM",
    lon = "LONGNUM"
  )
) {
  result <- calc_anemia_mbg(
    dhs_pr = dhs_pr,
    gps_data = gps_data,
    indicators = indicator,
    age_min = age_min,
    age_max = age_max,
    survey_vars = survey_vars,
    gps_vars = gps_vars
  )

  result[[1]]
}
