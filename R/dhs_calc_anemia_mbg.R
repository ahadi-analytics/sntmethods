#' Prepare Anemia Data for MBG Analysis
#'
#' Prepares cluster-level anemia prevalence data for MBG analysis.
#' Supports multiple severity thresholds (mild, moderate, severe) and
#' both cumulative and exclusive categories.
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

  if (!is.data.frame(dhs_pr)) {
    cli::cli_abort("`dhs_pr` must be a data.frame or tibble")
  }

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

  # Check hemoglobin variable
  if (!survey_vars$hemoglobin %in% names(dhs_pr)) {
    cli::cli_abort(
      c(
        "Hemoglobin variable {.var {survey_vars$hemoglobin}} not found",
        "i" = "Anemia data may not be available in this survey"
      )
    )
  }

  # ---- Prepare GPS data ----

  gps_clean <- gps_data |>
    dplyr::transmute(
      cluster_id = .data[[gps_vars$cluster]],
      x = as.numeric(.data[[gps_vars$lon]]),
      y = as.numeric(.data[[gps_vars$lat]])
    ) |>
    dplyr::filter(!is.na(x), !is.na(y), x != 0, y != 0) |>
    dplyr::distinct()

  cli::cli_alert_info(
    "GPS data: {nrow(gps_clean)} clusters with valid coordinates"
  )

  # ---- Prepare PR data ----

  pr <- dhs_pr |>
    dplyr::mutate(dplyr::across(dplyr::everything(), haven::zap_labels)) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), as.vector)) |>
    dplyr::transmute(
      cluster_id = .data[[survey_vars$cluster]],
      age = .data[[survey_vars$age]],
      present = .data[[survey_vars$present]],
      mother = .data[[survey_vars$mother]],
      hb_raw = .data[[survey_vars$hemoglobin]]
    ) |>
    dplyr::filter(
      present == 1,
      mother == 1,
      age >= age_min,
      age <= age_max,
      !is.na(hb_raw),
      hb_raw < 900  # Valid range (exclude not tested, refused, etc.)
    ) |>
    dplyr::mutate(
      # Convert from g/dL * 10 to g/dL
      hemoglobin = hb_raw / 10
    )

  if (nrow(pr) == 0) {
    cli::cli_abort("No eligible children with valid hemoglobin data found")
  }

  cli::cli_alert_info(
    "PR data: {format(nrow(pr), big.mark = ',')} children with Hb measurements"
  )

  # ---- Calculate anemia indicators ----

  # Define thresholds (g/dL)
  threshold_any <- 11      # Any anemia
  threshold_moderate <- 10 # Moderate or worse
  threshold_severe <- 8    # Severe

  pr <- pr |>
    dplyr::mutate(
      # Cumulative categories
      has_any_anemia = as.integer(hemoglobin < threshold_any),
      has_moderate_plus = as.integer(hemoglobin < threshold_moderate),
      has_severe = as.integer(hemoglobin < threshold_severe),

      # Exclusive categories
      has_mild_only = as.integer(
        hemoglobin >= threshold_moderate & hemoglobin < threshold_any
      ),
      has_moderate_only = as.integer(
        hemoglobin >= threshold_severe & hemoglobin < threshold_moderate
      ),
      has_severe_only = as.integer(hemoglobin < threshold_severe)
    )

  # ---- Aggregate to cluster level ----

  results <- list()

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

  for (ind in indicators) {
    var_name <- indicator_map[[ind]]
    result_name <- result_names[[ind]]

    cluster_data <- pr |>
      dplyr::group_by(cluster_id) |>
      dplyr::summarise(
        indicator = sum(.data[[var_name]], na.rm = TRUE),
        samplesize = dplyr::n(),
        .groups = "drop"
      ) |>
      dplyr::inner_join(gps_clean, by = "cluster_id") |>
      dplyr::filter(samplesize > 0)

    results[[result_name]] <- data.table::as.data.table(cluster_data)

    cli::cli_alert_success(
      "{result_name}: {nrow(cluster_data)} clusters, ",
      "{sum(cluster_data$indicator)} / {sum(cluster_data$samplesize)} anemic"
    )
  }

  if (length(results) == 0) {
    cli::cli_abort("No valid MBG data could be prepared")
  }

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
