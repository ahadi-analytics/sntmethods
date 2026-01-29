#' Prepare ANC Data for MBG Analysis
#'
#' Prepares cluster-level Antenatal Care (ANC) attendance data for Model-Based
#' Geostatistics (MBG) analysis. Calculates the proportion of women who had
#' at least N ANC visits during their most recent pregnancy.
#'
#' @param dhs_ir DHS Individual Recode dataset.
#' @param gps_data DHS GPS dataset with cluster coordinates.
#' @param indicators Character vector of indicators to calculate:
#'   \itemize{
#'     \item "anc1": At least 1 ANC visit
#'     \item "anc4": At least 4 ANC visits
#'     \item "anc8": At least 8 ANC visits (2016 WHO recommendation)
#'   }
#'   Default: c("anc1", "anc4").
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
#'   indicators = c("anc1", "anc4")
#' )
#' }
#'
#' @export
calc_anc_mbg <- function(
  dhs_ir,
  gps_data,
  indicators = c("anc1", "anc4"),
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

  if (!is.data.frame(dhs_ir)) {
    cli::cli_abort("`dhs_ir` must be a data.frame or tibble")
  }

  if (!is.data.frame(gps_data)) {
    cli::cli_abort("`gps_data` must be a data.frame or tibble")
  }

  valid_indicators <- c("anc1", "anc4", "anc8")
  invalid <- setdiff(indicators, valid_indicators)
  if (length(invalid) > 0) {
    cli::cli_abort("Invalid indicators: {.val {invalid}}")
  }

  if (birth_window_months < 1 || birth_window_months > 60) {
    cli::cli_abort("`birth_window_months` must be between 1 and 60")
  }

  # Check required columns
  required_cols <- unlist(survey_vars)
  missing_cols <- setdiff(required_cols, names(dhs_ir))

  if (length(missing_cols) > 0) {
    cli::cli_abort("Required columns not found: {.var {missing_cols}}")
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

  # ---- Prepare IR data ----

  ir <- dhs_ir |>
    dplyr::mutate(dplyr::across(dplyr::everything(), haven::zap_labels)) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), as.vector)) |>
    dplyr::transmute(
      cluster_id = .data[[survey_vars$cluster]],
      interview_cmc = .data[[survey_vars$interview_date]],
      birth_cmc = .data[[survey_vars$birth_date]],
      anc_visits = .data[[survey_vars$anc_visits]]
    ) |>
    dplyr::filter(
      !is.na(birth_cmc),
      !is.na(interview_cmc)
    ) |>
    dplyr::mutate(
      months_since_birth = interview_cmc - birth_cmc
    ) |>
    dplyr::filter(
      months_since_birth >= 0,
      months_since_birth <= birth_window_months
    )

  # Filter valid ANC responses (not missing, not "don't know")
  # In DHS, 98 typically means "don't know"
  ir <- ir |>
    dplyr::filter(
      !is.na(anc_visits),
      anc_visits < 98
    )

  if (nrow(ir) == 0) {
    cli::cli_abort("No eligible women with valid ANC data found")
  }

  cli::cli_alert_info(
    "IR data: {format(nrow(ir), big.mark = ',')} women with births in last ",
    "{birth_window_months} months"
  )

  # ---- Calculate indicators ----

  results <- list()

  if ("anc1" %in% indicators) {
    ir_anc1 <- ir |>
      dplyr::mutate(has_anc1 = as.integer(anc_visits >= 1))

    anc1_cluster <- ir_anc1 |>
      dplyr::group_by(cluster_id) |>
      dplyr::summarise(
        indicator = sum(has_anc1, na.rm = TRUE),
        samplesize = dplyr::n(),
        .groups = "drop"
      ) |>
      dplyr::inner_join(gps_clean, by = "cluster_id") |>
      dplyr::filter(samplesize > 0)

    results[["anc_1plus"]] <- data.table::as.data.table(anc1_cluster)

    cli::cli_alert_success(
      "ANC 1+: {nrow(anc1_cluster)} clusters"
    )
  }

  if ("anc4" %in% indicators) {
    ir_anc4 <- ir |>
      dplyr::mutate(has_anc4 = as.integer(anc_visits >= 4))

    anc4_cluster <- ir_anc4 |>
      dplyr::group_by(cluster_id) |>
      dplyr::summarise(
        indicator = sum(has_anc4, na.rm = TRUE),
        samplesize = dplyr::n(),
        .groups = "drop"
      ) |>
      dplyr::inner_join(gps_clean, by = "cluster_id") |>
      dplyr::filter(samplesize > 0)

    results[["anc_4plus"]] <- data.table::as.data.table(anc4_cluster)

    cli::cli_alert_success(
      "ANC 4+: {nrow(anc4_cluster)} clusters"
    )
  }

  if ("anc8" %in% indicators) {
    ir_anc8 <- ir |>
      dplyr::mutate(has_anc8 = as.integer(anc_visits >= 8))

    anc8_cluster <- ir_anc8 |>
      dplyr::group_by(cluster_id) |>
      dplyr::summarise(
        indicator = sum(has_anc8, na.rm = TRUE),
        samplesize = dplyr::n(),
        .groups = "drop"
      ) |>
      dplyr::inner_join(gps_clean, by = "cluster_id") |>
      dplyr::filter(samplesize > 0)

    results[["anc_8plus"]] <- data.table::as.data.table(anc8_cluster)

    cli::cli_alert_success(
      "ANC 8+: {nrow(anc8_cluster)} clusters"
    )
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
  indicator_name <- paste0("anc", threshold)

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
