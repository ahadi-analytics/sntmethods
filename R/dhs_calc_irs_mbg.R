#' Prepare IRS Data for MBG Analysis
#'
#' Prepares cluster-level Indoor Residual Spraying (IRS) coverage data for
#' Model-Based Geostatistics (MBG) analysis. Calculates the proportion of
#' households sprayed in the last 12 months.
#'
#' @param dhs_hr DHS Household Records dataset.
#' @param gps_data DHS GPS dataset with cluster coordinates.
#' @param survey_vars Named list mapping DHS variable names.
#' @param gps_vars Named list for GPS variable mapping.
#'
#' @return A data.table with columns:
#'   \itemize{
#'     \item cluster_id: Cluster identifier
#'     \item indicator: Number of households sprayed
#'     \item samplesize: Total number of households
#'     \item x: Longitude
#'     \item y: Latitude
#'   }
#'
#' @details
#' IRS coverage is measured using variable hv253 (household sprayed in last
#' 12 months). This is a household-level indicator.
#'
#' @examples
#' \dontrun{
#' irs_mbg <- calc_irs_mbg(
#'   dhs_hr = hr_data,
#'   gps_data = gps_data
#' )
#' }
#'
#' @export
calc_irs_mbg <- function(
  dhs_hr,
  gps_data,
  survey_vars = list(
    cluster = "hv001",
    irs = "hv253"
  ),
  gps_vars = list(
    cluster = "DHSCLUST",
    lat = "LATNUM",
    lon = "LONGNUM"
  )
) {
  # ---- Input validation ----

  if (!is.data.frame(dhs_hr)) {
    cli::cli_abort("`dhs_hr` must be a data.frame or tibble")
  }

  if (!is.data.frame(gps_data)) {
    cli::cli_abort("`gps_data` must be a data.frame or tibble")
  }

  # Check IRS variable exists
  if (!survey_vars$irs %in% names(dhs_hr)) {
    cli::cli_abort(
      c(
        "IRS variable {.var {survey_vars$irs}} not found in HR data",
        "i" = "IRS coverage data may not be available for this survey"
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

  # ---- Prepare HR data ----

  hr <- dhs_hr |>
    dplyr::mutate(dplyr::across(dplyr::everything(), haven::zap_labels)) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), as.vector)) |>
    dplyr::transmute(
      cluster_id = .data[[survey_vars$cluster]],
      irs_sprayed = .data[[survey_vars$irs]]
    ) |>
    dplyr::filter(!is.na(irs_sprayed)) |>
    dplyr::mutate(
      # hv253: 0 = No, 1 = Yes
      sprayed = as.integer(irs_sprayed == 1)
    )

  cli::cli_alert_info(
    "HR data: {format(nrow(hr), big.mark = ',')} households with valid IRS data"
  )

  # ---- Aggregate to cluster level ----

  irs_cluster <- hr |>
    dplyr::group_by(cluster_id) |>
    dplyr::summarise(
      indicator = sum(sprayed, na.rm = TRUE),
      samplesize = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::inner_join(gps_clean, by = "cluster_id") |>
    dplyr::filter(samplesize > 0)

  cli::cli_alert_success(
    "IRS coverage: {nrow(irs_cluster)} clusters, ",
    "{sum(irs_cluster$indicator)} sprayed / {sum(irs_cluster$samplesize)} households"
  )

  data.table::as.data.table(irs_cluster)
}


#' Prepare IRS Data for MBG (Alias)
#'
#' Alias for calc_irs_mbg for consistent naming.
#'
#' @inheritParams calc_irs_mbg
#' @return A data.table with columns: cluster_id, indicator, samplesize, x, y
#' @export
prep_irs_mbg <- calc_irs_mbg
