#' Prepare GPS Data for MBG Analysis
#'
#' Shared GPS cleaning used by all MBG functions. Extracts cluster coordinates,
#' filters invalid values, and deduplicates.
#'
#' @param gps_data DHS GPS dataset with cluster coordinates.
#' @param gps_vars Named list for GPS variable mapping with keys:
#'   cluster, lat, lon.
#'
#' @return A tibble with columns: cluster_id, x (longitude), y (latitude).
#'
#' @noRd
.prepare_gps_data <- function(
  gps_data,
  gps_vars = list(
    cluster = "DHSCLUST",
    lat = "LATNUM",
    lon = "LONGNUM"
  )
) {
  if (!is.data.frame(gps_data)) {
    cli::cli_abort("`gps_data` must be a data.frame or tibble")
  }

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

  gps_clean
}


#' Aggregate Individual Data to MBG Cluster Counts
#'
#' Shared cluster-level aggregation pattern used by all MBG functions.
#' Groups individual-level data by cluster, counts indicator positives and
#' total sample size, then joins GPS coordinates.
#'
#' @param individual_data Data frame with individual-level records. Must have
#'   columns `cluster_id` and the column named by `indicator_col`.
#' @param indicator_col Character. Name of the binary (0/1) indicator column
#'   to aggregate.
#' @param gps_clean Cleaned GPS data from `.prepare_gps_data()`.
#' @param result_name Character. Name for the result in log messages.
#'
#' @return A data.table with columns: cluster_id, indicator, samplesize, x, y.
#'   Returns NULL if no valid clusters could be created.
#'
#' @noRd
.aggregate_to_mbg_clusters <- function(
  individual_data,
  indicator_col,
  gps_clean,
  result_name = "indicator"
) {
  cluster_data <- individual_data |>
    dplyr::group_by(cluster_id) |>
    dplyr::summarise(
      indicator = sum(.data[[indicator_col]], na.rm = TRUE),
      samplesize = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::inner_join(gps_clean, by = "cluster_id") |>
    dplyr::filter(samplesize > 0)

  if (nrow(cluster_data) == 0) {
    cli::cli_alert_warning("{result_name}: no valid clusters")
    return(NULL)
  }

  cli::cli_alert_success("{result_name}: {nrow(cluster_data)} clusters")

  data.table::as.data.table(cluster_data)
}
