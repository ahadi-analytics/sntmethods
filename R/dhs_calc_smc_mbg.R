#' Prepare SMC Data for MBG Analysis
#'
#' Prepares cluster-level Seasonal Malaria Chemoprevention (SMC) receipt data
#' for MBG analysis. SMC coverage among children under 5.
#'
#' @param dhs_kr DHS Children's Recode (KR) dataset.
#' @param gps_data DHS GPS dataset with cluster coordinates.
#' @param survey_vars Named list mapping DHS variable names.
#' @param gps_vars Named list for GPS variable mapping.
#'
#' @return A data.table with columns:
#'   \itemize{
#'     \item cluster_id: Cluster identifier
#'     \item indicator: Number of children who received SMC
#'     \item samplesize: Total number of children in analysis
#'     \item x: Longitude
#'     \item y: Latitude
#'   }
#'
#' @details
#' SMC variable availability varies by survey. Common variables include:
#' \itemize{
#'   \item hml43: SMC in malaria season (DHS-7+)
#'   \item ml13g: Received antimalarial for prevention
#' }
#'
#' This function first checks which SMC-related variables are available
#' and uses the most appropriate one.
#'
#' @examples
#' \dontrun{
#' smc_mbg <- calc_smc_mbg(
#'   dhs_kr = kr_data,
#'   gps_data = gps_data
#' )
#' }
#'
#' @export
calc_smc_mbg <- function(
  dhs_kr,
  gps_data,
  survey_vars = list(
    cluster = "v001",
    age = "hw1",
    smc_primary = "hml43",
    smc_alt = "ml13g"
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

  # Determine which SMC variable to use
  smc_var <- NULL

  if (survey_vars$smc_primary %in% names(dhs_kr)) {
    smc_var <- survey_vars$smc_primary
    cli::cli_alert_info("Using SMC variable: {.var {smc_var}} (primary)")
  } else if (survey_vars$smc_alt %in% names(dhs_kr)) {
    smc_var <- survey_vars$smc_alt
    cli::cli_alert_info("Using SMC variable: {.var {smc_var}} (alternative)")
  } else {
    cli::cli_abort(
      c(
        "No SMC variable found in data",
        "i" = "Checked for: {.var {survey_vars$smc_primary}}, {.var {survey_vars$smc_alt}}",
        "i" = "SMC data may not be available for this survey"
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

  # ---- Prepare KR data ----

  kr <- dhs_kr |>
    dplyr::mutate(dplyr::across(dplyr::everything(), haven::zap_labels)) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), as.vector)) |>
    dplyr::transmute(
      cluster_id = .data[[survey_vars$cluster]],
      age_months = .data[[survey_vars$age]],
      smc_receipt = .data[[smc_var]]
    )

  # Filter to U5 children
  kr <- kr |>
    dplyr::filter(
      age_months >= 0,
      age_months <= 59
    )

  # Filter valid SMC responses
  # Typically: 0=No, 1=Yes, 8/9=Don't know
  kr <- kr |>
    dplyr::filter(
      !is.na(smc_receipt),
      smc_receipt %in% c(0, 1)
    ) |>
    dplyr::mutate(
      received_smc = as.integer(smc_receipt == 1)
    )

  if (nrow(kr) == 0) {
    cli::cli_abort("No eligible children with valid SMC data found")
  }

  cli::cli_alert_info(
    "KR data: {format(nrow(kr), big.mark = ',')} children with SMC data"
  )

  # ---- Aggregate to cluster level ----

  smc_cluster <- kr |>
    dplyr::group_by(cluster_id) |>
    dplyr::summarise(
      indicator = sum(received_smc, na.rm = TRUE),
      samplesize = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::inner_join(gps_clean, by = "cluster_id") |>
    dplyr::filter(samplesize > 0)

  cli::cli_alert_success(
    "SMC coverage: {nrow(smc_cluster)} clusters, ",
    "{sum(smc_cluster$indicator)} received / {sum(smc_cluster$samplesize)} children"
  )

  data.table::as.data.table(smc_cluster)
}


#' Prepare SMC Data for MBG (Alias)
#'
#' @inheritParams calc_smc_mbg
#' @return A data.table with columns: cluster_id, indicator, samplesize, x, y
#' @export
prep_smc_mbg <- calc_smc_mbg
