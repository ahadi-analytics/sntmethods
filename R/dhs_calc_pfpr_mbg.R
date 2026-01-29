#' Prepare PfPR Data for MBG Analysis
#'
#' Prepares cluster-level malaria parasite prevalence data for Model-Based
#' Geostatistics (MBG) analysis. Aggregates individual test results to cluster
#' counts WITHOUT survey weights - MBG handles spatial smoothing internally.
#'
#' @param dhs_pr DHS Person Records dataset (data.frame or tibble).
#' @param gps_data DHS GPS dataset with cluster coordinates.
#' @param test_type Character. Type of test: "rdt", "mic", or "both" (default).
#' @param age_groups Named list of age ranges (in months) to calculate. Each
#'   element should be a length-2 vector c(min, max). Default includes:
#'   \itemize{
#'     \item u5: c(6, 59)
#'     \item 5_9: c(60, 119)
#'     \item u10: c(6, 119)
#'     \item 2_10: c(24, 119)
#'   }
#' @param survey_vars Named list mapping DHS variable names. Required keys:
#'   \itemize{
#'     \item cluster: Cluster ID (default: "hv001")
#'     \item age: Age in months (default: "hc1")
#'     \item present: Present in household (default: "hv103")
#'     \item mother: Mother listed in household (default: "hv042")
#'     \item rdt: RDT result variable (default: "hml35")
#'     \item mic: Microscopy result variable (default: "hml32")
#'   }
#' @param gps_vars Named list for GPS variable mapping.
#'
#' @return A list of data.tables (one per age group + test type combination),
#'   each with columns:
#'   \itemize{
#'     \item cluster_id: Cluster identifier
#'     \item indicator: Number of positive tests (numerator)
#'     \item samplesize: Number of children tested (denominator)
#'     \item x: Longitude
#'     \item y: Latitude
#'   }
#'
#' @details
#' This function prepares data for MBG spatial modeling. Unlike the survey-
#' weighted `calc_pfpr_dhs()` function, this uses simple cluster-level counts
#' because MBG handles spatial smoothing and uncertainty internally.
#'
#' The output format matches the expected input for `mbg::MbgModelRunner`:
#' `data.table(cluster_id, indicator, samplesize, x, y)`
#'
#' @examples
#' \dontrun{
#' pfpr_mbg <- calc_pfpr_mbg(
#'   dhs_pr = pr_data,
#'   gps_data = gps_data,
#'   test_type = "rdt",
#'   age_groups = list(u5 = c(6, 59))
#' )
#' }
#'
#' @seealso [calc_pfpr_dhs()] for survey-weighted estimates
#' @export
calc_pfpr_mbg <- function(
  dhs_pr,
  gps_data,
  test_type = "both",
  age_groups = list(
    u5 = c(6, 59),
    `5_9` = c(60, 119),
    u10 = c(6, 119),
    `2_10` = c(24, 119)
  ),
  survey_vars = list(
    cluster = "hv001",
    age = "hc1",
    present = "hv103",
    mother = "hv042",
    rdt = "hml35",
    mic = "hml32"
  ),
  gps_vars = list(
    cluster = "DHSCLUST",
    lat = "LATNUM",
    lon = "LONGNUM"
  )
) {
  # Check for required spatial packages
  .check_spatial_pkg("mbg", "calc_pfpr_mbg")

  # ---- Input validation ----

  if (!is.data.frame(dhs_pr)) {
    cli::cli_abort("`dhs_pr` must be a data.frame or tibble")
  }

  if (!is.data.frame(gps_data)) {
    cli::cli_abort("`gps_data` must be a data.frame or tibble")
  }

  if (nrow(dhs_pr) == 0) {
    cli::cli_abort("`dhs_pr` is empty")
  }

  if (nrow(gps_data) == 0) {
    cli::cli_abort("`gps_data` is empty")
  }

  test_type <- match.arg(test_type, c("rdt", "mic", "both"))

  # Check required columns
  required_cols <- c(
    survey_vars$cluster,
    survey_vars$age,
    survey_vars$present,
    survey_vars$mother
  )

  if (test_type %in% c("rdt", "both")) {
    required_cols <- c(required_cols, survey_vars$rdt)
  }

  if (test_type %in% c("mic", "both")) {
    required_cols <- c(required_cols, survey_vars$mic)
  }

  missing_cols <- setdiff(required_cols, names(dhs_pr))

  if (length(missing_cols) > 0) {
    cli::cli_abort("Required columns not found: {.var {missing_cols}}")
  }

  # Check GPS columns
  gps_cols <- c(gps_vars$cluster, gps_vars$lat, gps_vars$lon)
  missing_gps <- setdiff(gps_cols, names(gps_data))

  if (length(missing_gps) > 0) {
    cli::cli_abort("GPS columns not found: {.var {missing_gps}}")
  }

  # ---- Prepare GPS data ----

  gps_clean <- gps_data |>
    dplyr::transmute(
      cluster_id = .data[[gps_vars$cluster]],
      x = as.numeric(.data[[gps_vars$lon]]),
      y = as.numeric(.data[[gps_vars$lat]])
    ) |>
    dplyr::filter(
      !is.na(x),
      !is.na(y),
      x != 0,
      y != 0
    ) |>
    dplyr::distinct()

  cli::cli_alert_info(
    "GPS data: {nrow(gps_clean)} clusters with valid coordinates"
  )

  # ---- Prepare PR data ----

  pr <- dhs_pr |>
    dplyr::mutate(
      dplyr::across(dplyr::everything(), haven::zap_labels)
    ) |>
    dplyr::mutate(
      dplyr::across(dplyr::everything(), as.vector)
    ) |>
    dplyr::transmute(
      cluster_id = .data[[survey_vars$cluster]],
      age = .data[[survey_vars$age]],
      present = .data[[survey_vars$present]],
      mother = .data[[survey_vars$mother]],
      rdt_res = if (survey_vars$rdt %in% names(dhs_pr)) {
        .data[[survey_vars$rdt]]
      } else {
        NA_real_
      },
      mic_res = if (survey_vars$mic %in% names(dhs_pr)) {
        .data[[survey_vars$mic]]
      } else {
        NA_real_
      }
    )

  # ---- Process each age group and test type ----

  results <- list()

  for (age_name in names(age_groups)) {
    age_range <- age_groups[[age_name]]
    age_min <- age_range[1]
    age_max <- age_range[2]

    # Filter to eligible children for this age group
    pr_age <- pr |>
      dplyr::filter(
        present == 1,
        mother == 1,
        age >= age_min,
        age <= age_max
      )

    if (nrow(pr_age) == 0) {
      cli::cli_alert_warning(
        "No eligible children for age group {age_name} ({age_min}-{age_max} months)"
      )
      next
    }

    cli::cli_alert_info(
      "Age group {age_name}: {format(nrow(pr_age), big.mark = ',')} eligible children"
    )

    # RDT results
    if (test_type %in% c("rdt", "both")) {
      pr_rdt <- pr_age |>
        dplyr::filter(rdt_res %in% c(0, 1)) |>
        dplyr::mutate(
          positive = as.integer(rdt_res == 1)
        )

      if (nrow(pr_rdt) > 0) {
        result_name <- paste0("pfpr_rdt_", age_name)

        rdt_cluster <- pr_rdt |>
          dplyr::group_by(cluster_id) |>
          dplyr::summarise(
            indicator = sum(positive, na.rm = TRUE),
            samplesize = dplyr::n(),
            .groups = "drop"
          ) |>
          dplyr::inner_join(gps_clean, by = "cluster_id") |>
          dplyr::filter(samplesize > 0)

        if (nrow(rdt_cluster) > 0) {
          results[[result_name]] <- data.table::as.data.table(rdt_cluster)

          cli::cli_alert_success(
            "{result_name}: {nrow(rdt_cluster)} clusters, ",
            "{sum(rdt_cluster$indicator)} positive / {sum(rdt_cluster$samplesize)} tested"
          )
        }
      }
    }

    # Microscopy results
    if (test_type %in% c("mic", "both")) {
      # mic_res: 0=neg, 1=Pf positive, 6=other species
      pr_mic <- pr_age |>
        dplyr::filter(mic_res %in% c(0, 1, 6)) |>
        dplyr::mutate(
          positive = as.integer(mic_res == 1)  # Only Pf counts as positive
        )

      if (nrow(pr_mic) > 0) {
        result_name <- paste0("pfpr_mic_", age_name)

        mic_cluster <- pr_mic |>
          dplyr::group_by(cluster_id) |>
          dplyr::summarise(
            indicator = sum(positive, na.rm = TRUE),
            samplesize = dplyr::n(),
            .groups = "drop"
          ) |>
          dplyr::inner_join(gps_clean, by = "cluster_id") |>
          dplyr::filter(samplesize > 0)

        if (nrow(mic_cluster) > 0) {
          results[[result_name]] <- data.table::as.data.table(mic_cluster)

          cli::cli_alert_success(
            "{result_name}: {nrow(mic_cluster)} clusters, ",
            "{sum(mic_cluster$indicator)} positive / {sum(mic_cluster$samplesize)} tested"
          )
        }
      }
    }
  }

  if (length(results) == 0) {
    cli::cli_abort("No valid MBG data could be prepared from the input data")
  }

  results
}


#' Prepare Single PfPR Indicator for MBG
#'
#' Simplified function to prepare a single PfPR indicator for MBG. Returns
#' a single data.table rather than a list.
#'
#' @inheritParams calc_pfpr_mbg
#' @param age_min Minimum age in months (inclusive). Default: 6.
#' @param age_max Maximum age in months (inclusive). Default: 59.
#'
#' @return A data.table with columns: cluster_id, indicator, samplesize, x, y
#'
#' @export
prep_pfpr_mbg <- function(
  dhs_pr,
  gps_data,
  test_type = "rdt",
  age_min = 6,

  age_max = 59,
  survey_vars = list(
    cluster = "hv001",
    age = "hc1",
    present = "hv103",
    mother = "hv042",
    rdt = "hml35",
    mic = "hml32"
  ),
  gps_vars = list(
    cluster = "DHSCLUST",
    lat = "LATNUM",
    lon = "LONGNUM"
  )
) {
  test_type <- match.arg(test_type, c("rdt", "mic"))

  age_label <- paste0(age_min, "_", age_max)

  result <- calc_pfpr_mbg(
    dhs_pr = dhs_pr,
    gps_data = gps_data,
    test_type = test_type,
    age_groups = stats::setNames(list(c(age_min, age_max)), age_label),
    survey_vars = survey_vars,
    gps_vars = gps_vars
  )

  # Return the single result
  result[[1]]
}
