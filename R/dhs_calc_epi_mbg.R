#' Prepare EPI (Vaccination) Data for MBG Analysis
#'
#' Prepares cluster-level vaccination coverage data for MBG analysis.
#' Calculates coverage for standard EPI vaccines plus malaria vaccine.
#'
#' @param dhs_kr DHS Children's Recode (KR) dataset.
#' @param gps_data DHS GPS dataset with cluster coordinates.
#' @param indicators Character vector of vaccines to calculate:
#'   \itemize{
#'     \item "bcg": BCG vaccine
#'     \item "dpt1", "dpt2", "dpt3": DPT doses 1-3
#'     \item "polio1", "polio2", "polio3": Polio doses 1-3
#'     \item "measles1", "measles2": Measles doses 1-2
#'     \item "vita1", "vita2": Vitamin A doses 1-2
#'     \item "malaria": Malaria vaccine (RTS,S/R21)
#'     \item "fully_vaccinated": Basic fully vaccinated
#'   }
#'   Default: c("bcg", "dpt3", "measles1").
#' @param age_min_months Minimum age in months (default: 12).
#' @param age_max_months Maximum age in months (default: 23).
#' @param survey_vars Named list mapping DHS variable names.
#' @param gps_vars Named list for GPS variable mapping.
#'
#' @return A list of data.tables (one per vaccine).
#'
#' @details
#' Standard EPI target age is 12-23 months (children who have had time to
#' complete basic vaccination schedule). Malaria vaccine (RTS,S) is measured
#' by h68 variable.
#'
#' @examples
#' \dontrun{
#' epi_mbg <- calc_epi_mbg(
#'   dhs_kr = kr_data,
#'   gps_data = gps_data,
#'   indicators = c("bcg", "dpt3", "measles1")
#' )
#' }
#'
#' @export
calc_epi_mbg <- function(
  dhs_kr,
  gps_data,
  indicators = c("bcg", "dpt3", "measles1"),
  age_min_months = 12,
  age_max_months = 23,
  survey_vars = list(
    cluster = "v001",
    age = "hw1",
    bcg = "h2",
    dpt1 = "h3", dpt2 = "h4", dpt3 = "h5",
    polio1 = "h6", polio2 = "h7", polio3 = "h8",
    measles1 = "h9", measles2 = "h9a",
    vita1 = "h33", vita2 = "h33a",
    malaria = "h68"
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

  valid_indicators <- c(
    "bcg", "dpt1", "dpt2", "dpt3",
    "polio1", "polio2", "polio3",
    "measles1", "measles2",
    "vita1", "vita2",
    "malaria", "fully_vaccinated"
  )
  invalid <- setdiff(indicators, valid_indicators)
  if (length(invalid) > 0) {
    cli::cli_abort("Invalid indicators: {.val {invalid}}")
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

  # Build selection columns
  select_cols <- c(survey_vars$cluster, survey_vars$age)

  # Add vaccine columns that exist in data
  vaccine_mapping <- list(
    bcg = survey_vars$bcg,
    dpt1 = survey_vars$dpt1, dpt2 = survey_vars$dpt2, dpt3 = survey_vars$dpt3,
    polio1 = survey_vars$polio1, polio2 = survey_vars$polio2, polio3 = survey_vars$polio3,
    measles1 = survey_vars$measles1, measles2 = survey_vars$measles2,
    vita1 = survey_vars$vita1, vita2 = survey_vars$vita2,
    malaria = survey_vars$malaria
  )

  available_vaccines <- sapply(vaccine_mapping, function(v) v %in% names(dhs_kr))
  available_vaccine_cols <- unlist(vaccine_mapping[available_vaccines])
  select_cols <- c(select_cols, available_vaccine_cols)
  select_cols <- unique(select_cols[select_cols %in% names(dhs_kr)])

  kr <- dhs_kr |>
    dplyr::select(dplyr::all_of(select_cols)) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), haven::zap_labels)) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), as.vector)) |>
    dplyr::rename(
      cluster_id = !!survey_vars$cluster,
      age_months = !!survey_vars$age
    )

  # Filter to eligible age range
  kr <- kr |>
    dplyr::filter(
      age_months >= age_min_months,
      age_months <= age_max_months
    )

  if (nrow(kr) == 0) {
    cli::cli_abort("No eligible children found in age range {age_min_months}-{age_max_months} months")
  }

  cli::cli_alert_info(
    "KR data: {format(nrow(kr), big.mark = ',')} children aged {age_min_months}-{age_max_months} months"
  )

  # ---- Helper function to check vaccination ----
  # In DHS: 1 = vaccination card, 2 = reported by mother, 3 = both
  # Values 1, 2, 3 all indicate child received vaccine
  check_vaccinated <- function(x) {
    as.integer(!is.na(x) & x %in% c(1, 2, 3))
  }

  # ---- Calculate vaccine indicators ----

  results <- list()

  for (ind in indicators) {
    if (ind == "fully_vaccinated") {
      # Fully vaccinated = BCG + DPT3 + Polio3 + Measles1
      required_vars <- c(
        survey_vars$bcg, survey_vars$dpt3,
        survey_vars$polio3, survey_vars$measles1
      )

      if (!all(required_vars %in% names(kr))) {
        cli::cli_alert_warning(
          "Cannot calculate fully_vaccinated - missing required vaccine variables"
        )
        next
      }

      kr_fv <- kr |>
        dplyr::mutate(
          has_bcg = check_vaccinated(.data[[survey_vars$bcg]]),
          has_dpt3 = check_vaccinated(.data[[survey_vars$dpt3]]),
          has_polio3 = check_vaccinated(.data[[survey_vars$polio3]]),
          has_measles1 = check_vaccinated(.data[[survey_vars$measles1]]),
          fully_vaccinated = as.integer(
            has_bcg == 1 & has_dpt3 == 1 & has_polio3 == 1 & has_measles1 == 1
          )
        )

      cluster_data <- kr_fv |>
        dplyr::group_by(cluster_id) |>
        dplyr::summarise(
          indicator = sum(fully_vaccinated, na.rm = TRUE),
          samplesize = dplyr::n(),
          .groups = "drop"
        ) |>
        dplyr::inner_join(gps_clean, by = "cluster_id") |>
        dplyr::filter(samplesize > 0)

      results[["epi_fully_vaccinated"]] <- data.table::as.data.table(cluster_data)
      cli::cli_alert_success("epi_fully_vaccinated: {nrow(cluster_data)} clusters")

    } else {
      # Single vaccine indicator
      var_name <- vaccine_mapping[[ind]]

      if (!var_name %in% names(kr)) {
        cli::cli_alert_warning(
          "Vaccine variable {.var {var_name}} for {ind} not found in data"
        )
        next
      }

      kr_vax <- kr |>
        dplyr::mutate(
          vaccinated = check_vaccinated(.data[[var_name]])
        )

      cluster_data <- kr_vax |>
        dplyr::group_by(cluster_id) |>
        dplyr::summarise(
          indicator = sum(vaccinated, na.rm = TRUE),
          samplesize = dplyr::n(),
          .groups = "drop"
        ) |>
        dplyr::inner_join(gps_clean, by = "cluster_id") |>
        dplyr::filter(samplesize > 0)

      result_name <- paste0("epi_", ind)
      results[[result_name]] <- data.table::as.data.table(cluster_data)
      cli::cli_alert_success("{result_name}: {nrow(cluster_data)} clusters")
    }
  }

  if (length(results) == 0) {
    cli::cli_abort("No valid MBG data could be prepared")
  }

  results
}


#' Prepare Single EPI Indicator for MBG
#'
#' @inheritParams calc_epi_mbg
#' @param vaccine Single vaccine name. Default: "measles1".
#'
#' @return A data.table with columns: cluster_id, indicator, samplesize, x, y
#' @export
prep_epi_mbg <- function(
  dhs_kr,
  gps_data,
  vaccine = "measles1",
  age_min_months = 12,
  age_max_months = 23,
  survey_vars = list(
    cluster = "v001",
    age = "hw1",
    bcg = "h2",
    dpt1 = "h3", dpt2 = "h4", dpt3 = "h5",
    polio1 = "h6", polio2 = "h7", polio3 = "h8",
    measles1 = "h9", measles2 = "h9a",
    vita1 = "h33", vita2 = "h33a",
    malaria = "h68"
  ),
  gps_vars = list(
    cluster = "DHSCLUST",
    lat = "LATNUM",
    lon = "LONGNUM"
  )
) {
  result <- calc_epi_mbg(
    dhs_kr = dhs_kr,
    gps_data = gps_data,
    indicators = vaccine,
    age_min_months = age_min_months,
    age_max_months = age_max_months,
    survey_vars = survey_vars,
    gps_vars = gps_vars
  )

  result[[1]]
}
