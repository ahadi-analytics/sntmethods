#' Prepare U5MR Data for MBG Analysis
#'
#' Prepares cluster-level Under-5 Mortality Rate (U5MR) data for MBG analysis.
#' Splits birth histories into age-specific cohorts for separate modeling.
#'
#' @param dhs_br DHS Birth Recode dataset.
#' @param gps_data DHS GPS dataset with cluster coordinates.
#' @param age_groups Named list of age group boundaries in months. Default:
#'   \itemize{
#'     \item under1: c(0, 12) - Infant mortality
#'     \item age1: c(12, 24) - Child mortality age 1
#'     \item age2: c(24, 36) - Child mortality age 2
#'     \item age3: c(36, 48) - Child mortality age 3
#'     \item age4: c(48, 60) - Child mortality age 4
#'   }
#' @param retrospective_months Number of months to look back. Default: 60.
#' @param survey_vars Named list mapping DHS variable names.
#' @param gps_vars Named list for GPS variable mapping.
#'
#' @return A list of data.tables (one per age group), each with columns:
#'   \itemize{
#'     \item cluster_id: Cluster identifier
#'     \item indicator: Number of deaths in age group
#'     \item samplesize: Number of children exposed to age group
#'     \item x: Longitude
#'     \item y: Latitude
#'   }
#'
#' @details
#' U5MR is modeled as a composite of age-specific mortality risks. This
#' function prepares separate datasets for each age interval. To get U5MR:
#'
#' U5MR = 1 - (1 - q_under1) * (1 - q_age1) * ... * (1 - q_age4)
#'
#' Each age group's MBG model produces a mortality risk (q). These are
#' combined using survival probability multiplication.
#'
#' **Right-censoring:** Only children who have fully completed an age group
#' before the interview date are included. This prevents downward bias from
#' including children still at risk of dying within the interval.
#'
#' @examples
#' \dontrun{
#' u5mr_mbg <- calc_u5mr_mbg(
#'   dhs_br = br_data,
#'   gps_data = gps_data
#' )
#' }
#'
#' @seealso [calc_u5mr_dhs()] for survey-weighted estimates
#' @export
calc_u5mr_mbg <- function(
  dhs_br,
  gps_data,
  age_groups = list(
    under1 = c(0, 12),
    age1 = c(12, 24),
    age2 = c(24, 36),
    age3 = c(36, 48),
    age4 = c(48, 60)
  ),
  retrospective_months = 60,
  survey_vars = list(
    cluster = "v001",
    hhid = "v002",
    birth_index = "bidx",
    interview_date = "v008",
    birth_date = "b3",
    child_alive = "b5",
    death_age = "b7"
  ),
  gps_vars = list(
    cluster = "DHSCLUST",
    lat = "LATNUM",
    lon = "LONGNUM"
  )
) {
  # ---- Input validation ----

  if (!is.data.frame(dhs_br)) {
    cli::cli_abort("`dhs_br` must be a data.frame or tibble")
  }

  if (!is.data.frame(gps_data)) {
    cli::cli_abort("`gps_data` must be a data.frame or tibble")
  }

  # Check required columns
  required <- unlist(survey_vars)
  missing <- setdiff(required, names(dhs_br))
  if (length(missing) > 0) {
    cli::cli_abort("Required columns not found: {.var {missing}}")
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

  # ---- Prepare BR data ----

  br <- dhs_br |>
    dplyr::mutate(dplyr::across(dplyr::everything(), haven::zap_labels)) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), as.vector)) |>
    dplyr::transmute(
      cluster_id = .data[[survey_vars$cluster]],
      hhid = .data[[survey_vars$hhid]],
      birth_index = .data[[survey_vars$birth_index]],
      interview_cmc = .data[[survey_vars$interview_date]],
      birth_cmc = .data[[survey_vars$birth_date]],
      child_alive = .data[[survey_vars$child_alive]],
      death_age_months = .data[[survey_vars$death_age]]
    ) |>
    dplyr::filter(
      !is.na(birth_cmc),
      !is.na(interview_cmc)
    )

  # Calculate time difference
  br <- br |>
    dplyr::mutate(
      months_since_birth = interview_cmc - birth_cmc
    )

  cli::cli_alert_info(
    "BR data: {format(nrow(br), big.mark = ',')} birth records"
  )

  # ---- Process each age group ----

  results <- list()

  for (age_name in names(age_groups)) {
    age_range <- age_groups[[age_name]]
    age_start <- age_range[1]
    age_end <- age_range[2]

    cli::cli_alert_info(
      "Processing age group: {age_name} ({age_start}-{age_end} months)"
    )

    # Filter to children who completed this age group during analysis period
    # Criteria:
    # 1. Child must have fully exited the age group before interview (right-censoring)
    # 2. Child didn't die before reaching this age group
    # 3. Child's entry to this age group was within retrospective window

    br_age <- br |>
      dplyr::filter(
        # Child must have completed the age group before interview (right-censoring)
        # This prevents bias from including children still at risk
        months_since_birth >= age_end,
        # Child didn't die before reaching this age group
        # (child_alive == 0 means died, death_age_months is age at death)
        child_alive == 1 | (child_alive == 0 & !is.na(death_age_months) & death_age_months >= age_start),
        # Entry to this age group was within retrospective window
        months_since_birth - age_start <= retrospective_months
      ) |>
      dplyr::mutate(
        # Did child die during this age interval?
        died_in_interval = as.integer(
          child_alive == 0 &
          !is.na(death_age_months) &
          death_age_months >= age_start &
          death_age_months < age_end
        )
      )

    if (nrow(br_age) == 0) {
      cli::cli_alert_warning("No eligible children for age group {age_name}")
      next
    }

    # Aggregate to cluster level
    cluster_data <- br_age |>
      dplyr::group_by(cluster_id) |>
      dplyr::summarise(
        indicator = sum(died_in_interval, na.rm = TRUE),
        samplesize = dplyr::n(),
        .groups = "drop"
      ) |>
      dplyr::inner_join(gps_clean, by = "cluster_id") |>
      dplyr::filter(samplesize > 0)

    result_name <- paste0("u5mr_", age_name)
    results[[result_name]] <- data.table::as.data.table(cluster_data)

    n_deaths <- sum(cluster_data$indicator)
    n_exposed <- sum(cluster_data$samplesize)
    rate <- round(n_deaths / n_exposed * 1000, 1)

    cli::cli_alert_success(
      "{result_name}: {nrow(cluster_data)} clusters, ",
      "{n_deaths} deaths / {n_exposed} exposed ({rate} per 1000)"
    )
  }

  if (length(results) == 0) {
    cli::cli_abort("No valid MBG data could be prepared")
  }

  results
}


#' Combine Age-Specific U5MR Predictions into Composite U5MR
#'
#' Combines age-specific mortality rates (q) from separate MBG models
#' into composite U5MR using survival probability multiplication.
#'
#' @param q_under1 Raster or vector of infant mortality rates (q0)
#' @param q_age1 Raster or vector of age 1 mortality rates (q1)
#' @param q_age2 Raster or vector of age 2 mortality rates (q2)
#' @param q_age3 Raster or vector of age 3 mortality rates (q3)
#' @param q_age4 Raster or vector of age 4 mortality rates (q4)
#'
#' @return U5MR in same format as inputs (raster or vector)
#'
#' @details
#' Formula: U5MR = 1 - (1-q0) * (1-q1) * (1-q2) * (1-q3) * (1-q4)
#'
#' @export
combine_u5mr_components <- function(
  q_under1,
  q_age1,
  q_age2,
  q_age3,
  q_age4
) {
  # Calculate survival probabilities
  surv_under1 <- 1 - q_under1
  surv_age1 <- 1 - q_age1
  surv_age2 <- 1 - q_age2
  surv_age3 <- 1 - q_age3
  surv_age4 <- 1 - q_age4

  # Combine: U5MR = 1 - product of survival probabilities
  u5mr <- 1 - (surv_under1 * surv_age1 * surv_age2 * surv_age3 * surv_age4)

  u5mr
}


#' Prepare Single U5MR Age Group for MBG
#'
#' @inheritParams calc_u5mr_mbg
#' @param age_group Name of age group ("under1", "age1", "age2", "age3", "age4")
#'
#' @return A data.table with columns: cluster_id, indicator, samplesize, x, y
#' @export
prep_u5mr_mbg <- function(
  dhs_br,
  gps_data,
  age_group = "under1",
  retrospective_months = 60,
  survey_vars = list(
    cluster = "v001",
    hhid = "v002",
    birth_index = "bidx",
    interview_date = "v008",
    birth_date = "b3",
    child_alive = "b5",
    death_age = "b7"
  ),
  gps_vars = list(
    cluster = "DHSCLUST",
    lat = "LATNUM",
    lon = "LONGNUM"
  )
) {
  # Default age groups
  default_age_groups <- list(
    under1 = c(0, 12),
    age1 = c(12, 24),
    age2 = c(24, 36),
    age3 = c(36, 48),
    age4 = c(48, 60)
  )

  if (!age_group %in% names(default_age_groups)) {
    cli::cli_abort("Invalid age_group: {.val {age_group}}")
  }

  age_groups <- default_age_groups[age_group]

  result <- calc_u5mr_mbg(
    dhs_br = dhs_br,
    gps_data = gps_data,
    age_groups = age_groups,
    retrospective_months = retrospective_months,
    survey_vars = survey_vars,
    gps_vars = gps_vars
  )

  result[[1]]
}
