#' Prepare U5MR Data for MBG Analysis
#'
#' Prepares cluster-level Under-5 Mortality Rate (U5MR) data for MBG analysis.
#' Splits birth histories into age-specific cohorts for separate modeling,
#' then combines into composite U5MR.
#'
#' @details
#' Methodology: \url{https://github.com/ahadi-analytics/sntmethods/blob/master/inst/methods/u5mr_dhs.yml}
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
#' @return A list with one data.table named "u5mr" containing:
#'   \itemize{
#'     \item cluster_id: Cluster identifier
#'     \item indicator: Number of deaths (sum across age groups)
#'     \item samplesize: Number of children exposed (from under1 group)
#'     \item x: Longitude
#'     \item y: Latitude
#'     \item u5mr: Combined U5MR per 1,000 live births
#'   }
#'
#' @details
#' **Important:** The `indicator` and `samplesize` columns are used by MBG to model
#' the death proportion (indicator/samplesize). The MBG pipeline automatically
#' converts model outputs to "per 1,000" units to match epidemiological standards
#' and the scale of the `u5mr` column.
#'
#' @details
#' U5MR is modeled as a composite of age-specific mortality risks:
#'
#' U5MR = 1 - (1 - q_under1) * (1 - q_age1) * (1 - q_age2) * (1 - q_age3) * (1 - q_age4)
#'
#' This function:
#' 1. Splits birth histories into 5 age intervals
#' 2. Calculates deaths/exposed for each interval at cluster level
#' 3. Computes age-specific mortality rates (q)
#' 4. Combines into composite U5MR using survival probability multiplication
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
#' # Access combined U5MR
#' u5mr_mbg$u5mr
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
    cli::cli_warn(
      "U5MR column(s) not found in BR data: {.var {missing}}; U5MR not available for this survey"
    )
    return(NULL)
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

  # Calculate time since birth
  br <- br |>
    dplyr::mutate(
      months_since_birth = interview_cmc - birth_cmc
    )

  cli::cli_alert_info(
    "BR data: {format(nrow(br), big.mark = ',')} birth records"
  )

  # ---- Process each age group ----

  age_specific_results <- list()

  for (age_name in names(age_groups)) {
    age_range <- age_groups[[age_name]]
    age_start <- age_range[1]
    age_end <- age_range[2]

    # Filter to children who completed this age group during analysis period
    # Criteria:
    # 1. Child must have fully exited the age group before interview (right-censoring)
    # 2. Child didn't die before reaching this age group
    # 3. Child's entry to this age group was within retrospective window

    br_age <- br |>
      dplyr::filter(
        # Child must have completed the age group before interview (right-censoring)
        months_since_birth >= age_end,
        # Child didn't die before reaching this age group
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
        deaths = sum(died_in_interval, na.rm = TRUE),
        exposed = dplyr::n(),
        .groups = "drop"
      ) |>
      dplyr::mutate(
        q = deaths / exposed  # Age-specific mortality rate
      )

    age_specific_results[[age_name]] <- cluster_data

    n_deaths <- sum(cluster_data$deaths)
    n_exposed <- sum(cluster_data$exposed)
    rate <- round(n_deaths / n_exposed * 1000, 1)

    cli::cli_alert_info(
      "{age_name}: {n_deaths} deaths / {n_exposed} exposed ({rate} per 1000)"
    )
  }

  if (length(age_specific_results) == 0) {
    cli::cli_warn("No valid U5MR data for any age group; U5MR not available for this survey")
    return(NULL)
  }

  # ---- Combine age groups into U5MR ----

  # Start with under1 as base (has most children)
  combined <- age_specific_results[["under1"]] |>
    dplyr::select(cluster_id, q_under1 = q, deaths_under1 = deaths, exposed_under1 = exposed)

  # Join other age groups
  for (age_name in c("age1", "age2", "age3", "age4")) {
    if (age_name %in% names(age_specific_results)) {
      age_data <- age_specific_results[[age_name]] |>
        dplyr::select(
          cluster_id,
          !!paste0("q_", age_name) := q,
          !!paste0("deaths_", age_name) := deaths
        )
      combined <- combined |>
        dplyr::left_join(age_data, by = "cluster_id")
    }
  }

  # Fill missing q values with 0 (no deaths in that age group for that cluster)
  combined <- combined |>
    dplyr::mutate(
      dplyr::across(dplyr::starts_with("q_"), ~ dplyr::if_else(is.na(.), 0, .)),
      dplyr::across(dplyr::starts_with("deaths_"), ~ dplyr::if_else(is.na(.), 0L, as.integer(.)))
    )

  # Calculate composite U5MR
  # U5MR = 1 - (1-q0)*(1-q1)*(1-q2)*(1-q3)*(1-q4)
  # Multiply by 1000 to convert to "per 1,000 live births" (standard epidemiological unit)
  combined <- combined |>
    dplyr::mutate(
      u5mr_raw = (1 - (1 - q_under1) * (1 - q_age1) * (1 - q_age2) * (1 - q_age3) * (1 - q_age4)) * 1000,
      # Total deaths across all age groups
      total_deaths = deaths_under1 + deaths_age1 + deaths_age2 + deaths_age3 + deaths_age4
    )

  # Join GPS coordinates
  combined <- combined |>
    dplyr::inner_join(gps_clean, by = "cluster_id")

  # Create final output in MBG format
  u5mr_output <- combined |>
    dplyr::transmute(
      cluster_id,
      indicator = total_deaths,
      samplesize = exposed_under1,  # Use under1 exposed as denominator
      x,
      y,
      u5mr = u5mr_raw
    )

  n_clusters <- nrow(u5mr_output)
  total_deaths <- sum(u5mr_output$indicator)
  total_exposed <- sum(u5mr_output$samplesize)
  mean_u5mr <- round(mean(u5mr_output$u5mr), 1)

  cli::cli_alert_success(
    "U5MR: {n_clusters} clusters, {total_deaths} total deaths, ",
    "mean U5MR = {mean_u5mr} per 1,000 live births"
  )

  # Return combined result
  list(
    u5mr = data.table::as.data.table(u5mr_output)
  )
}
