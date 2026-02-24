#' Prepare ITN Data for MBG Analysis
#'
#' Prepares cluster-level ITN ownership, access, and use data for Model-Based
#' Geostatistics (MBG) analysis. Aggregates to cluster counts WITHOUT survey
#' weights - MBG handles spatial smoothing internally.
#'
#' @details
#' Methodology: \url{https://github.com/ahadi-analytics/sntmethods/blob/master/inst/methods/itn_dhs.yml}
#'
#' @param dhs_hr DHS Household Records dataset.
#' @param dhs_pr DHS Person Records dataset.
#' @param gps_data DHS GPS dataset with cluster coordinates.
#' @param indicators Character vector of indicators to calculate. Options:
#'   \itemize{
#'     \item "itn_ownership": Households with at least one ITN
#'     \item "itn_access": Population with access to ITN (potential users / hh size)
#'     \item "itn_use_all": Population that used ITN last night
#'     \item "itn_use_u5": Under-5 children that used ITN
#'     \item "itn_use_5_10": Children 5-10 years that used ITN
#'     \item "itn_use_10_20": Adolescents 10-20 years that used ITN
#'     \item "itn_use_20plus": Adults 20+ that used ITN
#'     \item "itn_use_pregnant": Pregnant women that used ITN
#'     \item "itn_use_if_access": Of those with access, proportion that used ITN
#'   }
#'   Default: all indicators.
#' @param survey_vars Named list mapping DHS variable names.
#' @param gps_vars Named list for GPS variable mapping.
#' @param seed Deprecated. Previously used for probabilistic access assignment.
#'   Access is now calculated deterministically following standard DHS methodology.
#'
#' @return A list of data.tables (one per indicator), each with columns:
#'   \itemize{
#'     \item cluster_id: Cluster identifier
#'     \item indicator: Numerator count
#'     \item samplesize: Denominator count
#'     \item x: Longitude
#'     \item y: Latitude
#'   }
#'
#' @details
#' This function prepares data for MBG spatial modeling. Unlike the survey-
#' weighted `calc_itn_dhs()` function, this uses simple cluster-level counts.
#'
#' ITN access is calculated using the standard DHS deterministic assignment method:
#' \enumerate{
#'   \item Calculate potential users per household: min(ITNs * 2, household_size)
#'   \item Sort individuals within each household by ITN use (users first)
#'   \item Assign access to the first N individuals where N = potential_users
#' }
#'
#' This method guarantees that use <= access at the individual level, since
#' anyone who used an ITN is prioritized for access assignment.
#'
#' @examples
#' \dontrun{
#' itn_mbg <- calc_itn_mbg(
#'   dhs_hr = hr_data,
#'   dhs_pr = pr_data,
#'   gps_data = gps_data,
#'   indicators = c("itn_access", "itn_use_u5")
#' )
#' }
#'
#' @seealso [calc_itn_dhs()] for survey-weighted estimates
#' @export
calc_itn_mbg <- function(
  dhs_hr,
  dhs_pr,
  gps_data,
  indicators = c(
    "itn_ownership", "itn_access", "itn_use_all", "itn_use_u5", "itn_use_5_10",
    "itn_use_10_20", "itn_use_20plus", "itn_use_pregnant", "itn_use_if_access"
  ),
  survey_vars = list(
    cluster = "hv001",
    hhid = "hhid",
    hhsize = "hv013",
    age = "hv105",
    sex = "hv104",
    pregnant = "hml18",
    itn_use = "hml12",
    itn_prefix = "hml10_"
  ),
  gps_vars = list(
    cluster = "DHSCLUST",
    lat = "LATNUM",
    lon = "LONGNUM"
  ),
  seed = NULL
) {
  # ---- Input validation ----

  valid_indicators <- c(
    "itn_ownership", "itn_access", "itn_use_all", "itn_use_u5", "itn_use_5_10",
    "itn_use_10_20", "itn_use_20plus", "itn_use_pregnant", "itn_use_if_access"
  )

  invalid <- setdiff(indicators, valid_indicators)
  if (length(invalid) > 0) {
    cli::cli_abort("Invalid indicators: {.val {invalid}}")
  }

  # ---- Prepare GPS data ----

  gps_clean <- .prepare_gps_data(gps_data, gps_vars)

  # ---- Prepare HR data (household level) ----

  hr <- .prepare_itn_household_data(dhs_hr, survey_vars, include_survey_vars = FALSE)

  # ---- Prepare PR data (individual level with deterministic access) ----

  pr <- .prepare_itn_person_data(dhs_pr, hr, survey_vars, include_survey_vars = FALSE)

  # ---- Calculate indicators ----

  results <- list()

  # 1. Household ownership
  if ("itn_ownership" %in% indicators) {
    result <- .aggregate_to_mbg_clusters(hr, "has_itn", gps_clean, "itn_ownership")
    if (!is.null(result)) results[["itn_ownership"]] <- result
  }

  # 2. Population access
  if ("itn_access" %in% indicators) {
    result <- .aggregate_to_mbg_clusters(pr, "has_access", gps_clean, "itn_access")
    if (!is.null(result)) results[["itn_access"]] <- result
  }

  # 3. Population use (all ages)
  if ("itn_use_all" %in% indicators) {
    result <- .aggregate_to_mbg_clusters(pr, "itn_used", gps_clean, "itn_use_all")
    if (!is.null(result)) results[["itn_use_all"]] <- result
  }

  # 4. Under-5 use
  if ("itn_use_u5" %in% indicators) {
    pr_u5 <- pr |> dplyr::filter(age < 5)
    if (nrow(pr_u5) > 0) {
      result <- .aggregate_to_mbg_clusters(pr_u5, "itn_used", gps_clean, "itn_use_u5")
      if (!is.null(result)) results[["itn_use_u5"]] <- result
    }
  }

  # 5. Ages 5-10 use
  if ("itn_use_5_10" %in% indicators) {
    pr_5_10 <- pr |> dplyr::filter(age >= 5, age <= 9)
    if (nrow(pr_5_10) > 0) {
      result <- .aggregate_to_mbg_clusters(pr_5_10, "itn_used", gps_clean, "itn_use_5_10")
      if (!is.null(result)) results[["itn_use_5_10"]] <- result
    }
  }

  # 6. Ages 10-20 use
  if ("itn_use_10_20" %in% indicators) {
    pr_10_20 <- pr |> dplyr::filter(age >= 10, age <= 19)
    if (nrow(pr_10_20) > 0) {
      result <- .aggregate_to_mbg_clusters(pr_10_20, "itn_used", gps_clean, "itn_use_10_20")
      if (!is.null(result)) results[["itn_use_10_20"]] <- result
    }
  }

  # 7. Ages 20+ use
  if ("itn_use_20plus" %in% indicators) {
    pr_20plus <- pr |> dplyr::filter(age >= 20)
    if (nrow(pr_20plus) > 0) {
      result <- .aggregate_to_mbg_clusters(pr_20plus, "itn_used", gps_clean, "itn_use_20plus")
      if (!is.null(result)) results[["itn_use_20plus"]] <- result
    }
  }

  # 8. Pregnant women use
  if ("itn_use_pregnant" %in% indicators) {
    pr_preg <- pr |> dplyr::filter(is_pregnant == 1, sex == 2)
    if (nrow(pr_preg) > 0) {
      result <- .aggregate_to_mbg_clusters(pr_preg, "itn_used", gps_clean, "itn_use_pregnant")
      if (!is.null(result)) results[["itn_use_pregnant"]] <- result
    } else {
      cli::cli_alert_warning("No pregnant women found in data")
    }
  }

  # 9. Use if access (proportion of those with access who used ITN)
  if ("itn_use_if_access" %in% indicators) {
    pr_with_access <- pr |> dplyr::filter(has_access == 1)
    if (nrow(pr_with_access) > 0) {
      result <- .aggregate_to_mbg_clusters(
        pr_with_access, "itn_used", gps_clean, "itn_use_if_access"
      )
      if (!is.null(result)) results[["itn_use_if_access"]] <- result
    } else {
      cli::cli_alert_warning("No individuals with access found in data")
    }
  }

  if (length(results) == 0) {
    cli::cli_abort("No valid MBG data could be prepared")
  }

  results
}


#' Prepare Single ITN Indicator for MBG
#'
#' Simplified function to prepare a single ITN indicator for MBG.
#'
#' @inheritParams calc_itn_mbg
#' @param indicator Single indicator name. Default: "access".
#'
#' @return A data.table with columns: cluster_id, indicator, samplesize, x, y
#'
#' @export
prep_itn_mbg <- function(
  dhs_hr,
  dhs_pr,
  gps_data,
  indicator = "itn_access",
  survey_vars = list(
    cluster = "hv001",
    hhid = "hhid",
    hhsize = "hv013",
    age = "hv105",
    sex = "hv104",
    pregnant = "hml18",
    itn_use = "hml12",
    itn_prefix = "hml10_"
  ),
  gps_vars = list(
    cluster = "DHSCLUST",
    lat = "LATNUM",
    lon = "LONGNUM"
  ),
  seed = NULL
) {
  result <- calc_itn_mbg(
    dhs_hr = dhs_hr,
    dhs_pr = dhs_pr,
    gps_data = gps_data,
    indicators = indicator,
    survey_vars = survey_vars,
    gps_vars = gps_vars,
    seed = seed
  )

  result[[1]]
}
