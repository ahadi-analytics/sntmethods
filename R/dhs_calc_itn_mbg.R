#' Prepare ITN Data for MBG Analysis
#'
#' Prepares cluster-level ITN ownership, access, and use data for Model-Based
#' Geostatistics (MBG) analysis. Aggregates to cluster counts WITHOUT survey
#' weights - MBG handles spatial smoothing internally.
#'
#' @param dhs_hr DHS Household Records dataset.
#' @param dhs_pr DHS Person Records dataset.
#' @param gps_data DHS GPS dataset with cluster coordinates.
#' @param indicators Character vector of indicators to calculate. Options:
#'   \itemize{
#'     \item "ownership": Households with at least one ITN
#'     \item "access": Population with access to ITN (potential users / hh size)
#'     \item "use_all": Population that used ITN last night
#'     \item "use_u5": Under-5 children that used ITN
#'     \item "use_5_9": Children 5-9 years that used ITN
#'     \item "use_10_19": Adolescents 10-19 years that used ITN
#'     \item "use_20plus": Adults 20+ that used ITN
#'     \item "use_pregnant": Pregnant women that used ITN
#'   }
#'   Default: all indicators.
#' @param survey_vars Named list mapping DHS variable names.
#' @param gps_vars Named list for GPS variable mapping.
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
#' ITN access is calculated as: min(ITNs * 2, household_size) / household_size,
#' then converted to individual-level access using probabilistic assignment.
#'
#' @examples
#' \dontrun{
#' itn_mbg <- calc_itn_mbg(
#'   dhs_hr = hr_data,
#'   dhs_pr = pr_data,
#'   gps_data = gps_data,
#'   indicators = c("access", "use_u5")
#' )
#' }
#'
#' @param seed Optional random seed for reproducibility of probabilistic access
#'   assignment. Set to NULL to disable (different results each run).
#'   Default: 42 for reproducibility.
#'
#' @seealso [calc_itn_dhs()] for survey-weighted estimates
#' @export
calc_itn_mbg <- function(
  dhs_hr,
  dhs_pr,
  gps_data,
  indicators = c(
    "ownership", "access", "use_all", "use_u5", "use_5_9",
    "use_10_19", "use_20plus", "use_pregnant"
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
  seed = 42
) {
  # ---- Input validation ----

  if (!is.data.frame(dhs_hr)) {
    cli::cli_abort("`dhs_hr` must be a data.frame or tibble")
  }

  if (!is.data.frame(dhs_pr)) {
    cli::cli_abort("`dhs_pr` must be a data.frame or tibble")
  }

  if (!is.data.frame(gps_data)) {
    cli::cli_abort("`gps_data` must be a data.frame or tibble")
  }

  valid_indicators <- c(
    "ownership", "access", "use_all", "use_u5", "use_5_9",
    "use_10_19", "use_20plus", "use_pregnant"
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

  # ---- Prepare HR data (household level) ----

  # Find ITN variables
  itn_vars <- names(dhs_hr)[grepl(paste0("^", survey_vars$itn_prefix), names(dhs_hr))]

  if (length(itn_vars) == 0) {
    cli::cli_abort("No ITN variables found with prefix {.var {survey_vars$itn_prefix}}")
  }

  cli::cli_alert_info("Found {length(itn_vars)} ITN variables")

  hr <- dhs_hr |>
    dplyr::mutate(dplyr::across(dplyr::everything(), haven::zap_labels)) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), as.vector)) |>
    dplyr::transmute(
      cluster_id = .data[[survey_vars$cluster]],
      hhid = .data[[survey_vars$hhid]],
      hh_size = .data[[survey_vars$hhsize]],
      n_itns = dplyr::select(dhs_hr, dplyr::all_of(itn_vars)) |>
        dplyr::mutate(dplyr::across(dplyr::everything(), ~ dplyr::if_else(. == 1, 1L, 0L))) |>
        rowSums(na.rm = TRUE)
    ) |>
    dplyr::mutate(
      has_itn = as.integer(n_itns >= 1),
      potential_users = pmin(n_itns * 2, hh_size)
    )

  # ---- Prepare PR data (individual level) ----

  pr <- dhs_pr |>
    dplyr::mutate(dplyr::across(dplyr::everything(), haven::zap_labels)) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), as.vector)) |>
    dplyr::transmute(
      cluster_id = .data[[survey_vars$cluster]],
      hhid = .data[[survey_vars$hhid]],
      age = .data[[survey_vars$age]],
      sex = .data[[survey_vars$sex]],
      itn_used = dplyr::if_else(
        .data[[survey_vars$itn_use]] %in% c(1, 2),
        1L, 0L,
        missing = 0L
      ),
      is_pregnant = if (survey_vars$pregnant %in% names(dhs_pr)) {
        dplyr::if_else(.data[[survey_vars$pregnant]] == 1, 1L, 0L, missing = 0L)
      } else {
        0L
      }
    )

  # Merge household ITN info
  pr <- pr |>
    dplyr::left_join(
      hr |> dplyr::select(cluster_id, hhid, hh_size, n_itns, potential_users),
      by = c("cluster_id", "hhid")
    )

  # Calculate access ratio and probabilistic access
  if (!is.null(seed)) {
    set.seed(seed)  # For reproducibility
  }
  pr <- pr |>
    dplyr::mutate(
      access_ratio = dplyr::if_else(
        !is.na(hh_size) & hh_size > 0,
        pmin(potential_users / hh_size, 1),
        0
      ),
      has_access = dplyr::if_else(
        access_ratio >= stats::runif(dplyr::n()),
        1L, 0L
      )
    )

  # ---- Calculate indicators ----

  results <- list()

  # 1. Household ownership
  if ("ownership" %in% indicators) {
    ownership_cluster <- hr |>
      dplyr::group_by(cluster_id) |>
      dplyr::summarise(
        indicator = sum(has_itn, na.rm = TRUE),
        samplesize = dplyr::n(),
        .groups = "drop"
      ) |>
      dplyr::inner_join(gps_clean, by = "cluster_id") |>
      dplyr::filter(samplesize > 0)

    results[["itn_ownership"]] <- data.table::as.data.table(ownership_cluster)

    cli::cli_alert_success(
      "itn_ownership: {nrow(ownership_cluster)} clusters"
    )
  }

  # 2. Population access
  if ("access" %in% indicators) {
    access_cluster <- pr |>
      dplyr::group_by(cluster_id) |>
      dplyr::summarise(
        indicator = sum(has_access, na.rm = TRUE),
        samplesize = dplyr::n(),
        .groups = "drop"
      ) |>
      dplyr::inner_join(gps_clean, by = "cluster_id") |>
      dplyr::filter(samplesize > 0)

    results[["itn_access"]] <- data.table::as.data.table(access_cluster)

    cli::cli_alert_success(
      "itn_access: {nrow(access_cluster)} clusters"
    )
  }

  # 3. Population use (all ages)
  if ("use_all" %in% indicators) {
    use_cluster <- pr |>
      dplyr::group_by(cluster_id) |>
      dplyr::summarise(
        indicator = sum(itn_used, na.rm = TRUE),
        samplesize = dplyr::n(),
        .groups = "drop"
      ) |>
      dplyr::inner_join(gps_clean, by = "cluster_id") |>
      dplyr::filter(samplesize > 0)

    results[["itn_use_all"]] <- data.table::as.data.table(use_cluster)

    cli::cli_alert_success(
      "itn_use_all: {nrow(use_cluster)} clusters"
    )
  }

  # 4. Under-5 use
  if ("use_u5" %in% indicators) {
    pr_u5 <- pr |> dplyr::filter(age < 5)

    if (nrow(pr_u5) > 0) {
      u5_cluster <- pr_u5 |>
        dplyr::group_by(cluster_id) |>
        dplyr::summarise(
          indicator = sum(itn_used, na.rm = TRUE),
          samplesize = dplyr::n(),
          .groups = "drop"
        ) |>
        dplyr::inner_join(gps_clean, by = "cluster_id") |>
        dplyr::filter(samplesize > 0)

      results[["itn_use_u5"]] <- data.table::as.data.table(u5_cluster)

      cli::cli_alert_success(
        "itn_use_u5: {nrow(u5_cluster)} clusters"
      )
    }
  }

  # 5. Ages 5-9 use
  if ("use_5_9" %in% indicators) {
    pr_5_9 <- pr |> dplyr::filter(age >= 5, age <= 9)

    if (nrow(pr_5_9) > 0) {
      age_cluster <- pr_5_9 |>
        dplyr::group_by(cluster_id) |>
        dplyr::summarise(
          indicator = sum(itn_used, na.rm = TRUE),
          samplesize = dplyr::n(),
          .groups = "drop"
        ) |>
        dplyr::inner_join(gps_clean, by = "cluster_id") |>
        dplyr::filter(samplesize > 0)

      results[["itn_use_5_9"]] <- data.table::as.data.table(age_cluster)

      cli::cli_alert_success(
        "itn_use_5_9: {nrow(age_cluster)} clusters"
      )
    }
  }

  # 6. Ages 10-19 use
  if ("use_10_19" %in% indicators) {
    pr_10_19 <- pr |> dplyr::filter(age >= 10, age <= 19)

    if (nrow(pr_10_19) > 0) {
      age_cluster <- pr_10_19 |>
        dplyr::group_by(cluster_id) |>
        dplyr::summarise(
          indicator = sum(itn_used, na.rm = TRUE),
          samplesize = dplyr::n(),
          .groups = "drop"
        ) |>
        dplyr::inner_join(gps_clean, by = "cluster_id") |>
        dplyr::filter(samplesize > 0)

      results[["itn_use_10_19"]] <- data.table::as.data.table(age_cluster)

      cli::cli_alert_success(
        "itn_use_10_19: {nrow(age_cluster)} clusters"
      )
    }
  }

  # 7. Ages 20+ use
  if ("use_20plus" %in% indicators) {
    pr_20plus <- pr |> dplyr::filter(age >= 20)

    if (nrow(pr_20plus) > 0) {
      age_cluster <- pr_20plus |>
        dplyr::group_by(cluster_id) |>
        dplyr::summarise(
          indicator = sum(itn_used, na.rm = TRUE),
          samplesize = dplyr::n(),
          .groups = "drop"
        ) |>
        dplyr::inner_join(gps_clean, by = "cluster_id") |>
        dplyr::filter(samplesize > 0)

      results[["itn_use_20plus"]] <- data.table::as.data.table(age_cluster)

      cli::cli_alert_success(
        "itn_use_20plus: {nrow(age_cluster)} clusters"
      )
    }
  }

  # 8. Pregnant women use
  if ("use_pregnant" %in% indicators) {
    pr_preg <- pr |> dplyr::filter(is_pregnant == 1, sex == 2)

    if (nrow(pr_preg) > 0) {
      preg_cluster <- pr_preg |>
        dplyr::group_by(cluster_id) |>
        dplyr::summarise(
          indicator = sum(itn_used, na.rm = TRUE),
          samplesize = dplyr::n(),
          .groups = "drop"
        ) |>
        dplyr::inner_join(gps_clean, by = "cluster_id") |>
        dplyr::filter(samplesize > 0)

      results[["itn_use_pregnant"]] <- data.table::as.data.table(preg_cluster)

      cli::cli_alert_success(
        "itn_use_pregnant: {nrow(preg_cluster)} clusters"
      )
    } else {
      cli::cli_alert_warning("No pregnant women found in data")
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
  indicator = "access",
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
  seed = 42
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
