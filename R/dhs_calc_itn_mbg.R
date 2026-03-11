#' Prepare ITN Data for MBG Analysis
#'
#' Prepares cluster-level ITN ownership, access, and use data for Model-Based
#' Geostatistics (MBG) analysis. Aggregates to cluster counts WITHOUT survey
#' weights - MBG handles spatial smoothing internally.
#'
#' Uses a dictionary-driven approach matching the indicator codes from
#' \code{\link{calc_itn_dhs}}. The dictionary mirrors the DHS
#' \code{.itn_conditions()} — same outcome variables, same filters, same data
#' sources (HR vs PR).
#'
#' @details
#' Methodology: \url{https://github.com/ahadi-analytics/sntmethods/blob/master/inst/methods/itn_dhs.yml}
#'
#' @param dhs_hr DHS Household Records dataset.
#' @param dhs_pr DHS Person Records dataset.
#' @param gps_data DHS GPS dataset with cluster coordinates.
#' @param indicators Character vector of indicators to calculate. Options
#'   from \code{.itn_mbg_dictionary()}:
#'   \itemize{
#'     \item \code{"with_itn"}: Households with at least one ITN (HR)
#'     \item \code{"enough_itn"}: Households with enough ITNs for every 2
#'       people (HR)
#'     \item \code{"access_itn"}: Population with access to ITN — binary
#'       indicator (PR)
#'     \item \code{"use_itn"}: Population that used ITN last night (PR)
#'     \item \code{"use_itn_chu5"}: Under-5 children that used ITN (PR)
#'     \item \code{"use_itn_5_10"}: Children 5-9 years that used ITN (PR)
#'     \item \code{"use_itn_10_20"}: Adolescents 10-19 years that used ITN (PR)
#'     \item \code{"use_itn_20plus"}: Adults 20+ that used ITN (PR)
#'     \item \code{"use_itn_preg"}: Pregnant women that used ITN (PR)
#'     \item \code{"use_itn_if_access"}: Of those with access, proportion that
#'       used ITN (PR)
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
#' weighted \code{calc_itn_dhs()} function, this uses simple cluster-level counts.
#'
#' ITN access is calculated using the standard DHS deterministic assignment method:
#' \enumerate{
#'   \item Calculate potential users per household: min(ITNs * 2, household_size)
#'   \item Sort individuals within each household by ITN use (users first)
#'   \item Assign access to the first N individuals where N = potential_users
#' }
#'
#' This method guarantees that use <= access at the individual level, since
#' anyone who used an ITN is prioritised for access assignment.
#'
#' @examples
#' \dontrun{
#' itn_mbg <- calc_itn_mbg(
#'   dhs_hr = hr_data,
#'   dhs_pr = pr_data,
#'   gps_data = gps_data,
#'   indicators = c("access_itn", "use_itn_chu5")
#' )
#' }
#'
#' @seealso [calc_itn_dhs()] for survey-weighted estimates,
#'   [.itn_mbg_dictionary()] for indicator definitions
#' @export
calc_itn_mbg <- function(
  dhs_hr,
  dhs_pr,
  gps_data,
  indicators = NULL,
  survey_vars = list(
    cluster = "hv001",
    hhid = "hhid",
    hhsize = "hv013",
    age = "hv105",
    sex = "hv104",
    pregnant = "hml18",
    itn_use = "hml12",
    itn_prefix = "hml10_",
    itn_treated_prefix = "hml7_"
  ),
  gps_vars = list(
    cluster = "DHSCLUST",
    lat = "LATNUM",
    lon = "LONGNUM"
  ),
  seed = NULL
) {
  # ---- Input validation ----

  if (!is.data.frame(dhs_hr)) cli::cli_abort("`dhs_hr` must be a data.frame or tibble")
  if (!is.data.frame(dhs_pr)) cli::cli_abort("`dhs_pr` must be a data.frame or tibble")
  if (!is.data.frame(gps_data)) cli::cli_abort("`gps_data` must be a data.frame or tibble")

  dict <- .itn_mbg_dictionary()
  dict_names <- vapply(dict, `[[`, character(1), "name")

  # Default: all indicators
  if (is.null(indicators)) {
    indicators <- dict_names
  }

  invalid <- setdiff(indicators, dict_names)
  if (length(invalid) > 0) {
    cli::cli_abort("Invalid indicators: {.val {invalid}}")
  }

  # Filter dictionary to requested indicators
  dict_specs <- dict[vapply(dict, function(d) d$name %in% indicators, logical(1))]

  # ---- Prepare GPS data ----

  gps_clean <- .prepare_gps_data(gps_data, gps_vars)

  # ---- Prepare HR data (household level) ----

  hr <- .prepare_itn_household_data(dhs_hr, survey_vars, include_survey_vars = FALSE)

  if (is.null(hr)) {
    return(NULL)
  }

  # Add sufficient nets indicator for enough_itn (1 if n_itns * 2 >= hh_size)
  hr <- hr |>
    dplyr::mutate(
      hh_sufficient_nets = as.integer(n_itns >= (hh_size / 2))
    )

  # ---- Prepare PR data (individual level with deterministic access) ----

  pr <- .prepare_itn_person_data(dhs_pr, hr, survey_vars, include_survey_vars = FALSE)

  # ---- Enrich PR data with derived columns ----

  pr <- pr |>
    dplyr::mutate(
      # Binary access indicator (MBG needs binary, not ratio)
      itn_access_ind = as.integer(has_access == 1),
      # Under-5 flag
      is_under5 = as.integer(age < 5),
      # Pregnant woman flag
      is_pregnant_woman = as.integer(sex == 2 & is_pregnant == 1),
      # Age group for age-specific use indicators
      age_group = dplyr::case_when(
        age >= 5  & age < 10 ~ "5_10",
        age >= 10 & age < 20 ~ "10_20",
        age >= 20            ~ "20plus",
        TRUE                 ~ NA_character_
      )
    )

  # ---- Dictionary-driven indicator loop ----

  results <- list()

  for (spec in dict_specs) {
    # Select the right data source
    src <- if (spec$data_source == "hr") hr else pr

    # Apply filter
    filtered <- src
    if (!is.null(spec$filter_col)) {
      col <- spec$filter_col
      val <- spec$filter_val
      if (!col %in% names(filtered)) {
        cli::cli_alert_warning("Column {.var {col}} not found for {.val {spec$name}} - skipping")
        next
      }
      filtered <- filtered[
        !is.na(filtered[[col]]) & filtered[[col]] == val, ,
        drop = FALSE
      ]
    }

    # Check outcome variable exists
    outcome_col <- spec$outcome
    if (!outcome_col %in% names(filtered)) {
      cli::cli_alert_warning("Outcome {.var {outcome_col}} not found for {.val {spec$name}} - skipping")
      next
    }

    # Drop NAs in outcome
    filtered <- filtered[!is.na(filtered[[outcome_col]]), , drop = FALSE]

    if (nrow(filtered) == 0) {
      cli::cli_alert_warning("No data for {.val {spec$name}} - skipping")
      next
    }

    dt <- .aggregate_to_mbg_clusters(filtered, outcome_col, gps_clean, spec$name)
    if (!is.null(dt)) {
      results[[spec$name]] <- dt
    }
  }

  if (length(results) == 0) {
    cli::cli_abort("No valid MBG data could be prepared")
  }

  results
}


# =============================================================================
# ITN MBG Indicator Dictionary
# =============================================================================

#' ITN MBG Indicator Dictionary
#'
#' Returns the full set of indicator specifications for cluster-level MBG output.
#' Each entry defines the indicator name, data source (HR or PR), outcome column,
#' and optional filter.
#'
#' Mirrors the DHS \code{.itn_conditions()} — same outcome variables, same
#' filters, same data sources. The MBG dictionary uses simple column-based
#' filtering instead of quoted expressions.
#'
#' @return List of named lists with fields: \code{name}, \code{data_source},
#'   \code{outcome}, \code{filter_col}, \code{filter_val}.
#' @noRd
.itn_mbg_dictionary <- function() {
  list(
    # Household-level (same as DHS WITH_ITN, ENOUGH_ITN)
    list(name = "with_itn",          data_source = "hr", outcome = "has_itn",
         filter_col = NULL, filter_val = NULL),
    list(name = "enough_itn",        data_source = "hr", outcome = "hh_sufficient_nets",
         filter_col = NULL, filter_val = NULL),

    # Person-level (same as DHS ACCESS_ITN, USE_ITN_*)
    list(name = "access_itn",        data_source = "pr", outcome = "itn_access_ind",
         filter_col = NULL, filter_val = NULL),
    list(name = "use_itn",           data_source = "pr", outcome = "itn_used",
         filter_col = NULL, filter_val = NULL),
    list(name = "use_itn_chu5",      data_source = "pr", outcome = "itn_used",
         filter_col = "is_under5", filter_val = 1),
    list(name = "use_itn_preg",      data_source = "pr", outcome = "itn_used",
         filter_col = "is_pregnant_woman", filter_val = 1),
    list(name = "use_itn_5_10",      data_source = "pr", outcome = "itn_used",
         filter_col = "age_group", filter_val = "5_10"),
    list(name = "use_itn_10_20",     data_source = "pr", outcome = "itn_used",
         filter_col = "age_group", filter_val = "10_20"),
    list(name = "use_itn_20plus",    data_source = "pr", outcome = "itn_used",
         filter_col = "age_group", filter_val = "20plus"),
    list(name = "use_itn_if_access", data_source = "pr", outcome = "itn_used",
         filter_col = "has_access", filter_val = 1)
  )
}


#' Prepare Single ITN Indicator for MBG
#'
#' Simplified function to prepare a single ITN indicator for MBG.
#'
#' @inheritParams calc_itn_mbg
#' @param indicator Single indicator name. Default: "access_itn".
#'
#' @return A data.table with columns: cluster_id, indicator, samplesize, x, y
#'
#' @export
prep_itn_mbg <- function(
  dhs_hr,
  dhs_pr,
  gps_data,
  indicator = "access_itn",
  survey_vars = list(
    cluster = "hv001",
    hhid = "hhid",
    hhsize = "hv013",
    age = "hv105",
    sex = "hv104",
    pregnant = "hml18",
    itn_use = "hml12",
    itn_prefix = "hml10_",
    itn_treated_prefix = "hml7_"
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
