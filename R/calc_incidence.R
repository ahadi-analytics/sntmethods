#' Calculate Malaria Incidence from Routine Health Facility Data (N0-N4)
#'
#' Calculates malaria incidence at the admin-month level (e.g., district-month)
#' using a structured cascade framework (N0 through N4) to adjust for testing
#' gaps, reporting incompleteness, and care-seeking behavior. Facility-level
#' data is aggregated to admin level before calculating incidence. Returns a
#' validated dataset with all incidence levels, quality flags, and source
#' tracking.
#'
#' The incidence cascade framework:
#' \itemize{
#'   \item **N0 (Crude Incidence)**:
#'     \itemize{
#'       \item n0_cases = conf
#'       \item n0_incidence = (n0_cases / pop) * rate_multiplier
#'     }
#'   \item **N1 (Testing-Adjusted)**:
#'     \itemize{
#'       \item n1_cases = n0_cases + (pres * tpr)
#'       \item n1_incidence = (n1_cases / pop) * rate_multiplier
#'     }
#'   \item **N2 (Reporting-Adjusted)**:
#'     \itemize{
#'       \item n2_cases = n1_cases / reprate
#'       \item n2_incidence = (n2_cases / pop) * rate_multiplier
#'     }
#'   \item **N3 (Care-Seeking-Adjusted)**:
#'     \itemize{
#'       \item n3_cases = n2_cases + (n2_cases * cs_private/cs_public) +
#'         (n2_cases * cs_none/cs_public)
#'       \item n3_incidence = (n3_cases / pop) * rate_multiplier
#'     }
#'   \item **N4 (Public + Non-Seekers Adjusted)**:
#'     \itemize{
#'       \item n4_cases = n2_cases * (1 + cs_none/cs_public)
#'       \item n4_incidence = (n4_cases / pop) * rate_multiplier
#'       \item **Note**: Excludes private sector adjustment. Use when
#'         private data is already captured in DHIS2 or private burden
#'         is negligible.
#'       \item N4 is always between N2 and N3 in magnitude (N2 < N4 < N3).
#'     }
#' }
#'
#' Each level builds on the previous, with N3 representing the most complete
#' estimate of true community-level malaria incidence. N4 provides a conservative
#' alternative to N3 that excludes private sector.
#'
#' @param data Routine health facility data at facility-month level
#'   (data.frame or tibble). Must contain one row per facility per month.
#' @param levels Character vector specifying which incidence levels to calculate
#'   (default: `c("N0", "N1", "N2", "N3")`). Can specify subset like `c("N0",
#'   "N1")` or include N4 with `c("N0", "N1", "N2", "N3", "N4")`.
#' @param hf_var Column name for health facility unique identifier
#'   (default: "hf_uid").
#' @param adm0_var Column name for national/country level. If NULL (default),
#'   creates a single "country" value for all records.
#' @param adm1_var Column name for first administrative level/region
#'   (default: "adm1").
#' @param adm2_var Column name for second administrative level/district
#'   (default: "adm2").
#' @param adm3_var Column name for third administrative level/sub-district.
#'   If NULL (default), adm3 column is not included in output.
#' @param date_var Column name for date of reporting period (default: "date").
#' @param year_var Column name for year (default: "year"). Used for year-level
#'   aggregation in summary output.
#' @param pop_var Column name for population denominator (default: "pop").
#'   Must be present in `data`.
#' @param conf_var Column name for number of confirmed malaria cases
#'   (default: "conf").
#' @param test_var Column name for number of individuals tested
#'   (default: "test").
#' @param pres_var Column name for number of presumed cases (default: "pres").
#' @param tpr_var Column name for test positivity rate (default: "tpr").
#'   Must be present in `data` for N1/N2/N3 calculations.
#' @param reprate_var Column name for reporting rate (default: "reprate").
#'   Must be present in `data` for N2/N3 calculations.
#' @param cs_public_var Column name for proportion seeking care at public
#'   facilities (default: "cs_public"). Required for N3.
#' @param cs_private_var Column name for proportion seeking care at private
#'   facilities (default: "cs_private"). Required for N3.
#' @param cs_none_var Column name for proportion seeking no care
#'   (default: "cs_none"). Required for N3.
#' @param rate_multiplier Denominator for incidence rate calculation
#'   (default: 1000, for cases per 1,000 population).
#' @param scale_factor Deprecated. Use `rate_multiplier` instead.
#' @param include_flags Logical; if `TRUE`, includes all quality flag columns
#'   in the output. If `FALSE` (default), returns only the core incidence
#'   variables without flags.
#' @param suffix Character string to append to incidence column names
#'   (default: NULL). If provided (e.g., `suffix = "u5"`), output columns
#'   will be named `n0_cases_u5`, `n0_incidence_u5`, etc. Useful for
#'   distinguishing outputs for different population groups.
#' @param return_facility Logical; if `TRUE`, includes facility-level data
#'   in the output with diagnostic flags. If `FALSE` (default), returns only
#'   aggregated admin-level data. Useful for investigating data quality issues
#'   before aggregation (e.g., why n1_cases < n0_cases).
#'
#' @return A named list with components:
#'   \describe{
#'     \item{monthly}{A named list with tibbles at each admin level (N0-N4):
#'       \itemize{
#'         \item `adm0`: National level monthly incidence (N0-N4)
#'         \item `adm1`: First admin level (region) monthly incidence (N0-N4)
#'         \item `adm2`: Second admin level (district) monthly incidence (N0-N4)
#'         \item `adm3`: Third admin level monthly incidence (N0-N4, if `adm3_var` provided)
#'       }
#'     }
#'     \item{annual}{A named list with tibbles at each admin level (N0-N4):
#'       \itemize{
#'         \item `adm0`: National level annual incidence (N0-N4)
#'         \item `adm1`: First admin level (region) annual incidence (N0-N4)
#'         \item `adm2`: Second admin level (district) annual incidence (N0-N4)
#'         \item `adm3`: Third admin level annual incidence (N0-N4, if `adm3_var` provided)
#'       }
#'     }
#'     \item{facility}{(Only if `return_facility = TRUE`) A named list with tibbles:
#'       \itemize{
#'         \item `monthly`: Facility-month level data
#'         \item `annual`: Facility-year level data (if applicable)
#'       }
#'     }
#'   }
#'
#'   Monthly and annual tibbles contain:
#'   \itemize{
#'     \item ID columns: `adm0`, `adm1`, `adm2`, `adm3` (depending on level)
#'     \item Time columns: `year`, `month`, `date` (monthly) or `year` (annual)
#'     \item `pop`: Population denominator (summed)
#'     \item `conf`: Confirmed cases (summed)
#'     \item `test`, `pres`, `tpr`: Testing data (summed, if N1+ calculated)
#'     \item `reprate`: Reporting rate (summed, if N2+ calculated)
#'     \item `cs_public`, `cs_private`, `cs_none`: Care-seeking proportions (if N3+ calculated)
#'     \item `n0_cases`, `n0_incidence`: N0 (crude)
#'     \item `n1_cases`, `n1_incidence`: N1 (testing-adjusted)
#'     \item `n2_cases`, `n2_incidence`: N2 (reporting-adjusted)
#'     \item `n3_cases`, `n3_incidence`: N3 (care-seeking-adjusted)
#'     \item `n4_cases`, `n4_incidence`: N4 (public + non-seekers)
#'   }
#'
#' @examples
#' # Example workflow: Calculate TPR first, then incidence
#' # library(tibble)
#' #
#' # # Step 1: Calculate TPR
#' # facility_data <- tibble(
#' #   hf_uid = c("HF001", "HF002", "HF003"),
#' #   adm1 = rep("RegionA", 3),
#' #   adm2 = rep("DistrictX", 3),
#' #   date = as.Date(c("2023-01-01", "2023-02-01", "2023-01-01")),
#' #   conf = c(10, 15, 12),
#' #   test = c(100, 120, 80),
#' #   pres = c(5, 8, 10)
#' # )
#' #
#' # tpr_data <- calc_tpr(facility_data)
#' #
#' # # Step 2: Add population, reporting rate, and care-seeking data
#' # tpr_data$pop <- c(5000, 6000, 4500)
#' # tpr_data$reprate <- c(0.85, 0.90, 0.80)
#' # tpr_data$cs_public <- 0.60
#' # tpr_data$cs_private <- 0.25
#' # tpr_data$cs_none <- 0.15
#' #
#' # # Step 3: Calculate incidence (all levels N0-N3)
#' # result <- calc_incidence(tpr_data)
#' #
#' # # Access monthly facility-level data
#' # result$monthly$hf
#' #
#' # # Access monthly district-level data
#' # result$monthly$adm2
#' #
#' # # Access annual facility-level data
#' # result$annual$hf
#' #
#' # # Access annual national-level data
#' # result$annual$adm0
#'
#' @importFrom lifecycle deprecated
#' @export
calc_incidence <- function(
  data,
  levels = c("N0", "N1", "N2", "N3"),
  hf_var = "hf_uid",
  adm0_var = NULL,
  adm1_var = "adm1",
  adm2_var = "adm2",
  adm3_var = NULL,
  date_var = "date",
  year_var = "year",
  pop_var = "pop",
  conf_var = "conf",
  test_var = "test",
  pres_var = "pres",
  tpr_var = "tpr",
  reprate_var = "reprate",
  cs_public_var = "cs_public",
  cs_private_var = "cs_private",
  cs_none_var = "cs_none",
  rate_multiplier = 1000,
  scale_factor = lifecycle::deprecated(),
  include_flags = FALSE,
  suffix = NULL,
  return_facility = FALSE
) {
  # ---- handle deprecated scale_factor parameter -----------------------------

  if (lifecycle::is_present(scale_factor)) {
    lifecycle::deprecate_warn(
      "0.1.0",
      "calc_incidence(scale_factor)",
      "calc_incidence(rate_multiplier)"
    )
    rate_multiplier <- scale_factor
  }

  # ---- validate data -------------------------------------------------------

  if (!is.data.frame(data)) {
    cli::cli_abort("`data` must be a data.frame or tibble.")
  }

  if (nrow(data) == 0) {
    cli::cli_abort("`data` is empty.")
  }

  # Validate levels parameter
  valid_levels <- c("N0", "N1", "N2", "N3", "N4")
  invalid_levels <- setdiff(levels, valid_levels)

  if (length(invalid_levels) > 0) {
    cli::cli_abort(
      c(
        "Invalid incidence level(s):",
        "x" = "{.var {invalid_levels}}",
        "i" = "Valid options: {.val {valid_levels}}"
      )
    )
  }

  # Base required columns for N0
  required <- c(
    hf_var,
    adm1_var,
    adm2_var,
    date_var,
    pop_var,
    conf_var
  )

  # Additional requirements for N1 (only if N1 explicitly requested)
  if ("N1" %in% levels) {
    required <- c(required, test_var, pres_var)
  }

  missing_cols <- setdiff(required, names(data))

  if (length(missing_cols) > 0) {
    cli::cli_abort(
      c(
        "Missing required columns:",
        "{.var {missing_cols}}"
      )
    )
  }

  # ---- validate parameters -------------------------------------------------

  if (!is.numeric(rate_multiplier) || length(rate_multiplier) != 1) {
    cli::cli_abort("`rate_multiplier` must be a single numeric value.")
  }

  if (rate_multiplier <= 0) {
    cli::cli_abort("`rate_multiplier` must be positive.")
  }

  cli::cli_alert_info(
    "Processing {sntutils::big_mark(nrow(data))} facility-month records."
  )

  # ---- validate TPR presence -----------------------------------------------
  # TPR only required if N1 is explicitly requested
  # If N2/N3 requested without N1, we skip the testing adjustment (N1 = N0)

  needs_tpr <- "N1" %in% levels

  if (needs_tpr && !tpr_var %in% names(data)) {
    cli::cli_abort(
      c(
        "TPR column '{tpr_var}' not found in data.",
        "i" = paste0(
          "Calculate TPR first using calc_tpr() ",
          "before calling calc_incidence()."
        )
      )
    )
  }

  # ---- validate reporting rate presence ------------------------------------
  # Reprate only required if N2 is explicitly requested
  # If N3 requested without N2, we skip the reporting adjustment (N2 = N1)

  needs_reprate <- "N2" %in% levels

  if (needs_reprate && !reprate_var %in% names(data)) {
    cli::cli_abort(
      c(
        "Reporting rate column '{reprate_var}' not found in data.",
        "i" = "Required for N2 calculation."
      )
    )
  }

  # ---- validate data ranges (hard validation) --------------------------------

  # Validate confirmed cases are non-negative
  if (any(data[[conf_var]] < 0, na.rm = TRUE)) {
    n_invalid <- sum(data[[conf_var]] < 0, na.rm = TRUE)
    cli::cli_abort(c(
      "Column '{conf_var}' contains {n_invalid} negative value{?s}.",
      "x" = "Confirmed cases must be >= 0.",
      "i" = "Check your data for negative values before calling calc_incidence()."
    ))
  }

  # Validate test counts are non-negative (if N1+ requested)
  if (needs_tpr && test_var %in% names(data)) {
    if (any(data[[test_var]] < 0, na.rm = TRUE)) {
      n_invalid <- sum(data[[test_var]] < 0, na.rm = TRUE)
      cli::cli_abort(c(
        "Column '{test_var}' contains {n_invalid} negative value{?s}.",
        "x" = "Test counts must be >= 0.",
        "i" = "Check your data for negative values before calling calc_incidence()."
      ))
    }
  }

  # Validate presumed cases are non-negative (if N1+ requested)
  if (needs_tpr && pres_var %in% names(data)) {
    if (any(data[[pres_var]] < 0, na.rm = TRUE)) {
      n_invalid <- sum(data[[pres_var]] < 0, na.rm = TRUE)
      cli::cli_abort(c(
        "Column '{pres_var}' contains {n_invalid} negative value{?s}.",
        "x" = "Presumed cases must be >= 0.",
        "i" = "Check your data for negative values before calling calc_incidence()."
      ))
    }
  }

  # Validate TPR is in [0, 1] range (if present and N1+ requested)
  if (needs_tpr && tpr_var %in% names(data)) {
    tpr_vals <- data[[tpr_var]]
    invalid_tpr <- !is.na(tpr_vals) & (tpr_vals < 0 | tpr_vals > 1)
    if (any(invalid_tpr)) {
      n_invalid <- sum(invalid_tpr)
      tpr_range <- range(tpr_vals[invalid_tpr], na.rm = TRUE)
      cli::cli_abort(c(
        "Column '{tpr_var}' contains {n_invalid} value{?s} outside [0, 1] range.",
        "x" = "TPR (test positivity rate) must be between 0 and 1.",
        "i" = "Found values ranging from {round(tpr_range[1], 3)} to {round(tpr_range[2], 3)}.",
        "i" = "Ensure TPR is a proportion, not a percentage."
      ))
    }
  }

  # Validate reporting rate is in (0, 1] range (if present and N2+ requested)
  if (needs_reprate && reprate_var %in% names(data)) {
    reprate_vals <- data[[reprate_var]]
    invalid_reprate <- !is.na(reprate_vals) & (reprate_vals < 0 | reprate_vals > 1)
    if (any(invalid_reprate)) {
      n_invalid <- sum(invalid_reprate)
      reprate_range <- range(reprate_vals[invalid_reprate], na.rm = TRUE)
      cli::cli_abort(c(
        "Column '{reprate_var}' contains {n_invalid} value{?s} outside (0, 1] range.",
        "x" = "Reporting rate must be > 0 and <= 1.",
        "i" = "Found values ranging from {round(reprate_range[1], 3)} to {round(reprate_range[2], 3)}.",
        "i" = "Ensure reporting rate is a proportion, not a percentage."
      ))
    }
  }

  # ---- warn on inconsistent care-seeking values ----------------------------

  needs_cs <- any(c("N3", "N4") %in% levels)

  if (needs_cs) {
    cs_vars_present <- c(cs_public_var, cs_private_var, cs_none_var) %in% names(data)

    if (any(cs_vars_present)) {
      # Build grouping for admin-month check
      group_cols <- c(adm1_var, adm2_var, date_var)

      # Check each cs variable for consistency within admin-month groups
      for (cs_var in c(cs_public_var, cs_private_var, cs_none_var)) {
        if (cs_var %in% names(data)) {
          n_unique <- data |>
            dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
            dplyr::summarise(
              n_distinct = dplyr::n_distinct(.data[[cs_var]], na.rm = TRUE),
              .groups = "drop"
            ) |>
            dplyr::filter(n_distinct > 1) |>
            nrow()

          if (n_unique > 0) {
            cli::cli_warn(c(
              "Column '{cs_var}' has varying values within {n_unique} admin-month group{?s}.",
              "i" = "Care-seeking variables should be constant within admin areas.",
              "i" = "Only the first value per group will be used during aggregation."
            ))
          }
        }
      }
    }
  }

  # ---- rename vars internally ----------------------------------------------

  # Build list of columns that will be renamed to standard names
  # We need to drop any existing columns with these standard names to avoid

  # duplicates (e.g., if data has both "conf" and "conf_u5" and user specifies
  # conf_var = "conf_u5", we need to drop "conf" before renaming "conf_u5")
  standard_names <- c("hf_uid", "adm1", "adm2", "date_raw", "pop", "conf")
  input_vars <- c(hf_var, adm1_var, adm2_var, date_var, pop_var, conf_var)

  if (needs_tpr) {
    standard_names <- c(standard_names, "test", "pres", "tpr")
    input_vars <- c(input_vars, test_var, pres_var, tpr_var)
  }

  if (needs_reprate && reprate_var != "reprate") {
    standard_names <- c(standard_names, "reprate")
    input_vars <- c(input_vars, reprate_var)
  }

  if ("N3" %in% levels || "N4" %in% levels) {
    if (cs_public_var %in% names(data) && cs_public_var != "cs_public") {
      standard_names <- c(standard_names, "cs_public")
      input_vars <- c(input_vars, cs_public_var)
    }
    if (cs_private_var %in% names(data) && cs_private_var != "cs_private") {
      standard_names <- c(standard_names, "cs_private")
      input_vars <- c(input_vars, cs_private_var)
    }
    if (cs_none_var %in% names(data) && cs_none_var != "cs_none") {
      standard_names <- c(standard_names, "cs_none")
      input_vars <- c(input_vars, cs_none_var)
    }
  }

  # Find columns that would cause duplicates (standard name exists but isn't
  # the input var being renamed)
  cols_to_drop <- setdiff(
    intersect(standard_names, names(data)),
    input_vars
  )

  if (length(cols_to_drop) > 0) {
    data <- data |>
      dplyr::select(-dplyr::all_of(cols_to_drop))
  }

  df <- data |>
    dplyr::rename(
      hf_uid = !!hf_var,
      adm1 = !!adm1_var,
      adm2 = !!adm2_var,
      date_raw = !!date_var,
      pop = !!pop_var,
      conf = !!conf_var
    ) |>
    dplyr::mutate(
      pop = as.numeric(pop),
      conf = as.numeric(conf)
    )

  # Add test and pres for N1+
  if (needs_tpr) {
    df <- df |>
      dplyr::rename(
        test = !!test_var,
        pres = !!pres_var
      ) |>
      dplyr::mutate(
        test = as.numeric(test),
        pres = as.numeric(pres)
      )

    # Rename TPR if exists
    if (tpr_var %in% names(df) && tpr_var != "tpr") {
      df <- df |>
        dplyr::rename(tpr = !!tpr_var)
    }
  }

  # Add reprate for N2+
  if (needs_reprate) {
    if (reprate_var %in% names(df) && reprate_var != "reprate") {
      df <- df |>
        dplyr::rename(reprate = !!reprate_var)
    }
    # Explicitly convert to numeric (like test and pres)
    # Use as.character first to handle factor inputs correctly
    df <- df |>
      dplyr::mutate(reprate = as.numeric(as.character(reprate)))
  }

  # Add care-seeking for N3 and N4
  if ("N3" %in% levels || "N4" %in% levels) {
    if (cs_public_var %in% names(data)) {
      df <- df |>
        dplyr::rename(cs_public = !!cs_public_var)
    }
    if (cs_private_var %in% names(data)) {
      df <- df |>
        dplyr::rename(cs_private = !!cs_private_var)
    }
    if (cs_none_var %in% names(data)) {
      df <- df |>
        dplyr::rename(cs_none = !!cs_none_var)
    }
  }

  # ---- adm0 handling -------------------------------------------------------

  if (!is.null(adm0_var)) {
    if (!adm0_var %in% names(data)) {
      cli::cli_abort("adm0_var not found in data.")
    }
    df <- df |>
      dplyr::mutate(adm0 = .data[[adm0_var]])
  } else {
    df <- df |>
      dplyr::mutate(adm0 = "country")
  }

  # ---- adm3 handling -------------------------------------------------------

  if (!is.null(adm3_var)) {
    if (!adm3_var %in% names(data)) {
      cli::cli_abort("adm3_var '{adm3_var}' not found in data.")
    }
    df <- df |>
      dplyr::mutate(adm3 = .data[[adm3_var]])
  }

  # ---- date handling -------------------------------------------------------

  df <- df |>
    dplyr::mutate(
      date = as.Date(date_raw),
      date = lubridate::floor_date(date, "month"),
      year = lubridate::year(date),
      month = lubridate::month(date)
    )

  # ---- duplicate check (at facility-month level, before aggregation) -------

  n_dup <- df |>
    dplyr::group_by(hf_uid, date) |>
    dplyr::filter(dplyr::n() > 1) |>
    nrow()

  if (n_dup > 0) {
    cli::cli_alert_warning(
      "Found {sntutils::big_mark(n_dup)} duplicate facility-months."
    )
  }

  # ---- N1 calculation: ADMIN-LEVEL ONLY (NOT facility-level) ----------------
  #
  # IMPORTANT: We do NOT calculate N1 at facility level before aggregation.
  #
  # Previous approach (facility-level N1 → sum to admin):
  #   - Calculated n1_cases_fac = conf + pres × tpr for each facility
  #   - Summed n1_cases_fac to get admin-level n1_cases
  #   - ISSUE: Facilities with missing TPR were excluded (n1_cases_fac = NA)
  #   - Result: n1_cases < n0_cases (data loss from excluded facilities)
  #   - Example: BLITTA 2018-03 had 5/22 facilities with missing TPR
  #             → 809 confirmed cases excluded from N1
  #             → n0=3300, n1=2491 (impossible!)
  #
  # Current approach (admin-level N1 after aggregation):
  #   - Aggregate conf, pres, test to admin level
  #   - Calculate admin-level TPR = sum(conf) / sum(test)
  #   - Calculate n1_cases = sum(conf) + sum(pres) × admin_tpr
  #   - Result: All facilities contribute, n1 >= n0 (mathematically correct)
  #
  # Trade-off: We lose facility-specific TPR variation, but gain data completeness.
  # This is acceptable because missing TPR is a data quality issue that would
  # otherwise cause severe underestimation of N1.
  #
  # N1 will be calculated after aggregation in .calc_n1_internal()
  # ----------------------------------------------------------------------------

  # ---- store facility-level data before aggregation -------------------------

  # Keep facility-level data for hf-level output
  df_facility <- df

  # ---- aggregate to admin-month level --------------------------------------

  cli::cli_alert_info("Aggregating to admin-month level...")
  n_facilities <- length(unique(df$hf_uid))
  n_admins <- df |>
    dplyr::distinct(adm0, adm1, adm2) |>
    nrow()

  df <- .aggregate_to_admin_month(df)

  cli::cli_alert_success(
    "Aggregated {sntutils::big_mark(n_facilities)} facilities to ",
    "{sntutils::big_mark(n_admins)} admin units."
  )

  # ---- create flags --------------------------------------------------------

  df <- df |>
    dplyr::mutate(
      flag_pop_zero = pop == 0 | is.na(pop),
      flag_pop_missing = is.na(pop)
    )

  if (needs_tpr) {
    df <- df |>
      dplyr::mutate(
        flag_tpr_missing = is.na(tpr)
      )
  }

  if (needs_reprate) {
    df <- df |>
      dplyr::mutate(
        flag_reprate_missing = is.na(reprate),
        flag_reprate_low = !is.na(reprate) & reprate < 0.5
      )
  }

  if ("N3" %in% levels || "N4" %in% levels) {
    # Only create flag if care-seeking columns exist
    if (all(c("cs_public", "cs_none") %in% names(df))) {
      df <- df |>
        dplyr::mutate(
          flag_cs_missing = is.na(cs_public) | is.na(cs_none)
        )
    }
  }

  # ---- calculate N0: crude incidence ---------------------------------------
  # N0 is always needed as the base for all higher levels

  needs_n0 <- any(c("N0", "N1", "N2", "N3", "N4") %in% levels)

  if (needs_n0) {
    if ("N0" %in% levels) {
      cli::cli_alert_info("Calculating N0 (crude incidence)...")
    } else {
      cli::cli_alert_info("Calculating N0 as base for higher levels...")
    }

    df <- df |>
      .calc_n0_internal(scale_factor = rate_multiplier)

    n_n0_valid <- sum(!is.na(df$n0_incidence))
    cli::cli_alert_success(
      "N0 calculated for {sntutils::big_mark(n_n0_valid)} records."
    )
  }

  # ---- calculate N1: testing-adjusted incidence ----------------------------

  # Check if higher levels need N1 but N1 wasn't explicitly requested
  needs_n1_for_higher <- any(c("N2", "N3", "N4") %in% levels) && !("N1" %in% levels)

  if ("N1" %in% levels) {
    cli::cli_alert_info("Calculating N1 (testing-adjusted incidence)...")

    df <- df |>
      .calc_n1_internal(scale_factor = rate_multiplier)

    n_n1_valid <- sum(!is.na(df$n1_incidence))
    n_n1_invalid <- sum(is.na(df$n1_incidence))

    cli::cli_alert_success(
      "N1 calculated for {sntutils::big_mark(n_n1_valid)} records."
    )

    if (n_n1_invalid > 0) {
      cli::cli_alert_warning(
        "{sntutils::big_mark(n_n1_invalid)} records have invalid N1 ",
        "(missing TPR or invalid values)."
      )
    }
  } else if (needs_n1_for_higher) {
    # N1 not requested but higher levels need it - use N1 = N0 (skip TPR adjustment)
    cli::cli_alert_info(
      "N1 not requested - using N1 = N0 (skipping testing adjustment)..."
    )

    df <- df |>
      dplyr::mutate(
        n1_cases = n0_cases,
        n1_incidence = n0_incidence
      )
  }

  # ---- calculate N2: reporting-adjusted incidence --------------------------

  # Check if higher levels need N2 but N2 wasn't explicitly requested
  needs_n2_for_higher <- any(c("N3", "N4") %in% levels) && !("N2" %in% levels)

  if ("N2" %in% levels) {
    cli::cli_alert_info(
      "Calculating N2 (reporting-adjusted incidence)..."
    )

    df <- df |>
      .calc_n2_internal(scale_factor = rate_multiplier)

    n_n2_valid <- sum(!is.na(df$n2_incidence))
    n_n2_invalid <- sum(is.na(df$n2_incidence))

    cli::cli_alert_success(
      "N2 calculated for {sntutils::big_mark(n_n2_valid)} records."
    )

    if (n_n2_invalid > 0) {
      cli::cli_alert_warning(
        "{sntutils::big_mark(n_n2_invalid)} records have invalid N2 ",
        "(missing reprate or invalid N1)."
      )
    }
  } else if (needs_n2_for_higher) {
    # N2 not requested but higher levels need it - use N2 = N1 (skip reprate adjustment)
    cli::cli_alert_info(
      "N2 not requested - using N2 = N1 (skipping reporting adjustment)..."
    )

    df <- df |>
      dplyr::mutate(
        n2_cases = n1_cases,
        n2_incidence = n1_incidence
      )
  }

  # ---- calculate N3: care-seeking-adjusted incidence -----------------------

  if ("N3" %in% levels) {
    cli::cli_alert_info(
      "Calculating N3 (care-seeking-adjusted incidence)..."
    )

    df <- df |>
      .calc_n3_internal(scale_factor = rate_multiplier)

    cli::cli_alert_success("N3 calculated (care-seeking adjusted).")
  }

  # ---- calculate N4: public + non-seekers adjusted incidence ----------------

  if ("N4" %in% levels) {
    cli::cli_alert_info(
      "Calculating N4 (public + non-seekers adjusted incidence)..."
    )

    df <- df |>
      .calc_n4_internal(scale_factor = rate_multiplier)

    cli::cli_alert_success("N4 calculated (excludes private sector).")
  }

  # ---- determine highest valid incidence level -----------------------------

  df <- df |>
    .determine_incidence_level(levels = levels)

  # ---- select output columns -----------------------------------------------

  # Check if adm3 exists
  has_adm3 <- "adm3" %in% names(df)

  # Note: Output is at admin-month level (no hf_uid)
  core_cols <- c(
    "adm0",
    "adm1",
    "adm2"
  )

  # Add adm3 if it exists
  if (has_adm3) {
    core_cols <- c(core_cols, "adm3")
  }

  core_cols <- c(
    core_cols,
    "date",
    "year",
    "month",
    "pop",
    "conf"
  )

  if (needs_tpr) {
    core_cols <- c(core_cols, "test", "pres", "tpr")
    # Add data quality diagnostic columns if they exist
    diagnostic_cols <- c("n_facilities", "n_facilities_with_conf",
                         "n_facilities_with_tpr")
    existing_diagnostic_cols <- intersect(diagnostic_cols, names(df))
    if (length(existing_diagnostic_cols) > 0) {
      core_cols <- c(core_cols, existing_diagnostic_cols)
    }
  }

  if (needs_reprate) {
    core_cols <- c(core_cols, "reprate")
  }

  # Add N-level columns that were calculated
  # Note: When higher levels are calculated without lower levels (pass-through),
 # the intermediate columns are still included since they were computed

  # N0 is always needed if any level is calculated
  if (any(c("N0", "N1", "N2", "N3", "N4") %in% levels)) {
    core_cols <- c(core_cols, "n0_cases", "n0_incidence")
  }

  # N1 columns included if N1 requested OR if higher levels need N1 (pass-through)
  if (any(c("N1", "N2", "N3", "N4") %in% levels)) {
    core_cols <- c(core_cols, "n1_cases", "n1_incidence")
  }

  # N2 columns included if N2 requested OR if higher levels need N2 (pass-through)
  if (any(c("N2", "N3", "N4") %in% levels)) {
    core_cols <- c(core_cols, "n2_cases", "n2_incidence")
  }

  if ("N3" %in% levels) {
    core_cols <- c(core_cols, "cs_public", "cs_private", "cs_none",
                   "n3_cases", "n3_incidence", "adj_priv", "adj_none")
  }
  if ("N4" %in% levels) {
    # N4 needs cs_public and cs_none but not cs_private
    if (!"cs_public" %in% core_cols) {
      core_cols <- c(core_cols, "cs_public")
    }
    if (!"cs_none" %in% core_cols) {
      core_cols <- c(core_cols, "cs_none")
    }
    core_cols <- c(core_cols, "n4_cases", "n4_incidence")
    # adj_none may already be added by N3
    if (!"adj_none" %in% core_cols) {
      core_cols <- c(core_cols, "adj_none")
    }
  }

  core_cols <- c(core_cols, "incidence_level")

  if (include_flags) {
    flag_cols <- grep("^flag_", names(df), value = TRUE)
    output_cols <- c(core_cols, flag_cols)
  } else {
    output_cols <- core_cols
  }

  # Select only columns that exist
  output_cols <- intersect(output_cols, names(df))

  df_final <- df |>
    dplyr::select(dplyr::all_of(output_cols))

  cli::cli_alert_success(
    "Incidence calculation complete for ",
    "{sntutils::big_mark(nrow(df_final))} records."
  )

  # ---- Calculate facility-level incidence -----------------------------------

  cli::cli_alert_info("Calculating facility-level incidence...")

  df_facility_final <- .calc_facility_incidence(
    df_facility,
    levels = levels,
    scale_factor = rate_multiplier
  )

  # ---- Build and return output list -----------------------------------------

  cli::cli_alert_info(
    "Building output list with monthly and annual aggregations..."
  )

  output <- .build_output_list(
    df_admin = df_final,
    df_facility = df_facility_final,
    scale_factor = rate_multiplier,
    has_adm3 = has_adm3,
    return_facility = return_facility
  )

  # ---- Rename TPR column in output if different from "tpr" ------------------

  if (needs_tpr && tpr_var != "tpr") {
    # Rename tpr in all monthly tibbles
    for (level in names(output$monthly)) {
      if ("tpr" %in% names(output$monthly[[level]])) {
        output$monthly[[level]] <- output$monthly[[level]] |>
          dplyr::rename(!!tpr_var := tpr)
      }
    }

    # Rename tpr in all annual tibbles
    for (level in names(output$annual)) {
      if ("tpr" %in% names(output$annual[[level]])) {
        output$annual[[level]] <- output$annual[[level]] |>
          dplyr::rename(!!tpr_var := tpr)
      }
    }
  }

  # ---- Validate cascade monotonicity -----------------------------------------

  .validate_cascade_monotonicity(output, levels)

  # Add info about output structure
  cli::cli_alert_success("Output contains:")
  cli::cli_bullets(c(
    "*" = "monthly: adm0, adm1, adm2{if (has_adm3) ', adm3' else ''} (N0-N4)",
    "*" = "annual: adm0, adm1, adm2{if (has_adm3) ', adm3' else ''} (N0-N4)"
  ))

  # ---- Print CLI Summary -----------------------------------------------------

  cli::cli_rule()
  cli::cli_h2("Incidence Cascade Summary")

  # Show formulas used (with suffix if provided)
  cli::cli_h3("Formulas Used")
  sfx <- if (!is.null(suffix)) paste0("_", suffix) else ""

  if ("N0" %in% levels) {
    cli::cli_text("N0: n0_cases{sfx} = conf{sfx}")
    cli::cli_text("    n0_incidence{sfx} = (n0_cases{sfx} / pop{sfx}) * {rate_multiplier}")
  }

  if ("N1" %in% levels) {
    cli::cli_text("N1: n1_cases{sfx} = n0_cases{sfx} + (pres{sfx} * tpr)")
    cli::cli_text("    n1_incidence{sfx} = (n1_cases{sfx} / pop{sfx}) * {rate_multiplier}")
  }
  if ("N2" %in% levels) {
    cli::cli_text("N2: n2_cases{sfx} = n1_cases{sfx} / reprate")
    cli::cli_text("    n2_incidence{sfx} = (n2_cases{sfx} / pop{sfx}) * {rate_multiplier}")
  }
  if ("N3" %in% levels) {
    cli::cli_text(
      "N3: n3_cases{sfx} = n2_cases{sfx} + (n2_cases{sfx} * cs_private/cs_public) + ",
      "(n2_cases{sfx} * cs_none/cs_public)"
    )
    cli::cli_text(
      "    n3_incidence{sfx} = (n3_cases{sfx} / pop{sfx}) * {rate_multiplier}"
    )
  }
  if ("N4" %in% levels) {
    cli::cli_text(
      "N4: n4_cases{sfx} = n2_cases{sfx} * (1 + cs_none/cs_public)"
    )
    cli::cli_text(
      "    n4_incidence{sfx} = (n4_cases{sfx} / pop{sfx}) * {rate_multiplier}"
    )
    cli::cli_text(
      "    Note: Excludes private sector adjustment"
    )
  }
  cli::cli_rule()

  # Data Overview for most recent year
  latest_year <- max(df_final$year, na.rm = TRUE)
  df_latest <- df_final |> dplyr::filter(year == latest_year)

  cli::cli_h3("Data Overview ({latest_year})")
  total_pop <- sum(df_latest$pop, na.rm = TRUE)
  total_conf <- sum(df_latest$conf, na.rm = TRUE)
  cli::cli_text(
    "Total population: {sntutils::big_mark(round(total_pop))}"
  )
  cli::cli_text(
    "Total confirmed cases: {sntutils::big_mark(round(total_conf))}"
  )

  if ("test" %in% names(df_latest)) {
    total_test <- sum(df_latest$test, na.rm = TRUE)
    cli::cli_text(
      "Total tests: {sntutils::big_mark(round(total_test))}"
    )
  }
  if ("pres" %in% names(df_latest)) {
    total_pres <- sum(df_latest$pres, na.rm = TRUE)
    cli::cli_text(
      "Total presumed cases: {sntutils::big_mark(round(total_pres))}"
    )
  }
  if ("tpr" %in% names(df_latest)) {
    tpr_mean <- mean(df_latest$tpr, na.rm = TRUE)
    tpr_median <- stats::median(df_latest$tpr, na.rm = TRUE)
    cli::cli_text(
      "TPR (mean/median): {round(tpr_mean, 3)} / {round(tpr_median, 3)}"
    )
  }
  if ("reprate" %in% names(df_latest)) {
    reprate_mean <- mean(df_latest$reprate, na.rm = TRUE)
    reprate_median <- stats::median(df_latest$reprate, na.rm = TRUE)
    cli::cli_text(
      "Reporting rate (mean/median): {round(reprate_mean, 3)} / ",
      "{round(reprate_median, 3)}"
    )
  }
  if (all(c("cs_public", "cs_private", "cs_none") %in% names(df_latest))) {
    cs_pub_mean <- mean(df_latest$cs_public, na.rm = TRUE)
    cs_priv_mean <- mean(df_latest$cs_private, na.rm = TRUE)
    cs_none_mean <- mean(df_latest$cs_none, na.rm = TRUE)
    cli::cli_text(
      "Care-seeking (mean): Public {round(cs_pub_mean, 3)}, ",
      "Private {round(cs_priv_mean, 3)}, None {round(cs_none_mean, 3)}"
    )
  }
  cli::cli_rule()

  # ---- Round output values ----------------------------------------------------

  output <- .round_incidence_output(output)

  # ---- Apply suffix to output columns if specified ----------------------------

  if (!is.null(suffix)) {
    output <- .apply_suffix_to_output(output, suffix)
  }

  # Results by Region (ADM1) for most recent year
  regional_annual <- output$annual$adm1
  if (nrow(regional_annual) > 0) {
    latest_year <- max(regional_annual$year, na.rm = TRUE)
    latest_regional <- regional_annual |>
      dplyr::filter(year == latest_year) |>
      dplyr::select(-year)

    cli::cli_h3("Results by Region (ADM1) - {latest_year}")
    print(latest_regional, n = Inf)
    cli::cli_rule()
  }

  # Results by Year
  national_annual <- output$annual$adm0
  if (nrow(national_annual) > 0) {
    cli::cli_h3("Results by Year")
    national_summary <- national_annual |>
      dplyr::select(-adm0) |>
      dplyr::arrange(year)
    print(national_summary, n = Inf)
    cli::cli_rule()
  }

  return(output)
}


# =============================================================================
# Internal helper functions for N0-N3 calculations
# =============================================================================

#' Round Incidence Output Values - Internal
#'
#' Rounds case counts to whole numbers and incidence rates to 2 decimal places.
#'
#' @param output The output list from calc_incidence.
#'
#' @return The output list with rounded values.
#'
#' @keywords internal
.round_incidence_output <- function(output) {
  # Columns to round to whole numbers (cases/counts)
  integer_cols <- c(
    "n0_cases", "n1_cases", "n2_cases", "n3_cases", "n4_cases",
    "pop", "conf", "test", "pres"
  )

  # Columns to round to 2 decimal places (rates/proportions)
  decimal_cols <- c(
    "n0_incidence", "n1_incidence", "n2_incidence",
    "n3_incidence", "n4_incidence", "tpr", "reprate",
    "cs_public", "cs_private", "cs_none",
    "adj_priv", "adj_none"
  )

  # Intermediate columns to drop from final output
  cols_to_drop <- c("adj_priv", "adj_none", "adj_pub")

  # Helper to round columns in a single data frame
  round_df <- function(df) {
    if (is.null(df) || nrow(df) == 0) return(df)

    # Round integer columns to whole numbers
    for (col in intersect(integer_cols, names(df))) {
      df[[col]] <- round(df[[col]], 0)
    }

    # Round decimal columns to 2 decimal places
    for (col in intersect(decimal_cols, names(df))) {
      df[[col]] <- round(df[[col]], 2)
    }

    # Drop intermediate calculation columns
    cols_present <- intersect(cols_to_drop, names(df))
    if (length(cols_present) > 0) {
      df <- df |> dplyr::select(-dplyr::all_of(cols_present))
    }

    df
  }

  # Apply to all monthly tibbles
  if (!is.null(output$monthly)) {
    output$monthly <- lapply(output$monthly, round_df)
  }

  # Apply to all annual tibbles
  if (!is.null(output$annual)) {
    output$annual <- lapply(output$annual, round_df)
  }

  output
}


#' Validate Cascade Monotonicity - Internal
#'
#' Checks that each incidence level is >= the previous level. Violations
#' indicate potential data issues (e.g., reporting rate > 100%).
#'
#' @param output The output list from calc_incidence.
#' @param levels Character vector of levels calculated.
#'
#' @return Invisibly returns TRUE if valid, FALSE if violations found.
#'   Emits warnings for any violations.
#'
#' @keywords internal
.validate_cascade_monotonicity <- function(output, levels) {
  violations <- list()

  # Helper to check a single data frame
  check_df <- function(df, df_name) {
    if (is.null(df) || nrow(df) == 0) return(NULL)

    issues <- list()

    # N1 >= N0 (testing adjustment should not decrease cases)
    if (all(c("n1_cases", "n0_cases") %in% names(df))) {
      bad <- df |>
        dplyr::filter(!is.na(n1_cases) & !is.na(n0_cases) & n1_cases < n0_cases)
      if (nrow(bad) > 0) {
        issues$n1_lt_n0 <- nrow(bad)
      }
    }

    # N2 >= N1 (reporting adjustment should not decrease cases)
    if (all(c("n2_cases", "n1_cases") %in% names(df))) {
      bad <- df |>
        dplyr::filter(!is.na(n2_cases) & !is.na(n1_cases) & n2_cases < n1_cases)
      if (nrow(bad) > 0) {
        issues$n2_lt_n1 <- nrow(bad)
      }
    }

    # N3 >= N2 (care-seeking adjustment should not decrease cases)
    if (all(c("n3_cases", "n2_cases") %in% names(df))) {
      bad <- df |>
        dplyr::filter(!is.na(n3_cases) & !is.na(n2_cases) & n3_cases < n2_cases)
      if (nrow(bad) > 0) {
        issues$n3_lt_n2 <- nrow(bad)
      }
    }

    # N4 >= N2 (non-seeker adjustment should not decrease cases)
    if (all(c("n4_cases", "n2_cases") %in% names(df))) {
      bad <- df |>
        dplyr::filter(!is.na(n4_cases) & !is.na(n2_cases) & n4_cases < n2_cases)
      if (nrow(bad) > 0) {
        issues$n4_lt_n2 <- nrow(bad)
      }
    }

    # N3 >= N4 (N3 includes private sector, N4 does not)
    if (all(c("n3_cases", "n4_cases") %in% names(df))) {
      bad <- df |>
        dplyr::filter(!is.na(n3_cases) & !is.na(n4_cases) & n3_cases < n4_cases)
      if (nrow(bad) > 0) {
        issues$n3_lt_n4 <- nrow(bad)
      }
    }

    if (length(issues) > 0) {
      return(list(df_name = df_name, issues = issues))
    }
    NULL
  }

  # Check all output data frames
  for (period in c("monthly", "annual")) {
    if (!is.null(output[[period]])) {
      for (adm_level in names(output[[period]])) {
        df_name <- paste0(period, "$", adm_level)
        result <- check_df(output[[period]][[adm_level]], df_name)
        if (!is.null(result)) {
          violations <- c(violations, list(result))
        }
      }
    }
  }

  # Emit warnings if violations found
  if (length(violations) > 0) {
    cli::cli_warn(c(
      "!" = "Cascade monotonicity violations detected.",
      "i" = "Each adjustment level should be >= the previous level.",
      "i" = paste0(
        "Violations may indicate data issues ",
        "(e.g., reprate > 100%, negative pres*tpr)."
      )
    ))

    for (v in violations) {
      for (issue_name in names(v$issues)) {
        n_bad <- v$issues[[issue_name]]
        msg <- switch(
          issue_name,
          "n1_lt_n0" = "N1 < N0 (testing adjustment decreased cases)",
          "n2_lt_n1" = "N2 < N1 (reporting rate > 100%?)",
          "n3_lt_n2" = "N3 < N2 (care-seeking adjustment decreased cases)",
          "n4_lt_n2" = "N4 < N2 (non-seeker adjustment decreased cases)",
          "n3_lt_n4" = "N3 < N4 (private sector adjustment is negative?)",
          issue_name
        )
        cli::cli_warn("
          {v$df_name}: {n_bad} record{?s} with {msg}"
        )
      }
    }

    return(invisible(FALSE))
  }

  invisible(TRUE)
}


#' Apply Suffix to Incidence Output Columns - Internal
#'
#' Renames incidence-related columns (n0_cases, n0_incidence, n1_cases, etc.)
#' by appending a suffix. Used to distinguish outputs for different populations
#' (e.g., u5 for under-5, all_ages, etc.).
#'
#' @param output The output list from calc_incidence containing monthly and
#'   annual tibbles at each admin level.
#' @param suffix Character string to append to column names (e.g., "u5").
#'
#' @return The output list with renamed columns.
#'
#' @keywords internal
.apply_suffix_to_output <- function(output, suffix) {
  # Columns to rename: core data columns + incidence cascade outputs
  cols_to_rename <- c(
    # Core data columns (population-specific)
    "pop", "conf", "test", "pres",
    # Incidence cascade outputs
    "n0_cases", "n0_incidence",
    "n1_cases", "n1_incidence",
    "n2_cases", "n2_incidence",
    "n3_cases", "n3_incidence",
    "n4_cases", "n4_incidence"
  )

  # Helper to rename columns in a single data frame
  rename_df <- function(df) {
    if (is.null(df) || nrow(df) == 0) return(df)

    existing_cols <- intersect(cols_to_rename, names(df))
    if (length(existing_cols) == 0) return(df)

    # dplyr::rename with all_of() expects: new_name = old_name
    # So names of the vector are the NEW names (with suffix)
    # and values are the OLD names (existing columns)
    old_names <- existing_cols
    names(old_names) <- paste0(existing_cols, "_", suffix)

    dplyr::rename(df, dplyr::all_of(old_names))
  }

  # Apply to all monthly tibbles
  if (!is.null(output$monthly)) {
    output$monthly <- lapply(output$monthly, rename_df)
  }

  # Apply to all annual tibbles
  if (!is.null(output$annual)) {
    output$annual <- lapply(output$annual, rename_df)
  }

  output
}


#' Aggregate Facility-Month Data to Admin-Month Level - Internal
#'
#' Aggregates facility-month level data to administrative unit-month level.
#' This is used as the first step in the incidence cascade to produce
#' output at the admin-month level rather than facility-month level.
#'
#' @param df Data frame with standardized column names at facility-month level
#'
#' @return Data frame aggregated to admin-month level with:
#'   - Sum: pop, conf, test, pres
#'   - Recalculated: tpr = sum(conf)/sum(test)
#'   - Mean: reprate
#'   - First: cs_public, cs_private, cs_none
#'   - Diagnostics: n_facilities, n_facilities_with_conf, n_facilities_with_tpr
#'
#' @keywords internal
.aggregate_to_admin_month <- function(df) {

  # Check which columns exist for conditional aggregation
  has_cs <- all(c("cs_public", "cs_private", "cs_none") %in% names(df))
  has_reprate <- "reprate" %in% names(df)
  has_tpr <- "tpr" %in% names(df)
  has_adm3 <- "adm3" %in% names(df)

  # Build grouping variables dynamically

  group_vars <- c("adm0", "adm1", "adm2")
  if (has_adm3) {
    group_vars <- c(group_vars, "adm3")
  }
  group_vars <- c(group_vars, "year", "month", "date")

  df_agg <- df |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) |>
    dplyr::summarise(
      # Population is at admin level (same for all facilities)
      pop = dplyr::first(pop),
      conf = sum(conf, na.rm = TRUE),
      test = if ("test" %in% names(df)) {
        sum(test, na.rm = TRUE)
      } else {
        NA_real_
      },
      pres = if ("pres" %in% names(df)) {
        sum(pres, na.rm = TRUE)
      } else {
        NA_real_
      },
      # Recalculate TPR at admin level: total confirmed / total tested
      tpr = if (has_tpr) {
        sum(conf, na.rm = TRUE) / sum(test, na.rm = TRUE)
      } else {
        NA_real_
      },
      # Average reporting rate across facilities
      reprate = if (has_reprate) {
        mean(reprate, na.rm = TRUE)
      } else {
        NA_real_
      },
      # Care-seeking proportions (same for all facilities in admin)
      cs_public = if (has_cs) {
        dplyr::first(cs_public)
      } else {
        NA_real_
      },
      cs_private = if (has_cs) {
        dplyr::first(cs_private)
      } else {
        NA_real_
      },
      cs_none = if (has_cs) {
        dplyr::first(cs_none)
      } else {
        NA_real_
      },
      # Diagnostic outputs for data quality monitoring
      n_facilities = dplyr::n_distinct(hf_uid),
      n_facilities_with_conf = sum(!is.na(conf) & conf > 0, na.rm = TRUE),
      n_facilities_with_tpr = if (has_tpr) {
        sum(!is.na(tpr), na.rm = TRUE)
      } else {
        NA_integer_
      },
      .groups = "drop"
    )

  # Handle Inf from division by zero in TPR

  if (has_tpr) {
    df_agg <- df_agg |>
      dplyr::mutate(
        tpr = dplyr::if_else(is.infinite(tpr) | is.nan(tpr), NA_real_, tpr)
      )
  }

  df_agg
}


#' Calculate N0 (Crude Incidence) - Internal
#'
#' @param df Data frame with standardized column names
#' @param scale_factor Denominator for incidence rate
#'
#' @return Data frame with n0_cases and n0_incidence columns added
#'
#' @keywords internal
.calc_n0_internal <- function(df, scale_factor) {
  df |>
    dplyr::mutate(
      n0_cases = conf,
      n0_incidence = dplyr::if_else(
        pop > 0 & !is.na(pop) & !is.na(conf),
        (n0_cases / pop) * scale_factor,
        NA_real_
      )
    )
}


#' Calculate N1 (Testing-Adjusted Incidence) - Internal
#'
#' N1 = conf + (pres * tpr)
#'
#' When n1_cases already exists (from facility-level aggregation), it is used
#' directly to calculate incidence. Otherwise, N1 cases are calculated from
#' the aggregated values (legacy/fallback behavior).
#'
#' @param df Data frame with standardized column names
#' @param scale_factor Denominator for incidence rate
#'
#' @return Data frame with n1_cases and n1_incidence columns added
#'
#' @keywords internal
.calc_n1_internal <- function(df, scale_factor) {
  # Check if n1_cases already exists (from facility-level aggregation)
  if ("n1_cases" %in% names(df)) {
    # N1 cases were aggregated from facility level - just calculate incidence
    df <- df |>
      dplyr::mutate(
        n1_incidence = dplyr::if_else(
          pop > 0 & !is.na(pop) & !is.na(n1_cases),
          (n1_cases / pop) * scale_factor,
          NA_real_
        ),
        flag_n1_invalid = is.na(n1_incidence)
      )
  } else {
    # Fallback: calculate N1 from aggregated values (legacy/edge case)
    df <- df |>
      dplyr::mutate(
        tpr_clean = dplyr::case_when(
          is.na(tpr) ~ NA_real_,
          tpr < 0 ~ 0,
          tpr > 1 ~ 1,
          TRUE ~ tpr
        ),
        pres_clean = dplyr::if_else(is.na(pres) | pres < 0, 0, pres),
        n1_cases = dplyr::if_else(
          !is.na(conf) & !is.na(tpr_clean),
          conf + pres_clean * tpr_clean,
          NA_real_
        ),
        n1_incidence = dplyr::if_else(
          pop > 0 & !is.na(pop) & !is.na(n1_cases),
          (n1_cases / pop) * scale_factor,
          NA_real_
        ),
        flag_n1_invalid = is.na(n1_incidence)
      ) |>
      dplyr::select(-tpr_clean, -pres_clean)
  }
  df
}


#' Calculate N2 (Reporting-Adjusted Incidence) - Internal
#'
#' N2 = N1 / reprate
#'
#' @param df Data frame with standardized column names
#' @param scale_factor Denominator for incidence rate
#'
#' @return Data frame with n2_cases and n2_incidence columns added
#'
#' @keywords internal
.calc_n2_internal <- function(df, scale_factor) {
  df |>
    dplyr::mutate(
      # Ensure reprate is bounded [0, 1]
      reprate_clean = dplyr::case_when(
        is.na(reprate) ~ NA_real_,
        reprate <= 0 ~ NA_real_,
        reprate > 1 ~ 1,
        TRUE ~ reprate
      ),
      # Calculate N2 cases (adjust for incomplete reporting)
      n2_cases = dplyr::if_else(
        !is.na(n1_cases) & !is.na(reprate_clean) & reprate_clean > 0,
        n1_cases / reprate_clean,
        NA_real_
      ),
      # Calculate N2 incidence
      n2_incidence = dplyr::if_else(
        pop > 0 & !is.na(pop) & !is.na(n2_cases),
        (n2_cases / pop) * scale_factor,
        NA_real_
      ),
      # Flag invalid N2
      flag_n2_invalid = is.na(n2_incidence)
    ) |>
    dplyr::select(-reprate_clean)
}


#' Calculate N3 (Care-Seeking-Adjusted Incidence) - Internal
#'
#' Calculates N3 by applying the CSB adjustment to N2 cases.
#'
#' N3 = N2 * (1 + CS_Priv/CS_Pub + CS_None/CS_Pub)
#'
#' @param df Data frame with standardized column names at admin-month level
#' @param scale_factor Denominator for incidence rate
#'
#' @return Data frame with n3_cases and n3_incidence columns added
#'
#' @keywords internal
.calc_n3_internal <- function(df, scale_factor) {
  df |>
    dplyr::mutate(
      # Validate care-seeking proportions
      cs_public_clean = dplyr::case_when(
        is.na(cs_public) ~ NA_real_,
        cs_public <= 0 ~ NA_real_,
        cs_public > 1 ~ 1,
        TRUE ~ cs_public
      ),
      cs_private_clean = dplyr::case_when(
        is.na(cs_private) ~ 0,
        cs_private < 0 ~ 0,
        cs_private > 1 ~ 1,
        TRUE ~ cs_private
      ),
      cs_none_clean = dplyr::case_when(
        is.na(cs_none) ~ 0,
        cs_none < 0 ~ 0,
        cs_none > 1 ~ 1,
        TRUE ~ cs_none
      ),
      # Calculate adjustment factors
      adj_priv = dplyr::if_else(
        !is.na(cs_public_clean) & cs_public_clean > 0,
        cs_private_clean / cs_public_clean,
        0
      ),
      adj_none = dplyr::if_else(
        !is.na(cs_public_clean) & cs_public_clean > 0,
        cs_none_clean / cs_public_clean,
        0
      ),
      # Calculate N3 cases
      n3_cases = dplyr::if_else(
        !is.na(n2_cases) & !is.na(cs_public_clean),
        n2_cases + (n2_cases * adj_priv) + (n2_cases * adj_none),
        NA_real_
      ),
      # Calculate N3 incidence
      n3_incidence = dplyr::if_else(
        pop > 0 & !is.na(pop) & !is.na(n3_cases),
        (n3_cases / pop) * scale_factor,
        NA_real_
      ),
      # Flag invalid N3
      flag_n3_invalid = is.na(n3_incidence)
    ) |>
    dplyr::select(-cs_public_clean, -cs_private_clean, -cs_none_clean)
}


#' Calculate N4 (Public + Non-Seekers Adjusted Incidence) - Internal
#'
#' Calculates N4 by applying only the non-seeker adjustment (excludes private
#' sector).
#'
#' N4 = N2 * (1 + CS_None/CS_Pub)
#'
#' @param df Data frame with standardized column names at admin-month level
#' @param scale_factor Denominator for incidence rate
#'
#' @return Data frame with n4_cases and n4_incidence columns added
#'
#' @keywords internal
.calc_n4_internal <- function(df, scale_factor) {
  # If adj_none already exists (from N3 calculation), use it directly
  if ("adj_none" %in% names(df)) {
    df <- df |>
      dplyr::mutate(
        # Calculate N4 cases
        n4_cases = dplyr::if_else(
          !is.na(n2_cases) & !is.na(adj_none),
          n2_cases * (1 + adj_none),
          NA_real_
        ),
        # Calculate N4 incidence
        n4_incidence = dplyr::if_else(
          pop > 0 & !is.na(pop) & !is.na(n4_cases),
          (n4_cases / pop) * scale_factor,
          NA_real_
        ),
        # Flag invalid N4
        flag_n4_invalid = is.na(n4_incidence)
      )
  } else {
    # Calculate adj_none from scratch
    df <- df |>
      dplyr::mutate(
        # Validate care-seeking proportions
        cs_public_clean = dplyr::case_when(
          is.na(cs_public) ~ NA_real_,
          cs_public <= 0 ~ NA_real_,
          cs_public > 1 ~ 1,
          TRUE ~ cs_public
        ),
        cs_none_clean = dplyr::case_when(
          is.na(cs_none) ~ 0,
          cs_none < 0 ~ 0,
          cs_none > 1 ~ 1,
          TRUE ~ cs_none
        ),
        # Calculate adjustment factor (non-seekers only, excludes private)
        adj_none = dplyr::if_else(
          !is.na(cs_public_clean) & cs_public_clean > 0,
          cs_none_clean / cs_public_clean,
          0
        ),
        # Calculate N4 cases
        n4_cases = dplyr::if_else(
          !is.na(n2_cases) & !is.na(cs_public_clean),
          n2_cases * (1 + adj_none),
          NA_real_
        ),
        # Calculate N4 incidence
        n4_incidence = dplyr::if_else(
          pop > 0 & !is.na(pop) & !is.na(n4_cases),
          (n4_cases / pop) * scale_factor,
          NA_real_
        ),
        # Flag invalid N4
        flag_n4_invalid = is.na(n4_incidence)
      ) |>
      dplyr::select(-cs_public_clean, -cs_none_clean)
  }

  df
}


#' Determine Highest Valid Incidence Level - Internal
#'
#' @param df Data frame with N0-N4 calculations
#' @param levels Character vector of requested levels
#'
#' @return Data frame with incidence_level column added
#'
#' @keywords internal
.determine_incidence_level <- function(df, levels) {
  # Build conditions only for levels that were calculated
  # Priority order: N3 > N4 > N2 > N1 > N0
  # N3 takes priority over N4 as it's the most complete estimate
  conditions <- list()

  if ("N3" %in% levels && "n3_incidence" %in% names(df)) {
    conditions <- c(
      conditions,
      list(quote(!is.na(n3_incidence) ~ "N3"))
    )
  }

  if ("N4" %in% levels && "n4_incidence" %in% names(df)) {
    conditions <- c(
      conditions,
      list(quote(!is.na(n4_incidence) ~ "N4"))
    )
  }

  if ("N2" %in% levels && "n2_incidence" %in% names(df)) {
    conditions <- c(
      conditions,
      list(quote(!is.na(n2_incidence) ~ "N2"))
    )
  }

  if ("N1" %in% levels && "n1_incidence" %in% names(df)) {
    conditions <- c(
      conditions,
      list(quote(!is.na(n1_incidence) ~ "N1"))
    )
  }

  if ("N0" %in% levels && "n0_incidence" %in% names(df)) {
    conditions <- c(
      conditions,
      list(quote(!is.na(n0_incidence) ~ "N0"))
    )
  }

  # Add default condition
  conditions <- c(conditions, list(quote(TRUE ~ NA_character_)))

  # Use case_when with dynamic conditions
  df |>
    dplyr::mutate(
      incidence_level = dplyr::case_when(!!!conditions)
    )
}


#' Aggregate Incidence Data to Specified Admin Level - Internal
#'
#' Aggregates monthly incidence data to a specified administrative level
#' (adm0, adm1, adm2, or adm3).
#'
#' @param df Data frame with monthly incidence data
#' @param admin_level Character. One of "adm0", "adm1", "adm2", "adm3"
#' @param scale_factor Numeric. Scale factor for incidence calculation
#' @param time_period Character. Either "monthly" or "annual"
#'
#' @return Aggregated tibble at specified admin level
#'
#' @keywords internal
.aggregate_to_level <- function(
    df,
    admin_level,
    scale_factor,
    time_period = "monthly"
) {

  has_adm3 <- "adm3" %in% names(df)

  # Define grouping columns based on admin level
  admin_cols <- switch(
    admin_level,
    "adm0" = "adm0",
    "adm1" = c("adm0", "adm1"),
    "adm2" = c("adm0", "adm1", "adm2"),
    "adm3" = if (has_adm3) {
      c("adm0", "adm1", "adm2", "adm3")
    } else {
      return(NULL)
    }
  )

  # Check which incidence columns exist
  has_n0 <- "n0_cases" %in% names(df)
  has_n1 <- "n1_cases" %in% names(df)
  has_n2 <- "n2_cases" %in% names(df)
  has_cs <- all(c("cs_public", "cs_private", "cs_none") %in% names(df))

  if (time_period == "monthly") {
    # For monthly: aggregate to admin-month level (sum across lower levels)
    group_cols <- c(admin_cols, "year", "month", "date")

    df_agg <- df |>
      dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
      dplyr::summarise(
        pop = sum(pop, na.rm = TRUE),
        conf = sum(conf, na.rm = TRUE),
        test = if ("test" %in% names(df)) {
          sum(test, na.rm = TRUE)
        } else {
          NA_real_
        },
        pres = if ("pres" %in% names(df)) {
          sum(pres, na.rm = TRUE)
        } else {
          NA_real_
        },
        tpr = if ("tpr" %in% names(df)) {
          mean(tpr, na.rm = TRUE)
        } else {
          NA_real_
        },
        reprate = if ("reprate" %in% names(df)) {
          mean(reprate, na.rm = TRUE)
        } else {
          NA_real_
        },
        cs_public = if ("cs_public" %in% names(df)) {
          mean(cs_public, na.rm = TRUE)
        } else {
          NA_real_
        },
        cs_private = if ("cs_private" %in% names(df)) {
          mean(cs_private, na.rm = TRUE)
        } else {
          NA_real_
        },
        cs_none = if ("cs_none" %in% names(df)) {
          mean(cs_none, na.rm = TRUE)
        } else {
          NA_real_
        },
        n0_cases = if (has_n0) {
          sum(n0_cases, na.rm = TRUE)
        } else {
          NA_real_
        },
        n1_cases = if (has_n1) {
          sum(n1_cases, na.rm = TRUE)
        } else {
          NA_real_
        },
        n2_cases = if (has_n2) {
          sum(n2_cases, na.rm = TRUE)
        } else {
          NA_real_
        },
        .groups = "drop"
      )
  } else {
    # For annual: TWO-STEP aggregation
    # Step 1: First aggregate to admin-month level (sum across lower levels)
    monthly_cols <- c(admin_cols, "year", "month")

    df_monthly <- df |>
      dplyr::group_by(dplyr::across(dplyr::all_of(monthly_cols))) |>
      dplyr::summarise(
        pop = sum(pop, na.rm = TRUE),
        conf = sum(conf, na.rm = TRUE),
        test = if ("test" %in% names(df)) {
          sum(test, na.rm = TRUE)
        } else {
          NA_real_
        },
        pres = if ("pres" %in% names(df)) {
          sum(pres, na.rm = TRUE)
        } else {
          NA_real_
        },
        tpr = if ("tpr" %in% names(df)) {
          mean(tpr, na.rm = TRUE)
        } else {
          NA_real_
        },
        reprate = if ("reprate" %in% names(df)) {
          mean(reprate, na.rm = TRUE)
        } else {
          NA_real_
        },
        cs_public = if ("cs_public" %in% names(df)) {
          mean(cs_public, na.rm = TRUE)
        } else {
          NA_real_
        },
        cs_private = if ("cs_private" %in% names(df)) {
          mean(cs_private, na.rm = TRUE)
        } else {
          NA_real_
        },
        cs_none = if ("cs_none" %in% names(df)) {
          mean(cs_none, na.rm = TRUE)
        } else {
          NA_real_
        },
        n0_cases = if (has_n0) {
          sum(n0_cases, na.rm = TRUE)
        } else {
          NA_real_
        },
        n1_cases = if (has_n1) {
          sum(n1_cases, na.rm = TRUE)
        } else {
          NA_real_
        },
        n2_cases = if (has_n2) {
          sum(n2_cases, na.rm = TRUE)
        } else {
          NA_real_
        },
        .groups = "drop"
      )

    # Step 2: Aggregate monthly to annual (mean pop, sum cases)
    annual_cols <- c(admin_cols, "year")

    df_agg <- df_monthly |>
      dplyr::group_by(dplyr::across(dplyr::all_of(annual_cols))) |>
      dplyr::summarise(
        pop = mean(pop, na.rm = TRUE),  # Mean of monthly totals
        conf = sum(conf, na.rm = TRUE),
        test = sum(test, na.rm = TRUE),
        pres = sum(pres, na.rm = TRUE),
        tpr = mean(tpr, na.rm = TRUE),
        reprate = mean(reprate, na.rm = TRUE),
        cs_public = mean(cs_public, na.rm = TRUE),
        cs_private = mean(cs_private, na.rm = TRUE),
        cs_none = mean(cs_none, na.rm = TRUE),
        n0_cases = if (has_n0) sum(n0_cases, na.rm = TRUE) else NA_real_,
        n1_cases = if (has_n1) sum(n1_cases, na.rm = TRUE) else NA_real_,
        n2_cases = if (has_n2) sum(n2_cases, na.rm = TRUE) else NA_real_,
        .groups = "drop"
      )
  }

  # Calculate incidence for N0-N4
  df_agg <- df_agg |>
    dplyr::mutate(
      n0_incidence = if (has_n0) {
        dplyr::if_else(pop > 0, (n0_cases / pop) * scale_factor, NA_real_)
      } else {
        NA_real_
      },
      n1_incidence = if (has_n1) {
        dplyr::if_else(pop > 0, (n1_cases / pop) * scale_factor, NA_real_)
      } else {
        NA_real_
      },
      n2_incidence = if (has_n2) {
        dplyr::if_else(pop > 0, (n2_cases / pop) * scale_factor, NA_real_)
      } else {
        NA_real_
      }
    )

  # Calculate N3 and N4 for both monthly and annual
  if (has_n2 && has_cs) {
    df_agg <- df_agg |>
      dplyr::mutate(
        # Calculate adjustment factors from care-seeking proportions
        adj_priv = dplyr::if_else(
          !is.na(cs_public) & cs_public > 0,
          cs_private / cs_public,
          0
        ),
        adj_none = dplyr::if_else(
          !is.na(cs_public) & cs_public > 0,
          cs_none / cs_public,
          0
        ),
        # N3 = N2 + (N2 * adj_priv) + (N2 * adj_none)
        n3_cases = dplyr::if_else(
          !is.na(n2_cases) & !is.na(cs_public) & cs_public > 0,
          n2_cases + (n2_cases * adj_priv) + (n2_cases * adj_none),
          NA_real_
        ),
        n3_incidence = dplyr::if_else(
          pop > 0 & !is.na(n3_cases),
          (n3_cases / pop) * scale_factor,
          NA_real_
        ),
        # N4 = N2 * (1 + adj_none) - excludes private sector
        n4_cases = dplyr::if_else(
          !is.na(n2_cases) & !is.na(cs_public) & cs_public > 0,
          n2_cases * (1 + adj_none),
          NA_real_
        ),
        n4_incidence = dplyr::if_else(
          pop > 0 & !is.na(n4_cases),
          (n4_cases / pop) * scale_factor,
          NA_real_
        )
      )
  }

  # Remove columns that are all NA
  df_agg <- df_agg |>
    dplyr::select(dplyr::where(~ !all(is.na(.x))))

  tibble::as_tibble(df_agg)
}


#' Calculate Incidence at Facility Level - Internal
#'
#' Calculates N0-N4 incidence at facility-month level.
#'
#' @param df Data frame with facility-month level data
#' @param levels Character vector of requested incidence levels
#' @param scale_factor Numeric. Scale factor for incidence calculation
#'
#' @return Data frame with facility-level incidence calculations
#'
#' @keywords internal
.calc_facility_incidence <- function(df, levels, scale_factor) {

  # Calculate N0 at facility level
  if ("N0" %in% levels) {
    df <- df |>
      dplyr::mutate(
        n0_cases = conf,
        n0_incidence = dplyr::if_else(
          pop > 0 & !is.na(pop) & !is.na(conf),
          (n0_cases / pop) * scale_factor,
          NA_real_
        )
      )
  }

  # Calculate N1 at facility level
  if ("N1" %in% levels && "tpr" %in% names(df)) {
    df <- df |>
      dplyr::mutate(
        tpr_clean = dplyr::case_when(
          is.na(tpr) ~ NA_real_,
          tpr < 0 ~ 0,
          tpr > 1 ~ 1,
          TRUE ~ tpr
        ),
        pres_clean = dplyr::if_else(is.na(pres) | pres < 0, 0, pres),
        n1_cases = dplyr::if_else(
          !is.na(conf) & !is.na(tpr_clean),
          conf + pres_clean * tpr_clean,
          NA_real_
        ),
        n1_incidence = dplyr::if_else(
          pop > 0 & !is.na(pop) & !is.na(n1_cases),
          (n1_cases / pop) * scale_factor,
          NA_real_
        )
      ) |>
      dplyr::select(-tpr_clean, -pres_clean)
  }

  # Calculate N2 at facility level
  has_n2_req <- "N2" %in% levels &&
    "reprate" %in% names(df) &&
    "n1_cases" %in% names(df)
  if (has_n2_req) {
    df <- df |>
      dplyr::mutate(
        reprate_clean = dplyr::case_when(
          is.na(reprate) ~ NA_real_,
          reprate <= 0 ~ NA_real_,
          reprate > 1 ~ 1,
          TRUE ~ reprate
        ),
        n2_cases = dplyr::if_else(
          !is.na(n1_cases) & !is.na(reprate_clean),
          n1_cases / reprate_clean,
          NA_real_
        ),
        n2_incidence = dplyr::if_else(
          pop > 0 & !is.na(pop) & !is.na(n2_cases),
          (n2_cases / pop) * scale_factor,
          NA_real_
        )
      ) |>
      dplyr::select(-reprate_clean)
  }

  # Calculate N3 at facility level
  if ("N3" %in% levels && "cs_public" %in% names(df)) {
    df <- df |>
      dplyr::mutate(
        cs_public_clean = dplyr::case_when(
          is.na(cs_public) ~ NA_real_,
          cs_public <= 0 ~ NA_real_,
          cs_public > 1 ~ 1,
          TRUE ~ cs_public
        ),
        cs_private_clean = dplyr::case_when(
          is.na(cs_private) ~ 0,
          cs_private < 0 ~ 0,
          cs_private > 1 ~ 1,
          TRUE ~ cs_private
        ),
        cs_none_clean = dplyr::case_when(
          is.na(cs_none) ~ 0,
          cs_none < 0 ~ 0,
          cs_none > 1 ~ 1,
          TRUE ~ cs_none
        ),
        adj_priv = dplyr::if_else(
          !is.na(cs_public_clean) & cs_public_clean > 0,
          cs_private_clean / cs_public_clean,
          0
        ),
        adj_none = dplyr::if_else(
          !is.na(cs_public_clean) & cs_public_clean > 0,
          cs_none_clean / cs_public_clean,
          0
        ),
        n3_cases = dplyr::if_else(
          !is.na(n2_cases) & !is.na(cs_public_clean),
          n2_cases + (n2_cases * adj_priv) + (n2_cases * adj_none),
          NA_real_
        ),
        n3_incidence = dplyr::if_else(
          pop > 0 & !is.na(pop) & !is.na(n3_cases),
          (n3_cases / pop) * scale_factor,
          NA_real_
        )
      ) |>
      dplyr::select(-cs_public_clean, -cs_private_clean, -cs_none_clean)
  }

  # Calculate N4 at facility level
  if ("N4" %in% levels && "cs_public" %in% names(df)) {
    if ("adj_none" %in% names(df)) {
      df <- df |>
        dplyr::mutate(
          n4_cases = dplyr::if_else(
            !is.na(n2_cases) & !is.na(adj_none),
            n2_cases * (1 + adj_none),
            NA_real_
          ),
          n4_incidence = dplyr::if_else(
            pop > 0 & !is.na(pop) & !is.na(n4_cases),
            (n4_cases / pop) * scale_factor,
            NA_real_
          )
        )
    } else {
      df <- df |>
        dplyr::mutate(
          cs_public_clean = dplyr::case_when(
            is.na(cs_public) ~ NA_real_,
            cs_public <= 0 ~ NA_real_,
            cs_public > 1 ~ 1,
            TRUE ~ cs_public
          ),
          cs_none_clean = dplyr::case_when(
            is.na(cs_none) ~ 0,
            cs_none < 0 ~ 0,
            cs_none > 1 ~ 1,
            TRUE ~ cs_none
          ),
          adj_none = dplyr::if_else(
            !is.na(cs_public_clean) & cs_public_clean > 0,
            cs_none_clean / cs_public_clean,
            0
          ),
          n4_cases = dplyr::if_else(
            !is.na(n2_cases) & !is.na(cs_public_clean),
            n2_cases * (1 + adj_none),
            NA_real_
          ),
          n4_incidence = dplyr::if_else(
            pop > 0 & !is.na(pop) & !is.na(n4_cases),
            (n4_cases / pop) * scale_factor,
            NA_real_
          )
        ) |>
        dplyr::select(-cs_public_clean, -cs_none_clean)
    }
  }

  tibble::as_tibble(df)
}


#' Build Output List with Monthly and Annual Aggregations - Internal
#'
#' Creates a structured list with monthly and annual aggregations at each
#' admin level (adm0, adm1, adm2, adm3). Optionally includes facility-level
#' data with diagnostic flags.
#'
#' @param df_admin Data frame with monthly incidence data at admin level
#' @param df_facility Data frame with monthly incidence data at facility level
#' @param scale_factor Numeric. Scale factor for incidence calculation
#' @param has_adm3 Logical. Whether adm3 column exists
#' @param return_facility Logical. Whether to include facility-level data in output
#'
#' @return Named list with monthly and annual components, and optionally facility
#'
#' @keywords internal
.build_output_list <- function(
    df_admin,
    df_facility,
    scale_factor,
    has_adm3 = FALSE,
    return_facility = FALSE
) {

  admin_levels <- c("adm0", "adm1", "adm2")
  if (has_adm3) {
    admin_levels <- c(admin_levels, "adm3")
  }

  # Build monthly list (admin levels only, no hf)
  monthly <- list()

  # Add admin-level monthly aggregations
  for (level in admin_levels) {
    monthly[[level]] <- .aggregate_to_level(
      df_admin, level, scale_factor, "monthly"
    )
  }

  # Build annual list (admin levels only, no hf)
  annual <- list()

  # Add admin-level annual aggregations
  for (level in admin_levels) {
    annual[[level]] <- .aggregate_to_level(
      df_admin, level, scale_factor, "annual"
    )
  }

  output <- list(
    monthly = monthly,
    annual = annual
  )

  # Add facility-level data if requested
  if (return_facility && !is.null(df_facility)) {
    output$facility <- list(
      monthly = df_facility
    )
  }

  output
}


#' Aggregate Facility Data to Annual Level - Internal
#'
#' Aggregates facility-month data to facility-year level.
#'
#' @param df Data frame with facility-month level data
#' @param scale_factor Numeric. Scale factor for incidence calculation
#'
#' @return Aggregated tibble at facility-year level
#'
#' @keywords internal
.aggregate_facility_to_annual <- function(df, scale_factor) {

  has_adm3 <- "adm3" %in% names(df)

  # Define grouping columns
  group_cols <- c("hf_uid", "adm0", "adm1", "adm2")
  if (has_adm3) {
    group_cols <- c(group_cols, "adm3")
  }
  group_cols <- c(group_cols, "year")

  # Check which incidence columns exist
  has_n0 <- "n0_cases" %in% names(df)
  has_n1 <- "n1_cases" %in% names(df)
  has_n2 <- "n2_cases" %in% names(df)
  has_cs <- all(c("cs_public", "cs_private", "cs_none") %in% names(df))

  df_agg <- df |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
    dplyr::summarise(
      pop = mean(pop, na.rm = TRUE),
      conf = sum(conf, na.rm = TRUE),
      test = if ("test" %in% names(df)) {
        sum(test, na.rm = TRUE)
      } else {
        NA_real_
      },
      pres = if ("pres" %in% names(df)) {
        sum(pres, na.rm = TRUE)
      } else {
        NA_real_
      },
      tpr = if ("tpr" %in% names(df)) {
        mean(tpr, na.rm = TRUE)
      } else {
        NA_real_
      },
      reprate = if ("reprate" %in% names(df)) {
        mean(reprate, na.rm = TRUE)
      } else {
        NA_real_
      },
      cs_public = if ("cs_public" %in% names(df)) {
        mean(cs_public, na.rm = TRUE)
      } else {
        NA_real_
      },
      cs_private = if ("cs_private" %in% names(df)) {
        mean(cs_private, na.rm = TRUE)
      } else {
        NA_real_
      },
      cs_none = if ("cs_none" %in% names(df)) {
        mean(cs_none, na.rm = TRUE)
      } else {
        NA_real_
      },
      n0_cases = if (has_n0) {
        sum(n0_cases, na.rm = TRUE)
      } else {
        NA_real_
      },
      n1_cases = if (has_n1) {
        sum(n1_cases, na.rm = TRUE)
      } else {
        NA_real_
      },
      n2_cases = if (has_n2) {
        sum(n2_cases, na.rm = TRUE)
      } else {
        NA_real_
      },
      .groups = "drop"
    ) |>
    dplyr::mutate(
      n0_incidence = if (has_n0) {
        dplyr::if_else(pop > 0, (n0_cases / pop) * scale_factor, NA_real_)
      } else {
        NA_real_
      },
      n1_incidence = if (has_n1) {
        dplyr::if_else(pop > 0, (n1_cases / pop) * scale_factor, NA_real_)
      } else {
        NA_real_
      },
      n2_incidence = if (has_n2) {
        dplyr::if_else(pop > 0, (n2_cases / pop) * scale_factor, NA_real_)
      } else {
        NA_real_
      }
    )

  # Calculate N3 and N4
  if (has_n2 && has_cs) {
    df_agg <- df_agg |>
      dplyr::mutate(
        adj_priv = dplyr::if_else(
          !is.na(cs_public) & cs_public > 0,
          cs_private / cs_public,
          0
        ),
        adj_none = dplyr::if_else(
          !is.na(cs_public) & cs_public > 0,
          cs_none / cs_public,
          0
        ),
        n3_cases = dplyr::if_else(
          !is.na(n2_cases) & !is.na(cs_public) & cs_public > 0,
          n2_cases + (n2_cases * adj_priv) + (n2_cases * adj_none),
          NA_real_
        ),
        n3_incidence = dplyr::if_else(
          pop > 0 & !is.na(n3_cases),
          (n3_cases / pop) * scale_factor,
          NA_real_
        ),
        # N4 = N2 * (1 + adj_none) - excludes private sector
        n4_cases = dplyr::if_else(
          !is.na(n2_cases) & !is.na(cs_public) & cs_public > 0,
          n2_cases * (1 + adj_none),
          NA_real_
        ),
        n4_incidence = dplyr::if_else(
          pop > 0 & !is.na(n4_cases),
          (n4_cases / pop) * scale_factor,
          NA_real_
        )
      )
  }

  # Remove columns that are all NA
  df_agg <- df_agg |>
    dplyr::select(dplyr::where(~ !all(is.na(.x))))

  tibble::as_tibble(df_agg)
}


#' Create a New SNT Incidence Object
#'
#' Constructor for `snt_incidence` S3 class. Creates a structured object
#' containing incidence data with metadata tracking scale, method, formula,
#' and calculation parameters.
#'
#' @param data Tibble with incidence calculations (output from
#'   `calc_incidence()`).
#' @param scale Numeric. Scale factor used (e.g., 1000, 10000).
#' @param levels Character vector of incidence levels calculated
#'   (e.g., c("N0", "N1")).
#' @param formulas Named list of formulas for each level calculated.
#' @param version Character. Package version used for calculation.
#'
#' @return An object of class `snt_incidence` with:
#'   \itemize{
#'     \item `data`: Tibble with incidence calculations
#'     \item `meta`: List of metadata (scale, levels, formulas, version,
#'       created_at)
#'   }
#'
#' @keywords internal
new_incidence <- function(
  data,
  scale,
  levels,
  formulas,
  version = "1.0.0"
) {
  if (!inherits(data, "data.frame")) {
    cli::cli_abort("`data` must be a data.frame or tibble.")
  }

  if (!all(c("pop") %in% names(data))) {
    cli::cli_abort("`data` must contain 'pop' column.")
  }

  structure(
    list(
      data = tibble::as_tibble(data),
      meta = list(
        scale = scale,
        levels = levels,
        formulas = formulas,
        version = version,
        created_at = Sys.time()
      )
    ),
    class = "snt_incidence"
  )
}


#' Create an SNT Incidence Object from Tibble
#'
#' User-facing wrapper to convert the output of `calc_incidence()` into
#' an `snt_incidence` S3 object with metadata. This is useful for tracking
#' provenance, enabling custom print/summary methods, and maintaining
#' calculation metadata.
#'
#' @param data Tibble output from `calc_incidence()`.
#' @param scale Numeric. Scale factor used in calculation (default: 1000).
#' @param levels Character vector of incidence levels present in data
#'   (default: c("N0", "N1", "N2", "N3")). Will auto-detect from columns.
#'
#' @return An object of class `snt_incidence`.
#'
#' @examples
#' # facility_data <- tibble::tibble(
#' #   hf_uid = "HF001",
#' #   adm1 = "RegionA",
#' #   adm2 = "DistrictX",
#' #   date = as.Date("2023-01-01"),
#' #   conf = 10,
#' #   test = 100,
#' #   pres = 5,
#' #   tpr = 0.10,
#' #   reprate = 0.85,
#' #   pop = 5000,
#' #   cs_public = 0.60,
#' #   cs_private = 0.25,
#' #   cs_none = 0.15
#' # )
#' #
#' # result_tbl <- calc_incidence(facility_data)
#' # result_obj <- create_incidence(result_tbl, scale = 1000)
#' # print(result_obj)
#' # summary(result_obj)
#'
#' @export
create_incidence <- function(
  data,
  scale = 1000,
  levels = c("N0", "N1", "N2", "N3")
) {
  if (!inherits(data, "data.frame")) {
    cli::cli_abort("`data` must be a data.frame or tibble.")
  }

  # Auto-detect levels from column names
  detected_levels <- character(0)
  if ("n0_incidence" %in% names(data)) {
    detected_levels <- c(detected_levels, "N0")
  }
  if ("n1_incidence" %in% names(data)) {
    detected_levels <- c(detected_levels, "N1")
  }
  if ("n2_incidence" %in% names(data)) {
    detected_levels <- c(detected_levels, "N2")
  }
  if ("n3_incidence" %in% names(data)) {
    detected_levels <- c(detected_levels, "N3")
  }
  if ("n4_incidence" %in% names(data)) {
    detected_levels <- c(detected_levels, "N4")
  }

  if (length(detected_levels) == 0) {
    cli::cli_abort(
      "No incidence columns found. Data must contain n*_incidence columns."
    )
  }

  # Define formulas
  formulas <- list(
    N0 = "N0 = conf",
    N1 = "N1 = conf + (pres * tpr)",
    N2 = "N2 = N1 / reprate",
    N3 = "N3 = N2 + (N2 * CS_Priv / CS_Pub) + (N2 * CS_None / CS_Pub)",
    N4 = "N4 = N2 * (1 + CS_None / CS_Pub)"
  )

  # Keep only formulas for detected levels
  formulas <- formulas[detected_levels]

  new_incidence(
    data = data,
    scale = scale,
    levels = detected_levels,
    formulas = formulas,
    version = utils::packageVersion("sntmethods")
  )
}


#' Print Method for SNT Incidence Objects
#'
#' @param x An object of class `snt_incidence`.
#' @param ... Additional arguments (ignored).
#'
#' @return Invisibly returns `x`.
#'
#' @export
print.snt_incidence <- function(x, ...) {
  cli::cli_h1("SNT Incidence Object")
  cli::cli_text("Levels: {.val {x$meta$levels}}")
  cli::cli_text("Scale: per {.val {x$meta$scale}} population")
  cli::cli_text("Records: {.val {sntutils::big_mark(nrow(x$data))}}")
  cli::cli_rule()

  cli::cli_h2("Cascade Formulas")
  for (level in names(x$meta$formulas)) {
    cli::cli_text("{.strong {level}}: {x$meta$formulas[[level]]}")
  }
  cli::cli_rule()

  cli::cli_h2("Column Summary")

  # Key columns present
  key_cols <- c(
    "pop", "conf", "test", "pres", "tpr", "reprate",
    "cs_public", "cs_private", "cs_none",
    "n0_cases", "n0_incidence",
    "n1_cases", "n1_incidence",
    "n2_cases", "n2_incidence",
    "n3_cases", "n3_incidence"
  )

  present <- intersect(key_cols, names(x$data))
  missing <- setdiff(key_cols, names(x$data))

  if (length(present) > 0) {
    cli::cli_text("Present ({length(present)}): {.field {present}}")
  }
  if (length(missing) > 0) {
    cli::cli_text("Not calculated ({length(missing)}): {.field {missing}}")
  }

  cli::cli_rule()

  cli::cli_h2("Data Preview")
  print(utils::head(x$data, 5))

  cli::cli_rule()
  cli::cli_text(
    "Created: {format(x$meta$created_at, '%Y-%m-%d %H:%M:%S')}"
  )
  cli::cli_text("Version: {x$meta$version}")
  cli::cli_text("")
  cli::cli_text("{.emph Use summary() for detailed statistics}")

  invisible(x)
}


#' Summary Method for SNT Incidence Objects
#'
#' Provides summary statistics for each incidence level including
#' number of valid records, mean, median, range, and missing counts,
#' plus overall data quality metrics.
#'
#' @param object An object of class `snt_incidence`.
#' @param ... Additional arguments (ignored).
#'
#' @return Invisibly returns a list of summary statistics.
#'
#' @export
summary.snt_incidence <- function(object, ...) {
  cli::cli_h1("SNT Incidence Summary")
  cli::cli_text("Scale: per {.val {object$meta$scale}} population")
  cli::cli_text("Records: {.val {sntutils::big_mark(nrow(object$data))}}")
  cli::cli_rule()

  # Display formulas
  cli::cli_h2("Cascade Formulas")
  for (level in names(object$meta$formulas)) {
    cli::cli_text("{.strong {level}}: {object$meta$formulas[[level]]}")
  }
  cli::cli_rule()

  # Overall data quality summary
  cli::cli_h2("Data Quality Overview")

  # Population summary
  if ("pop" %in% names(object$data)) {
    pop_total <- sum(object$data$pop, na.rm = TRUE)
    pop_missing <- sum(is.na(object$data$pop))
    cli::cli_text(
      "Total population: {.val {sntutils::big_mark(round(pop_total))}}"
    )
    if (pop_missing > 0) {
      cli::cli_alert_warning(
        "{sntutils::big_mark(pop_missing)} records with missing population"
      )
    }
  }

  # Input data summary
  if ("conf" %in% names(object$data)) {
    conf_total <- sum(object$data$conf, na.rm = TRUE)
    cli::cli_text(
      "Total confirmed cases: ",
      "{.val {sntutils::big_mark(round(conf_total))}}"
    )
  }

  if ("test" %in% names(object$data)) {
    test_total <- sum(object$data$test, na.rm = TRUE)
    test_missing <- sum(is.na(object$data$test))
    cli::cli_text(
      "Total tests: {.val {sntutils::big_mark(round(test_total))}}"
    )
    if (test_missing > 0) {
      cli::cli_text("  Missing: {sntutils::big_mark(test_missing)}")
    }
  }

  if ("pres" %in% names(object$data)) {
    pres_total <- sum(object$data$pres, na.rm = TRUE)
    cli::cli_text(
      "Total presumed cases: ",
      "{.val {sntutils::big_mark(round(pres_total))}}"
    )
  }

  # TPR summary
  if ("tpr" %in% names(object$data)) {
    tpr_mean <- mean(object$data$tpr, na.rm = TRUE)
    tpr_median <- stats::median(object$data$tpr, na.rm = TRUE)
    tpr_missing <- sum(is.na(object$data$tpr))
    cli::cli_text(
      "TPR - Mean: {.val {round(tpr_mean, 3)}}, ",
      "Median: {.val {round(tpr_median, 3)}}"
    )
    if (tpr_missing > 0) {
      cli::cli_text("  Missing: {sntutils::big_mark(tpr_missing)}")
    }
  }

  # Reporting rate summary
  if ("reprate" %in% names(object$data)) {
    reprate_mean <- mean(object$data$reprate, na.rm = TRUE)
    reprate_median <- stats::median(object$data$reprate, na.rm = TRUE)
    reprate_missing <- sum(is.na(object$data$reprate))
    cli::cli_text(
      "Reporting rate - Mean: {.val {round(reprate_mean, 3)}}, ",
      "Median: {.val {round(reprate_median, 3)}}"
    )
    if (reprate_missing > 0) {
      cli::cli_text("  Missing: {sntutils::big_mark(reprate_missing)}")
    }
  }

  # Care-seeking summary
  if (all(c("cs_public", "cs_private", "cs_none") %in% names(object$data))) {
    cs_pub_mean <- mean(object$data$cs_public, na.rm = TRUE)
    cs_priv_mean <- mean(object$data$cs_private, na.rm = TRUE)
    cs_none_mean <- mean(object$data$cs_none, na.rm = TRUE)
    cli::cli_text("Care-seeking proportions (mean):")
    cli::cli_text("  Public: {.val {round(cs_pub_mean, 3)}}")
    cli::cli_text("  Private: {.val {round(cs_priv_mean, 3)}}")
    cli::cli_text("  None: {.val {round(cs_none_mean, 3)}}")
  }

  cli::cli_rule()

  # Incidence level summaries
  cli::cli_h2("Incidence by Level")
  summary_list <- list()

  for (level in object$meta$levels) {
    inc_col <- paste0(tolower(level), "_incidence")
    case_col <- paste0(tolower(level), "_cases")

    if (inc_col %in% names(object$data)) {
      inc_vals <- object$data[[inc_col]]
      case_vals <- if (case_col %in% names(object$data)) {
        object$data[[case_col]]
      } else {
        NULL
      }

      n_valid <- sum(!is.na(inc_vals))
      n_missing <- sum(is.na(inc_vals))

      cli::cli_h2(level)
      cli::cli_text("Valid records: {.val {sntutils::big_mark(n_valid)}}")
      cli::cli_text("Missing: {.val {sntutils::big_mark(n_missing)}}")

      if (n_valid > 0) {
        inc_mean <- mean(inc_vals, na.rm = TRUE)
        inc_median <- stats::median(inc_vals, na.rm = TRUE)
        inc_min <- min(inc_vals, na.rm = TRUE)
        inc_max <- max(inc_vals, na.rm = TRUE)

        cli::cli_text(
          "Incidence - Mean: {.val {round(inc_mean, 2)}}, ",
          "Median: {.val {round(inc_median, 2)}}"
        )
        cli::cli_text(
          "Incidence - Range: [{.val {round(inc_min, 2)}}, ",
          "{.val {round(inc_max, 2)}}]"
        )

        if (!is.null(case_vals)) {
          case_total <- sum(case_vals, na.rm = TRUE)
          cli::cli_text(
            "Total cases: {.val {sntutils::big_mark(round(case_total))}}"
          )
        }
      }

      summary_list[[level]] <- list(
        n_valid = n_valid,
        n_missing = n_missing,
        mean = if (n_valid > 0) mean(inc_vals, na.rm = TRUE) else NA,
        median = if (n_valid > 0) {
          stats::median(inc_vals, na.rm = TRUE)
        } else {
          NA
        },
        min = if (n_valid > 0) min(inc_vals, na.rm = TRUE) else NA,
        max = if (n_valid > 0) max(inc_vals, na.rm = TRUE) else NA
      )
    }
  }

  invisible(summary_list)
}


#' Convert SNT Incidence Object to Tibble
#'
#' Extracts the data component from an `snt_incidence` object and
#' returns it as a tibble, discarding metadata.
#'
#' @param x An object of class `snt_incidence`.
#' @param ... Additional arguments (ignored).
#'
#' @return A tibble with incidence data.
#'
#' @exportS3Method tibble::as_tibble
as_tibble.snt_incidence <- function(x, ...) {
  x$data
}


#' Plot Method for SNT Incidence Objects
#'
#' Creates a time series plot of incidence rates by level. If multiple
#' administrative units are present, will facet by adm2 (up to 12 units).
#'
#' @param x An object of class `snt_incidence`.
#' @param level Character. Which incidence level to plot
#'   (default: highest available).
#' @param by Character. Grouping variable for plot
#'   (default: "adm2"). Set to NULL for aggregate plot.
#' @param max_facets Numeric. Maximum number of facets to display
#'   (default: 12).
#' @param ... Additional arguments passed to ggplot2 functions.
#'
#' @return A ggplot2 object.
#'
#' @export
plot.snt_incidence <- function(
  x,
  level = NULL,
  by = "adm2",
  max_facets = 12,
  ...
) {
  # Select level to plot
  if (is.null(level)) {
    level <- x$meta$levels[length(x$meta$levels)]
  }

  if (!level %in% x$meta$levels) {
    cli::cli_abort(
      "Level '{level}' not found in object. Available: {x$meta$levels}"
    )
  }

  inc_col <- paste0(tolower(level), "_incidence")

  if (!inc_col %in% names(x$data)) {
    cli::cli_abort("Column '{inc_col}' not found in data.")
  }

  plot_data <- x$data |>
    dplyr::select(
      date,
      dplyr::any_of(c(by, "adm1", "adm2")),
      incidence = !!inc_col
    ) |>
    dplyr::filter(!is.na(incidence))

  # Limit facets if needed
  if (!is.null(by) && by %in% names(plot_data)) {
    n_groups <- length(unique(plot_data[[by]]))
    if (n_groups > max_facets) {
      top_groups <- plot_data |>
        dplyr::group_by(.data[[by]]) |>
        dplyr::summarise(
          total = sum(incidence, na.rm = TRUE),
          .groups = "drop"
        ) |>
        dplyr::slice_max(total, n = max_facets) |>
        dplyr::pull(!!by)

      plot_data <- plot_data |>
        dplyr::filter(.data[[by]] %in% top_groups)

      cli::cli_alert_info(
        "Displaying top {max_facets} of {n_groups} {by} units."
      )
    }
  }

  # Create plot
  p <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = date, y = incidence)
  ) +
    ggplot2::geom_line(color = "#2E86AB", linewidth = 0.8) +
    ggplot2::geom_point(color = "#2E86AB", size = 1.5, alpha = 0.7) +
    ggplot2::labs(
      title = paste0(level, " Incidence Over Time"),
      subtitle = paste0("Per ", x$meta$scale, " population"),
      x = "Date",
      y = paste0("Incidence (per ", x$meta$scale, ")")
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 14),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )

  # Add faceting if requested
  if (!is.null(by) && by %in% names(plot_data)) {
    p <- p + ggplot2::facet_wrap(stats::as.formula(paste("~", by)))
  }

  return(p)
}

#' Check Incidence Trends
#'
#' Creates diagnostic plots to visualize incidence trends across the cascade
#' levels (N0-N4) over time, faceted by location (adm1 ~ adm2).
#'
#' @param incidence_output Output from `calc_incidence()` containing monthly
#'   and annual aggregations at different admin levels.
#' @param ncol Integer. Number of columns for facet layout. Default is 4.
#'   All locations are always shown; this controls the grid layout.
#'
#' @return A list containing:
#'   \describe{
#'     \item{monthly_plot}{ggplot2 object showing monthly incidence (N0-N4)
#'       by date}
#'     \item{annual_plot}{ggplot2 object showing annual incidence (N0-N4)
#'       by year}
#'   }
#'
#' @examples
#' # result <- calc_incidence(data)
#' # plots <- check_incidence(result)
#' # plots$monthly_plot
#' # plots$annual_plot
#'
#' @export
check_incidence <- function(
    incidence_output,
    ncol = 4
) {

  # Validate input
  expected_names <- c("monthly", "annual")
  if (!is.list(incidence_output) ||
      !all(expected_names %in% names(incidence_output))) {
    cli::cli_abort(
      "Input must be output from calc_incidence() with ",
      "'monthly' and 'annual' components."
    )
  }

  # Get monthly and annual data at adm2 level

  monthly_data <- incidence_output$monthly$adm2
  annual_data <- incidence_output$annual$adm2

  if (is.null(monthly_data) || nrow(monthly_data) == 0) {
    cli::cli_abort("No monthly data available at adm2 level.")
  }

  # Create location label (adm1 ~ adm2)
  monthly_data <- monthly_data |>
    dplyr::mutate(
      location = paste0(adm1, " ~ ", adm2)
    )

  annual_data <- annual_data |>
    dplyr::mutate(
      location = paste0(adm1, " ~ ", adm2)
    )


  # Determine which incidence columns exist (with or without suffix)
  # Pattern matches n0_incidence, n1_incidence_u5, etc.
  monthly_inc_cols <- grep(
    "^n[0-4]_incidence(_[a-zA-Z0-9_]+)?$",
    names(monthly_data),
    value = TRUE
  )
  annual_inc_cols <- grep(
    "^n[0-4]_incidence(_[a-zA-Z0-9_]+)?$",
    names(annual_data),
    value = TRUE
  )

  # Detect suffix from column names (if any)
  detected_suffix <- NULL
  if (length(monthly_inc_cols) > 0) {
    suffix_match <- regmatches(
      monthly_inc_cols[1],
      regexec("^n[0-4]_incidence(_([a-zA-Z0-9_]+))?$", monthly_inc_cols[1])
    )[[1]]
    if (length(suffix_match) >= 3 && nchar(suffix_match[3]) > 0) {
      detected_suffix <- suffix_match[3]
    }
  }

  if (length(monthly_inc_cols) == 0) {
    cli::cli_abort("No incidence columns found in monthly data.")
  }

  # Get all unique locations
  all_locations <- unique(monthly_data$location)
  n_locations <- length(all_locations)

  cli::cli_alert_info(
    "Creating plots for {n_locations} locations with {ncol} columns"
  )

  # Pivot monthly data to long format for plotting
  monthly_long <- monthly_data |>
    dplyr::select(
      location, year, month, date,
      dplyr::all_of(monthly_inc_cols)
    ) |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(monthly_inc_cols),
      names_to = "level",
      values_to = "incidence"
    ) |>
    dplyr::mutate(
      yearmon = zoo::as.yearmon(date),
      # Remove _incidence and any suffix (e.g., n0_incidence_u5 -> N0)
      level = toupper(gsub("_incidence(_[a-zA-Z0-9_]+)?$", "", level)),
      level = factor(level, levels = c("N0", "N1", "N2", "N3", "N4"))
    )

  # Pivot annual data to long format
  annual_long <- annual_data |>
    dplyr::select(
      location, year,
      dplyr::all_of(annual_inc_cols)
    ) |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(annual_inc_cols),
      names_to = "level",
      values_to = "incidence"
    ) |>
    dplyr::mutate(
      # Remove _incidence and any suffix (e.g., n0_incidence_u5 -> N0)
      level = toupper(gsub("_incidence(_[a-zA-Z0-9_]+)?$", "", level)),
      level = factor(level, levels = c("N0", "N1", "N2", "N3", "N4"))
    )

  # Create monthly plot (N0-N4)
  monthly_plot <- ggplot2::ggplot(
    monthly_long,
    ggplot2::aes(
      x = yearmon,
      y = incidence,
      colour = level,
      group = level
    )
  ) +
    ggplot2::geom_line(linewidth = 0.7) +
    ggplot2::facet_wrap(~location, scales = "free_y", ncol = ncol) +
    zoo::scale_x_yearmon() +
    ggplot2::scale_y_continuous(labels = scales::label_comma()) +
    ggplot2::scale_colour_manual(
      values = c(
        "N0" = "#1b9e77", "N1" = "#d95f02", "N2" = "#7570b3",
        "N3" = "#e7298a", "N4" = "#66a61e"
      ),
      name = "Incidence Level"
    ) +
    ggplot2::labs(
      title = "Monthly Incidence Trends by ADM1 ~ ADM2",
      subtitle = paste0(
        "Shows incidence cascade (N0-N4) over time. ",
        "N0=crude, N1=testing-adjusted, N2=reporting-adjusted, ",
        "N3=care-seeking-adjusted, N4=public+non-seekers.\n"
      ),
      x = NULL,
      y = "Incidence (per 1,000)\n"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.margin = ggplot2::margin(10, 10, 10, 10),
      panel.border = ggplot2::element_rect(
        fill = NA,
        colour = "black",
        linewidth = 0.6
      ),
      axis.text.x = ggplot2::element_text(
        angle = 45,
        hjust = 1
      ),
      legend.position = "bottom",
      legend.title = ggplot2::element_text(size = 9),
      legend.text = ggplot2::element_text(size = 8)
    )

  # Create annual plot (N0-N4)
  annual_plot <- ggplot2::ggplot(
    annual_long,
    ggplot2::aes(
      x = year,
      y = incidence,
      colour = level,
      group = level
    )
  ) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::geom_point(size = 2) +
    ggplot2::facet_wrap(~location, scales = "free_y", ncol = ncol) +
    ggplot2::scale_y_continuous(labels = scales::label_comma()) +
    ggplot2::scale_x_continuous(breaks = unique(annual_long$year)) +
    ggplot2::scale_colour_manual(
      values = c("N0" = "#1b9e77", "N1" = "#d95f02",
      "N2" = "#7570b3", "N3" = "#e7298a", "N4" = "#66a61e"),
      name = "Incidence Level"
    ) +
    ggplot2::labs(
      title = "Annual Incidence Trends by ADM1 ~ ADM2",
      subtitle = paste0(
        "Shows full incidence cascade (N0-N4) by year. ",
        "N3=full CSB, N4=public+non-seekers only.\n"
      ),
      x = NULL,
      y = "Incidence (per 1,000)\n"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.margin = ggplot2::margin(10, 10, 10, 10),
      panel.border = ggplot2::element_rect(
        fill = NA,
        colour = "black",
        linewidth = 0.6
      ),
      axis.text.x = ggplot2::element_text(
        angle = 45,
        hjust = 1
      ),
      legend.position = "bottom",
      legend.title = ggplot2::element_text(size = 9),
      legend.text = ggplot2::element_text(size = 8)
    )

  cli::cli_alert_success(
    "Incidence check plots created for {n_locations} locations."
  )

  list(
    monthly_plot = monthly_plot,
    annual_plot = annual_plot
  )
}
