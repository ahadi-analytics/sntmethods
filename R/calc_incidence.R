#' Calculate Malaria Incidence from Routine Health Facility Data (N0-N3)
#'
#' Calculates malaria incidence at the admin-month level (e.g., district-month)
#' using a structured cascade framework (N0 through N3) to adjust for testing
#' gaps, reporting incompleteness, and care-seeking behavior. Facility-level
#' data is aggregated to admin level before calculating incidence. Returns a
#' validated dataset with all incidence levels, quality flags, and source tracking.
#'
#' The incidence cascade framework:
#' \itemize{
#'   \item **N0 (Crude Incidence)**:
#'     \itemize{
#'       \item n0_cases = conf
#'       \item n0_incidence = (n0_cases / pop) * scale_factor
#'     }
#'   \item **N1 (Testing-Adjusted)**:
#'     \itemize{
#'       \item n1_cases = conf + (pres * tpr)
#'       \item n1_incidence = (n1_cases / pop) * scale_factor
#'     }
#'   \item **N2 (Reporting-Adjusted)**:
#'     \itemize{
#'       \item n2_cases = n1_cases / reprate
#'       \item n2_incidence = (n2_cases / pop) * scale_factor
#'     }
#'   \item **N3 (Care-Seeking-Adjusted)**:
#'     \itemize{
#'       \item N3 uses annual aggregation of N2 before applying CSB adjustment
#'       \item n3_annual = n2_annual * (1 + cs_private/cs_public + cs_none/cs_public)
#'       \item n3_monthly = n3_annual * (n2_monthly / n2_annual)
#'       \item n3_incidence = (n3_cases / pop) * scale_factor
#'     }
#' }
#'
#' Each level builds on the previous, with N3 representing the most complete
#' estimate of true community-level malaria incidence. N3 uses annual aggregation
#' because care-seeking behavior (CSB) data from DHS/MIS is annual.
#'
#' @param data Routine health facility data at facility-month level
#'   (data.frame or tibble). Must contain one row per facility per month.
#' @param levels Character vector specifying which incidence levels to calculate
#'   (default: `c("N0", "N1", "N2", "N3")`). Can specify subset like `c("N0",
#'   "N1")`.
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
#' @param scale_factor Denominator for incidence rate calculation
#'   (default: 1000, for cases per 1,000 population).
#' @param include_flags Logical; if `TRUE`, includes all quality flag columns
#'   in the output. If `FALSE` (default), returns only the core incidence
#'   variables without flags.
#'
#' @return A tibble with incidence estimates per admin-month including:
#'   \itemize{
#'     \item `adm0`: National/country level
#'     \item `adm1`: First administrative level
#'     \item `adm2`: Second administrative level
#'     \item `date`: Standardised date (first of month)
#'     \item `year`: Year extracted from date
#'     \item `month`: Month extracted from date
#'     \item `pop`: Population denominator (summed across facilities)
#'     \item `conf`: Confirmed cases (summed across facilities)
#'     \item `test`: Number tested (summed across facilities)
#'     \item `pres`: Presumed cases (summed across facilities)
#'     \item `tpr`: Test positivity rate (recalculated at admin level)
#'     \item `reprate`: Reporting rate (mean across facilities)
#'     \item `n0_cases`: N0 case count (crude)
#'     \item `n0_incidence`: N0 incidence per scale_factor
#'     \item `n1_cases`: N1 case count (testing-adjusted)
#'     \item `n1_incidence`: N1 incidence per scale_factor
#'     \item `n2_cases`: N2 case count (reporting-adjusted)
#'     \item `n2_incidence`: N2 incidence per scale_factor
#'     \item `n3_cases`: N3 case count (care-seeking-adjusted, distributed from annual)
#'     \item `n3_incidence`: N3 incidence per scale_factor
#'     \item `adj_priv`: CSB adjustment factor for private care-seeking
#'     \item `adj_none`: CSB adjustment factor for non-care-seeking
#'     \item `incidence_level`: Highest level successfully calculated
#'     \item Quality flags (if `include_flags = TRUE`):
#'       \itemize{
#'         \item `flag_pop_zero`: TRUE if population == 0
#'         \item `flag_pop_missing`: TRUE if population is NA
#'         \item `flag_tpr_missing`: TRUE if TPR is NA
#'         \item `flag_reprate_missing`: TRUE if reporting rate is NA
#'         \item `flag_reprate_low`: TRUE if reporting rate < 0.5
#'         \item `flag_cs_missing`: TRUE if care-seeking data is NA
#'         \item `flag_n1_invalid`: TRUE if N1 could not be calculated
#'         \item `flag_n2_invalid`: TRUE if N2 could not be calculated
#'         \item `flag_n3_invalid`: TRUE if N3 could not be calculated
#'       }
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
#' # # Calculate only N0 and N1 (no reprate needed)
#' # result_n01 <- calc_incidence(tpr_data, levels = c("N0", "N1"))
#'
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
  scale_factor = 1000,
  include_flags = FALSE
) {
  # ---- validate data -------------------------------------------------------

  if (!is.data.frame(data)) {
    cli::cli_abort("`data` must be a data.frame or tibble.")
  }

  if (nrow(data) == 0) {
    cli::cli_abort("`data` is empty.")
  }

  # Validate levels parameter
  valid_levels <- c("N0", "N1", "N2", "N3")
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

  # Additional requirements for N1
  if ("N1" %in% levels || "N2" %in% levels || "N3" %in% levels) {
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

  if (!is.numeric(scale_factor) || length(scale_factor) != 1) {
    cli::cli_abort("`scale_factor` must be a single numeric value.")
  }

  if (scale_factor <= 0) {
    cli::cli_abort("`scale_factor` must be positive.")
  }

  cli::cli_alert_info(
    "Processing {sntutils::big_mark(nrow(data))} facility-month records."
  )

  # ---- validate TPR presence -----------------------------------------------

  needs_tpr <- any(c("N1", "N2", "N3") %in% levels)

  if (needs_tpr && !tpr_var %in% names(data)) {
    cli::cli_abort(
      c(
        "TPR column '{tpr_var}' not found in data.",
        "i" = "Calculate TPR first using calc_tpr() before calling calc_incidence()."
      )
    )
  }

  # ---- validate reporting rate presence ------------------------------------

  needs_reprate <- any(c("N2", "N3") %in% levels)

  if (needs_reprate && !reprate_var %in% names(data)) {
    cli::cli_abort(
      c(
        "Reporting rate column '{reprate_var}' not found in data.",
        "i" = "Required for N2/N3 calculations."
      )
    )
  }

  # ---- rename vars internally ----------------------------------------------

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
  }

  # Add care-seeking for N3
  if ("N3" %in% levels) {
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

  if ("N3" %in% levels) {
    # Only create flag if care-seeking columns exist
    if (all(c("cs_public", "cs_private", "cs_none") %in% names(df))) {
      df <- df |>
        dplyr::mutate(
          flag_cs_missing = is.na(cs_public) |
            is.na(cs_private) |
            is.na(cs_none)
        )
    }
  }

  # ---- calculate N0: crude incidence ---------------------------------------

  if ("N0" %in% levels) {
    cli::cli_alert_info("Calculating N0 (crude incidence)...")

    df <- df |>
      .calc_n0_internal(scale_factor = scale_factor)

    n_n0_valid <- sum(!is.na(df$n0_incidence))
    cli::cli_alert_success(
      "N0 calculated for {sntutils::big_mark(n_n0_valid)} records."
    )
  }

  # ---- calculate N1: testing-adjusted incidence ----------------------------

  if ("N1" %in% levels) {
    cli::cli_alert_info("Calculating N1 (testing-adjusted incidence)...")

    df <- df |>
      .calc_n1_internal(scale_factor = scale_factor)

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
  }

  # ---- calculate N2: reporting-adjusted incidence --------------------------

  if ("N2" %in% levels) {
    cli::cli_alert_info(
      "Calculating N2 (reporting-adjusted incidence)..."
    )

    df <- df |>
      .calc_n2_internal(scale_factor = scale_factor)

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
  }

  # ---- calculate N3: care-seeking-adjusted incidence -----------------------

  if ("N3" %in% levels) {
    cli::cli_alert_info(
      "Calculating N3 (care-seeking-adjusted incidence)..."
    )

    df <- df |>
      .calc_n3_internal(scale_factor = scale_factor)

    n_n3_valid <- sum(!is.na(df$n3_incidence))
    n_n3_invalid <- sum(is.na(df$n3_incidence))

    cli::cli_alert_success(
      "N3 calculated for {sntutils::big_mark(n_n3_valid)} records."
    )

    if (n_n3_invalid > 0) {
      cli::cli_alert_warning(
        "{sntutils::big_mark(n_n3_invalid)} records have invalid N3 ",
        "(missing care-seeking data or invalid N2)."
      )
    }
  }

  # ---- determine highest valid incidence level -----------------------------

  df <- df |>
    .determine_incidence_level(levels = levels)

  # ---- select output columns -----------------------------------------------

  # Note: Output is at admin-month level (no hf_uid)
  core_cols <- c(
    "adm0",
    "adm1",
    "adm2",
    "date",
    "year",
    "month",
    "pop",
    "conf"
  )

  if (needs_tpr) {
    core_cols <- c(core_cols, "test", "pres", "tpr")
  }

  if (needs_reprate) {
    core_cols <- c(core_cols, "reprate")
  }

  # Add N-level columns that were calculated
  if ("N0" %in% levels) {
    core_cols <- c(core_cols, "n0_cases", "n0_incidence")
  }
  if ("N1" %in% levels) {
    core_cols <- c(core_cols, "n1_cases", "n1_incidence")
  }
  if ("N2" %in% levels) {
    core_cols <- c(core_cols, "n2_cases", "n2_incidence")
  }
  if ("N3" %in% levels) {
    core_cols <- c(core_cols, "cs_public", "cs_private", "cs_none",
                   "n3_cases", "n3_incidence", "adj_priv", "adj_none")
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

  # ---- Display summary ------------------------------------------------------

  cli::cli_rule()
  cli::cli_h2("Incidence Cascade Summary")

  # Show formulas
  cli::cli_h3("Formulas Used")
  if ("N0" %in% levels) {
    cli::cli_text("{.strong N0}: n0_cases = {conf_var}")
    cli::cli_text("         n0_incidence = (n0_cases / {pop_var}) * {scale_factor}")
  }
  if ("N1" %in% levels) {
    cli::cli_text("{.strong N1}: n1_cases = {conf_var} + ({pres_var} * {tpr_var})")
    cli::cli_text("         n1_incidence = (n1_cases / {pop_var}) * {scale_factor}")
  }
  if ("N2" %in% levels) {
    cli::cli_text("{.strong N2}: n2_cases = n1_cases / {reprate_var}")
    cli::cli_text("         n2_incidence = (n2_cases / {pop_var}) * {scale_factor}")
  }
  if ("N3" %in% levels) {
    cli::cli_text("{.strong N3}: n3_annual = n2_annual * (1 + {cs_private_var}/{cs_public_var} + {cs_none_var}/{cs_public_var})")
    cli::cli_text("         n3_monthly = n3_annual * (n2_monthly / n2_annual)")
    cli::cli_text("         n3_incidence = (n3_cases / {pop_var}) * {scale_factor}")
  }

  cli::cli_rule()

  # Data overview
  cli::cli_h3("Data Overview")

  # Population
  if ("pop" %in% names(df_final)) {
    pop_total <- sum(df_final$pop, na.rm = TRUE)
    cli::cli_text("Total population: {.val {sntutils::big_mark(round(pop_total))}}")
  }

  # Input data
  if ("conf" %in% names(df_final)) {
    conf_total <- sum(df_final$conf, na.rm = TRUE)
    cli::cli_text("Total confirmed cases: {.val {sntutils::big_mark(round(conf_total))}}")
  }

  if ("test" %in% names(df_final)) {
    test_total <- sum(df_final$test, na.rm = TRUE)
    cli::cli_text("Total tests: {.val {sntutils::big_mark(round(test_total))}}")
  }

  if ("pres" %in% names(df_final)) {
    pres_total <- sum(df_final$pres, na.rm = TRUE)
    cli::cli_text("Total presumed cases: {.val {sntutils::big_mark(round(pres_total))}}")
  }

  # Key indicators
  if ("tpr" %in% names(df_final)) {
    tpr_mean <- mean(df_final$tpr, na.rm = TRUE)
    tpr_median <- stats::median(df_final$tpr, na.rm = TRUE)
    cli::cli_text("TPR (mean/median): {.val {round(tpr_mean, 3)}} / {.val {round(tpr_median, 3)}}")
  }

  if ("reprate" %in% names(df_final)) {
    reprate_mean <- mean(df_final$reprate, na.rm = TRUE)
    reprate_median <- stats::median(df_final$reprate, na.rm = TRUE)
    cli::cli_text("Reporting rate (mean/median): {.val {round(reprate_mean, 3)}} / {.val {round(reprate_median, 3)}}")
  }

  # Care-seeking
  if (all(c("cs_public", "cs_private", "cs_none") %in% names(df_final))) {
    cs_pub_mean <- mean(df_final$cs_public, na.rm = TRUE)
    cs_priv_mean <- mean(df_final$cs_private, na.rm = TRUE)
    cs_none_mean <- mean(df_final$cs_none, na.rm = TRUE)
    cli::cli_text("Care-seeking (mean): Public {.val {round(cs_pub_mean, 3)}}, Private {.val {round(cs_priv_mean, 3)}}, None {.val {round(cs_none_mean, 3)}}")
  }

  cli::cli_rule()

  # Results by ADM1 (regions)
  if ("adm1" %in% names(df_final) && "adm2" %in% names(df_final)) {
    cli::cli_h3("Results by Region (ADM1)")

    # Step 1: Aggregate facilities to ADM2-year-month level
    adm2_month <- df_final |>
      dplyr::group_by(adm1, adm2, year, month) |>
      dplyr::summarise(
        pop = mean(pop, na.rm = TRUE),  # Mean across facilities in district-month
        conf = sum(conf, na.rm = TRUE),
        test = if ("test" %in% names(df_final)) sum(test, na.rm = TRUE) else NA_real_,
        pres = if ("pres" %in% names(df_final)) sum(pres, na.rm = TRUE) else NA_real_,
        tpr = if ("tpr" %in% names(df_final)) mean(tpr, na.rm = TRUE) else NA_real_,
        reprate = if ("reprate" %in% names(df_final)) mean(reprate, na.rm = TRUE) else NA_real_,
        cs_public = if ("cs_public" %in% names(df_final)) mean(cs_public, na.rm = TRUE) else NA_real_,
        cs_private = if ("cs_private" %in% names(df_final)) mean(cs_private, na.rm = TRUE) else NA_real_,
        cs_none = if ("cs_none" %in% names(df_final)) mean(cs_none, na.rm = TRUE) else NA_real_,
        n0_cases = if ("n0_cases" %in% names(df_final)) sum(n0_cases, na.rm = TRUE) else NA_real_,
        n1_cases = if ("n1_cases" %in% names(df_final)) sum(n1_cases, na.rm = TRUE) else NA_real_,
        n2_cases = if ("n2_cases" %in% names(df_final)) sum(n2_cases, na.rm = TRUE) else NA_real_,
        n3_cases = if ("n3_cases" %in% names(df_final)) sum(n3_cases, na.rm = TRUE) else NA_real_,
        .groups = "drop"
      )

    # Step 2: Aggregate ADM2-month to ADM1-year-month level (sum populations across districts)
    adm1_year_month <- adm2_month |>
      dplyr::group_by(adm1, year, month) |>
      dplyr::summarise(
        pop = sum(pop, na.rm = TRUE),
        conf = sum(conf, na.rm = TRUE),
        test = sum(test, na.rm = TRUE),
        pres = sum(pres, na.rm = TRUE),
        tpr = mean(tpr, na.rm = TRUE),
        reprate = mean(reprate, na.rm = TRUE),
        cs_public = mean(cs_public, na.rm = TRUE),
        cs_private = mean(cs_private, na.rm = TRUE),
        cs_none = mean(cs_none, na.rm = TRUE),
        n0_cases = sum(n0_cases, na.rm = TRUE),
        n1_cases = sum(n1_cases, na.rm = TRUE),
        n2_cases = sum(n2_cases, na.rm = TRUE),
        n3_cases = sum(n3_cases, na.rm = TRUE),
        .groups = "drop"
      )

    # Step 3: Aggregate ADM1-year-month to ADM1 level (average monthly populations)
    adm1_summary <- adm1_year_month |>
      dplyr::group_by(adm1) |>
      dplyr::summarise(
        pop = mean(pop, na.rm = TRUE),
        conf = sum(conf, na.rm = TRUE),
        test = sum(test, na.rm = TRUE),
        pres = sum(pres, na.rm = TRUE),
        tpr = mean(tpr, na.rm = TRUE),
        reprate = mean(reprate, na.rm = TRUE),
        cs_public = mean(cs_public, na.rm = TRUE),
        cs_private = mean(cs_private, na.rm = TRUE),
        cs_none = mean(cs_none, na.rm = TRUE),
        n0_cases = sum(n0_cases, na.rm = TRUE),
        n1_cases = sum(n1_cases, na.rm = TRUE),
        n2_cases = sum(n2_cases, na.rm = TRUE),
        n3_cases = sum(n3_cases, na.rm = TRUE),
        .groups = "drop"
      ) |>
      dplyr::mutate(
        n0_incidence = (n0_cases / pop) * scale_factor,
        n1_incidence = (n1_cases / pop) * scale_factor,
        n2_incidence = (n2_cases / pop) * scale_factor,
        n3_incidence = (n3_cases / pop) * scale_factor
      ) |>
      dplyr::select(adm1, pop, dplyr::everything())

    # Display table
    print(adm1_summary)

    cli::cli_rule()
  }

  # Results by Year
  if ("year" %in% names(df_final) && "adm2" %in% names(df_final)) {
    cli::cli_h3("Results by Year")

    # Step 1: Aggregate facilities to ADM2-year-month level
    adm2_year_month <- df_final |>
      dplyr::group_by(adm2, year, month) |>
      dplyr::summarise(
        pop = mean(pop, na.rm = TRUE),  # Mean across facilities in district-month
        conf = sum(conf, na.rm = TRUE),
        test = if ("test" %in% names(df_final)) sum(test, na.rm = TRUE) else NA_real_,
        pres = if ("pres" %in% names(df_final)) sum(pres, na.rm = TRUE) else NA_real_,
        tpr = if ("tpr" %in% names(df_final)) mean(tpr, na.rm = TRUE) else NA_real_,
        reprate = if ("reprate" %in% names(df_final)) mean(reprate, na.rm = TRUE) else NA_real_,
        cs_public = if ("cs_public" %in% names(df_final)) mean(cs_public, na.rm = TRUE) else NA_real_,
        cs_private = if ("cs_private" %in% names(df_final)) mean(cs_private, na.rm = TRUE) else NA_real_,
        cs_none = if ("cs_none" %in% names(df_final)) mean(cs_none, na.rm = TRUE) else NA_real_,
        n0_cases = if ("n0_cases" %in% names(df_final)) sum(n0_cases, na.rm = TRUE) else NA_real_,
        n1_cases = if ("n1_cases" %in% names(df_final)) sum(n1_cases, na.rm = TRUE) else NA_real_,
        n2_cases = if ("n2_cases" %in% names(df_final)) sum(n2_cases, na.rm = TRUE) else NA_real_,
        n3_cases = if ("n3_cases" %in% names(df_final)) sum(n3_cases, na.rm = TRUE) else NA_real_,
        .groups = "drop"
      )

    # Step 2: Aggregate ADM2-year-month to year-month level (sum populations across districts)
    year_month <- adm2_year_month |>
      dplyr::group_by(year, month) |>
      dplyr::summarise(
        pop = sum(pop, na.rm = TRUE),
        conf = sum(conf, na.rm = TRUE),
        test = sum(test, na.rm = TRUE),
        pres = sum(pres, na.rm = TRUE),
        tpr = mean(tpr, na.rm = TRUE),
        reprate = mean(reprate, na.rm = TRUE),
        cs_public = mean(cs_public, na.rm = TRUE),
        cs_private = mean(cs_private, na.rm = TRUE),
        cs_none = mean(cs_none, na.rm = TRUE),
        n0_cases = sum(n0_cases, na.rm = TRUE),
        n1_cases = sum(n1_cases, na.rm = TRUE),
        n2_cases = sum(n2_cases, na.rm = TRUE),
        n3_cases = sum(n3_cases, na.rm = TRUE),
        .groups = "drop"
      )

    # Step 3: Aggregate year-month to year level (average monthly populations)
    year_summary <- year_month |>
      dplyr::group_by(year) |>
      dplyr::summarise(
        pop = mean(pop, na.rm = TRUE),
        conf = sum(conf, na.rm = TRUE),
        test = sum(test, na.rm = TRUE),
        pres = sum(pres, na.rm = TRUE),
        tpr = mean(tpr, na.rm = TRUE),
        reprate = mean(reprate, na.rm = TRUE),
        cs_public = mean(cs_public, na.rm = TRUE),
        cs_private = mean(cs_private, na.rm = TRUE),
        cs_none = mean(cs_none, na.rm = TRUE),
        n0_cases = sum(n0_cases, na.rm = TRUE),
        n1_cases = sum(n1_cases, na.rm = TRUE),
        n2_cases = sum(n2_cases, na.rm = TRUE),
        n3_cases = sum(n3_cases, na.rm = TRUE),
        .groups = "drop"
      ) |>
      dplyr::mutate(
        n0_incidence = (n0_cases / pop) * scale_factor,
        n1_incidence = (n1_cases / pop) * scale_factor,
        n2_incidence = (n2_cases / pop) * scale_factor,
        n3_incidence = (n3_cases / pop) * scale_factor
      ) |>
      dplyr::select(year, pop, dplyr::everything())

    # Display table
    print(year_summary)

    cli::cli_rule()
  }

  return(tibble::as_tibble(df_final))
}


# =============================================================================
# Internal helper functions for N0-N3 calculations
# =============================================================================

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
#'
#' @keywords internal
.aggregate_to_admin_month <- function(df) {
  # Check which columns exist for conditional aggregation
  has_cs <- all(c("cs_public", "cs_private", "cs_none") %in% names(df))
  has_reprate <- "reprate" %in% names(df)
  has_tpr <- "tpr" %in% names(df)

  df_agg <- df |>
    dplyr::group_by(adm0, adm1, adm2, year, month, date) |>
    dplyr::summarise(
      pop = sum(pop, na.rm = TRUE),
      conf = sum(conf, na.rm = TRUE),
      test = if ("test" %in% names(df)) sum(test, na.rm = TRUE) else NA_real_,
      pres = if ("pres" %in% names(df)) sum(pres, na.rm = TRUE) else NA_real_,
      # Recalculate TPR at admin level: total confirmed / total tested
      tpr = if (has_tpr) {
        sum(conf, na.rm = TRUE) / sum(test, na.rm = TRUE)
      } else {
        NA_real_
      },
      # Average reporting rate across facilities
      reprate = if (has_reprate) mean(reprate, na.rm = TRUE) else NA_real_,
      # Care-seeking proportions (should be same for all facilities in admin)
      cs_public = if (has_cs) dplyr::first(cs_public) else NA_real_,
      cs_private = if (has_cs) dplyr::first(cs_private) else NA_real_,
      cs_none = if (has_cs) dplyr::first(cs_none) else NA_real_,
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
#' @param df Data frame with standardized column names
#' @param scale_factor Denominator for incidence rate
#'
#' @return Data frame with n1_cases and n1_incidence columns added
#'
#' @keywords internal
.calc_n1_internal <- function(df, scale_factor) {
  df |>
    dplyr::mutate(
      # Ensure TPR is bounded [0, 1]
      tpr_clean = dplyr::case_when(
        is.na(tpr) ~ NA_real_,
        tpr < 0 ~ 0,
        tpr > 1 ~ 1,
        TRUE ~ tpr
      ),
      # Ensure pres is non-negative
      pres_clean = dplyr::if_else(
        is.na(pres) | pres < 0,
        0,
        pres
      ),
      # Calculate N1 cases
      n1_cases = dplyr::if_else(
        !is.na(conf) & !is.na(tpr_clean),
        conf + pres_clean * tpr_clean,
        NA_real_
      ),
      # Calculate N1 incidence
      n1_incidence = dplyr::if_else(
        pop > 0 & !is.na(pop) & !is.na(n1_cases),
        (n1_cases / pop) * scale_factor,
        NA_real_
      ),
      # Flag invalid N1
      flag_n1_invalid = is.na(n1_incidence)
    ) |>
    dplyr::select(-tpr_clean, -pres_clean)
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
#' Calculates N3 by aggregating N2 to annual level, applying the CSB adjustment
#' once per year, then distributing the annual N3 back to monthly level
#' proportionally based on each month's share of annual N2.
#'
#' N3_annual = N2_annual * (1 + CS_Priv/CS_Pub + CS_None/CS_Pub)
#' N3_monthly = N3_annual * (N2_monthly / N2_annual)
#'
#' @param df Data frame with standardized column names at admin-month level
#' @param scale_factor Denominator for incidence rate
#'
#' @return Data frame with n3_cases and n3_incidence columns added
#'
#' @keywords internal
.calc_n3_internal <- function(df, scale_factor) {
  # Step 1: Aggregate N2 to admin-year level
  annual_n2 <- df |>
    dplyr::group_by(adm0, adm1, adm2, year) |>
    dplyr::summarise(
      n2_annual = sum(n2_cases, na.rm = TRUE),
      # CSB should be the same for all months within admin-year
      cs_public = dplyr::first(cs_public),
      cs_private = dplyr::first(cs_private),
      cs_none = dplyr::first(cs_none),
      .groups = "drop"
    )

  # Step 2: Validate care-seeking proportions and apply CSB adjustment at annual level
  annual_n3 <- annual_n2 |>
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
      # Calculate annual N3 cases
      n3_annual = dplyr::if_else(
        !is.na(n2_annual) & !is.na(cs_public_clean),
        n2_annual + (n2_annual * adj_priv) + (n2_annual * adj_none),
        NA_real_
      )
    ) |>
    dplyr::select(
      adm0, adm1, adm2, year,
      n2_annual, n3_annual, adj_priv, adj_none
    )

  # Step 3: Calculate each month's share of annual N2 and join annual N3
  df <- df |>
    dplyr::left_join(
      annual_n3,
      by = c("adm0", "adm1", "adm2", "year")
    ) |>
    dplyr::mutate(
      # Calculate month's share of annual N2
      n2_share = dplyr::if_else(
        !is.na(n2_annual) & n2_annual > 0,
        n2_cases / n2_annual,
        NA_real_
      ),
      # Distribute annual N3 to months proportionally
      n3_cases = dplyr::if_else(
        !is.na(n3_annual) & !is.na(n2_share),
        n3_annual * n2_share,
        NA_real_
      ),
      # Calculate N3 incidence using monthly population
      n3_incidence = dplyr::if_else(
        pop > 0 & !is.na(pop) & !is.na(n3_cases),
        (n3_cases / pop) * scale_factor,
        NA_real_
      ),
      # Flag invalid N3
      flag_n3_invalid = is.na(n3_incidence)
    ) |>
    dplyr::select(-n2_annual, -n3_annual, -n2_share)

  df
}


#' Determine Highest Valid Incidence Level - Internal
#'
#' @param df Data frame with N0-N3 calculations
#' @param levels Character vector of requested levels
#'
#' @return Data frame with incidence_level column added
#'
#' @keywords internal
.determine_incidence_level <- function(df, levels) {
  # Build conditions only for levels that were calculated
  conditions <- list()

  if ("N3" %in% levels && "n3_incidence" %in% names(df)) {
    conditions <- c(
      conditions,
      list(quote(!is.na(n3_incidence) ~ "N3"))
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
    N3 = "N3 = N2 + (N2 * CS_Priv / CS_Pub) + (N2 * CS_None / CS_Pub)"
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
    cli::cli_text("Total population: {.val {sntutils::big_mark(round(pop_total))}}")
    if (pop_missing > 0) {
      cli::cli_alert_warning("{sntutils::big_mark(pop_missing)} records with missing population")
    }
  }

  # Input data summary
  if ("conf" %in% names(object$data)) {
    conf_total <- sum(object$data$conf, na.rm = TRUE)
    cli::cli_text("Total confirmed cases: {.val {sntutils::big_mark(round(conf_total))}}")
  }

  if ("test" %in% names(object$data)) {
    test_total <- sum(object$data$test, na.rm = TRUE)
    test_missing <- sum(is.na(object$data$test))
    cli::cli_text("Total tests: {.val {sntutils::big_mark(round(test_total))}}")
    if (test_missing > 0) {
      cli::cli_text("  Missing: {sntutils::big_mark(test_missing)}")
    }
  }

  if ("pres" %in% names(object$data)) {
    pres_total <- sum(object$data$pres, na.rm = TRUE)
    cli::cli_text("Total presumed cases: {.val {sntutils::big_mark(round(pres_total))}}")
  }

  # TPR summary
  if ("tpr" %in% names(object$data)) {
    tpr_mean <- mean(object$data$tpr, na.rm = TRUE)
    tpr_median <- stats::median(object$data$tpr, na.rm = TRUE)
    tpr_missing <- sum(is.na(object$data$tpr))
    cli::cli_text("TPR - Mean: {.val {round(tpr_mean, 3)}}, Median: {.val {round(tpr_median, 3)}}")
    if (tpr_missing > 0) {
      cli::cli_text("  Missing: {sntutils::big_mark(tpr_missing)}")
    }
  }

  # Reporting rate summary
  if ("reprate" %in% names(object$data)) {
    reprate_mean <- mean(object$data$reprate, na.rm = TRUE)
    reprate_median <- stats::median(object$data$reprate, na.rm = TRUE)
    reprate_missing <- sum(is.na(object$data$reprate))
    cli::cli_text("Reporting rate - Mean: {.val {round(reprate_mean, 3)}}, Median: {.val {round(reprate_median, 3)}}")
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


#' Validate Incidence Cascade (N0-N3) with Core HF Diagnostics
#'
#' @description
#' Produces three core diagnostics at health-facility level for the
#' incidence cascade (N0-N3):
#'
#' 1. Scatter cascade (N1 vs N0, N2 vs N1, N3 vs N2)
#' 2. Error by reporting rate (MAE patterns for N2 and N3 steps)
#' 3. Seasonality profiles at ADM1~ADM2 level (N0-N3)
#'
#' Expects data that has already passed through `calc_incidence()`.
#'
#' @param data data.frame or tibble. Output from `calc_incidence()`.
#' @inheritParams calc_incidence
#' @param month_var Column name for month (default: "month").
#' @param n0_col Column name for N0 incidence (default: "n0_incidence").
#' @param n1_col Column name for N1 incidence (default: "n1_incidence").
#' @param n2_col Column name for N2 incidence (default: "n2_incidence").
#' @param n3_col Column name for N3 incidence (default: "n3_incidence").
#' @param generate_plots logical; if `TRUE` (default), returns ggplot
#'   objects for the three diagnostics.
#'
#' @return A list with:
#'   * `metrics`: tibble with simple step-level MAE and RMSE
#'   * `validation_data`: cleaned data used for diagnostics
#'   * `plots`: list of three ggplot objects (if `generate_plots = TRUE`)
#'
#' @examples
#' # incid <- calc_incidence(hf_data_with_tpr_reprate)
#' # diag  <- validate_incidence_cascade(incid)
#' # diag$plots$scatter
#'
#' @importFrom stats sd
#' @export
validate_incidence_cascade <- function(
  data,
  hf_var = "hf_uid",
  adm0_var = "adm0",
  adm1_var = "adm1",
  adm2_var = "adm2",
  year_var = "year",
  month_var = "month",
  pop_var = "pop",
  conf_var = "conf",
  test_var = "test",
  reprate_var = "reprate",
  cs_public_var = "cs_public",
  cs_private_var = "cs_private",
  cs_none_var = "cs_none",
  n0_col = "n0_incidence",
  n1_col = "n1_incidence",
  n2_col = "n2_incidence",
  n3_col = "n3_incidence",
  generate_plots = TRUE
) {

  # validate input -------------------------------------------------------------

  if (!is.data.frame(data)) {
    cli::cli_abort("`data` must be a data.frame or tibble.")
  }

  required_cols <- c(
    hf_var,
    adm0_var,
    adm1_var,
    adm2_var,
    year_var,
    month_var,
    pop_var,
    conf_var,
    test_var,
    reprate_var,
    cs_public_var,
    cs_private_var,
    cs_none_var,
    n0_col,
    n1_col,
    n2_col,
    n3_col
  )

  missing_cols <- setdiff(required_cols, names(data))

  if (length(missing_cols) > 0) {
    cli::cli_abort(
      c(
        "Missing required columns in incidence data:",
        "{.var {missing_cols}}"
      )
    )
  }

  cli::cli_alert_info(
    "Validating incidence cascade for ",
    "{sntutils::big_mark(nrow(data))} facility-months."
  )


  # rename for internal use --------------------------------------------------

  df <- data |>
    dplyr::rename(
      hf = !!hf_var,
      adm0 = !!adm0_var,
      adm1 = !!adm1_var,
      adm2 = !!adm2_var,
      year = !!year_var,
      month = !!month_var,
      pop = !!pop_var,
      conf = !!conf_var,
      test = !!test_var,
      reprate = !!reprate_var,
      cs_public = !!cs_public_var,
      cs_private = !!cs_private_var,
      cs_none = !!cs_none_var,
      n0 = !!n0_col,
      n1 = !!n1_col,
      n2 = !!n2_col,
      n3 = !!n3_col
    )

  # drop rows with missing core incidence values for any step
  df <- df |>
    dplyr::filter(
      !is.na(n0),
      !is.na(n1),
      !is.na(n2),
      !is.na(n3)
    )

  if (nrow(df) < 10) {
    cli::cli_abort(
      "Insufficient usable records after filtering missing incidence values."
    )
  }

  cli::cli_alert_success(
    "Using {sntutils::big_mark(nrow(df))} records for diagnostics."
  )

  # -------------------------------------------------------------------------
  # build ADM1~ADM2 location factor (your code)
  # -------------------------------------------------------------------------

  df <- df |>
    dplyr::mutate(location = paste(adm1, "~", adm2))

  location_levels <- df |>
    dplyr::mutate(loc = paste(adm1, "~", adm2)) |>
    dplyr::distinct(loc, adm1, adm2) |>
    dplyr::arrange(adm1, adm2) |>
    dplyr::pull(loc)

  df <- df |>
    dplyr::mutate(
      location = factor(location, levels = location_levels)
    )


  # compute cascade diagnostics (expanded metrics) -----------------------------

  # helper to compute metrics for each step
  compute_step_metrics <- function(x, y, step_name) {
    # calibration fit
    cal_fit <- try(stats::lm(y ~ x), silent = TRUE)
    slope <- ifelse(
      inherits(cal_fit, "try-error"),
      NA_real_,
      stats::coef(cal_fit)[2]
    )
    intercept <- ifelse(
      inherits(cal_fit, "try-error"),
      NA_real_,
      stats::coef(cal_fit)[1]
    )

    tibble::tibble(
      step = step_name,
      mae = mean(abs(y - x), na.rm = TRUE),
      rmse = sqrt(mean((y - x)^2, na.rm = TRUE)),
      bias = mean(y - x, na.rm = TRUE),
      spearman = stats::cor(x, y, use = "complete.obs", method = "spearman"),
      amp_median = stats::median(y / pmax(x, 1e-8), na.rm = TRUE)
    )
  }

  metrics <- dplyr::bind_rows(
    compute_step_metrics(df$n0, df$n1, "N1 vs N0"),
    compute_step_metrics(df$n1, df$n2, "N2 vs N1"),
    compute_step_metrics(df$n2, df$n3, "N3 vs N2")
  )


  # simple step-level metrics (HF-month scale) ---------------------------------

  cli::cli_alert_info(
    "Cascade diagnostics computed for {nrow(metrics)} steps."
  )

  print(metrics)
  cli::cli_alert_info("MAE: average absolute deviation between steps.")
  cli::cli_alert_info("RMSE: deviation emphasising larger errors.")
  cli::cli_alert_info(
    "Bias: signed error; positive indicates inflation after adjustment."
  )
  cli::cli_alert_info(
    "Spearman: rank agreement between facilities before and after adjustment."
  )
  cli::cli_alert_info(
    paste0(
      "Amplification (median): typical multiplicative increase (y/x)",
      " from each step."
    )
  )

  plots <- list()

  if (generate_plots) {
    cli::cli_alert_info("Generating incidence cascade diagnostic plots...")


    # 1) scatter cascade: N1 vs N0, N2 vs N1, N3 vs N2 --------------------

    scatter_df <- dplyr::bind_rows(
      df |>
        dplyr::select(x = n0, y = n1) |>
        dplyr::mutate(step = "N1 vs N0"),
      df |>
        dplyr::select(x = n1, y = n2) |>
        dplyr::mutate(step = "N2 vs N1"),
      df |>
        dplyr::select(x = n2, y = n3) |>
        dplyr::mutate(step = "N3 vs N2")
    ) |>
      dplyr::filter(
        !is.na(x),
        !is.na(y)
      )

    scatter_df$step <- factor(
      scatter_df$step,
      levels = c("N1 vs N0", "N2 vs N1", "N3 vs N2")
    )

    plots$scatter <- ggplot2::ggplot(
      scatter_df,
      ggplot2::aes(x = x, y = y)
    ) +
      ggplot2::geom_point(
        alpha = 0.21,
        size = 1.95,
        colour = "steelblue2"
      ) +
      ggplot2::geom_abline(
        slope = 1,
        intercept = 0,
        colour = "red",
        linetype = "dashed"
      ) +
      ggplot2::facet_wrap(~step, scales = "free") +
      ggplot2::labs(
        title = "Incidence cascade: adjusted vs preceding step",
        subtitle = paste0(
          "Shows how each adjustment step shifts incidence ",
          "relative to the previous level"
        ),
        x = "\n\nBaseline incidence",
        y = "Adjusted incidence\n\n"
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
        )
      )

    # 2) error by reporting rate: MAE patterns for N2 and N3 ----------------

    df_repr <- df |>
      dplyr::mutate(
        err_n2 = abs(n2 - n1),
        err_n3 = abs(n3 - n2),
        repr_bin = cut(
          reprate,
          breaks = seq(0, 1, by = 0.05),
          include.lowest = TRUE
        )
      )

    # function for CI
    summ_ci <- function(x) {
      m <- mean(x, na.rm = TRUE)
      s <- stats::sd(x, na.rm = TRUE)
      n <- sum(!is.na(x))
      se <- s / sqrt(n)
      ci_low <- m - 1.96 * se
      ci_high <- m + 1.96 * se
      tibble::tibble(mae = m, ci_low = ci_low, ci_high = ci_high)
    }

    df_long <- dplyr::bind_rows(
      df_repr |>
        dplyr::group_by(repr_bin) |>
        dplyr::summarise(summ_ci(err_n2), .groups = "drop") |>
        dplyr::mutate(step = "N2 vs N1"),

      df_repr |>
        dplyr::group_by(repr_bin) |>
        dplyr::summarise(summ_ci(err_n3), .groups = "drop") |>
        dplyr::mutate(step = "N3 vs N2")
    )

    df_long$repr_bin <- factor(
      df_long$repr_bin,
      levels = levels(df_repr$repr_bin),
      ordered = TRUE
    )

    plots$error_by_reporting_rate <- ggplot2::ggplot(
      df_long,
      ggplot2::aes(
        x = repr_bin,
        y = mae,
        colour = step,
        group = step
      )
    ) +
      ggplot2::geom_ribbon(
        ggplot2::aes(
          ymin = ci_low,
          ymax = ci_high,
          fill = step
        ),
        alpha = 0.12,
        colour = NA
      ) +
      ggplot2::geom_line(linewidth = 1.4) +
      ggplot2::geom_point(size = 2.3) +
      ggplot2::scale_colour_manual(
        values = c(
          "N2 vs N1" = "#3288BD",
          "N3 vs N2" = "#D53E4F"
        )
      ) +
      ggplot2::scale_fill_manual(
        values = c(
          "N2 vs N1" = "#3288BD",
          "N3 vs N2" = "#D53E4F"
        )
      ) +
      ggplot2::labs(
        title = paste0(
          "MAE across reporting-rate bins ",
          "(With 95% CI per reporting bin)"
        ),
        subtitle = paste0(
          "N2 errors decline as reporting improves because N2 ",
          "adjusts for completeness.\n",
          "N3 errors remain high because they reflect care-seeking ",
          "inflation rather than reporting quality.\n"
        ),
        x = "\n\nReporting rate",
        y = "Mean absolute error\n\n"
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
        legend.position = "right",
        legend.title = ggplot2::element_text(size = 10),
        legend.text = ggplot2::element_text(size = 9)
      )

    # 3) MAE across care-seeking bins (with 95% CI) --------------------------

    df_cs <- df |>
      dplyr::mutate(
        err_n2 = abs(n2 - n1),
        err_n3 = abs(n3 - n2),

        # Bin care-seeking public share into equal-width bins (0 to 1)
        cs_bin = cut(
          cs_public,
          breaks = seq(0, 1, by = 0.05),
          include.lowest = TRUE
        )
      )

    df_cs_long <- dplyr::bind_rows(
      df_cs |>
        dplyr::group_by(cs_bin) |>
        dplyr::summarise(
          mae = mean(err_n2, na.rm = TRUE),
          sd = stats::sd(err_n2, na.rm = TRUE),
          n = sum(!is.na(err_n2)),
          se = sd / sqrt(n),
          ci_low = mae - 1.96 * se,
          ci_high = mae + 1.96 * se,
          step = "N2 vs N1",
          .groups = "drop"
        ),
      df_cs |>
        dplyr::group_by(cs_bin) |>
        dplyr::summarise(
          mae = mean(err_n3, na.rm = TRUE),
          sd = stats::sd(err_n3, na.rm = TRUE),
          n = sum(!is.na(err_n3)),
          se = sd / sqrt(n),
          ci_low = mae - 1.96 * se,
          ci_high = mae + 1.96 * se,
          step = "N3 vs N2",
          .groups = "drop"
        )
    )

    df_cs_long$cs_bin <- factor(
      df_cs_long$cs_bin,
      levels = levels(df_cs$cs_bin),
      ordered = TRUE
    )

    plots$error_by_careseeking_pub <- ggplot2::ggplot(
      df_cs_long,
      ggplot2::aes(
        x = cs_bin,
        y = mae,
        colour = step,
        group = step
      )
    ) +
      ggplot2::geom_ribbon(
        ggplot2::aes(
          ymin = ci_low,
          ymax = ci_high,
          fill = step
        ),
        alpha = 0.12,
        colour = NA
      ) +
      ggplot2::geom_line(linewidth = 1.4) +
      ggplot2::geom_point(size = 2.3) +
      ggplot2::scale_colour_manual(
        values = c(
          "N2 vs N1" = "#3288BD",
          "N3 vs N2" = "#D53E4F"
        )
      ) +
      ggplot2::scale_fill_manual(
        values = c(
          "N2 vs N1" = "#3288BD",
          "N3 vs N2" = "#D53E4F"
        )
      ) +
      ggplot2::labs(
        title = "MAE across care-seeking public-share bins (with 95% CI)",
        subtitle = paste0(
          "Bins are based on cs_public. When public share is low, inflation ",
          "is high and N3 errors sharply increase.\n",
          "N2 errors stay stable because they do not depend on care-seeking."
        ),
        x = "\n\nCare-seeking public proportion (cs_public)",
        y = "Mean absolute error\n\n"
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
        legend.position = "right",
        legend.title = ggplot2::element_text(size = 10),
        legend.text = ggplot2::element_text(size = 9)
      )

    # 4) MAE across reporting rate and  care-seeking bins --------------------

    heat_df <- df |>
      dplyr::mutate(
        repr_bin = cut(
          reprate,
          breaks = seq(0, 1, by = 0.05),
          include.lowest = TRUE
        ),
        cs_bin = cut(
          cs_public,
          breaks = seq(0, 1, by = 0.05),
          include.lowest = TRUE
        ),
        err_n2 = abs(n2 - n1),
        err_n3 = abs(n3 - n2)
      ) |>
      dplyr::group_by(repr_bin, cs_bin) |>
      dplyr::summarise(
        mae_n2 = mean(err_n2, na.rm = TRUE),
        mae_n3 = mean(err_n3, na.rm = TRUE),
        n = dplyr::n(),
        .groups = "drop"
      ) |>
      tidyr::pivot_longer(
        cols = c(mae_n2, mae_n3),
        names_to = "step",
        values_to = "mae"
      ) |>
      dplyr::mutate(
        step = dplyr::recode(
          step,
          mae_n2 = "N2 vs N1",
          mae_n3 = "N3 vs N2"
        ),
        step = factor(step, levels = c("N2 vs N1", "N3 vs N2"))
      )

    plots$error_by_care_report <- ggplot2::ggplot(
      heat_df,
      ggplot2::aes(
        x = repr_bin,
        y = cs_bin,
        fill = mae
      )
    ) +
      ggplot2::geom_tile() +
      ggplot2::scale_fill_viridis_c(
        option = "plasma",
        direction = 1,
        name = "MAE"
      ) +
      ggplot2::facet_wrap(~step) +
      ggplot2::labs(
        title = "Error surface across reporting-rate x care-seeking bins",
        subtitle = paste0(
          "Contours reveal where reporting completeness and ",
          "care-seeking interact.\n",
          "N2 error is driven by poor reporting. N3 error ",
          "increases when public share is low or mixed."
        ),
        x = "\n\nReporting-rate bin",
        y = "Care-seeking public-share bin\n\n"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        strip.text = ggplot2::element_text(face = "bold", size = 11),
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
        panel.border = ggplot2::element_rect(fill = NA, colour = "black"),
        legend.position = "right"
      )

    # 5) seasonality profiles by ADM1~ADM2 (location) --------------------------

    adm2_month <- df |>
      dplyr::mutate(
        month_num = month,
        month = factor(
          lubridate::month(month_num, label = TRUE, abbr = TRUE),
          levels = lubridate::month(1:12, label = TRUE, abbr = TRUE),
          ordered = TRUE
        )
      ) |>
      dplyr::group_by(location, month) |>
      dplyr::summarise(
        n0_m = mean(n0, na.rm = TRUE),
        n1_m = mean(n1, na.rm = TRUE),
        n2_m = mean(n2, na.rm = TRUE),
        n3_m = mean(n3, na.rm = TRUE),
        .groups = "drop"
      ) |>
      tidyr::pivot_longer(
        cols = c("n0_m", "n1_m", "n2_m", "n3_m"),
        names_to = "level",
        values_to = "incidence"
      ) |>
      dplyr::mutate(
        level = dplyr::recode(
          level,
          n0_m = "N0",
          n1_m = "N1",
          n2_m = "N2",
          n3_m = "N3"
        )
      )

    plots$seasonality <- ggplot2::ggplot(
      adm2_month,
      ggplot2::aes(
        x = month,
        y = incidence,
        colour = level,
        group = level
      )
    ) +
      ggplot2::geom_line(linewidth = 0.7) +
      ggplot2::facet_wrap(~location, scales = "free_y") +
      ggplot2::labs(
        title = "Monthly seasonality profiles by ADM1~ADM2",
        subtitle = paste0(
          "Shows whether adjusted incidence (N1-N3) preserves the seasonal ",
          "timing and amplitude observed in N0.\n"
        ),
        x = "\n\nMonth",
        y = "Incidence per 1,000\n\n"
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

    cli::cli_alert_success("Incidence cascade diagnostic plots ready.")
  }

  # return structure -----------------------------------------------------------

  out <- list(
    metrics = metrics,
    validation_data = df
  )

  if (generate_plots) {
    out$plots <- plots
  }

  out
}
