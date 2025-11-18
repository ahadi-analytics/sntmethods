#' Calculate Test Positivity Rate from Routine Health Facility Data
#'
#' Calculates malaria Test Positivity Rate (TPR) at the health facility-month
#' level and applies structured fallback logic to derive proxy values when
#' confirmed or tested data are missing. Input data must be at the health
#' facility-month level, with one observation per facility per reporting month.
#' Returns a validated TPR dataset with quality flags and source tracking.
#'
#' The fallback hierarchy for proxy TPR values is:
#' 1. District (adm2) TPR from same month
#' 2. Regional (adm1) TPR from same month
#' 3. Same month from previous year (facility-level)
#' 4. National (adm0) TPR from same month
#'
#' @param data Routine health facility data at the facility-month level
#'   (data.frame or tibble). Must contain one row per facility per month.
#' @param hf_var Column name for health facility unique identifier
#'   (default: "hf_uid").
#' @param adm0_var Column name for national/country level. If NULL (default),
#'   creates a single "country" value for all records.
#' @param adm1_var Column name for first administrative level/region
#'   (default: "adm1").
#' @param adm2_var Column name for second administrative level/district
#'   (default: "adm2").
#' @param date_var Column name for date of reporting period (default: "date").
#' @param conf_var Column name for number of confirmed malaria cases
#'   (default: "conf").
#' @param test_var Column name for number of individuals tested
#'   (default: "test").
#' @param pres_var Column name for number of presumed cases (default: "pres").
#' @param reporting_threshold Numeric threshold for minimum reporting rate
#'   used when calculating district-level proxies. Facility-months from
#'   districts with reporting rate below this value are excluded from proxy
#'   calculations but kept in output. Default is 0.8 (80%). Reporting rate
#'   is calculated internally using `sntutils::calculate_reporting_metrics()`.
#' @param extreme_threshold Numeric vector of length 2 specifying lower and
#'   upper bounds for flagging extreme TPR values (default: `c(0.01, 0.99)`).
#' @param include_flags Logical; if `TRUE`, includes all quality flag columns
#'   in the output. If `FALSE` (default), returns only the core TPR variables
#'   without flags.
#' @param activity_indicators Character vector of indicator columns used to
#'   calculate reporting rate and determine facility activity. Default is
#'   `c("conf", "test")`.
#' @param activity_method Numeric. Classification method for facility activity
#'   (1, 2, or 3). Default is 3 (dynamic activation/inactivation). Used to
#'   flag inactive facility-months which are excluded from proxy calculations.
#' @param nonreport_window Integer. Number of consecutive non-reporting periods
#'   before a facility is considered inactive (for method 3). Default is 6.
#' @param fallback_method Character vector specifying which proxy fallback
#'   levels to use and their order. Valid options: "adm2" (district-level),
#'   "adm1" (regional-level), "prev_year" (same month previous year), "adm0"
#'   (national-level), "rolling" (3-month rolling average from same facility).
#'   Default is `c("adm2", "adm1", "prev_year", "adm0")`. Proxies are applied
#'   sequentially in the order specified. Set to `character(0)` to disable all
#'   fallbacks.
#' @param prev_year_window Integer specifying the seasonal window in months
#'   for previous year fallback. 0 = exact month match only (default), 1 = +/-1
#'   month window (3-month average), 2 = +/-2 months (5-month average), etc.
#'   Maximum value is 6. When window > 0, averages all available months within
#'   the window from the previous year, weighted by test counts.
#' @param fallback_triggers Character vector specifying when to apply proxy
#'   fallbacks. Valid options: "missing" (conf/test is NA), "extreme" (TPR
#'   outside extreme_threshold), "low_test" (test < 5), "impossible" (conf >
#'   test), "low_reprate" (reporting rate < threshold). Default is
#'   `c("missing")`. Multiple triggers can be combined.
#'
#' @return A list containing three elements:
#'   \itemize{
#'     \item `data`: A tibble with TPR estimates per facility-month including:
#'       \itemize{
#'         \item `hf_uid`: Health facility identifier
#'         \item `adm0`: National/country level
#'         \item `adm1`: First administrative level
#'         \item `adm2`: Second administrative level
#'         \item `date`: Standardised date (first of month)
#'         \item `year`: Year extracted from date
#'         \item `month`: Month extracted from date
#'         \item `conf`: Confirmed cases
#'         \item `test`: Number tested
#'         \item `pres`: Presumed cases
#'         \item `reprate`: Reporting rate (0-1 scale)
#'         \item `tpr`: Final validated or proxy TPR (0-1 scale)
#'         \item `tpr_source`: Source of TPR value (facility_raw, proxy_adm2,
#'           proxy_adm1, proxy_prev_year, or proxy_adm0)
#'         \item `flag_tpr_valid`: TRUE if raw TPR could be calculated
#'         \item `flag_tpr_extreme`: TRUE if TPR outside extreme_threshold
#'         \item `flag_tpr_proxy`: TRUE if proxy value was used
#'         \item `flag_tpr_missing`: TRUE if no TPR could be assigned
#'         \item `flag_conf_gt_test`: TRUE if conf > test (impossible value)
#'         \item `flag_zero_test`: TRUE if test == 0
#'         \item `flag_low_test`: TRUE if test < 5
#'         \item `flag_missing_conf`: TRUE if conf is NA
#'         \item `flag_inactive`: TRUE if facility-month is inactive
#'         \item `flag_low_reprate`: TRUE if reporting rate below threshold
#'       }
#'   }
#'
#' @examples
#' # Example with minimal data
#' # facility_data <- tibble::tibble(
#' #   hf_uid = c("HF001", "HF002", "HF003"),
#' #   adm1 = rep("RegionA", 3),
#' #   adm2 = rep("DistrictX", 3),
#' #   date = as.Date(c("2023-01-01", "2023-02-01", "2023-01-01")),
#' #   conf = c(10, 15, NA),
#' #   test = c(100, 120, 80),
#' #   pres = c(5, 8, 10),
#' #   report_complete = c(1, 1, 1)
#' # )
#' #
#' # result <- calc_tpr(facility_data)
#' # tpr_data <- result$data
#' # tpr_dict <- result$dict
#' # result$metadata
#'
#' @export
calc_tpr <- function(
  data,
  hf_var = "hf_uid",
  adm0_var = NULL,
  adm1_var = "adm1",
  adm2_var = "adm2",
  date_var = "date",
  conf_var = "conf",
  test_var = "test",
  pres_var = "pres",
  reporting_threshold = .80,
  extreme_threshold = c(0.01, 0.99),
  include_flags = FALSE,
  activity_indicators = c("conf", "test"),
  activity_method = 3,
  nonreport_window = 6,
  fallback_method = c("adm2", "prev_year", "adm1", "adm0"),
  prev_year_window = 2,
  fallback_triggers = c("missing")
) {
  # ---- validate data -------------------------------------------------------

  if (!is.data.frame(data)) {
    cli::cli_abort("`data` must be a data.frame or tibble.")
  }

  if (nrow(data) == 0) {
    cli::cli_abort("`data` is empty.")
  }

  required <- c(
    hf_var,
    adm1_var,
    adm2_var,
    date_var,
    conf_var,
    test_var,
    pres_var
  )

  missing_cols <- setdiff(required, names(data))

  if (length(missing_cols) > 0) {
    cli::cli_abort(
      c(
        "Missing required columns:",
        "{.var {missing_cols}}"
      )
    )
  }

  if (
    !is.numeric(extreme_threshold) ||
      length(extreme_threshold) != 2
  ) {
    cli::cli_abort(
      "`extreme_threshold` must be numeric length 2."
    )
  }

  if (extreme_threshold[1] >= extreme_threshold[2]) {
    cli::cli_abort(
      "extreme_threshold[1] must be < extreme_threshold[2]."
    )
  }

  # Validate fallback_method
  valid_methods <- c("adm2", "adm1", "prev_year", "adm0", "rolling")
  invalid_methods <- setdiff(fallback_method, valid_methods)

  if (length(invalid_methods) > 0) {
    cli::cli_abort(
      c(
        "Invalid fallback method(s):",
        "x" = "{.var {invalid_methods}}",
        "i" = "Valid options: {.val {valid_methods}}"
      )
    )
  }

  # Validate prev_year_window
  if (!is.numeric(prev_year_window) || length(prev_year_window) != 1) {
    cli::cli_abort("`prev_year_window` must be a single numeric value.")
  }

  if (prev_year_window < 0 || prev_year_window > 6) {
    cli::cli_abort("`prev_year_window` must be between 0 and 6.")
  }

  # Validate fallback_triggers
  valid_triggers <- c(
    "missing",
    "extreme",
    "low_test",
    "impossible",
    "low_reprate"
  )
  invalid_triggers <- setdiff(fallback_triggers, valid_triggers)

  if (length(invalid_triggers) > 0) {
    cli::cli_abort(
      c(
        "Invalid fallback trigger(s):",
        "x" = "{.var {invalid_triggers}}",
        "i" = "Valid options: {.val {valid_triggers}}"
      )
    )
  }

  cli::cli_alert_info(
    "Processing {sntutils::big_mark(nrow(data))} facility-month records."
  )

  # ---- calculate reporting rate --------------------------------------------

  cli::cli_alert_info("Calculating reporting rate...")

  suppressMessages(
    reporting_metrics <- sntutils::calculate_reporting_metrics(
      data = data,
      hf_col = hf_var,
      key_indicators = activity_indicators[1],
      vars_of_interest = activity_indicators,
      x_var = date_var,
      y_var = adm2_var
    )
  )

  # Join reporting rate back to main data
  data <- data |>
    dplyr::left_join(
      reporting_metrics |>
        dplyr::select(
          !!adm2_var,
          !!date_var,
          reprate
        ),
      by = c(adm2_var, date_var)
    )

  # ---- flag inactive facilities --------------------------------------------

  cli::cli_alert_info("Identifying inactive facilities...")

  # Get active facility-months
  active_data <- get_active_facilities(
    data = data,
    hf_col = hf_var,
    date_col = date_var,
    key_indicators = activity_indicators,
    method = activity_method,
    nonreport_window = nonreport_window,
    return_summary = FALSE
  )

  # Create key for active facility-months

  active_keys <- active_data |>
    dplyr::mutate(
      .active_key = paste(.data[[hf_var]], .data[[date_var]], sep = "_")
    ) |>
    dplyr::pull(.active_key)

  # Flag inactive facility-months in original data
  data <- data |>
    dplyr::mutate(
      .temp_key = paste(.data[[hf_var]], .data[[date_var]], sep = "_"),
      flag_inactive = !.temp_key %in% active_keys
    ) |>
    dplyr::select(-.temp_key)

  n_inactive <- sum(data$flag_inactive)
  n_total <- nrow(data)

  cli::cli_alert_info(
    "{sntutils::big_mark(n_inactive)} of {sntutils::big_mark(n_total)} facility-months flagged as inactive."
  )

  # ---- rename vars internally ----------------------------------------------

  df <- data |>
    dplyr::rename(
      hf_uid = !!hf_var,
      adm1 = !!adm1_var,
      adm2 = !!adm2_var,
      date_raw = !!date_var,
      conf = !!conf_var,
      test = !!test_var,
      pres = !!pres_var
    ) |>
    dplyr::mutate(
      conf = as.numeric(conf),
      test = as.numeric(test),
      pres = as.numeric(pres)
    )

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

  # ---- date handling -------------------------------------------------------

  df <- df |>
    dplyr::mutate(
      date = as.Date(date_raw),
      date = lubridate::floor_date(date, "month"),
      year = lubridate::year(date),
      month = lubridate::month(date)
    )

  # ---- duplicate check -----------------------------------------------------

  n_dup <- df |>
    dplyr::group_by(hf_uid, date) |>
    dplyr::filter(dplyr::n() > 1) |>
    nrow()

  if (n_dup > 0) {
    cli::cli_alert_warning(
      "Found {sntutils::big_mark(n_dup)} duplicate facility-months."
    )
  }

  # ---- impossible values ---------------------------------------------------

  df <- df |>
    dplyr::mutate(
      flag_conf_gt_test = dplyr::if_else(
        !is.na(conf) & !is.na(test) & conf > test,
        TRUE,
        FALSE,
        missing = FALSE
      )
    )

  n_imp <- sum(df$flag_conf_gt_test)

  if (n_imp > 0) {
    cli::cli_alert_warning(
      "{sntutils::big_mark(n_imp)} rows have conf > test."
    )
  }

  # ---- flags ---------------------------------------------------------------

  df <- df |>
    dplyr::mutate(
      flag_zero_test = test == 0,
      flag_low_test = !is.na(test) & test < 5,
      flag_missing_conf = is.na(conf),
      flag_low_reprate = !is.na(reprate) & reprate < reporting_threshold,
      test = dplyr::if_else(test == 0, NA_real_, test)
    )

  n_low_reprate <- sum(df$flag_low_reprate)
  if (n_low_reprate > 0) {
    cli::cli_alert_info(
      "{sntutils::big_mark(n_low_reprate)} facility-months below ",
      "{reporting_threshold * 100}% reporting."
    )
  }

  # ---- clean dataset for proxies -------------------------------------------
  # Exclude: impossible values, inactive facilities, low reporting rate

  df_clean <- df |>
    dplyr::filter(
      !flag_conf_gt_test,
      !flag_inactive,
      !flag_low_reprate
    )

  # ---- raw tpr --------------------------------------------------------------

  df_raw <- df_clean |>
    dplyr::mutate(
      tpr_raw = dplyr::if_else(
        !is.na(conf) & !is.na(test),
        conf / test,
        NA_real_
      )
    ) |>
    dplyr::select(hf_uid, date, tpr_raw)

  df <- df |>
    dplyr::left_join(df_raw, by = c("hf_uid", "date"))

  n_raw <- sum(!is.na(df$tpr_raw))

  cli::cli_alert_info(
    "Calculated raw TPR for {sntutils::big_mark(n_raw)} facility-months."
  )

  df <- df |>
    dplyr::mutate(
      tpr = tpr_raw,
      tpr_source = dplyr::if_else(
        !is.na(tpr_raw),
        "facility_raw",
        NA_character_
      )
    )

  # ---- fallback proxies -----------------------------------------------------
  # Build dynamic condition for when to apply fallbacks

  # Always exclude inactive facilities
  df <- df |>
    dplyr::mutate(.should_fallback = !flag_inactive)

  # Handle each trigger
  if (!"impossible" %in% fallback_triggers) {
    df <- df |>
      dplyr::mutate(.should_fallback = .should_fallback & !flag_conf_gt_test)
  }

  if ("missing" %in% fallback_triggers) {
    df <- df |>
      dplyr::mutate(
        .should_fallback = .should_fallback & is.na(tpr)
      )
  }

  if ("extreme" %in% fallback_triggers) {
    df <- df |>
      dplyr::mutate(
        .should_fallback = .should_fallback &
          (is.na(tpr) | flag_tpr_extreme)
      )
  }

  if ("low_test" %in% fallback_triggers) {
    df <- df |>
      dplyr::mutate(
        .should_fallback = .should_fallback &
          (is.na(tpr) | flag_low_test)
      )
  }

  if ("low_reprate" %in% fallback_triggers) {
    df <- df |>
      dplyr::mutate(
        .should_fallback = .should_fallback &
          (is.na(tpr) | flag_low_reprate)
      )
  }

  # Define fallback helper functions
  apply_adm2_fallback <- function(df_main, df_clean_data) {
    adm2_tpr <- df_clean_data |>
      dplyr::filter(!is.na(conf), !is.na(test)) |>
      dplyr::group_by(adm2, year, month) |>
      dplyr::summarise(
        tpr_adm2 = sum(conf) / sum(test),
        .groups = "drop"
      )

    df_main |>
      dplyr::left_join(adm2_tpr, by = c("adm2", "year", "month")) |>
      dplyr::mutate(
        tpr = dplyr::if_else(
          .should_fallback & !is.na(tpr_adm2),
          tpr_adm2,
          tpr
        ),
        tpr_source = dplyr::if_else(
          .should_fallback & !is.na(tpr_adm2),
          "proxy_adm2",
          tpr_source
        ),
        .should_fallback = dplyr::if_else(
          !is.na(tpr_adm2),
          FALSE,
          .should_fallback
        )
      ) |>
      dplyr::select(-tpr_adm2)
  }

  apply_adm1_fallback <- function(df_main, df_clean_data) {
    adm1_tpr <- df_clean_data |>
      dplyr::filter(!is.na(conf), !is.na(test)) |>
      dplyr::group_by(adm1, year, month) |>
      dplyr::summarise(
        tpr_adm1 = sum(conf) / sum(test),
        .groups = "drop"
      )

    df_main |>
      dplyr::left_join(adm1_tpr, by = c("adm1", "year", "month")) |>
      dplyr::mutate(
        tpr = dplyr::if_else(
          .should_fallback & !is.na(tpr_adm1),
          tpr_adm1,
          tpr
        ),
        tpr_source = dplyr::if_else(
          .should_fallback & !is.na(tpr_adm1),
          "proxy_adm1",
          tpr_source
        ),
        .should_fallback = dplyr::if_else(
          !is.na(tpr_adm1),
          FALSE,
          .should_fallback
        )
      ) |>
      dplyr::select(-tpr_adm1)
  }

  apply_prev_year_fallback <- function(df_main, df_clean_data, window) {
    if (window == 0) {
      # Exact month match
      prev <- df_main |>
        dplyr::filter(!is.na(tpr_raw), !flag_conf_gt_test) |>
        dplyr::mutate(year = year + 1) |>
        dplyr::select(hf_uid, year, month, tpr_prev_year = tpr_raw)

      df_main |>
        dplyr::left_join(prev, by = c("hf_uid", "year", "month")) |>
        dplyr::mutate(
          tpr = dplyr::if_else(
            .should_fallback & !is.na(tpr_prev_year),
            tpr_prev_year,
            tpr
          ),
          tpr_source = dplyr::if_else(
            .should_fallback & !is.na(tpr_prev_year),
            "proxy_prev_year",
            tpr_source
          ),
          .should_fallback = dplyr::if_else(
            !is.na(tpr_prev_year),
            FALSE,
            .should_fallback
          )
        ) |>
        dplyr::select(-tpr_prev_year)
    } else {
      # Seasonal window average
      prev <- df_main |>
        dplyr::filter(!is.na(tpr_raw), !flag_conf_gt_test) |>
        dplyr::select(hf_uid, year, month, conf, test, tpr_raw)

      # Create all month combinations within window
      df_expanded <- df_main |>
        dplyr::select(hf_uid, year, month) |>
        dplyr::distinct() |>
        tidyr::expand_grid(month_offset = -window:window) |>
        dplyr::mutate(
          prev_year = year - 1,
          prev_month = month + month_offset,
          prev_month = dplyr::case_when(
            prev_month < 1 ~ prev_month + 12,
            prev_month > 12 ~ prev_month - 12,
            TRUE ~ prev_month
          )
        )

      # Join and average
      prev_avg <- df_expanded |>
        dplyr::left_join(
          prev |>
            dplyr::rename(
              prev_year = year,
              prev_month = month,
              prev_conf = conf,
              prev_test = test
            ),
          by = c("hf_uid", "prev_year", "prev_month")
        ) |>
        dplyr::filter(!is.na(prev_conf), !is.na(prev_test)) |>
        dplyr::group_by(hf_uid, year, month) |>
        dplyr::summarise(
          tpr_prev_year = sum(prev_conf) / sum(prev_test),
          .groups = "drop"
        )

      df_main |>
        dplyr::left_join(prev_avg, by = c("hf_uid", "year", "month")) |>
        dplyr::mutate(
          tpr = dplyr::if_else(
            .should_fallback & !is.na(tpr_prev_year),
            tpr_prev_year,
            tpr
          ),
          tpr_source = dplyr::if_else(
            .should_fallback & !is.na(tpr_prev_year),
            "proxy_prev_year",
            tpr_source
          ),
          .should_fallback = dplyr::if_else(
            !is.na(tpr_prev_year),
            FALSE,
            .should_fallback
          )
        ) |>
        dplyr::select(-tpr_prev_year)
    }
  }

  apply_adm0_fallback <- function(df_main, df_clean_data) {
    adm0_tpr <- df_clean_data |>
      dplyr::filter(!is.na(conf), !is.na(test)) |>
      dplyr::group_by(adm0, year, month) |>
      dplyr::summarise(
        tpr_adm0 = sum(conf) / sum(test),
        .groups = "drop"
      )

    df_main |>
      dplyr::left_join(adm0_tpr, by = c("adm0", "year", "month")) |>
      dplyr::mutate(
        tpr = dplyr::if_else(
          .should_fallback & !is.na(tpr_adm0),
          tpr_adm0,
          tpr
        ),
        tpr_source = dplyr::if_else(
          .should_fallback & !is.na(tpr_adm0),
          "proxy_adm0",
          tpr_source
        ),
        .should_fallback = dplyr::if_else(
          !is.na(tpr_adm0),
          FALSE,
          .should_fallback
        )
      ) |>
      dplyr::select(-tpr_adm0)
  }

  apply_rolling_fallback <- function(df_main, df_clean_data) {
    # 3-month rolling average (+/-1 month) from same facility, same year
    rolling_data <- df_main |>
      dplyr::filter(!is.na(tpr_raw), !flag_conf_gt_test) |>
      dplyr::select(hf_uid, year, month, conf, test, tpr_raw)

    # Create expanded dataset with +/-1 month offsets
    df_expanded <- df_main |>
      dplyr::select(hf_uid, year, month) |>
      dplyr::distinct() |>
      tidyr::expand_grid(month_offset = -1:1) |>
      dplyr::mutate(
        rolling_month = month + month_offset,
        rolling_month = dplyr::case_when(
          rolling_month < 1 ~ rolling_month + 12,
          rolling_month > 12 ~ rolling_month - 12,
          TRUE ~ rolling_month
        )
      )

    # Join and average (weighted by test counts)
    rolling_avg <- df_expanded |>
      dplyr::left_join(
        rolling_data |>
          dplyr::rename(
            rolling_year = year,
            rolling_month = month,
            rolling_conf = conf,
            rolling_test = test
          ),
        by = c("hf_uid", "year" = "rolling_year", "rolling_month")
      ) |>
      dplyr::filter(
        !is.na(rolling_conf),
        !is.na(rolling_test),
        month_offset != 0  # Exclude the target month itself
      ) |>
      dplyr::group_by(hf_uid, year, month) |>
      dplyr::summarise(
        tpr_rolling = sum(rolling_conf) / sum(rolling_test),
        n_months_used = dplyr::n(),
        .groups = "drop"
      ) |>
      dplyr::filter(n_months_used >= 1)  # At least 1 other month

    df_main |>
      dplyr::left_join(rolling_avg, by = c("hf_uid", "year", "month")) |>
      dplyr::mutate(
        tpr = dplyr::if_else(
          .should_fallback & !is.na(tpr_rolling),
          tpr_rolling,
          tpr
        ),
        tpr_source = dplyr::if_else(
          .should_fallback & !is.na(tpr_rolling),
          "proxy_rolling",
          tpr_source
        ),
        .should_fallback = dplyr::if_else(
          !is.na(tpr_rolling),
          FALSE,
          .should_fallback
        )
      ) |>
      dplyr::select(-tpr_rolling, -n_months_used)
  }

  # Apply fallbacks in user-specified order
  if (length(fallback_method) > 0) {
    cli::cli_alert_info(
      "Applying fallback proxies: {paste(fallback_method, collapse = ' -> ')}"
    )

    for (method in fallback_method) {
      df <- switch(
        method,
        "adm2" = apply_adm2_fallback(df, df_clean),
        "adm1" = apply_adm1_fallback(df, df_clean),
        "prev_year" = apply_prev_year_fallback(df, df_clean, prev_year_window),
        "adm0" = apply_adm0_fallback(df, df_clean),
        "rolling" = apply_rolling_fallback(df, df_clean),
        {
          cli::cli_warn("Unknown fallback method: {method}")
          df
        }
      )
    }
  } else {
    cli::cli_alert_info("No fallback methods specified. TPR = raw only.")
  }

  # Remove temporary column
  df <- df |> dplyr::select(-.should_fallback)

  # ---- quality flags --------------------------------------------------------

  df <- df |>
    dplyr::mutate(
      flag_tpr_valid = !is.na(tpr_raw),
      flag_tpr_proxy = tpr_source != "facility_raw" &
        !is.na(tpr_source),
      flag_tpr_extreme = dplyr::if_else(
        !is.na(tpr),
        tpr < extreme_threshold[1] |
          tpr > extreme_threshold[2],
        FALSE
      ),
      flag_tpr_missing = is.na(tpr)
    )

  # ---- summary --------------------------------------------------------------

  proxy_counts <- df |>
    dplyr::count(tpr_source)

  for (i in seq_len(nrow(proxy_counts))) {
    src <- proxy_counts$tpr_source[i]
    n <- proxy_counts$n[i]

    if (!is.na(src)) {
      cli::cli_alert_info(
        "{sntutils::big_mark(n)} facility-months with source = {src}."
      )
    }
  }

  # Breakdown of missing TPR
  n_missing_inactive <- sum(is.na(df$tpr) & df$flag_inactive)
  n_missing_conf_gt_test <- sum(is.na(df$tpr) & df$flag_conf_gt_test)
  n_missing_other <- sum(
    is.na(df$tpr) & !df$flag_inactive & !df$flag_conf_gt_test
  )

  if (n_missing_inactive > 0) {
    cli::cli_alert_info(
      "{sntutils::big_mark(n_missing_inactive)} facility-months NA (inactive)."
    )
  }

  if (n_missing_conf_gt_test > 0) {
    cli::cli_alert_info(
      "{sntutils::big_mark(n_missing_conf_gt_test)} facility-months NA ",
      "(conf > test)."
    )
  }

  if (n_missing_other > 0) {
    cli::cli_alert_warning(
      "{sntutils::big_mark(n_missing_other)} facility-months still ",
      "missing TPR."
    )
  }

  n_ext <- sum(df$flag_tpr_extreme)

  if (n_ext > 0) {
    cli::cli_alert_warning(
      "{sntutils::big_mark(n_ext)} extreme TPR values flagged."
    )
  }

  # ---- metadata -------------------------------------------------------------

  metadata <- list(
    analysis_type = "TPR",
    input_rows = nrow(data),
    output_rows = nrow(df),
    n_facilities = dplyr::n_distinct(df$hf_uid),
    n_admin1 = dplyr::n_distinct(df$adm1),
    n_admin2 = dplyr::n_distinct(df$adm2),
    date_range = c(
      min(df$date, na.rm = TRUE),
      max(df$date, na.rm = TRUE)
    ),
    n_raw_tpr = sum(df$flag_tpr_valid),
    n_proxy_tpr = sum(df$flag_tpr_proxy),
    n_missing_tpr = sum(df$flag_tpr_missing),
    n_extreme_tpr = sum(df$flag_tpr_extreme),
    n_impossible_values = sum(df$flag_conf_gt_test),
    n_inactive = sum(df$flag_inactive),
    n_low_reprate = sum(df$flag_low_reprate),
    activity_method = activity_method,
    nonreport_window = nonreport_window,
    reporting_threshold = reporting_threshold,
    extreme_threshold = extreme_threshold,
    hf_var = hf_var,
    adm0_var = adm0_var,
    adm1_var = adm1_var,
    adm2_var = adm2_var,
    date_var = date_var,
    processed_date = Sys.Date(),
    processed_time = Sys.time()
  )

  # ---- final dataset --------------------------------------------------------

  # Core output columns (always included)
  core_cols <- c(
    "hf_uid",
    "adm0",
    "adm1",
    "adm2",
    "date",
    "year",
    "month",
    "conf",
    "test",
    "pres",
    "reprate",
    "tpr",
    "tpr_source"
  )

  # Flag columns (optionally included)
  flag_cols <- c(
    "flag_tpr_valid",
    "flag_tpr_extreme",
    "flag_tpr_proxy",
    "flag_tpr_missing",
    "flag_conf_gt_test",
    "flag_zero_test",
    "flag_low_test",
    "flag_missing_conf",
    "flag_inactive",
    "flag_low_reprate"
  )

  # Get all other columns from original data (preserving extra columns)
  internal_cols <- c(
    "date_raw",
    "tpr_raw"
  )

  other_cols <- setdiff(
    names(df),
    c(core_cols, flag_cols, internal_cols)
  )

  # Build final column selection
  if (include_flags) {
    final_cols <- c(core_cols, other_cols, flag_cols)
  } else {
    final_cols <- c(core_cols, other_cols)
  }

  # Select only columns that exist
  final_cols <- intersect(final_cols, names(df))

  df_final <- df |>
    dplyr::select(dplyr::all_of(final_cols)) |>
    tibble::as_tibble()

  cli::cli_alert_success(
    "TPR calculation complete for {sntutils::big_mark(nrow(df_final))} rows."
  )

  df_final
}

#' Validate TPR Proxy Quality Using Leave-One-Out Cross-Validation
#'
#' Evaluates the accuracy of TPR proxy estimates by comparing them against
#' actual facility-level TPR values. Uses leave-one-out cross-validation to
#' calculate what each proxy level (adm2, adm1, prev_year, rolling, adm0)
#' would have estimated for facility-months with valid raw TPR data.
#'
#' @param data Output from `calc_tpr()` with `include_flags = TRUE`.
#' @param hf_var Column name for health facility unique identifier. Should
#'   match the value used in `calc_tpr()`. Default is "hf_uid".
#' @param adm0_var Column name for national/country level. Default is "adm0".
#' @param adm1_var Column name for first administrative level/region. Default
#'   is "adm1".
#' @param adm2_var Column name for second administrative level/district.
#'   Default is "adm2".
#' @param year_var Column name for year. Default is "year".
#' @param month_var Column name for month. Default is "month".
#' @param conf_var Column name for confirmed cases. Default is "conf".
#' @param test_var Column name for tests. Default is "test".
#' @param min_facilities Minimum number of facilities in an admin unit to
#'   include in validation. Default is 2.
#' @param generate_plots Logical; if TRUE (default), generates diagnostic
#'   plots. Set to FALSE for metrics only.
#'
#' @return A list containing:
#'   \itemize{
#'     \item `metrics`: Data frame with validation metrics for each proxy level
#'     \item `validation_data`: Data frame with actual vs proxy comparisons
#'     \item `plots`: List of ggplot objects (if generate_plots = TRUE):
#'       \itemize{
#'         \item `scatter`: Actual vs proxy scatterplots
#'         \item `error_dist`: Error distribution by proxy level
#'         \item `mae_by_reprate`: MAE vs reporting rate
#'         \item `mae_by_nfacilities`: MAE vs number of facilities
#'         \item `error_by_month`: Proxy error by calendar month
#'       }
#'     \item `summary`: Character vector with key findings
#'   }
#'
#' @examples
#' # result <- calc_tpr(facility_data, include_flags = TRUE)
#' # validation <- validate_tpr_proxies(result)
#' # validation$metrics
#' # validation$plots$scatter
#'
#' @export
validate_tpr_proxies <- function(
  data,
  hf_var = "hf_uid",
  adm0_var = "adm0",
  adm1_var = "adm1",
  adm2_var = "adm2",
  year_var = "year",
  month_var = "month",
  conf_var = "conf",
  test_var = "test",
  min_facilities = 2,
  generate_plots = TRUE
) {
  # validate input -----------------------------------------------------------
  if (!is.data.frame(data)) {
    cli::cli_abort("`data` must be a data.frame or tibble.")
  }

  required_cols <- c(
    hf_var,
    adm0_var,
    adm1_var,
    adm2_var,
    "date",
    year_var,
    month_var,
    conf_var,
    test_var,
    "tpr",
    "tpr_source",
    "reprate",
    "flag_inactive",
    "flag_conf_gt_test",
    "flag_low_reprate"
  )

  missing_cols <- setdiff(required_cols, names(data))

  if (length(missing_cols) > 0) {
    cli::cli_abort(
      c(
        "Missing required columns in TPR data:",
        "{.var {missing_cols}}"
      )
    )
  }

  cli::cli_alert_info(
    "Validating {sntutils::big_mark(nrow(data))} facility-months."
  )

  # strict df_clean matching calc_tpr() --------------------------------------
  df_clean <- data |>
  dplyr::filter(
    !flag_conf_gt_test,
    !flag_inactive,
    !flag_low_reprate,
    !is.na(.data[[conf_var]]),
    !is.na(.data[[test_var]]),
    .data[[test_var]] > 0
  )

  if (nrow(df_clean) < 10) {
    cli::cli_abort(
      "Insufficient usable data for validation after filtering."
    )
  }

  # actual TPR for validation ------------------------------------------------
  df_valid <- df_clean |>
  dplyr::mutate(
    tpr_actual = .data[[conf_var]] / .data[[test_var]]
  )

  # leave-one-out proxy calculations ----------------------------------------
  cli::cli_alert_info("Computing leave-one-out proxy estimates...")

  # ADM2 ---------------------------------------------------------------------
  df_valid <- df_valid |>
  dplyr::group_by(
    .data[[adm2_var]],
    .data[[year_var]],
    .data[[month_var]]
  ) |>
  dplyr::mutate(
    n_fac_adm2 = dplyr::n(),
    sum_c_adm2 = sum(.data[[conf_var]]),
    sum_t_adm2 = sum(.data[[test_var]]),
    proxy_adm2 = dplyr::if_else(
      n_fac_adm2 >= min_facilities &
      (sum_t_adm2 - .data[[test_var]]) > 0,
      (sum_c_adm2 - .data[[conf_var]]) / (sum_t_adm2 - .data[[test_var]]),
      NA_real_
    )
  ) |>
  dplyr::ungroup()

  # ADM1 ---------------------------------------------------------------------
  df_valid <- df_valid |>
  dplyr::group_by(
    .data[[adm1_var]],
    .data[[year_var]],
    .data[[month_var]]
  ) |>
  dplyr::mutate(
    n_fac_adm1 = dplyr::n(),
    sum_c_adm1 = sum(.data[[conf_var]]),
    sum_t_adm1 = sum(.data[[test_var]]),
    proxy_adm1 = dplyr::if_else(
      n_fac_adm1 >= min_facilities &
      (sum_t_adm1 - .data[[test_var]]) > 0,
      (sum_c_adm1 - .data[[conf_var]]) / (sum_t_adm1 - .data[[test_var]]),
      NA_real_
    )
  ) |>
  dplyr::ungroup()

  # ADM0 ---------------------------------------------------------------------
  df_valid <- df_valid |>
  dplyr::group_by(
    .data[[adm0_var]],
    .data[[year_var]],
    .data[[month_var]]
  ) |>
  dplyr::mutate(
    n_fac_adm0 = dplyr::n(),
    sum_c_adm0 = sum(.data[[conf_var]]),
    sum_t_adm0 = sum(.data[[test_var]]),
    proxy_adm0 = dplyr::if_else(
      n_fac_adm0 >= min_facilities &
      (sum_t_adm0 - .data[[test_var]]) > 0,
      (sum_c_adm0 - .data[[conf_var]]) / (sum_t_adm0 - .data[[test_var]]),
      NA_real_
    )
  ) |>
  dplyr::ungroup()

  # previous year -------------------------------------------------------------
  # primary: same month last year
  prev_same <- df_valid |>
  dplyr::mutate(
    "{year_var}" := .data[[year_var]] + 1,
    "{month_var}" := .data[[month_var]]
  ) |>
  dplyr::select(
    !!hf_var,
    !!year_var,
    !!month_var,
    proxy_prev_year_same = tpr_actual
  )

  # fallback: previous month last year
  prev_prev_month <- df_valid |>
  dplyr::mutate(
    "{year_var}" := .data[[year_var]] + 1,
    "{month_var}" := .data[[month_var]] - 1
  ) |>
  # wrap-around (Jan -> Dec)
  dplyr::mutate(
    "{month_var}" := dplyr::if_else(
      .data[[month_var]] < 1,
      .data[[month_var]] + 12,
      .data[[month_var]]
    )
  ) |>
  dplyr::select(
    !!hf_var,
    !!year_var,
    !!month_var,
    proxy_prev_year_prev_month = tpr_actual
  )

  # merge previous-year proxies
  df_valid <- df_valid |>
  dplyr::left_join(
    prev_same,
    by = c(hf_var, year_var, month_var)
  ) |>
  dplyr::left_join(
    prev_prev_month,
    by = c(hf_var, year_var, month_var)
  ) |>
  dplyr::mutate(
    proxy_prev_year = dplyr::coalesce(
      proxy_prev_year_same,
      proxy_prev_year_prev_month
    )
  )

  # rolling 3-month mean (facility) --------------------------------------------
  # weighted average by test counts, matching calc_tpr() implementation
  rolling_data <- df_valid |>
  dplyr::select(
    !!hf_var,
    !!year_var,
    !!month_var,
    !!conf_var,
    !!test_var,
    tpr_actual
  )

  # Create expanded dataset with +/-1 month offsets
  df_expanded <- df_valid |>
  dplyr::select(!!hf_var, !!year_var, !!month_var) |>
  dplyr::distinct() |>
  tidyr::expand_grid(month_offset = -1:1) |>
  dplyr::mutate(
    rolling_month = .data[[month_var]] + month_offset,
    rolling_month = dplyr::case_when(
      rolling_month < 1 ~ rolling_month + 12,
      rolling_month > 12 ~ rolling_month - 12,
      TRUE ~ rolling_month
    )
  )

  # Join and compute weighted average (excluding target month itself)
  rolling_avg <- df_expanded |>
  dplyr::left_join(
    rolling_data |>
      dplyr::rename(
        rolling_year = !!rlang::sym(year_var),
        rolling_month_join = !!rlang::sym(month_var),
        rolling_conf = !!rlang::sym(conf_var),
        rolling_test = !!rlang::sym(test_var)
      ),
    by = stats::setNames(
      c(hf_var, "rolling_year", "rolling_month_join"),
      c(hf_var, year_var, "rolling_month")
    )
  ) |>
  dplyr::filter(
    !is.na(rolling_conf),
    !is.na(rolling_test),
    month_offset != 0  # Exclude the target month itself
  ) |>
  dplyr::group_by(!!rlang::sym(hf_var), !!rlang::sym(year_var),
                  !!rlang::sym(month_var)) |>
  dplyr::summarise(
    proxy_roll3 = sum(rolling_conf) / sum(rolling_test),
    n_months_used = dplyr::n(),
    .groups = "drop"
  ) |>
  dplyr::filter(n_months_used >= 1)

  df_valid <- df_valid |>
  dplyr::left_join(
    rolling_avg,
    by = c(hf_var, year_var, month_var)
  ) |>
  dplyr::select(-n_months_used)

  # proxy adjacent month ------------------------------------------------------------
  df_valid <- df_valid |>
  dplyr::group_by(.data[[hf_var]]) |>
  dplyr::arrange(.data[[year_var]], .data[[month_var]]) |>
  dplyr::mutate(
    proxy_adjacent_month = dplyr::coalesce(
      dplyr::lag(tpr_actual, 1), # previous month
      dplyr::lead(tpr_actual, 1) # next month
    )
  ) |>
  dplyr::ungroup()

  # error metrics -------------------------------------------------------------
  calc_metrics <- function(actual, proxy, name) {
    ok <- !is.na(actual) & !is.na(proxy)
    if (sum(ok) < 2) {
      return(
        tibble::tibble(
          proxy_level = name,
          n = sum(ok),
          mae = NA_real_,
          medae = NA_real_,
          rmse = NA_real_,
          mape = NA_real_,
          correlation = NA_real_,
          bias = NA_real_,
          cal_slope = NA_real_,
          cal_intercept = NA_real_
        )
      )
    }

    a <- actual[ok]
    p <- proxy[ok]
    e <- p - a

    # safe regression (handles near-zero variance)
    lm_fit <- try(stats::lm(p ~ a), silent = TRUE)

    if (inherits(lm_fit, "try-error")) {
      slope <- NA_real_
      intercept <- NA_real_
    } else {
      slope <- stats:: coef(lm_fit)[2]
      intercept <- stats:: coef(lm_fit)[1]
    }

    tibble::tibble(
      proxy_level = name,
      n = length(a),
      mae = mean(abs(e)),
      medae = stats::median(abs(e)),
      rmse = sqrt(mean(e^2)),
      mape = mean(abs(e) / pmax(a, 1e-6)),
      correlation = stats::cor(a, p),
      bias = mean(e),
      cal_slope = slope,
      cal_intercept = intercept
    )
  }

    metrics <- dplyr::bind_rows(
      calc_metrics(df_valid$tpr_actual, df_valid$proxy_adm2, "adm2"),
      calc_metrics(df_valid$tpr_actual, df_valid$proxy_adm1, "adm1"),
      calc_metrics(df_valid$tpr_actual, df_valid$proxy_prev_year, "prev_year"),
      calc_metrics(df_valid$tpr_actual, df_valid$proxy_roll3, "rolling"),
      calc_metrics(df_valid$tpr_actual, df_valid$proxy_adm0, "adm0")
    )

  cli::cli_alert_success("Computed validation metrics.")
  print(metrics)

  # plots --------------------------------------------------------------------
  plots <- list()

  if (generate_plots) {
    cli::cli_alert_info("Generating diagnostic plots...")

    long <- df_valid |>
    tidyr::pivot_longer(
      cols = c(
        proxy_adm2,
        proxy_adm1,
        proxy_prev_year,
        proxy_adjacent_month,
        proxy_roll3,
        proxy_adm0
      ),
      names_to = "proxy_level",
      names_prefix = "proxy_",
      values_to = "proxy_value"
    ) |>
    dplyr::mutate(
      error = proxy_value - tpr_actual
    ) |>
    # relabel proxy levels for cleaner plots
    dplyr::mutate(
      proxy_level = factor(
        proxy_level,
        levels = c(
          "adm2",
          "adm1",
          "prev_year",
          "adjacent_month",
          "roll3",
          "adm0"
        ),
        labels = c(
          "ADM2 (District)",
          "ADM1 (Region)",
          "Last Year (Same or -/+1 Month)",
          "Last or Next Month",
          "Rolling 3-Month Mean (Facility)",
          "ADM0 (National)"
        )
      )
    )

    # scatter plot
    plots$scatter <- ggplot2::ggplot(
      long |> dplyr::filter(!is.na(proxy_value)),
      ggplot2::aes(tpr_actual, proxy_value)
    ) +
    ggplot2::geom_point(alpha = 0.25, size = 0.5) +
    ggplot2::geom_abline(
      slope = 1,
      intercept = 0,
      colour = "red",
      linetype = "dashed"
    ) +
    ggplot2::facet_wrap(~proxy_level) +
    ggplot2::labs(
      title = "Actual vs Proxy TPR",
      y = "Proxy TPR Value\n\n",
      x = "\n\nActual TPR Value"
    ) +
    ggplot2::coord_equal() +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.margin = ggplot2::margin(10, 10, 10, 10),
      panel.border = ggplot2::element_rect(
        colour = "black",
        fill = NA,
        linewidth = 1
      )
    )

    # error distribution
    plots$error_dist <- ggplot2::ggplot(
      long |> dplyr::filter(!is.na(error)),
      ggplot2::aes(x = error, fill = proxy_level)
    ) +
      ggplot2::geom_density(alpha = 0.45, linewidth = 0.2) +
      ggplot2::geom_vline(
        xintercept = 0,
        colour = "gray30",
        linewidth = 0.6,
        linetype = 3
      ) +
      ggplot2::theme_minimal() +
      ggplot2::labs(
        title = "Error Distribution",
        subtitle = "Proxy TPR minus actual TPR",
        x = "\n\nError",
        y = "Density\n\n",
        fill = "Proxy Level"
      ) +
      ggplot2::facet_wrap(
        ~proxy_level
      ) +
      ggplot2::scale_fill_brewer(palette = "Spectral") +
    ggplot2::guides(fill = "none") +
    ggplot2::theme(
      plot.margin = ggplot2::margin(15, 15, 15, 15),
      panel.border = ggplot2::element_rect(
        colour = "black",
        fill = NA,
        linewidth = 1
      ),
      legend.position = "right",
      legend.title = ggplot2::element_text(size = 10),
      legend.text = ggplot2::element_text(size = 9),
      plot.title = ggplot2::element_text(size = 12, face = "bold"),
      plot.subtitle = ggplot2::element_text(size = 10),
      axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 5)),
      axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 5))
    )

    # MAE vs reprate
    mae_reprate <- long |>
    dplyr::mutate(
      reprate_bin = cut(
        reprate,
        breaks = seq(0, 1.0, by = 0.05),
        include.lowest = TRUE
      )
    ) |>
    dplyr::group_by(proxy_level, reprate_bin) |>
    dplyr::summarise(
      mae = mean(abs(error), na.rm = TRUE),
      n = dplyr::n(),
      .groups = "drop"
    )

    mae_reprate$reprate_bin <- factor(
      mae_reprate$reprate_bin,
      levels = levels(mae_reprate$reprate_bin),
      ordered = TRUE
    )

    plots$mae_by_reprate <- ggplot2::ggplot(
      mae_reprate,
      ggplot2::aes(reprate_bin, mae, colour = proxy_level, group = proxy_level)
    ) +
      ggplot2::geom_line(size = 1.2) +
      ggplot2::geom_point(size = 2) +
      ggplot2::scale_color_brewer(palette = "Spectral") +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = "MAE by Reporting Rate",
      subtitle = "Proxy TPR minus actual TPR",
      y = "\n\nMAE",
      x = "Reporting rate breaks\n\n",
      color = "Proxy Level"
    ) +
    ggplot2::theme(
      plot.margin = ggplot2::margin(10, 10, 10, 10),
      panel.border = ggplot2::element_rect(
        colour = "black",
        fill = NA,
        linewidth = 1
      )
    )

    cli::cli_alert_success("Diagnostic plots ready.")
  }

  list(
    metrics = metrics,
    validation_data = df_valid,
    plots = plots
  )
}
