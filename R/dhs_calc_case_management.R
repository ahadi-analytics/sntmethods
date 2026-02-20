#' Calculate Full Case Management Cascade from DHS Data
#'
#' Produces the complete WMR case management cascade by calling individual
#' indicator functions and combining results into a single output.
#'
#' The cascade tracks febrile children through 5 steps:
#' Fever -> Sought care (CSB) -> Tested (malaria_dx) -> Any antimalarial -> ACT
#'
#' @param dhs_kr DHS children's recode (KR) dataset (data.frame or tibble).
#' @param survey_vars Named list mapping DHS variable names. Required keys:
#'   \itemize{
#'     \item `cluster`: Cluster/PSU ID (default: "v021")
#'     \item `weight`: Survey weight (default: "v005")
#'     \item `stratum`: Stratum variable (default: "v022")
#'     \item `age`: Child's age in months (default: "hw1")
#'     \item `fever`: Had fever in last 2 weeks (default: "h22")
#'     \item `alive`: Child survival status (default: "b5")
#'     \item `malaria_dx`: Blood taken for malaria test (default: "h47")
#'     \item `act`: Received ACT treatment (default: "ml13e")
#'     \item `test`: Filter for act_tested denominator (default: "ml13a")
#'   }
#' @param csb_classification Data frame specifying h32 variable to CSB category
#'   mapping (passed to calc_csb_dhs_core). If NULL, uses WMR default.
#' @param region_var Optional column name in `dhs_kr` to use as grouping
#'   variable (e.g., "v024" for region).
#' @param steps Character vector of cascade steps to include. Default includes
#'   all steps. Available: "fever", "sought_care", "tested", "any_antimalarial",
#'   "received_act".
#'
#' @return List with:
#'   \itemize{
#'     \item `cascade`: Long-format tibble with one row per step (per group),
#'       columns: step, indicator, estimate, low, upp, n_eligible, n_positive
#'     \item `data`: Wide-format tibble with all dhs_* columns merged
#'     \item `dict`: Data dictionary from sntutils::build_dictionary()
#'     \item `metadata`: List with survey metadata including cascade info
#'   }
#'
#' @details
#' Each step uses its own core function internally:
#' \itemize{
#'   \item Step 0 (fever): calc_fever_dhs_core() - denominator: all alive U5
#'   \item Step 1 (sought_care): calc_csb_dhs_core() - denominator: febrile U5
#'   \item Step 2 (tested): calc_malaria_dx_dhs_core() - denominator: febrile U5
#'   \item Step 3 (any_antimalarial): calc_antimalarial_dhs_core() - denominator: febrile U5
#'   \item Step 4 (received_act): calc_act_dhs() - denominator: febrile U5
#' }
#'
#' Steps 2-4 are reported as proportions of FEBRILE children, consistent
#' with WMR methodology. Step 0 (fever) uses all alive U5 as denominator.
#'
#' If a step's required variables are missing from the data, that step is
#' skipped with a warning rather than failing the entire cascade.
#'
#' @examples
#' \dontrun{
#' cascade <- calc_case_management_dhs(
#'   dhs_kr = kr_data,
#'   region_var = "v024"
#' )
#'
#' # Long-format cascade table
#' cascade$cascade
#'
#' # Wide-format data with all indicators
#' cascade$data
#' }
#'
#' @seealso [calc_fever_dhs_core()], [calc_csb_dhs_core()],
#'   [calc_malaria_dx_dhs_core()], [calc_antimalarial_dhs_core()],
#'   [calc_act_dhs()]
#' @export
calc_case_management_dhs <- function(
  dhs_kr,
  survey_vars = list(
    cluster = "v021",
    weight = "v005",
    stratum = "v022",
    age = "hw1",
    fever = "h22",
    alive = "b5",
    malaria_dx = "h47",
    act = "ml13e",
    test = "ml13a"
  ),
  csb_classification = NULL,
  region_var = NULL,
  steps = c("fever", "sought_care", "tested", "any_antimalarial", "received_act")
) {
  # ---- 1. Input validation ----

  if (!is.data.frame(dhs_kr)) {
    cli::cli_abort("`dhs_kr` must be a data.frame or tibble.")
  }
  if (nrow(dhs_kr) == 0) {
    cli::cli_abort("`dhs_kr` is empty.")
  }

  valid_steps <- c("fever", "sought_care", "tested", "any_antimalarial", "received_act")
  invalid_steps <- setdiff(steps, valid_steps)
  if (length(invalid_steps) > 0) {
    cli::cli_abort(c(
      "Invalid cascade steps: {.val {invalid_steps}}",
      "i" = "Valid steps: {.val {valid_steps}}"
    ))
  }

  cli::cli_alert_info("Calculating case management cascade ({length(steps)} steps)")

  cascade_rows <- list()
  wide_results <- list()

  # ---- 2. Step 0: Fever prevalence ----

  if ("fever" %in% steps) {
    fever_result <- tryCatch({
      cli::cli_alert_info("Step 0: Fever prevalence")
      fever_vars <- survey_vars[c("cluster", "weight", "stratum", "age", "fever", "alive")]
      res <- calc_fever_dhs_core(
        dhs_kr = dhs_kr,
        survey_vars = fever_vars,
        region_var = region_var
      )

      cascade_rows[["fever"]] <- .build_cascade_row(
        step = 0L,
        indicator = "fever",
        result = res,
        est_col = "dhs_fever",
        low_col = "dhs_fever_low",
        upp_col = "dhs_fever_upp",
        n_eligible_col = "dhs_n_children",
        n_positive_col = "dhs_n_fever",
        region_var = region_var
      )

      wide_results[["fever"]] <- res
      cli::cli_alert_success("Step 0: Fever - done")
      TRUE
    }, error = function(e) {
      cli::cli_alert_warning("Step 0 (fever) skipped: {e$message}")
      FALSE
    })
  }

  # ---- 3. Step 1: Sought care (CSB) ----

  if ("sought_care" %in% steps) {
    csb_result <- tryCatch({
      cli::cli_alert_info("Step 1: Care-seeking behavior (CSB)")
      csb_vars <- survey_vars[c("cluster", "weight", "stratum", "age", "fever", "alive")]
      res <- calc_csb_dhs_core(
        dhs_kr = dhs_kr,
        survey_vars = csb_vars,
        csb_classification = csb_classification,
        region_var = region_var
      )

      cascade_rows[["sought_care"]] <- .build_cascade_row(
        step = 1L,
        indicator = "sought_care",
        result = res,
        est_col = "dhs_csb_any",
        low_col = "dhs_csb_any_low",
        upp_col = "dhs_csb_any_upp",
        n_eligible_col = "dhs_n_fever",
        n_positive_col = NULL,
        region_var = region_var
      )

      wide_results[["csb"]] <- res
      cli::cli_alert_success("Step 1: CSB - done")
      TRUE
    }, error = function(e) {
      cli::cli_alert_warning("Step 1 (sought_care) skipped: {e$message}")
      FALSE
    })
  }

  # ---- 4. Step 2: Tested (malaria diagnosis) ----

  if ("tested" %in% steps) {
    dx_result <- tryCatch({
      cli::cli_alert_info("Step 2: Malaria diagnostic testing")
      dx_vars <- survey_vars[c("cluster", "weight", "stratum", "age", "fever")]
      dx_vars$malaria_dx <- survey_vars$malaria_dx
      res <- calc_malaria_dx_dhs_core(
        dhs_kr = dhs_kr,
        survey_vars = dx_vars,
        region_var = region_var
      )

      cascade_rows[["tested"]] <- .build_cascade_row(
        step = 2L,
        indicator = "tested",
        result = res,
        est_col = "dhs_malaria_dx",
        low_col = "dhs_malaria_dx_low",
        upp_col = "dhs_malaria_dx_upp",
        n_eligible_col = "dhs_n_febrile",
        n_positive_col = "dhs_n_tested",
        region_var = region_var
      )

      wide_results[["malaria_dx"]] <- res
      cli::cli_alert_success("Step 2: Malaria Dx - done")
      TRUE
    }, error = function(e) {
      cli::cli_alert_warning("Step 2 (tested) skipped: {e$message}")
      FALSE
    })
  }

  # ---- 5. Step 3: Any antimalarial ----

  if ("any_antimalarial" %in% steps) {
    am_result <- tryCatch({
      cli::cli_alert_info("Step 3: Any antimalarial treatment")
      am_vars <- survey_vars[c("cluster", "weight", "stratum", "age", "fever")]
      res <- calc_antimalarial_dhs_core(
        dhs_kr = dhs_kr,
        survey_vars = am_vars,
        region_var = region_var
      )

      cascade_rows[["any_antimalarial"]] <- .build_cascade_row(
        step = 3L,
        indicator = "any_antimalarial",
        result = res,
        est_col = "dhs_antimalarial",
        low_col = "dhs_antimalarial_low",
        upp_col = "dhs_antimalarial_upp",
        n_eligible_col = "dhs_n_febrile",
        n_positive_col = "dhs_n_antimalarial",
        region_var = region_var
      )

      wide_results[["antimalarial"]] <- res
      cli::cli_alert_success("Step 3: Antimalarial - done")
      TRUE
    }, error = function(e) {
      cli::cli_alert_warning("Step 3 (any_antimalarial) skipped: {e$message}")
      FALSE
    })
  }

  # ---- 6. Step 4: Received ACT ----

  if ("received_act" %in% steps) {
    act_result <- tryCatch({
      cli::cli_alert_info("Step 4: ACT treatment")
      act_vars <- survey_vars[c("cluster", "weight", "stratum", "age", "fever")]
      act_vars$act <- survey_vars$act
      act_vars$test <- survey_vars$test
      res <- calc_act_dhs(
        dhs_kr = dhs_kr,
        survey_vars = act_vars,
        region_var = region_var
      )

      cascade_rows[["received_act"]] <- .build_cascade_row(
        step = 4L,
        indicator = "received_act",
        result = res,
        est_col = "dhs_act",
        low_col = "dhs_act_low",
        upp_col = "dhs_act_upp",
        n_eligible_col = "dhs_n_fever",
        n_positive_col = "dhs_n_act",
        region_var = region_var
      )

      wide_results[["act"]] <- res
      cli::cli_alert_success("Step 4: ACT - done")
      TRUE
    }, error = function(e) {
      cli::cli_alert_warning("Step 4 (received_act) skipped: {e$message}")
      FALSE
    })
  }

  # ---- 7. Build cascade table ----

  if (length(cascade_rows) == 0) {
    cli::cli_abort("No cascade steps could be computed.")
  }

  cascade <- dplyr::bind_rows(cascade_rows) |>
    dplyr::arrange(step)

  # ---- 8. Build wide-format summary ----

  wide_data <- .merge_wide_results(wide_results, region_var)

  # ---- 9. Build metadata ----

  metadata <- list(
    analysis_type = "Case Management Cascade",
    methodology = "WHO World Malaria Report (WMR)",
    steps_computed = names(cascade_rows),
    n_steps = length(cascade_rows),
    age_group = "0-59 months",
    processed_date = Sys.Date()
  )

  if ("v000" %in% names(dhs_kr)) {
    metadata$country_code <- unique(dhs_kr$v000)[1]
  }
  if ("v007" %in% names(dhs_kr)) {
    metadata$survey_year <- unique(dhs_kr$v007)[1]
  }

  cli::cli_alert_success(
    "Case management cascade complete: {length(cascade_rows)} of {length(steps)} steps computed"
  )

  list(
    cascade = tibble::as_tibble(cascade),
    data = wide_data,
    dict = sntutils::build_dictionary(wide_data),
    metadata = metadata
  )
}


#' Build a single cascade row from indicator results
#'
#' @param step Integer step number.
#' @param indicator Character name of the indicator.
#' @param result Tibble with indicator results.
#' @param est_col Column name for the estimate.
#' @param low_col Column name for the lower CI.
#' @param upp_col Column name for the upper CI.
#' @param n_eligible_col Column name for denominator count.
#' @param n_positive_col Column name for numerator count (can be NULL).
#' @param region_var Region variable name (or NULL).
#'
#' @return Tibble with cascade row(s).
#' @noRd
.build_cascade_row <- function(
  step, indicator, result,
  est_col, low_col, upp_col,
  n_eligible_col, n_positive_col,
  region_var = NULL
) {
  rows <- tibble::tibble(
    step = step,
    indicator = indicator,
    estimate = result[[est_col]],
    low = result[[low_col]],
    upp = result[[upp_col]],
    n_eligible = result[[n_eligible_col]]
  )

  if (!is.null(n_positive_col) && n_positive_col %in% names(result)) {
    rows$n_positive <- result[[n_positive_col]]
  } else {
    rows$n_positive <- as.integer(round(rows$estimate * rows$n_eligible))
  }

  # Add region if grouped
  if (!is.null(region_var) && region_var %in% names(result)) {
    rows <- dplyr::bind_cols(
      result[, region_var, drop = FALSE],
      rows
    )
  }

  rows
}


#' Merge wide-format results from individual cascade steps
#'
#' @param wide_results Named list of tibbles from each step.
#' @param region_var Region variable name (or NULL).
#'
#' @return Single merged tibble.
#' @noRd
.merge_wide_results <- function(wide_results, region_var = NULL) {
  if (length(wide_results) == 0) {
    return(tibble::tibble())
  }

  # Start with the first result
  merged <- wide_results[[1]]

  if (length(wide_results) > 1) {
    for (i in 2:length(wide_results)) {
      next_result <- wide_results[[i]]

      if (!is.null(region_var) && region_var %in% names(merged) &&
          region_var %in% names(next_result)) {
        # Join on region_var, keeping only new columns
        existing_cols <- names(merged)
        new_cols <- setdiff(names(next_result), existing_cols)
        new_cols <- c(region_var, new_cols)
        merged <- merged |>
          dplyr::left_join(
            next_result[, new_cols, drop = FALSE],
            by = region_var
          )
      } else {
        # National level - just bind columns (same single row)
        new_cols <- setdiff(names(next_result), names(merged))
        if (length(new_cols) > 0) {
          merged <- dplyr::bind_cols(
            merged,
            next_result[, new_cols, drop = FALSE]
          )
        }
      }
    }
  }

  tibble::as_tibble(merged)
}
