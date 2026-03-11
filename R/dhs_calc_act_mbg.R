#' Prepare ACT Treatment Data for MBG Analysis
#'
#' Prepares cluster-level ACT (Artemisinin-based Combination Therapy) treatment
#' data for MBG analysis. Calculates counts of febrile children under 5 who
#' received ACT treatment.
#'
#' @details
#' Methodology: \url{https://github.com/ahadi-analytics/sntmethods/blob/master/inst/methods/act_dhs.yml}
#'
#' @param dhs_kr DHS Children's Recode (KR) dataset.
#' @param gps_data DHS GPS dataset with cluster coordinates.
#' @param dhs_pr Optional DHS Person Recode (PR) dataset. Required for
#'   "febrile_rdt_pos" and "febrile_rdt_pos_act" indicators (provides hml35).
#' @param indicators Character vector of indicators to calculate:
#'   \itemize{
#'     \item "act": Received ACT among febrile children under 5
#'     \item "act_public": Received ACT among febrile U5 who sought public care
#'     \item "act_tested": Received ACT among children who tested positive
#'       (RDT or microscopy)
#'     \item "febrile_rdt_pos": RDT positivity rate among febrile U5 children
#'       (requires dhs_pr)
#'     \item "febrile_rdt_pos_act": ACT coverage among febrile RDT-positive
#'       children (requires dhs_pr)
#'   }
#'   Default: c("act", "act_tested").
#' @param survey_vars Named list mapping DHS variable names:
#'   \itemize{
#'     \item `cluster`: Cluster ID (default: "v001")
#'     \item `age`: Child's age in months (default: "hw1")
#'     \item `fever`: Fever in last 2 weeks (default: "h22")
#'     \item `act`: Received ACT (default: "ml13e")
#'     \item `test`: Filter variable for act_tested denominator (default: "ml13a").
#'       NOTE: ml13a is chloroquine in standard DHS; verify meaning per survey.
#'   }
#' @param gps_vars Named list for GPS variable mapping.
#'
#' @return A named list of data.tables (one per indicator), each with columns:
#'   \itemize{
#'     \item cluster_id: Cluster identifier
#'     \item indicator: Numerator count
#'     \item samplesize: Denominator count
#'     \item x: Longitude
#'     \item y: Latitude
#'   }
#'
#' @examples
#' \dontrun{
#' act_mbg <- calc_act_mbg(
#'   dhs_kr = kr_data,
#'   gps_data = gps_data,
#'   indicators = c("act", "act_tested")
#' )
#' }
#'
#' @seealso [calc_act_dhs()] for survey-weighted estimates,
#'   [calc_csb_mbg()] for care-seeking behavior
#' @export
calc_act_mbg <- function(
  dhs_kr,
  gps_data,
  dhs_pr = NULL,
  indicators = c("act", "act_tested"),
  survey_vars = list(
    cluster = "v001",
    age = "hw1",
    fever = "h22",
    alive = "b5",
    act = "ml13e",
    test = "ml13a"
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

  valid_indicators <- c("act", "act_public", "act_among_am", "act_tested", "febrile_rdt_pos", "febrile_rdt_pos_act")
  invalid <- setdiff(indicators, valid_indicators)
  if (length(invalid) > 0) {
    cli::cli_abort("Invalid indicators: {.val {invalid}}")
  }

  pr_required <- intersect(indicators, c("febrile_rdt_pos", "febrile_rdt_pos_act"))
  if (length(pr_required) > 0 && is.null(dhs_pr)) {
    cli::cli_alert_warning(
      "Indicators {.val {pr_required}} require `dhs_pr` - these will be skipped"
    )
  }

  # ---- Prepare data using shared helpers ----

  gps_clean <- .prepare_gps_data(gps_data, gps_vars)

  kr_fever <- tryCatch(
    .prepare_act_data(
      dhs_kr = dhs_kr,
      survey_vars = survey_vars,
      include_survey_vars = FALSE
    ),
    error = function(e) {
      cli::cli_alert_warning(conditionMessage(e))
      return(NULL)
    }
  )

  if (is.null(kr_fever)) return(list())

  # Check if ACT data is all NA
  if (all(is.na(kr_fever$received_act))) {
    cli::cli_alert_warning(
      "ACT variable {.var {survey_vars$act}} is all NA - no ACT data available"
    )
    return(list())
  }

  # ---- Calculate cluster-level indicators ----

  results <- list()
  has_test_var <- survey_vars$test %in% names(dhs_kr)

  if ("act" %in% indicators) {
    act_data <- kr_fever |>
      dplyr::filter(!is.na(received_act)) |>
      dplyr::mutate(act_binary = as.integer(received_act == 1))

    dt <- .aggregate_to_mbg_clusters(
      individual_data = act_data,
      indicator_col = "act_binary",
      gps_clean = gps_clean,
      result_name = "act"
    )

    if (!is.null(dt)) {
      results[["act"]] <- dt
    }
  }

  if ("act_public" %in% indicators) {
    # Apply CSB classification to identify public care seekers
    kr_fever_csb <- tryCatch(
      .classify_csb_from_h32(kr_fever),
      error = function(e) {
        cli::cli_alert_warning(
          "Cannot compute act_public: {conditionMessage(e)}"
        )
        NULL
      }
    )

    if (!is.null(kr_fever_csb)) {
      act_public_data <- kr_fever_csb |>
        dplyr::filter(csb_public == 1, !is.na(received_act)) |>
        dplyr::mutate(act_binary = as.integer(received_act == 1))

      if (nrow(act_public_data) > 0) {
        dt <- .aggregate_to_mbg_clusters(
          individual_data = act_public_data,
          indicator_col = "act_binary",
          gps_clean = gps_clean,
          result_name = "act_public"
        )

        if (!is.null(dt)) {
          results[["act_public"]] <- dt
        }
      } else {
        cli::cli_alert_warning(
          "No febrile children who sought public care - skipping act_public"
        )
      }
    }
  }

  if ("act_among_am" %in% indicators) {
    # Enrich with CSB + antimalarial for act_among_am indicator
    kr_fever_enriched <- tryCatch(
      .classify_csb_from_h32(kr_fever),
      error = function(e) {
        cli::cli_alert_warning(
          "Cannot compute act_among_am (CSB): {conditionMessage(e)}"
        )
        NULL
      }
    )

    if (!is.null(kr_fever_enriched)) {
      # Zap labels on the raw dataset for safe comparisons
      dhs_kr_zapped <- dhs_kr |>
        dplyr::mutate(dplyr::across(dplyr::everything(), haven::zap_labels)) |>
        dplyr::mutate(dplyr::across(dplyr::everything(), as.vector))

      # Add antimalarial composite.
      # Antimalarial series must align with the ACT variable used by
      # .prepare_act_data(). If ACT uses h37e, use h37 series.
      ml13_vars <- grep("^ml13[a-z]+$", names(dhs_kr_zapped), value = TRUE)
      h37_vars_am <- grep("^h37[a-z]+$", names(dhs_kr_zapped), value = TRUE)

      # Read which ACT variable(s) .prepare_act_data() actually resolved
      act_vars_used <- attr(kr_fever_enriched, "act_vars_used") %||%
        attr(kr_fever_enriched, "act_var_used") %||% (survey_vars$act %||% "ml13e")
      act_used_h37 <- any(grepl("^h37", act_vars_used))

      if (act_used_h37 && length(h37_vars_am) > 0) {
        drug_series <- h37_vars_am
        cli::cli_alert_info(
          "Antimalarial composite using h37 series (aligned with ACT h37e fallback)"
        )
      } else if (length(ml13_vars) > 0) {
        ml13_has_data <- any(sapply(ml13_vars, function(v) {
          any(dhs_kr_zapped[[v]] == 1, na.rm = TRUE)
        }))
        if (ml13_has_data) {
          drug_series <- ml13_vars
        } else if (length(h37_vars_am) > 0 && any(sapply(h37_vars_am, function(v) {
          any(dhs_kr_zapped[[v]] == 1, na.rm = TRUE)
        }))) {
          cli::cli_alert_info(
            "ml13 antimalarial variables have no positive values; using h37 series"
          )
          drug_series <- h37_vars_am
        } else {
          drug_series <- ml13_vars
        }
      } else {
        drug_series <- h37_vars_am
      }

      if (length(drug_series) > 0) {
        # Build febrile index matching .prepare_act_data() filtering (on zapped data)
        has_alive_var <- !is.null(survey_vars$alive) &&
          survey_vars$alive %in% names(dhs_kr_zapped)
        febrile_cond <- dhs_kr_zapped[[survey_vars$fever]] == 1 &
          dhs_kr_zapped[[survey_vars$age]] >= 0 &
          dhs_kr_zapped[[survey_vars$age]] <= 59
        if (has_alive_var) {
          febrile_cond <- febrile_cond & dhs_kr_zapped[[survey_vars$alive]] == 1
        }
        febrile_idx <- which(febrile_cond)

        for (dvar in drug_series) {
          kr_fever_enriched[[dvar]] <- dhs_kr_zapped[[dvar]][febrile_idx]
          kr_fever_enriched[[dvar]][!kr_fever_enriched[[dvar]] %in% c(0, 1)] <- NA
        }
        drug_matrix <- as.matrix(kr_fever_enriched[, drug_series, drop = FALSE])
        kr_fever_enriched$received_antimalarial <- apply(
          drug_matrix, 1, function(row) {
            if (all(is.na(row))) return(NA_real_)
            if (any(row == 1, na.rm = TRUE)) return(1)
            return(0)
          }
        )

        act_am_data <- kr_fever_enriched |>
          dplyr::filter(
            csb_any == 1,
            received_antimalarial == 1,
            !is.na(received_act)
          ) |>
          dplyr::mutate(act_binary = as.integer(received_act == 1))

        if (nrow(act_am_data) > 0) {
          dt <- .aggregate_to_mbg_clusters(
            individual_data = act_am_data,
            indicator_col = "act_binary",
            gps_clean = gps_clean,
            result_name = "act_among_am"
          )
          if (!is.null(dt)) results[["act_among_am"]] <- dt
        } else {
          cli::cli_alert_warning(
            "No antimalarial recipients who sought care - skipping act_among_am"
          )
        }
      } else {
        cli::cli_alert_warning(
          "No antimalarial variables found - skipping act_among_am"
        )
      }
    }
  }

  if ("act_tested" %in% indicators) {
    if (!has_test_var) {
      cli::cli_alert_warning(
        "Test variable {.var {survey_vars$test}} not found - skipping act_tested"
      )
    } else if (all(is.na(kr_fever$test_positive))) {
      cli::cli_alert_warning(
        "Test variable {.var {survey_vars$test}} is all NA - skipping act_tested"
      )
    } else {
      tested_data <- kr_fever |>
        dplyr::filter(test_positive == 1, !is.na(received_act)) |>
        dplyr::mutate(act_binary = as.integer(received_act == 1))

      dt <- .aggregate_to_mbg_clusters(
        individual_data = tested_data,
        indicator_col = "act_binary",
        gps_clean = gps_clean,
        result_name = "act_tested"
      )

      if (!is.null(dt)) {
        results[["act_tested"]] <- dt
      }
    }
  }

  # ---- Febrile RDT indicators (require dhs_pr) ----

  if (!is.null(dhs_pr) && length(pr_required) > 0) {
    kr_merged <- .merge_kr_pr_febrile(kr_fever = kr_fever, dhs_pr = dhs_pr)

    if (!is.null(kr_merged)) {
      if ("febrile_rdt_pos" %in% indicators) {
        dt <- .aggregate_to_mbg_clusters(
          individual_data = kr_merged,
          indicator_col   = "has_rdt_pos",
          gps_clean       = gps_clean,
          result_name     = "febrile_rdt_pos"
        )
        if (!is.null(dt)) results[["febrile_rdt_pos"]] <- dt
      }

      if ("febrile_rdt_pos_act" %in% indicators) {
        rdt_pos_data <- kr_merged |>
          dplyr::filter(has_rdt_pos == 1, !is.na(has_act))

        if (nrow(rdt_pos_data) > 0) {
          dt <- .aggregate_to_mbg_clusters(
            individual_data = rdt_pos_data,
            indicator_col   = "has_act",
            gps_clean       = gps_clean,
            result_name     = "febrile_rdt_pos_act"
          )
          if (!is.null(dt)) results[["febrile_rdt_pos_act"]] <- dt
        } else {
          cli::cli_alert_warning(
            "No RDT-positive febrile children found for febrile_rdt_pos_act"
          )
        }
      }
    }
  }

  if (length(results) == 0) {
    cli::cli_alert_warning("No valid ACT MBG data could be prepared")
  }

  results
}


#' Prepare Single ACT Indicator for MBG
#'
#' Convenience wrapper around [calc_act_mbg()] to prepare a single ACT
#' indicator for MBG analysis.
#'
#' @inheritParams calc_act_mbg
#' @param indicator Single indicator name. Default: "act".
#'
#' @return A data.table with columns: cluster_id, indicator, samplesize, x, y
#' @export
prep_act_mbg <- function(
  dhs_kr,
  gps_data,
  indicator = "act",
  survey_vars = list(
    cluster = "v001",
    age = "hw1",
    fever = "h22",
    alive = "b5",
    act = "ml13e",
    test = "ml13a"
  ),
  gps_vars = list(
    cluster = "DHSCLUST",
    lat = "LATNUM",
    lon = "LONGNUM"
  )
) {
  result <- calc_act_mbg(
    dhs_kr = dhs_kr,
    gps_data = gps_data,
    indicators = indicator,
    survey_vars = survey_vars,
    gps_vars = gps_vars
  )

  if (length(result) == 0) {
    cli::cli_abort("No data returned for indicator {.val {indicator}}")
  }

  result[[1]]
}
