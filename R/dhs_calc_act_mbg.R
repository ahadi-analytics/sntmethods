#' Prepare ACT and Antimalarial Data for MBG Analysis
#'
#' Prepares cluster-level ACT (Artemisinin-based Combination
#' Therapy), antimalarial treatment, and malaria diagnostic
#' data for MBG analysis. Uses a dictionary-driven approach
#' matching the indicator codes from \code{\link{calc_act_dhs}}.
#'
#' @details
#' All dictionary-based indicators share the same data
#' preparation pipeline:
#' \enumerate{
#'   \item Filter to febrile U5 children (via
#'     \code{.prepare_act_data()})
#'   \item Classify care-seeking sectors (via
#'     \code{.classify_csb_from_h32()})
#'   \item Build antimalarial composite from ml13/h37 series
#'   \item Build malaria diagnostic flag from ml1/h47
#'   \item Apply per-indicator filters and aggregate to
#'     cluster-level counts
#' }
#'
#' The dictionary includes three indicator families:
#' \itemize{
#'   \item \strong{ACT} (\code{act_*}): ACT receipt among
#'     febrile U5, with sector and AM filters
#'   \item \strong{Antimalarial} (\code{antimal_*}):
#'     Antimalarial receipt among febrile U5, with sector
#'     filters
#'   \item \strong{Malaria diagnostic} (\code{mal_dx_*}):
#'     Malaria diagnostic test (ml1/h47) among AM recipients,
#'     with sector filters
#' }
#'
#' @param dhs_kr DHS Children's Recode (KR) dataset.
#' @param gps_data DHS GPS dataset with cluster coordinates.
#' @param dhs_pr Optional DHS Person Recode (PR) dataset.
#'   Required for \code{"febrile_rdt_pos"} and
#'   \code{"febrile_rdt_pos_act"} indicators (provides hml35).
#' @param indicators Character vector of indicators to
#'   calculate. See \code{.act_mbg_dictionary()} for the full
#'   list of standardized indicator codes. Legacy names
#'   \code{"act_public"} and \code{"act_among_am"} are also
#'   accepted. Special indicators:
#'   \itemize{
#'     \item \code{"act_tested"}: ACT among test-positive
#'     \item \code{"febrile_rdt_pos"}: RDT positivity
#'       (requires dhs_pr)
#'     \item \code{"febrile_rdt_pos_act"}: ACT among
#'       RDT-positive (requires dhs_pr)
#'   }
#'   Default: \code{c("act", "act_tested")}.
#' @param survey_vars Named list mapping DHS variable names:
#'   \itemize{
#'     \item \code{cluster}: Cluster ID (default: "v001")
#'     \item \code{age}: Child's age in months
#'       (default: "hw1")
#'     \item \code{fever}: Fever in last 2 weeks
#'       (default: "h22")
#'     \item \code{alive}: Child is alive (default: "b5")
#'     \item \code{act}: ACT variable (default: "ml13e")
#'     \item \code{test}: Diagnostic test variable
#'       (default: "ml13a")
#'   }
#' @param gps_vars Named list for GPS variable mapping.
#'
#' @return A named list of data.tables (one per indicator),
#'   each with columns:
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
#'   indicators = c("act_pub", "act_trained", "antimal_chw",
#'                   "mal_dx_am", "mal_dx_pub_am")
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
    cli::cli_abort(
      "`dhs_kr` must be a data.frame or tibble"
    )
  }
  if (!is.data.frame(gps_data)) {
    cli::cli_abort(
      "`gps_data` must be a data.frame or tibble"
    )
  }

  # Resolve legacy aliases
  legacy_map <- list(
    act_public = "act_public",
    act_among_am = "act_any_tx"
  )
  indicators <- vapply(indicators, function(ind) {
    if (ind %in% names(legacy_map)) {
      legacy_map[[ind]]
    } else {
      ind
    }
  }, character(1), USE.NAMES = FALSE)

  # Build valid indicator set from dictionary + specials
  dict <- .act_mbg_dictionary()
  dict_names <- vapply(
    dict, `[[`, character(1), "name"
  )
  special_indicators <- c(
    "act_public", "act_tested",
    "febrile_rdt_pos", "febrile_rdt_pos_act"
  )
  valid_indicators <- unique(c(
    dict_names, special_indicators
  ))

  invalid <- setdiff(indicators, valid_indicators)
  if (length(invalid) > 0) {
    cli::cli_abort(
      "Invalid indicators: {.val {invalid}}"
    )
  }

  pr_required <- intersect(
    indicators,
    c("febrile_rdt_pos", "febrile_rdt_pos_act")
  )
  if (length(pr_required) > 0 && is.null(dhs_pr)) {
    cli::cli_alert_warning(
      "Indicators {.val {pr_required}} require \\
      `dhs_pr` - these will be skipped"
    )
  }

  # ---- Prepare base data ----

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

  if (all(is.na(kr_fever$received_act))) {
    cli::cli_alert_warning(
      "ACT variable {.var {survey_vars$act}} is \\
      all NA - no ACT data available"
    )
    return(list())
  }

  # ---- Determine which enrichments are needed ----

  # Which dictionary indicators were requested?
  dict_requested <- indicators[
    indicators %in% dict_names
  ]
  dict_specs <- dict[
    vapply(dict, function(d) {
      d$name %in% dict_requested
    }, logical(1))
  ]

  needs_csb <- any(vapply(dict_specs, function(d) {
    !is.null(d$csb_filter)
  }, logical(1))) ||
    "act_public" %in% indicators
  needs_am <- any(vapply(dict_specs, function(d) {
    isTRUE(d$am_filter)
  }, logical(1))) ||
    any(vapply(dict_specs, function(d) {
      identical(d$outcome, "received_antimalarial")
    }, logical(1)))

  # ---- Enrich with CSB flags if needed ----

  enriched <- kr_fever
  if (needs_csb) {
    # Detect CSB classification from haven labels on raw data (before zapping)
    # so CHW/pharmacy slots are correctly identified across DHS versions
    csb_class <- .detect_csb_from_labels(dhs_kr)
    if (nrow(csb_class) == 0) csb_class <- NULL
    enriched <- tryCatch(
      .classify_csb_from_h32(enriched, csb_classification = csb_class),
      error = function(e) {
        cli::cli_alert_warning(
          "CSB classification failed: \\
          {conditionMessage(e)}"
        )
        NULL
      }
    )
    if (is.null(enriched)) {
      # Fall back to unenriched data for non-CSB
      enriched <- kr_fever
      needs_csb <- FALSE
    }
  }

  # ---- Enrich with antimalarial composite if needed ----

  if (needs_am) {
    enriched <- .enrich_with_antimalarial(
      enriched, dhs_kr, survey_vars
    )
  }

  # ---- Enrich with malaria diagnostic if needed ----

  needs_dx <- any(vapply(dict_specs, function(s) {
    s$outcome == "had_test"
  }, logical(1)))
  if (needs_dx) {
    # Mirror DHS derivation: try ml1 first, then h47 fallback
    if ("ml1" %in% names(enriched)) {
      enriched$had_test <- as.integer(
        !is.na(enriched$ml1) & enriched$ml1 == 1
      )
      cli::cli_alert_info(
        "Enriched with malaria diagnostic (ml1): \\
        {sum(enriched$had_test == 1, na.rm = TRUE)} \\
        tested"
      )
    } else if ("h47" %in% names(enriched)) {
      enriched$had_test <- as.integer(
        !is.na(enriched$h47) & enriched$h47 == 1
      )
      cli::cli_alert_info(
        "Enriched with malaria diagnostic (h47): \\
        {sum(enriched$had_test == 1, na.rm = TRUE)} \\
        tested"
      )
    } else {
      cli::cli_alert_warning(
        "Neither ml1 nor h47 found \\
        {cli::symbol$em_dash} mal_dx indicators \\
        will be skipped"
      )
      enriched$had_test <- NA_integer_
    }
  }

  # ---- Dictionary-driven indicator loop ----

  results <- list()

  for (spec in dict_specs) {
    # Skip CSB-filtered indicators if CSB failed
    if (!is.null(spec$csb_filter) && !needs_csb) {
      cli::cli_alert_warning(
        "Skipping {.val {spec$name}}: \\
        CSB classification not available"
      )
      next
    }

    # Skip AM-dependent indicators if AM not built
    if (isTRUE(spec$am_filter) &&
        !"received_antimalarial" %in% names(enriched)) {
      cli::cli_alert_warning(
        "Skipping {.val {spec$name}}: \\
        antimalarial composite not available"
      )
      next
    }

    # Apply filters
    filtered <- enriched
    if (!is.null(spec$csb_filter)) {
      col <- spec$csb_filter
      if (!col %in% names(filtered)) next
      filtered <- filtered[
        !is.na(filtered[[col]]) &
          filtered[[col]] == 1, ,
        drop = FALSE
      ]
    }
    if (isTRUE(spec$am_filter)) {
      filtered <- filtered[
        !is.na(filtered$received_antimalarial) &
          filtered$received_antimalarial == 1, ,
        drop = FALSE
      ]
    }

    # Filter to non-NA outcome and build binary
    outcome_col <- spec$outcome
    if (!outcome_col %in% names(filtered)) next
    filtered <- filtered[
      !is.na(filtered[[outcome_col]]), ,
      drop = FALSE
    ]
    if (nrow(filtered) == 0) {
      cli::cli_alert_warning(
        "No data for {.val {spec$name}} — skipping"
      )
      next
    }
    filtered$.binary <- as.integer(
      filtered[[outcome_col]] == 1
    )

    dt <- .aggregate_to_mbg_clusters(
      individual_data = filtered,
      indicator_col = ".binary",
      gps_clean = gps_clean,
      result_name = spec$name
    )
    if (!is.null(dt)) {
      results[[spec$name]] <- dt
    }
  }

  # ---- Legacy act_public (no AM filter) ----

  if ("act_public" %in% indicators &&
      needs_csb &&
      !"act_public" %in% names(results)) {
    pub_data <- enriched[
      !is.na(enriched$csb_public) &
        enriched$csb_public == 1 &
        !is.na(enriched$received_act), ,
      drop = FALSE
    ]
    if (nrow(pub_data) > 0) {
      pub_data$.binary <- as.integer(
        pub_data$received_act == 1
      )
      dt <- .aggregate_to_mbg_clusters(
        individual_data = pub_data,
        indicator_col = ".binary",
        gps_clean = gps_clean,
        result_name = "act_public"
      )
      if (!is.null(dt)) {
        results[["act_public"]] <- dt
      }
    }
  }

  # ---- Special: act_tested ----

  has_test_var <- survey_vars$test %in% names(dhs_kr)
  if ("act_tested" %in% indicators) {
    if (!has_test_var) {
      cli::cli_alert_warning(
        "Test variable {.var {survey_vars$test}} \\
        not found - skipping act_tested"
      )
    } else if (all(is.na(kr_fever$test_positive))) {
      cli::cli_alert_warning(
        "Test variable {.var {survey_vars$test}} \\
        is all NA - skipping act_tested"
      )
    } else {
      tested_data <- kr_fever |>
        dplyr::filter(
          test_positive == 1,
          !is.na(received_act)
        ) |>
        dplyr::mutate(
          .binary = as.integer(received_act == 1)
        )

      dt <- .aggregate_to_mbg_clusters(
        individual_data = tested_data,
        indicator_col = ".binary",
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
    kr_merged <- .merge_kr_pr_febrile(
      kr_fever = kr_fever, dhs_pr = dhs_pr
    )

    if (!is.null(kr_merged)) {
      if ("febrile_rdt_pos" %in% indicators) {
        dt <- .aggregate_to_mbg_clusters(
          individual_data = kr_merged,
          indicator_col = "has_rdt_pos",
          gps_clean = gps_clean,
          result_name = "febrile_rdt_pos"
        )
        if (!is.null(dt)) {
          results[["febrile_rdt_pos"]] <- dt
        }
      }

      if ("febrile_rdt_pos_act" %in% indicators) {
        rdt_pos_data <- kr_merged |>
          dplyr::filter(
            has_rdt_pos == 1, !is.na(has_act)
          )

        if (nrow(rdt_pos_data) > 0) {
          dt <- .aggregate_to_mbg_clusters(
            individual_data = rdt_pos_data,
            indicator_col = "has_act",
            gps_clean = gps_clean,
            result_name = "febrile_rdt_pos_act"
          )
          if (!is.null(dt)) {
            results[["febrile_rdt_pos_act"]] <- dt
          }
        } else {
          cli::cli_alert_warning(
            "No RDT-positive febrile children \\
            for febrile_rdt_pos_act"
          )
        }
      }
    }
  }

  if (length(results) == 0) {
    cli::cli_alert_warning(
      "No valid ACT MBG data could be prepared"
    )
  }

  results
}


#' ACT/Antimalarial/Malaria Diagnostic MBG Indicator Dictionary
#'
#' Returns the full set of standardized indicator
#' specifications for cluster-level MBG output.
#' Each entry defines the outcome variable, optional
#' CSB filter column, and whether an antimalarial
#' receipt filter applies.
#'
#' Three indicator families:
#' \itemize{
#'   \item \strong{ACT} (13): \code{outcome = "received_act"}
#'   \item \strong{Antimalarial} (11):
#'     \code{outcome = "received_antimalarial"}
#'   \item \strong{Malaria diagnostic} (9):
#'     \code{outcome = "had_test"} (ml1/h47 == 1)
#' }
#'
#' @return List of named lists with fields:
#'   \code{name}, \code{outcome}, \code{csb_filter},
#'   \code{am_filter}.
#' @noRd
.act_mbg_dictionary <- function() {
  list(
    # -- ACT indicators (outcome = received_act) --
    list(
      name = "act",
      outcome = "received_act",
      csb_filter = NULL,
      am_filter = FALSE
    ),
    list(
      name = "act_care_seek",
      outcome = "received_act",
      csb_filter = "csb_any",
      am_filter = FALSE
    ),
    list(
      name = "act_antimal",
      outcome = "received_act",
      csb_filter = NULL,
      am_filter = TRUE
    ),
    list(
      name = "act_any_tx",
      outcome = "received_act",
      csb_filter = "csb_any",
      am_filter = TRUE
    ),
    list(
      name = "act_trained",
      outcome = "received_act",
      csb_filter = "csb_trained",
      am_filter = TRUE
    ),
    list(
      name = "act_pub",
      outcome = "received_act",
      csb_filter = "csb_public",
      am_filter = TRUE
    ),
    list(
      name = "act_pub_nochw",
      outcome = "received_act",
      csb_filter = "csb_public_nochw",
      am_filter = TRUE
    ),
    list(
      name = "act_chw",
      outcome = "received_act",
      csb_filter = "csb_chw",
      am_filter = TRUE
    ),
    list(
      name = "act_priv",
      outcome = "received_act",
      csb_filter = "csb_private",
      am_filter = TRUE
    ),
    list(
      name = "act_priv_formal",
      outcome = "received_act",
      csb_filter = "csb_private_formal_ind",
      am_filter = TRUE
    ),
    list(
      name = "act_priv_pharm",
      outcome = "received_act",
      csb_filter = "csb_pharmacy",
      am_filter = TRUE
    ),
    list(
      name = "act_priv_informal",
      outcome = "received_act",
      csb_filter = "csb_private_informal",
      am_filter = TRUE
    ),
    list(
      name = "act_priv_form_pha",
      outcome = "received_act",
      csb_filter = "csb_private_formal_pha",
      am_filter = TRUE
    ),

    # -- Antimalarial indicators (outcome = received_antimalarial) --
    list(
      name = "antimal",
      outcome = "received_antimalarial",
      csb_filter = NULL,
      am_filter = FALSE
    ),
    list(
      name = "antimal_any_tx",
      outcome = "received_antimalarial",
      csb_filter = "csb_any",
      am_filter = FALSE
    ),
    list(
      name = "antimal_trained",
      outcome = "received_antimalarial",
      csb_filter = "csb_trained",
      am_filter = FALSE
    ),
    list(
      name = "antimal_pub",
      outcome = "received_antimalarial",
      csb_filter = "csb_public",
      am_filter = FALSE
    ),
    list(
      name = "antimal_pub_nochw",
      outcome = "received_antimalarial",
      csb_filter = "csb_public_nochw",
      am_filter = FALSE
    ),
    list(
      name = "antimal_chw",
      outcome = "received_antimalarial",
      csb_filter = "csb_chw",
      am_filter = FALSE
    ),
    list(
      name = "antimal_priv",
      outcome = "received_antimalarial",
      csb_filter = "csb_private",
      am_filter = FALSE
    ),
    list(
      name = "antimal_formal",
      outcome = "received_antimalarial",
      csb_filter = "csb_private_formal_ind",
      am_filter = FALSE
    ),
    list(
      name = "antimal_pharm",
      outcome = "received_antimalarial",
      csb_filter = "csb_pharmacy",
      am_filter = FALSE
    ),
    list(
      name = "antimal_priv_informal",
      outcome = "received_antimalarial",
      csb_filter = "csb_private_informal",
      am_filter = FALSE
    ),
    list(
      name = "antimal_form_pharm",
      outcome = "received_antimalarial",
      csb_filter = "csb_private_formal_pha",
      am_filter = FALSE
    ),

    # -- Malaria diagnostic indicators (outcome = had_test) --
    # Malaria diagnostic among AM recipients, by care-seeking sector
    list(
      name = "mal_dx_am",
      outcome = "had_test",
      csb_filter = NULL,
      am_filter = TRUE
    ),
    list(
      name = "mal_dx_pub_am",
      outcome = "had_test",
      csb_filter = "csb_public",
      am_filter = TRUE
    ),
    list(
      name = "mal_dx_pub_nochw_am",
      outcome = "had_test",
      csb_filter = "csb_public_nochw",
      am_filter = TRUE
    ),
    list(
      name = "mal_dx_chw_am",
      outcome = "had_test",
      csb_filter = "csb_chw",
      am_filter = TRUE
    ),
    list(
      name = "mal_dx_priv_am",
      outcome = "had_test",
      csb_filter = "csb_private",
      am_filter = TRUE
    ),
    list(
      name = "mal_dx_priv_formal_am",
      outcome = "had_test",
      csb_filter = "csb_private_formal_ind",
      am_filter = TRUE
    ),
    list(
      name = "mal_dx_pharm_am",
      outcome = "had_test",
      csb_filter = "csb_pharmacy",
      am_filter = TRUE
    ),
    list(
      name = "mal_dx_priv_informal_am",
      outcome = "had_test",
      csb_filter = "csb_private_informal",
      am_filter = TRUE
    ),
    list(
      name = "mal_dx_priv_form_pha_am",
      outcome = "had_test",
      csb_filter = "csb_private_formal_pha",
      am_filter = TRUE
    )
  )
}


#' Enrich Febrile Data with Antimalarial Composite
#'
#' Builds a \code{received_antimalarial} column on the
#' enriched febrile dataset by detecting the antimalarial
#' drug series (ml13 or h37) and compositing across all
#' variables in the series.
#'
#' @param enriched Febrile U5 data (possibly with CSB flags).
#' @param dhs_kr Original KR dataset (for drug variables).
#' @param survey_vars Survey variable mapping.
#'
#' @return The \code{enriched} data frame with
#'   \code{received_antimalarial} column added.
#' @noRd
.enrich_with_antimalarial <- function(
  enriched, dhs_kr, survey_vars
) {
  # Zap labels on raw data for safe comparisons
  dhs_kr_zapped <- dhs_kr |>
    dplyr::mutate(dplyr::across(
      dplyr::everything(), haven::zap_labels
    )) |>
    dplyr::mutate(dplyr::across(
      dplyr::everything(), as.vector
    ))

  ml13_vars <- grep(
    "^ml13[a-z]+$",
    names(dhs_kr_zapped), value = TRUE
  )
  h37_vars_am <- grep(
    "^h37[a-z]+$",
    names(dhs_kr_zapped), value = TRUE
  )

  # Align with ACT variable series
  act_vars_used <- attr(
    enriched, "act_vars_used"
  ) %||%
    attr(enriched, "act_var_used") %||%
    (survey_vars$act %||% "ml13e")
  act_used_h37 <- any(grepl("^h37", act_vars_used))

  if (act_used_h37 && length(h37_vars_am) > 0) {
    drug_series <- h37_vars_am
    cli::cli_alert_info(
      "Antimalarial composite using h37 series \\
      (aligned with ACT h37 fallback)"
    )
  } else if (length(ml13_vars) > 0) {
    ml13_has_data <- any(sapply(
      ml13_vars,
      function(v) {
        any(dhs_kr_zapped[[v]] == 1, na.rm = TRUE)
      }
    ))
    if (ml13_has_data) {
      drug_series <- ml13_vars
    } else if (
      length(h37_vars_am) > 0 &&
      any(sapply(h37_vars_am, function(v) {
        any(dhs_kr_zapped[[v]] == 1, na.rm = TRUE)
      }))
    ) {
      cli::cli_alert_info(
        "ml13 antimalarial variables have no \\
        positive values; using h37 series"
      )
      drug_series <- h37_vars_am
    } else {
      drug_series <- ml13_vars
    }
  } else {
    drug_series <- h37_vars_am
  }

  if (length(drug_series) == 0) {
    cli::cli_alert_warning(
      "No antimalarial variables found — \\
      antimalarial indicators will be skipped"
    )
    return(enriched)
  }

  # Build febrile index matching .prepare_act_data()
  has_alive_var <- !is.null(survey_vars$alive) &&
    survey_vars$alive %in% names(dhs_kr_zapped)
  febrile_cond <-
    dhs_kr_zapped[[survey_vars$fever]] == 1 &
    dhs_kr_zapped[[survey_vars$age]] >= 0 &
    dhs_kr_zapped[[survey_vars$age]] <= 59
  if (has_alive_var) {
    febrile_cond <- febrile_cond &
      dhs_kr_zapped[[survey_vars$alive]] == 1
  }
  febrile_idx <- which(febrile_cond)

  # Copy drug variables into enriched data
  for (dvar in drug_series) {
    enriched[[dvar]] <-
      dhs_kr_zapped[[dvar]][febrile_idx]
    enriched[[dvar]][
      !enriched[[dvar]] %in% c(0, 1)
    ] <- NA
  }

  drug_matrix <- as.matrix(
    enriched[, drug_series, drop = FALSE]
  )
  enriched$received_antimalarial <- apply(
    drug_matrix, 1,
    function(row) {
      if (all(is.na(row))) return(NA_real_)
      if (any(row == 1, na.rm = TRUE)) return(1)
      return(0)
    }
  )

  cli::cli_alert_info(
    "Antimalarial composite built from \\
    {length(drug_series)} variables"
  )

  enriched
}


#' Prepare Single ACT Indicator for MBG
#'
#' Convenience wrapper around [calc_act_mbg()] to prepare
#' a single ACT indicator for MBG analysis.
#'
#' @inheritParams calc_act_mbg
#' @param indicator Single indicator name. Default: "act".
#'
#' @return A data.table with columns: cluster_id, indicator,
#'   samplesize, x, y
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
    cli::cli_abort(
      "No data returned for indicator \\
      {.val {indicator}}"
    )
  }

  result[[1]]
}
