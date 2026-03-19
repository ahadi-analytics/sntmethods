#' Detect ACT Variables from Haven Labels
#'
#' Scans ml13* and h37* variables in a DHS dataset for haven labels indicating
#' ACT (Artemisinin-based Combination Therapy). ACT is a drug CLASS -- multiple
#' variables may contain different ACT formulations (e.g., artemether-lumefantrine
#' in ml13f, artesunate-amodiaquine in ml13g). Returns ALL matching variables.
#'
#' Excludes artemisinin monotherapies (artesunate rectal/injection/IV) which
#' are NOT combination therapies.
#'
#' @param dhs_kr DHS dataset with haven_labelled columns.
#' @param default_vars Default ACT variable(s) to return if no label match found.
#'
#' @return Character vector of detected ACT variable names, or `default_vars`.
#' @noRd
.detect_act_vars <- function(dhs_kr, default_vars = "ml13e") {
  # Inclusion pattern: ACT combinations
  # - combin.*artemi / artemi.*combin: "Combination with artemisinin" (standardised)
  # - artemether.+lumef: artemether-lumefantrine (Coartem)
  # - artesunate.+amodiaq: artesunate-amodiaquine (ASAQ)
  # - dihydroartemis: DHA-piperaquine
  # - \bact\b: "ACT" as a word
  # - \bcta\b: French "Combinaison Therapeutique a base d'Artemisinine"
  # - coartem: brand name
  act_pattern <- paste0(
    "\\bact\\b|combin.*artemi|artemi.*combin|",
    "artemether.+lumef|artesunate.+amodiaq|dihydroartemis|",
    "coartem|\\bcta\\b"
  )
  # Exclusion pattern: artemisinin monotherapies (not combination therapy)
  exclude_pattern <- "rectal|injection|\\biv\\b|monotherapy"

  # Helper: scan a set of candidates for ACT labels
  .scan_labels <- function(candidates) {
    matched <- character(0)
    for (v in candidates) {
      lbl <- attr(dhs_kr[[v]], "label")
      if (!is.null(lbl) && is.character(lbl) && length(lbl) == 1 &&
          grepl(act_pattern, lbl, ignore.case = TRUE) &&
          !grepl(exclude_pattern, lbl, ignore.case = TRUE)) {
        matched <- c(matched, v)
      }
    }
    matched
  }

  # Search ml13 series first (newer surveys). Only fall back to h37 (older
  # surveys) if ml13 yields no matches. The two series are PARALLEL -- they
  # represent the same drug slots in different DHS coding systems and must
  # never be mixed into a single composite.
  ml13_candidates <- grep("^ml13[a-z]", names(dhs_kr), value = TRUE)
  act_vars <- .scan_labels(ml13_candidates)

  if (length(act_vars) == 0) {
    h37_candidates <- grep("^h37[a-z]", names(dhs_kr), value = TRUE)
    act_vars <- .scan_labels(h37_candidates)
  }

  if (length(act_vars) == 0) {
    cli::cli_alert_warning(
      "No ACT variables detected from labels; defaulting to {.var {default_vars}}"
    )
    return(default_vars)
  }

  if (length(act_vars) > 1) {
    cli::cli_alert_info(
      "Detected {length(act_vars)} ACT variables from labels: {paste(act_vars, collapse = ', ')}"
    )
  } else if (act_vars[1] != default_vars[1]) {
    lbl <- attr(dhs_kr[[act_vars[1]]], "label")
    cli::cli_alert_info(
      "Auto-detected ACT variable {.var {act_vars[1]}} from label: {.val {lbl}}"
    )
  }

  act_vars
}


#' Detect ACT Variable from Haven Labels (deprecated wrapper)
#'
#' @param dhs_kr DHS dataset with haven_labelled columns.
#' @param default_var Default ACT variable to return if no label match found.
#' @return Single variable name (first detected ACT variable).
#' @noRd
.detect_act_var_from_labels <- function(dhs_kr, default_var = "ml13e") {
  .detect_act_vars(dhs_kr, default_vars = default_var)[1]
}


#' Prepare ACT Data for Analysis
#'
#' Shared data cleaning and indicator computation for ACT functions.
#' Used by both calc_act_dhs() and calc_act_mbg().
#'
#' @param dhs_kr DHS Children's Recode (KR) dataset.
#' @param survey_vars Named list mapping DHS variable names.
#' @param include_survey_vars Logical. If TRUE, includes survey design columns.
#'
#' @return A data frame of febrile children with columns:
#'   cluster_id, age_months, received_act, test_positive, has_act.
#'   If include_survey_vars = TRUE, also: survey_weight, stratum_id.
#'   Attribute "act_var_used" records which variable was resolved as ACT.
#'
#' @noRd
.prepare_act_data <- function(
  dhs_kr,
  survey_vars,
  include_survey_vars = FALSE
) {
  if (!is.data.frame(dhs_kr)) {
    cli::cli_abort("`dhs_kr` must be a data.frame or tibble.")
  }
  if (nrow(dhs_kr) == 0) {
    cli::cli_abort("`dhs_kr` is empty.")
  }

  # Auto-detect age variable if specified one is missing
  # Fallback order: hw1 (anthropometry) -> hc1 (standard KR) -> b8 (current age)
  if (!survey_vars$age %in% names(dhs_kr)) {
    age_candidates <- c("hc1", "b8", "hw1")
    available_age <- intersect(age_candidates, names(dhs_kr))

    if (length(available_age) > 0) {
      old_age_var <- survey_vars$age
      survey_vars$age <- available_age[1]
      cli::cli_alert_info(
        "Age variable {.var {old_age_var}} not found; using {.var {survey_vars$age}} instead"
      )
    }
  }

  # Check required columns
  needed <- c(survey_vars$cluster, survey_vars$age, survey_vars$fever)
  if (include_survey_vars) {
    needed <- c(needed, survey_vars$weight, survey_vars$stratum)
  }
  missing_vars <- setdiff(needed, names(dhs_kr))
  if (length(missing_vars) > 0) {
    cli::cli_abort(c(
      "Required variables not found: {.var {missing_vars}}",
      "i" = "Check your survey_vars mapping"
    ))
  }

  # Detect ACT variables with multi-stage resolution:
  # 1. Label-based detection: find ALL ACT combination variables
  #    (handles surveys with multiple ACT formulations, e.g. Togo MIS 2017)
  # 2. Positive-value fallback: if no ACT vars have data, try h37 series
  # 3. Presence fallback: try h37e if ml13 vars are missing entirely
  act_input <- survey_vars$act

  # Stage 1: auto-detect from haven labels when using default mapping
  if (length(act_input) == 1 && act_input == "ml13e") {
    act_vars <- .detect_act_vars(dhs_kr, default_vars = act_input)
  } else {
    act_vars <- act_input
  }

  # Validate presence
  act_vars <- intersect(act_vars, names(dhs_kr))

  # Stage 2-3: fallback logic
  if (length(act_vars) == 0) {
    # Try h37 series
    h37_acts <- .detect_act_vars(dhs_kr, default_vars = "h37e")
    act_vars <- intersect(h37_acts, names(dhs_kr))
    if (length(act_vars) > 0) {
      cli::cli_alert_info(
        "ml13 ACT variables not found; using h37 series: {paste(act_vars, collapse = ', ')}"
      )
    } else {
      cli::cli_abort(
        "No ACT variables found in data (tried ml13* and h37*)"
      )
    }
  }

  # Check if any ACT var has positive values
  act_has_data <- any(sapply(act_vars, function(v) {
    any(as.vector(haven::zap_labels(dhs_kr[[v]])) == 1, na.rm = TRUE)
  }))

  if (!act_has_data && "h37e" %in% names(dhs_kr)) {
    h37e_vals <- as.vector(haven::zap_labels(dhs_kr[["h37e"]]))
    if (any(h37e_vals == 1, na.rm = TRUE)) {
      cli::cli_alert_info(
        "ACT variable{?s} {.var {act_vars}} ha{?s/ve} no positive values; using {.var h37e} which has data"
      )
      act_vars <- "h37e"
    }
  }

  has_test_var <- !is.null(survey_vars$test) && survey_vars$test %in% names(dhs_kr)

  # Zap labels
  kr <- dhs_kr |>
    dplyr::mutate(dplyr::across(dplyr::everything(), haven::zap_labels)) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), as.vector))

  # Force indicator columns to numeric (guards against haven character residuals)
  for (col in c(act_vars, survey_vars$fever, survey_vars$age, survey_vars$alive, survey_vars$test)) {
    if (!is.null(col) && col %in% names(kr)) {
      kr[[col]] <- suppressWarnings(as.numeric(as.character(kr[[col]])))
    }
  }

  # Build composite received_act from all ACT variables
  # (same pattern as received_antimalarial in .prepare_antimalarial_data)
  act_matrix <- as.matrix(kr[, act_vars, drop = FALSE])
  act_matrix[!act_matrix %in% c(0, 1)] <- NA
  kr$received_act <- apply(act_matrix, 1, function(row) {
    if (any(row == 1, na.rm = TRUE)) return(1)
    if (any(is.na(row))) return(NA_real_)
    return(0)
  })

  # Build columns (include .row_id for downstream enrichment)
  kr <- kr |>
    dplyr::mutate(
      .row_id = dplyr::row_number(),
      cluster_id = .data[[survey_vars$cluster]],
      age_months = .data[[survey_vars$age]],
      had_fever = .data[[survey_vars$fever]]
    )

  if (has_test_var) {
    kr$test_positive <- kr[[survey_vars$test]]
  } else {
    kr$test_positive <- NA_real_
  }

  if (include_survey_vars) {
    kr <- kr |>
      dplyr::mutate(
        survey_weight = .data[[survey_vars$weight]] / 1e6,
        stratum_id = .data[[survey_vars$stratum]]
      )
  }

  # Check alive variable if present
  has_alive <- !is.null(survey_vars$alive) &&
    survey_vars$alive %in% names(dhs_kr)
  if (has_alive) {
    kr <- kr |>
      dplyr::mutate(child_alive = .data[[survey_vars$alive]])
  }

  # Filter to U5 children
  kr_u5 <- kr |>
    dplyr::filter(
      age_months >= 0,
      age_months <= 59
    )

  if (has_alive) {
    kr_u5 <- kr_u5 |>
      dplyr::filter(child_alive == 1)
  }

  # Detect fever coding scheme: Some surveys use 0=No/1=Yes, others use 1=No/2=Yes
  fever_values <- unique(kr_u5$had_fever[!is.na(kr_u5$had_fever)])

  if (length(fever_values) == 0) {
    cli::cli_abort(
      "Fever variable has no valid values. Check that {.var {survey_vars$fever}} exists and contains data."
    )
  }

  # Determine "Yes" value: if values are strictly {1, 2} or {2}, assume 2=Yes
  # Otherwise, assume 1=Yes (standard DHS coding)
  if (all(fever_values %in% c(1, 2)) && 2 %in% fever_values && !0 %in% fever_values) {
    fever_yes_value <- 2
    cli::cli_alert_info(
      "Detected alternative fever coding (1=No, 2=Yes) - using 2 as 'Yes'"
    )
  } else {
    fever_yes_value <- 1
  }

  # Filter to children with fever
  kr_fever <- kr_u5 |>
    dplyr::filter(had_fever == fever_yes_value)

  if (nrow(kr_fever) == 0) {
    n_with_data <- sum(!is.na(kr_u5$had_fever))
    cli::cli_abort(c(
      "No children with fever in the last 2 weeks found.",
      "i" = "Total U5 children: {nrow(kr_u5)}",
      "i" = "Children with fever data: {n_with_data}",
      "i" = "Unique fever values: {paste(sort(fever_values), collapse = ', ')}",
      "i" = "Expected 'Yes' value: {fever_yes_value}"
    ))
  }

  if (all(is.na(kr_fever$received_act))) {
    cli::cli_abort("ACT variable {.var {act_var}} is all NA for febrile children")
  }

  # Create binary ACT indicator
  kr_fever <- kr_fever |>
    dplyr::mutate(
      has_act = dplyr::if_else(received_act == 1, 1, 0, missing = NA_real_)
    )

  cli::cli_alert_info(
    "Found {format(nrow(kr_fever), big.mark = ',')} febrile children under 5"
  )
  cli::cli_alert_info(
    "Using {length(act_vars)} ACT variable{?s}: {paste(act_vars, collapse = ', ')}"
  )

  # Record which ACT variables were resolved for downstream alignment
  attr(kr_fever, "act_vars_used") <- act_vars
  attr(kr_fever, "act_var_used") <- act_vars[1]  # backward compat

  kr_fever
}


#' Merge Febrile KR Children with PR RDT Results
#'
#' Links febrile U5 children from the KR file to their RDT results in the
#' PR (Person Recode) file. The merge uses cluster number (v001), household
#' number (v002), and child line number (b16_01) as the linkage key.
#'
#' @param kr_fever Febrile U5 data prepared by .prepare_act_data().
#' @param dhs_pr DHS Person Recode (PR) dataset containing hml35 (RDT result).
#' @param kr_cluster_var Column in kr_fever for geographic cluster number
#'   (default: "v001"). Used for PR linkage; distinct from survey design PSU.
#' @param kr_hh_var KR column for household number (default: "v002").
#' @param kr_line_var KR column for the child's line number in the household
#'   (default: "b16_01"). Links to PR hvidx.
#'
#' @return A data frame of febrile children matched to valid RDT results,
#'   containing all kr_fever columns plus: rdt_result (0/1) and
#'   has_rdt_pos (integer 0/1). Returns NULL if hml35 is absent, link
#'   variables are missing, or no children can be matched.
#'
#' @noRd
.merge_kr_pr_febrile <- function(
  kr_fever,
  dhs_pr,
  kr_cluster_var = "v001",
  kr_hh_var = "v002",
  kr_line_var = "b16_01"
) {
  if (!is.data.frame(dhs_pr)) {
    cli::cli_alert_warning(
      "`dhs_pr` must be a data.frame - skipping febrile RDT indicators"
    )
    return(NULL)
  }

  rdt_var <- "hml35"
  if (!rdt_var %in% names(dhs_pr)) {
    cli::cli_alert_warning(
      "RDT variable {.var {rdt_var}} not found in PR data - skipping febrile RDT indicators"
    )
    return(NULL)
  }

  missing_link <- setdiff(c(kr_cluster_var, kr_hh_var, kr_line_var), names(kr_fever))
  if (length(missing_link) > 0) {
    cli::cli_alert_warning(
      "KR link variables {.var {missing_link}} not found - skipping febrile RDT indicators"
    )
    return(NULL)
  }

  # Zap labels on PR
  pr <- dhs_pr |>
    dplyr::mutate(dplyr::across(dplyr::everything(), haven::zap_labels)) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), as.vector))

  # Force RDT column to numeric (guards against haven character residuals)
  if (rdt_var %in% names(pr)) {
    pr[[rdt_var]] <- suppressWarnings(as.numeric(as.character(pr[[rdt_var]])))
  }

  # Subset PR: link vars + RDT result (valid tests only: 0 = negative, 1 = positive)
  pr_link <- pr |>
    dplyr::select(
      pr_cluster = hv001,
      pr_hh      = hv002,
      pr_line    = hvidx,
      rdt_result = !!rdt_var
    ) |>
    dplyr::filter(rdt_result %in% c(0, 1))

  if (nrow(pr_link) == 0) {
    cli::cli_alert_warning(
      "No valid RDT results (0/1) found in PR data - skipping febrile RDT indicators"
    )
    return(NULL)
  }

  # Build join key: KR column names -> PR column names
  join_key <- stats::setNames(
    c("pr_cluster", "pr_hh", "pr_line"),
    c(kr_cluster_var, kr_hh_var, kr_line_var)
  )

  merged <- dplyr::inner_join(kr_fever, pr_link, by = join_key)

  n_total   <- nrow(kr_fever)
  n_matched <- nrow(merged)

  if (n_matched == 0) {
    cli::cli_alert_warning(
      "No febrile children matched to RDT results in PR data - skipping febrile RDT indicators"
    )
    return(NULL)
  }

  cli::cli_alert_info(
    "Matched {format(n_matched, big.mark = ',')} of {format(n_total, big.mark = ',')} febrile children to RDT results"
  )

  merged |>
    dplyr::mutate(has_rdt_pos = as.integer(rdt_result == 1))
}
