#' Calculate Effective Coverage of Case Management from DHS Data
#'
#' Computes the effective coverage of case management as the product of two
#' survey-weighted proportions:
#' \deqn{Effective\;CM = CSB\;rate \times P(ACT \mid antimalarial)}
#'
#' where CSB rate is the care-seeking rate for fever among children under 5,
#' and P(ACT | antimalarial) is the proportion receiving ACT among febrile
#' children who received any antimalarial treatment.
#'
#' Two variants are produced:
#' \itemize{
#'   \item \code{EFF_CM_ANY}: using any care-seeking (public or private)
#'   \item \code{EFF_CM_PUBLIC}: using public sector care-seeking only
#' }
#'
#' Returns results in standardized long format with
#' \code{list(adm0, adm1)} structure.
#'
#' @param dhs_kr DHS children's recode (KR) dataset (data.frame or tibble).
#' @param survey_vars Named list mapping DHS variable names. Required keys:
#'   \itemize{
#'     \item \code{cluster}: Cluster/PSU ID (default: "v021")
#'     \item \code{weight}: Survey weight (default: "v005")
#'     \item \code{stratum}: Stratum variable (default: "v022")
#'     \item \code{age}: Child's age in months (default: "hw1")
#'     \item \code{fever}: Had fever in last 2 weeks (default: "h22")
#'     \item \code{alive}: Child survival status (default: "b5")
#'     \item \code{act}: Received ACT treatment (default: "ml13e")
#'   }
#' @param region_var Optional column name in \code{dhs_kr} to use as grouping
#'   variable (e.g., "v024" for region).
#'
#' @return Named list of tibbles:
#'   \describe{
#'     \item{`adm0`}{National-level estimates (always present)}
#'     \item{`adm1`}{Admin-1 estimates (when `region_var` provided)}
#'   }
#'   Each tibble contains columns: survey_id, iso3, iso2, survey_type,
#'   survey_year, adm0, adm1, type, geo_source, point, ci_l, ci_u,
#'   numerator, denominator, indicator, indicator_code,
#'   numerator_description, denominator_description, denominator_code.
#'
#' @details
#' The effective coverage indicator captures the probability that a febrile
#' child both seeks care AND receives ACT (given they receive any antimalarial).
#' CIs are approximated using the delta method assuming independence:
#' \deqn{SE(A \times B) \approx \sqrt{A^2 \cdot SE(B)^2 + B^2 \cdot SE(A)^2}}
#'
#' The antimalarial denominator includes any child receiving at least one drug
#' from the \code{ml13} series (or \code{h37a-h} fallback for older surveys).
#' ACT is identified by \code{ml13e} (or \code{h37e} fallback).
#'
#' @examples
#' \dontrun{
#' result <- calc_case_management_dhs(
#'   dhs_kr = kr_data,
#'   region_var = "v024"
#' )
#' }
#'
#' @seealso [case_management_dictionary()] for indicator definitions,
#'   [calc_csb_dhs_core()], [calc_act_dhs()]
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
    act = "ml13e"
  ),
  region_var = NULL
) {
  # Fail fast on missing suggested dependencies
  .check_pkg(
    c("tibble"),
    reason = "for `calc_case_management_dhs()`"
  )

  # ---- 1. Input validation ----

  if (!is.data.frame(dhs_kr)) {
    cli::cli_abort("`dhs_kr` must be a data.frame or tibble.")
  }
  if (nrow(dhs_kr) == 0) {
    cli::cli_abort("`dhs_kr` is empty.")
  }

  needed <- unlist(survey_vars[c("cluster", "weight", "stratum", "age", "fever")])
  missing_vars <- setdiff(needed, names(dhs_kr))
  if (length(missing_vars) > 0) {
    cli::cli_abort(c(
      "Required variables not found: {.var {missing_vars}}",
      "i" = "Check your survey_vars mapping"
    ))
  }

  if (!is.null(region_var)) {
    if (!is.character(region_var) || length(region_var) != 1) {
      cli::cli_abort("`region_var` must be a single character string.")
    }
    if (!region_var %in% names(dhs_kr)) {
      cli::cli_abort(c(
        "Column {.var {region_var}} not found in `dhs_kr`.",
        "i" = "Available columns: {.var {head(names(dhs_kr), 10)}}..."
      ))
    }
  }

  # Auto-fallback to v024 when no region_var provided
  if (is.null(region_var) && "v024" %in% names(dhs_kr)) {
    region_var <- "v024"
    cli::cli_alert_info(
      "No region_var specified; defaulting to {.var v024} for adm1"
    )
  }

  # ---- 2. Extract survey metadata ----

  survey_meta <- .extract_survey_meta(dhs_kr)
  geo_src <- if (!is.null(region_var)) "survey" else NA_character_

  # ---- 3. Prepare CSB data (febrile U5 with care-seeking indicators) ----

  csb_vars <- survey_vars[c("cluster", "weight", "stratum", "age", "fever", "alive")]
  kr_fever <- .prepare_csb_data(
    dhs_kr = dhs_kr,
    survey_vars = csb_vars,
    include_survey_vars = TRUE
  )

  # Zap labels on the raw dataset for safe comparisons
  dhs_kr_zapped <- dhs_kr |>
    dplyr::mutate(dplyr::across(dplyr::everything(), haven::zap_labels)) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), as.vector))

  # Build row index matching .prepare_csb_data() filtering (fever + age + alive)
  has_alive_var <- !is.null(survey_vars$alive) &&
    survey_vars$alive %in% names(dhs_kr_zapped)

  # Detect fever coding scheme: Some surveys use 0=No/1=Yes, others use 1=No/2=Yes
  fever_values <- unique(dhs_kr_zapped[[survey_vars$fever]][
    !is.na(dhs_kr_zapped[[survey_vars$fever]])
  ])

  # Determine "Yes" value: if values are strictly {1, 2} or {2}, assume 2=Yes
  # Otherwise, assume 1=Yes (standard DHS coding)
  if (all(fever_values %in% c(1, 2)) && 2 %in% fever_values && !0 %in% fever_values) {
    fever_yes_value <- 2
  } else {
    fever_yes_value <- 1
  }

  febrile_condition <- dhs_kr_zapped[[survey_vars$fever]] == fever_yes_value &
    dhs_kr_zapped[[survey_vars$age]] >= 0 &
    dhs_kr_zapped[[survey_vars$age]] <= 59

  if (has_alive_var) {
    febrile_condition <- febrile_condition &
      dhs_kr_zapped[[survey_vars$alive]] == 1
  }

  febrile_idx <- which(febrile_condition)

  # Preserve region_var from original data
  if (!is.null(region_var)) {
    kr_fever$region <- .resolve_region_labels(
      dhs_kr[[region_var]][febrile_idx], region_var
    )
  }

  # ---- 4. Add antimalarial variable ----

  # Label-based antimalarial variable detection: only include variables whose
  # labels contain actual drug names -- excludes non-drug response codes
  # ("Don't know", "Other", etc.) that would inflate the antimalarial composite.
  antimalarial_pattern <- paste0(
    "antimalarial|fansidar|chloroquine|amodiaquine|quinine|",
    "artemether|artesunate|dihydroartemis|artemisinin|coartem|",
    "\\bsp\\b|\\bcta\\b|\\bact\\b|mefloquine|piperaquine|lumefantrine"
  )

  .detect_am_labels_cm <- function(candidates) {
    matched <- character(0)
    for (v in candidates) {
      lbl <- attr(dhs_kr[[v]], "label")
      if (is.null(lbl) || !is.character(lbl) ||
          length(lbl) != 1) next
      if (grepl(antimalarial_pattern, lbl, ignore.case = TRUE)) {
        matched <- c(matched, v)
      }
    }
    matched
  }

  ml13_candidates <- grep("^ml13[a-z]+$", names(dhs_kr), value = TRUE)
  h37_candidates  <- grep("^h37[a-z]+$", names(dhs_kr), value = TRUE)

  ml13_vars <- .detect_am_labels_cm(ml13_candidates)
  h37_vars  <- .detect_am_labels_cm(h37_candidates)

  # Fall back to standard drug slots (a-h) if no labels available
  if (length(ml13_vars) == 0 && length(h37_vars) == 0) {
    ml13_vars <- grep("^ml13[a-h]$", names(dhs_kr_zapped), value = TRUE)
    h37_vars  <- grep("^h37[a-h]$", names(dhs_kr_zapped), value = TRUE)
  }

  act_var_name <- survey_vars$act %||% "ml13e"
  act_used_h37 <- FALSE
  if (act_var_name %in% names(dhs_kr_zapped)) {
    if (!any(dhs_kr_zapped[[act_var_name]] == 1, na.rm = TRUE) &&
        "h37e" %in% names(dhs_kr_zapped) &&
        any(dhs_kr_zapped[["h37e"]] == 1, na.rm = TRUE)) {
      act_used_h37 <- TRUE
    }
  } else if ("h37e" %in% names(dhs_kr_zapped)) {
    act_used_h37 <- TRUE
  }

  if (act_used_h37 && length(h37_vars) > 0) {
    drug_series <- h37_vars
    cli::cli_alert_info(
      "Antimalarial composite using {length(h37_vars)} h37 series (aligned with ACT h37e fallback)"
    )
  } else if (length(ml13_vars) > 0) {
    ml13_has_data <- any(sapply(ml13_vars, function(v) {
      any(dhs_kr_zapped[[v]] == 1, na.rm = TRUE)
    }))
    if (ml13_has_data) {
      drug_series <- ml13_vars
      cli::cli_alert_info("Detected {length(ml13_vars)} ml13 antimalarial variables")
    } else if (length(h37_vars) > 0) {
      h37_has_data <- any(sapply(h37_vars, function(v) {
        any(dhs_kr_zapped[[v]] == 1, na.rm = TRUE)
      }))
      if (h37_has_data) {
        drug_series <- h37_vars
        cli::cli_alert_info(
          "ml13 variables have no positive values; using {length(h37_vars)} h37 series which has data"
        )
      } else {
        drug_series <- ml13_vars
        cli::cli_alert_info("Detected {length(ml13_vars)} ml13 antimalarial variables (no positive values found)")
      }
    } else {
      drug_series <- ml13_vars
      cli::cli_alert_info("Detected {length(ml13_vars)} ml13 antimalarial variables (no positive values found)")
    }
  } else if (length(h37_vars) > 0) {
    drug_series <- h37_vars
    cli::cli_alert_info(
      "ml13 series not found; using {length(h37_vars)} h37 fallback variables"
    )
  } else {
    cli::cli_abort(
      "No antimalarial variables found (ml13* or h37a-h)."
    )
  }

  for (dvar in drug_series) {
    kr_fever[[dvar]] <- dhs_kr_zapped[[dvar]][febrile_idx]
    kr_fever[[dvar]][!kr_fever[[dvar]] %in% c(0, 1)] <- NA
  }

  drug_matrix <- as.matrix(kr_fever[, drug_series, drop = FALSE])
  kr_fever$received_antimalarial <- apply(drug_matrix, 1, function(row) {
    if (any(row == 1, na.rm = TRUE)) return(1)
    if (any(is.na(row))) return(NA_real_)
    return(0)
  })

  n_am <- sum(kr_fever$received_antimalarial == 1, na.rm = TRUE)
  cli::cli_alert_info(
    "{format(n_am, big.mark = ',')} of {format(nrow(kr_fever), big.mark = ',')} febrile children received any antimalarial"
  )

  if (n_am == 0) {
    cli::cli_abort("No children received any antimalarial treatment.")
  }

  # ---- 5. Add ACT variable (composite) ----

  act_input <- survey_vars$act %||% "ml13e"
  if (length(act_input) == 1 && act_input == "ml13e") {
    act_vars <- .detect_act_vars(dhs_kr, default_vars = act_input)
  } else {
    act_vars <- act_input
  }
  act_vars <- intersect(act_vars, names(dhs_kr_zapped))

  if (length(act_vars) == 0) {
    h37_acts <- .detect_act_vars(dhs_kr, default_vars = "h37e")
    act_vars <- intersect(h37_acts, names(dhs_kr_zapped))
    if (length(act_vars) > 0) {
      cli::cli_alert_info(
        "ml13 ACT variables not found; using h37 series: {paste(act_vars, collapse = ', ')}"
      )
    } else {
      cli::cli_abort(c(
        "No ACT variables found (tried ml13* and h37*).",
        "i" = "Check your survey_vars$act mapping"
      ))
    }
  }

  act_has_data <- any(sapply(act_vars, function(v) {
    any(dhs_kr_zapped[[v]][febrile_idx] == 1, na.rm = TRUE)
  }))

  if (!act_has_data && "h37e" %in% names(dhs_kr_zapped)) {
    h37e_vals <- dhs_kr_zapped[["h37e"]][febrile_idx]
    if (any(h37e_vals == 1, na.rm = TRUE)) {
      cli::cli_alert_info(
        "ACT variable{?s} {.var {act_vars}} ha{?s/ve} no positive values; using {.var h37e} which has data"
      )
      act_vars <- "h37e"
    }
  }

  act_matrix <- sapply(act_vars, function(v) dhs_kr_zapped[[v]][febrile_idx])
  act_matrix <- matrix(act_matrix, nrow = length(febrile_idx), ncol = length(act_vars))
  act_matrix[!act_matrix %in% c(0, 1)] <- NA
  kr_fever$received_act <- apply(act_matrix, 1, function(row) {
    if (any(row == 1, na.rm = TRUE)) return(1)
    if (any(is.na(row))) return(NA_real_)
    return(0)
  })

  cli::cli_alert_info(
    "Using {length(act_vars)} ACT variable{?s}: {paste(act_vars, collapse = ', ')}"
  )

  kr_fever$has_act <- dplyr::if_else(
    kr_fever$received_act == 1, 1, 0,
    missing = NA_real_
  )

  # ---- 6. Set up survey design ----

  use_strata <- dplyr::n_distinct(kr_fever$stratum_id) > 1

  if (use_strata) {
    survey_options <- options(survey.lonely.psu = "adjust")
    on.exit(options(survey_options), add = TRUE)

    des <- survey::svydesign(
      ids = ~cluster_id,
      strata = ~stratum_id,
      weights = ~survey_weight,
      data = kr_fever,
      nest = TRUE
    )
  } else {
    des <- survey::svydesign(
      ids = ~cluster_id,
      weights = ~survey_weight,
      data = kr_fever,
      nest = TRUE
    )
  }

  # ---- 7. Compute estimates and build long-format output ----

  conds <- .case_management_conditions()

  meta_cols <- tibble::tibble(
    survey_id   = survey_meta$survey_id,
    iso3        = survey_meta$iso3,
    iso2        = survey_meta$iso2,
    survey_type = survey_meta$survey_type,
    survey_year = survey_meta$survey_year,
    adm0        = survey_meta$country_upper
  )

  # --- National estimates ---
  national_result <- .compute_eff_cm_national(
    kr_fever = kr_fever,
    des = des
  )

  national_long <- .eff_cm_to_long(national_result, conds, level = "adm0",
                                    location = "National")

  national_long <- national_long |>
    dplyr::mutate(
      point       = round(point, 4),
      ci_l        = round(pmax(ci_l, 0, na.rm = TRUE), 4),
      ci_u        = round(pmin(ci_u, 1, na.rm = TRUE), 4),
      numerator   = as.integer(numerator),
      denominator = as.integer(denominator)
    )

  adm0_tbl <- dplyr::bind_cols(
    meta_cols[rep(1, nrow(national_long)), ],
    tibble::tibble(type = "survey_weighted", geo_source = NA_character_),
    national_long |> dplyr::select(-level, -location)
  ) |>
    tibble::as_tibble()

  out <- list(adm0 = adm0_tbl)

  # --- Regional estimates ---
  if (!is.null(region_var)) {
    group_var <- "region"
    regional_result <- .compute_eff_cm_grouped(
      kr_fever = kr_fever,
      des = des,
      region_var = group_var
    )

    # Convert each region row to long format
    regions <- unique(kr_fever[[group_var]])
    regions <- regions[!is.na(regions)]

    regional_long_list <- list()
    for (rgn in regions) {
      rgn_row <- regional_result[regional_result[[group_var]] == rgn, ]
      if (nrow(rgn_row) == 0) next
      rgn_long <- .eff_cm_to_long(rgn_row, conds, level = "adm1",
                                    location = as.character(rgn))
      regional_long_list[[as.character(rgn)]] <- rgn_long
    }

    sub_results <- dplyr::bind_rows(regional_long_list)

    if (nrow(sub_results) > 0) {
      sub_results <- sub_results |>
        dplyr::mutate(
          point       = round(point, 4),
          ci_l        = round(pmax(ci_l, 0, na.rm = TRUE), 4),
          ci_u        = round(pmin(ci_u, 1, na.rm = TRUE), 4),
          numerator   = as.integer(numerator),
          denominator = as.integer(denominator)
        )

      sub_tbl <- dplyr::bind_cols(
        meta_cols[rep(1, nrow(sub_results)), ],
        sub_results |>
          dplyr::transmute(
            adm1       = toupper(location),
            type       = "survey_weighted",
            geo_source = geo_src,
            point, ci_l, ci_u,
            numerator, denominator,
            indicator, indicator_code,
            numerator_description,
            denominator_description, denominator_code
          )
      ) |>
        tibble::as_tibble()

      out[["adm1"]] <- sub_tbl
    }
  }

  cli::cli_alert_success("Effective coverage of case management computed")

  out
}


# =============================================================================
# Case management indicator conditions and dictionary
# =============================================================================

#' Internal: Case management indicator conditions
#'
#' Returns list of indicator specifications for effective case management
#' indicators. These are product indicators (CSB * ACT|AM) computed via
#' delta method, not standard proportions.
#'
#' @return List of named lists.
#' @noRd
.case_management_conditions <- function() {
  list(
    list(
      indicator       = "EFF_CM_ANY",
      indicator_code  = "eff_cm_any",
      indicator_title = "Effective case management (any care-seeking)",
      outcome_var     = NA_character_,
      filter_expr     = NULL,
      num_desc        = "Febrile U5 who sought any care AND received ACT given antimalarial",
      denom_desc      = "Febrile children under 5",
      denom_code      = "feb_u5"
    ),
    list(
      indicator       = "EFF_CM_PUBLIC",
      indicator_code  = "eff_cm_public",
      indicator_title = "Effective case management (public sector)",
      outcome_var     = NA_character_,
      filter_expr     = NULL,
      num_desc        = "Febrile U5 who sought public care AND received ACT given antimalarial",
      denom_desc      = "Febrile children under 5",
      denom_code      = "feb_u5"
    )
  )
}


#' Case Management Indicator Dictionary
#'
#' Returns the dictionary of effective case management indicators.
#'
#' @return Tibble with columns: indicator, indicator_code, indicator_title,
#'   numerator_description, denominator_description, denominator_code.
#'
#' @keywords internal
case_management_dictionary <- function() {
  conds <- .case_management_conditions()
  tibble::tibble(
    indicator               = vapply(conds, `[[`, character(1), "indicator"),
    indicator_code          = vapply(conds, `[[`, character(1), "indicator_code"),
    indicator_title         = vapply(conds, `[[`, character(1), "indicator_title"),
    numerator_description   = vapply(conds, `[[`, character(1), "num_desc"),
    denominator_description = vapply(conds, `[[`, character(1), "denom_desc"),
    denominator_code        = vapply(conds, `[[`, character(1), "denom_code")
  )
}


# =============================================================================
# Internal helpers
# =============================================================================

#' Convert effective CM result row to long format
#'
#' Takes a single-row tibble from .compute_eff_cm_national() or a single
#' region row from .compute_eff_cm_grouped() and pivots to long format
#' with one row per indicator.
#'
#' @param result_row Single-row tibble with dhs_eff_cm_any, dhs_eff_cm_public, etc.
#' @param conds List of indicator conditions.
#' @param level Character: "adm0" or "adm1".
#' @param location Character: region name or "National".
#' @return Tibble with standardized long-format columns.
#' @noRd
.eff_cm_to_long <- function(result_row, conds, level, location) {
  rows <- list()

  # EFF_CM_ANY
  any_cond <- conds[[1]]
  n_fever <- if ("dhs_n_fever" %in% names(result_row)) result_row$dhs_n_fever[1] else NA_integer_
  n_am <- if ("dhs_n_antimalarial" %in% names(result_row)) result_row$dhs_n_antimalarial[1] else NA_integer_

  rows[[1]] <- tibble::tibble(
    level    = level,
    location = location,
    point    = result_row$dhs_eff_cm_any[1],
    ci_l     = result_row$dhs_eff_cm_any_low[1],
    ci_u     = result_row$dhs_eff_cm_any_upp[1],
    numerator   = n_am,
    denominator = n_fever,
    indicator               = any_cond$indicator_title,
    indicator_code          = any_cond$indicator_code,
    numerator_description   = any_cond$num_desc,
    denominator_description = any_cond$denom_desc,
    denominator_code        = any_cond$denom_code
  )

  # EFF_CM_PUBLIC
  pub_cond <- conds[[2]]
  n_am_pub <- if ("dhs_n_antimalarial_public" %in% names(result_row)) {
    result_row$dhs_n_antimalarial_public[1]
  } else {
    NA_integer_
  }

  rows[[2]] <- tibble::tibble(
    level    = level,
    location = location,
    point    = result_row$dhs_eff_cm_public[1],
    ci_l     = result_row$dhs_eff_cm_public_low[1],
    ci_u     = result_row$dhs_eff_cm_public_upp[1],
    numerator   = n_am_pub,
    denominator = n_fever,
    indicator               = pub_cond$indicator_title,
    indicator_code          = pub_cond$indicator_code,
    numerator_description   = pub_cond$num_desc,
    denominator_description = pub_cond$denom_desc,
    denominator_code        = pub_cond$denom_code
  )

  dplyr::bind_rows(rows)
}


#' Compute effective CM at national level (no grouping)
#'
#' @param kr_fever Prepared febrile U5 dataset with CSB, antimalarial, ACT cols.
#' @param des Survey design object for all febrile U5.
#' @return Single-row tibble with effective CM estimates.
#' @noRd
.compute_eff_cm_national <- function(kr_fever, des) {
  # CSB rates among all febrile U5
  csb_means <- survey::svymean(
    ~ csb_any_treatment + csb_public,
    design = des,
    na.rm = TRUE
  )

  csb_se <- sqrt(diag(stats::vcov(csb_means)))
  csb_any_est <- as.numeric(csb_means["csb_any_treatment"])
  csb_any_se <- as.numeric(csb_se["csb_any_treatment"])
  csb_pub_est <- as.numeric(csb_means["csb_public"])
  csb_pub_se <- as.numeric(csb_se["csb_public"])

  # ACT rate among antimalarial recipients who sought ANY care (for eff_cm_any)
  act_am_any <- .compute_act_among_am(kr_fever, csb_filter = "csb_any_treatment")

  # ACT rate among PUBLIC-CARE antimalarial recipients (for eff_cm_public)
  act_am_public <- .compute_act_among_am(kr_fever, csb_filter = "csb_public")

  # Compute products with delta method CIs
  eff_any <- .delta_product(csb_any_est, csb_any_se, act_am_any$est, act_am_any$se)
  eff_pub <- .delta_product(csb_pub_est, csb_pub_se, act_am_public$est, act_am_public$se)

  tibble::tibble(
    dhs_eff_cm_any = eff_any$est,
    dhs_eff_cm_any_low = eff_any$low,
    dhs_eff_cm_any_upp = eff_any$upp,
    dhs_eff_cm_public = eff_pub$est,
    dhs_eff_cm_public_low = eff_pub$low,
    dhs_eff_cm_public_upp = eff_pub$upp,
    dhs_n_fever = nrow(kr_fever),
    dhs_n_antimalarial = sum(kr_fever$received_antimalarial == 1, na.rm = TRUE),
    dhs_n_antimalarial_public = sum(
      kr_fever$received_antimalarial == 1 & kr_fever$csb_public == 1,
      na.rm = TRUE
    )
  )
}


#' Compute effective CM by region group
#'
#' @param kr_fever Prepared febrile U5 dataset.
#' @param des Survey design object for all febrile U5.
#' @param region_var Character name of grouping column.
#' @return Tibble with one row per region.
#' @noRd
.compute_eff_cm_grouped <- function(kr_fever, des, region_var) {
  group_formula <- stats::as.formula(paste("~", region_var))

  # CSB rates by group
  csb_by <- tryCatch({
    survey::svyby(
      ~ csb_any_treatment + csb_public,
      by = group_formula,
      design = des,
      FUN = survey::svymean,
      vartype = c("se"),
      na.rm = TRUE,
      keep.names = FALSE
    ) |>
      tibble::as_tibble()
  }, error = function(e) {
    if (grepl("has only one PSU", e$message)) {
      cli::cli_alert_warning("Single PSU issue; trying without strata")
      des_ns <- survey::svydesign(
        ids = ~cluster_id, weights = ~survey_weight,
        data = kr_fever, nest = TRUE
      )
      survey::svyby(
        ~ csb_any_treatment + csb_public,
        by = group_formula,
        design = des_ns,
        FUN = survey::svymean,
        vartype = c("se"),
        na.rm = TRUE,
        keep.names = FALSE
      ) |>
        tibble::as_tibble()
    } else {
      stop(e)
    }
  })

  # Sample sizes by group
  sample_sizes <- kr_fever |>
    dplyr::group_by(.data[[region_var]]) |>
    dplyr::summarise(
      dhs_n_fever = dplyr::n(),
      dhs_n_antimalarial = sum(received_antimalarial == 1, na.rm = TRUE),
      dhs_n_antimalarial_public = sum(
        received_antimalarial == 1 & csb_public == 1,
        na.rm = TRUE
      ),
      .groups = "drop"
    )

  # ACT among antimalarial recipients, by group
  groups <- unique(kr_fever[[region_var]])
  group_results <- list()

  for (grp in groups) {
    kr_grp <- kr_fever[kr_fever[[region_var]] == grp, ]
    act_am_any <- .compute_act_among_am(kr_grp, csb_filter = "csb_any_treatment")
    act_am_public <- .compute_act_among_am(kr_grp, csb_filter = "csb_public")

    # Get CSB estimates for this group
    grp_row <- csb_by[csb_by[[region_var]] == grp, ]
    csb_any_est <- grp_row$csb_any_treatment
    csb_any_se <- grp_row$`se.csb_any_treatment`
    csb_pub_est <- grp_row$csb_public
    csb_pub_se <- grp_row$`se.csb_public`

    eff_any <- .delta_product(csb_any_est, csb_any_se, act_am_any$est, act_am_any$se)
    eff_pub <- .delta_product(csb_pub_est, csb_pub_se, act_am_public$est, act_am_public$se)

    group_results[[as.character(grp)]] <- tibble::tibble(
      !!region_var := grp,
      dhs_eff_cm_any = eff_any$est,
      dhs_eff_cm_any_low = eff_any$low,
      dhs_eff_cm_any_upp = eff_any$upp,
      dhs_eff_cm_public = eff_pub$est,
      dhs_eff_cm_public_low = eff_pub$low,
      dhs_eff_cm_public_upp = eff_pub$upp
    )
  }

  result <- dplyr::bind_rows(group_results) |>
    dplyr::left_join(sample_sizes, by = region_var)

  result
}


#' Compute ACT rate among antimalarial recipients
#'
#' Builds a survey design on the subset of febrile children who received
#' any antimalarial and estimates the proportion who received ACT.
#'
#' @param kr_data Data frame with received_antimalarial, has_act, cluster_id,
#'   stratum_id, survey_weight columns.
#' @param csb_filter Optional column name to filter by care-seeking source.
#'   When set (e.g. "csb_public"), only antimalarial recipients where that
#'   column == 1 are included. Used to condition ACT rate on public care-seeking.
#' @return List with est (point estimate) and se (standard error).
#' @noRd
.compute_act_among_am <- function(kr_data, csb_filter = NULL) {
  kr_am <- kr_data |>
    dplyr::filter(received_antimalarial == 1, !is.na(has_act))

  # Apply optional care-seeking filter
  if (!is.null(csb_filter)) {
    if (!csb_filter %in% names(kr_am)) {
      cli::cli_alert_warning(
        "Column {.var {csb_filter}} not found - returning NA"
      )
      return(list(est = NA_real_, se = NA_real_))
    }
    kr_am <- kr_am |>
      dplyr::filter(.data[[csb_filter]] == 1)
  }

  if (nrow(kr_am) == 0 || dplyr::n_distinct(kr_am$cluster_id) < 2) {
    cli::cli_alert_warning(
      "Too few antimalarial recipients for ACT rate estimation"
    )
    return(list(est = NA_real_, se = NA_real_))
  }

  use_strata_am <- dplyr::n_distinct(kr_am$stratum_id) > 1

  if (use_strata_am) {
    des_am <- survey::svydesign(
      ids = ~cluster_id, strata = ~stratum_id,
      weights = ~survey_weight, data = kr_am, nest = TRUE
    )
  } else {
    des_am <- survey::svydesign(
      ids = ~cluster_id, weights = ~survey_weight,
      data = kr_am, nest = TRUE
    )
  }

  act_mean <- tryCatch(
    survey::svymean(~has_act, design = des_am, na.rm = TRUE),
    error = function(e) {
      if (grepl("has only one PSU", e$message)) {
        des_ns <- survey::svydesign(
          ids = ~cluster_id, weights = ~survey_weight,
          data = kr_am, nest = TRUE
        )
        survey::svymean(~has_act, design = des_ns, na.rm = TRUE)
      } else {
        stop(e)
      }
    }
  )

  act_se <- sqrt(diag(stats::vcov(act_mean)))
  list(
    est = as.numeric(act_mean["has_act"]),
    se = as.numeric(act_se["has_act"])
  )
}


#' Delta method for product of two independent proportions
#'
#' Computes point estimate and approximate 95% CI for A * B using:
#' SE(A*B) = sqrt(A^2 * SE(B)^2 + B^2 * SE(A)^2)
#'
#' @param a_est Point estimate of A.
#' @param a_se Standard error of A.
#' @param b_est Point estimate of B.
#' @param b_se Standard error of B.
#' @return List with est, se, low, upp.
#' @noRd
.delta_product <- function(a_est, a_se, b_est, b_se) {
  if (is.na(a_est) || is.na(b_est)) {
    return(list(est = NA_real_, se = NA_real_,
                low = NA_real_, upp = NA_real_))
  }

  product <- a_est * b_est
  se <- sqrt(a_est^2 * b_se^2 + b_est^2 * a_se^2)

  list(
    est = product,
    se = se,
    low = product - 1.96 * se,
    upp = product + 1.96 * se
  )
}
