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
#'   \item \code{dhs_eff_cm_any}: using any care-seeking (public or private)
#'   \item \code{dhs_eff_cm_public}: using public sector care-seeking only
#' }
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
#' @param csb_classification Data frame specifying h32 variable to CSB category
#'   mapping (passed to \code{.prepare_csb_data()}). Must have columns
#'   \code{variable} and \code{csb}. If NULL, uses default WMR classification.
#' @param region_var Optional column name in \code{dhs_kr} to use as grouping
#'   variable (e.g., "v024" for region).
#'
#' @return Tibble with effective coverage estimates by grouping level:
#'   \itemize{
#'     \item Grouping variable column (if \code{region_var} provided)
#'     \item \code{dhs_eff_cm_any}: Effective CM using any care-seeking
#'     \item \code{dhs_eff_cm_any_low}, \code{dhs_eff_cm_any_upp}: 95\% CI
#'     \item \code{dhs_eff_cm_public}: Effective CM using public care-seeking
#'     \item \code{dhs_eff_cm_public_low}, \code{dhs_eff_cm_public_upp}: 95\% CI
#'     \item \code{dhs_n_fever}: Unweighted count of febrile U5 children
#'     \item \code{dhs_n_antimalarial}: Unweighted count receiving any antimalarial
#'   }
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
#' @seealso [calc_csb_dhs_core()], [calc_act_dhs()]
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
  csb_classification = NULL,
  region_var = NULL
) {
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

  # ---- 2. Prepare CSB data (febrile U5 with care-seeking indicators) ----

  csb_vars <- survey_vars[c("cluster", "weight", "stratum", "age", "fever", "alive")]
  kr_fever <- .prepare_csb_data(
    dhs_kr = dhs_kr,
    survey_vars = csb_vars,
    csb_classification = csb_classification,
    include_survey_vars = TRUE
  )

  # Build row index matching .prepare_csb_data() filtering (fever + age + alive)
  has_alive_var <- !is.null(survey_vars$alive) &&
    survey_vars$alive %in% names(dhs_kr)

  febrile_condition <- dhs_kr[[survey_vars$fever]] == 1 &
    dhs_kr[[survey_vars$age]] >= 0 &
    dhs_kr[[survey_vars$age]] <= 59

  if (has_alive_var) {
    febrile_condition <- febrile_condition &
      dhs_kr[[survey_vars$alive]] == 1
  }

  febrile_idx <- which(febrile_condition)

  # Preserve region_var from original data
  if (!is.null(region_var)) {
    kr_fever[[region_var]] <- dhs_kr[[region_var]][febrile_idx]
  }

  # ---- 3. Add antimalarial variable ----

  # Detect ml13 series or h37 fallback (check for positive values)
  ml13_vars <- grep("^ml13[a-z]+$", names(dhs_kr), value = TRUE)
  h37_vars <- grep("^h37[a-h]$", names(dhs_kr), value = TRUE)

  if (length(ml13_vars) > 0) {
    ml13_has_data <- any(
      sapply(ml13_vars, function(v) any(dhs_kr[[v]] == 1, na.rm = TRUE))
    )
    if (ml13_has_data) {
      drug_series <- ml13_vars
      cli::cli_alert_info("Detected {length(ml13_vars)} ml13 antimalarial variables")
    } else if (length(h37_vars) > 0) {
      h37_has_data <- any(
        sapply(h37_vars, function(v) any(dhs_kr[[v]] == 1, na.rm = TRUE))
      )
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
    kr_fever[[dvar]] <- haven::zap_labels(dhs_kr[[dvar]][febrile_idx])
    kr_fever[[dvar]] <- as.vector(kr_fever[[dvar]])
    # Recode "don't know" (8) and coded-missing (9) to NA
    kr_fever[[dvar]][!kr_fever[[dvar]] %in% c(0, 1)] <- NA
  }

  # Create received_antimalarial: 1 if any drug variable == 1, NA if all NA
  drug_matrix <- as.matrix(kr_fever[, drug_series, drop = FALSE])
  kr_fever$received_antimalarial <- apply(drug_matrix, 1, function(row) {
    if (all(is.na(row))) return(NA_real_)
    if (any(row == 1, na.rm = TRUE)) return(1)
    return(0)
  })

  n_am <- sum(kr_fever$received_antimalarial == 1, na.rm = TRUE)
  cli::cli_alert_info(
    "{format(n_am, big.mark = ',')} of {format(nrow(kr_fever), big.mark = ',')} febrile children received any antimalarial"
  )

  if (n_am == 0) {
    cli::cli_abort("No children received any antimalarial treatment.")
  }

  # ---- 4. Add ACT variable ----

  act_var <- survey_vars$act %||% "ml13e"
  if (act_var %in% names(dhs_kr)) {
    raw_act <- as.vector(haven::zap_labels(dhs_kr[[act_var]][febrile_idx]))
    # Check if act_var has any positive values; if not, try h37e fallback
    if (!any(raw_act == 1, na.rm = TRUE) && "h37e" %in% names(dhs_kr)) {
      h37e_vals <- as.vector(haven::zap_labels(dhs_kr[["h37e"]][febrile_idx]))
      if (any(h37e_vals == 1, na.rm = TRUE)) {
        cli::cli_alert_info(
          "ACT variable {.var {act_var}} has no positive values; using {.var h37e} which has data"
        )
        raw_act <- h37e_vals
      }
    }
    kr_fever$received_act <- dplyr::if_else(raw_act %in% c(0, 1), raw_act, NA_real_)
  } else if ("h37e" %in% names(dhs_kr)) {
    cli::cli_alert_info(
      "ACT variable {.var {act_var}} not found; using {.var h37e} fallback"
    )
    raw_act <- as.vector(haven::zap_labels(dhs_kr[["h37e"]][febrile_idx]))
    kr_fever$received_act <- dplyr::if_else(raw_act %in% c(0, 1), raw_act, NA_real_)
  } else {
    cli::cli_abort(c(
      "ACT variable {.var {act_var}} not found (also tried {.var h37e}).",
      "i" = "Check your survey_vars$act mapping"
    ))
  }

  # Binary indicator for ACT among antimalarial recipients
  kr_fever$has_act <- dplyr::if_else(
    kr_fever$received_act == 1, 1, 0,
    missing = NA_real_
  )

  # ---- 5. Set up survey design ----

  use_strata <- dplyr::n_distinct(kr_fever$stratum_id) > 1

  if (use_strata) {
    survey_options <- options(survey.lonely.psu = "certainty")
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

  # ---- 6. Compute estimates ----

  if (!is.null(region_var)) {
    result <- .compute_eff_cm_grouped(
      kr_fever = kr_fever,
      des = des,
      region_var = region_var
    )
  } else {
    result <- .compute_eff_cm_national(
      kr_fever = kr_fever,
      des = des
    )
  }

  # ---- 7. Format output ----

  result <- result |>
    dplyr::mutate(
      dplyr::across(dplyr::matches("^dhs_eff_cm_"), ~ round(.x, 4)),
      dplyr::across(dplyr::matches("_low$"), ~ pmax(0, .)),
      dplyr::across(dplyr::matches("_upp$"), ~ pmin(1, .))
    )

  count_cols <- intersect(
    c("dhs_n_fever", "dhs_n_antimalarial"),
    names(result)
  )
  result <- result |>
    dplyr::mutate(
      dplyr::across(dplyr::all_of(count_cols), ~ as.integer(round(.x)))
    )

  cli::cli_alert_success("Effective coverage of case management computed")

  tibble::as_tibble(result)
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
