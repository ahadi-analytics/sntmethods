#' Calculate WMR ACT Treatment Indicators from DHS Data
#'
#' Computes the full set of WMR (World Malaria Report) ACT treatment indicators
#' from DHS Children's Recode (KR) data. Returns survey-weighted proportions
#' with logit confidence intervals in WMR long format.
#'
#' @details
#' Computes up to 10 ACT indicators following WMR methodology. Each indicator
#' measures the proportion of febrile U5 children receiving ACT within a
#' specific subpopulation defined by care-seeking behaviour and antimalarial
#' receipt. See [act_wmr_dictionary()] for the full indicator list.
#'
#' The function uses three internal helpers:
#' \itemize{
#'   \item [.prepare_act_data()] for ACT variable detection and febrile U5 filtering
#'   \item [.classify_csb_from_h32()] for care-seeking behaviour classification
#'   \item Antimalarial composite built from ml13/h37 drug series
#' }
#'
#' @param dhs_kr DHS Children's Recode (KR) dataset (data.frame or tibble).
#'   If read via `dhs_read()`, haven labels may be standardised. Supply
#'   `dhs_kr_raw` for accurate ACT variable detection.
#' @param dhs_kr_raw Optional un-standardised KR dataset (e.g., from
#'   `arrow::read_parquet()`) with original survey-specific haven labels.
#'   When provided, ACT and antimalarial variables are detected from its
#'   labels, and any extra variables (e.g., ml13aa, ml13da) are copied
#'   into the analysis dataset. This is the "two-pass" approach needed when
#'   `dhs_read()` strips country-specific drug names.
#' @param survey_vars Named list mapping DHS variable names. Required keys:
#'   \itemize{
#'     \item `cluster`: Cluster/PSU ID (default: "v021")
#'     \item `weight`: Survey weight (default: "v005")
#'     \item `stratum`: Stratum variable (default: "v022")
#'     \item `age`: Child's age in months (default: "hw1")
#'     \item `fever`: Had fever in last 2 weeks (default: "h22")
#'     \item `alive`: Child alive (default: "b5")
#'     \item `act`: ACT variable(s) (default: "ml13e"; auto-detected from labels)
#'     \item `test`: Test-positive filter variable (default: "ml13a")
#'   }
#' @param region_var Optional column name for subnational grouping (e.g., "v024").
#' @param gps_data Optional DHS GE (Geographic) dataset with cluster coordinates.
#' @param gps_vars Named list for GE variables: cluster, lat, lon.
#' @param shapefile Optional sf object with administrative boundaries.
#' @param admin_level Character vector of admin columns from shapefile.
#' @param join_nearest Logical; if TRUE, assigns unmatched clusters to nearest
#'   admin unit. Default: TRUE.
#' @param dhs_pr Optional DHS Person Recode (PR) for febrile RDT indicators.
#' @param indicators Character vector of indicator names to compute. If NULL
#'   (default), computes all indicators from [act_wmr_dictionary()].
#' @param ci_method Method for confidence intervals. Default: "logit".
#'
#' @return Named list of tibbles, one per admin level:
#'   \describe{
#'     \item{`adm0`}{National-level estimates (always present)}
#'     \item{`adm1`}{Admin-1 estimates (when `region_var` or shapefile used)}
#'     \item{`adm2`}{Admin-2 estimates (when shapefile with adm2 used)}
#'   }
#'   Each tibble contains columns:
#'   \itemize{
#'     \item `survey_id`: Survey identifier (e.g., "TG2017DHS")
#'     \item `iso3`: ISO 3166-1 alpha-3 country code (e.g., "TGO")
#'     \item `iso2`: DHS 2-letter country code (e.g., "TG")
#'     \item `survey_type`: Survey type ("DHS", "MIS", or "AIS")
#'     \item `survey_year`: Survey year (integer)
#'     \item `adm0`: Country name in UPPERCASE (e.g., "TOGO")
#'     \item `adm1`, `adm2`: Admin names in UPPERCASE (subnational tabs only)
#'     \item `type`: Analysis type ("survey_weighted")
#'     \item `geo_source`: Source of admin names ("survey" or "gps")
#'     \item `point`: Survey-weighted proportion (0-1 scale)
#'     \item `ci_l`, `ci_u`: Lower and upper 95\% CI bounds
#'     \item `counts`: Unweighted numerator count (has_act == 1 in filtered subgroup)
#'     \item `denominator`: Unweighted denominator count (condition-filtered subgroup size, matches denominator_description)
#'     \item `indicator`: Indicator name in Title Case (e.g., "Act Antimalarial")
#'     \item `indicator_code`: Short indicator code (e.g., "act_antimal")
#'     \item `numerator_description`: Description of numerator
#'     \item `denominator_description`: Description of denominator
#'   }
#'
#' @examples
#' \dontrun{
#' # Two-pass approach (recommended): read standardised + raw
#' kr <- sntmethods::dhs_read(path, file_type = "KR", ...)
#' kr_raw <- arrow::read_parquet(parquet_path)
#' act <- calc_act_dhs(dhs_kr = kr, dhs_kr_raw = kr_raw)
#'
#' # By region
#' act <- calc_act_dhs(dhs_kr = kr, dhs_kr_raw = kr_raw, region_var = "v024")
#'
#' # With GE spatial join
#' act <- calc_act_dhs(
#'   dhs_kr = kr, dhs_kr_raw = kr_raw,
#'   gps_data = ge_data,
#'   shapefile = admin_sf,
#'   admin_level = "adm1"
#' )
#'
#' # Subset of indicators
#' act <- calc_act_dhs(
#'   dhs_kr = kr, dhs_kr_raw = kr_raw,
#'   indicators = c("ACT_ANTIMALARIAL", "ACT_PUBLIC_ANTIMALARIAL")
#' )
#' }
#'
#' @seealso [act_wmr_dictionary()] for indicator definitions,
#'   [calc_act_mbg()] for cluster-level MBG inputs
#' @export
calc_act_dhs <- function(
  dhs_kr,
  dhs_kr_raw   = NULL,
  survey_vars = list(
    cluster = "v021",
    weight  = "v005",
    stratum = "v022",
    age     = "hw1",
    fever   = "h22",
    alive   = "b5",
    act     = "ml13e",
    test    = "ml13a"
  ),
  region_var   = NULL,
  gps_data     = NULL,
  gps_vars     = list(
    cluster = "DHSCLUST",
    lat     = "LATNUM",
    lon     = "LONGNUM"
  ),
  shapefile    = NULL,
  admin_level  = NULL,
  join_nearest = TRUE,
  dhs_pr       = NULL,
  indicators   = NULL,
  ci_method    = "logit"
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
      cli::cli_abort("Column {.var {region_var}} not found in `dhs_kr`.")
    }
  }

  # ---- 1b. Extract survey metadata ----
  survey_meta <- .extract_survey_meta(dhs_kr)

  # ---- 2. Two-pass ACT/antimalarial detection (when dhs_kr_raw provided) ----
  #
  # dhs_read() standardises haven labels, replacing country-specific drug

  # names with "NA - CS antimalarial". This makes auto-detection of ACT
  # variables fail. When dhs_kr_raw (un-standardised parquet data) is
  # provided, we detect ACT and antimalarial variables from its labels,
  # then override survey_vars$act with the detected variables.

  act_pattern <- paste0(
    "\\bact\\b|combin.*artemi|artemi.*combin|",
    "artemether.+lumef|artesunate.+amodiaq|dihydroartemis|",
    "coartem|\\bcta\\b"
  )
  exclude_pattern <- "rectal|injection|\\biv\\b|monotherapy"
  antimalarial_pattern <- paste0(
    "antimalarial|fansidar|chloroquine|amodiaquine|quinine|",
    "artemether|artesunate|dihydroartemis|artemisinin|coartem|",
    "\\bsp\\b|\\bcta\\b|\\bact\\b|mefloquine|piperaquine|lumefantrine"
  )

  raw_act_vars <- character(0)
  raw_am_vars  <- character(0)

  if (!is.null(dhs_kr_raw)) {
    cli::cli_h3("Detecting drug variables from raw labels")

    ml13_candidates <- grep("^ml13[a-z]", names(dhs_kr_raw), value = TRUE)

    for (v in ml13_candidates) {
      lbl <- attr(dhs_kr_raw[[v]], "label")
      if (is.null(lbl) || !is.character(lbl) || length(lbl) != 1) next

      is_act <- grepl(act_pattern, lbl, ignore.case = TRUE) &&
        !grepl(exclude_pattern, lbl, ignore.case = TRUE)
      is_am  <- grepl(antimalarial_pattern, lbl, ignore.case = TRUE)

      raw <- as.vector(haven::zap_labels(dhs_kr_raw[[v]]))
      n_pos <- sum(raw == 1, na.rm = TRUE)
      marker <- if (is_act) " <<< ACT" else ""
      cli::cli_alert_info(
        "  {v}: {.val {lbl}}, n_positive = {n_pos}{marker}"
      )

      if (is_act) raw_act_vars <- c(raw_act_vars, v)
      if (is_am)  raw_am_vars  <- c(raw_am_vars, v)
    }

    if (length(raw_act_vars) > 0) {
      cli::cli_alert_success(
        "ACT variables (from raw labels): {paste(raw_act_vars, collapse = ', ')}"
      )
      # Override survey_vars$act with detected variables
      survey_vars$act <- raw_act_vars
    }
    if (length(raw_am_vars) > 0) {
      cli::cli_alert_success(
        "Antimalarial variables: {paste(raw_am_vars, collapse = ', ')}"
      )
    }
  }

  # ---- 3. Prepare febrile U5 dataset with ACT composite ----

  # When dhs_kr_raw is provided, copy any extra variables into dhs_kr
  # so .prepare_act_data() can find them
  if (!is.null(dhs_kr_raw)) {
    all_needed <- unique(c(raw_act_vars, raw_am_vars))
    extra_vars <- setdiff(all_needed, names(dhs_kr))
    for (ev in extra_vars) {
      dhs_kr[[ev]] <- dhs_kr_raw[[ev]]
      cli::cli_alert_info("  Copied {.var {ev}} from raw data")
    }
  }

  kr_fever <- .prepare_act_data(
    dhs_kr           = dhs_kr,
    survey_vars      = survey_vars,
    include_survey_vars = TRUE
  )

  act_vars_used <- attr(kr_fever, "act_vars_used") %||%
    attr(kr_fever, "act_var_used") %||% (survey_vars$act %||% "ml13e")

  cli::cli_alert_success(
    "Febrile U5: {format(nrow(kr_fever), big.mark = ',')} children"
  )

  # ---- 4. Add antimalarial composite ----

  dhs_kr_zapped <- dhs_kr |>
    dplyr::mutate(dplyr::across(dplyr::everything(), haven::zap_labels)) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), as.vector))

  # Reconstruct febrile index to align zapped data with kr_fever
  has_alive_var <- !is.null(survey_vars$alive) &&
    survey_vars$alive %in% names(dhs_kr_zapped)

  febrile_cond <- dhs_kr_zapped[[survey_vars$fever]] == 1 &
    dhs_kr_zapped[[survey_vars$age]] >= 0 &
    dhs_kr_zapped[[survey_vars$age]] <= 59
  if (has_alive_var) {
    febrile_cond <- febrile_cond & dhs_kr_zapped[[survey_vars$alive]] == 1
  }
  febrile_idx <- which(febrile_cond)

  # Determine antimalarial drug series
  if (length(raw_am_vars) > 0) {
    # Use raw-label-detected antimalarial variables
    drug_series <- raw_am_vars
    cli::cli_alert_info(
      "Antimalarial composite: {length(drug_series)} variables from raw labels"
    )
  } else {
    # Fallback: detect from zapped data (no raw labels available)
    act_used_h37 <- any(grepl("^h37", act_vars_used))
    ml13_vars <- grep("^ml13[a-z]+$", names(dhs_kr_zapped), value = TRUE)
    h37_vars  <- grep("^h37[a-z]+$", names(dhs_kr_zapped), value = TRUE)

    if (act_used_h37 && length(h37_vars) > 0) {
      drug_series <- h37_vars
      cli::cli_alert_info(
        "Antimalarial composite: h37 series (aligned with ACT h37 fallback)"
      )
    } else if (length(ml13_vars) > 0) {
      ml13_has_data <- any(sapply(ml13_vars, function(v) {
        any(dhs_kr_zapped[[v]] == 1, na.rm = TRUE)
      }))
      if (ml13_has_data) {
        drug_series <- ml13_vars
      } else if (length(h37_vars) > 0) {
        h37_has_data <- any(sapply(h37_vars, function(v) {
          any(dhs_kr_zapped[[v]] == 1, na.rm = TRUE)
        }))
        drug_series <- if (h37_has_data) h37_vars else ml13_vars
      } else {
        drug_series <- ml13_vars
      }
    } else if (length(h37_vars) > 0) {
      drug_series <- h37_vars
    } else {
      drug_series <- character(0)
    }
  }

  has_am_data <- length(drug_series) > 0
  if (has_am_data) {
    for (dvar in drug_series) {
      if (dvar %in% names(dhs_kr_zapped)) {
        kr_fever[[dvar]] <- dhs_kr_zapped[[dvar]][febrile_idx]
      }
      if (dvar %in% names(kr_fever)) {
        kr_fever[[dvar]][!kr_fever[[dvar]] %in% c(0, 1)] <- NA
      }
    }
    am_cols <- intersect(drug_series, names(kr_fever))
    drug_matrix <- as.matrix(kr_fever[, am_cols, drop = FALSE])
    kr_fever$received_antimalarial <- apply(drug_matrix, 1, function(row) {
      if (all(is.na(row))) return(NA_real_)
      if (any(row == 1, na.rm = TRUE)) return(1)
      return(0)
    })
    n_am <- sum(kr_fever$received_antimalarial == 1, na.rm = TRUE)
    cli::cli_alert_info(
      "Antimalarial composite: {n_am}/{nrow(kr_fever)} received any antimalarial"
    )
  } else {
    cli::cli_alert_warning("No antimalarial variables found")
    kr_fever$received_antimalarial <- NA_real_
  }

  # ---- 5. Add care-seeking behaviour (CSB) indicators ----

  kr_fever_csb <- tryCatch(
    .classify_csb_from_h32(kr_fever),
    error = function(e) {
      cli::cli_alert_warning("CSB classification failed: {e$message}")
      NULL
    }
  )

  has_csb <- !is.null(kr_fever_csb)
  if (has_csb) {
    kr_fever <- kr_fever_csb |>
      dplyr::mutate(
        csb_any_treatment      = csb_any,
        csb_no_treatment       = csb_none,
        csb_trained_provider   = csb_trained,
        csb_public_nochw       = as.numeric(has_public == 1 & has_chw == 0),
        csb_chw                = as.numeric(has_chw == 1),
        csb_private_formal_ind = as.numeric(has_private_formal == 1),
        csb_pharmacy           = as.numeric(has_pharmacy == 1),
        csb_private_informal   = as.numeric(has_private_informal == 1)
      )
    cli::cli_alert_success("CSB indicators created")
  }

  # ---- 6. Spatial join (GE + shapefile) or region grouping ----

  group_var <- NULL
  subnational_level <- NULL

  # Fallback: admin_level requested without GPS data → use v024
  if (is.null(region_var) && !is.null(admin_level) &&
      (is.null(gps_data) || is.null(shapefile))) {
    cli::cli_alert_warning(
      "{.var admin_level} = {.val {admin_level}} requested but {.var gps_data}/{.var shapefile} not provided. Falling back to survey region variable {.var v024} for adm1."
    )
    if ("v024" %in% names(dhs_kr)) {
      region_var <- "v024"
    } else {
      cli::cli_alert_warning("Variable {.var v024} not found in data. Cannot produce subnational estimates.")
    }
  }

  if (!is.null(region_var)) {
    # Get human-readable labels from haven; fall back to raw values
    region_values <- tryCatch({
      as.character(haven::as_factor(dhs_kr[[region_var]])[febrile_idx])
    }, error = function(e) {
      as.character(dhs_kr_zapped[[region_var]][febrile_idx])
    })

    kr_fever$region <- region_values

    group_var <- "region"
    subnational_level <- "adm1"
    cli::cli_alert_info(
      "Grouping by {.var {region_var}}: {paste(unique(kr_fever$region), collapse = ', ')}"
    )

  } else if (!is.null(gps_data) && !is.null(shapefile)) {
    kr_fever <- .spatial_join_ge(
      kr_fever    = kr_fever,
      gps_data    = gps_data,
      gps_vars    = gps_vars,
      shapefile   = shapefile,
      admin_level = admin_level,
      join_nearest = join_nearest
    )
    group_var <- attr(kr_fever, "group_var")
    subnational_level <- attr(kr_fever, "admin_level_label") %||%
      if (!is.null(admin_level)) admin_level[1] else "adm1"
  }

  # ---- 7. Get indicator dictionary and filter ----

  dict <- .act_wmr_conditions()

  if (!is.null(indicators)) {
    valid <- vapply(dict, function(d) d$indicator, character(1))
    bad <- setdiff(indicators, valid)
    if (length(bad) > 0) {
      cli::cli_warn("Unknown indicators ignored: {paste(bad, collapse = ', ')}")
    }
    dict <- dict[vapply(dict, function(d) d$indicator %in% indicators, logical(1))]
  }

  if (length(dict) == 0) {
    cli::cli_abort("No valid indicators to compute.")
  }

  # ---- 8. Compute each indicator ----

  options(survey.lonely.psu = "adjust")

  results <- purrr::map_dfr(dict, function(cond) {
    .compute_wmr_indicator(
      data              = kr_fever,
      condition         = cond,
      group_var         = group_var,
      subnational_level = subnational_level,
      ci_method         = ci_method
    )
  })

  # ---- 9. Add febrile RDT indicators if PR provided ----

  if (!is.null(dhs_pr)) {
    rdt_results <- .compute_rdt_indicators(
      kr_fever          = kr_fever,
      dhs_pr            = dhs_pr,
      group_var         = group_var,
      subnational_level = subnational_level,
      ci_method         = ci_method
    )
    if (nrow(rdt_results) > 0) {
      results <- dplyr::bind_rows(results, rdt_results)
    }
  }

  # ---- 10. Format and return as named list ----

  # Round proportions, clamp CIs
  results <- results |>
    dplyr::mutate(
      point  = round(point, 3),
      ci_l   = round(pmax(ci_l, 0, na.rm = TRUE), 3),
      ci_u   = round(pmin(ci_u, 1, na.rm = TRUE), 3),
      counts      = as.integer(counts),
      denominator = as.integer(denominator)
    )

  # Common metadata columns
  meta_cols <- tibble::tibble(
    survey_id    = survey_meta$survey_id,
    iso3        = survey_meta$iso3,
    iso2        = survey_meta$iso2,
    survey_type = survey_meta$survey_type,
    survey_year  = survey_meta$survey_year,
    adm0        = survey_meta$country_upper
  )

  # Determine geo_source once
  geo_src <- if (!is.null(gps_data) && !is.null(shapefile)) "gps" else "survey"

  # --- adm0 tab (national) ---
  national_rows <- results |>
    dplyr::filter(level == "adm0") |>
    dplyr::select(-level, -location)

  adm0_tbl <- dplyr::bind_cols(
    meta_cols[rep(1, nrow(national_rows)), ],
    tibble::tibble(type = "survey_weighted", geo_source = geo_src),
    national_rows
  ) |>
    tibble::as_tibble()

  out <- list(adm0 = adm0_tbl)

  # --- subnational tab(s) ---
  regional_rows <- results |>
    dplyr::filter(level != "adm0")

  if (nrow(regional_rows) > 0) {
    admin_col <- subnational_level %||% "adm1"

    sub_tbl <- dplyr::bind_cols(
      meta_cols[rep(1, nrow(regional_rows)), ],
      regional_rows |>
        dplyr::mutate(
          !!admin_col := toupper(location)
        ) |>
        dplyr::select(dplyr::all_of(admin_col), point, ci_l, ci_u,
                       counts, denominator,
                       indicator, indicator_code,
                       numerator_description, denominator_description)
    ) |>
      dplyr::mutate(
        type       = "survey_weighted",
        geo_source = geo_src,
        .after     = dplyr::all_of(admin_col)
      ) |>
      tibble::as_tibble()

    out[[admin_col]] <- sub_tbl
  }

  out
}


# =============================================================================
# WMR Indicator Dictionary
# =============================================================================

#' ACT WMR Indicator Dictionary
#'
#' Returns the full dictionary of WMR ACT indicators with metadata.
#' Each indicator measures the proportion of febrile U5 children receiving
#' ACT within a specific subpopulation.
#'
#' @return Tibble with columns: indicator, indicator_code, numerator_description,
#'   denominator_description, wmr_cascade_step, requires_csb, requires_am.
#'
#' @examples
#' act_wmr_dictionary()
#'
#' @export
act_wmr_dictionary <- function() {
  tibble::tribble(
    ~indicator,                          ~indicator_code,         ~numerator_description,                    ~denominator_description,                                                                              ~wmr_cascade_step, ~requires_csb, ~requires_am,
    "ACT_ANTIMALARIAL",                  "act_antimal",           "Received ACT treatment",                  "Febrile U5 who received any antimalarial",                                                            4L,                FALSE,         TRUE,
    "ACT_ANY_TREATMENT",                 "act_any_tx",            "Received ACT treatment",                  "Febrile U5 who sought any treatment (public or private) and received antimalarial",                    4L,                TRUE,          TRUE,
    "ACT_TRAINED_ANTIMALARIAL",          "act_trained",           "Received ACT treatment",                  "Febrile U5 who saw trained provider and received antimalarial",                                        4L,                TRUE,          TRUE,
    "ACT_PUBLIC_ANTIMALARIAL",           "act_pub",               "Received ACT treatment",                  "Febrile U5 who sought public sector care (incl. CHW) and received antimalarial",                       4L,                TRUE,          TRUE,
    "ACT_PUBLIC_NOCHW_ANTIMALARIAL",     "act_pub_nochw",         "Received ACT treatment",                  "Febrile U5 who sought public sector care (excl. CHW) and received antimalarial",                       4L,                TRUE,          TRUE,
    "ACT_PUBLIC_CHW_ANTIMALARIAL",       "act_chw",               "Received ACT treatment",                  "Febrile U5 who sought CHW care and received antimalarial",                                             4L,                TRUE,          TRUE,
    "ACT_PRIVATE_FORMAL_ANTIMALARIAL",   "act_priv_formal",       "Received ACT treatment",                  "Febrile U5 who sought private formal sector care and received antimalarial",                           4L,                TRUE,          TRUE,
    "ACT_PRIVATE_PHARMACY_ANTIMALARIAL", "act_priv_pharm",        "Received ACT treatment",                  "Febrile U5 who sought pharmacy care and received antimalarial",                                        4L,                TRUE,          TRUE,
    "ACT_PRIVATE_INFORMAL_ANTIMALARIAL", "act_priv_informal",     "Received ACT treatment",                  "Febrile U5 who sought private informal sector care and received antimalarial",                         4L,                TRUE,          TRUE,
    "ACT_PRIVATE_FORMAL_PHA_ANTIMALARIAL", "act_priv_form_pha",   "Received ACT treatment",                  "Febrile U5 who sought private formal or pharmacy care and received antimalarial",                      4L,                TRUE,          TRUE
  )
}


#' Internal: ACT WMR indicator conditions (with filter expressions)
#'
#' Returns a list of indicator specifications, each containing the filter
#' expression, indicator name, and description metadata.
#'
#' @return List of named lists, each with: indicator, filter_expr,
#'   num_desc, denom_desc.
#' @noRd
.act_wmr_conditions <- function() {
  list(
    list(
      indicator      = "ACT_ANTIMALARIAL",
      indicator_code = "act_antimal",
      filter_expr    = quote(received_antimalarial == 1),
      num_desc       = "Received ACT treatment",
      denom_desc     = "Febrile U5 who received any antimalarial"
    ),
    list(
      indicator      = "ACT_ANY_TREATMENT",
      indicator_code = "act_any_tx",
      filter_expr    = quote(csb_any_treatment == 1 & received_antimalarial == 1),
      num_desc       = "Received ACT treatment",
      denom_desc     = "Febrile U5 who sought any treatment (public or private) and received antimalarial"
    ),
    list(
      indicator      = "ACT_TRAINED_ANTIMALARIAL",
      indicator_code = "act_trained",
      filter_expr    = quote(csb_trained_provider == 1 & received_antimalarial == 1),
      num_desc       = "Received ACT treatment",
      denom_desc     = "Febrile U5 who saw trained provider and received antimalarial"
    ),
    list(
      indicator      = "ACT_PUBLIC_ANTIMALARIAL",
      indicator_code = "act_pub",
      filter_expr    = quote(csb_public == 1 & received_antimalarial == 1),
      num_desc       = "Received ACT treatment",
      denom_desc     = "Febrile U5 who sought public sector care (incl. CHW) and received antimalarial"
    ),
    list(
      indicator      = "ACT_PUBLIC_NOCHW_ANTIMALARIAL",
      indicator_code = "act_pub_nochw",
      filter_expr    = quote(csb_public_nochw == 1 & received_antimalarial == 1),
      num_desc       = "Received ACT treatment",
      denom_desc     = "Febrile U5 who sought public sector care (excl. CHW) and received antimalarial"
    ),
    list(
      indicator      = "ACT_PUBLIC_CHW_ANTIMALARIAL",
      indicator_code = "act_chw",
      filter_expr    = quote(csb_chw == 1 & received_antimalarial == 1),
      num_desc       = "Received ACT treatment",
      denom_desc     = "Febrile U5 who sought CHW care and received antimalarial"
    ),
    list(
      indicator      = "ACT_PRIVATE_FORMAL_ANTIMALARIAL",
      indicator_code = "act_priv_formal",
      filter_expr    = quote(csb_private_formal_ind == 1 & received_antimalarial == 1),
      num_desc       = "Received ACT treatment",
      denom_desc     = "Febrile U5 who sought private formal sector care and received antimalarial"
    ),
    list(
      indicator      = "ACT_PRIVATE_PHARMACY_ANTIMALARIAL",
      indicator_code = "act_priv_pharm",
      filter_expr    = quote(csb_pharmacy == 1 & received_antimalarial == 1),
      num_desc       = "Received ACT treatment",
      denom_desc     = "Febrile U5 who sought pharmacy care and received antimalarial"
    ),
    list(
      indicator      = "ACT_PRIVATE_INFORMAL_ANTIMALARIAL",
      indicator_code = "act_priv_informal",
      filter_expr    = quote(csb_private_informal == 1 & received_antimalarial == 1),
      num_desc       = "Received ACT treatment",
      denom_desc     = "Febrile U5 who sought private informal sector care and received antimalarial"
    ),
    list(
      indicator      = "ACT_PRIVATE_FORMAL_PHA_ANTIMALARIAL",
      indicator_code = "act_priv_form_pha",
      filter_expr    = quote(csb_private_formal_pha == 1 & received_antimalarial == 1),
      num_desc       = "Received ACT treatment",
      denom_desc     = "Febrile U5 who sought private formal or pharmacy care and received antimalarial"
    )
  )
}


# =============================================================================
# Survey metadata helper
# =============================================================================

#' Extract survey metadata from DHS KR dataset
#'
#' Derives survey_id, iso3, country name (UPPERCASE), survey type, and
#' survey year from v000/v007.
#'
#' @param dhs_kr DHS children's recode dataset.
#' @return Named list: survey_id, iso3, country_upper, survey_type, survey_year.
#' @noRd
.extract_survey_meta <- function(dhs_kr) {
  # Extract v000 (DHS country code, e.g. "TG7") and v007 (survey year)
  v000 <- NA_character_
  v007 <- NA_integer_

  if ("v000" %in% names(dhs_kr)) {
    v000_raw <- unique(as.character(haven::zap_labels(dhs_kr$v000)))
    v000 <- v000_raw[!is.na(v000_raw)][1]
  }
  if ("v007" %in% names(dhs_kr)) {
    v007_raw <- unique(as.integer(haven::zap_labels(dhs_kr$v007)))
    v007 <- v007_raw[!is.na(v007_raw)][1]
  }

  # Derive iso2 prefix from v000 (first 2 letters)
  iso2 <- if (!is.na(v000)) toupper(substr(v000, 1, 2)) else NA_character_

  # Derive iso3 and country name via countrycode
  iso3 <- NA_character_
  country_upper <- NA_character_
  if (!is.na(iso2)) {
    iso3 <- tryCatch(
      countrycode::countrycode(iso2, origin = "iso2c", destination = "iso3c"),
      warning = function(w) NA_character_
    )
    if (!is.na(iso3)) {
      country_name <- tryCatch(
        countrycode::countrycode(iso3, origin = "iso3c", destination = "country.name"),
        warning = function(w) NA_character_
      )
      country_upper <- if (!is.na(country_name)) toupper(country_name) else NA_character_
    }
  }

  # Detect survey type from v000 suffix (digit indicates phase; letter for MIS/AIS)
  survey_type <- "DHS"
  if (!is.na(v000) && nchar(v000) >= 3) {
    suffix <- substr(v000, 3, nchar(v000))
    if (grepl("[Ii]", suffix)) survey_type <- "MIS"
    else if (grepl("[Aa]", suffix)) survey_type <- "AIS"
  }

  # Build survey_id: iso2 + year + survey_type
  survey_year <- if (!is.na(v007)) as.integer(v007) else NA_integer_
  survey_id <- if (!is.na(iso2) && !is.na(survey_year)) {
    paste0(iso2, survey_year, survey_type)
  } else {
    NA_character_
  }

  list(
    survey_id      = survey_id,
    iso3          = iso3,
    iso2          = iso2,
    country_upper = country_upper,
    survey_type   = survey_type,
    survey_year    = survey_year
  )
}


# =============================================================================
# Formatting helpers
# =============================================================================

#' Convert SCREAMING_SNAKE indicator names to Title Case
#'
#' @param x Character string like "ACT_PUBLIC_CHW_ANTIMALARIAL".
#' @return "Act Public Chw Antimalarial"
#' @noRd
.indicator_title <- function(x) {
  x |>
    tolower() |>
    gsub("_", " ", x = _) |>
    gsub("(?<=^|\\s)([a-z])", "\\U\\1", x = _, perl = TRUE)
}


# =============================================================================
# Core computation helpers
# =============================================================================

#' Compute a single WMR indicator (national + optional regional)
#'
#' @param data Prepared febrile U5 dataset with ACT, CSB, antimalarial columns.
#' @param condition List with indicator, filter_expr, num_desc, denom_desc.
#' @param group_var Optional grouping variable name for regional estimates.
#' @param subnational_level Character admin level for grouped rows (e.g., "adm1").
#' @param ci_method CI method for svyciprop. Default: "logit".
#'
#' @return Tibble with columns: level, location, point, ci_l, ci_u, counts,
#'   indicator, numerator_description, denominator_description.
#' @noRd
.compute_wmr_indicator <- function(data, condition, group_var = NULL,
                                    subnational_level = NULL,
                                    ci_method = "logit") {

  # Apply condition filter
  filtered <- tryCatch(
    dplyr::filter(data, !!condition$filter_expr),
    error = function(e) tibble::tibble()
  )

  ind_title <- .indicator_title(condition$indicator)
  n_denom <- nrow(filtered)  # denominator (matches denominator_description)
  if (n_denom == 0) {
    return(tibble::tibble())
  }

  # --- Survey design ---
  use_strata <- dplyr::n_distinct(filtered$stratum_id) > 1

  svy <- if (use_strata) {
    survey::svydesign(
      ids = ~cluster_id, strata = ~stratum_id,
      weights = ~survey_weight, data = filtered, nest = TRUE
    )
  } else {
    survey::svydesign(
      ids = ~cluster_id, weights = ~survey_weight,
      data = filtered, nest = TRUE
    )
  }

  # --- National estimate ---
  n_numerator <- sum(filtered$has_act == 1, na.rm = TRUE)

  national <- tryCatch({
    est <- survey::svyciprop(~has_act, svy, method = ci_method, na.rm = TRUE)
    ci  <- stats::confint(est)
    tibble::tibble(
      level       = "adm0",
      location    = "National",
      point       = as.numeric(est),
      ci_l        = ci[1],
      ci_u        = ci[2],
      counts      = n_numerator,
      denominator = n_denom
    )
  }, error = function(e) {
    cli::cli_alert_warning("    {ind_title} national: {e$message}")
    tibble::tibble(
      level = "adm0", location = "National", point = NA_real_,
      ci_l = NA_real_, ci_u = NA_real_, counts = n_numerator,
      denominator = n_denom
    )
  })

  # --- Regional estimates ---
  regional <- tibble::tibble()

  sub_level <- subnational_level %||% "adm1"

  if (!is.null(group_var) && group_var %in% names(filtered)) {
    group_formula <- stats::as.formula(paste("~", group_var))

    regional <- tryCatch({
      by_result <- survey::svyby(
        ~has_act,
        by       = group_formula,
        design   = svy,
        FUN      = survey::svyciprop,
        vartype  = "ci",
        method   = ci_method,
        na.rm    = TRUE,
        keep.names = FALSE
      ) |>
        tibble::as_tibble()

      # Numerator per group (has_act == 1 in condition-filtered data)
      region_num <- filtered |>
        dplyr::group_by(.data[[group_var]]) |>
        dplyr::summarise(counts = sum(has_act == 1, na.rm = TRUE), .groups = "drop")

      # Denominator per group (condition-filtered subgroup size)
      region_denom <- filtered |>
        dplyr::group_by(.data[[group_var]]) |>
        dplyr::summarise(denominator = dplyr::n(), .groups = "drop")

      # Normalize CI column names (svyby names vary)
      names(by_result)[names(by_result) == "ci_l"] <- "ci_l.has_act"
      names(by_result)[names(by_result) == "ci_u"] <- "ci_u.has_act"

      by_result |>
        dplyr::rename(
          location = !!group_var,
          point    = has_act,
          ci_l     = `ci_l.has_act`,
          ci_u     = `ci_u.has_act`
        ) |>
        dplyr::mutate(location = as.character(location)) |>
        dplyr::left_join(
          region_num |>
            dplyr::mutate(location = as.character(.data[[group_var]])) |>
            dplyr::select(location, counts),
          by = "location"
        ) |>
        dplyr::left_join(
          region_denom |>
            dplyr::mutate(location = as.character(.data[[group_var]])) |>
            dplyr::select(location, denominator),
          by = "location"
        ) |>
        dplyr::mutate(level = sub_level) |>
        dplyr::select(level, location, point, ci_l, ci_u, counts, denominator)

    }, error = function(e) {
      cli::cli_alert_warning(
        "    {ind_title} by group: {e$message}"
      )
      tibble::tibble()
    })

    # region already contains text labels (set in section 6), no mapping needed
  }

  # --- Combine ---
  dplyr::bind_rows(national, regional) |>
    dplyr::mutate(
      indicator               = .indicator_title(condition$indicator),
      indicator_code          = condition$indicator_code,
      numerator_description   = condition$num_desc,
      denominator_description = condition$denom_desc
    )
}


# =============================================================================
# GE spatial join helper
# =============================================================================

#' Join GE GPS data to febrile dataset via shapefile
#'
#' @param kr_fever Febrile U5 data with cluster_id column.
#' @param gps_data DHS GE dataset.
#' @param gps_vars Named list: cluster, lat, lon.
#' @param shapefile sf object with admin boundaries.
#' @param admin_level Character vector of admin columns.
#' @param join_nearest Logical; nearest-feature fallback.
#'
#' @return kr_fever with admin columns added. Attribute "group_var" set.
#' @noRd
.spatial_join_ge <- function(kr_fever, gps_data, gps_vars, shapefile,
                              admin_level = NULL, join_nearest = TRUE) {

  if (!requireNamespace("sf", quietly = TRUE)) {
    cli::cli_abort("Package {.pkg sf} is required for spatial operations.")
  }

  cli::cli_alert_info("Joining GE coordinates to administrative boundaries")

  gps_clean <- gps_data |>
    dplyr::mutate(dplyr::across(dplyr::everything(), haven::zap_labels)) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), as.vector)) |>
    dplyr::select(
      cluster_id = !!gps_vars$cluster,
      lat        = !!gps_vars$lat,
      lon        = !!gps_vars$lon
    ) |>
    dplyr::distinct()

  kr_fever <- kr_fever |>
    dplyr::left_join(gps_clean, by = "cluster_id")

  clusters_sf <- kr_fever |>
    dplyr::select(cluster_id, lat, lon) |>
    dplyr::distinct() |>
    dplyr::filter(!is.na(lat), !is.na(lon)) |>
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)

  shapefile <- shapefile |>
    sf::st_transform(4326) |>
    sf::st_make_valid()

  if (is.null(admin_level)) {
    available_admins <- names(shapefile)[grep("^adm[0-9]+$", names(shapefile))]
    if (length(available_admins) == 0) {
      cli::cli_abort("No admin columns (adm0, adm1, ...) found in shapefile.")
    }
    admin_level <- available_admins
  }

  cluster_admin <- sf::st_join(
    clusters_sf,
    shapefile[, c(admin_level, "geometry")],
    join = sf::st_within,
    left = TRUE
  )

  if (join_nearest) {
    unmatched <- is.na(cluster_admin[[admin_level[1]]])
    if (any(unmatched)) {
      nearest_idx <- sf::st_nearest_feature(cluster_admin[unmatched, ], shapefile)
      for (col in admin_level) {
        if (col %in% names(shapefile)) {
          cluster_admin[unmatched, col] <- shapefile[[col]][nearest_idx]
        }
      }
      cli::cli_alert_info(
        "  {sum(unmatched)} cluster{?s} assigned via nearest-feature"
      )
    }
  }

  cluster_admin_df <- sf::st_drop_geometry(cluster_admin)
  kr_fever <- kr_fever |>
    dplyr::left_join(cluster_admin_df, by = "cluster_id")

  # Set grouping variable
  if (length(admin_level) > 1) {
    kr_fever$admin_class <- apply(
      kr_fever[, admin_level, drop = FALSE], 1, paste, collapse = "_"
    )
    grp <- "admin_class"
  } else {
    grp <- admin_level[1]
  }

  # Map admin codes to region/region_label for display
  kr_fever$region <- kr_fever[[grp]]
  kr_fever$region_label <- as.character(kr_fever$region)

  attr(kr_fever, "group_var") <- "region"
  attr(kr_fever, "admin_level_label") <- if (length(admin_level) == 1) {
    admin_level[1]
  } else {
    paste(admin_level, collapse = "_")
  }
  kr_fever
}


# =============================================================================
# Febrile RDT indicators (optional, when dhs_pr provided)
# =============================================================================

#' Compute febrile RDT indicators
#'
#' @param kr_fever Prepared febrile U5 data.
#' @param dhs_pr DHS Person Recode.
#' @param group_var Optional grouping variable.
#' @param subnational_level Admin level label for grouped rows.
#' @param ci_method CI method.
#'
#' @return Tibble in WMR long format with RDT indicators, or empty tibble.
#' @noRd
.compute_rdt_indicators <- function(kr_fever, dhs_pr, group_var = NULL,
                                     subnational_level = NULL,
                                     ci_method = "logit") {

  kr_merged <- .merge_kr_pr_febrile(kr_fever = kr_fever, dhs_pr = dhs_pr)
  if (is.null(kr_merged) || nrow(kr_merged) == 0) {
    return(tibble::tibble())
  }

  results <- tibble::tibble()

  # RDT positivity among febrile children
  rdt_pos_cond <- list(
    indicator      = "FEBRILE_RDT_POS",
    indicator_code = "feb_rdt_pos",
    filter_expr    = quote(TRUE),
    num_desc       = "RDT positive among febrile U5 with valid test",
    denom_desc     = "Febrile U5 with valid RDT result"
  )

  # For RDT positivity, the outcome is has_rdt_pos instead of has_act
  kr_merged_rdt <- kr_merged |>
    dplyr::mutate(has_act = has_rdt_pos)

  rdt_pos <- .compute_wmr_indicator(
    kr_merged_rdt, rdt_pos_cond, group_var, subnational_level, ci_method
  )
  results <- dplyr::bind_rows(results, rdt_pos)

  # ACT among RDT-positive febrile children
  kr_rdt_pos <- kr_merged |>
    dplyr::filter(has_rdt_pos == 1, !is.na(received_act)) |>
    dplyr::mutate(has_act = dplyr::if_else(received_act == 1, 1, 0,
                                            missing = NA_real_))

  if (nrow(kr_rdt_pos) > 0 && dplyr::n_distinct(kr_rdt_pos$cluster_id) > 1) {
    rdt_act_cond <- list(
      indicator      = "ACT_FEBRILE_RDT_POS",
      indicator_code = "act_feb_rdt_pos",
      filter_expr    = quote(TRUE),
      num_desc       = "Received ACT among febrile RDT-positive U5",
      denom_desc     = "Febrile U5 with positive RDT result"
    )
    rdt_act <- .compute_wmr_indicator(
      kr_rdt_pos, rdt_act_cond, group_var, subnational_level, ci_method
    )
    results <- dplyr::bind_rows(results, rdt_act)
  }

  results
}
