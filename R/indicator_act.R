# act indicator
#
# Merged from: dhs_calc_act.R dhs_calc_act_mbg.R dhs_helpers_act.R
# Contains the survey-weighted calc, MBG cluster-prep, and indicator-
# specific helpers for this family.

# ---- dhs_calc_act.R ----

#' Calculate ACT Treatment Indicators from DHS Data
#'
#' Computes the full set of (World Malaria Report) ACT treatment indicators
#' from DHS Children's Recode (KR) data. Returns survey-weighted proportions
#' with logit confidence intervals in standardized long format.
#'
#' @details
#' Computes up to 12 ACT indicators following DHS methodology. Each indicator
#' measures the proportion of febrile U5 children receiving ACT within a
#' specific subpopulation defined by care-seeking behaviour and antimalarial
#' receipt. See [act_dictionary()] for the full indicator list.
#'
#' The function uses three internal helpers:
#' \itemize{
#'   \item `.prepare_act_data()` for ACT variable detection
#'     and febrile U5 filtering
#'   \item `.classify_csb_from_h32()` for care-seeking behaviour classification
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
#'     \item `act`: ACT variable(s) (default: "ml13e";
#'       auto-detected from labels)
#'     \item `test`: Test-positive filter variable (default: "ml13a")
#'   }
#' @param region_var Optional column name for subnational
#'   grouping (e.g., "v024").
#' @param gps_data Optional DHS GE (Geographic) dataset
#'   with cluster coordinates.
#' @param gps_vars Named list for GE variables: cluster, lat, lon.
#' @param shapefile Optional sf object with administrative boundaries.
#' @param admin_level Character vector of admin columns from shapefile.
#' @param join_nearest Logical; if TRUE, assigns unmatched clusters to nearest
#'   admin unit. Default: TRUE.
#' @param dhs_pr Optional DHS Person Recode (PR) for febrile RDT indicators.
#' @param indicators Character vector of indicator names to compute. If NULL
#'   (default), computes all indicators from [act_dictionary()].
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
#'     \item `numerator`: Unweighted numerator count
#'       (has_act == 1 in filtered subgroup)
#'     \item `denominator`: Unweighted denominator count
#'       (condition-filtered subgroup size)
#'     \item `indicator`: Indicator name in Title Case
#'       (e.g., "Act Antimalarial")
#'     \item `indicator_code`: Short indicator code (e.g., "act_am")
#'     \item `numerator_description`: Description of numerator
#'     \item `denominator_description`: Description of denominator
#'     \item `denominator_code`: Short code for the denominator subpopulation
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
#' @seealso [act_dictionary()] for indicator definitions,
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
  # Fail fast on missing suggested dependencies
  .check_pkg(
    c("countrycode", "purrr", "stringr", "tibble"),
    reason = "for `calc_act_dhs()`"
  )

  # ---- 1. Input validation ----

  if (!is.data.frame(dhs_kr)) {
    cli::cli_abort("`dhs_kr` must be a data.frame or tibble.")
  }
  if (nrow(dhs_kr) == 0) {
    cli::cli_abort("`dhs_kr` is empty.")
  }

  needed <- unlist(survey_vars[c(
    "cluster", "weight", "stratum", "age", "fever"
  )])
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
        "ACT variables (from raw labels): \\
        {paste(raw_act_vars, collapse = ', ')}"
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
    "Under 5 with fever: {format(nrow(kr_fever), big.mark = ',')} children"
  )

  # ---- 4. Add antimalarial composite ----
  #
  # Follows gold standard (2d_act_indicators.R): detect antimalarial variables
  # from labels, excluding non-drug responses (don't know, other, etc.), then
  # build composite directly from kr_fever's own columns.

  am_exclude <- paste0(
    "don.t know|\\bdk\\b|\\bother\\b",
    "|\\bnone\\b|\\bno \\b|\\bmissing\\b"
  )

  if (length(raw_am_vars) > 0) {
    # Gold standard path: antimalarial vars detected from dhs_kr_raw labels
    antimalarial_vars <- raw_am_vars
  } else {
    # Fallback: detect from dhs_kr labels (same pattern as gold standard)
    antimalarial_vars <- character(0)
    ml13_candidates <- grep("^ml13[a-z]", names(dhs_kr), value = TRUE)
    for (v in ml13_candidates) {
      lbl <- attr(dhs_kr[[v]], "label")
      if (is.null(lbl) || !is.character(lbl) || length(lbl) != 1) next
      if (grepl(antimalarial_pattern, lbl, ignore.case = TRUE) &&
          !grepl(am_exclude, lbl, ignore.case = TRUE)) {
        antimalarial_vars <- c(antimalarial_vars, v)
      }
    }
    # Try h37 series if no ml13 labels found
    if (length(antimalarial_vars) == 0) {
      h37_candidates <- grep("^h37[a-z]", names(dhs_kr), value = TRUE)
      for (v in h37_candidates) {
        lbl <- attr(dhs_kr[[v]], "label")
        if (is.null(lbl) || !is.character(lbl) || length(lbl) != 1) next
        if (grepl(antimalarial_pattern, lbl, ignore.case = TRUE) &&
            !grepl(am_exclude, lbl, ignore.case = TRUE)) {
          antimalarial_vars <- c(antimalarial_vars, v)
        }
      }
    }
    # Last resort: standard DHS drug slots only (a-h), prefer series with data
    if (length(antimalarial_vars) == 0) {
      ml13_slots <- grep("^ml13[a-h]$", names(kr_fever), value = TRUE)
      h37_slots  <- grep("^h37[a-h]$", names(kr_fever), value = TRUE)

      ml13_has_pos <- length(ml13_slots) > 0 &&
        any(sapply(ml13_slots, function(v) {
          any(kr_fever[[v]] == 1, na.rm = TRUE)
        }))
      h37_has_pos <- length(h37_slots) > 0 &&
        any(sapply(h37_slots, function(v) {
          any(kr_fever[[v]] == 1, na.rm = TRUE)
        }))

      if (ml13_has_pos) {
        antimalarial_vars <- ml13_slots
      } else if (h37_has_pos) {
        antimalarial_vars <- h37_slots
      } else {
        antimalarial_vars <- c(ml13_slots, h37_slots)
      }
    }
  }

  # Build composite directly from kr_fever (already zapped by .prepare_act_data)
  am_cols <- intersect(antimalarial_vars, names(kr_fever))
  has_am_data <- length(am_cols) > 0

  if (has_am_data) {
    for (dvar in am_cols) {
      kr_fever[[dvar]][!kr_fever[[dvar]] %in% c(0, 1)] <- NA
    }
    drug_matrix <- as.matrix(kr_fever[, am_cols, drop = FALSE])
    kr_fever$received_antimalarial <- apply(drug_matrix, 1, function(row) {
      if (any(row == 1, na.rm = TRUE)) return(1)
      if (any(is.na(row))) return(NA_real_)
      return(0)
    })
    n_am <- sum(kr_fever$received_antimalarial == 1, na.rm = TRUE)
    cli::cli_alert_info(
      "Antimalarial composite ({length(am_cols)} vars: \
      {paste(am_cols, collapse = ', ')}): \
      {n_am}/{nrow(kr_fever)}"
    )
  } else {
    cli::cli_alert_warning("No antimalarial variables found")
    kr_fever$received_antimalarial <- NA_real_
  }

  # ---- 5. Add care-seeking behaviour (CSB) indicators ----

  # Detect CSB classification from haven labels (same approach as ACT detection)
  # Uses original dhs_kr (pre-zap) for label access; kr_fever has been zapped
  label_source <- dhs_kr_raw %||% dhs_kr
  csb_class <- .detect_csb_from_labels(label_source)

  kr_fever_csb <- tryCatch(
    .classify_csb_from_h32(kr_fever, classification = csb_class),
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

  # ---- 5b. Add malaria diagnostic test variable ----
  # h47 ("blood taken from finger/heel for testing") is the standard
  # DHS malaria diagnostic variable. ml1 means "blood taken for
  # testing" in KR (children) recode but "times took Fansidar" in
  # IR (women) recode. Reject ml1 only if its label indicates IPTp.
  malaria_dx_var <- survey_vars$malaria_dx %||% "h47"
  dx_resolved <- NULL

  if (malaria_dx_var %in% names(kr_fever)) {
    if (malaria_dx_var == "ml1") {
      ml1_lbl <- attr(dhs_kr[["ml1"]], "label") %||% ""
      is_iptp <- grepl(
        "fansidar|sp\\/fansidar|pregnancy|iptp|ipt\\b|dose.*preg",
        ml1_lbl, ignore.case = TRUE
      )
      if (!is_iptp) {
        dx_resolved <- "ml1"
      } else {
        cli::cli_alert_warning(
          "ml1 label is {.val {ml1_lbl}} -- IPTp variable, not malaria test"
        )
      }
    } else {
      dx_resolved <- malaria_dx_var
    }
  }

  # Fallback: h47 -> ml1 (with label validation)
  if (is.null(dx_resolved) && "h47" %in% names(kr_fever)) {
    dx_resolved <- "h47"
  }
  if (is.null(dx_resolved) && "ml1" %in% names(kr_fever)) {
    ml1_lbl <- attr(dhs_kr[["ml1"]], "label") %||% ""
    is_iptp <- grepl(
      "fansidar|sp\\/fansidar|pregnancy|iptp|ipt\\b|dose.*preg",
      ml1_lbl, ignore.case = TRUE
    )
    if (!is_iptp) dx_resolved <- "ml1"
  }

  if (!is.null(dx_resolved)) {
    kr_fever$had_test <- dplyr::if_else(
      kr_fever[[dx_resolved]] == 1, 1, 0, missing = NA_real_
    )
    n_tested <- sum(kr_fever$had_test == 1, na.rm = TRUE)
    cli::cli_alert_info(
      "Malaria diagnostic test ({.var {dx_resolved}}): {n_tested}/{nrow(kr_fever)}"
    )
  } else {
    cli::cli_alert_warning(
      "No valid malaria diagnostic variable found; MALARIA_DX indicators will be skipped"
    )
    kr_fever$had_test <- NA_real_
  }

  # ---- 6. Spatial join (GE + shapefile) or region grouping ----

  group_var <- NULL
  subnational_level <- NULL

  # Auto-fallback to v024 when no spatial parameters provided
  if (is.null(region_var) && is.null(gps_data) && is.null(shapefile)) {
    if ("v024" %in% names(dhs_kr)) {
      region_var <- "v024"
      cli::cli_alert_info(
        "No region_var/GPS/shapefile specified; defaulting to {.var v024} for adm1"
      )
    }
  } else if (is.null(region_var) && !is.null(admin_level) &&
             (is.null(gps_data) || is.null(shapefile))) {
    cli::cli_alert_warning(
      "{.var admin_level} = {.val {admin_level}} \\
      requested but {.var gps_data}/{.var shapefile} \\
      not provided. Falling back to {.var v024}."
    )
    if ("v024" %in% names(dhs_kr)) {
      region_var <- "v024"
    } else {
      cli::cli_alert_warning(
        "{.var v024} not found. Cannot produce \\
        subnational estimates."
      )
    }
  }

  # admin_hierarchy: list of list(group_var, level_name) for each admin level
  admin_hierarchy <- list()

  if (!is.null(region_var)) {
    # Use dhs_kr_raw labels if available (dhs_read may strip them)
    label_src <- if (!is.null(dhs_kr_raw) &&
                     region_var %in% names(dhs_kr_raw)) {
      dhs_kr_raw[[region_var]]
    } else {
      dhs_kr[[region_var]]
    }

    # Build lookup from full dataset, then map to febrile subset
    resolved_all <- .resolve_region_labels(label_src, region_var)
    raw_all <- as.character(as.vector(haven::zap_labels(label_src)))
    lookup <- stats::setNames(resolved_all, raw_all)
    febrile_raw <- as.character(kr_fever[[region_var]])
    kr_fever$region <- unname(lookup[febrile_raw])
    admin_hierarchy <- list(list(group_var = "region", level_name = "adm1"))
    geo_src <- "survey"

    cli::cli_alert_info(
      "Grouping by {.var {region_var}}: \\
      {paste(unique(kr_fever$region), collapse = ', ')}"
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
    geo_src <- "gps"

    # Build hierarchy from all admin levels attached by spatial join
    admin_lvls <- attr(kr_fever, "admin_levels") %||% character(0)
    # Drop adm0 from hierarchy: meta_cols$adm0 already carries the country.
    admin_lvls <- setdiff(admin_lvls, "adm0")
    if ("adm0" %in% names(kr_fever)) {
      kr_fever <- dplyr::select(kr_fever, -dplyr::any_of("adm0"))
    }
    for (lvl in admin_lvls) {
      admin_hierarchy <- c(admin_hierarchy, list(
        list(group_var = lvl, level_name = lvl)
      ))
    }
  }

  # ---- 7. Get indicator dictionary and filter ----

  dict <- .act_conditions()

  if (!is.null(indicators)) {
    valid <- vapply(dict, function(d) d$indicator, character(1))
    bad <- setdiff(indicators, valid)
    if (length(bad) > 0) {
      cli::cli_warn("Unknown indicators ignored: {paste(bad, collapse = ', ')}")
    }
    dict <- dict[vapply(
      dict,
      function(d) d$indicator %in% indicators,
      logical(1)
    )]
  }

  if (length(dict) == 0) {
    cli::cli_abort("No valid indicators to compute.")
  }

  # ---- 8. Compute each indicator (national + each admin level) ----

  options(survey.lonely.psu = "adjust")

  if (!exists("geo_src")) {
    geo_src <- if (!is.null(gps_data) && !is.null(shapefile)) "gps" else "survey"
  }

  # Common metadata columns
  meta_cols <- tibble::tibble(
    survey_id    = survey_meta$survey_id,
    iso3        = survey_meta$iso3,
    iso2        = survey_meta$iso2,
    survey_type = survey_meta$survey_type,
    survey_year  = survey_meta$survey_year,
    adm0        = survey_meta$country_upper
  )

  .round_results <- function(tbl) {
    tbl |>
      dplyr::mutate(
        point  = round(point, 3),
        ci_l   = round(pmax(ci_l, 0, na.rm = TRUE), 3),
        ci_u   = round(pmin(ci_u, 1, na.rm = TRUE), 3),
        numerator   = as.integer(numerator),
        denominator = as.integer(denominator)
      )
  }

  # --- adm0 (national, no group_var) ---
  national_results <- purrr::map_dfr(dict, function(cond) {
    .compute_dhs_indicator(
      data      = kr_fever,
      condition = cond,
      group_var = NULL,
      ci_method = ci_method
    )
  })

  # ---- 9. Add febrile RDT indicators if PR provided ----

  if (!is.null(dhs_pr)) {
    rdt_results <- .compute_rdt_indicators(
      kr_fever  = kr_fever,
      dhs_pr    = dhs_pr,
      group_var = NULL,
      ci_method = ci_method
    )
    if (nrow(rdt_results) > 0) {
      national_results <- dplyr::bind_rows(national_results, rdt_results)
    }
  }

  national_results <- .round_results(national_results)

  adm0_tbl <- dplyr::bind_cols(
    meta_cols[rep(1, nrow(national_results)), ],
    tibble::tibble(type = "survey_weighted", geo_source = geo_src),
    national_results |> dplyr::select(-level, -location)
  ) |>
    tibble::as_tibble()

  out <- list(adm0 = adm0_tbl)

  # --- subnational tabs (one per admin level) ---
  # Collect all admin level names for parent-column lookups
  all_level_names <- vapply(admin_hierarchy, `[[`, character(1), "level_name")

  for (i in seq_along(admin_hierarchy)) {
    ah <- admin_hierarchy[[i]]
    grp <- ah$group_var
    lvl_name <- ah$level_name

    sub_results <- purrr::map_dfr(dict, function(cond) {
      .compute_dhs_indicator(
        data              = kr_fever,
        condition         = cond,
        group_var         = grp,
        subnational_level = lvl_name,
        ci_method         = ci_method
      )
    })

    # RDT indicators at subnational level
    if (!is.null(dhs_pr)) {
      rdt_sub <- .compute_rdt_indicators(
        kr_fever          = kr_fever,
        dhs_pr            = dhs_pr,
        group_var         = grp,
        subnational_level = lvl_name,
        ci_method         = ci_method
      )
      if (nrow(rdt_sub) > 0) {
        sub_results <- dplyr::bind_rows(sub_results, rdt_sub)
      }
    }

    # Filter to regional rows only (drop the national duplicate)
    sub_results <- sub_results |>
      dplyr::filter(level != "adm0")

    if (nrow(sub_results) == 0) next

    sub_results <- .round_results(sub_results)

    # Add the current admin level column
    sub_results <- sub_results |>
      dplyr::mutate(!!lvl_name := toupper(location))

    # Add parent admin columns (e.g., adm1 in the adm2 tab)
    parent_levels <- all_level_names[seq_len(i - 1)]
    parent_cols_in_data <- intersect(parent_levels, names(kr_fever))
    if (length(parent_cols_in_data) > 0 && grp %in% names(kr_fever)) {
      # Build lookup: current level value -> parent level values
      parent_lookup <- kr_fever |>
        dplyr::select(dplyr::all_of(c(grp, parent_cols_in_data))) |>
        dplyr::mutate(dplyr::across(dplyr::everything(), ~toupper(as.character(.)))) |>
        dplyr::distinct()
      sub_results <- sub_results |>
        dplyr::left_join(parent_lookup, by = stats::setNames(grp, lvl_name))
    }

    # Select columns in proper order: parent admins, then current admin
    admin_cols <- c(parent_cols_in_data, lvl_name)
    admin_cols <- intersect(admin_cols, names(sub_results))

    sub_tbl <- dplyr::bind_cols(
      meta_cols[rep(1, nrow(sub_results)), ],
      sub_results |>
        dplyr::select(
          dplyr::all_of(admin_cols), point, ci_l, ci_u,
          numerator, denominator,
          indicator, indicator_code,
          numerator_description,
          denominator_description, denominator_code
        )
    ) |>
      dplyr::mutate(
        type       = "survey_weighted",
        geo_source = geo_src,
        .after     = dplyr::all_of(lvl_name)
      ) |>
      tibble::as_tibble()

    out[[lvl_name]] <- sub_tbl
  }

  out
}


# =============================================================================
# Indicator Dictionary
# =============================================================================

#' ACT Indicator Dictionary
#'
#' Returns the full dictionary of ACT indicators with metadata.
#' Each indicator measures the proportion of febrile U5 children receiving
#' ACT within a specific subpopulation.
#'
#' @return Tibble with columns: indicator,
#'   indicator_code, indicator_title, numerator_description,
#'   denominator_description, denominator_code,
#'   cascade_step, requires_csb, requires_am.
#'
#' @keywords internal
#' @noRd
act_dictionary <- function() {
  conds <- .act_conditions()
  tibble::tibble(
    indicator = vapply(conds, `[[`, character(1), "indicator"),
    indicator_code = vapply(conds, `[[`, character(1), "indicator_code"),
    indicator_title = vapply(conds, `[[`, character(1), "indicator_title"),
    outcome_var = vapply(conds, function(x) x$outcome_var %||% "has_act",
                          character(1)),
    numerator_description = vapply(conds, `[[`, character(1), "num_desc"),
    denominator_description = vapply(conds, `[[`, character(1), "denom_desc"),
    denominator_code = vapply(conds, `[[`, character(1), "denom_code"),
    data_level = "adm0"
  )
}


#' Internal: ACT indicator conditions (with filter expressions)
#'
#' Returns a list of indicator specifications, each containing the filter
#' expression, indicator name, and description metadata.
#'
#' @return List of named lists, each with: indicator, filter_expr,
#'   num_desc, denom_desc.
#' @noRd
.act_conditions <- function() {
  # Common strings
  num <- "Received ACT treatment"
  am1 <- " and received antimalarial"

  list(
    list(
      indicator      = "ACT",
      indicator_code = "act",
      indicator_title = "Use of ACTs among febrile U5",
      denom_code     = "feb_u5",
      filter_expr    = NULL,
      num_desc       = num,
      denom_desc     = "Under 5 with fever"
    ),
    list(
      indicator      = "ACT_TESTED",
      indicator_code = "act_tested",
      indicator_title = "Use of ACTs among test-positive febrile U5",
      denom_code     = "feb_u5_test_pos",
      filter_expr    = quote(test_positive == 1),
      num_desc       = num,
      denom_desc     = "Under 5 with fever who tested positive for malaria"
    ),
    list(
      indicator      = "ACT_CARE_SEEKERS",
      indicator_code = "act_care_seek",
      indicator_title = "Use of ACTs among care seekers",
      denom_code     = "feb_u5_any_tx",
      filter_expr    = quote(csb_any_treatment == 1),
      num_desc       = num,
      denom_desc     = "Under 5 with fever who sought any treatment (public or private)"
    ),
    list(
      indicator      = "ACT_ANTIMALARIAL",
      indicator_code = "act_am",
      indicator_title = "Use of ACTs among antimalarial recipients",
      denom_code     = "feb_u5_am",
      filter_expr    = quote(received_antimalarial == 1),
      num_desc       = num,
      denom_desc     = "Under 5 with fever who received any antimalarial"
    ),
    list(
      indicator      = "ACT_ANY_TREATMENT",
      indicator_code = "act_any_tx_am",
      indicator_title = paste0(
        "Use of ACTs among care seekers ",
        "treated with antimalarial"),
      denom_code     = "feb_u5_any_tx_am",
      filter_expr    = quote(
        csb_any_treatment == 1 &
          received_antimalarial == 1),
      num_desc       = num,
      denom_desc     = paste0(
        "Under 5 with fever who sought any treatment ",
        "(public or private) and received ",
        "antimalarial")
    ),
    list(
      indicator      = "ACT_TRAINED_ANTIMALARIAL",
      indicator_code = "act_trained_am",
      indicator_title = "Use of ACTs among trained provider + antimalarial",
      denom_code     = "feb_u5_trained_am",
      filter_expr    = quote(
        csb_trained_provider == 1 &
          received_antimalarial == 1),
      num_desc       = num,
      denom_desc     = paste0(
        "Under 5 with fever who saw trained ",
        "provider and received antimalarial")
    ),
    list(
      indicator      = "ACT_PUBLIC_ANTIMALARIAL",
      indicator_code = "act_pub_am",
      indicator_title = "Use of ACTs among public sector + antimalarial",
      denom_code     = "feb_u5_pub_am",
      filter_expr    = quote(csb_public == 1 & received_antimalarial == 1),
      num_desc       = num,
      denom_desc     = paste0(
        "Under 5 with fever who sought public ",
        "sector care (incl. CHW) and ",
        "received antimalarial")
    ),
    list(
      indicator      = "ACT_PUBLIC",
      indicator_code = "act_pub",
      indicator_title = "Use of ACTs among public sector care seekers",
      denom_code     = "feb_u5_pub",
      filter_expr    = quote(csb_public == 1),
      num_desc       = num,
      denom_desc     = paste0(
        "Under 5 with fever who sought public ",
        "sector care (incl. CHW)")
    ),
    list(
      indicator      = "ACT_PUBLIC_NOCHW_ANTIMALARIAL",
      indicator_code = "act_pub_nochw_am",
      indicator_title = paste0(
        "Use of ACTs among public sector ",
        "excl. CHW + antimalarial"),
      denom_code     = "feb_u5_pub_nochw_am",
      filter_expr    = quote(
        csb_public_nochw == 1 &
          received_antimalarial == 1),
      num_desc       = num,
      denom_desc     = paste0(
        "Under 5 with fever who sought public ",
        "sector care (excl. CHW) and ",
        "received antimalarial")
    ),
    list(
      indicator      = "ACT_PUBLIC_CHW_ANTIMALARIAL",
      indicator_code = "act_chw_am",
      indicator_title = "Use of ACTs among CHW + antimalarial",
      denom_code     = "feb_u5_chw_am",
      filter_expr    = quote(csb_chw == 1 & received_antimalarial == 1),
      num_desc       = num,
      denom_desc     = paste0(
        "Under 5 with fever who sought CHW care ",
        "and received antimalarial")
    ),
    list(
      indicator      = "ACT_PRIVATE_FORMAL_ANTIMALARIAL",
      indicator_code = "act_priv_formal_am",
      indicator_title = "Use of ACTs among private formal + antimalarial",
      denom_code     = "feb_u5_priv_formal_am",
      filter_expr    = quote(
        csb_private_formal_ind == 1 &
          received_antimalarial == 1),
      num_desc       = num,
      denom_desc     = paste0(
        "Under 5 with fever who sought private ",
        "formal sector care and ",
        "received antimalarial")
    ),
    list(
      indicator      = "ACT_PRIVATE_PHARMACY_ANTIMALARIAL",
      indicator_code = "act_priv_pharm_am",
      indicator_title = "Use of ACTs among pharmacy + antimalarial",
      denom_code     = "feb_u5_priv_pharm_am",
      filter_expr    = quote(csb_pharmacy == 1 & received_antimalarial == 1),
      num_desc       = num,
      denom_desc     = paste0(
        "Under 5 with fever who sought pharmacy ",
        "care and received antimalarial")
    ),
    list(
      indicator      = "ACT_PRIVATE_INFORMAL_ANTIMALARIAL",
      indicator_code = "act_priv_informal_am",
      indicator_title = "Use of ACTs among private informal + antimalarial",
      denom_code     = "feb_u5_priv_informal_am",
      filter_expr    = quote(
        csb_private_informal == 1 &
          received_antimalarial == 1),
      num_desc       = num,
      denom_desc     = paste0(
        "Under 5 with fever who sought private ",
        "informal sector care and ",
        "received antimalarial")
    ),
    list(
      indicator      = "ACT_PRIVATE_FORMAL_PHA_ANTIMALARIAL",
      indicator_code = "act_priv_form_pha_am",
      indicator_title = paste0(
        "Use of ACTs among private formal ",
        "or pharmacy + antimalarial"),
      denom_code     = "feb_u5_priv_form_pha_am",
      filter_expr    = quote(
        csb_private_formal_pha == 1 &
          received_antimalarial == 1),
      num_desc       = num,
      denom_desc     = paste0(
        "Under 5 with fever who sought private ",
        "formal or pharmacy care and ",
        "received antimalarial")
    ),
    # ACT_PRIVATE_ANTIMALARIAL (private sector + antimalarial)
    list(
      indicator      = "ACT_PRIVATE_ANTIMALARIAL",
      indicator_code = "act_priv_am",
      indicator_title = "Use of ACTs among private sector + antimalarial",
      denom_code     = "feb_u5_priv_am",
      filter_expr    = quote(
        csb_private == 1 & received_antimalarial == 1),
      num_desc       = num,
      denom_desc     = paste0(
        "Under 5 with fever who sought private ",
        "sector care and received antimalarial")
    ),

    # ====================================================================
    # ANTIMALARIAL indicators -- outcome: received_antimalarial
    # ====================================================================
    list(
      indicator      = "ANTIMALARIAL",
      indicator_code = "antimal",
      indicator_title = "Receive antimalarial",
      denom_code     = "feb_u5",
      filter_expr    = NULL,
      outcome_var    = "received_antimalarial",
      num_desc       = "Receive antimalarial",
      denom_desc     = "Under 5 with fever"
    ),
    list(
      indicator      = "ANTIMALARIAL_ANY_TREATMENT",
      indicator_code = "antimal_any_tx",
      indicator_title = "Receive antimalarial among care seekers",
      denom_code     = "feb_u5_any_tx",
      filter_expr    = quote(csb_any_treatment == 1),
      outcome_var    = "received_antimalarial",
      num_desc       = "Receive antimalarial",
      denom_desc     = paste0(
        "Under 5 with fever who sought any ",
        "treatment (public or private)")
    ),
    list(
      indicator      = "ANTIMALARIAL_TRAINED",
      indicator_code = "antimal_trained",
      indicator_title = "Receive antimalarial among trained provider",
      denom_code     = "feb_u5_trained",
      filter_expr    = quote(csb_trained_provider == 1),
      outcome_var    = "received_antimalarial",
      num_desc       = "Receive antimalarial",
      denom_desc     = paste0(
        "Under 5 with fever who sought treatment ",
        "in trained provider")
    ),
    list(
      indicator      = "ANTIMALARIAL_PUBLIC",
      indicator_code = "antimal_pub",
      indicator_title = "Receive antimalarial among public sector",
      denom_code     = "feb_u5_pub",
      filter_expr    = quote(csb_public == 1),
      outcome_var    = "received_antimalarial",
      num_desc       = "Receive antimalarial",
      denom_desc     = paste0(
        "Under 5 with fever who sought treatment ",
        "in public sector")
    ),
    list(
      indicator      = "ANTIMALARIAL_PUBLIC_NOCHW",
      indicator_code = "antimal_pub_nochw",
      indicator_title = paste0(
        "Receive antimalarial among public sector ",
        "excl. CHW"),
      denom_code     = "feb_u5_pub_nochw",
      filter_expr    = quote(csb_public_nochw == 1),
      outcome_var    = "received_antimalarial",
      num_desc       = "Receive antimalarial",
      denom_desc     = paste0(
        "Under 5 with fever who sought treatment ",
        "in public sector (excluding CHW)")
    ),
    list(
      indicator      = "ANTIMALARIAL_CHW",
      indicator_code = "antimal_chw",
      indicator_title = "Receive antimalarial among CHW",
      denom_code     = "feb_u5_chw",
      filter_expr    = quote(csb_chw == 1),
      outcome_var    = "received_antimalarial",
      num_desc       = "Receive antimalarial",
      denom_desc     = paste0(
        "Under 5 with fever who sought treatment ",
        "in CHW")
    ),
    list(
      indicator      = "ANTIMALARIAL_PRIVATE",
      indicator_code = "antimal_priv",
      indicator_title = "Receive antimalarial among private sector",
      denom_code     = "feb_u5_priv",
      filter_expr    = quote(csb_private == 1),
      outcome_var    = "received_antimalarial",
      num_desc       = "Receive antimalarial",
      denom_desc     = paste0(
        "Under 5 with fever who sought treatment ",
        "in private sector")
    ),
    list(
      indicator      = "ANTIMALARIAL_FORMAL",
      indicator_code = "antimal_formal",
      indicator_title = "Receive antimalarial among private formal",
      denom_code     = "feb_u5_priv_formal",
      filter_expr    = quote(csb_private_formal_ind == 1),
      outcome_var    = "received_antimalarial",
      num_desc       = "Receive antimalarial",
      denom_desc     = paste0(
        "Under 5 with fever who sought treatment ",
        "in private formal sector")
    ),
    list(
      indicator      = "ANTIMALARIAL_PHARMACY",
      indicator_code = "antimal_pharm",
      indicator_title = "Receive antimalarial among pharmacy",
      denom_code     = "feb_u5_pharm",
      filter_expr    = quote(csb_pharmacy == 1),
      outcome_var    = "received_antimalarial",
      num_desc       = "Receive antimalarial",
      denom_desc     = paste0(
        "Under 5 with fever who sought treatment ",
        "in pharmacy")
    ),
    list(
      indicator      = "ANTIMALARIAL_PRIVATE_INFORMAL",
      indicator_code = "antimal_priv_informal",
      indicator_title = "Receive antimalarial among private informal",
      denom_code     = "feb_u5_priv_informal",
      filter_expr    = quote(csb_private_informal == 1),
      outcome_var    = "received_antimalarial",
      num_desc       = "Receive antimalarial",
      denom_desc     = paste0(
        "Under 5 with fever who sought treatment ",
        "in private informal")
    ),
    list(
      indicator      = "ANTIMALARIAL_FORMAL_PHARMACY",
      indicator_code = "antimal_form_pharm",
      indicator_title = paste0(
        "Receive antimalarial among private ",
        "formal or pharmacy"),
      denom_code     = "feb_u5_priv_form_pha",
      filter_expr    = quote(csb_private_formal_pha == 1),
      outcome_var    = "received_antimalarial",
      num_desc       = "Receive antimalarial",
      denom_desc     = paste0(
        "Under 5 with fever who sought treatment ",
        "in private formal or pharmacy")
    ),

    # ====================================================================
    # MALARIA_DX indicators -- outcome: had_test (malaria diagnostic)
    # ====================================================================
    list(
      indicator      = "MALARIA_DX_ANTIMALARIAL",
      indicator_code = "mal_dx_am",
      indicator_title = "Get malaria diagnostic test among AM recipients",
      denom_code     = "feb_u5_am",
      filter_expr    = quote(received_antimalarial == 1),
      outcome_var    = "had_test",
      num_desc       = "Get malaria diagnostic test",
      denom_desc     = paste0(
        "Under 5 with fever who received ",
        "antimalarial")
    ),
    list(
      indicator      = "MALARIA_DX_PUBLIC_ANTIMALARIAL",
      indicator_code = "mal_dx_pub_am",
      indicator_title = paste0(
        "Get malaria diagnostic test among ",
        "public sector + AM"),
      denom_code     = "feb_u5_pub_am",
      filter_expr    = quote(
        csb_public == 1 & received_antimalarial == 1),
      outcome_var    = "had_test",
      num_desc       = "Get malaria diagnostic test",
      denom_desc     = paste0(
        "Under 5 with fever who sought treatment ",
        "in public sector and received antimalarial")
    ),
    list(
      indicator      = "MALARIA_DX_PUBLIC_NOCHW_ANTIMALARIAL",
      indicator_code = "mal_dx_pub_nochw_am",
      indicator_title = paste0(
        "Get malaria diagnostic test among ",
        "public excl. CHW + AM"),
      denom_code     = "feb_u5_pub_nochw_am",
      filter_expr    = quote(
        csb_public_nochw == 1 & received_antimalarial == 1),
      outcome_var    = "had_test",
      num_desc       = "Get malaria diagnostic test",
      denom_desc     = paste0(
        "Under 5 with fever who sought treatment ",
        "in public sector (excluding CHW) and ",
        "received antimalarial")
    ),
    list(
      indicator      = "MALARIA_DX_CSB_CHW_ANTIMALARIAL",
      indicator_code = "mal_dx_chw_am",
      indicator_title = paste0(
        "Get malaria diagnostic test among ",
        "CHW + AM"),
      denom_code     = "feb_u5_chw_am",
      filter_expr    = quote(
        csb_chw == 1 & received_antimalarial == 1),
      outcome_var    = "had_test",
      num_desc       = "Get malaria diagnostic test",
      denom_desc     = paste0(
        "Under 5 with fever who sought treatment ",
        "in CHW and received antimalarial")
    ),
    list(
      indicator      = "MALARIA_DX_CSB_PRIVATE_ANTIMALARIAL",
      indicator_code = "mal_dx_priv_am",
      indicator_title = paste0(
        "Get malaria diagnostic test among ",
        "private sector + AM"),
      denom_code     = "feb_u5_priv_am",
      filter_expr    = quote(
        csb_private == 1 & received_antimalarial == 1),
      outcome_var    = "had_test",
      num_desc       = "Get malaria diagnostic test",
      denom_desc     = paste0(
        "Under 5 with fever who sought treatment ",
        "in private sector and received antimalarial")
    ),
    list(
      indicator      = "MALARIA_DX_CSB_PRIVATE_FORMAL_ANTIMALARIAL",
      indicator_code = "mal_dx_priv_formal_am",
      indicator_title = paste0(
        "Get malaria diagnostic test among ",
        "private formal + AM"),
      denom_code     = "feb_u5_priv_formal_am",
      filter_expr    = quote(
        csb_private_formal_ind == 1 &
          received_antimalarial == 1),
      outcome_var    = "had_test",
      num_desc       = "Get malaria diagnostic test",
      denom_desc     = paste0(
        "Under 5 with fever who sought treatment ",
        "in private formal sector and ",
        "received antimalarial")
    ),
    list(
      indicator      = "MALARIA_DX_CSB_PHARMACY_ANTIMALARIAL",
      indicator_code = "mal_dx_pharm_am",
      indicator_title = paste0(
        "Get malaria diagnostic test among ",
        "pharmacy + AM"),
      denom_code     = "feb_u5_pharm_am",
      filter_expr    = quote(
        csb_pharmacy == 1 & received_antimalarial == 1),
      outcome_var    = "had_test",
      num_desc       = "Get malaria diagnostic test",
      denom_desc     = paste0(
        "Under 5 with fever who sought treatment ",
        "in pharmacy and received antimalarial")
    ),
    list(
      indicator      = "MALARIA_DX_PRIVATE_INFORMAL_ANTIMALARIAL",
      indicator_code = "mal_dx_priv_informal_am",
      indicator_title = paste0(
        "Get malaria diagnostic test among ",
        "private informal + AM"),
      denom_code     = "feb_u5_priv_informal_am",
      filter_expr    = quote(
        csb_private_informal == 1 &
          received_antimalarial == 1),
      outcome_var    = "had_test",
      num_desc       = "Get malaria diagnostic test",
      denom_desc     = paste0(
        "Under 5 with fever who sought treatment ",
        "in private informal and ",
        "received antimalarial")
    ),
    list(
      indicator      = "MALARIA_DX_CSB_PRIVATE_FORMAL_PHA_ANTIMALARIAL",
      indicator_code = "mal_dx_priv_form_pha_am",
      indicator_title = paste0(
        "Get malaria diagnostic test among ",
        "private formal or pharmacy + AM"),
      denom_code     = "feb_u5_priv_form_pha_am",
      filter_expr    = quote(
        csb_private_formal_pha == 1 &
          received_antimalarial == 1),
      outcome_var    = "had_test",
      num_desc       = "Get malaria diagnostic test",
      denom_desc     = paste0(
        "Under 5 with fever who sought treatment ",
        "in private formal or pharmacy and ",
        "received antimalarial")
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
      countrycode::countrycode(
        iso2, origin = "iso2c",
        destination = "iso3c"
      ),
      warning = function(w) NA_character_
    )
    if (!is.na(iso3)) {
      country_name <- tryCatch(
        countrycode::countrycode(
          iso3, origin = "iso3c",
          destination = "country.name"
        ),
        warning = function(w) NA_character_
      )
      country_upper <- if (!is.na(country_name)) {
        toupper(country_name)
      } else {
        NA_character_
      }
    }
  }

  # Detect survey type: priority order:
  # 1. survey_type column (from dhs_read() preprocessing)
  # 2. surveyid column (e.g. "TG2017MIS")
  # 3. v000 suffix fallback
  survey_type <- NA_character_
  survey_year <- if (!is.na(v007)) as.integer(v007) else NA_integer_

  # 1. Check for survey_type column directly (from dhs_read() or user-added)
  if ("survey_type" %in% names(dhs_kr)) {
    st <- unique(as.character(haven::zap_labels(dhs_kr$survey_type)))
    st <- st[!is.na(st) & nchar(st) > 0][1]
    if (!is.na(st)) {
      survey_type <- toupper(st)
    }
  }

  # 2. Check surveyid column (common in pre-processed DHS data)
  if (is.na(survey_type) && "surveyid" %in% names(dhs_kr)) {
    sid <- unique(as.character(haven::zap_labels(dhs_kr$surveyid)))
    sid <- sid[!is.na(sid)][1]
    if (!is.na(sid)) {
      if (grepl("MIS", sid, ignore.case = TRUE)) survey_type <- "MIS"
      else if (grepl("AIS", sid, ignore.case = TRUE)) survey_type <- "AIS"
      else if (grepl("DHS", sid, ignore.case = TRUE)) survey_type <- "DHS"
    }
  }

  # 3. Fall back to v000 suffix detection
  if (is.na(survey_type)) {
    survey_type <- "DHS"
    if (!is.na(v000) && nchar(v000) >= 3) {
      suffix <- substr(v000, 3, nchar(v000))
      if (grepl("[Ii]", suffix)) survey_type <- "MIS"
      else if (grepl("[Aa]", suffix)) survey_type <- "AIS"
    }
  }

  # Build survey_id: iso2 + year + survey_type
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

#' Compute a single indicator (national + optional regional)
#'
#' @param data Prepared febrile U5 dataset with ACT, CSB, antimalarial columns.
#' @param condition List with indicator, filter_expr, num_desc, denom_desc.
#' @param group_var Optional grouping variable name for regional estimates.
#' @param subnational_level Character admin level for
#'   grouped rows (e.g., "adm1").
#' @param ci_method CI method for svyciprop. Default: "logit".
#'
#' @return Tibble with columns: level, location, point, ci_l, ci_u, numerator,
#'   denominator, indicator, numerator_description, denominator_description.
#' @noRd
.compute_dhs_indicator <- function(data, condition, group_var = NULL,
                                    subnational_level = NULL,
                                    ci_method = "logit") {

  # Determine outcome variable (default: has_act for backwards compat)
  outcome_var <- condition$outcome_var %||% "has_act"

  # Apply condition filter (NULL means no filter -- use all data)
  if (is.null(condition$filter_expr)) {
    filtered <- data
  } else {
    filtered <- tryCatch(
      dplyr::filter(data, !!condition$filter_expr),
      error = function(e) tibble::tibble()
    )
  }

  ind_title <- condition$indicator_title

  # Check outcome var exists

  if (!outcome_var %in% names(filtered)) {
    return(tibble::tibble())
  }

  # Set the outcome column as .dhs_outcome for survey estimation
  filtered$.dhs_outcome <- filtered[[outcome_var]]

  # Drop rows with NA outcome
  filtered <- filtered[!is.na(filtered$.dhs_outcome), ]

  n_denom <- nrow(filtered)
  if (n_denom == 0) {
    return(tibble::tibble())
  }

  # --- Survey design ---
  n_clusters <- dplyr::n_distinct(filtered$cluster_id)
  use_strata <- dplyr::n_distinct(filtered$stratum_id) > 1

  # If only 1 cluster, can't estimate variance
  if (n_clusters < 2) {
    n_denom_w <- round(sum(filtered$survey_weight, na.rm = TRUE))
    n_numer_w <- round(sum(
      filtered$survey_weight * (filtered$.dhs_outcome == 1), na.rm = TRUE
    ))
    point_est <- n_numer_w / n_denom_w
    national <- tibble::tibble(
      level = "adm0", location = "National",
      point = point_est, ci_l = NA_real_, ci_u = NA_real_,
      numerator = n_numer_w, denominator = n_denom_w
    )
    return(
      national |>
        dplyr::mutate(
          indicator               = condition$indicator_title,
          indicator_code          = condition$indicator_code,
          numerator_description   = condition$num_desc,
          denominator_description = condition$denom_desc,
          denominator_code        = condition$denom_code
        )
    )
  }

  svy <- tryCatch({
    if (use_strata) {
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
  }, error = function(e) {
    survey::svydesign(
      ids = ~cluster_id, weights = ~survey_weight,
      data = filtered, nest = TRUE
    )
  })

  # --- National estimate ---
  # Weighted counts (matches format: numerator/denominator ~ point)
  n_denom_w <- round(sum(filtered$survey_weight, na.rm = TRUE))
  n_numer_w <- round(sum(
    filtered$survey_weight * (filtered$.dhs_outcome == 1), na.rm = TRUE
  ))

  national <- tryCatch({
    est <- survey::svyciprop(~.dhs_outcome, svy, method = ci_method,
                              na.rm = TRUE)
    ci  <- stats::confint(est)
    tibble::tibble(
      level       = "adm0",
      location    = "National",
      point       = as.numeric(est),
      ci_l        = ci[1],
      ci_u        = ci[2],
      numerator   = n_numer_w,
      denominator = n_denom_w
    )
  }, error = function(e) {
    cli::cli_alert_warning("    {ind_title} national: {e$message}")
    tibble::tibble(
      level = "adm0", location = "National", point = NA_real_,
      ci_l = NA_real_, ci_u = NA_real_, numerator = n_numer_w,
      denominator = n_denom_w
    )
  })

  # --- Regional estimates ---
  regional <- tibble::tibble()

  sub_level <- subnational_level %||% "adm1"

  if (!is.null(group_var) && group_var %in% names(filtered)) {
    group_formula <- stats::as.formula(paste("~", group_var))

    regional <- tryCatch({
      by_result <- survey::svyby(
        ~.dhs_outcome,
        by       = group_formula,
        design   = svy,
        FUN      = survey::svyciprop,
        vartype  = "ci",
        method   = ci_method,
        na.rm    = TRUE,
        keep.names = FALSE
      ) |>
        tibble::as_tibble()

      # Weighted numerator per group
      region_num <- filtered |>
        dplyr::group_by(.data[[group_var]]) |>
        dplyr::summarise(
          numerator = round(sum(
            survey_weight * (.dhs_outcome == 1), na.rm = TRUE
          )),
          .groups = "drop"
        )

      # Weighted denominator per group
      region_denom <- filtered |>
        dplyr::group_by(.data[[group_var]]) |>
        dplyr::summarise(
          denominator = round(sum(survey_weight, na.rm = TRUE)),
          .groups = "drop"
        )

      # Normalize CI column names (svyby names vary)
      names(by_result)[names(by_result) == "ci_l"] <- "ci_l..dhs_outcome"
      names(by_result)[names(by_result) == "ci_u"] <- "ci_u..dhs_outcome"

      by_result |>
        dplyr::rename(
          location = !!group_var,
          point    = .dhs_outcome,
          ci_l     = `ci_l..dhs_outcome`,
          ci_u     = `ci_u..dhs_outcome`
        ) |>
        dplyr::mutate(location = as.character(location)) |>
        dplyr::left_join(
          region_num |>
            dplyr::mutate(location = as.character(.data[[group_var]])) |>
            dplyr::select(location, numerator),
          by = "location"
        ) |>
        dplyr::left_join(
          region_denom |>
            dplyr::mutate(location = as.character(.data[[group_var]])) |>
            dplyr::select(location, denominator),
          by = "location"
        ) |>
        dplyr::mutate(level = sub_level) |>
        dplyr::select(level, location, point, ci_l, ci_u, numerator,
                       denominator)

    }, error = function(e) {
      cli::cli_alert_warning(
        "    {ind_title} by group: {e$message}"
      )
      tibble::tibble()
    })
  }

  # --- Combine ---
  dplyr::bind_rows(national, regional) |>
    dplyr::mutate(
      indicator               = condition$indicator_title,
      indicator_code          = condition$indicator_code,
      numerator_description   = condition$num_desc,
      denominator_description = condition$denom_desc,
      denominator_code        = condition$denom_code
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

  # Auto-detect available admin columns in shapefile
  available_admins <- sort(
    names(shapefile)[grep("^adm[0-9]+$", names(shapefile))]
  )
  if (length(available_admins) == 0) {
    cli::cli_abort("No admin columns (adm0, adm1, ...) found in shapefile.")
  }

  # Expand admin_level to include all intermediate levels

  # e.g., admin_level = "adm2" -> include adm1, adm2 (all levels in shapefile up to adm2)
  if (!is.null(admin_level) && length(admin_level) == 1) {
    target_num <- as.integer(sub("^adm", "", admin_level))
    all_cols <- available_admins[
      as.integer(sub("^adm", "", available_admins)) >= 1 &
        as.integer(sub("^adm", "", available_admins)) <= target_num
    ]
    admin_level <- if (length(all_cols) > 0) all_cols else admin_level
  } else if (is.null(admin_level)) {
    # Use all available admin levels (excluding adm0 which is national)
    admin_level <- available_admins[available_admins != "adm0"]
    if (length(admin_level) == 0) admin_level <- available_admins
  }

  # Ensure requested columns exist in shapefile
  admin_level <- intersect(admin_level, available_admins)
  if (length(admin_level) == 0) {
    cli::cli_abort("Requested admin columns not found in shapefile.")
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
      nearest_idx <- sf::st_nearest_feature(
        cluster_admin[unmatched, ], shapefile
      )
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

  # Drop any pre-existing admin columns from kr_fever so the join below
  # doesn't suffix-rename them (e.g., adm1.x / adm1.y), which would leave
  # `kr_fever[[col]]` as NULL in the uppercase loop.
  existing_admin_cols <- intersect(admin_level, names(kr_fever))
  if (length(existing_admin_cols) > 0) {
    kr_fever <- dplyr::select(
      kr_fever, -dplyr::all_of(existing_admin_cols)
    )
  }

  kr_fever <- kr_fever |>
    dplyr::left_join(cluster_admin_df, by = "cluster_id")

  # UPPERCASE all admin columns
  for (col in admin_level) {
    if (col %in% names(kr_fever)) {
      kr_fever[[col]] <- toupper(as.character(kr_fever[[col]]))
    }
  }

  # Store the full admin hierarchy as attribute (e.g., c("adm1", "adm2"))
  attr(kr_fever, "admin_levels") <- admin_level
  attr(kr_fever, "geo_source") <- "gps"
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
#' @return Tibble in standardized long format with RDT indicators, or empty tibble.
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
    denom_code     = "feb_u5_rdt_valid",
    filter_expr    = quote(TRUE),
    num_desc       = "RDT positive among under 5 with fever with valid test",
    denom_desc     = "Under 5 with fever with valid RDT result"
  )

  # For RDT positivity, the outcome is has_rdt_pos instead of has_act
  kr_merged_rdt <- kr_merged |>
    dplyr::mutate(has_act = has_rdt_pos)

  rdt_pos <- .compute_dhs_indicator(
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
      denom_code     = "feb_u5_rdt_pos",
      filter_expr    = quote(TRUE),
      num_desc       = "Received ACT among RDT-positive under 5 with fever",
      denom_desc     = "Under 5 with fever with positive RDT result"
    )
    rdt_act <- .compute_dhs_indicator(
      kr_rdt_pos, rdt_act_cond, group_var, subnational_level, ci_method
    )
    results <- dplyr::bind_rows(results, rdt_act)
  }

  results
}


# ---- dhs_calc_act_mbg.R ----

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
#'   \code{"act_pub"} and \code{"act_among_am"} are also
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
#'   indicators = c("act_pub_am", "act_trained_am", "antimal_chw",
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
    act_public = "act_pub",
    act_among_am = "act_any_tx_am",
    act_antimal = "act_am",
    act_any_tx = "act_any_tx_am",
    act_trained = "act_trained_am",
    act_pub_nochw = "act_pub_nochw_am",
    act_chw = "act_chw_am",
    act_priv = "act_priv_am",
    act_priv_formal = "act_priv_formal_am",
    act_priv_pharm = "act_priv_pharm_am",
    act_priv_informal = "act_priv_informal_am",
    act_priv_form_pha = "act_priv_form_pha_am"
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
    "act_pub", "act_tested",
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
    "act_pub" %in% indicators
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
      .classify_csb_from_h32(enriched, classification = csb_class),
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
    # h47 ("blood taken from finger/heel for testing") is the standard
    # DHS malaria diagnostic variable. ml1 means "blood taken for
    # testing" in the KR (children) recode, but in the IR (women)
    # recode it means "times took Fansidar during pregnancy" (IPTp).
    # Since labels from parquet files sometimes carry the wrong recode
    # label, we validate ml1 by rejecting known-bad labels (IPTp/
    # pregnancy patterns) rather than requiring a positive match.
    dx_var <- NULL
    if ("h47" %in% names(enriched)) {
      dx_var <- "h47"
    } else if ("ml1" %in% names(enriched)) {
      ml1_label <- attr(dhs_kr[["ml1"]], "label") %||% ""
      is_iptp <- grepl(
        "fansidar|sp\\/fansidar|pregnancy|iptp|ipt\\b|dose.*preg",
        ml1_label, ignore.case = TRUE
      )
      if (is_iptp) {
        cli::cli_alert_warning(
          "ml1 label is {.val {ml1_label}} \\
          {cli::symbol$em_dash} IPTp variable, not malaria \\
          diagnostic; mal_dx indicators will be skipped"
        )
      } else {
        dx_var <- "ml1"
      }
    }

    if (!is.null(dx_var)) {
      enriched$had_test <- as.integer(
        !is.na(enriched[[dx_var]]) &
          enriched[[dx_var]] == 1
      )
      cli::cli_alert_info(
        "Enriched with malaria diagnostic ({dx_var}): \\
        {sum(enriched$had_test == 1, na.rm = TRUE)} \\
        tested"
      )
    } else {
      cli::cli_alert_warning(
        "No valid malaria diagnostic variable found \\
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
        "No data for {.val {spec$name}} -- skipping"
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

  # ---- Legacy act_pub (no AM filter) ----

  if ("act_pub" %in% indicators &&
      needs_csb &&
      !"act_pub" %in% names(results)) {
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
        result_name = "act_pub"
      )
      if (!is.null(dt)) {
        results[["act_pub"]] <- dt
      }
    }
  }

  # ---- Special: act_tested ----

  has_test_var <- !is.null(survey_vars$test) && survey_vars$test %in% names(dhs_kr)
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
      name = "act_am",
      outcome = "received_act",
      csb_filter = NULL,
      am_filter = TRUE
    ),
    list(
      name = "act_any_tx_am",
      outcome = "received_act",
      csb_filter = "csb_any",
      am_filter = TRUE
    ),
    list(
      name = "act_trained_am",
      outcome = "received_act",
      csb_filter = "csb_trained",
      am_filter = TRUE
    ),
    list(
      name = "act_pub_am",
      outcome = "received_act",
      csb_filter = "csb_public",
      am_filter = TRUE
    ),
    list(
      name = "act_pub_nochw_am",
      outcome = "received_act",
      csb_filter = "csb_public_nochw",
      am_filter = TRUE
    ),
    list(
      name = "act_chw_am",
      outcome = "received_act",
      csb_filter = "csb_chw",
      am_filter = TRUE
    ),
    list(
      name = "act_priv_am",
      outcome = "received_act",
      csb_filter = "csb_private",
      am_filter = TRUE
    ),
    list(
      name = "act_priv_formal_am",
      outcome = "received_act",
      csb_filter = "csb_private_formal_ind",
      am_filter = TRUE
    ),
    list(
      name = "act_priv_pharm_am",
      outcome = "received_act",
      csb_filter = "csb_pharmacy",
      am_filter = TRUE
    ),
    list(
      name = "act_priv_informal_am",
      outcome = "received_act",
      csb_filter = "csb_private_informal",
      am_filter = TRUE
    ),
    list(
      name = "act_priv_form_pha_am",
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

  # ---- Detect antimalarial variables using label-based filtering ----
  # Must match DHS path (calc_act_dhs): only include variables whose labels
  # contain actual drug names. Non-drug ml13 variables (e.g. "Don't know",
  # "No treatment", response-quality codes) are excluded because their
  # labels don't match the drug-name pattern. This prevents inflating the
  # antimalarial composite.
  antimalarial_pattern <- paste0(
    "antimalarial|fansidar|chloroquine|amodiaquine|quinine|",
    "artemether|artesunate|dihydroartemis|artemisinin|coartem|",
    "\\bsp\\b|\\bcta\\b|\\bact\\b|mefloquine|piperaquine|lumefantrine"
  )

  ml13_candidates <- grep(
    "^ml13[a-z]+$",
    names(dhs_kr), value = TRUE
  )
  h37_candidates <- grep(
    "^h37[a-z]+$",
    names(dhs_kr), value = TRUE
  )

  # Align with ACT variable series
  act_vars_used <- attr(
    enriched, "act_vars_used"
  ) %||%
    attr(enriched, "act_var_used") %||%
    (survey_vars$act %||% "ml13e")
  act_used_h37 <- any(grepl("^h37", act_vars_used))

  # Label-based detection from original dhs_kr (pre-zap).
  # Matches DHS primary path: include if label contains a drug name.
  .detect_am_from_labels <- function(candidates) {
    matched <- character(0)
    for (v in candidates) {
      lbl <- attr(dhs_kr[[v]], "label")
      if (is.null(lbl) || !is.character(lbl) ||
          length(lbl) != 1) next
      if (grepl(antimalarial_pattern, lbl,
                ignore.case = TRUE)) {
        matched <- c(matched, v)
      }
    }
    matched
  }

  drug_series <- character(0)

  if (act_used_h37 && length(h37_candidates) > 0) {
    drug_series <- .detect_am_from_labels(h37_candidates)
    if (length(drug_series) == 0) {
      # No labels -- fall back to standard h37 slots
      drug_series <- grep(
        "^h37[a-h]$",
        names(dhs_kr_zapped), value = TRUE
      )
    }
    cli::cli_alert_info(
      "Antimalarial composite using h37 series \\
      (aligned with ACT h37 fallback)"
    )
  } else {
    # Try ml13 labels first
    drug_series <- .detect_am_from_labels(ml13_candidates)

    # If no labels matched, try h37 labels
    if (length(drug_series) == 0) {
      drug_series <- .detect_am_from_labels(h37_candidates)
    }

    # Last resort: standard DHS drug slots (a-h)
    if (length(drug_series) == 0) {
      ml13_slots <- grep(
        "^ml13[a-h]$",
        names(dhs_kr_zapped), value = TRUE
      )
      h37_slots <- grep(
        "^h37[a-h]$",
        names(dhs_kr_zapped), value = TRUE
      )

      ml13_has_pos <- length(ml13_slots) > 0 &&
        any(sapply(ml13_slots, function(v) {
          any(dhs_kr_zapped[[v]] == 1, na.rm = TRUE)
        }))
      h37_has_pos <- length(h37_slots) > 0 &&
        any(sapply(h37_slots, function(v) {
          any(dhs_kr_zapped[[v]] == 1, na.rm = TRUE)
        }))

      if (ml13_has_pos) {
        drug_series <- ml13_slots
      } else if (h37_has_pos) {
        drug_series <- h37_slots
      } else {
        drug_series <- c(ml13_slots, h37_slots)
      }
    }
  }

  if (length(drug_series) == 0) {
    cli::cli_alert_warning(
      "No antimalarial variables found -- \\
      antimalarial indicators will be skipped"
    )
    return(enriched)
  }

  # Use .row_id from enriched to map back to original rows
  # (febrile_idx was already computed by .prepare_act_data())
  if (!".row_id" %in% names(enriched)) {
    cli::cli_abort(
      "Internal error: enriched data missing .row_id column. \\
      This column is required to map drug variables from the original dataset."
    )
  }

  febrile_idx <- enriched[[".row_id"]]

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
      if (any(row == 1, na.rm = TRUE)) return(1)
      if (any(is.na(row))) return(NA_real_)
      return(0)
    }
  )

  n_am <- sum(enriched$received_antimalarial == 1,
               na.rm = TRUE)
  cli::cli_alert_info(
    "Antimalarial composite ({length(drug_series)} vars: \\
    {paste(drug_series, collapse = ', ')}): \\
    {n_am}/{nrow(enriched)}"
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


# ---- dhs_helpers_act.R ----

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


