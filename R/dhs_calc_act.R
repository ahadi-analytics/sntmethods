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
#'   \item [.prepare_act_data()] for ACT variable detection
#'     and febrile U5 filtering
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
#'     \item `numerator`: Unweighted numerator count
#'       (has_act == 1 in filtered subgroup)
#'     \item `denominator`: Unweighted denominator count
#'       (condition-filtered subgroup size)
#'     \item `indicator`: Indicator name in Title Case
#'       (e.g., "Act Antimalarial")
#'     \item `indicator_code`: Short indicator code (e.g., "act_antimal")
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
      if (all(is.na(row))) return(NA_real_)
      if (any(row == 1, na.rm = TRUE)) return(1)
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
    .classify_csb_from_h32(kr_fever, csb_classification = csb_class),
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
    # Get human-readable labels from haven; fall back to kr_fever's own values
    region_values <- tryCatch({
      lbls <- as.character(haven::as_factor(dhs_kr[[region_var]]))
      raw_vals <- as.vector(haven::zap_labels(dhs_kr[[region_var]]))
      febrile_raw <- kr_fever[[region_var]]
      lookup <- stats::setNames(lbls, raw_vals)
      unname(lookup[as.character(febrile_raw)])
    }, error = function(e) {
      as.character(kr_fever[[region_var]])
    })

    kr_fever$region <- toupper(region_values)
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
    for (lvl in admin_lvls) {
      admin_hierarchy <- c(admin_hierarchy, list(
        list(group_var = lvl, level_name = lvl)
      ))
    }
  }

  # ---- 7. Get indicator dictionary and filter ----

  dict <- .act_wmr_conditions()

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
    .compute_wmr_indicator(
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
      .compute_wmr_indicator(
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
      # Build lookup: current level value → parent level values
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
# WMR Indicator Dictionary
# =============================================================================

#' ACT WMR Indicator Dictionary
#'
#' Returns the full dictionary of WMR ACT indicators with metadata.
#' Each indicator measures the proportion of febrile U5 children receiving
#' ACT within a specific subpopulation.
#'
#' @return Tibble with columns: indicator,
#'   indicator_code, indicator_title, numerator_description,
#'   denominator_description, denominator_code,
#'   wmr_cascade_step, requires_csb, requires_am.
#'
#' @examples
#' act_wmr_dictionary()
#'
#' @export
act_wmr_dictionary <- function() {
  n <- 11L
  tibble::tibble(
    indicator = c(
      "ACT_CARE_SEEKERS",
      "ACT_ANTIMALARIAL",
      "ACT_ANY_TREATMENT",
      "ACT_TRAINED_ANTIMALARIAL",
      "ACT_PUBLIC_ANTIMALARIAL",
      "ACT_PUBLIC_NOCHW_ANTIMALARIAL",
      "ACT_PUBLIC_CHW_ANTIMALARIAL",
      "ACT_PRIVATE_FORMAL_ANTIMALARIAL",
      "ACT_PRIVATE_PHARMACY_ANTIMALARIAL",
      "ACT_PRIVATE_INFORMAL_ANTIMALARIAL",
      "ACT_PRIVATE_FORMAL_PHA_ANTIMALARIAL"
    ),
    indicator_code = c(
      "act_care_seek",
      "act_antimal", "act_any_tx",
      "act_trained", "act_pub",
      "act_pub_nochw", "act_chw",
      "act_priv_formal", "act_priv_pharm",
      "act_priv_informal", "act_priv_form_pha"
    ),
    indicator_title = c(
      "Use of ACTs among care seekers",
      "Use of ACTs among antimalarial recipients",
      "Use of ACTs among care seekers treated with antimalarial",
      "Use of ACTs among trained provider + antimalarial",
      "Use of ACTs among public sector + antimalarial",
      "Use of ACTs among public sector excl. CHW + antimalarial",
      "Use of ACTs among CHW + antimalarial",
      "Use of ACTs among private formal + antimalarial",
      "Use of ACTs among pharmacy + antimalarial",
      "Use of ACTs among private informal + antimalarial",
      "Use of ACTs among private formal or pharmacy + antimalarial"
    ),
    numerator_description = rep(
      "Received ACT treatment", n
    ),
    denominator_description = c(
      "Under 5 with fever who sought any treatment (public or private)",
      "Under 5 with fever who received any antimalarial",
      paste0(
        "Under 5 with fever who sought any treatment ",
        "(public or private) and received ",
        "antimalarial"
      ),
      paste0(
        "Under 5 with fever who saw trained provider ",
        "and received antimalarial"
      ),
      paste0(
        "Under 5 with fever who sought public sector ",
        "care (incl. CHW) and received ",
        "antimalarial"
      ),
      paste0(
        "Under 5 with fever who sought public sector ",
        "care (excl. CHW) and received ",
        "antimalarial"
      ),
      paste0(
        "Under 5 with fever who sought CHW care and ",
        "received antimalarial"
      ),
      paste0(
        "Under 5 with fever who sought private formal ",
        "sector care and received antimalarial"
      ),
      paste0(
        "Under 5 with fever who sought pharmacy care ",
        "and received antimalarial"
      ),
      paste0(
        "Under 5 with fever who sought private ",
        "informal sector care and received ",
        "antimalarial"
      ),
      paste0(
        "Under 5 with fever who sought private formal ",
        "or pharmacy care and received ",
        "antimalarial"
      )
    ),
    denominator_code = c(
      "feb_u5_any_tx",
      "feb_u5_am", "feb_u5_any_tx_am",
      "feb_u5_trained_am", "feb_u5_pub_am",
      "feb_u5_pub_nochw_am", "feb_u5_chw_am",
      "feb_u5_priv_formal_am",
      "feb_u5_priv_pharm_am",
      "feb_u5_priv_informal_am",
      "feb_u5_priv_form_pha_am"
    ),
    wmr_cascade_step = c(3L, rep(4L, 10L)),
    requires_csb = c(
      TRUE, FALSE, TRUE, TRUE, TRUE, TRUE,
      TRUE, TRUE, TRUE, TRUE, TRUE
    ),
    requires_am = c(
      FALSE, rep(TRUE, 10L)
    )
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
  # Common strings
  num <- "Received ACT treatment"
  am1 <- " and received antimalarial"

  list(
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
      indicator_code = "act_antimal",
      indicator_title = "Use of ACTs among antimalarial recipients",
      denom_code     = "feb_u5_am",
      filter_expr    = quote(received_antimalarial == 1),
      num_desc       = num,
      denom_desc     = "Under 5 with fever who received any antimalarial"
    ),
    list(
      indicator      = "ACT_ANY_TREATMENT",
      indicator_code = "act_any_tx",
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
      indicator_code = "act_trained",
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
      indicator_code = "act_pub",
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
      indicator      = "ACT_PUBLIC_NOCHW_ANTIMALARIAL",
      indicator_code = "act_pub_nochw",
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
      indicator_code = "act_chw",
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
      indicator_code = "act_priv_formal",
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
      indicator_code = "act_priv_pharm",
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
      indicator_code = "act_priv_informal",
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
      indicator_code = "act_priv_form_pha",
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

#' Compute a single WMR indicator (national + optional regional)
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
.compute_wmr_indicator <- function(data, condition, group_var = NULL,
                                    subnational_level = NULL,
                                    ci_method = "logit") {

  # Apply condition filter
  filtered <- tryCatch(
    dplyr::filter(data, !!condition$filter_expr),
    error = function(e) tibble::tibble()
  )

  ind_title <- condition$indicator_title
  n_denom <- nrow(filtered)  # denominator (matches denominator_description)
  if (n_denom == 0) {
    # Drop indicators with 0 observations — no estimate possible
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
  # Weighted counts (matches WMR format: numerator/denominator ≈ point)
  n_denom_w <- round(sum(filtered$survey_weight, na.rm = TRUE))
  n_numer_w <- round(sum(
    filtered$survey_weight * (filtered$has_act == 1), na.rm = TRUE
  ))

  national <- tryCatch({
    est <- survey::svyciprop(~has_act, svy, method = ci_method, na.rm = TRUE)
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

      # Weighted numerator per group
      region_num <- filtered |>
        dplyr::group_by(.data[[group_var]]) |>
        dplyr::summarise(
          numerator = round(sum(
            survey_weight * (has_act == 1), na.rm = TRUE
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
        dplyr::select(level, location, point, ci_l, ci_u, numerator, denominator)

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

  # e.g., admin_level = "adm2" → include adm1, adm2 (all levels in shapefile up to adm2)
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
  kr_fever <- kr_fever |>
    dplyr::left_join(cluster_admin_df, by = "cluster_id")

  # UPPERCASE all admin columns
  for (col in admin_level) {
    kr_fever[[col]] <- toupper(as.character(kr_fever[[col]]))
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
    denom_code     = "feb_u5_rdt_valid",
    filter_expr    = quote(TRUE),
    num_desc       = "RDT positive among under 5 with fever with valid test",
    denom_desc     = "Under 5 with fever with valid RDT result"
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
      denom_code     = "feb_u5_rdt_pos",
      filter_expr    = quote(TRUE),
      num_desc       = "Received ACT among RDT-positive under 5 with fever",
      denom_desc     = "Under 5 with fever with positive RDT result"
    )
    rdt_act <- .compute_wmr_indicator(
      kr_rdt_pos, rdt_act_cond, group_var, subnational_level, ci_method
    )
    results <- dplyr::bind_rows(results, rdt_act)
  }

  results
}
