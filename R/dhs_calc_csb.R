#' Calculate Care-Seeking Behavior from DHS Data ( Methodology)
#'
#' Estimates care-seeking behavior for febrile children under 5 using
#' the WHO World Malaria Report methodology with overlapping indicators.
#'
#' @details
#' Methodology: \url{https://github.com/ahadi-analytics/sntmethods/blob/master/inst/methods/csb_dhs.yml}
#'
#' @param dhs_kr DHS children's recode (KR) dataset in tidy format
#'   (data.frame or tibble).
#' @param survey_vars Named list mapping DHS variable names. Required keys:
#'   \itemize{
#'     \item `cluster`: Cluster ID (default: "v021")
#'     \item `weight`: Survey weight (default: "v005")
#'     \item `stratum`: Stratum variable (default: "v022")
#'     \item `age`: Child's age in months (default: "hw1")
#'     \item `fever`: Had fever in last 2 weeks (default: "h22")
#'     \item `alive`: Child survival status (default: "b5"). NOTE: 
#'       methodology assumes filtering to living children (b5 == 1) is done
#'       upstream. This function does NOT filter by alive status.
#'   }
#' @param csb_classification Data frame specifying h32 variable to CSB category
#'   mapping. Must have columns:
#'   \itemize{
#'     \item `variable`: h32 variable name (e.g., "h32a", "h32j")
#'     \item `csb`: Category - one of: "public", "chw", "private_formal",
#'       "private_informal", "pharmacy"
#'   }
#'   If NULL, uses default classification. See Details for category
#'   meanings.
#' @param source_config **Deprecated**. Use `csb_classification` instead.
#'   Legacy parameter for backwards compatibility. Named list with:
#'   \itemize{
#'     \item `public`: Character vector of h32 codes for public sector
#'     \item `private`: Character vector of h32 codes for private sector
#'     \item `excluded`: Character vector of h32 codes to exclude
#'   }
#' @param region_var Optional column name (character string) in `dhs_kr` to
#'   use as the grouping variable (e.g., `"v024"` for region). When provided,
#'   this takes precedence over GPS/shapefile-based grouping and the column
#'   appears first in the output.
#' @param gps_data Optional DHS GPS dataset with cluster coordinates.
#' @param gps_vars Named list for GPS variables (cluster, lat, lon).
#' @param shapefile Optional sf object with administrative boundaries.
#' @param admin_level Character vector of admin columns from shapefile
#'   (e.g., c("adm1", "adm2")). If NULL, uses existing admin variables
#'   in data.
#' @param join_nearest Logical; if TRUE, assigns clusters outside polygons
#'   to nearest admin unit.
#'
#' @return Tibble with CSB estimates by administrative level, with
#'   confidence intervals and sample sizes.
#'
#' @details
#' This function implements the WHO World Malaria Report methodology
#' for care-seeking behavior analysis.
#'
#' \strong{5-Category Classification:}
#' \itemize{
#'   \item `public`: Government health facilities (hospitals, health
#'     centers, posts)
#'   \item `chw`: Community health workers (often NGO sector in DHS-8)
#'   \item `private_formal`: Private hospitals, clinics, and doctors
#'   \item `private_informal`: Traditional practitioners and other
#'     informal sources
#'   \item `pharmacy`: Pharmacies and drug shops
#' }
#'
#' \strong{Derived Indicators (OVERLAPPING):}
#' These indicators are NOT mutually exclusive. A child can be counted in
#' multiple categories if they visited multiple source types.
#' \itemize{
#'   \item `dhs_csb_public`: Public sector care (public OR chw)
#'   \item `dhs_csb_private`: Any private sector care (private_formal OR
#'     private_informal OR pharmacy)
#'   \item `dhs_csb_trained`: Trained provider (public OR private_formal
#'     OR pharmacy)
#'   \item `dhs_csb_any`: Any treatment sought (public OR private)
#'   \item `dhs_csb_none`: No treatment sought (NOT any)
#' }
#'
#' \strong{Important:} Only `dhs_csb_any` and `dhs_csb_none` are mutually
#' exclusive. The equation `dhs_csb_any + dhs_csb_none = 1` always holds.
#' However, `dhs_csb_public + dhs_csb_private + dhs_csb_none` may exceed
#' 1.0 when children visit both public and private sources.
#'
#' @references
#' WHO. World Malaria Report. Geneva: World Health Organization.
#' \url{https://www.who.int/teams/global-malaria-programme/reports}
#'
#' @export
calc_csb_dhs_core <- function(
  dhs_kr,
  survey_vars = list(
    cluster = "v021",
    weight = "v005",
    stratum = "v022",
    age = "hw1",
    fever = "h22",
    alive = "b5"
  ),
  csb_classification = NULL,
  source_config = NULL,
  region_var = NULL,
  gps_data = NULL,
  gps_vars = list(
    cluster = "DHSCLUST",
    lat = "LATNUM",
    lon = "LONGNUM"
  ),
  shapefile = NULL,
  admin_level = NULL,
  join_nearest = TRUE
) {
  # ---- 1. Input validation ---------------------------------------------------

  if (!is.data.frame(dhs_kr)) {
    cli::cli_abort("`dhs_kr` must be a data.frame or tibble.")
  }

  if (nrow(dhs_kr) == 0) {
    cli::cli_abort("`dhs_kr` is empty.")
  }

  # Check required survey variables
  needed <- unlist(
    survey_vars[
      c(
        "cluster",
        "weight",
        "stratum",
        "age",
        "fever"
      )
    ]
  )

  missing_vars <- setdiff(needed, names(dhs_kr))

  if (length(missing_vars) > 0) {
    cli::cli_abort(
      c(
        "Required variables not found: {.var {missing_vars}}",
        "i" = "Check your survey_vars mapping"
      )
    )
  }

  # Validate region_var if provided
  if (!is.null(region_var)) {
    if (!is.character(region_var) || length(region_var) != 1) {
      cli::cli_abort(
        "`region_var` must be a single character string."
      )
    }
    if (!region_var %in% names(dhs_kr)) {
      cli::cli_abort(
        c(
          "Column {.var {region_var}} not found in `dhs_kr`.",
          "i" = "Available columns: {.var {head(names(dhs_kr), 10)}}..."
        )
      )
    }
    if (!is.null(gps_data) || !is.null(shapefile)) {
      cli::cli_alert_warning(
        "`region_var` provided with GPS/shapefile; `region_var` takes precedence"
      )
    }
  }

  # Auto-detect available h32 treatment source variables
  # Pattern includes digits for DHS-8 NGO sector variables (h32na, h32nb, etc.)
  available_h32 <- grep("^h32[a-z0-9]+$", names(dhs_kr), value = TRUE)

  if (length(available_h32) == 0) {
    cli::cli_abort(
      c(
        "No h32 treatment-seeking variables found in data.",
        "i" = "Expected variables like h32a, h32b, h32c, etc.",
        "i" = "Check your DHS KR data includes care-seeking variables"
      )
    )
  }

  cli::cli_alert_info(
    "Detected {length(available_h32)} h32 source variables"
  )

  # Warn if any detected h32 variables are not in the (default) classification
  {
    ref_class <- if (!is.null(csb_classification)) csb_classification else .default_csb_classification()
    expected_h32 <- ref_class$variable
    unexpected_h32 <- setdiff(available_h32, expected_h32)
    if (length(unexpected_h32) > 0) {
      cli::cli_warn(
        "Detected h32 variables not in standard classification: {paste(unexpected_h32, collapse = ', ')}. These may be country-specific non-standard slots. Check that the default classification is appropriate or supply a custom csb_classification."
      )
    }
  }

  # ---- 2. Handle classification parameter ------------------------------------

  if (!is.null(source_config) && is.null(csb_classification)) {
    cli::cli_alert_warning(
      "source_config is deprecated. Use csb_classification instead."
    )
    csb_classification <- .convert_source_config(source_config)
    cli::cli_alert_info(
      "Converted source_config to csb_classification format"
    )
  } else if (is.null(csb_classification)) {
    # Auto-detect from haven labels (same approach as ACT detection)
    csb_classification <- .detect_csb_from_labels(dhs_kr)
    if (nrow(csb_classification) == 0) {
      csb_classification <- .default_csb_classification()
    }
  }

  # Validate csb_classification
  if (!is.data.frame(csb_classification)) {
    cli::cli_abort("`csb_classification` must be a data.frame")
  }
  if (!all(c("variable", "csb") %in% names(csb_classification))) {
    cli::cli_abort(
      c(
        "`csb_classification` must have columns: variable, csb",
        "i" = "Got columns: {.var {names(csb_classification)}}"
      )
    )
  }

  # Filter to variables present in data
  csb_classification <- csb_classification |>
    dplyr::filter(variable %in% available_h32)

  if (nrow(csb_classification) == 0) {
    cli::cli_abort(
      c(
        "No h32 variables from csb_classification found in data.",
        "i" = "Available h32 variables: {.var {available_h32}}"
      )
    )
  }

  # ---- 3. Prepare base dataset -----------------------------------------------

  kr_fever <- .prepare_csb_data(
    dhs_kr = dhs_kr,
    survey_vars = survey_vars,
    csb_classification = csb_classification,
    include_survey_vars = TRUE
  )

  # Add aliases needed by indicator conditions (.csb_conditions() outcome_var)
  # Granular columns (csb_public_nochw, csb_chw, etc.) are already created
  # by .classify_csb_from_h32() -- don't redefine them here
  kr_fever <- kr_fever |>
    dplyr::mutate(
      csb_any_treatment      = csb_any,
      csb_no_treatment       = csb_none,
      csb_trained_provider   = csb_trained
    )

  cli::cli_alert_success(
    "Under 5 with fever: {format(nrow(kr_fever), big.mark = ',')} children"
  )

  # ---- 4. Extract survey metadata -------------------------------------------

  survey_meta <- .extract_survey_meta(dhs_kr)

  # ---- 5. Region grouping or spatial join -----------------------------------

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
  }

  # admin_hierarchy: list of list(group_var, level_name) for each admin level
  admin_hierarchy <- list()

  if (!is.null(region_var)) {
    # Resolve labels from raw data (pre-zap) then map onto kr_fever
    resolved_all <- .resolve_region_labels(dhs_kr[[region_var]], region_var)
    raw_all <- as.character(as.vector(haven::zap_labels(dhs_kr[[region_var]])))
    lookup <- stats::setNames(resolved_all, raw_all)
    febrile_raw <- as.character(kr_fever[[region_var]])
    kr_fever$region <- unname(lookup[febrile_raw])
    admin_hierarchy <- list(list(group_var = "region", level_name = "adm1"))
    geo_src <- "survey"

    cli::cli_alert_info(
      "Grouping by `{region_var}`: {paste(unique(kr_fever$region), collapse = ', ')}"
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

    admin_lvls <- attr(kr_fever, "admin_levels") %||% character(0)
    for (lvl in admin_lvls) {
      admin_hierarchy <- c(admin_hierarchy, list(
        list(group_var = lvl, level_name = lvl)
      ))
    }
  }

  if (!exists("geo_src")) geo_src <- "survey"

  # ---- 6. Compute each CSB indicator (national + each admin level) ----------

  options(survey.lonely.psu = "adjust")

  dict <- .csb_conditions()

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
    .compute_csb_indicator(
      data      = kr_fever,
      condition = cond,
      group_var = NULL,
      ci_method = "logit"
    )
  }) |> .round_results()

  adm0_tbl <- dplyr::bind_cols(
    meta_cols[rep(1, nrow(national_results)), ],
    tibble::tibble(type = "survey_weighted", geo_source = geo_src),
    national_results |> dplyr::select(-level, -location)
  ) |>
    tibble::as_tibble()

  out <- list(adm0 = adm0_tbl)

  # --- subnational tabs (one per admin level) ---
  all_level_names <- vapply(admin_hierarchy, `[[`, character(1), "level_name")

  for (i in seq_along(admin_hierarchy)) {
    ah <- admin_hierarchy[[i]]
    grp <- ah$group_var
    lvl_name <- ah$level_name

    sub_results <- purrr::map_dfr(dict, function(cond) {
      .compute_csb_indicator(
        data              = kr_fever,
        condition         = cond,
        group_var         = grp,
        subnational_level = lvl_name,
        ci_method         = "logit"
      )
    })

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
      parent_lookup <- kr_fever |>
        dplyr::select(dplyr::all_of(c(grp, parent_cols_in_data))) |>
        dplyr::mutate(dplyr::across(dplyr::everything(), ~toupper(as.character(.)))) |>
        dplyr::distinct()
      sub_results <- sub_results |>
        dplyr::left_join(parent_lookup, by = stats::setNames(grp, lvl_name))
    }

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

#' Default CSB classification
#'
#' Returns the default WHO World Malaria Report classification mapping
#' h32 variables to CSB categories.
#'
#' @return Data frame with columns: variable, csb
#' @noRd
.default_csb_classification <- function() {
  data.frame(
    variable = c(
      # Public sector (government facilities)
      "h32a", "h32b", "h32c", "h32d", "h32e", "h32f", "h32g", "h32h", "h32i",
      # CHW / NGO sector (DHS-8 added h32na-h32ne)
      "h32na", "h32nb", "h32nc", "h32nd", "h32ne",
      # Private formal (private hospitals, clinics, doctors)
      "h32j", "h32k", "h32l", "h32m",
      # Private informal (traditional practitioners, other)
      "h32s", "h32t", "h32u",
      # Pharmacy (pharmacies, drug shops)
      "h32n", "h32o", "h32p", "h32q", "h32r"
    ),
    csb = c(
      rep("public", 9),
      rep("chw", 5),
      rep("private_formal", 4),
      rep("private_informal", 3),
      rep("pharmacy", 5)
    ),
    stringsAsFactors = FALSE
  )
}

#' Convert legacy source_config to csb_classification
#'
#' @param source_config Named list with public, private, excluded vectors
#' @return Data frame with columns: variable, csb
#' @noRd
.convert_source_config <- function(source_config) {
  result <- dplyr::bind_rows(
    if (length(source_config$public) > 0) {
      data.frame(
        variable = source_config$public,
        csb = "public",
        stringsAsFactors = FALSE
      )
    },
    if (length(source_config$private) > 0) {
      # In legacy mode, all private sources are treated as private_formal
      # This maintains backwards compatibility with the old behavior
      data.frame(
        variable = source_config$private,
        csb = "private_formal",
        stringsAsFactors = FALSE
      )
    }
  )

  if (nrow(result) == 0) {
    cli::cli_abort(
      "source_config must have at least one public or private source"
    )
  }

  result
}

#' Internal: CSB indicator conditions (with filter expressions)
#'
#' Returns a list of indicator specifications following the same pattern
#' as `.act_conditions()`. Each condition specifies the outcome variable,
#' indicator metadata, and description.
#'
#' @return List of named lists, each with: indicator, indicator_code,
#'   indicator_title, outcome_var, num_desc, denom_desc, denom_code.
#' @noRd
.csb_conditions <- function() {
  denom <- "Under 5 with fever"
  list(
    list(
      indicator      = "CSB_ANY",
      indicator_code = "csb_any",
      indicator_title = "Care seeking among under 5 with fever",
      denom_code     = "feb_u5",
      outcome_var    = "csb_any_treatment",
      num_desc       = "Sought any treatment (public or private)",
      denom_desc     = denom
    ),
    list(
      indicator      = "CSB_NONE",
      indicator_code = "csb_none",
      indicator_title = "No care seeking among under 5 with fever",
      denom_code     = "feb_u5",
      outcome_var    = "csb_no_treatment",
      num_desc       = "Did not seek any treatment",
      denom_desc     = denom
    ),
    list(
      indicator      = "CSB_PUBLIC",
      indicator_code = "csb_public",
      indicator_title = "Public sector care seeking among under 5 with fever",
      denom_code     = "feb_u5",
      outcome_var    = "csb_public",
      num_desc       = "Sought public sector care (incl. CHW)",
      denom_desc     = denom
    ),
    list(
      indicator      = "CSB_PUBLIC_NOCHW",
      indicator_code = "csb_pub_nochw",
      indicator_title = "Public sector excl. CHW care seeking among under 5 with fever",
      denom_code     = "feb_u5",
      outcome_var    = "csb_public_nochw",
      num_desc       = "Sought public sector care (excl. CHW)",
      denom_desc     = denom
    ),
    list(
      indicator      = "CSB_CHW",
      indicator_code = "csb_chw",
      indicator_title = "CHW care seeking among under 5 with fever",
      denom_code     = "feb_u5",
      outcome_var    = "csb_chw",
      num_desc       = "Sought CHW care",
      denom_desc     = denom
    ),
    list(
      indicator      = "CSB_PRIVATE",
      indicator_code = "csb_private",
      indicator_title = "Private sector care seeking among under 5 with fever",
      denom_code     = "feb_u5",
      outcome_var    = "csb_private",
      num_desc       = "Sought any private sector care",
      denom_desc     = denom
    ),
    list(
      indicator      = "CSB_PRIVATE_FORMAL",
      indicator_code = "csb_priv_formal",
      indicator_title = "Private formal care seeking among under 5 with fever",
      denom_code     = "feb_u5",
      outcome_var    = "csb_private_formal_ind",
      num_desc       = "Sought private formal sector care",
      denom_desc     = denom
    ),
    list(
      indicator      = "CSB_PHARMACY",
      indicator_code = "csb_pharmacy",
      indicator_title = "Pharmacy care seeking among under 5 with fever",
      denom_code     = "feb_u5",
      outcome_var    = "csb_pharmacy",
      num_desc       = "Sought pharmacy care",
      denom_desc     = denom
    ),
    list(
      indicator      = "CSB_PRIVATE_INFORMAL",
      indicator_code = "csb_priv_informal",
      indicator_title = "Private informal care seeking among under 5 with fever",
      denom_code     = "feb_u5",
      outcome_var    = "csb_private_informal",
      num_desc       = "Sought private informal sector care",
      denom_desc     = denom
    ),
    list(
      indicator      = "CSB_PRIVATE_FORMAL_PHA",
      indicator_code = "csb_priv_form_pha",
      indicator_title = "Private formal or pharmacy care seeking among under 5 with fever",
      denom_code     = "feb_u5",
      outcome_var    = "csb_private_formal_pha",
      num_desc       = "Sought private formal or pharmacy care",
      denom_desc     = denom
    ),
    list(
      indicator      = "CSB_TRAINED",
      indicator_code = "csb_trained",
      indicator_title = "Trained provider care seeking among under 5 with fever",
      denom_code     = "feb_u5",
      outcome_var    = "csb_trained_provider",
      num_desc       = "Sought care from trained provider",
      denom_desc     = denom
    )
  )
}


#' CSB Indicator Dictionary
#'
#' Returns the full dictionary of CSB indicators with metadata.
#' Each indicator measures the proportion of febrile U5 children seeking
#' care from a specific source type.
#'
#' @return Tibble with columns: indicator, indicator_code, indicator_title,
#'   numerator_description, denominator_description, denominator_code.
#'
#' @examples
#' csb_dictionary()
#'
#' @export
csb_dictionary <- function() {
  conds <- .csb_conditions()
  tibble::tibble(
    indicator               = vapply(conds, `[[`, character(1), "indicator"),
    indicator_code          = vapply(conds, `[[`, character(1), "indicator_code"),
    indicator_title         = vapply(conds, `[[`, character(1), "indicator_title"),
    numerator_description   = vapply(conds, `[[`, character(1), "num_desc"),
    denominator_description = vapply(conds, `[[`, character(1), "denom_desc"),
    denominator_code        = vapply(conds, `[[`, character(1), "denom_code")
  )
}


#' Compute a single CSB indicator (national + optional regional)
#'
#' Same pattern as `.compute_dhs_indicator()` from ACT, but the outcome
#' variable is specified by name instead of always being `has_act`.
#'
#' @param data Prepared febrile U5 dataset with CSB columns.
#' @param condition List with indicator metadata and outcome_var.
#' @param group_var Optional grouping variable for regional estimates.
#' @param subnational_level Character admin level (e.g., "adm1").
#' @param ci_method CI method for svyciprop. Default: "logit".
#'
#' @return Tibble with columns: level, location, point, ci_l, ci_u,
#'   counts, denominator, indicator, indicator_code, etc.
#' @noRd
.compute_csb_indicator <- function(data, condition, group_var = NULL,
                                    subnational_level = NULL,
                                    ci_method = "logit") {

  outcome_var <- condition$outcome_var

  # Check outcome variable exists

  if (!outcome_var %in% names(data)) {
    return(tibble::tibble())
  }

  # All febrile U5 are the denominator (no filter needed for CSB)
  filtered <- data
  n_denom <- nrow(filtered)

  if (n_denom == 0) {
    return(tibble::tibble())
  }

  # Temporarily set has_act = outcome column for survey estimation
  filtered$has_act <- filtered[[outcome_var]]

  # Drop rows where outcome is NA
  filtered <- filtered[!is.na(filtered$has_act), ]
  n_denom <- nrow(filtered)
  if (n_denom == 0) return(tibble::tibble())

  # Weighted counts (matches format: numerator/denominator ~ point)
  n_denom_w <- round(sum(filtered$survey_weight, na.rm = TRUE))
  n_numer_w <- round(sum(
    filtered$survey_weight * (filtered$has_act == 1), na.rm = TRUE
  ))

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
        ~has_act, by = group_formula, design = svy,
        FUN = survey::svyciprop, vartype = "ci",
        method = ci_method, na.rm = TRUE, keep.names = FALSE
      ) |> tibble::as_tibble()

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


#' Calculate Care-Seeking Behavior from DHS Data ( Methodology)
#'
#' Main function for calculating care-seeking behavior (CSB)
#' from DHS children's recode data following the WHO World Malaria Report
#' methodology. Supports spatial aggregation using administrative boundary
#' shapefiles to calculate CSB at any administrative level.
#' Returns both data and a data dictionary.
#'
#' This is a convenience wrapper around calc_csb_dhs_core() that also extracts
#' survey metadata and builds a data dictionary.
#'
#' @inheritParams calc_csb_dhs_core
#'
#' @return List with:
#'   \itemize{
#'     \item `data`: Tibble with CSB estimates by admin level
#'     \item `dict`: Data dictionary from sntutils::build_dictionary()
#'     \item `metadata`: List with survey metadata
#'   }
#'
#' @details
#' See calc_csb_dhs_core() for full details on the DHS methodology, including:
#' \itemize{
#'   \item The 5-category classification system
#'   \item How derived indicators are calculated
#'   \item How to configure country-specific source mappings
#' }
#'
#' @examples
#' # Example with default classification
#' # csb_results <- calc_csb_dhs(
#' #   dhs_kr = kr_data,
#' #   gps_data = gps_data,
#' #   shapefile = admin_shapefile,
#' #   admin_level = c("adm1")
#' # )
#' #
#' # # Example with custom classification (country-specific)
#' # my_classification <- data.frame(
#' #   variable = c("h32a", "h32b", "h32c", "h32j", "h32k", "h32n"),
#' #   csb = c("public", "public", "chw",
#' #           "private_formal", "pharmacy", "pharmacy")
#' # )
#' # csb_results <- calc_csb_dhs(
#' #   dhs_kr = kr_data,
#' #   csb_classification = my_classification
#' # )
#' #
#' # # Access the data
#' # csb_data <- csb_results$data
#' #
#' # # Access the dictionary
#' # csb_dict <- csb_results$dict
#' #
#' # # Access the metadata
#' # csb_metadata <- csb_results$metadata
#'
#' @export
calc_csb_dhs <- function(
  dhs_kr,
  survey_vars = list(
    cluster = "v021",
    weight = "v005",
    stratum = "v022",
    age = "hw1",
    fever = "h22",
    alive = "b5"
  ),
  csb_classification = NULL,
  source_config = NULL,
  region_var = NULL,
  gps_data = NULL,
  gps_vars = list(
    cluster = "DHSCLUST",
    lat = "LATNUM",
    lon = "LONGNUM"
  ),
  shapefile = NULL,
  admin_level = NULL,
  join_nearest = TRUE
) {
  # calc_csb_dhs is now a thin wrapper around calc_csb_dhs_core
  # which returns the same named list structure as calc_act_dhs
  calc_csb_dhs_core(
    dhs_kr = dhs_kr,
    survey_vars = survey_vars,
    csb_classification = csb_classification,
    source_config = source_config,
    region_var = region_var,
    gps_data = gps_data,
    gps_vars = gps_vars,
    shapefile = shapefile,
    admin_level = admin_level,
    join_nearest = join_nearest
  )
}

#' Aggregate CSB to administrative levels
#'
#' Helper to aggregate CSB results to administrative levels using a shapefile.
#' Performs spatial joins and calculates weighted averages by administrative
#' unit.
#'
#' @param csb_results CSB results with coordinates.
#' @param shapefile sf object with administrative boundaries.
#' @param admin_level Character vector of admin levels to aggregate to.
#' @param weighted Logical. If TRUE (default), uses sample size as weights.
#'
#' @return sf object with aggregated CSB by administrative level.
#'
#' @export
aggregate_csb_admin <- function(
  csb_results,
  shapefile,
  admin_level = c("adm1"),
  weighted = TRUE
) {
  if (!requireNamespace("sf", quietly = TRUE)) {
    cli::cli_abort("Package 'sf' is required for spatial operations")
  }

  # Convert to sf if needed
  if (!inherits(csb_results, "sf")) {
    if (!all(c("lat", "lon") %in% names(csb_results))) {
      cli::cli_abort(
        "csb_results must have lat and lon columns for spatial join"
      )
    }

    csb_sf <- csb_results |>
      sf::st_as_sf(
        coords = c("lon", "lat"),
        crs = 4326,
        remove = FALSE
      )
  } else {
    csb_sf <- csb_results
  }

  # Prepare shapefile
  shapefile <- shapefile |>
    sf::st_transform(4326) |>
    sf::st_make_valid()

  # Spatial join
  joined <- sf::st_join(
    csb_sf,
    shapefile[, c(admin_level, "geometry")],
    join = sf::st_within,
    left = TRUE
  )

  # Handle unmatched clusters
  unmatched <- is.na(joined[[admin_level[1]]])

  if (any(unmatched)) {
    nearest_idx <- sf::st_nearest_feature(
      joined[unmatched, ],
      shapefile
    )

    for (col in admin_level) {
      joined[unmatched, col] <- shapefile[[col]][nearest_idx]
    }
  }

  # Convert to data frame for aggregation
  joined_df <- sf::st_drop_geometry(joined)

  # Aggregate
  if (weighted && "dhs_n_fever" %in% names(joined_df)) {
    # Weighted aggregation
    aggregated <- joined_df |>
      dplyr::group_by(
        dplyr::across(
          dplyr::all_of(admin_level)
        )
      ) |>
      dplyr::summarise(
        dhs_csb_any = if ("dhs_csb_any" %in% names(joined_df)) {
          stats::weighted.mean(
            dhs_csb_any,
            w = dhs_n_fever,
            na.rm = TRUE
          )
        } else NA_real_,
        dhs_csb_public = if ("dhs_csb_public" %in% names(joined_df)) {
          stats::weighted.mean(
            dhs_csb_public,
            w = dhs_n_fever,
            na.rm = TRUE
          )
        } else NA_real_,
        dhs_csb_private = if ("dhs_csb_private" %in% names(joined_df)) {
          stats::weighted.mean(
            dhs_csb_private,
            w = dhs_n_fever,
            na.rm = TRUE
          )
        } else NA_real_,
        dhs_csb_trained = if ("dhs_csb_trained" %in% names(joined_df)) {
          stats::weighted.mean(
            dhs_csb_trained,
            w = dhs_n_fever,
            na.rm = TRUE
          )
        } else NA_real_,
        dhs_csb_none = if ("dhs_csb_none" %in% names(joined_df)) {
          stats::weighted.mean(
            dhs_csb_none,
            w = dhs_n_fever,
            na.rm = TRUE
          )
        } else NA_real_,
        dhs_n_fever = sum(
          dhs_n_fever,
          na.rm = TRUE
        ),
        dhs_n_public = if ("dhs_n_public" %in% names(joined_df)) {
          sum(dhs_n_public, na.rm = TRUE)
        } else NA_integer_,
        dhs_n_private = if ("dhs_n_private" %in% names(joined_df)) {
          sum(dhs_n_private, na.rm = TRUE)
        } else NA_integer_,
        dhs_n_trained = if ("dhs_n_trained" %in% names(joined_df)) {
          sum(dhs_n_trained, na.rm = TRUE)
        } else NA_integer_,
        dhs_n_none = if ("dhs_n_none" %in% names(joined_df)) {
          sum(dhs_n_none, na.rm = TRUE)
        } else NA_integer_,
        .groups = "drop"
      )
  } else {
    # Simple average
    aggregated <- joined_df |>
      dplyr::group_by(
        dplyr::across(
          dplyr::all_of(admin_level)
        )
      ) |>
      dplyr::summarise(
        dhs_csb_any = if ("dhs_csb_any" %in% names(joined_df)) {
          mean(dhs_csb_any, na.rm = TRUE)
        } else NA_real_,
        dhs_csb_public = if ("dhs_csb_public" %in% names(joined_df)) {
          mean(dhs_csb_public, na.rm = TRUE)
        } else NA_real_,
        dhs_csb_private = if ("dhs_csb_private" %in% names(joined_df)) {
          mean(dhs_csb_private, na.rm = TRUE)
        } else NA_real_,
        dhs_csb_trained = if ("dhs_csb_trained" %in% names(joined_df)) {
          mean(dhs_csb_trained, na.rm = TRUE)
        } else NA_real_,
        dhs_csb_none = if ("dhs_csb_none" %in% names(joined_df)) {
          mean(dhs_csb_none, na.rm = TRUE)
        } else NA_real_,
        dhs_n_fever = sum(
          dhs_n_fever,
          na.rm = TRUE
        ),
        dhs_n_public = if ("dhs_n_public" %in% names(joined_df)) {
          sum(dhs_n_public, na.rm = TRUE)
        } else NA_integer_,
        dhs_n_private = if ("dhs_n_private" %in% names(joined_df)) {
          sum(dhs_n_private, na.rm = TRUE)
        } else NA_integer_,
        dhs_n_trained = if ("dhs_n_trained" %in% names(joined_df)) {
          sum(dhs_n_trained, na.rm = TRUE)
        } else NA_integer_,
        dhs_n_none = if ("dhs_n_none" %in% names(joined_df)) {
          sum(dhs_n_none, na.rm = TRUE)
        } else NA_integer_,
        .groups = "drop"
      )
  }

  # Round percentages
  aggregated <- aggregated |>
    dplyr::mutate(
      dplyr::across(
        dplyr::starts_with("dhs_csb_"),
        ~ round(.x, 1)
      )
    )

  # Detect and preserve admin name columns
  admin_name_cols <- paste0(admin_level, "_name")
  admin_name_cols <- admin_name_cols[
    admin_name_cols %in% names(shapefile)
  ]
  all_admin_cols <- c(admin_level, admin_name_cols)

  # Join back with shapefile geometry
  result_with_geometry <- shapefile |>
    dplyr::select(
      dplyr::all_of(all_admin_cols)
    ) |>
    dplyr::distinct() |>
    dplyr::left_join(
      aggregated,
      by = admin_level
    )

  result_with_geometry
}
