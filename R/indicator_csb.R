# csb indicator
#
# Merged from: dhs_calc_csb.R dhs_calc_csb_mbg.R dhs_helpers_csb.R dhs_calc_csb_custom_mbg.R dhs_calc_csb_by_wealth_dhs.R dhs_calc_csb_by_wealth_mbg.R
# Contains the survey-weighted calc, MBG cluster-prep, and indicator-
# specific helpers for this family.

# ---- dhs_calc_csb.R ----

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
#' @param csb_priority_method Character, one of "all" (default), "first",
#'   "public", or "private". Controls how overlapping care-seeking records
#'   are resolved so each child is assigned to at most one sector.
#'   \itemize{
#'     \item `"all"`: WHO default. Overlaps allowed; csb_public + csb_private
#'       may exceed 100%.
#'     \item `"first"`: Take the first h32 source (alphabetical order) visited
#'       per child.
#'     \item `"public"`: Public/CHW priority when a child sought both sectors.
#'     \item `"private"`: Private priority when a child sought both sectors.
#'   }
#'   With non-`"all"` values, csb_public + csb_private + csb_none sums to 100%.
#' @param source_config **Deprecated**. No longer used.
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
#' @param custom_csb_indicator Optional named list defining a user-specified,
#'   mutually-exclusive care-seeking partition fitted in addition to the
#'   built-in CSB indicators. When supplied, three derived indicators are
#'   produced: \code{<name>_dhis} (sought care at any user-listed DHIS
#'   source), \code{<name>_nondhis} (sought care at any user-listed
#'   non-DHIS source and not at any DHIS source), and \code{<name>_untreat}
#'   (did not seek care at any user-listed source). The list must have
#'   four character fields: \code{name} (alphanumeric prefix matching
#'   pattern \code{^csb_[a-z0-9_]+$}), \code{dhis_locs},
#'   \code{nondhis_locs}, and \code{untreat_locs}. Each \code{*_locs}
#'   vector may contain either \strong{h32 variable names}
#'   (e.g. \code{"h32a"}, \code{"h32e"}) which are matched as columns in
#'   \code{dhs_kr}, or \strong{haven label strings} which are matched
#'   case-insensitively against the \code{label} attribute of each h32
#'   column. The two styles can be mixed in the same vector. Variable-name
#'   matches take precedence over label matches, which is useful when two
#'   h32 columns share an identical haven label (e.g. \code{h32e} and
#'   \code{h32n} both labelled "comm.health wrkr"). The custom triple is
#'   always mutually exclusive at the child level (priority
#'   \code{dhis > nondhis > untreat}). Default: NULL (disabled).
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
#' @keywords internal
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
  csb_priority_method = c("all", "first", "public", "private"),
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
  join_nearest = TRUE,
  custom_csb_indicator = NULL
) {
  csb_priority_method <- match.arg(csb_priority_method)
  # ---- 1. Input validation ---------------------------------------------------

  if (!is.data.frame(dhs_kr)) {
    cli::cli_abort("`dhs_kr` must be a data.frame or tibble.")
  }

  if (nrow(dhs_kr) == 0) {
    cli::cli_abort("`dhs_kr` is empty.")
  }

  # Validate custom_csb_indicator (if supplied) and pre-build the
  # label-to-bucket classification from the *raw* dhs_kr (haven labels
  # are still intact at this stage; .prepare_csb_data() zaps them later).
  custom_csb_classification <- NULL
  if (!is.null(custom_csb_indicator)) {
    custom_csb_indicator <- .validate_custom_csb_indicator_spec(
      custom_csb_indicator
    )
    custom_csb_classification <- .build_custom_csb_classification(
      dhs_kr, custom_csb_indicator
    )
    custom_codes <- .custom_csb_indicator_names(custom_csb_indicator)
    cli::cli_alert_info(
      paste0(
        "Custom CSB partition active: prefix {.val ",
        "{custom_csb_indicator$name}} -> {.val {custom_codes}}"
      )
    )
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

  # Warn if any detected h32 variables are not in the default classification
  {
    ref_class <- .default_csb_classification()
    expected_h32 <- ref_class$variable
    unexpected_h32 <- setdiff(available_h32, expected_h32)
    if (length(unexpected_h32) > 0) {
      cli::cli_warn(
        "Detected h32 variables not in standard classification: {paste(unexpected_h32, collapse = ', ')}. These may be country-specific non-standard slots."
      )
    }
  }

  # ---- 2. Prepare base dataset -----------------------------------------------

  if (!is.null(source_config)) {
    cli::cli_alert_warning(
      "source_config is deprecated and ignored. Default classification is used."
    )
  }

  kr_fever <- .prepare_csb_data(
    dhs_kr = dhs_kr,
    survey_vars = survey_vars,
    include_survey_vars = TRUE,
    csb_priority_method = csb_priority_method
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

  # Classify into custom CSB triple if requested. Adds three mutually
  # exclusive 0/1 columns named <prefix>_dhis / _nondhis / _untreat, derived
  # directly from raw h32 columns and the per-survey label classification
  # built earlier (haven labels were available at that point).
  if (!is.null(custom_csb_indicator)) {
    h32_cols <- grep("^h32[a-z0-9]+$", names(kr_fever), value = TRUE)
    h32_cols <- setdiff(h32_cols, c("h32y", "h32z"))
    kr_fever <- .classify_custom_csb_from_h32(
      data = kr_fever,
      h32_cols = h32_cols,
      classification = custom_csb_classification,
      prefix = custom_csb_indicator$name
    )
  }

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

  if (!exists("geo_src")) geo_src <- "survey"

  # ---- 6. Compute each CSB indicator (national + each admin level) ----------

  options(survey.lonely.psu = "adjust")

  dict <- .csb_conditions()

  # Append per-survey custom CSB conditions so the standard computation
  # loop emits the three derived indicators alongside the built-ins.
  if (!is.null(custom_csb_indicator)) {
    dict <- c(dict, .custom_csb_dhs_conditions(custom_csb_indicator))
  }

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


#' Build per-survey indicator conditions for a custom CSB partition
#'
#' Mirrors the shape of `.csb_conditions()` so the resulting list can be
#' concatenated with the built-in dict and consumed by the standard
#' `.compute_csb_indicator()` loop. Each derived sub-indicator
#' (`<name>_dhis`, `<name>_nondhis`, `<name>_untreat`) is mapped to the
#' 0/1 column added by `.classify_custom_csb_from_h32()`.
#'
#' @param custom_csb_indicator A validated user spec list.
#' @return A list of three condition specs.
#' @noRd
.custom_csb_dhs_conditions <- function(custom_csb_indicator) {
  spec <- custom_csb_indicator
  prefix <- spec$name
  denom <- "Under 5 with fever"

  list(
    list(
      indicator      = toupper(paste0(prefix, "_DHIS")),
      indicator_code = paste0(prefix, "_dhis"),
      indicator_title = paste0(
        "Custom care seeking (DHIS sources) [", prefix,
        "] among under 5 with fever"
      ),
      denom_code     = "feb_u5",
      outcome_var    = paste0(prefix, "_dhis"),
      num_desc       = "Sought care at user-listed DHIS sources",
      denom_desc     = denom
    ),
    list(
      indicator      = toupper(paste0(prefix, "_NONDHIS")),
      indicator_code = paste0(prefix, "_nondhis"),
      indicator_title = paste0(
        "Custom care seeking (non-DHIS sources) [", prefix,
        "] among under 5 with fever"
      ),
      denom_code     = "feb_u5",
      outcome_var    = paste0(prefix, "_nondhis"),
      num_desc       = paste0(
        "Sought care at user-listed non-DHIS sources ",
        "(and not at any DHIS source)"
      ),
      denom_desc     = denom
    ),
    list(
      indicator      = toupper(paste0(prefix, "_UNTREAT")),
      indicator_code = paste0(prefix, "_untreat"),
      indicator_title = paste0(
        "Custom no/untreated care seeking [", prefix,
        "] among under 5 with fever"
      ),
      denom_code     = "feb_u5",
      outcome_var    = paste0(prefix, "_untreat"),
      num_desc       = paste0(
        "Did not seek care at any user-listed DHIS or ",
        "non-DHIS source"
      ),
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
#' @keywords internal
#' @noRd
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
  csb_priority_method = c("all", "first", "public", "private"),
  source_config = NULL,
  custom_csb_indicator = NULL,
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
  # Fail fast on missing suggested dependencies
  .check_pkg(
    c("purrr", "tibble", "tidyr"),
    reason = "for `calc_csb_dhs()`"
  )

  csb_priority_method <- match.arg(csb_priority_method)
  # calc_csb_dhs is now a thin wrapper around calc_csb_dhs_core
  # which returns the same named list structure as calc_act_dhs
  calc_csb_dhs_core(
    dhs_kr = dhs_kr,
    survey_vars = survey_vars,
    csb_priority_method = csb_priority_method,
    source_config = source_config,
    custom_csb_indicator = custom_csb_indicator,
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
#' @keywords internal
#' @noRd
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


# ---- dhs_calc_csb_mbg.R ----

#' Prepare Care-Seeking Behavior Data for MBG Analysis
#'
#' Prepares cluster-level care-seeking behavior data for MBG analysis.
#' Calculates proportions of febrile children who sought care at various
#' source types.
#'
#' @details
#' Methodology: \url{https://github.com/ahadi-analytics/sntmethods/blob/master/inst/methods/csb_dhs.yml}
#'
#' @param dhs_kr DHS Children's Recode (KR) dataset.
#' @param gps_data DHS GPS dataset with cluster coordinates.
#' @param indicators Character vector of indicators to
#'   calculate. standardized sector breakdown:
#'   \itemize{
#'     \item "any": Sought care anywhere (public or private)
#'     \item "public": Public sector including CHW
#'     \item "pub_nochw": Public sector excluding CHW
#'     \item "chw": Community health worker only
#'     \item "private": Any private sector
#'     \item "priv_formal": Private formal sector only
#'     \item "pharmacy": Pharmacy / drug shop only
#'     \item "priv_informal": Private informal only
#'     \item "priv_form_pha": Private formal or pharmacy
#'     \item "trained": Trained provider (public + formal +
#'       pharmacy)
#'     \item "none": Did not seek care
#'   }
#'   Default: c("any", "public", "private", "none").
#' @param csb_priority_method Character, one of "all" (default), "first",
#'   "public", or "private". Controls how overlapping care-seeking records
#'   are resolved so each individual is assigned to at most one sector:
#'   \itemize{
#'     \item "all": Keep WHO methodology; overlaps allowed (csb_public and
#'       csb_private can both be 1 for the same child).
#'     \item "first": Take the first recurring h32 source visited per child
#'       (alphabetical h32 order: h32a, h32b, ..., h32x). Mutually exclusive.
#'     \item "public": If child sought any public/CHW care, classify as
#'       public; otherwise private if any private; otherwise none.
#'     \item "private": If child sought any private care, classify as
#'       private; otherwise public if any public; otherwise none.
#'   }
#'   With non-"all" values, csb_public + csb_private + csb_none sums to 100%.
#' @param survey_vars Named list mapping DHS variable names.
#' @param gps_vars Named list for GPS variable mapping.
#'
#' @return A list of data.tables (one per indicator), each with columns:
#'   \itemize{
#'     \item cluster_id: Cluster identifier
#'     \item indicator: Numerator count
#'     \item samplesize: Denominator count
#'     \item x: Longitude
#'     \item y: Latitude
#'   }
#'
#' @details
#' This function uses KR data on children under 5 who had fever in the last
#' 2 weeks. Care-seeking is determined using h32 variables.
#'
#' Note: Care-seeking indicators (except "none") are NOT mutually exclusive.
#' A child can appear in both "public" and "private" if they visited both.
#'
#' @examples
#' \dontrun{
#' csb_mbg <- calc_csb_mbg(
#'   dhs_kr = kr_data,
#'   gps_data = gps_data,
#'   indicators = c("public", "none")
#' )
#' }
#'
#' @seealso [calc_csb_dhs()] for survey-weighted estimates
#' @export
calc_csb_mbg <- function(
  dhs_kr,
  gps_data,
  indicators = c("any", "public", "private", "none"),
  csb_priority_method = c("all", "first", "public", "private"),
  survey_vars = list(
    cluster = "v001",
    age = "hw1",
    fever = "h22"
  ),
  gps_vars = list(
    cluster = "DHSCLUST",
    lat = "LATNUM",
    lon = "LONGNUM"
  )
) {
  csb_priority_method <- match.arg(csb_priority_method)

  # ---- Input validation ----

  if (!is.data.frame(dhs_kr)) {
    cli::cli_abort("`dhs_kr` must be a data.frame or tibble")
  }
  if (!is.data.frame(gps_data)) {
    cli::cli_abort("`gps_data` must be a data.frame or tibble")
  }

  # Accept both prefixed ("csb_public") and short ("public") forms
  indicators <- sub("^csb_", "", indicators)

  valid_indicators <- c(
    "any", "public", "pub_nochw", "chw",
    "private", "priv_formal", "pharmacy",
    "priv_informal", "priv_form_pha",
    "trained", "none"
  )
  invalid <- setdiff(indicators, valid_indicators)
  if (length(invalid) > 0) {
    cli::cli_abort(
      "Invalid indicators: {.val {invalid}}"
    )
  }

  # ---- Prepare data using shared helpers ----

  gps_clean <- .prepare_gps_data(gps_data, gps_vars)

  kr_fever <- .prepare_csb_data(
    dhs_kr = dhs_kr,
    survey_vars = survey_vars,
    include_survey_vars = FALSE,
    csb_priority_method = csb_priority_method
  )

  # ---- Calculate cluster-level indicators ----

  # Maps short indicator name -> CSB column in data
  indicator_map <- list(
    any = "csb_any",
    public = "csb_public",
    pub_nochw = "csb_public_nochw",
    chw = "csb_chw",
    private = "csb_private",
    priv_formal = "csb_private_formal_ind",
    pharmacy = "csb_pharmacy",
    priv_informal = "csb_private_informal",
    priv_form_pha = "csb_private_formal_pha",
    trained = "csb_trained",
    none = "csb_none"
  )

  # Maps short indicator name -> output key name
  result_names <- list(
    any = "csb_any",
    public = "csb_public",
    pub_nochw = "csb_pub_nochw",
    chw = "csb_chw",
    private = "csb_private",
    priv_formal = "csb_priv_formal",
    pharmacy = "csb_pharmacy",
    priv_informal = "csb_priv_informal",
    priv_form_pha = "csb_priv_form_pha",
    trained = "csb_trained",
    none = "csb_none"
  )

  results <- list()

  for (ind in indicators) {
    indicator_col <- indicator_map[[ind]]
    result_name <- result_names[[ind]]

    dt <- .aggregate_to_mbg_clusters(
      individual_data = kr_fever,
      indicator_col = indicator_col,
      gps_clean = gps_clean,
      result_name = result_name
    )

    if (!is.null(dt)) {
      results[[result_name]] <- dt
    }
  }

  if (length(results) == 0) {
    cli::cli_abort("No valid MBG data could be prepared")
  }

  results
}


#' Prepare Single CSB Indicator for MBG
#'
#' @inheritParams calc_csb_mbg
#' @param indicator Single indicator name. Default: "public".
#'
#' @return A data.table with columns: cluster_id, indicator, samplesize, x, y
#' @export
prep_csb_mbg <- function(
  dhs_kr,
  gps_data,
  indicator = "public",
  csb_priority_method = c("all", "first", "public", "private"),
  survey_vars = list(
    cluster = "v001",
    age = "hw1",
    fever = "h22"
  ),
  gps_vars = list(
    cluster = "DHSCLUST",
    lat = "LATNUM",
    lon = "LONGNUM"
  )
) {
  csb_priority_method <- match.arg(csb_priority_method)

  result <- calc_csb_mbg(
    dhs_kr = dhs_kr,
    gps_data = gps_data,
    indicators = indicator,
    csb_priority_method = csb_priority_method,
    survey_vars = survey_vars,
    gps_vars = gps_vars
  )

  result[[1]]
}


# ---- dhs_helpers_csb.R ----

#' Prepare CSB Data for Analysis
#'
#' Shared data cleaning and indicator computation for CSB functions.
#' Used by both calc_csb_dhs_core() and calc_csb_mbg().
#'
#' @param dhs_kr DHS Children's Recode (KR) dataset.
#' @param survey_vars Named list mapping DHS variable names. Must include:
#'   cluster, age, fever. Optionally: weight, stratum.
#' @param include_survey_vars Logical. If TRUE, includes survey_weight and
#'   stratum_id columns for DHS survey design. If FALSE, omits them (for MBG).
#' @param csb_priority_method Character, one of "all" (default), "first",
#'   "public", or "private". Controls how overlapping care-seeking is
#'   handled so each individual is assigned to at most one sector:
#'   \itemize{
#'     \item "all": Keep WHO methodology (overlaps allowed; a child can be
#'       in both csb_public and csb_private).
#'     \item "first": Keep the first recurring h32 source visited per child
#'       (based on h32 alphabetical order: h32a, h32b, ..., h32x). Each
#'       child is assigned to exactly one category.
#'     \item "public": Public priority - if any public/CHW care, classify
#'       as public; else private if any private; else none.
#'     \item "private": Private priority - if any private care, classify
#'       as private; else public if any public; else none.
#'   }
#'   Non-"all" options make csb_public + csb_private + csb_none sum to 100%.
#'
#' @return A data frame of febrile children with columns:
#'   cluster_id, age_months, and binary indicators:
#'   csb_public, csb_private, csb_trained, csb_any, csb_none.
#'   If include_survey_vars = TRUE, also: survey_weight, stratum_id.
#'   Plus any additional columns from the original KR data needed downstream.
#'
#' @noRd
.prepare_csb_data <- function(
  dhs_kr,
  survey_vars,
  include_survey_vars = FALSE,
  csb_priority_method = c("all", "first", "public", "private")
) {
  csb_priority_method <- match.arg(csb_priority_method)
  # Input validation
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

  # Auto-detect h32 variables (before label zapping)
  available_h32 <- grep("^h32[a-z0-9]+$", names(dhs_kr), value = TRUE)

  # Classification: detect from haven labels first, fall back to the
  # hardcoded default mapping. Label detection correctly classifies CHW
  # and pharmacy slots across DHS-7 and DHS-8 survey versions.
  classification <- .detect_csb_from_labels(dhs_kr)
  if (nrow(classification) == 0) {
    classification <- .default_csb_classification()
  }
  if (length(available_h32) == 0) {
    cli::cli_warn("No h32 treatment-seeking variables found in data.")
    return(NULL)
  }

  # Warn if any detected h32 variables are not in the classification table
  expected_h32 <- classification$variable
  unexpected_h32 <- setdiff(available_h32, expected_h32)
  if (length(unexpected_h32) > 0) {
    cli::cli_warn(
      "Detected h32 variables not in standard classification: {paste(unexpected_h32, collapse = ', ')}. These may be country-specific non-standard slots."
    )
  }

  # Filter classification to available variables
  classification <- classification |>
    dplyr::filter(variable %in% available_h32)

  h32_cols <- intersect(classification$variable, names(dhs_kr))

  # Zap labels and build base dataset
  kr <- dhs_kr |>
    dplyr::mutate(dplyr::across(dplyr::everything(), haven::zap_labels)) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), as.vector))

  # Build selection (force numeric to guard against haven character residuals)
  # Diagnostic: check fever values before coercion
  fever_raw <- kr[[survey_vars$fever]]
  unique_fever_raw <- unique(fever_raw[!is.na(fever_raw)])

  kr <- kr |>
    dplyr::mutate(
      cluster_id = .data[[survey_vars$cluster]],
      age_months = suppressWarnings(as.numeric(as.character(.data[[survey_vars$age]]))),
      had_fever = suppressWarnings(as.numeric(as.character(.data[[survey_vars$fever]])))
    )

  # Sanitize fever: only 0/1 (and 1/2 for some surveys) are valid yes/no
  # responses. DHS codes 8 ("don't know") and 9 ("missing") and any other
  # stray value should be treated as NA so they don't leak through to the
  # fever subset below or to fever-coding auto-detection.
  invalid_fever_mask <- !is.na(kr$had_fever) &
    !(kr$had_fever %in% c(0, 1, 2))
  n_invalid_fever <- sum(invalid_fever_mask)
  if (n_invalid_fever > 0) {
    invalid_vals <- sort(unique(kr$had_fever[invalid_fever_mask]))
    cli::cli_alert_info(
      "Fever variable ({.var {survey_vars$fever}}): coercing {format(n_invalid_fever, big.mark = ',')} row{?s} with values {paste(invalid_vals, collapse = ', ')} to NA."
    )
    kr$had_fever[invalid_fever_mask] <- NA_real_
  }

  # Diagnostic: check fever values after coercion + sanitization
  unique_fever_coerced <- unique(kr$had_fever[!is.na(kr$had_fever)])

  if (length(unique_fever_coerced) > 0) {
    cli::cli_alert_info(
      "Fever variable ({.var {survey_vars$fever}}) unique values: {paste(sort(unique_fever_coerced), collapse = ', ')}"
    )
  } else {
    cli::cli_warn(
      "Fever variable ({.var {survey_vars$fever}}) has no non-NA values after coercion. Raw values: {paste(head(unique_fever_raw, 10), collapse = ', ')}"
    )
  }

  if (include_survey_vars) {
    kr <- kr |>
      dplyr::mutate(
        survey_weight = .data[[survey_vars$weight]] / 1e6,
        stratum_id = .data[[survey_vars$stratum]]
      )
  }

  # Drop rows that cannot enter the survey design at all. svydesign() aborts
  # with "missing values in `id'" if any cluster id is NA; the same holds
  # for the stratum id and weight when include_survey_vars = TRUE. These
  # rows would also be silently excluded by survey:::svydesign() anyway,
  # so dropping them up front gives a useful audit log instead of a crash.
  design_drop_mask <- is.na(kr$cluster_id)
  if (include_survey_vars) {
    design_drop_mask <- design_drop_mask |
      is.na(kr$survey_weight) |
      is.na(kr$stratum_id)
  }
  n_design_drop <- sum(design_drop_mask)
  if (n_design_drop > 0) {
    cli::cli_alert_info(
      "Dropped {format(n_design_drop, big.mark = ',')} row{?s} with NA in cluster{?/, stratum, or weight} (survey-design columns)."
    )
    kr <- kr[!design_drop_mask, , drop = FALSE]
  }
  if (nrow(kr) == 0) {
    cli::cli_abort(
      "All rows have NA in survey-design columns; nothing to estimate."
    )
  }

  # Check alive variable if present
  has_alive <- !is.null(survey_vars$alive) &&
    survey_vars$alive %in% names(dhs_kr)
  if (has_alive) {
    kr <- kr |>
      dplyr::mutate(
        child_alive = suppressWarnings(as.numeric(as.character(.data[[survey_vars$alive]])))
      )
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

  cli::cli_alert_info(
    "Total children under 5 (alive): {format(nrow(kr_u5), big.mark = ',')}"
  )

  # Detect fever coding scheme: Some surveys use 0=No/1=Yes, others use 1=No/2=Yes
  # Standardize: if unique values are {1, 2}, assume 2=Yes; otherwise assume 1=Yes
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

  cli::cli_alert_info(
    "Found {format(nrow(kr_fever), big.mark = ',')} children under 5 with fever"
  )

  # Apply care-seeking classification from h32 variables
  kr_fever <- .classify_csb_from_h32(
    kr_fever,
    h32_cols,
    classification = classification,
    csb_priority_method = csb_priority_method
  )

  kr_fever
}


#' Classify Care-Seeking from h32 Variables
#'
#' Applies the h32 treatment-seeking classification to a data frame that
#' already contains h32 columns. Creates binary indicators for care-seeking
#' categories: csb_public, csb_private, csb_any, csb_none, csb_trained.
#'
#' This is the core classification logic used by \code{.prepare_csb_data()}
#' and also reused by ACT/antimalarial MBG functions to create public
#' care-seeking subsets.
#'
#' @param data Data frame containing h32 columns (already filtered to the
#'   target population, e.g. febrile U5).
#' @param h32_cols Character vector of h32 column names present in data.
#'   If NULL, auto-detects from column names.
#' @param classification Data frame with variable and csb columns mapping
#'   h32 slots to sector labels. If NULL, uses the package default
#'   (see \code{.default_csb_classification()}).
#' @param csb_priority_method Character, one of "all" (default), "first",
#'   "public", or "private". Controls overlap handling. See
#'   \code{.prepare_csb_data()} for details. With non-"all" values, each
#'   child is assigned to at most one of csb_public / csb_private so that
#'   csb_public + csb_private + csb_none sums to 100%.
#'
#' @return The input data frame with added columns: .row_id, has_public,
#'   has_chw, has_private_formal, has_private_informal, has_pharmacy,
#'   csb_public, csb_private, csb_private_formal_pha, csb_any, csb_none,
#'   csb_trained.
#'
#' @noRd
.classify_csb_from_h32 <- function(data, h32_cols = NULL,
                                    classification = NULL,
                                    csb_priority_method = c("all", "first",
                                                             "public",
                                                             "private")) {
  csb_priority_method <- match.arg(csb_priority_method)
  # Default classification
  if (is.null(classification)) {
    classification <- .default_csb_classification()
  }

  # Auto-detect h32 columns if not provided
  if (is.null(h32_cols)) {
    available_h32 <- grep("^h32[a-z0-9]+$", names(data), value = TRUE)
    if (length(available_h32) == 0) {
      cli::cli_abort("No h32 treatment-seeking variables found in data.")
    }
    classification <- classification |>
      dplyr::filter(variable %in% available_h32)
    h32_cols <- intersect(classification$variable, names(data))
  }

  if (length(h32_cols) == 0) {
    cli::cli_abort("No h32 treatment-seeking variables found in data.")
  }

  # Preserve existing .row_id if present (from upstream like .prepare_act_data)
  # Use internal .csb_row_id for pivot logic to avoid overwriting
  has_original_row_id <- ".row_id" %in% names(data)
  if (has_original_row_id) {
    data$.original_row_id <- data$.row_id
  }

  data <- data |>
    dplyr::mutate(.csb_row_id = dplyr::row_number())

  # Ensure h32 columns are numeric (guards against residual haven labels
  # or character values in older DHS surveys like BDI 2012)
  for (col in h32_cols) {
    data[[col]] <- suppressWarnings(as.numeric(as.character(data[[col]])))
  }

  # Convert h32 to binary and reshape
  kr_long <- data |>
    dplyr::select(.csb_row_id, dplyr::all_of(h32_cols)) |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(h32_cols),
      names_to = "variable",
      values_to = "visited"
    ) |>
    dplyr::left_join(
      classification |> dplyr::select(variable, csb),
      by = "variable"
    ) |>
    dplyr::filter(!is.na(visited) & visited == 1)

  # Optional: keep only the FIRST recurring h32 source visited per child
  # (ordered alphabetically: h32a, h32b, ..., h32x). This makes the resulting
  # sector assignment mutually exclusive so csb_public + csb_private +
  # csb_none sums to exactly 100% at the cluster level.
  #
  # We sort alphabetically (rather than using the classification order) so
  # behavior is deterministic. The default classification groups h32 slots
  # by sector, which would otherwise change the meaning of "first".
  if (csb_priority_method == "first" && nrow(kr_long) > 0) {
    h32_sorted <- sort(h32_cols)
    h32_order <- data.frame(
      variable = h32_sorted,
      .h32_order = seq_along(h32_sorted),
      stringsAsFactors = FALSE
    )
    kr_long <- kr_long |>
      dplyr::left_join(h32_order, by = "variable") |>
      dplyr::group_by(.csb_row_id) |>
      dplyr::arrange(.h32_order, .by_group = TRUE) |>
      dplyr::slice_head(n = 1) |>
      dplyr::ungroup() |>
      dplyr::select(-.h32_order)
  }

  # Aggregate to base categories per child
  if (nrow(kr_long) > 0) {
    base_cats <- kr_long |>
      dplyr::group_by(.csb_row_id, csb) |>
      dplyr::summarise(visited = 1L, .groups = "drop") |>
      tidyr::pivot_wider(
        names_from = csb,
        values_from = visited,
        values_fill = 0L,
        names_prefix = "has_"
      )
    data <- data |>
      dplyr::left_join(base_cats, by = ".csb_row_id")
  }

  # Ensure all base categories exist
  for (col in c("has_public", "has_chw", "has_private_formal",
                 "has_private_informal", "has_pharmacy")) {
    if (!col %in% names(data)) {
      data[[col]] <- 0L
    }
    data[[col]] <- tidyr::replace_na(data[[col]], 0L)
  }

  # Sector-priority resolution for overlapping care-seeking.
  # When csb_priority_method is "public" or "private", zero out the
  # non-priority sector for children who sought both. This ensures each
  # child is classified into exactly one of public / private so that
  # csb_public + csb_private + csb_none sums to 100% at the cluster level.
  if (csb_priority_method == "public") {
    .has_public_any <- data$has_public == 1 | data$has_chw == 1
    data$has_private_formal[.has_public_any] <- 0L
    data$has_private_informal[.has_public_any] <- 0L
    data$has_pharmacy[.has_public_any] <- 0L
  } else if (csb_priority_method == "private") {
    .has_private_any <- data$has_private_formal == 1 |
      data$has_private_informal == 1 |
      data$has_pharmacy == 1
    data$has_public[.has_private_any] <- 0L
    data$has_chw[.has_private_any] <- 0L
  }

  # Create derived indicators
  result <- data |>
    dplyr::mutate(
      # Composite sectors
      csb_public = as.numeric(
        has_public == 1 | has_chw == 1
      ),
      csb_private = as.numeric(
        has_private_formal == 1 |
          has_private_informal == 1 |
          has_pharmacy == 1
      ),
      csb_private_formal_pha = as.numeric(
        has_private_formal == 1 | has_pharmacy == 1
      ),
      csb_any = as.numeric(
        csb_public == 1 | csb_private == 1
      ),
      csb_none = as.numeric(
        csb_public == 0 & csb_private == 0
      ),
      csb_trained = as.numeric(
        csb_public == 1 | csb_private_formal_pha == 1
      ),
      # Granular sectors (indicator alignment)
      csb_public_nochw = as.numeric(has_public == 1),
      csb_chw = as.numeric(has_chw == 1),
      csb_private_formal_ind = as.numeric(
        has_private_formal == 1
      ),
      csb_pharmacy = as.numeric(has_pharmacy == 1),
      csb_private_informal = as.numeric(
        has_private_informal == 1
      ),
      # Aliases for downstream naming consistency
      csb_any_treatment = csb_any,
      csb_trained_provider = csb_trained,
      csb_no_treatment = csb_none
    )

  # Restore original .row_id if it was present, remove temporary columns
  if (has_original_row_id) {
    result$.row_id <- result$.original_row_id
    result$.original_row_id <- NULL
  }
  result$.csb_row_id <- NULL

  result
}


#' Detect CSB classification from haven variable labels
#'
#' Scans h32 variables for haven labels and classifies each into a CSB
#' category (public, chw, private_formal, pharmacy, private_informal)
#' based on label content. This is the same approach used for ACT detection
#' in `.detect_act_vars()` -- label-based, not slot-based.
#'
#' DHS variable slots change between survey versions (DHS-7 vs DHS-8), so
#' hardcoded slot-to-category mappings break. Label detection works across
#' all versions.
#'
#' @param dhs_kr Original DHS dataset with haven labels intact (pre-zap).
#' @param fallback_classification Data frame with variable and csb columns.
#'   Used as fallback for variables whose labels don't match any pattern.
#'   If NULL, uses `.default_csb_classification()`.
#'
#' @return Data frame with columns: variable, csb -- one row per classified
#'   h32 variable found in the data. Excludes meta variables (h32y, h32z)
#'   and unclassified/NA-prefix variables.
#' @noRd
.detect_csb_from_labels <- function(dhs_kr,
                                     fallback_classification = NULL) {

  available_h32 <- grep("^h32[a-z0-9]+$", names(dhs_kr), value = TRUE)
  if (length(available_h32) == 0) return(data.frame(
    variable = character(0), csb = character(0), stringsAsFactors = FALSE
  ))

  # Exclude meta variables (no treatment / medical treatment flags)
  meta_vars <- c("h32y", "h32z")
  available_h32 <- setdiff(available_h32, meta_vars)

  if (is.null(fallback_classification)) {
    fallback_classification <- .default_csb_classification()
  }

  # Label patterns -- ordered from most specific to most general.
  # Same philosophy as .detect_act_vars(): scan haven labels, not slot letters.
  #
  # CHW / community health worker / fieldworker
  chw_pattern <- paste0(
    "community.?health.?worker|\\bchw\\b|field.?worker|",
    "community.oriented.resource|",
    "agent.communautaire|relais.communautaire"
  )
  # Pharmacy / drug shop / chemist / PPMV
  pharm_pattern <- paste0(
    "\\bpharmac|drug.?shop|drug.?store|\\bchemist|\\bppmv\\b|",
    "patent.medicine"
  )
  # NGO sector (treated as public )
  ngo_pattern <- "\\bngo\\b|non.governmental|faith.based"
  # Public sector (government / dispensary / health center / gov mobile clinic)
  public_pattern <- paste0(
    "government|\\bgov\\b|public.sector|other.public|dispensar|",
    "health.center|health.centre|health.post|\\bmch\\b"
  )
  # Private formal (hospital / clinic / doctor / private mobile)
  priv_formal_pattern <- paste0(
    "private.hospital|private.clinic|private.doctor|",
    "private.mobile|other.private"
  )
  # Private informal (shop / market / traditional / itinerant / drug seller)
  priv_informal_pattern <- paste0(
    "\\bshop\\b|\\bmarket\\b|traditional|\\bitinerant\\b|",
    "drug.seller"
  )

  result <- data.frame(
    variable = character(0), csb = character(0),
    stringsAsFactors = FALSE
  )

  for (v in available_h32) {
    lbl <- attr(dhs_kr[[v]], "label")

    # Skip NA-prefixed labels (unused DHS country-specific slots)
    if (!is.null(lbl) && is.character(lbl) && length(lbl) == 1 &&
        grepl("^NA\\s*-", lbl)) {
      next
    }

    csb_cat <- NA_character_

    if (!is.null(lbl) && is.character(lbl) && length(lbl) == 1) {
      # Match most specific first
      if (grepl(chw_pattern, lbl, ignore.case = TRUE)) {
        csb_cat <- "chw"
      } else if (grepl(pharm_pattern, lbl, ignore.case = TRUE)) {
        csb_cat <- "pharmacy"
      } else if (grepl(ngo_pattern, lbl, ignore.case = TRUE)) {
        # NGO treated as public sector for analysis purposes
        csb_cat <- "public"
      } else if (grepl(public_pattern, lbl, ignore.case = TRUE)) {
        csb_cat <- "public"
      } else if (grepl(priv_formal_pattern, lbl, ignore.case = TRUE)) {
        csb_cat <- "private_formal"
      } else if (grepl(priv_informal_pattern, lbl, ignore.case = TRUE,
                        perl = TRUE)) {
        csb_cat <- "private_informal"
      }
    }

    # Fallback to hardcoded classification if label didn't match
    if (is.na(csb_cat)) {
      fb_row <- fallback_classification[
        fallback_classification$variable == v, , drop = FALSE
      ]
      if (nrow(fb_row) == 1) {
        csb_cat <- fb_row$csb
      }
    }

    # Skip if still unclassified (unusual variables like h32x "other")
    if (is.na(csb_cat)) next

    result <- rbind(result, data.frame(
      variable = v, csb = csb_cat, stringsAsFactors = FALSE
    ))
  }

  if (nrow(result) > 0) {
    # Log the classification
    cats <- table(result$csb)
    cat_str <- paste(
      names(cats), cats, sep = ": ", collapse = ", "
    )
    cli::cli_alert_info(
      "CSB classification from labels ({nrow(result)} vars): {cat_str}"
    )
  }

  result
}


# ---------------------------------------------------------------------------
# Custom CSB indicator helpers (runtime-scoped)
#
# These helpers support the optional `custom_csb_indicator` argument of
# `run_mbg_pipeline()`, which lets a user define a single mutually exclusive
# care-seeking partition (`<name>_dhis`, `<name>_nondhis`, `<name>_untreat`)
# from three lists of treatment-source labels. The helpers are intentionally
# kept separate from `.classify_csb_from_h32()` so the built-in CSB pipeline
# remains untouched.
# ---------------------------------------------------------------------------

#' Built-in CSB indicator codes that custom names must not collide with.
#' @noRd
.builtin_csb_indicator_codes <- function() {
  c(
    "csb_any", "csb_public", "csb_pub_nochw", "csb_chw",
    "csb_private", "csb_priv_formal", "csb_pharmacy",
    "csb_priv_informal", "csb_priv_form_pha",
    "csb_trained", "csb_none",
    "csb_q1", "csb_q2", "csb_q3", "csb_q4", "csb_q5"
  )
}


#' Derived custom CSB sub-indicator names
#'
#' @param custom_csb_indicator Validated user spec list (must contain `name`).
#' @return Character vector `c("<name>_dhis", "<name>_nondhis", "<name>_untreat")`.
#' @noRd
.custom_csb_indicator_names <- function(custom_csb_indicator) {
  if (is.null(custom_csb_indicator)) return(character(0))
  paste0(
    custom_csb_indicator$name,
    c("_dhis", "_nondhis", "_untreat")
  )
}


#' Normalize a CSB label string for matching
#'
#' Lowercases, strips leading/trailing whitespace, and collapses runs of
#' internal whitespace to a single space. Used so user-supplied labels and
#' DHS haven labels match consistently across surveys with minor formatting
#' differences (extra spaces, mixed case).
#'
#' `NA` values are returned as-is so the helper can be vectorized over
#' character vectors that may contain `NA_character_`.
#'
#' @param x Character vector.
#' @return Character vector of the same length, lowercased and trimmed.
#' @noRd
.normalize_custom_csb_label <- function(x) {
  if (is.null(x)) return(character(0))
  if (length(x) == 0) return(character(0))
  out <- ifelse(is.na(x), NA_character_, tolower(trimws(as.character(x))))
  # Collapse runs of internal whitespace to a single space (only for non-NA)
  out <- ifelse(
    is.na(out), NA_character_, gsub("\\s+", " ", out, perl = TRUE)
  )
  out
}


#' Validate the structure of a `custom_csb_indicator` spec
#'
#' Performs all checks that can be done without a DHS dataset: presence of
#' the four required fields, type checks, name pattern, name collisions,
#' and pairwise-disjointness of the three label lists after normalization.
#' Per-survey label coverage is checked separately by
#' `.build_custom_csb_classification()`.
#'
#' @param custom_csb_indicator A list with `name`, `dhis_locs`,
#'   `nondhis_locs`, `untreat_locs`.
#' @param other_indicators Optional character vector of other indicator codes
#'   in the current pipeline run. Used to flag derived-name collisions.
#' @return Invisibly returns the spec with normalized label vectors attached
#'   as attributes (`label_norm_dhis`, etc.) for downstream reuse.
#' @noRd
.validate_custom_csb_indicator_spec <- function(
  custom_csb_indicator,
  other_indicators = character(0)
) {
  spec <- custom_csb_indicator
  if (!is.list(spec) || is.data.frame(spec)) {
    cli::cli_abort(
      "{.arg custom_csb_indicator} must be a named list."
    )
  }

  required <- c("name", "dhis_locs", "nondhis_locs", "untreat_locs")
  missing_fields <- setdiff(required, names(spec))
  if (length(missing_fields) > 0) {
    cli::cli_abort(c(
      "{.arg custom_csb_indicator} is missing required field{?s}: {.val {missing_fields}}",
      "i" = "Required fields: {.val {required}}"
    ))
  }

  # name -- single non-empty string, regex constrained, no collision
  nm <- spec$name
  if (!is.character(nm) || length(nm) != 1L || is.na(nm) || !nzchar(nm)) {
    cli::cli_abort(
      "{.field custom_csb_indicator$name} must be a single non-empty character."
    )
  }
  if (!grepl("^csb_[a-z0-9_]+$", nm)) {
    cli::cli_abort(c(
      "{.field custom_csb_indicator$name} = {.val {nm}} is not a valid identifier.",
      "i" = "Must match pattern {.code ^csb_[a-z0-9_]+$} (lowercase letters, digits, underscores)."
    ))
  }
  builtin <- .builtin_csb_indicator_codes()
  if (nm %in% builtin) {
    cli::cli_abort(c(
      "{.field custom_csb_indicator$name} = {.val {nm}} collides with a built-in CSB indicator.",
      "i" = "Pick a different prefix (e.g. {.val csb_eff})."
    ))
  }
  derived <- paste0(nm, c("_dhis", "_nondhis", "_untreat"))
  collide_derived <- intersect(derived, builtin)
  if (length(collide_derived) > 0) {
    cli::cli_abort(c(
      "Derived custom CSB names collide with built-in indicators: {.val {collide_derived}}",
      "i" = "Pick a different prefix for {.field custom_csb_indicator$name}."
    ))
  }
  collide_other <- intersect(derived, other_indicators)
  if (length(collide_other) > 0) {
    cli::cli_abort(c(
      "Derived custom CSB names collide with user-requested indicators: {.val {collide_other}}",
      "i" = "Pick a different prefix for {.field custom_csb_indicator$name} or remove the conflicting indicators."
    ))
  }

  # label vectors -- character, NA only allowed in untreat_locs
  for (slot in c("dhis_locs", "nondhis_locs", "untreat_locs")) {
    val <- spec[[slot]]
    if (!is.character(val)) {
      cli::cli_abort(
        "{.field custom_csb_indicator${slot}} must be a character vector."
      )
    }
  }
  if (anyNA(spec$dhis_locs)) {
    cli::cli_abort(
      "{.field custom_csb_indicator$dhis_locs} must not contain NA values."
    )
  }
  if (anyNA(spec$nondhis_locs)) {
    cli::cli_abort(
      "{.field custom_csb_indicator$nondhis_locs} must not contain NA values."
    )
  }

  # Each *_locs entry may be either an h32 variable name (matches
  # ^h32[a-z0-9]+$) or a haven label string. Split the two so the
  # classification builder can resolve var names first and labels second.
  is_h32_varname <- function(x) {
    !is.na(x) & grepl("^h32[a-z0-9]+$", x)
  }
  split_spec <- function(vec) {
    vec_chr <- as.character(vec)
    is_var <- is_h32_varname(vec_chr)
    list(
      vars   = unique(vec_chr[is_var & !is.na(vec_chr)]),
      labels = vec_chr[!is_var | is.na(vec_chr)]
    )
  }
  parts_dhis    <- split_spec(spec$dhis_locs)
  parts_nondhis <- split_spec(spec$nondhis_locs)
  parts_untreat <- split_spec(spec$untreat_locs)

  # Normalize labels (NAs in untreat are tolerated but ignored for
  # conflict detection since they cannot conflict with non-NA labels).
  norm_dhis    <- unique(.normalize_custom_csb_label(parts_dhis$labels))
  norm_nondhis <- unique(.normalize_custom_csb_label(parts_nondhis$labels))
  norm_untreat <- unique(.normalize_custom_csb_label(parts_untreat$labels))
  norm_untreat_nona <- norm_untreat[!is.na(norm_untreat)]

  # Pairwise disjointness on labels
  label_pairs <- list(
    list("dhis_locs", "nondhis_locs", intersect(norm_dhis, norm_nondhis)),
    list("dhis_locs", "untreat_locs", intersect(norm_dhis, norm_untreat_nona)),
    list("nondhis_locs", "untreat_locs",
         intersect(norm_nondhis, norm_untreat_nona))
  )
  label_overlaps <- Filter(function(p) length(p[[3]]) > 0, label_pairs)

  # Pairwise disjointness on var names
  var_pairs <- list(
    list("dhis_locs", "nondhis_locs",
         intersect(parts_dhis$vars, parts_nondhis$vars)),
    list("dhis_locs", "untreat_locs",
         intersect(parts_dhis$vars, parts_untreat$vars)),
    list("nondhis_locs", "untreat_locs",
         intersect(parts_nondhis$vars, parts_untreat$vars))
  )
  var_overlaps <- Filter(function(p) length(p[[3]]) > 0, var_pairs)

  overlaps <- c(label_overlaps, var_overlaps)
  if (length(overlaps) > 0) {
    msgs <- vapply(overlaps, function(p) {
      sprintf("%s & %s: %s", p[[1]], p[[2]],
              paste(p[[3]], collapse = ", "))
    }, character(1))
    cli::cli_abort(c(
      "{.field custom_csb_indicator} entries overlap (must be disjoint after normalization):",
      stats::setNames(msgs, rep("x", length(msgs)))
    ))
  }

  attr(spec, "label_norm_dhis")    <- norm_dhis
  attr(spec, "label_norm_nondhis") <- norm_nondhis
  attr(spec, "label_norm_untreat") <- norm_untreat
  attr(spec, "vars_dhis")    <- parts_dhis$vars
  attr(spec, "vars_nondhis") <- parts_nondhis$vars
  attr(spec, "vars_untreat") <- parts_untreat$vars
  invisible(spec)
}


#' Extract usable h32 variable labels from a DHS KR dataset
#'
#' Reads haven labels from each `^h32[a-z0-9]+$` variable in `dhs_kr`,
#' skipping `h32y`/`h32z` (no-treatment / medical-treatment flags) and any
#' slot whose label is `NA - ...` (DHS country-specific placeholder for
#' unused slots). Labels must be read **before** any zap/coercion.
#'
#' Variable names listed in `force_keep_vars` are always kept even when
#' their label is empty or starts with `"NA -"`, because the caller is
#' explicitly routing those columns by variable name (the resulting row
#' will carry `raw_label = ""` / `label_norm = NA_character_` which is
#' fine: var-name matching wins over label matching downstream).
#'
#' @param dhs_kr DHS Children's Recode with haven labels intact.
#' @param force_keep_vars Optional character vector of h32 variable names
#'   that must be retained regardless of label content (e.g. user-listed
#'   slots with country-specific `NA -` placeholders).
#' @return Tibble with columns `variable`, `raw_label`, `label_norm`. Rows
#'   are returned only for slots that have a usable scalar character label
#'   OR appear in `force_keep_vars`.
#' @noRd
.extract_custom_csb_h32_labels <- function(dhs_kr,
                                           force_keep_vars = character(0)) {
  if (!is.data.frame(dhs_kr)) {
    cli::cli_abort("`dhs_kr` must be a data.frame or tibble.")
  }
  available_h32 <- grep("^h32[a-z0-9]+$", names(dhs_kr), value = TRUE)
  meta_vars <- c("h32y", "h32z")
  # `force_keep_vars` may include h32y/h32z if the caller really wants
  # them in their custom partition; honour the explicit user listing.
  meta_vars <- setdiff(meta_vars, force_keep_vars)
  available_h32 <- setdiff(available_h32, meta_vars)
  if (length(available_h32) == 0) {
    return(tibble::tibble(
      variable = character(0),
      raw_label = character(0),
      label_norm = character(0)
    ))
  }

  rows <- list()
  for (v in available_h32) {
    lbl <- attr(dhs_kr[[v]], "label")
    forced <- v %in% force_keep_vars
    has_lbl <- is.character(lbl) && length(lbl) == 1L &&
      !is.na(lbl) && nzchar(lbl) && !grepl("^NA\\s*-", lbl)
    if (!has_lbl && !forced) next

    raw <- if (has_lbl) lbl else ""
    norm <- if (has_lbl) {
      .normalize_custom_csb_label(lbl)
    } else {
      NA_character_
    }
    rows[[length(rows) + 1L]] <- tibble::tibble(
      variable = v,
      raw_label = raw,
      label_norm = norm
    )
  }

  if (length(rows) == 0) {
    return(tibble::tibble(
      variable = character(0),
      raw_label = character(0),
      label_norm = character(0)
    ))
  }
  dplyr::bind_rows(rows)
}


#' Build a slot-to-bucket lookup for a custom CSB spec against one survey
#'
#' Maps each usable `h32*` variable in `dhs_kr` to one of `dhis`, `nondhis`,
#' or `untreat` based on the user-supplied label lists. Validates that every
#' observed label is mapped exactly once. User-supplied labels that are not
#' present in the current survey are tolerated (the spec is treated as a
#' superset across surveys).
#'
#' @param dhs_kr DHS Children's Recode with haven labels intact.
#' @param custom_csb_indicator Validated user spec.
#' @return Tibble with columns `variable`, `csb_custom` (one of "dhis",
#'   "nondhis", "untreat"), `raw_label`, `label_norm`.
#' @noRd
.build_custom_csb_classification <- function(dhs_kr, custom_csb_indicator) {
  spec <- .validate_custom_csb_indicator_spec(custom_csb_indicator)

  norm_dhis    <- attr(spec, "label_norm_dhis")
  norm_nondhis <- attr(spec, "label_norm_nondhis")
  norm_untreat <- attr(spec, "label_norm_untreat")
  norm_untreat_nona <- norm_untreat[!is.na(norm_untreat)]

  vars_dhis    <- attr(spec, "vars_dhis")    %||% character(0)
  vars_nondhis <- attr(spec, "vars_nondhis") %||% character(0)
  vars_untreat <- attr(spec, "vars_untreat") %||% character(0)

  # Pass explicit var-name listings so country-specific "NA -" placeholders
  # are still kept when the user has actively routed them by variable name.
  observed <- .extract_custom_csb_h32_labels(
    dhs_kr,
    force_keep_vars = unique(c(vars_dhis, vars_nondhis, vars_untreat))
  )
  if (nrow(observed) == 0) {
    cli::cli_warn(
      "No usable h32 labels found in {.arg dhs_kr}; custom CSB indicator will be empty."
    )
    return(tibble::tibble(
      variable = character(0),
      csb_custom = character(0),
      raw_label = character(0),
      label_norm = character(0)
    ))
  }

  # Var-name routing wins over label routing because it is more specific
  # (it disambiguates h32 columns that share an identical haven label,
  # e.g. h32e and h32n both labelled "Fever/cough: comm.health wrkr").
  observed$csb_custom <- dplyr::case_when(
    observed$variable %in% vars_dhis    ~ "dhis",
    observed$variable %in% vars_nondhis ~ "nondhis",
    observed$variable %in% vars_untreat ~ "untreat",
    observed$label_norm %in% norm_dhis    ~ "dhis",
    observed$label_norm %in% norm_nondhis ~ "nondhis",
    observed$label_norm %in% norm_untreat_nona ~ "untreat",
    TRUE ~ NA_character_
  )

  unmapped <- observed[is.na(observed$csb_custom), , drop = FALSE]
  if (nrow(unmapped) > 0) {
    msgs <- sprintf(
      "%s: %s",
      unmapped$variable,
      ifelse(nzchar(unmapped$raw_label), unmapped$raw_label, "<no label>")
    )
    # Non-fatal: any child whose only positive h32 source falls in an
    # unmapped slot will be classified as `untreat` via the residual rule
    # in .classify_custom_csb_from_h32(). This keeps the pipeline running
    # for surveys that include extra country-specific h32 slots the user
    # did not enumerate.
    cli::cli_alert_info(c(
      "Custom CSB: {nrow(unmapped)} h32 column{?s} not listed in any of {.field dhis_locs}, {.field nondhis_locs}, {.field untreat_locs} (skipped):",
      stats::setNames(msgs, rep("*", length(msgs)))
    ))
    observed <- observed[!is.na(observed$csb_custom), , drop = FALSE]
  }

  # Informational: list extra user-supplied entries not present in this survey
  used_vars <- observed$variable
  used_norm <- observed$label_norm
  extra_var_dhis    <- setdiff(vars_dhis,    used_vars)
  extra_var_nondhis <- setdiff(vars_nondhis, used_vars)
  extra_var_untreat <- setdiff(vars_untreat, used_vars)
  extra_lab_dhis    <- setdiff(norm_dhis,    used_norm)
  extra_lab_nondhis <- setdiff(norm_nondhis, used_norm)
  extra_lab_untreat <- setdiff(norm_untreat_nona, used_norm)
  n_extra <- length(extra_var_dhis) + length(extra_var_nondhis) +
    length(extra_var_untreat) + length(extra_lab_dhis) +
    length(extra_lab_nondhis) + length(extra_lab_untreat)
  if (n_extra > 0) {
    cli::cli_alert_info(
      "Custom CSB: {n_extra} user-supplied entr{?y/ies} not present in this survey (ignored)."
    )
  }

  observed[, c("variable", "csb_custom", "raw_label", "label_norm")]
}


#' Classify febrile-U5 children into a custom CSB partition
#'
#' Adds three mutually exclusive 0/1 columns to `data` named
#' `<prefix>_dhis`, `<prefix>_nondhis`, `<prefix>_untreat`. Children with
#' no positive `h32*` slot are classified as `untreat`. When a child reports
#' positive sources spanning multiple buckets, priority is
#' `dhis > nondhis > untreat` so the cluster numerators sum to the
#' denominator by construction.
#'
#' @param data Data frame already filtered to the target population.
#' @param h32_cols Character vector of h32 column names present in `data`.
#' @param classification Tibble from `.build_custom_csb_classification()`
#'   with columns `variable`, `csb_custom`.
#' @param prefix User-supplied indicator prefix (`custom_csb_indicator$name`).
#' @return The input data with three new 0/1 columns.
#' @noRd
.classify_custom_csb_from_h32 <- function(
  data,
  h32_cols,
  classification,
  prefix
) {
  col_dhis    <- paste0(prefix, "_dhis")
  col_nondhis <- paste0(prefix, "_nondhis")
  col_untreat <- paste0(prefix, "_untreat")

  if (!is.data.frame(data) || nrow(data) == 0) {
    data[[col_dhis]] <- integer(0)
    data[[col_nondhis]] <- integer(0)
    data[[col_untreat]] <- integer(0)
    return(data)
  }

  # Restrict h32 columns to those present in BOTH the data and the
  # classification (a column may be missing from classification if its
  # label was empty / "NA - ..." / not detectable; treat it as absent).
  h32_cols <- intersect(intersect(h32_cols, names(data)),
                        classification$variable)

  if (length(h32_cols) == 0) {
    # No usable slots -> everyone is untreat
    data[[col_dhis]] <- 0L
    data[[col_nondhis]] <- 0L
    data[[col_untreat]] <- 1L
    return(data)
  }

  # Coerce h32 columns to plain numeric (defensive; .prepare_csb_data
  # already zaps labels, but this function may be called directly in
  # tests OR on a KR frame that still carries haven_labelled attributes.
  # `pivot_longer()` over labelled columns can produce a `haven_labelled`
  # value column whose `== 1` comparison silently misbehaves, so we zap
  # labels explicitly before any reshape.
  for (col in h32_cols) {
    x <- data[[col]]
    if (inherits(x, "haven_labelled")) {
      x <- haven::zap_labels(x)
    }
    data[[col]] <- suppressWarnings(as.numeric(as.character(x)))
  }

  # IMPORTANT: use seq_len(nrow(data)) — `dplyr::row_number(data)` on a
  # data frame ranks the first column's values, NOT row positions, which
  # produces duplicate `.csb_custom_row_id`s and causes the downstream
  # left_join to broadcast positives across many children (everyone
  # ended up in `_dhis`). See regression test below.
  data$.csb_custom_row_id <- seq_len(nrow(data))

  long <- data |>
    dplyr::select(.csb_custom_row_id, dplyr::all_of(h32_cols)) |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(h32_cols),
      names_to = "variable",
      values_to = "visited"
    ) |>
    dplyr::filter(!is.na(visited) & visited == 1) |>
    dplyr::left_join(
      classification[, c("variable", "csb_custom")],
      by = "variable"
    )

  per_child <- if (nrow(long) > 0) {
    long |>
      dplyr::group_by(.csb_custom_row_id) |>
      dplyr::summarise(
        had_dhis    = as.integer(any(csb_custom == "dhis")),
        had_nondhis = as.integer(any(csb_custom == "nondhis")),
        had_untreat = as.integer(any(csb_custom == "untreat")),
        .groups = "drop"
      )
  } else {
    tibble::tibble(
      .csb_custom_row_id = integer(0),
      had_dhis = integer(0),
      had_nondhis = integer(0),
      had_untreat = integer(0)
    )
  }

  data <- data |>
    dplyr::left_join(per_child, by = ".csb_custom_row_id") |>
    dplyr::mutate(
      had_dhis    = tidyr::replace_na(had_dhis, 0L),
      had_nondhis = tidyr::replace_na(had_nondhis, 0L),
      had_untreat = tidyr::replace_na(had_untreat, 0L)
    )

  # Mutually exclusive assignment with priority dhis > nondhis > untreat.
  # Children with no positive slot fall into _untreat by construction.
  data[[col_dhis]] <- as.integer(data$had_dhis == 1L)
  data[[col_nondhis]] <- as.integer(
    data$had_dhis == 0L & data$had_nondhis == 1L
  )
  data[[col_untreat]] <- as.integer(
    data$had_dhis == 0L & data$had_nondhis == 0L
  )

  data$.csb_custom_row_id <- NULL
  data$had_dhis <- NULL
  data$had_nondhis <- NULL
  data$had_untreat <- NULL
  data
}


# ---- dhs_calc_csb_custom_mbg.R ----

#' Prepare a Custom CSB Partition for MBG Analysis
#'
#' Internal MBG-prep function that computes one user-defined, mutually
#' exclusive care-seeking partition from a DHS Children's Recode (KR)
#' dataset. The partition is defined at runtime by the user via the
#' `custom_csb_indicator` argument and produces three derived cluster-level
#' indicators:
#'
#' \itemize{
#'   \item `<name>_dhis`     - sought care at any user-listed DHIS source
#'   \item `<name>_nondhis`  - sought care at any non-DHIS source (and
#'     never at a DHIS source)
#'   \item `<name>_untreat`  - did not seek care at any positive `h32*`
#'     source, or only at user-listed untreat sources
#' }
#'
#' The triple is mutually exclusive at the child level: each febrile U5
#' child is assigned to exactly one bucket via the priority rule
#' `dhis > nondhis > untreat`.
#'
#' @param dhs_kr DHS Children's Recode (KR) dataset (haven labels intact).
#' @param gps_data DHS GPS dataset with cluster coordinates.
#' @param custom_csb_indicator A validated spec list with fields `name`,
#'   `dhis_locs`, `nondhis_locs`, `untreat_locs`. See
#'   \code{run_mbg_pipeline()} for the public interface.
#' @param survey_vars Named list mapping DHS variable names. Must include
#'   `cluster`, `age`, `fever`.
#' @param gps_vars Named list for GPS variable mapping with keys `cluster`,
#'   `lat`, `lon`.
#'
#' @return A named list of data.tables (one per derived indicator), each
#'   with columns `cluster_id`, `indicator`, `samplesize`, `x`, `y`. The
#'   list is keyed by the derived indicator codes
#'   (`<name>_dhis`, `<name>_nondhis`, `<name>_untreat`).
#'
#' @details
#' Step 1 builds a per-survey label-to-bucket lookup from the original
#' KR file (where haven labels are still intact). Step 2 reuses
#' \code{.prepare_csb_data()} with `csb_priority_method = "all"` to obtain
#' the febrile-U5 cleaned dataset (the built-in `csb_priority_method`
#' setting does not apply to the custom partition because the custom
#' triple is always mutually exclusive by construction). Step 3 classifies
#' each febrile child into exactly one custom bucket. Step 4 aggregates
#' each bucket to cluster level via the shared
#' \code{.aggregate_to_mbg_clusters()} helper.
#'
#' @noRd
calc_csb_custom_mbg <- function(
  dhs_kr,
  gps_data,
  custom_csb_indicator,
  survey_vars = list(
    cluster = "v001",
    age = "hw1",
    fever = "h22"
  ),
  gps_vars = list(
    cluster = "DHSCLUST",
    lat = "LATNUM",
    lon = "LONGNUM"
  )
) {
  if (!is.data.frame(dhs_kr)) {
    cli::cli_abort("`dhs_kr` must be a data.frame or tibble")
  }
  if (!is.data.frame(gps_data)) {
    cli::cli_abort("`gps_data` must be a data.frame or tibble")
  }

  spec <- .validate_custom_csb_indicator_spec(custom_csb_indicator)
  prefix <- spec$name
  out_names <- .custom_csb_indicator_names(spec)

  # ---- Build the per-survey custom slot-to-bucket classification ----
  # Labels must be read from the ORIGINAL `dhs_kr` (before any zap_labels()
  # downstream) so haven labels are still intact.
  classification <- .build_custom_csb_classification(dhs_kr, spec)

  # ---- Prepare GPS clusters ----
  gps_clean <- .prepare_gps_data(gps_data, gps_vars)

  # ---- Prepare febrile U5 dataset ----
  # We use csb_priority_method = "all" because the built-in priority
  # resolution doesn't matter for the custom triple: we re-derive
  # mutually exclusive buckets directly from raw h32 columns below.
  kr_fever <- .prepare_csb_data(
    dhs_kr = dhs_kr,
    survey_vars = survey_vars,
    include_survey_vars = FALSE,
    csb_priority_method = "all"
  )

  if (is.null(kr_fever) || nrow(kr_fever) == 0) {
    cli::cli_alert_warning(
      "Custom CSB ({.val {prefix}}): no febrile U5 records; returning empty result."
    )
    return(stats::setNames(vector("list", length(out_names)), out_names))
  }

  # h32 columns present in the cleaned febrile dataset
  h32_cols <- grep("^h32[a-z0-9]+$", names(kr_fever), value = TRUE)
  h32_cols <- setdiff(h32_cols, c("h32y", "h32z"))

  # ---- Classify into <prefix>_dhis / _nondhis / _untreat ----
  kr_fever <- .classify_custom_csb_from_h32(
    data = kr_fever,
    h32_cols = h32_cols,
    classification = classification,
    prefix = prefix
  )

  # ---- Aggregate each derived column to cluster level ----
  results <- list()
  for (out_name in out_names) {
    if (!out_name %in% names(kr_fever)) next
    dt <- .aggregate_to_mbg_clusters(
      individual_data = kr_fever,
      indicator_col = out_name,
      gps_clean = gps_clean,
      result_name = out_name
    )
    if (!is.null(dt)) {
      results[[out_name]] <- dt
    }
  }

  if (length(results) == 0) {
    cli::cli_alert_warning(
      "Custom CSB ({.val {prefix}}): no valid clusters produced."
    )
  }

  results
}


# ---- dhs_calc_csb_by_wealth_dhs.R ----

#' Calculate Care-Seeking Behavior by Wealth Quintile from DHS Data
#'
#' Estimates care-seeking behavior for febrile children under 5, stratified
#' by wealth quintile. Uses WHO World Malaria Report methodology with
#' survey-weighted estimates.
#'
#' @details
#' This function extends [calc_csb_dhs()] to provide wealth-stratified estimates.
#' Each wealth quintile produces separate survey-weighted estimates with
#' confidence intervals.
#'
#' Methodology: \url{https://github.com/ahadi-analytics/sntmethods/blob/master/inst/methods/csb_dhs.yml}
#'
#' @param dhs_kr DHS children's recode (KR) dataset in tidy format.
#' @param survey_vars Named list mapping DHS variable names. See [calc_csb_dhs_core()]
#'   for details.
#' @param quintiles Numeric vector of wealth quintiles to include. Default: 1:5
#'   (all quintiles). Use c(1) for poorest only, c(1,2) for poorest + poorer, etc.
#' @param wealth_var Name of wealth quintile variable in dhs_kr. Default: "v190".
#' @param csb_priority_method Character, one of "all" (default), "first",
#'   "public", or "private". Controls how overlapping care-seeking records
#'   are resolved so each individual is assigned to at most one sector.
#'   See [calc_csb_mbg()] for details. With non-"all" values, csb_public +
#'   csb_private + csb_none sums to 100% within each quintile.
#' @param region_var Optional column name for regional grouping.
#' @param ci_method Method for confidence intervals. Default: "logit".
#'
#' @return Named list of tibbles with two levels:
#'   \describe{
#'     \item{`adm0`}{National-level estimates by wealth quintile (always present)}
#'     \item{`adm1`}{Admin-1 estimates by wealth quintile (when region_var provided)}
#'   }
#'   Each tibble contains columns: survey_id, iso3, iso2, survey_type,
#'   survey_year, adm0, adm1 (if applicable), wealth_quintile, type,
#'   geo_source, point, ci_l, ci_u, numerator, denominator, indicator,
#'   indicator_code, numerator_description, denominator_description,
#'   denominator_code.
#'
#' @section Indicators:
#' The function calculates these indicators (overlapping, not mutually exclusive):
#' \itemize{
#'   \item `csb_any`: Sought care anywhere
#'   \item `csb_public`: Public sector (including CHW)
#'   \item `csb_pub_nochw`: Public sector excluding CHW
#'   \item `csb_chw`: Community health worker
#'   \item `csb_private`: Any private sector
#'   \item `csb_priv_formal`: Private formal sector
#'   \item `csb_pharmacy`: Pharmacy/drug shop
#'   \item `csb_priv_informal`: Private informal
#'   \item `csb_priv_form_pha`: Private formal or pharmacy
#'   \item `csb_trained`: Trained provider
#'   \item `csb_none`: Did not seek care
#' }
#'
#' @examples
#' \dontrun{
#' # Care-seeking for poorest quintile only
#' csb_poorest <- calc_csb_by_wealth_dhs(
#'   dhs_kr = kr_data,
#'   quintiles = 1
#' )
#'
#' # Compare all quintiles nationally
#' csb_all <- calc_csb_by_wealth_dhs(
#'   dhs_kr = kr_data,
#'   quintiles = 1:5
#' )
#'
#' # Regional estimates for poorest and richest
#' csb_regional <- calc_csb_by_wealth_dhs(
#'   dhs_kr = kr_data,
#'   quintiles = c(1, 5),
#'   region_var = "v024"
#' )
#' }
#'
#' @seealso
#' * [calc_csb_by_wealth_mbg()] for wealth-stratified MBG cluster data
#' * [calc_csb_dhs()] for standard survey-weighted estimates
#' * [calc_csb_mbg()] for non-stratified MBG data
#' @export
calc_csb_by_wealth_dhs <- function(
  dhs_kr,
  survey_vars = list(
    cluster = "v021",
    weight = "v005",
    stratum = "v022",
    age = "hw1",
    fever = "h22",
    alive = "b5"
  ),
  quintiles = 1:5,
  wealth_var = "v190",
  csb_priority_method = c("all", "first", "public", "private"),
  region_var = NULL,
  ci_method = "logit"
) {
  # Fail fast on missing suggested dependencies
  .check_pkg(
    c("purrr", "tibble"),
    reason = "for `calc_csb_by_wealth_dhs()`"
  )

  csb_priority_method <- match.arg(csb_priority_method)
  # ---- 1. Input validation ----

  if (!is.data.frame(dhs_kr)) {
    cli::cli_abort("`dhs_kr` must be a data.frame or tibble.")
  }
  if (nrow(dhs_kr) == 0) {
    cli::cli_abort("`dhs_kr` is empty.")
  }

  # Validate quintiles
  if (!all(quintiles %in% 1:5)) {
    cli::cli_abort("`quintiles` must be numeric values between 1 and 5")
  }

  # Check required survey variables
  needed_cols <- c(survey_vars$cluster, survey_vars$weight,
                   survey_vars$stratum, survey_vars$age, survey_vars$fever)
  missing_cols <- setdiff(needed_cols, names(dhs_kr))
  if (length(missing_cols) > 0) {
    cli::cli_abort(c(
      "Required variables not found: {.var {missing_cols}}",
      "i" = "Check your survey_vars mapping"
    ))
  }

  # ---- 2. Extract survey metadata ----

  survey_meta <- .extract_survey_meta(dhs_kr)

  # ---- 3. Prepare CSB data ----

  kr_fever <- .prepare_csb_data(
    dhs_kr = dhs_kr,
    survey_vars = survey_vars,
    include_survey_vars = TRUE,
    csb_priority_method = csb_priority_method
  )

  # Add wealth quintile and filter
  kr_fever <- .add_wealth_quintile(
    dhs_data = kr_fever,
    wealth_var = wealth_var,
    quintiles = quintiles
  )

  cli::cli_alert_success(
    "Under 5 with fever: {format(nrow(kr_fever), big.mark = ',')} children"
  )

  # ---- 4. Region grouping ----

  group_var <- NULL

  # Auto-fallback to v024 when no region specified
  if (is.null(region_var)) {
    if ("v024" %in% names(dhs_kr)) {
      region_var <- "v024"
      cli::cli_alert_info(
        "No region_var specified; defaulting to {.var v024} for adm1"
      )
    }
  }

  if (!is.null(region_var)) {
    if (!region_var %in% names(dhs_kr)) {
      cli::cli_abort("Column {.var {region_var}} not found in `dhs_kr`.")
    }
    kr_fever$region <- .resolve_region_labels(
      dhs_kr[[region_var]], region_var
    )
    # Align with filtered febrile children
    valid_idx <- which(!is.na(dhs_kr[[survey_vars$fever]]) &
                       dhs_kr[[survey_vars$fever]] == 1)
    kr_fever$region <- .resolve_region_labels(
      dhs_kr[[region_var]][valid_idx], region_var
    )[seq_len(nrow(kr_fever))]
    group_var <- "region"
    geo_src <- "survey"
  }

  # ---- 5. Get conditions and compute indicators ----

  conds <- .csb_conditions()

  meta_cols <- tibble::tibble(
    survey_id   = survey_meta$survey_id,
    iso3        = survey_meta$iso3,
    iso2        = survey_meta$iso2,
    survey_type = survey_meta$survey_type,
    survey_year = survey_meta$survey_year,
    adm0        = survey_meta$country_upper
  )

  .round_results <- function(tbl) {
    tbl |>
      dplyr::mutate(
        point       = round(point, 3),
        ci_l        = round(pmax(ci_l, 0, na.rm = TRUE), 3),
        ci_u        = round(pmin(ci_u, 1, na.rm = TRUE), 3),
        numerator   = as.integer(numerator),
        denominator = as.integer(denominator)
      )
  }

  # --- adm0 (national) by wealth quintile ---
  national_results <- purrr::map_dfr(conds, function(cond) {
    .compute_dhs_indicator_by_wealth(
      data = kr_fever,
      condition = cond,
      group_var = NULL,
      ci_method = ci_method,
      quintiles = quintiles
    )
  })

  national_results <- .round_results(national_results)

  adm0_tbl <- dplyr::bind_cols(
    meta_cols[rep(1, nrow(national_results)), ],
    tibble::tibble(type = "survey_weighted", geo_source = NA_character_),
    national_results |> dplyr::select(-level, -location)
  ) |>
    tibble::as_tibble()

  out <- list(adm0 = adm0_tbl)

  # --- adm1 (subnational) by wealth quintile ---
  if (!is.null(group_var)) {
    sub_results <- purrr::map_dfr(conds, function(cond) {
      .compute_dhs_indicator_by_wealth(
        data = kr_fever,
        condition = cond,
        group_var = group_var,
        subnational_level = "adm1",
        ci_method = ci_method,
        quintiles = quintiles
      )
    })

    # Filter to regional rows only
    sub_results <- sub_results |>
      dplyr::filter(level != "adm0")

    if (nrow(sub_results) > 0) {
      sub_results <- .round_results(sub_results)

      sub_tbl <- dplyr::bind_cols(
        meta_cols[rep(1, nrow(sub_results)), ],
        sub_results |>
          dplyr::transmute(
            adm1       = toupper(location),
            wealth_quintile,
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

  out
}


# ---- dhs_calc_csb_by_wealth_mbg.R ----

#' Prepare Care-Seeking Behavior Data by Wealth Quintile for MBG Analysis
#'
#' Prepares cluster-level care-seeking behavior data stratified by wealth
#' quintile for MBG analysis. Calculates proportions of febrile children who
#' sought care, separately for each wealth quintile.
#'
#' @details
#' This function extends [calc_csb_mbg()] to provide wealth-stratified estimates.
#' Each quintile produces separate MBG-ready outputs with cluster-level
#' numerators and denominators.
#'
#' **Important:** This is a standalone utility function for specialized wealth
#' stratification analysis. It is NOT called by [run_mbg_pipeline()]. Use this
#' function directly when you need wealth-disaggregated indicators for custom
#' MBG modeling or equity analysis.
#'
#' Methodology: \url{https://github.com/ahadi-analytics/sntmethods/blob/master/inst/methods/csb_dhs.yml}
#'
#' @param dhs_kr DHS Children's Recode (KR) dataset.
#' @param gps_data DHS GPS dataset with cluster coordinates.
#' @param indicators Character vector of indicators to calculate:
#'   \itemize{
#'     \item "any": Sought care anywhere (public or private)
#'     \item "public": Public sector including CHW
#'     \item "pub_nochw": Public sector excluding CHW
#'     \item "chw": Community health worker only
#'     \item "private": Any private sector
#'     \item "priv_formal": Private formal sector only
#'     \item "pharmacy": Pharmacy / drug shop only
#'     \item "priv_informal": Private informal only
#'     \item "priv_form_pha": Private formal or pharmacy
#'     \item "trained": Trained provider (public + formal + pharmacy)
#'     \item "none": Did not seek care
#'   }
#'   Default: c("any", "public", "private", "none").
#' @param quintiles Numeric vector of wealth quintiles to include. Default: 1:5
#'   (all quintiles). Use c(1) for poorest only, c(1,2) for poorest + poorer, etc.
#' @param wealth_var Name of wealth quintile variable in dhs_kr. Default: "v190".
#' @param csb_priority_method Character, one of "all" (default), "first",
#'   "public", or "private". Controls how overlapping care-seeking records
#'   are resolved so each individual is assigned to at most one sector.
#'   See [calc_csb_mbg()] for details. With non-"all" values, csb_public +
#'   csb_private + csb_none sums to 100% within each quintile.
#' @param survey_vars Named list mapping DHS variable names.
#' @param gps_vars Named list for GPS variable mapping.
#'
#' @return A nested list structure: first level keys are quintiles (e.g., "q1",
#'   "q2"), second level keys are indicators (e.g., "csb_public_q1"). Each
#'   leaf is a data.table with columns:
#'   \itemize{
#'     \item cluster_id: Cluster identifier
#'     \item indicator: Numerator count (children who sought care)
#'     \item samplesize: Denominator count (all febrile children in quintile)
#'     \item x: Longitude
#'     \item y: Latitude
#'   }
#'
#' @section Output Structure:
#' The function returns a list where each indicator-quintile combination gets
#' its own data.table. For example, with indicators = c("public", "private")
#' and quintiles = c(1, 5):
#' \preformatted{
#' list(
#'   csb_public_q1 = data.table(...),
#'   csb_public_q5 = data.table(...),
#'   csb_private_q1 = data.table(...),
#'   csb_private_q5 = data.table(...)
#' )
#' }
#'
#' @examples
#' \dontrun{
#' # Care-seeking by wealth for poorest quintile only
#' csb_poorest <- calc_csb_by_wealth_mbg(
#'   dhs_kr = kr_data,
#'   gps_data = gps_data,
#'   indicators = c("public", "private"),
#'   quintiles = 1
#' )
#'
#' # Compare poorest vs richest quintiles
#' csb_comparison <- calc_csb_by_wealth_mbg(
#'   dhs_kr = kr_data,
#'   gps_data = gps_data,
#'   indicators = c("any", "public", "none"),
#'   quintiles = c(1, 5)
#' )
#' }
#'
#' @seealso
#' * [calc_csb_mbg()] for non-stratified care-seeking MBG data
#' * [calc_csb_by_wealth_dhs()] for wealth-stratified survey-weighted estimates
#' * [calc_csb_dhs()] for standard survey-weighted estimates
#' @export
calc_csb_by_wealth_mbg <- function(
  dhs_kr,
  gps_data,
  indicators = c("any", "public", "private", "none"),
  quintiles = 1:5,
  wealth_var = "v190",
  csb_priority_method = c("all", "first", "public", "private"),
  survey_vars = list(
    cluster = "v001",
    age = "hw1",
    fever = "h22"
  ),
  gps_vars = list(
    cluster = "DHSCLUST",
    lat = "LATNUM",
    lon = "LONGNUM"
  )
) {
  csb_priority_method <- match.arg(csb_priority_method)

  # ---- Input validation ----

  if (!is.data.frame(dhs_kr)) {
    cli::cli_abort("`dhs_kr` must be a data.frame or tibble")
  }
  if (!is.data.frame(gps_data)) {
    cli::cli_abort("`gps_data` must be a data.frame or tibble")
  }

  # Validate quintiles
  if (!all(quintiles %in% 1:5)) {
    cli::cli_abort("`quintiles` must be numeric values between 1 and 5")
  }

  # Accept both prefixed ("csb_public") and short ("public") forms
  indicators <- sub("^csb_", "", indicators)

  valid_indicators <- c(
    "any", "public", "pub_nochw", "chw",
    "private", "priv_formal", "pharmacy",
    "priv_informal", "priv_form_pha",
    "trained", "none"
  )
  invalid <- setdiff(indicators, valid_indicators)
  if (length(invalid) > 0) {
    cli::cli_abort(
      "Invalid indicators: {.val {invalid}}"
    )
  }

  # ---- Prepare data using shared helpers ----

  gps_clean <- .prepare_gps_data(gps_data, gps_vars)

  kr_fever <- .prepare_csb_data(
    dhs_kr = dhs_kr,
    survey_vars = survey_vars,
    include_survey_vars = FALSE,
    csb_priority_method = csb_priority_method
  )

  # Add wealth quintile and filter
  kr_fever <- .add_wealth_quintile(
    dhs_data = kr_fever,
    wealth_var = wealth_var,
    quintiles = quintiles
  )

  # ---- Calculate cluster-level indicators by wealth quintile ----

  # Maps short indicator name -> CSB column in data
  indicator_map <- list(
    any = "csb_any",
    public = "csb_public",
    pub_nochw = "csb_public_nochw",
    chw = "csb_chw",
    private = "csb_private",
    priv_formal = "csb_private_formal_ind",
    pharmacy = "csb_pharmacy",
    priv_informal = "csb_private_informal",
    priv_form_pha = "csb_private_formal_pha",
    trained = "csb_trained",
    none = "csb_none"
  )

  # Maps short indicator name -> output key base name
  result_names <- list(
    any = "csb_any",
    public = "csb_public",
    pub_nochw = "csb_pub_nochw",
    chw = "csb_chw",
    private = "csb_private",
    priv_formal = "csb_priv_formal",
    pharmacy = "csb_pharmacy",
    priv_informal = "csb_priv_informal",
    priv_form_pha = "csb_priv_form_pha",
    trained = "csb_trained",
    none = "csb_none"
  )

  results <- list()

  for (ind in indicators) {
    indicator_col <- indicator_map[[ind]]
    result_name <- result_names[[ind]]

    # Aggregate by wealth quintile
    wealth_results <- .aggregate_to_mbg_clusters_by_wealth(
      individual_data = kr_fever,
      indicator_col = indicator_col,
      gps_clean = gps_clean,
      result_name = result_name,
      quintiles = quintiles
    )

    # Merge into main results list
    results <- c(results, wealth_results)
  }

  if (length(results) == 0) {
    cli::cli_abort("No valid MBG data could be prepared")
  }

  cli::cli_alert_success(
    "Prepared {length(results)} indicator-quintile combinations"
  )

  results
}


#' Prepare Single CSB Indicator by Wealth Quintile for MBG
#'
#' Convenience wrapper around [calc_csb_by_wealth_mbg()] to prepare a single
#' care-seeking indicator stratified by wealth quintile.
#'
#' @inheritParams calc_csb_by_wealth_mbg
#' @param indicator Single indicator name. Default: "public".
#'
#' @return Named list of data.tables, one per quintile, each with columns:
#'   cluster_id, indicator, samplesize, x, y
#'
#' @examples
#' \dontrun{
#' # Public care-seeking for poorest quintile only
#' csb_pub_q1 <- prep_csb_by_wealth_mbg(
#'   dhs_kr = kr_data,
#'   gps_data = gps_data,
#'   indicator = "public",
#'   quintiles = 1
#' )
#' }
#'
#' @export
prep_csb_by_wealth_mbg <- function(
  dhs_kr,
  gps_data,
  indicator = "public",
  quintiles = 1:5,
  wealth_var = "v190",
  csb_priority_method = c("all", "first", "public", "private"),
  survey_vars = list(
    cluster = "v001",
    age = "hw1",
    fever = "h22"
  ),
  gps_vars = list(
    cluster = "DHSCLUST",
    lat = "LATNUM",
    lon = "LONGNUM"
  )
) {
  csb_priority_method <- match.arg(csb_priority_method)

  result <- calc_csb_by_wealth_mbg(
    dhs_kr = dhs_kr,
    gps_data = gps_data,
    indicators = indicator,
    quintiles = quintiles,
    wealth_var = wealth_var,
    csb_priority_method = csb_priority_method,
    survey_vars = survey_vars,
    gps_vars = gps_vars
  )

  result
}


