# =============================================================================
# Shared DHS survey-weighted indicator computation helpers
# =============================================================================

#' Compute a single DHS indicator in standardized long format
#'
#' Generic helper used by all DHS indicator functions. Applies optional filtering,
#' creates survey design, computes svyciprop nationally and optionally by region,
#' and returns a standardized tibble with indicator metadata.
#'
#' @param data Prepared dataset with standardized column names:
#'   `cluster_id`, `stratum_id`, `survey_weight`, and the outcome column.
#' @param condition Named list with indicator specification:
#'   \itemize{
#'     \item `indicator_title`: Human-readable indicator name
#'     \item `indicator_code`: Short code (e.g., "fever")
#'     \item `denom_code`: Denominator code
#'     \item `filter_expr`: Quoted filter expression or NULL
#'     \item `outcome_var`: Column name of the binary 0/1 outcome
#'     \item `num_desc`: Numerator description
#'     \item `denom_desc`: Denominator description
#'   }
#' @param group_var Optional grouping variable name for regional estimates.
#' @param subnational_level Admin level name for grouped rows (e.g., "adm1").
#' @param ci_method CI method for svyciprop. Default: "logit".
#'
#' @return Tibble with columns: level, location, point, ci_l, ci_u, numerator,
#'   denominator, indicator, indicator_code, numerator_description,
#'   denominator_description, denominator_code.
#' @noRd
.compute_dhs_indicator_generic <- function(data, condition, group_var = NULL,
                                            subnational_level = NULL,
                                            ci_method = "logit") {

  outcome_var <- condition$outcome_var
  ind_title   <- condition$indicator_title

  # Apply filter
  if (is.null(condition$filter_expr)) {
    filtered <- data
  } else {
    filtered <- tryCatch(
      dplyr::filter(data, !!condition$filter_expr),
      error = function(e) tibble::tibble()
    )
  }

  if (!outcome_var %in% names(filtered)) {
    return(tibble::tibble())
  }

  filtered$.dhs_outcome <- filtered[[outcome_var]]
  filtered <- filtered[!is.na(filtered$.dhs_outcome), ]

  n_denom <- nrow(filtered)
  if (n_denom == 0) return(tibble::tibble())

  # Survey design
  n_clusters <- dplyr::n_distinct(filtered$cluster_id)
  use_strata <- dplyr::n_distinct(filtered$stratum_id) > 1

  # Weighted counts
  n_denom_w <- round(sum(filtered$survey_weight, na.rm = TRUE))
  n_numer_w <- round(sum(
    filtered$survey_weight * (filtered$.dhs_outcome == 1), na.rm = TRUE
  ))

  # Single-cluster guard
  if (n_clusters < 2) {
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

  # National estimate
  old_opts <- options(survey.lonely.psu = "adjust")
  on.exit(options(old_opts), add = TRUE)

  national <- tryCatch({
    est <- survey::svyciprop(~.dhs_outcome, svy, method = ci_method,
                              na.rm = TRUE)
    ci  <- stats::confint(est)
    tibble::tibble(
      level = "adm0", location = "National",
      point = as.numeric(est), ci_l = ci[1], ci_u = ci[2],
      numerator = n_numer_w, denominator = n_denom_w
    )
  }, error = function(e) {
    cli::cli_alert_warning("    {ind_title} national: {e$message}")
    tibble::tibble(
      level = "adm0", location = "National", point = NA_real_,
      ci_l = NA_real_, ci_u = NA_real_,
      numerator = n_numer_w, denominator = n_denom_w
    )
  })

  # Regional estimates
  regional <- tibble::tibble()
  sub_level <- subnational_level %||% "adm1"

  if (!is.null(group_var) && group_var %in% names(filtered)) {
    group_formula <- stats::as.formula(paste("~", group_var))

    regional <- tryCatch({
      by_result <- survey::svyby(
        ~.dhs_outcome, by = group_formula, design = svy,
        FUN = survey::svyciprop, vartype = "ci",
        method = ci_method, na.rm = TRUE, keep.names = FALSE
      ) |> tibble::as_tibble()

      region_num <- filtered |>
        dplyr::group_by(.data[[group_var]]) |>
        dplyr::summarise(
          numerator = round(sum(
            survey_weight * (.dhs_outcome == 1), na.rm = TRUE
          )), .groups = "drop"
        )

      region_denom <- filtered |>
        dplyr::group_by(.data[[group_var]]) |>
        dplyr::summarise(
          denominator = round(sum(survey_weight, na.rm = TRUE)),
          .groups = "drop"
        )

      names(by_result)[names(by_result) == "ci_l"] <- "ci_l..dhs_outcome"
      names(by_result)[names(by_result) == "ci_u"] <- "ci_u..dhs_outcome"

      by_result |>
        dplyr::rename(
          location = !!group_var, point = .dhs_outcome,
          ci_l = `ci_l..dhs_outcome`, ci_u = `ci_u..dhs_outcome`
        ) |>
        dplyr::mutate(location = as.character(location)) |>
        dplyr::left_join(
          region_num |>
            dplyr::mutate(
              location = as.character(.data[[group_var]])
            ) |>
            dplyr::select(location, numerator),
          by = "location"
        ) |>
        dplyr::left_join(
          region_denom |>
            dplyr::mutate(
              location = as.character(.data[[group_var]])
            ) |>
            dplyr::select(location, denominator),
          by = "location"
        ) |>
        dplyr::mutate(level = sub_level) |>
        dplyr::select(
          level, location, point, ci_l, ci_u, numerator, denominator
        )
    }, error = function(e) {
      cli::cli_alert_warning("    {ind_title} by group: {e$message}")
      tibble::tibble()
    })
  }

  dplyr::bind_rows(national, regional) |>
    dplyr::mutate(
      indicator               = condition$indicator_title,
      indicator_code          = condition$indicator_code,
      numerator_description   = condition$num_desc,
      denominator_description = condition$denom_desc,
      denominator_code        = condition$denom_code
    )
}


#' Assemble standardized DHS output list
#'
#' Takes national and optional regional results in long format and assembles
#' the standardized `list(adm0 = ..., adm1 = ...)` return structure with
#' survey metadata columns prepended.
#'
#' @param national_results Tibble of national results with level/location columns.
#' @param regional_results Tibble of regional results (or NULL/empty tibble).
#' @param survey_meta Named list from `.extract_survey_meta()` or
#'   `.extract_survey_meta_hv()`.
#' @param geo_source Character. geo_source value for regional rows
#'   (e.g., "survey", "gps"). Default: NA.
#' @param admin_col Character. Column name for the admin level (e.g., "adm1").
#'
#' @return Named list with `adm0` tibble and optionally `adm1` (or other) tibble.
#' @noRd
.assemble_dhs_output <- function(national_results, regional_results = NULL,
                                  survey_meta, geo_source = NA_character_,
                                  admin_col = "adm1") {

  meta_cols <- tibble::tibble(
    survey_id   = survey_meta$survey_id,
    iso3        = survey_meta$iso3,
    iso2        = survey_meta$iso2,
    survey_type = survey_meta$survey_type,
    survey_year = survey_meta$survey_year,
    adm0        = survey_meta$country_upper
  )

  # Assemble adm0
  adm0_data <- national_results |>
    dplyr::filter(level == "adm0") |>
    dplyr::select(-level, -location)

  adm0_tbl <- dplyr::bind_cols(
    meta_cols[rep(1, nrow(adm0_data)), ],
    tibble::tibble(type = "survey_weighted", geo_source = NA_character_),
    adm0_data
  ) |> tibble::as_tibble()

  out <- list(adm0 = adm0_tbl)

  # Assemble subnational
  if (!is.null(regional_results) && nrow(regional_results) > 0) {
    sub_data <- regional_results |>
      dplyr::filter(level != "adm0")

    if (nrow(sub_data) > 0) {
      sub_tbl <- dplyr::bind_cols(
        meta_cols[rep(1, nrow(sub_data)), ],
        sub_data |>
          dplyr::transmute(
            !!admin_col := toupper(location),
            type       = "survey_weighted",
            geo_source = geo_source,
            point, ci_l, ci_u,
            numerator, denominator,
            indicator, indicator_code,
            numerator_description,
            denominator_description, denominator_code
          )
      ) |> tibble::as_tibble()

      out[[admin_col]] <- sub_tbl
    }
  }

  out
}


# =============================================================================
# Master DHS indicator dictionary
# =============================================================================

#' Master DHS Indicator Dictionary
#'
#' Returns a single consolidated tibble with every DHS indicator across all
#' domains (ACT, ITN, fever, etc.). Use this as a pre-analysis reference to
#' know what indicators are available, what DHS variables are needed, and
#' what each numerator/denominator represents.
#'
#' @return A tibble with one row per indicator and columns:
#'   \describe{
#'     \item{domain}{Topic area (e.g., "ACT", "ITN", "Fever")}
#'     \item{observation_unit}{Unit of analysis (e.g., "Individual", "Household", "Person")}
#'     \item{dhs_recode}{DHS file type needed (KR, IR, HR, PR, HR+PR)}
#'     \item{calc_function}{R function to call}
#'     \item{indicator_code}{Unique indicator identifier}
#'     \item{indicator}{Short indicator name}
#'     \item{indicator_title}{Full descriptive title}
#'     \item{numerator_code}{Auto-derived numerator code (n_<indicator_code>)}
#'     \item{numerator_description}{What the numerator counts}
#'     \item{denominator_code}{Short code for the denominator}
#'     \item{denominator_description}{What the denominator counts}
#'     \item{eligibility}{Who is eligible / inclusion criteria}
#'     \item{dhs_variables}{Key DHS variables needed (per domain)}
#'     \item{notes}{Additional context, caveats, or methodology notes}
#'   }
#'
#' @export
#' @examples
#' \dontrun{
#' # Browse all indicators
#' dhs_dictionary()
#'
#' # Filter to a specific domain
#' dhs_dictionary() |> dplyr::filter(domain == "ITN")
#'
#' # Find which DHS recode files you need
#' dhs_dictionary() |> dplyr::distinct(domain, dhs_recode, calc_function)
#' }
dhs_dictionary <- function() {

  # Domain specifications: dictionary function, metadata, DHS variables

  domain_specs <- list(
    list(
      fn          = act_dictionary,
      domain      = "ACT",
      dhs_recode  = "KR",
      calc_fn     = "calc_act_dhs",
      eligibility = "Children 0-59 months with fever (h22==1), alive (b5==1)",
      dhs_vars    = "h22, ml13e, ml13a-g, b5, hw1, h32a-r",
      notes       = NA_character_
    ),
    list(
      fn          = antimalarial_dictionary,
      domain      = "Antimalarial",
      dhs_recode  = "KR",
      calc_fn     = "calc_antimalarial_dhs",
      eligibility = "Children 0-59 months with fever (h22==1), alive (b5==1)",
      dhs_vars    = "h22, ml13a-g, h37a-h, b5, hw1",
      notes       = "ml13a-g primary; h37a-h fallback for older surveys"
    ),
    list(
      fn          = anc_dictionary,
      domain      = "ANC",
      dhs_recode  = "IR",
      calc_fn     = "calc_anc_dhs",
      eligibility = "Women 15-49 with a birth in last 2-5 years",
      dhs_vars    = "m14_1, v008, b3_01",
      notes       = NA_character_
    ),
    list(
      fn          = case_management_dictionary,
      domain      = "Case Management",
      dhs_recode  = "KR",
      calc_fn     = "calc_case_management_dhs",
      eligibility = "Children 0-59 months with fever (h22==1), alive (b5==1)",
      dhs_vars    = "h22, ml13e, ml13a, b5, hw1, h32a-r",
      notes       = "Composite indicator: P(test) x P(ACT|positive)"
    ),
    list(
      fn          = csb_dictionary,
      domain      = "CSB",
      dhs_recode  = "KR",
      calc_fn     = "calc_csb_dhs",
      eligibility = "Children 0-59 months with fever (h22==1), alive (b5==1)",
      dhs_vars    = "h22, b5, hw1, h32a-r",
      notes       = NA_character_
    ),
    list(
      fn          = epi_dictionary,
      domain      = "EPI",
      dhs_recode  = "KR",
      calc_fn     = "calc_epi_dhs",
      eligibility = "Children 12-23 months",
      dhs_vars    = "h2, h3-h8, h9, h33, h50-h68, hw1",
      notes       = "Card + maternal recall"
    ),
    list(
      fn          = fever_dictionary,
      domain      = "Fever",
      dhs_recode  = "KR",
      calc_fn     = "calc_fever_dhs",
      eligibility = "Children 0-59 months, alive (b5==1)",
      dhs_vars    = "h22, b5, hw1",
      notes       = NA_character_
    ),
    list(
      fn          = iptp_dictionary,
      domain      = "IPTp",
      dhs_recode  = "IR",
      calc_fn     = "calc_iptp_dhs",
      eligibility = "Women 15-49 with a birth in last 2-5 years",
      dhs_vars    = "ml1_1, m49a_1, v008, b3_01, b19_01",
      notes       = NA_character_
    ),
    list(
      fn          = irs_dictionary,
      domain      = "IRS",
      dhs_recode  = "HR",
      calc_fn     = "calc_irs_dhs",
      eligibility = "All surveyed households",
      dhs_vars    = "hv253",
      notes       = NA_character_
    ),
    list(
      fn          = itn_dictionary,
      domain      = "ITN",
      dhs_recode  = "HR+PR",
      calc_fn     = "calc_itn_dhs",
      eligibility = "HH for ownership indicators; de facto persons for use indicators",
      dhs_vars    = "hml10_1-n, hml12, hv013, hv105, hv104, hml18, hv270, hv025",
      notes       = "observation_unit distinguishes Household vs Person indicators"
    ),
    list(
      fn          = malaria_dx_dictionary,
      domain      = "Malaria Dx",
      dhs_recode  = "KR",
      calc_fn     = "calc_malaria_dx_dhs",
      eligibility = "Children 0-59 months with fever (h22==1), alive (b5==1)",
      dhs_vars    = "h22, h47, b5, hw1",
      notes       = NA_character_
    ),
    list(
      fn          = pfpr_dictionary,
      domain      = "PfPR",
      dhs_recode  = "PR",
      calc_fn     = "calc_pfpr_dhs",
      eligibility = "Children tested for malaria (RDT or microscopy)",
      dhs_vars    = "hml32, hml35, hc1, hv103, hv042",
      notes       = NA_character_
    ),
    list(
      fn          = severe_anemia_dictionary,
      domain      = "Severe Anemia",
      dhs_recode  = "PR",
      calc_fn     = "calc_severe_anemia_dhs",
      eligibility = "Children 6-59 months tested for hemoglobin",
      dhs_vars    = "hc56, hw53, hc1, hv103, hv042",
      notes       = "Altitude-adjusted Hb preferred (hw53)"
    ),
    list(
      fn          = smc_dictionary,
      domain      = "SMC",
      dhs_recode  = "KR",
      calc_fn     = "calc_smc_dhs",
      eligibility = "Children 3-59 months in SMC-eligible areas",
      dhs_vars    = "hml43, ml13g, hw1",
      notes       = "hml43 primary; ml13g fallback"
    ),
    list(
      fn          = u5mr_dictionary,
      domain      = "U5MR",
      dhs_recode  = "KR",
      calc_fn     = "calc_u5mr_dhs",
      eligibility = "Live births in recall period (last 5 years)",
      dhs_vars    = "v008, b3, b7, b5",
      notes       = "Rate per 1000 live births; uses DHS.rates::chmort()"
    ),
    list(
      fn          = wealth_dictionary,
      domain      = "Wealth",
      dhs_recode  = "HR",
      calc_fn     = "calc_wealth_dhs",
      eligibility = "All surveyed households",
      dhs_vars    = "hv270, hv271, hv012",
      notes       = NA_character_
    )
  )

  # Map data_level values to readable observation_unit labels
  obs_unit_map <- c(
    "adm0"      = "Country",
    "household" = "Household",
    "person"    = "Person"
  )

  # Build consolidated tibble
  result_list <- lapply(domain_specs, function(spec) {
    dict <- spec$fn()

    # Standardize data_level: use existing if present, else "adm0"
    if (!"data_level" %in% names(dict)) {
      dict$data_level <- "adm0"
    }

    # Map to observation_unit
    dict$observation_unit <- unname(
      obs_unit_map[as.character(dict$data_level)]
    )
    dict$observation_unit[is.na(dict$observation_unit)] <- "Country"

    # Keep only the core columns from individual dictionaries
    core_cols <- c("indicator", "indicator_code", "indicator_title",
                   "numerator_description", "denominator_description",
                   "denominator_code", "observation_unit")
    keep <- intersect(core_cols, names(dict))
    dict <- dict[, keep, drop = FALSE]

    # Add domain metadata
    dict$domain        <- spec$domain
    dict$dhs_recode    <- spec$dhs_recode
    dict$calc_function <- spec$calc_fn
    dict$eligibility   <- spec$eligibility
    dict$dhs_variables <- spec$dhs_vars
    dict$notes         <- spec$notes

    dict
  })

  out <- do.call(rbind, result_list)

  # Derive numerator_code
  out$numerator_code <- paste0("n_", out$indicator_code)

  # Reorder columns: observation_unit right after domain
  out <- out[, c(
    "domain", "observation_unit", "dhs_recode", "calc_function",
    "indicator_code", "indicator", "indicator_title",
    "numerator_code", "numerator_description",
    "denominator_code", "denominator_description",
    "eligibility", "dhs_variables", "notes"
  ), drop = FALSE]

  tibble::as_tibble(out)
}


# =============================================================================
# MBG helpers
# =============================================================================

#' Prepare GPS Data for MBG Analysis
#'
#' Shared GPS cleaning used by all MBG functions. Extracts cluster coordinates,
#' filters invalid values, and deduplicates.
#'
#' @param gps_data DHS GPS dataset with cluster coordinates.
#' @param gps_vars Named list for GPS variable mapping with keys:
#'   cluster, lat, lon.
#'
#' @return A tibble with columns: cluster_id, x (longitude), y (latitude).
#'
#' @noRd
.prepare_gps_data <- function(
  gps_data,
  gps_vars = list(
    cluster = "DHSCLUST",
    lat = "LATNUM",
    lon = "LONGNUM"
  )
) {
  if (!is.data.frame(gps_data)) {
    cli::cli_abort("`gps_data` must be a data.frame or tibble")
  }

  gps_clean <- gps_data |>
    dplyr::transmute(
      cluster_id = .data[[gps_vars$cluster]],
      x = as.numeric(.data[[gps_vars$lon]]),
      y = as.numeric(.data[[gps_vars$lat]])
    ) |>
    dplyr::filter(!is.na(x), !is.na(y), x != 0, y != 0) |>
    dplyr::distinct()

  cli::cli_alert_info(
    "GPS data: {nrow(gps_clean)} clusters with valid coordinates"
  )

  gps_clean
}


#' Aggregate Individual Data to MBG Cluster Counts
#'
#' Shared cluster-level aggregation pattern used by all MBG functions.
#' Groups individual-level data by cluster, counts indicator positives and
#' total sample size, then joins GPS coordinates.
#'
#' @param individual_data Data frame with individual-level records. Must have
#'   columns `cluster_id` and the column named by `indicator_col`.
#' @param indicator_col Character. Name of the binary (0/1) indicator column
#'   to aggregate.
#' @param gps_clean Cleaned GPS data from `.prepare_gps_data()`.
#' @param result_name Character. Name for the result in log messages.
#'
#' @return A data.table with columns: cluster_id, indicator, samplesize, x, y.
#'   Returns NULL if no valid clusters could be created.
#'
#' @noRd
.aggregate_to_mbg_clusters <- function(
  individual_data,
  indicator_col,
  gps_clean,
  result_name = "indicator"
) {
  cluster_data <- individual_data |>
    dplyr::group_by(cluster_id) |>
    dplyr::summarise(
      indicator = sum(.data[[indicator_col]], na.rm = TRUE),
      samplesize = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::inner_join(gps_clean, by = "cluster_id") |>
    dplyr::filter(samplesize > 0)

  if (nrow(cluster_data) == 0) {
    cli::cli_alert_warning("{result_name}: no valid clusters")
    return(NULL)
  }

  cli::cli_alert_success("{result_name}: {nrow(cluster_data)} clusters")

  cluster_data
}


#' Extract Interview Month per Cluster
#'
#' Extracts the median interview month for each DHS cluster from any
#' available recode file (KR > PR > HR > IR preference order).
#'
#' @param survey_data Named list of loaded survey data frames.
#' @param gps_clean Cleaned GPS data from `.prepare_gps_data()`.
#'
#' @return A tibble with columns: cluster_id, interview_month, x, y.
#'   Returns NULL if no interview month data could be extracted.
#'
#' @noRd
.extract_cluster_interview_month <- function(survey_data, gps_clean) {
  recode_vars <- list(
    KR = list(cluster = "v001", month = "v006"),
    PR = list(cluster = "hv001", month = "hv006"),
    HR = list(cluster = "hv001", month = "hv006"),
    IR = list(cluster = "v001", month = "v006")
  )

  for (recode in c("KR", "PR", "HR", "IR")) {
    if (!recode %in% names(survey_data)) next

    df <- survey_data[[recode]]
    vars <- recode_vars[[recode]]

    if (!all(c(vars$cluster, vars$month) %in% names(df))) next

    cluster_months <- df |>
      dplyr::transmute(
        cluster_id = as.integer(haven::zap_labels(.data[[vars$cluster]])),
        interview_month = as.integer(haven::zap_labels(.data[[vars$month]]))
      ) |>
      dplyr::filter(!is.na(interview_month), !is.na(cluster_id)) |>
      dplyr::group_by(cluster_id) |>
      dplyr::summarise(
        interview_month = as.integer(stats::median(interview_month, na.rm = TRUE)),
        .groups = "drop"
      )

    if (nrow(cluster_months) > 0) {
      cluster_months <- cluster_months |>
        dplyr::inner_join(gps_clean, by = "cluster_id")

      cli::cli_alert_info(
        "Interview month extracted from {recode}: {nrow(cluster_months)} clusters"
      )
      return(cluster_months)
    }
  }

  cli::cli_alert_warning("Could not extract interview month from any recode file")
  NULL
}


#' Aggregate Interview Month to Administrative Level
#'
#' Spatially joins cluster-level interview months to admin polygons and
#' computes the median month per administrative unit. Admin units with no
#' clusters are filled with the median of sibling units within the same
#' parent (e.g., missing adm2 filled from other adm2s in the same adm1).
#'
#' @param cluster_months Tibble from `.extract_cluster_interview_month()`.
#' @param admin_sf sf object with administrative boundaries.
#' @param admin_col Column name for admin identifier (e.g., "adm2").
#' @param parent_col Column name for parent admin level (e.g., "adm1").
#'   Used to fill missing values. If NULL or not present in admin_sf,
#'   no gap-filling is performed.
#'
#' @return A tibble with columns: `admin_col` and `median_survey_month`.
#'   Returns NULL if no valid data.
#'
#' @noRd
.aggregate_interview_month_to_admin <- function(
  cluster_months,
  admin_sf,
  admin_col = "adm2",
  parent_col = "adm1"
) {
  if (is.null(cluster_months) || nrow(cluster_months) == 0) return(NULL)

  # Determine which columns to carry from admin_sf for gap-filling
  has_parent <- !is.null(parent_col) && parent_col %in% names(admin_sf)
  select_cols <- if (has_parent) {
    c(admin_col, parent_col)
  } else {
    admin_col
  }

  cluster_sf <- cluster_months |>
    sf::st_as_sf(coords = c("x", "y"), crs = 4326, remove = FALSE)

  admin_crs <- sf::st_crs(admin_sf)
  if (!is.na(admin_crs)) {
    cluster_sf <- sf::st_transform(cluster_sf, admin_crs)
  }

  joined <- sf::st_join(
    cluster_sf,
    admin_sf |> dplyr::select(dplyr::all_of(select_cols)),
    join = sf::st_within,
    left = TRUE
  )

  unmatched <- is.na(joined[[admin_col]])
  if (any(unmatched)) {
    nearest_idx <- sf::st_nearest_feature(joined[unmatched, ], admin_sf)
    joined[[admin_col]][unmatched] <- admin_sf[[admin_col]][nearest_idx]
    if (has_parent) {
      joined[[parent_col]][unmatched] <- admin_sf[[parent_col]][nearest_idx]
    }
  }

  result <- sf::st_drop_geometry(joined) |>
    dplyr::filter(!is.na(.data[[admin_col]])) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(admin_col))) |>
    dplyr::summarise(
      median_survey_month = as.integer(
        round(stats::median(interview_month, na.rm = TRUE))
      ),
      .groups = "drop"
    )

  # Fill missing admin units using parent-level median
  if (has_parent) {
    # Get full list of admin units with their parent
    all_admins <- sf::st_drop_geometry(admin_sf) |>
      dplyr::select(dplyr::all_of(select_cols)) |>
      dplyr::distinct()

    result <- all_admins |>
      dplyr::left_join(result, by = admin_col)

    missing <- is.na(result$median_survey_month)
    if (any(missing)) {
      # Compute parent-level median from sibling units that have data
      parent_medians <- result |>
        dplyr::filter(!is.na(.data$median_survey_month)) |>
        dplyr::group_by(dplyr::across(dplyr::all_of(parent_col))) |>
        dplyr::summarise(
          parent_median = as.integer(
            round(stats::median(.data$median_survey_month, na.rm = TRUE))
          ),
          .groups = "drop"
        )

      result <- result |>
        dplyr::left_join(parent_medians, by = parent_col) |>
        dplyr::mutate(
          median_survey_month = dplyr::coalesce(
            .data$median_survey_month, .data$parent_median
          )
        ) |>
        dplyr::select(-"parent_median")
    }

    # Drop the parent column -- caller only expects admin_col + median_survey_month
    result <- result |>
      dplyr::select(dplyr::all_of(admin_col), "median_survey_month")
  }

  result
}


#' Resolve region variable to human-readable labels
#'
#' Converts a haven-labelled region variable to uppercase character labels.
#' If the result is still numeric (no haven labels), falls back to extracting
#' value labels from the `labels` attribute. Final fallback: "Region_N".
#'
#' @param region_col The region column (possibly haven_labelled).
#' @param region_var_name Name of the variable (for messages).
#' @return Character vector of uppercase region names.
#' @noRd
.resolve_region_labels <- function(region_col, region_var_name = "region") {
  # Try haven::as_factor first
  resolved <- tryCatch({
    as.character(haven::as_factor(region_col)) |> toupper()
  }, error = function(e) {
    as.character(region_col) |> toupper()
  })

  # Check if result is still numeric
  unique_vals <- unique(resolved[!is.na(resolved)])
  is_numeric <- length(unique_vals) > 0 &&
    all(grepl("^\\d+(\\.\\d+)?$", unique_vals))

  if (!is_numeric) return(resolved)

  # Try extracting from value labels attribute
  val_labels <- attr(region_col, "labels")
  if (!is.null(val_labels) && length(val_labels) > 0) {
    lookup <- stats::setNames(
      toupper(names(val_labels)), as.character(val_labels)
    )
    raw_vals <- as.character(as.vector(haven::zap_labels(region_col)))
    new_resolved <- unname(lookup[raw_vals])
    new_resolved[is.na(new_resolved)] <- resolved[is.na(new_resolved)]

    new_unique <- unique(new_resolved[!is.na(new_resolved)])
    if (!all(grepl("^\\d+(\\.\\d+)?$", new_unique))) {
      cli::cli_alert_info(
        "Resolved {.var {region_var_name}} codes to labels"
      )
      return(new_resolved)
    }
  }

  # Final fallback
  cli::cli_alert_warning(
    "{.var {region_var_name}} has numeric values with no labels; using Region_N"
  )
  paste0("REGION_", resolved)
}
