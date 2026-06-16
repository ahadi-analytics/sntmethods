# =============================================================================
# Demographic age-structure indicators (DHS Person Recode)
# =============================================================================
#
# Cluster-level proportion of the de jure household population in a given age
# band, prepared for model-based geostatistics (MBG). The motivating use case
# is mapping the proportion of the population under 5 (and, by complement, 5+)
# at the pixel and admin level, to examine urban/rural age-structure
# heterogeneity that a spatially-uniform age structure would erase.

#' Cluster-level Population Age Structure for MBG
#'
#' Computes cluster-level age-structure proportions from a DHS Person Recode
#' (PR / household member roster), formatted as binomial cluster inputs for
#' model-based geostatistics. The numerator is the count of de jure household
#' members in the target age band; the denominator is all de jure household
#' members with a known age.
#'
#' Unlike the malaria indicators (PfPR, ITN use) which restrict to *de facto*
#' residents (who slept in the household the previous night), age-structure
#' indicators use *de jure* residence (`hv102`, usual residents) so the
#' estimate reflects the resident population rather than visitors.
#'
#' @param dhs_pr DHS Person Recode (PR) data frame -- one row per household
#'   member. Must contain the cluster, age, and de jure residence variables
#'   named in `survey_vars`.
#' @param gps_data DHS GPS (GE) data frame with cluster coordinates.
#' @param indicators Character vector of indicator codes to compute. Any of
#'   `"prop_u5"` / `"prop_ov5"` (whole resident population) and the
#'   residence-stratified variants `"prop_u5_urban"`, `"prop_u5_rural"`,
#'   `"prop_ov5_urban"`, `"prop_ov5_rural"`. Default: all six. The stratified
#'   variants split clusters by `hv025` (each DHS EA is classified urban or
#'   rural), so an urban indicator is present only at urban clusters and a
#'   rural indicator only at rural clusters.
#' @param survey_vars Named list mapping logical names to PR variable names:
#'   `cluster` (default `"hv001"`), `age` (default `"hv105"`, age in years),
#'   `dejure` (default `"hv102"`, usual resident flag), and `residence`
#'   (default `"hv025"`, type of place of residence; 1 = urban, 2 = rural).
#' @param gps_vars Named list mapping GPS variable names: `cluster`
#'   (default `"DHSCLUST"`), `lat` (`"LATNUM"`), `lon` (`"LONGNUM"`).
#'
#' @return A named list keyed by indicator code. Each element is a tibble with
#'   columns `cluster_id`, `indicator` (numerator count), `samplesize`
#'   (denominator), `x`, `y`, matching the shape consumed by the MBG pipeline.
#'   Returns `NULL` if no valid clusters could be prepared.
#'
#' @details
#' Age values are taken from `hv105` (age in completed years). DHS special
#' codes for unknown age (96-99) and the `hv105 = 95` top-code are handled by
#' keeping values `0`-`95` (with `95` treated as "95+", i.e. 5+) and dropping
#' codes above `95` from both numerator and denominator.
#'
#' Because the indicator is a property of the whole resident population, its
#' aggregation weight in the pipeline (`pop_type`) is `"all"` (total
#' population), not the under-5 raster -- weighting the under-5 proportion by
#' the under-5 population would be circular.
#'
#' @examples
#' \dontrun{
#' pr <- sntutils::read("path/to/pr.parquet")
#' ge <- sntutils::read("path/to/ge.parquet")
#'
#' # Proportion under 5 and 5+ per cluster
#' pop <- calc_pop_structure_mbg(dhs_pr = pr, gps_data = ge)
#' pop$prop_u5
#'
#' # Via the pipeline
#' results <- run_mbg_pipeline(
#'   country_iso3 = "gin",
#'   indicators = "pop_structure",
#'   ...
#' )
#' }
#'
#' @seealso [run_mbg_pipeline()] for automated pipeline processing
#' @export
calc_pop_structure_mbg <- function(
  dhs_pr,
  gps_data,
  indicators = c(
    "prop_u5", "prop_ov5",
    "prop_u5_urban", "prop_u5_rural",
    "prop_ov5_urban", "prop_ov5_rural"
  ),
  survey_vars = list(
    cluster = "hv001",
    age = "hv105",
    dejure = "hv102",
    residence = "hv025"
  ),
  gps_vars = list(
    cluster = "DHSCLUST",
    lat = "LATNUM",
    lon = "LONGNUM"
  )
) {
  # Fail fast on missing suggested dependencies
  .check_pkg("tibble", reason = "for `calc_pop_structure_mbg()`")

  # ---- Input validation ----

  if (!is.data.frame(dhs_pr)) {
    cli::cli_abort("`dhs_pr` must be a data.frame or tibble")
  }
  if (!is.data.frame(gps_data)) {
    cli::cli_abort("`gps_data` must be a data.frame or tibble")
  }

  # ---- Resolve indicators ----

  dict <- .pop_structure_mbg_dictionary()
  dict_names <- vapply(dict, `[[`, character(1), "name")

  if (is.null(indicators)) {
    indicators <- dict_names
  }

  invalid <- setdiff(indicators, dict_names)
  if (length(invalid) > 0) {
    cli::cli_abort(
      "Invalid indicators: {.val {invalid}}. Valid codes: {.val {dict_names}}"
    )
  }

  dict_specs <- dict[vapply(dict, function(d) d$name %in% indicators, logical(1))]

  # ---- Prepare GPS data ----

  gps_clean <- .prepare_gps_data(gps_data, gps_vars)

  # ---- Prepare PR roster ----

  cluster_var <- survey_vars$cluster
  age_var <- survey_vars$age
  dejure_var <- survey_vars$dejure
  residence_var <- survey_vars$residence

  if (!cluster_var %in% names(dhs_pr)) {
    cli::cli_abort("Cluster variable {.var {cluster_var}} not found in dhs_pr")
  }
  if (!age_var %in% names(dhs_pr)) {
    cli::cli_abort("Age variable {.var {age_var}} not found in dhs_pr")
  }

  pr <- tibble::tibble(
    cluster_id = as.integer(haven::zap_labels(dhs_pr[[cluster_var]])),
    age = as.numeric(haven::zap_labels(dhs_pr[[age_var]])),
    dejure = if (dejure_var %in% names(dhs_pr)) {
      as.numeric(haven::zap_labels(dhs_pr[[dejure_var]]))
    } else {
      NA_real_
    },
    # hv025: 1 = urban, 2 = rural (cluster-level attribute)
    residence = if (!is.null(residence_var) && residence_var %in% names(dhs_pr)) {
      as.numeric(haven::zap_labels(dhs_pr[[residence_var]]))
    } else {
      NA_real_
    }
  )

  # De jure residence (hv102 == 1). If the column is absent, fall back to the
  # full roster with a warning -- the proportion is then over all listed
  # members rather than usual residents only.
  if (!dejure_var %in% names(dhs_pr)) {
    cli::cli_alert_warning(
      "De jure variable {.var {dejure_var}} not found -- using full roster"
    )
  } else {
    pr <- pr[!is.na(pr$dejure) & pr$dejure == 1, , drop = FALSE]
  }

  # Keep ages 0-95 (95 = top-coded 95+); drop unknown-age codes (96-99) and NA
  pr <- pr[!is.na(pr$age) & pr$age >= 0 & pr$age <= 95, , drop = FALSE]
  pr <- pr[!is.na(pr$cluster_id), , drop = FALSE]

  if (nrow(pr) == 0) {
    cli::cli_warn("No valid household-member records after filtering")
    return(NULL)
  }

  cli::cli_alert_success(
    "Valid de jure members: {format(nrow(pr), big.mark = ',')}"
  )

  # Age-band indicators
  pr$is_u5 <- as.integer(pr$age < 5)
  pr$is_ov5 <- as.integer(pr$age >= 5)

  # ---- Dictionary-driven indicator loop ----

  results <- list()

  for (spec in dict_specs) {
    outcome_col <- spec$outcome

    if (!outcome_col %in% names(pr)) {
      cli::cli_alert_warning(
        "Outcome {.var {outcome_col}} not found for {.val {spec$name}} - skipping"
      )
      next
    }

    # Restrict to a residence stratum for urban/rural indicators
    data_sub <- pr
    if (!is.null(spec$residence)) {
      if (all(is.na(pr$residence))) {
        cli::cli_alert_warning(
          "Residence variable {.var {residence_var}} unavailable - skipping {.val {spec$name}}"
        )
        next
      }
      res_code <- if (identical(spec$residence, "urban")) 1 else 2
      data_sub <- pr[!is.na(pr$residence) & pr$residence == res_code, ,
                     drop = FALSE]
      if (nrow(data_sub) == 0) {
        cli::cli_alert_warning(
          "No {spec$residence} members for {.val {spec$name}} - skipping"
        )
        next
      }
    }

    dt <- .aggregate_to_mbg_clusters(
      individual_data = data_sub,
      indicator_col = outcome_col,
      gps_clean = gps_clean,
      result_name = spec$name
    )
    if (!is.null(dt)) {
      results[[spec$name]] <- dt
    }
  }

  if (length(results) == 0) {
    cli::cli_warn("No valid population-structure data could be prepared")
    return(NULL)
  }

  results
}


# =============================================================================
# Population Structure MBG Indicator Dictionary
# =============================================================================

#' Population Structure MBG Indicator Dictionary
#'
#' Returns the standardized indicator specifications for cluster-level
#' age-structure MBG output. Each entry defines the indicator `name` and the
#' binary `outcome` column counted as the numerator.
#'
#' @return List of named lists with fields `name`, `outcome`, and `residence`
#'   (`NULL` for all-residence indicators, or `"urban"`/`"rural"` for the
#'   residence-stratified variants).
#' @noRd
.pop_structure_mbg_dictionary <- function() {
  list(
    list(name = "prop_u5",        outcome = "is_u5",  residence = NULL),
    list(name = "prop_ov5",       outcome = "is_ov5", residence = NULL),
    list(name = "prop_u5_urban",  outcome = "is_u5",  residence = "urban"),
    list(name = "prop_u5_rural",  outcome = "is_u5",  residence = "rural"),
    list(name = "prop_ov5_urban", outcome = "is_ov5", residence = "urban"),
    list(name = "prop_ov5_rural", outcome = "is_ov5", residence = "rural")
  )
}


#' Population Structure Indicator Dictionary
#'
#' Returns the dictionary of population age-structure indicators with metadata,
#' built from `.pop_structure_conditions()`. Consumed by [dhs_dictionary()] so
#' that pipeline output carries indicator titles and numerator/denominator
#' descriptions for `prop_u5` / `prop_ov5`.
#'
#' @return Tibble with columns: indicator, indicator_code, indicator_title,
#'   numerator_description, denominator_description, denominator_code,
#'   data_level.
#'
#' @keywords internal
pop_structure_dictionary <- function() {
  conds <- .pop_structure_conditions()
  tibble::tibble(
    indicator               = vapply(conds, `[[`, character(1), "indicator"),
    indicator_code          = vapply(conds, `[[`, character(1), "indicator_code"),
    indicator_title         = vapply(conds, `[[`, character(1), "indicator_title"),
    numerator_description   = vapply(conds, `[[`, character(1), "num_desc"),
    denominator_description = vapply(conds, `[[`, character(1), "denom_desc"),
    denominator_code        = vapply(conds, `[[`, character(1), "denom_code"),
    data_level              = "person"
  )
}


#' Population Structure Indicator Conditions
#'
#' Single source of truth for population age-structure indicator labels and
#' descriptions. Collected by `.dhs_indicator_lookup()` (for MBG output
#' labels) and [pop_structure_dictionary()] (for the consolidated
#' [dhs_dictionary()]) so the two stay consistent.
#'
#' @return List of conditions lists, one per indicator code.
#' @noRd
.pop_structure_conditions <- function() {
  list(
    list(
      indicator       = "POP_U5",
      indicator_code  = "prop_u5",
      indicator_title = "Proportion of population under 5 years",
      outcome_var     = "is_u5",
      filter_expr     = NULL,
      num_desc        = "De jure household members under 5 years",
      denom_desc      = "All de jure household members with known age",
      denom_code      = "hh_pop_dejure"
    ),
    list(
      indicator       = "POP_OV5",
      indicator_code  = "prop_ov5",
      indicator_title = "Proportion of population 5 years and older",
      outcome_var     = "is_ov5",
      filter_expr     = NULL,
      num_desc        = "De jure household members aged 5 years and older",
      denom_desc      = "All de jure household members with known age",
      denom_code      = "hh_pop_dejure"
    ),
    list(
      indicator       = "POP_U5_URBAN",
      indicator_code  = "prop_u5_urban",
      indicator_title = "Proportion of population under 5 years (urban)",
      outcome_var     = "is_u5",
      filter_expr     = "hv025 == 1",
      num_desc        = "De jure urban household members under 5 years",
      denom_desc      = "All de jure urban household members with known age",
      denom_code      = "hh_pop_dejure_urban"
    ),
    list(
      indicator       = "POP_U5_RURAL",
      indicator_code  = "prop_u5_rural",
      indicator_title = "Proportion of population under 5 years (rural)",
      outcome_var     = "is_u5",
      filter_expr     = "hv025 == 2",
      num_desc        = "De jure rural household members under 5 years",
      denom_desc      = "All de jure rural household members with known age",
      denom_code      = "hh_pop_dejure_rural"
    ),
    list(
      indicator       = "POP_OV5_URBAN",
      indicator_code  = "prop_ov5_urban",
      indicator_title = "Proportion of population 5 years and older (urban)",
      outcome_var     = "is_ov5",
      filter_expr     = "hv025 == 1",
      num_desc        = "De jure urban household members aged 5 years and older",
      denom_desc      = "All de jure urban household members with known age",
      denom_code      = "hh_pop_dejure_urban"
    ),
    list(
      indicator       = "POP_OV5_RURAL",
      indicator_code  = "prop_ov5_rural",
      indicator_title = "Proportion of population 5 years and older (rural)",
      outcome_var     = "is_ov5",
      filter_expr     = "hv025 == 2",
      num_desc        = "De jure rural household members aged 5 years and older",
      denom_desc      = "All de jure rural household members with known age",
      denom_code      = "hh_pop_dejure_rural"
    )
  )
}
