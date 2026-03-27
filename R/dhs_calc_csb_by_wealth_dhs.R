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
#' @param csb_classification Data frame with h32 variable to category mapping.
#'   See [calc_csb_dhs_core()] for details.
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
  csb_classification = NULL,
  region_var = NULL,
  ci_method = "logit"
) {
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
    csb_classification = csb_classification,
    include_survey_vars = TRUE
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
