#' EPI Vaccine Registry
#'
#' Centralized mapping of vaccine indicators to DHS variables, including
#' fallback variables for vaccines that changed names across DHS eras
#' (e.g., DPT -> Pentavalent).
#'
#' @return Named list. Each element contains: dhs_var, fallback_var (or NULL),
#'   fallback_source (or NULL).
#'
#' @noRd
.epi_vaccine_registry <- function() {
  list(
    # Classic EPI vaccines (all DHS eras)
    bcg      = list(dhs_var = "h2",   fallback_var = NULL),
    dpt1     = list(dhs_var = "h3",   fallback_var = "h51", fallback_source = "pentavalent"),
    dpt2     = list(dhs_var = "h4",   fallback_var = "h52", fallback_source = "pentavalent"),
    dpt3     = list(dhs_var = "h5",   fallback_var = "h53", fallback_source = "pentavalent"),
    polio1   = list(dhs_var = "h6",   fallback_var = NULL),
    polio2   = list(dhs_var = "h7",   fallback_var = NULL),
    polio3   = list(dhs_var = "h8",   fallback_var = NULL),
    measles1 = list(dhs_var = "h9",   fallback_var = NULL),
    measles2 = list(dhs_var = "h9a",  fallback_var = NULL),
    vita1    = list(dhs_var = "h33",  fallback_var = NULL),
    vita2    = list(dhs_var = "h33a", fallback_var = NULL),
    malaria  = list(dhs_var = "h68",  fallback_var = NULL),
    # Newer vaccines (DHS-7+, circa 2015+)
    penta1   = list(dhs_var = "h51",  fallback_var = NULL),
    penta2   = list(dhs_var = "h52",  fallback_var = NULL),
    penta3   = list(dhs_var = "h53",  fallback_var = NULL),
    pneumo1  = list(dhs_var = "h54",  fallback_var = NULL),
    pneumo2  = list(dhs_var = "h55",  fallback_var = NULL),
    pneumo3  = list(dhs_var = "h56",  fallback_var = NULL),
    rota1    = list(dhs_var = "h57",  fallback_var = NULL),
    rota2    = list(dhs_var = "h58",  fallback_var = NULL),
    rota3    = list(dhs_var = "h59",  fallback_var = NULL),
    ipv      = list(dhs_var = "h60",  fallback_var = NULL),
    hepb0    = list(dhs_var = "h50",  fallback_var = NULL),
    yellowfever = list(dhs_var = "h61", fallback_var = NULL)
  )
}


#' Prepare EPI Data for Analysis
#'
#' Shared data cleaning and indicator computation for EPI functions.
#' Used by both calc_epi_dhs_core() and calc_epi_mbg().
#'
#' @param dhs_kr DHS Children's Recode (KR) dataset.
#' @param survey_vars Named list mapping DHS variable names.
#' @param age_min_months Minimum age in months.
#' @param age_max_months Maximum age in months.
#' @param include_survey_vars Logical. If TRUE, includes survey design columns.
#'
#' @return A data frame of eligible children with columns:
#'   cluster_id, age_months, and binary vaccination columns for each
#'   available vaccine. If include_survey_vars = TRUE, also: survey_weight,
#'   stratum_id.
#'
#' @noRd
.prepare_epi_data <- function(
  dhs_kr,
  survey_vars,
  age_min_months = 12,
  age_max_months = 23,
  include_survey_vars = FALSE
) {
  if (!is.data.frame(dhs_kr)) {
    cli::cli_abort("`dhs_kr` must be a data.frame or tibble")
  }
  if (nrow(dhs_kr) == 0) {
    cli::cli_abort("`dhs_kr` is empty.")
  }

  # Build vaccine mapping from survey_vars
  vaccine_mapping <- list(
    bcg = survey_vars$bcg, dpt1 = survey_vars$dpt1,
    dpt2 = survey_vars$dpt2, dpt3 = survey_vars$dpt3,
    polio1 = survey_vars$polio1, polio2 = survey_vars$polio2,
    polio3 = survey_vars$polio3,
    measles1 = survey_vars$measles1, measles2 = survey_vars$measles2,
    vita1 = survey_vars$vita1, vita2 = survey_vars$vita2,
    malaria = survey_vars$malaria,
    penta1 = survey_vars$penta1, penta2 = survey_vars$penta2,
    penta3 = survey_vars$penta3,
    pneumo1 = survey_vars$pneumo1, pneumo2 = survey_vars$pneumo2,
    pneumo3 = survey_vars$pneumo3,
    rota1 = survey_vars$rota1, rota2 = survey_vars$rota2,
    rota3 = survey_vars$rota3,
    ipv = survey_vars$ipv, hepb0 = survey_vars$hepb0,
    yellowfever = survey_vars$yellowfever
  )

  # Apply DPT -> Pentavalent fallback when primary variable is missing
  registry <- .epi_vaccine_registry()
  fallback_applied <- character(0)

  for (vax_name in names(vaccine_mapping)) {
    var_name <- vaccine_mapping[[vax_name]]
    if (!is.null(var_name) && !(var_name %in% names(dhs_kr))) {
      reg_entry <- registry[[vax_name]]
      if (!is.null(reg_entry$fallback_var) &&
          reg_entry$fallback_var %in% names(dhs_kr)) {
        vaccine_mapping[[vax_name]] <- reg_entry$fallback_var
        fallback_applied <- c(
          fallback_applied,
          paste0(vax_name, ": ", var_name, " -> ",
                 reg_entry$fallback_var, " (", reg_entry$fallback_source, ")")
        )
      }
    }
  }

  if (length(fallback_applied) > 0) {
    cli::cli_alert_info(
      "Variable fallbacks applied: {paste(fallback_applied, collapse = '; ')}"
    )
  }

  available_vaccines <- sapply(vaccine_mapping, function(v) !is.null(v) && v %in% names(dhs_kr))
  available_vaccine_cols <- unlist(vaccine_mapping[available_vaccines])

  # Select columns
  select_cols <- unique(c(
    survey_vars$cluster, survey_vars$age,
    available_vaccine_cols
  ))
  if (include_survey_vars) {
    select_cols <- unique(c(select_cols, survey_vars$weight, survey_vars$stratum))
  }
  select_cols <- select_cols[select_cols %in% names(dhs_kr)]

  kr <- dhs_kr |>
    dplyr::select(dplyr::all_of(select_cols)) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), haven::zap_labels)) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), as.vector)) |>
    dplyr::mutate(
      cluster_id = .data[[survey_vars$cluster]],
      age_months = .data[[survey_vars$age]]
    )

  if (include_survey_vars) {
    kr <- kr |>
      dplyr::mutate(
        survey_weight = .data[[survey_vars$weight]] / 1e6,
        stratum_id = .data[[survey_vars$stratum]]
      )
  }

  # Filter to eligible age range
  kr <- kr |>
    dplyr::filter(
      age_months >= age_min_months,
      age_months <= age_max_months
    )

  if (nrow(kr) == 0) {
    cli::cli_abort(
      "No eligible children found in age range {age_min_months}-{age_max_months} months"
    )
  }

  cli::cli_alert_info(
    "Found {format(nrow(kr), big.mark = ',')} children aged {age_min_months}-{age_max_months} months"
  )

  # Add binary vaccination columns for each available vaccine
  # DHS: 1 = vaccination card, 2 = reported by mother, 3 = both
  for (vax_name in names(available_vaccines)[available_vaccines]) {
    var_name <- vaccine_mapping[[vax_name]]
    col_name <- paste0("vax_", vax_name)
    kr[[col_name]] <- as.integer(!is.na(kr[[var_name]]) & kr[[var_name]] %in% c(1, 2, 3))
  }

  # Add fully_vaccinated if all required vaccines are present
  required_for_fv <- c("bcg", "dpt3", "polio3", "measles1")
  if (all(required_for_fv %in% names(available_vaccines)[available_vaccines])) {
    kr$vax_fully_vaccinated <- as.integer(
      kr$vax_bcg == 1 & kr$vax_dpt3 == 1 &
      kr$vax_polio3 == 1 & kr$vax_measles1 == 1
    )
  }

  kr
}
