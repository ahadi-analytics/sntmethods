#' Calculate EPI Coverage from DHS Data
#'
#' Estimates vaccination coverage using survey-weighted methods from
#' DHS Children's Recode data.
#'
#' @param dhs_kr DHS Children's Recode (KR) dataset.
#' @param indicators Character vector of vaccines to calculate. Options:
#'   "bcg", "dpt1", "dpt2", "dpt3", "polio1", "polio2", "polio3",
#'   "measles1", "measles2", "vita1", "vita2", "malaria", "fully_vaccinated".
#'   Default: c("bcg", "dpt3", "measles1", "fully_vaccinated").
#' @param age_min_months Minimum age in months (default: 12).
#' @param age_max_months Maximum age in months (default: 23).
#' @param survey_vars Named list mapping DHS variable names.
#' @param region_var Optional column name to use as grouping variable.
#' @param gps_data Optional DHS GPS dataset.
#' @param gps_vars Named list for GPS variables.
#' @param shapefile Optional sf object with administrative boundaries.
#' @param admin_level Character vector of admin columns.
#' @param join_nearest Logical.
#'
#' @return Tibble with EPI estimates. For each vaccine: `dhs_epi_<vaccine>`,
#'   `dhs_epi_<vaccine>_low`, `dhs_epi_<vaccine>_upp`. Plus `dhs_n_epi_eligible`.
#'
#' @seealso [calc_epi_mbg()] for cluster-level MBG inputs
#' @export
calc_epi_dhs_core <- function(
  dhs_kr,
  indicators = c("bcg", "dpt3", "measles1", "fully_vaccinated"),
  age_min_months = 12,
  age_max_months = 23,
  survey_vars = list(
    cluster = "v021",
    weight = "v005",
    stratum = "v022",
    age = "hw1",
    bcg = "h2",
    dpt1 = "h3", dpt2 = "h4", dpt3 = "h5",
    polio1 = "h6", polio2 = "h7", polio3 = "h8",
    measles1 = "h9", measles2 = "h9a",
    vita1 = "h33", vita2 = "h33a",
    malaria = "h68"
  ),
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
  # Prepare data using shared helper
  kr <- .prepare_epi_data(
    dhs_kr = dhs_kr,
    survey_vars = survey_vars,
    age_min_months = age_min_months,
    age_max_months = age_max_months,
    include_survey_vars = TRUE
  )

  # Determine grouping
  class_var <- NULL
  if (!is.null(region_var)) {
    if (!region_var %in% names(dhs_kr)) {
      cli::cli_abort("Column {.var {region_var}} not found in `dhs_kr`.")
    }
    class_var <- region_var
  } else if ("v024" %in% names(kr)) {
    class_var <- "v024"
    cli::cli_alert_info("Using v024 (region) as grouping variable")
  }

  # Set up survey design
  use_strata <- dplyr::n_distinct(kr$stratum_id) > 1
  if (use_strata) {
    survey_options <- options(survey.lonely.psu = "certainty")
    on.exit(options(survey_options), add = TRUE)
    des <- survey::svydesign(
      ids = ~cluster_id, strata = ~stratum_id,
      weights = ~survey_weight, data = kr, nest = TRUE
    )
  } else {
    des <- survey::svydesign(
      ids = ~cluster_id, weights = ~survey_weight,
      data = kr, nest = TRUE
    )
  }

  # Build indicator formula from available vax_ columns
  vax_cols <- paste0("vax_", indicators)
  available_vax <- vax_cols[vax_cols %in% names(kr)]

  if (length(available_vax) == 0) {
    cli::cli_abort("No requested vaccine indicators found in prepared data")
  }

  indicator_formula <- stats::as.formula(
    paste("~", paste(available_vax, collapse = " + "))
  )

  # Calculate
  if (!is.null(class_var)) {
    group_formula <- stats::as.formula(paste("~", class_var))
    epi_results <- tryCatch({
      survey::svyby(
        indicator_formula, by = group_formula, design = des,
        FUN = survey::svymean, vartype = "ci",
        na.rm = TRUE, keep.names = FALSE
      ) |> tibble::as_tibble()
    }, error = function(e) {
      if (grepl("has only one PSU", e$message)) {
        des_ns <- survey::svydesign(
          ids = ~cluster_id, weights = ~survey_weight,
          data = kr, nest = TRUE
        )
        survey::svyby(
          indicator_formula, by = group_formula, design = des_ns,
          FUN = survey::svymean, vartype = "ci",
          na.rm = TRUE, keep.names = FALSE
        ) |> tibble::as_tibble()
      } else stop(e)
    })
  } else {
    epi_means <- survey::svymean(indicator_formula, design = des, na.rm = TRUE)
    epi_ci <- stats::confint(epi_means)
    epi_results <- tibble::tibble(level = "National")
    for (v in available_vax) {
      epi_results[[v]] <- as.numeric(epi_means[v])
      epi_results[[paste0("ci_l.", v)]] <- epi_ci[v, 1]
      epi_results[[paste0("ci_u.", v)]] <- epi_ci[v, 2]
    }
  }

  # Rename vax_ columns to dhs_epi_ format
  for (ind in indicators) {
    vax_col <- paste0("vax_", ind)
    if (vax_col %in% names(epi_results)) {
      epi_results <- epi_results |>
        dplyr::rename(
          !!paste0("dhs_epi_", ind) := !!vax_col,
          !!paste0("dhs_epi_", ind, "_low") := !!paste0("ci_l.", vax_col),
          !!paste0("dhs_epi_", ind, "_upp") := !!paste0("ci_u.", vax_col)
        )
    }
  }

  # Sample sizes
  if (!is.null(class_var)) {
    sample_sizes <- kr |>
      dplyr::group_by(.data[[class_var]]) |>
      dplyr::summarise(dhs_n_epi_eligible = dplyr::n(), .groups = "drop")
    epi_results <- epi_results |>
      dplyr::left_join(sample_sizes, by = class_var)
  } else {
    epi_results$dhs_n_epi_eligible <- nrow(kr)
  }

  # Format
  epi_cols <- names(epi_results)[grepl("^dhs_epi_", names(epi_results))]
  epi_results <- epi_results |>
    dplyr::mutate(
      dplyr::across(dplyr::all_of(epi_cols), ~ round(.x, 2)),
      dplyr::across(dplyr::matches("_low$"), ~ pmax(0, .)),
      dplyr::across(dplyr::matches("_upp$"), ~ pmin(1, .)),
      dhs_n_epi_eligible = as.integer(dhs_n_epi_eligible)
    ) |>
    dplyr::select(-dplyr::any_of(c("level")))

  tibble::as_tibble(epi_results)
}

#' Calculate EPI Coverage from DHS Data (with metadata)
#'
#' @inheritParams calc_epi_dhs_core
#' @return List with data, dict, and metadata.
#' @export
calc_epi_dhs <- function(
  dhs_kr,
  indicators = c("bcg", "dpt3", "measles1", "fully_vaccinated"),
  age_min_months = 12,
  age_max_months = 23,
  survey_vars = list(
    cluster = "v021",
    weight = "v005",
    stratum = "v022",
    age = "hw1",
    bcg = "h2",
    dpt1 = "h3", dpt2 = "h4", dpt3 = "h5",
    polio1 = "h6", polio2 = "h7", polio3 = "h8",
    measles1 = "h9", measles2 = "h9a",
    vita1 = "h33", vita2 = "h33a",
    malaria = "h68"
  ),
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
  epi_data <- calc_epi_dhs_core(
    dhs_kr = dhs_kr,
    indicators = indicators,
    age_min_months = age_min_months,
    age_max_months = age_max_months,
    survey_vars = survey_vars,
    region_var = region_var,
    gps_data = gps_data,
    gps_vars = gps_vars,
    shapefile = shapefile,
    admin_level = admin_level,
    join_nearest = join_nearest
  )

  labels <- tibble::tribble(
    ~variable, ~label_en, ~label_fr, ~dhs_variable, ~numerator, ~denominator, ~dhs_numerator_var, ~dhs_denominator_var, ~dhs_recode, ~indicator_category, ~wmr_cascade_step, ~age_group, ~units, ~notes,
    "dhs_n_epi_eligible", "Number of EPI-eligible children (denominator)", "Nombre d'enfants eligibles au PEV (denominateur)", "h2-h9", NA_character_, NA_character_, NA_character_, NA_character_, "KR", "Immunization", NA_integer_, "12-23 months", "count", "Unweighted count; children 12-23 months"
  )

  # Add dynamic labels for each vaccine indicator
  vaccine_labels_list <- list(
    bcg = list(en = "BCG vaccination coverage", fr = "Couverture vaccinale BCG", dhs_var = "h2", name = "BCG"),
    dpt1 = list(en = "DPT1 vaccination coverage", fr = "Couverture vaccinale DTC1", dhs_var = "h3", name = "DPT1"),
    dpt2 = list(en = "DPT2 vaccination coverage", fr = "Couverture vaccinale DTC2", dhs_var = "h5", name = "DPT2"),
    dpt3 = list(en = "DPT3 vaccination coverage", fr = "Couverture vaccinale DTC3", dhs_var = "h7", name = "DPT3"),
    measles1 = list(en = "Measles 1 vaccination coverage", fr = "Couverture vaccinale rougeole 1", dhs_var = "h9", name = "Measles 1"),
    fully_vaccinated = list(en = "Fully vaccinated", fr = "Completement vaccine", dhs_var = "h2-h9", name = "all vaccines")
  )
  for (vax in names(vaccine_labels_list)) {
    info <- vaccine_labels_list[[vax]]
    vax_var <- paste0("dhs_epi_", vax)
    if (vax_var %in% names(epi_data)) {
      labels <- dplyr::bind_rows(labels, tibble::tribble(
        ~variable, ~label_en, ~label_fr, ~dhs_variable, ~numerator, ~denominator, ~dhs_numerator_var, ~dhs_denominator_var, ~dhs_recode, ~indicator_category, ~wmr_cascade_step, ~age_group, ~units, ~notes,
        vax_var, info$en, info$fr, info$dhs_var, paste0("Children 12-23m with ", info$name), "Children 12-23 months", info$dhs_var, "hw1", "KR", "Immunization", NA_integer_, "12-23 months", "proportion (0-1)", "KR module; children 12-23 months; card + recall",
        paste0(vax_var, "_low"), paste0(info$en, " - lower 95% CI"), paste0(info$fr, " - IC 95% inferieur"), info$dhs_var, NA_character_, NA_character_, NA_character_, NA_character_, "KR", "Immunization", NA_integer_, "12-23 months", "proportion (0-1)", "Survey-weighted 95% CI, clamped to [0,1]",
        paste0(vax_var, "_upp"), paste0(info$en, " - upper 95% CI"), paste0(info$fr, " - IC 95% superieur"), info$dhs_var, NA_character_, NA_character_, NA_character_, NA_character_, "KR", "Immunization", NA_integer_, "12-23 months", "proportion (0-1)", "Survey-weighted 95% CI, clamped to [0,1]"
      ))
    }
  }

  dict <- sntutils::build_dictionary(epi_data)
  dict <- .enrich_dhs_dictionary(dict, labels)

  list(
    data = epi_data,
    dict = dict,
    metadata = list(
      analysis_type = "EPI (Expanded Programme on Immunization)",
      file_type = "KR",
      age_group = paste0(age_min_months, "-", age_max_months, " months"),
      indicators = indicators,
      processed_date = Sys.Date()
    )
  )
}
