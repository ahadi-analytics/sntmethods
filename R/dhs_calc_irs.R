#' Calculate IRS Coverage from DHS Data
#'
#' Estimates Indoor Residual Spraying (IRS) coverage at the household level
#' using survey-weighted methods from DHS Household Records data.
#'
#' @param dhs_hr DHS Household Records (HR) dataset.
#' @param survey_vars Named list mapping DHS variable names. Required keys:
#'   \itemize{
#'     \item `cluster`: Cluster/PSU ID (default: "hv021")
#'     \item `weight`: Survey weight (default: "hv005")
#'     \item `stratum`: Stratum variable (default: "hv022")
#'     \item `irs`: IRS variable (default: "hv253")
#'   }
#' @param region_var Optional column name to use as grouping variable.
#' @param gps_data Optional DHS GPS dataset.
#' @param gps_vars Named list for GPS variables.
#' @param shapefile Optional sf object with administrative boundaries.
#' @param admin_level Character vector of admin columns from shapefile.
#' @param join_nearest Logical; if TRUE, assigns clusters outside polygons
#'   to nearest admin unit.
#'
#' @return Tibble with IRS estimates including:
#'   dhs_irs, dhs_irs_low, dhs_irs_upp, dhs_n_households_irs, dhs_n_sprayed.
#'
#' @seealso [calc_irs_mbg()] for cluster-level MBG inputs
#' @export
calc_irs_dhs_core <- function(
  dhs_hr,
  survey_vars = list(
    cluster = "hv021",
    weight = "hv005",
    stratum = "hv022",
    irs = "hv253"
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
  hr <- .prepare_irs_data(
    dhs_hr = dhs_hr,
    survey_vars = survey_vars,
    include_survey_vars = TRUE
  )

  # Determine grouping variable
  class_var <- NULL
  if (!is.null(region_var)) {
    if (!region_var %in% names(hr)) {
      cli::cli_abort("Column {.var {region_var}} not found in data.")
    }
    class_var <- region_var
  } else if ("hv024" %in% names(hr)) {
    class_var <- "hv024"
    cli::cli_alert_info("Using hv024 (region) as grouping variable")
  }

  # Set up survey design
  use_strata <- dplyr::n_distinct(hr$stratum_id) > 1
  if (use_strata) {
    survey_options <- options(survey.lonely.psu = "certainty")
    on.exit(options(survey_options), add = TRUE)
    des <- survey::svydesign(
      ids = ~cluster_id, strata = ~stratum_id,
      weights = ~survey_weight, data = hr, nest = TRUE
    )
  } else {
    des <- survey::svydesign(
      ids = ~cluster_id, weights = ~survey_weight,
      data = hr, nest = TRUE
    )
  }

  # Calculate IRS coverage
  if (!is.null(class_var)) {
    group_formula <- stats::as.formula(paste("~", class_var))
    irs_results <- tryCatch({
      survey::svyby(
        ~sprayed, by = group_formula, design = des,
        FUN = survey::svymean, vartype = "ci",
        na.rm = TRUE, keep.names = FALSE
      ) |> tibble::as_tibble()
    }, error = function(e) {
      if (grepl("has only one PSU", e$message)) {
        des_ns <- survey::svydesign(
          ids = ~cluster_id, weights = ~survey_weight,
          data = hr, nest = TRUE
        )
        survey::svyby(
          ~sprayed, by = group_formula, design = des_ns,
          FUN = survey::svymean, vartype = "ci",
          na.rm = TRUE, keep.names = FALSE
        ) |> tibble::as_tibble()
      } else stop(e)
    })
    # svyby with single variable produces ci_l/ci_u (no suffix)
    irs_results <- dplyr::rename(irs_results,
      dhs_irs = sprayed, dhs_irs_low = ci_l, dhs_irs_upp = ci_u
    )
  } else {
    irs_mean <- survey::svymean(~sprayed, design = des, na.rm = TRUE)
    irs_ci <- stats::confint(irs_mean)
    irs_results <- tibble::tibble(
      level = "National",
      dhs_irs = as.numeric(irs_mean["sprayed"]),
      dhs_irs_low = irs_ci["sprayed", 1],
      dhs_irs_upp = irs_ci["sprayed", 2]
    )
  }

  # Sample sizes
  if (!is.null(class_var)) {
    sample_sizes <- hr |>
      dplyr::group_by(.data[[class_var]]) |>
      dplyr::summarise(
        dhs_n_households_irs = dplyr::n(),
        dhs_n_sprayed = sum(sprayed == 1, na.rm = TRUE),
        .groups = "drop"
      )
    irs_results <- irs_results |>
      dplyr::left_join(sample_sizes, by = class_var)
  } else {
    irs_results$dhs_n_households_irs <- nrow(hr)
    irs_results$dhs_n_sprayed <- sum(hr$sprayed == 1, na.rm = TRUE)
  }

  # Format
  irs_results <- irs_results |>
    dplyr::mutate(
      dhs_irs = round(dhs_irs, 2),
      dhs_irs_low = pmax(0, round(dhs_irs_low, 2)),
      dhs_irs_upp = pmin(1, round(dhs_irs_upp, 2)),
      dhs_n_households_irs = as.integer(dhs_n_households_irs),
      dhs_n_sprayed = as.integer(dhs_n_sprayed)
    ) |>
    dplyr::select(-dplyr::any_of(c("level")))

  tibble::as_tibble(irs_results)
}

#' Calculate IRS Coverage from DHS Data (with metadata)
#'
#' Wrapper around calc_irs_dhs_core() that also returns metadata and
#' data dictionary.
#'
#' @inheritParams calc_irs_dhs_core
#' @return List with data, dict, and metadata.
#' @export
calc_irs_dhs <- function(
  dhs_hr,
  survey_vars = list(
    cluster = "hv021",
    weight = "hv005",
    stratum = "hv022",
    irs = "hv253"
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
  irs_data <- calc_irs_dhs_core(
    dhs_hr = dhs_hr,
    survey_vars = survey_vars,
    region_var = region_var,
    gps_data = gps_data,
    gps_vars = gps_vars,
    shapefile = shapefile,
    admin_level = admin_level,
    join_nearest = join_nearest
  )

  labels <- tibble::tribble(
    ~variable, ~label_en, ~label_fr, ~dhs_variable, ~numerator, ~denominator, ~dhs_numerator_var, ~dhs_denominator_var, ~notes,
    "dhs_irs", "IRS coverage", "Couverture de la PID", "hv253", "Households sprayed in last 12 months", "All households", "hv253", NA_character_, "HR module; household-level IRS in last 12 months",
    "dhs_irs_low", "IRS - lower 95% CI", "PID - IC 95% inferieur", "hv253", NA_character_, NA_character_, NA_character_, NA_character_, "Survey-weighted 95% CI, clamped to [0,1]",
    "dhs_irs_upp", "IRS - upper 95% CI", "PID - IC 95% superieur", "hv253", NA_character_, NA_character_, NA_character_, NA_character_, "Survey-weighted 95% CI, clamped to [0,1]",
    "dhs_n_households_irs", "Number of households (denominator)", "Nombre de menages (denominateur)", NA_character_, NA_character_, NA_character_, NA_character_, NA_character_, "Unweighted count",
    "dhs_n_sprayed", "Number of households sprayed (numerator)", "Nombre de menages pulverises (numerateur)", "hv253", NA_character_, NA_character_, NA_character_, NA_character_, "Unweighted count"
  )

  dict <- sntutils::build_dictionary(irs_data)
  dict <- .enrich_dhs_dictionary(dict, labels)

  list(
    data = irs_data,
    dict = dict,
    metadata = list(
      analysis_type = "IRS (Indoor Residual Spraying)",
      file_type = "HR",
      processed_date = Sys.Date()
    )
  )
}
