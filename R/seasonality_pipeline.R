# ==============================================================================
# Malaria Seasonality Analysis Pipeline
#
# Self-contained — source this single file to load the full pipeline.
# source("seasonality_pipeline.R")
#
# sntutils folder contract
# ────────────────────────
# initialize_project_structure() creates all root folders once at project setup.
# setup_project_paths()          provides shortcuts (paths$val_fig, etc.).
# This pipeline NEVER calls dir.create() on root folders (val_fig, val_tbl).
# It only creates the specific subfolders that may not yet exist.
#
# Subfolder locations used by this pipeline:
#   figures → paths$val_fig/                     (heatmaps)
#           → paths$val_fig/rain_seas/            (rainfall seasonality + SMC maps)
#           → paths$val_fig/case_seas/            (cases seasonality maps)
#           → paths$val_fig/ov5/                  (cases SMC timing maps)
#           → paths$val_fig/cas_rain_graphs/      (rolling % overlay graphs)
#           → paths$val_fig/cas_v_pluie/          (median cases vs rainfall)
#           → paths$val_fig/cas_v_pluie_crude/    (crude time-series cases vs rainfall)
#   tables  → paths$val_tbl/                      (seasonality summaries)
#           → paths$val_tbl/rainfall/             (rainfall block tables)
#           → paths$val_tbl/cases_ov5_block/      (cases block tables)
# ==============================================================================


# ==============================================================================
# MASTER FUNCTION
# ==============================================================================

#' Run the Full Malaria Seasonality Analysis Pipeline
#'
#' Executes the complete seasonality analysis workflow in a single call,
#' covering heatmap visualisation, rolling-window seasonality classification,
#' 3m/4m/5m block concentration analysis, rainfall-vs-cases overlay graphs,
#' SMC eligibility/timing maps, and crude cases vs rainfall dual-axis plots.
#'
#' Each step is optional via \code{steps}. Steps that depend on earlier steps
#' read results from memory when available, and fall back to reading saved
#' files from disk when not.
#'
#' @param iso3 Character. ISO3 country code in lowercase (e.g. \code{"gha"}).
#' @param adm0_name Character. Country display name for plot titles.
#' @param paths Named list from \code{sntutils::setup_project_paths()}.
#'   When supplied, all output is routed through \code{paths$val_fig} and
#'   \code{paths$val_tbl}. The \code{output_dir} argument is ignored.
#'   Default: \code{NULL}.
#' @param base_path Character or \code{NULL}. Base project directory passed to
#'   \code{sntutils::setup_project_paths()}. Ignored when \code{paths} is
#'   supplied. Default: \code{NULL}.
#' @param climate_dir Character or \code{NULL}. Direct path to the processed
#'   climate/rainfall folder. Only used when neither \code{paths} nor
#'   \code{base_path} is supplied. Default: \code{NULL}.
#' @param dhis2_dir Character or \code{NULL}. Direct path to the processed
#'   DHIS2 folder. Only used when neither \code{paths} nor \code{base_path}
#'   is supplied. Default: \code{NULL}.
#' @param admin_shp_dir Character or \code{NULL}. Direct path to the processed
#'   shapefiles folder. Needed for \code{"smc_maps"} when using direct paths.
#'   Default: \code{NULL}.
#' @param type Character. Data type(s) to analyse. One of \code{"both"}
#'   (default), \code{"rainfall"}, or \code{"cases"}. The \code{"graphs"} step
#'   requires \code{"both"} and is skipped otherwise.
#' @param steps Character vector. Pipeline steps to run. Any subset of
#'   \code{c("heatmap", "seasonality", "blocks", "graphs", "smc_maps",
#'   "case_rain_plots")}. Default: all six steps.
#' @param s_year Integer or \code{NULL}. First year to include. Default:
#'   \code{NULL} (all available years).
#' @param e_year Integer or \code{NULL}. Last year to include. Default:
#'   \code{NULL} (all available years).
#' @param adm1_var Character. Column name for admin level 1. Default:
#'   \code{"adm1"}.
#' @param adm2_var Character. Column name for admin level 2. Default:
#'   \code{"adm2"}.
#' @param year_var Character. Column name for year. Default: \code{"year"}.
#' @param month_var Character. Column name for month (1-12). Default:
#'   \code{"month"}.
#' @param value_var_rainfall Character or \code{NULL}. Rainfall metric column.
#'   Default: \code{NULL} (uses \code{"mean_rainfall_mm"}).
#' @param value_var_cases Character or \code{NULL}. Cases metric column.
#'   Default: \code{NULL} (uses \code{"conf"}).
#' @param adm_level Character. Heatmap y-axis level. One of \code{"adm1"}
#'   (default) or \code{"adm2"}.
#' @param viridis_option Character. Viridis colour palette. Default:
#'   \code{"viridis"}.
#' @param x_breaks_by Numeric. Heatmap x-axis tick interval in years.
#'   Default: \code{0.5}.
#' @param analysis_start_month Integer (1-12). Month to begin rolling windows.
#'   Default: \code{1}.
#' @param seasonality_threshold Numeric. Percentage of annual total a 4-month
#'   window must reach to be classified as seasonal. Default: \code{60}.
#' @param max_non_seasonal_years Integer. Maximum non-seasonal years a location
#'   may have and still be classified as Seasonal. \strong{Required.}
#'   Formula: \code{total_years - min_seasonal_years}.
#' @param min_years_required Integer. Minimum years of data to run the
#'   seasonality step. Default: \code{6}.
#' @param n_cumulative_maps Integer. Number of cumulative threshold maps to
#'   generate. Auto-generates thresholds counting down from \code{n_years - 1}.
#'   For example, with 12 years of data and \code{n_cumulative_maps = 4}:
#'   \{11\}, \{11,10\}, \{11,10,9\}, \{11,10,9,8\}. Default: \code{4}.
#' @param cumulative_thresholds List of integer vectors or \code{NULL}.
#'   Manual override for cumulative threshold maps. Each vector contains the
#'   \code{SeasonalYears} values to classify as Seasonal at that threshold.
#'   When \code{NULL} (default), auto-generated from \code{n_cumulative_maps}.
#'   Example: \code{list(c(11), c(11, 10), c(11, 10, 9))}.
#' @param smc_eligible_districts Character vector or \code{NULL}. Manual SMC
#'   eligibility list. Default \code{NULL} auto-derives from the seasonality
#'   classification. Default: \code{NULL}.
#' @param smc_additional_districts Character vector or \code{NULL}. Districts
#'   to add to the derived SMC list. Default: \code{NULL}.
#' @param smc_remove_districts Character vector or \code{NULL}. Districts to
#'   remove from the derived SMC list. Applied last. Default: \code{NULL}.
#' @param case_stratification Character. Age stratification for cases data.
#'   One of \code{"ov5"} (over 5, default), \code{"u5"} (under 5), or
#'   \code{"all"} (all ages). Controls the output subfolder and file prefix
#'   so that multiple stratifications can be run back to back without
#'   overwriting each other. Pair with \code{value_var_cases} to point at
#'   the correct column (e.g. \code{value_var_cases = "conf_u5"} when
#'   \code{case_stratification = "u5"}).
#' @param case_var_ov5 Character. Column name for confirmed cases over 5 years.
#'   Default: \code{"conf_ov5"}.
#' @param case_var_u5 Character. Column name for confirmed cases under 5 years.
#'   Default: \code{"conf_u5"}.
#' @param rainfall_total_var Character. Column name for total rainfall (mm).
#'   Default: \code{"total_rainfall_mm"}.
#' @param case_rain_s_date Character or \code{NULL}. Start date filter for
#'   case_rain_plots in \code{"YYYY-MM-DD"} format. Default: \code{NULL}
#'   (uses \code{s_year} if supplied, otherwise all data).
#' @param case_rain_e_date Character or \code{NULL}. End date filter for
#'   case_rain_plots. Default: \code{NULL}.
#' @param panels_per_row Integer. Number of district panels per row in the
#'   cases vs rainfall faceted plots. Default: \code{3}.
#' @param output_dir Character. Root output folder. Only used for Option C
#'   (direct paths — no sntutils). Default: \code{here::here("outputs")}.
#' @param dpi Numeric. Resolution for all saved plots. Default: \code{400}.
#'
#' @return A named list (invisibly):
#'   \describe{
#'     \item{\code{heatmap}}{Named by type: \code{plot}, \code{data},
#'       \code{output_path}.}
#'     \item{\code{seasonality}}{Named by type: \code{detailed_results},
#'       \code{yearly_summary}, \code{location_summary}, \code{maps}.}
#'     \item{\code{blocks}}{Named by type: \code{summary}, \code{detailed},
#'       \code{frequency}.}
#'     \item{\code{graphs}}{Named by province: ggplot objects.}
#'     \item{\code{smc_maps}}{Named by type: \code{timing_maps},
#'       \code{coverage_maps}, \code{eligible}.}
#'     \item{\code{case_rain_plots}}{Named list with \code{median} and
#'       \code{crude}, each containing named ggplot objects per adm1.}
#'   }
#'
#' @examples
#' # Full pipeline — paths object already set up
#' # paths <- sntutils::setup_project_paths()
#' # run_seasonality_pipeline(
#' #   iso3 = "gha", adm0_name = "Ghana",
#' #   paths = paths, max_non_seasonal_years = 4
#' # )
#'
#' # 12 years of data — auto-generate cumulative maps from year 11 down
#' # run_seasonality_pipeline(
#' #   iso3 = "gha", adm0_name = "Ghana",
#' #   base_path = Sys.getenv("AHADI_ONEDRIVE_PROJECT"),
#' #   s_year = 2012, max_non_seasonal_years = 2,
#' #   n_cumulative_maps = 4
#' # )
#'
#' # Manual cumulative thresholds
#' # run_seasonality_pipeline(
#' #   ...,
#' #   cumulative_thresholds = list(c(11), c(11,10), c(11,10,9), c(11,10,9,8))
#' # )
#'
#' @export
run_seasonality_pipeline <- function(
    iso3,
    adm0_name,

    # ── paths (provide one of three options) ──────────────────────────────────
    paths         = NULL,
    base_path     = NULL,
    climate_dir   = NULL,
    dhis2_dir     = NULL,
    admin_shp_dir = NULL,

    # ── what to run ───────────────────────────────────────────────────────────
    type  = c("both", "rainfall", "cases"),
    steps = c("heatmap", "seasonality", "blocks",
              "graphs", "smc_maps", "case_rain_plots"),

    # ── year range ────────────────────────────────────────────────────────────
    s_year = NULL,
    e_year = NULL,

    # ── column name overrides ─────────────────────────────────────────────────
    adm1_var           = "adm1",
    adm2_var           = "adm2",
    year_var           = "year",
    month_var          = "month",
    value_var_rainfall = NULL,
    value_var_cases    = NULL,

    # ── heatmap aesthetics ────────────────────────────────────────────────────
    adm_level      = c("adm1", "adm2"),
    viridis_option = "viridis",
    x_breaks_by    = 0.5,

    # ── seasonality parameters ────────────────────────────────────────────────
    analysis_start_month   = 1,
    seasonality_threshold  = 60,
    max_non_seasonal_years,          
    min_years_required     = 6,

    # ── cumulative threshold maps ─────────────────────────────────────────────
    n_cumulative_maps     = 4,
    cumulative_thresholds = NULL,       

    # ── SMC eligibility ───────────────────────────────────────────────────────
    smc_eligible_districts   = NULL,
    smc_additional_districts = NULL,
    smc_remove_districts     = NULL,

    # ── case stratification ───────────────────────────────────────────────────
    case_stratification = "ov5",         # "ov5", "u5", or "all"

    # ── case vs rainfall plot options ─────────────────────────────────────────
    case_var_ov5       = "conf_ov5",
    case_var_u5        = "conf_u5",
    rainfall_total_var = "total_rainfall_mm",
    case_rain_s_date   = NULL,          
    case_rain_e_date   = NULL,
    panels_per_row     = 3,

    # ── output (Option C only — ignored for sntutils users) ───────────────────
    output_dir = here::here("outputs"),
    dpi        = 400
) {

  # ── validate inputs ─────────────────────────────────────────────────────────

  type      <- match.arg(type)
  adm_level <- match.arg(adm_level)

  valid_steps <- c("heatmap", "seasonality", "blocks",
                   "graphs", "smc_maps", "case_rain_plots")
  bad_steps   <- setdiff(steps, valid_steps)
  if (length(bad_steps) > 0) {
    cli::cli_abort(c(
      "Invalid step{?s}: {.val {bad_steps}}",
      "i" = "Valid options: {.val {valid_steps}}"
    ))
  }

  valid_strat <- c("ov5", "u5", "all")
  if (!case_stratification %in% valid_strat) {
    cli::cli_abort(c(
      "{.arg case_stratification} must be one of {.val {valid_strat}}.",
      "x" = "Got {.val {case_stratification}}."
    ))
  }

  if (!is.character(iso3) || nchar(trimws(iso3)) == 0)
    cli::cli_abort("{.arg iso3} must be a non-empty character string.")

  if (!is.character(adm0_name) || nchar(trimws(adm0_name)) == 0)
    cli::cli_abort("{.arg adm0_name} must be a non-empty character string.")

  if (!is.numeric(seasonality_threshold) ||
      seasonality_threshold <= 0 || seasonality_threshold > 100)
    cli::cli_abort("{.arg seasonality_threshold} must be between 0 and 100.")

  if (missing(max_non_seasonal_years)) {
    cli::cli_abort(c(
      "{.arg max_non_seasonal_years} must be provided.",
      "i" = "Formula: total_years \u2212 minimum_seasonal_years.",
      "i" = "Example: 12 years data, 10/12 rule \u2192 use {.val 2}."
    ))
  }

  if (!is.numeric(max_non_seasonal_years) || max_non_seasonal_years < 0)
    cli::cli_abort("{.arg max_non_seasonal_years} must be a non-negative integer.")

  if (!is.null(cumulative_thresholds) && !is.list(cumulative_thresholds)) {
    cli::cli_abort(c(
      "{.arg cumulative_thresholds} must be a list of integer vectors.",
      "i" = "Example: {.code list(c(11), c(11, 10), c(11, 10, 9))}"
    ))
  }

  # ── step dependency checks ──────────────────────────────────────────────────

  if ("smc_maps" %in% steps &&
      !("blocks" %in% steps) &&
      !("seasonality" %in% steps) &&
      is.null(smc_eligible_districts)) {
    cli::cli_abort(c(
      "Step {.val smc_maps} has unresolved dependencies.",
      "i" = "Add {.val 'blocks'} and {.val 'seasonality'} to {.arg steps}, or",
      "i" = "supply {.arg smc_eligible_districts} if those steps were run previously."
    ))
  }

  # ── resolve dirs ────────────────────────────────────────────────────────────

  dirs <- .resolve_dirs(
    paths         = paths,
    base_path     = base_path,
    climate_dir   = climate_dir,
    dhis2_dir     = dhis2_dir,
    admin_shp_dir = admin_shp_dir
  )

  # ── resolve output roots ────────────────────────────────────────────────────
  # sntutils users: val_fig / val_tbl already exist — never recreate them
  # Option C users: build under output_dir

  if (dirs$use_sntutils_output) {
    fig_root <- dirs$val_fig
    tbl_root <- dirs$val_tbl
  } else {
    fig_root <- file.path(output_dir, "figures")
    tbl_root <- file.path(output_dir, "tables")
    .ensure_dir(fig_root)
    .ensure_dir(tbl_root)
  }

  # ── types to run ────────────────────────────────────────────────────────────

  types_to_run <- if (type == "both") c("rainfall", "cases") else type

  # ── announce ────────────────────────────────────────────────────────────────

  cli::cli_h1("Seasonality Pipeline: {adm0_name} ({toupper(iso3)})")
  cli::cli_alert_info("Steps:   {.val {steps}}")
  cli::cli_alert_info("Type(s): {.val {types_to_run}}")
  cli::cli_alert_info("Output:  {.path {fig_root}}")

  results <- list()

  # ── STEP 1: HEATMAP ─────────────────────────────────────────────────────────

  if ("heatmap" %in% steps) {
    cli::cli_h2("Step [1/6] Heatmap")
    results$heatmap <- purrr::map(
      purrr::set_names(types_to_run),
      ~ .run_heatmap_step(
        dirs           = dirs,
        iso3           = iso3,
        adm0_name      = adm0_name,
        type           = .x,
        s_year         = s_year,
        e_year         = e_year,
        adm1_var       = adm1_var,
        adm2_var       = adm2_var,
        year_var       = year_var,
        month_var      = month_var,
        value_var      = if (.x == "rainfall") value_var_rainfall else value_var_cases,
        adm_level      = adm_level,
        viridis_option = viridis_option,
        x_breaks_by    = x_breaks_by,
        fig_root       = fig_root,
        dpi            = dpi
      )
    )
  }

  # ── STEP 2: SEASONALITY ─────────────────────────────────────────────────────

  if ("seasonality" %in% steps) {
    cli::cli_h2("Step [2/6] Seasonality Analysis")
    results$seasonality <- purrr::map(
      purrr::set_names(types_to_run),
      ~ .run_seasonality_step(
        dirs                   = dirs,
        iso3                   = iso3,
        adm0_name              = adm0_name,
        type                   = .x,
        s_year                 = s_year,
        e_year                 = e_year,
        adm1_var               = adm1_var,
        adm2_var               = adm2_var,
        year_var               = year_var,
        month_var              = month_var,
        value_var              = if (.x == "rainfall") value_var_rainfall else value_var_cases,
        analysis_start_month   = analysis_start_month,
        seasonality_threshold  = seasonality_threshold,
        max_non_seasonal_years = max_non_seasonal_years,
        min_years_required     = min_years_required,
        n_cumulative_maps      = n_cumulative_maps,
        cumulative_thresholds  = cumulative_thresholds,
        case_stratification    = case_stratification,
        fig_root               = fig_root,
        tbl_root               = tbl_root,
        dpi                    = dpi
      )
    )
  }

  # ── STEP 3: BLOCKS ──────────────────────────────────────────────────────────

  if ("blocks" %in% steps) {
    cli::cli_h2("Step [3/6] Block Concentration Analysis")
    results$blocks <- purrr::map(
      purrr::set_names(types_to_run),
      ~ .run_block_step(
        dirs      = dirs,
        iso3      = iso3,
        type      = .x,
        s_year    = s_year,
        e_year    = e_year,
        adm1_var  = adm1_var,
        adm2_var  = adm2_var,
        year_var  = year_var,
        month_var = month_var,
        value_var = if (.x == "rainfall") value_var_rainfall else value_var_cases,
        case_stratification = case_stratification,
        tbl_root  = tbl_root
      )
    )
  }

  # ── STEP 4: GRAPHS (rolling % overlay) ──────────────────────────────────────

  if ("graphs" %in% steps) {
    cli::cli_h2("Step [4/6] Rolling % Overlay Graphs")
    if (type != "both") {
      cli::cli_warn(c(
        "Step {.val graphs} requires both rainfall and cases.",
        "i" = "Skipping \u2014 {.arg type} is {.val {type}}.",
        "i" = "Set {.arg type = 'both'} to include overlay graphs."
      ))
    } else {
      results$graphs <- .run_graphs_step(
        dirs                = dirs,
        iso3                = iso3,
        adm0_name           = adm0_name,
        s_year              = s_year,
        e_year              = e_year,
        rain_detailed       = results$seasonality$rainfall$detailed_results,
        case_detailed       = results$seasonality$cases$detailed_results,
        case_stratification = case_stratification,
        tbl_root            = tbl_root,
        fig_root            = fig_root,
        dpi                 = dpi
      )
    }
  }

  # ── STEP 5: SMC MAPS ────────────────────────────────────────────────────────

  if ("smc_maps" %in% steps) {
    cli::cli_h2("Step [5/6] SMC Eligibility & Timing Maps")
    if (is.null(dirs$admin_shp)) {
      cli::cli_abort(c(
        "Step {.val smc_maps} requires a shapefile path.",
        "i" = "Provide {.arg paths}, {.arg base_path}, or {.arg admin_shp_dir}."
      ))
    }
    results$smc_maps <- purrr::map(
      purrr::set_names(types_to_run),
      ~ .run_smc_maps_step(
        dirs                     = dirs,
        iso3                     = iso3,
        adm0_name                = adm0_name,
        type                     = .x,
        block_frequency          = results$blocks[[.x]]$frequency,
        location_summary         = results$seasonality[[.x]]$location_summary,
        tbl_root                 = tbl_root,
        case_stratification      = case_stratification,
        smc_eligible_districts   = smc_eligible_districts,
        smc_additional_districts = smc_additional_districts,
        smc_remove_districts     = smc_remove_districts,
        fig_root                 = fig_root,
        dpi                      = dpi
      )
    )
  }

  # ── STEP 6: CASE vs RAINFALL PLOTS ──────────────────────────────────────────

  if ("case_rain_plots" %in% steps) {
    cli::cli_h2("Step [6/6] Cases vs Rainfall Plots")
    results$case_rain_plots <- .run_case_rain_plots_step(
      dirs               = dirs,
      iso3               = iso3,
      adm0_name          = adm0_name,
      adm1_var           = adm1_var,
      adm2_var           = adm2_var,
      year_var           = year_var,
      month_var          = month_var,
      case_var_ov5       = case_var_ov5,
      case_var_u5        = case_var_u5,
      rainfall_total_var = rainfall_total_var,
      case_rain_s_date   = case_rain_s_date,
      case_rain_e_date   = case_rain_e_date,
      s_year             = s_year,
      e_year             = e_year,
      panels_per_row     = panels_per_row,
      fig_root           = fig_root,
      dpi                = dpi
    )
  }

  # ── done ────────────────────────────────────────────────────────────────────

  cli::cli_rule(
    left  = "Pipeline complete",
    right = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  )

  invisible(results)
}


# ==============================================================================
# STEP RUNNERS
# ==============================================================================

#' @keywords internal
.run_heatmap_step <- function(
    dirs, iso3, adm0_name, type, s_year, e_year,
    adm1_var, adm2_var, year_var, month_var, value_var,
    adm_level, viridis_option, x_breaks_by, fig_root, dpi
) {
  cli::cli_alert_info("Heatmap \u2192 {.val {type}}")

  df_raw <- .load_analysis_data(
    dirs = dirs, iso3 = iso3, type = type,
    adm1_var = adm1_var, adm2_var = adm2_var,
    year_var = year_var, month_var = month_var,
    value_var = value_var
  ) |> .filter_years(s_year, e_year)

  df_ready <- .prepare_heatmap_data(df_raw, adm_level = adm_level)

  p <- .build_heatmap_plot(
    df             = df_ready,
    adm0_name      = adm0_name,
    type           = type,
    adm_level      = adm_level,
    viridis_option = viridis_option,
    x_breaks_by    = x_breaks_by
  )

  # Heatmaps go directly in val_fig (no subfolder)
  out <- .save_heatmap_plot(
    plot = p, output_dir = fig_root, filename = NULL,
    iso3 = iso3, type = type, width = 12, height = 8, dpi = dpi
  )

  invisible(list(plot = p, data = df_ready, output_path = out))
}


#' @keywords internal
.run_seasonality_step <- function(
    dirs, iso3, adm0_name, type, s_year, e_year,
    adm1_var, adm2_var, year_var, month_var, value_var,
    analysis_start_month, seasonality_threshold,
    max_non_seasonal_years, min_years_required,
    n_cumulative_maps, cumulative_thresholds,
    case_stratification,
    fig_root, tbl_root, dpi
) {
  cli::cli_alert_info("Seasonality \u2192 {.val {type}}")

  # ── load + validate ─────────────────────────────────────────────────────────

  df <- .load_analysis_data(
    dirs = dirs, iso3 = iso3, type = type,
    adm1_var = adm1_var, adm2_var = adm2_var,
    year_var = year_var, month_var = month_var,
    value_var = value_var
  ) |>
    .filter_years(s_year, e_year) |>
    .build_admin_group(admin_cols = c("adm1", "adm2"))

  available_years <- sort(unique(df$year))
  n_years         <- length(available_years)

  if (n_years < min_years_required) {
    cli::cli_abort(c(
      "Insufficient data for {.val {type}} seasonality.",
      "x" = "Need \u2265 {min_years_required} years; found {n_years}.",
      "i" = "Adjust {.arg s_year}/{.arg e_year} or {.arg min_years_required}."
    ))
  }

  cli::cli_alert_info(
    "Data: {min(available_years)}\u2013{max(available_years)} ({n_years} years)"
  )

  # ── analysis ────────────────────────────────────────────────────────────────

  blocks           <- .generate_rolling_blocks(df, analysis_start_month)
  detailed_results <- .calculate_seasonality(df, blocks, seasonality_threshold)
  yearly_summary   <- .build_yearly_summary(detailed_results)
  location_summary <- .build_location_summary(yearly_summary, max_non_seasonal_years)

  n_s  <- sum(location_summary$Seasonality == "Seasonal",     na.rm = TRUE)
  n_ns <- sum(location_summary$Seasonality == "Not Seasonal", na.rm = TRUE)
  cli::cli_alert_success("Classification: {n_s} seasonal, {n_ns} not seasonal.")

  # ── save tables ─────────────────────────────────────────────────────────────
  # Seasonality tables → val_tbl root (no subfolder)

  type_pfx <- .type_prefix(type, case_stratification)

  sntutils::write_snt_data(
    detailed_results, tbl_root,
    glue::glue("{iso3}_{type_pfx}_detailed_seasonality_results"), "xlsx"
  )
  sntutils::write_snt_data(
    yearly_summary, tbl_root,
    glue::glue("{iso3}_{type_pfx}_yearly_seasonality_summary"), "xlsx"
  )
  sntutils::write_snt_data(
    location_summary, tbl_root,
    glue::glue("{iso3}_{type_pfx}_location_seasonality_summary"), "xlsx"
  )

  # ── save maps ───────────────────────────────────────────────────────────────

  seas_fig_dir <- file.path(fig_root, .type_seas_subdir(type))
  .ensure_dir(seas_fig_dir)

  maps <- .build_seasonality_maps(
    dirs                  = dirs,
    iso3                  = iso3,
    adm0_name             = adm0_name,
    type                  = type,
    location_summary      = location_summary,
    available_years       = available_years,
    n_cumulative_maps     = n_cumulative_maps,
    cumulative_thresholds = cumulative_thresholds,
    fig_dir               = seas_fig_dir,
    dpi                   = dpi
  )

  invisible(list(
    detailed_results = detailed_results,
    yearly_summary   = yearly_summary,
    location_summary = location_summary,
    maps             = maps
  ))
}


#' @keywords internal
.run_block_step <- function(
    dirs, iso3, type, s_year, e_year,
    adm1_var, adm2_var, year_var, month_var, value_var,
    case_stratification,
    tbl_root
) {
  cli::cli_alert_info("Block analysis \u2192 {.val {type}}")

  df <- .load_analysis_data(
    dirs = dirs, iso3 = iso3, type = type,
    adm1_var = adm1_var, adm2_var = adm2_var,
    year_var = year_var, month_var = month_var,
    value_var = value_var
  ) |>
    .filter_years(s_year, e_year) |>
    dplyr::mutate(district = paste(adm1, adm2, sep = " - "))

  windows       <- .calculate_rolling_windows(df)
  summary_tbl   <- windows$summary
  detailed_tbl  <- windows$detailed
  frequency_tbl <- .build_block_frequency(summary_tbl, detailed_tbl)

  # ── save tables ─────────────────────────────────────────────────────────────
  # Block tables → type-specific subfolder (val_tbl/rainfall/ or val_tbl/cases_{strat}_block/)

  tbl_sub <- file.path(tbl_root, .type_tbl_subdir(type, case_stratification))
  .ensure_dir(tbl_sub)

  if (type == "rainfall") {
    sntutils::write_snt_data(
      summary_tbl,  tbl_sub,
      glue::glue("{iso3}_malaria_rainfall_block_analysis"), "xlsx"
    )
    sntutils::write_snt_data(
      detailed_tbl, tbl_sub,
      glue::glue("{iso3}_malaria_detailed_yearly_block_analysis"), "xlsx"
    )
  } else {
    sntutils::write_snt_data(
      summary_tbl,  tbl_sub,
      glue::glue("{iso3}_malaria_cases_block_analysis"), "xlsx"
    )
    sntutils::write_snt_data(
      detailed_tbl, tbl_sub,
      glue::glue("{iso3}_malaria_cases_detailed_yearly_block_analysis"), "xlsx"
    )
  }

  # Frequency file: same name for both types — subfolder differentiates them
  sntutils::write_snt_data(
    frequency_tbl, tbl_sub,
    glue::glue("{iso3}_malaria_block_frequency_analysis"), "xlsx"
  )

  cli::cli_alert_success(
    "Block tables saved \u2192 {.path {tbl_sub}}"
  )

  invisible(list(
    summary   = summary_tbl,
    detailed  = detailed_tbl,
    frequency = frequency_tbl
  ))
}


#' @keywords internal
.run_graphs_step <- function(
    dirs, iso3, adm0_name, s_year, e_year,
    rain_detailed, case_detailed,
    case_stratification,
    tbl_root, fig_root, dpi
) {
  # ── get detailed results (memory first, then disk) ───────────────────────────

  rain_df <- .get_or_read_detailed(rain_detailed, tbl_root, iso3, "rainfall",
                                   case_stratification)
  case_df <- .get_or_read_detailed(case_detailed,  tbl_root, iso3, "cases",
                                   case_stratification)

  # ── filter + reshape ─────────────────────────────────────────────────────────

  rain_df <- rain_df |>
    .filter_years(s_year, e_year, col = "StartYear") |>
    dplyr::rename(province = adm1, district = adm2,
                  Percent_Seasonality_Rainfall = Percent_Seasonality) |>
    dplyr::mutate(
      Date = lubridate::ymd(
        paste(StartYear, match(StartMonth, month.abb), "01", sep = "-")
      )
    )

  case_df <- case_df |>
    .filter_years(s_year, e_year, col = "StartYear") |>
    dplyr::rename(province = adm1, district = adm2,
                  Percent_Seasonality_Cases = Percent_Seasonality) |>
    dplyr::mutate(
      Date = lubridate::ymd(
        paste(StartYear, match(StartMonth, month.abb), "01", sep = "-")
      )
    )

  merged_df <- case_df |>
    dplyr::select(province, district, Block, Date,
                  StartMonth, StartYear, Percent_Seasonality_Cases) |>
    dplyr::left_join(
      rain_df |> dplyr::select(province, district, Block,
                               Percent_Seasonality_Rainfall),
      by = c("province", "district", "Block")
    ) |>
    tidyr::pivot_longer(
      cols      = c(Percent_Seasonality_Cases, Percent_Seasonality_Rainfall),
      names_to  = "Type",
      values_to = "Percent_Seasonality"
    ) |>
    dplyr::mutate(
      Type = ifelse(Type == "Percent_Seasonality_Cases", "Cases", "Rainfall")
    )

  # ── output → val_fig/cas_rain_graphs/ ───────────────────────────────────────

  graphs_dir <- file.path(fig_root, "cas_rain_graphs")
  .ensure_dir(graphs_dir)

  provinces <- sort(unique(merged_df$province))

  # Province-level plots
  plots <- purrr::map(purrr::set_names(provinces), function(prov) {
    prov_data  <- merged_df |> dplyr::filter(province == prov)
    fig_height <- max(8, dplyr::n_distinct(prov_data$district) * 1.5)

    p <- ggplot2::ggplot(
      prov_data,
      ggplot2::aes(x = Date, y = Percent_Seasonality, color = Type)
    ) +
      ggplot2::geom_line(linewidth = 0.8) +
      ggplot2::facet_wrap(~ district, ncol = 2, scales = "free_y") +
      ggplot2::scale_color_manual(
        values = c("Cases" = "#E74C3C", "Rainfall" = "#3498DB")
      ) +
      ggplot2::scale_x_date(
        breaks      = seq(min(prov_data$Date, na.rm = TRUE),
                          max(prov_data$Date, na.rm = TRUE),
                          by = "4 months"),
        date_labels = "%b %Y"
      ) +
      ggplot2::labs(
        title    = glue::glue("Seasonality Comparison: {prov}"),
        subtitle = "Percent Seasonality of Cases vs Rainfall by District",
        x = NULL, y = "Percent Seasonality (%)", color = "Type"
      ) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(
        plot.title       = ggplot2::element_text(size = 16, face = "bold", hjust = 0.5),
        plot.subtitle    = ggplot2::element_text(size = 11, hjust = 0.5, color = "gray30"),
        strip.text       = ggplot2::element_text(size = 10, face = "bold"),
        legend.position  = "bottom",
        panel.grid.minor = ggplot2::element_blank(),
        axis.text.x      = ggplot2::element_text(angle = 45, hjust = 1)
      )

    ggplot2::ggsave(
      filename = paste0(gsub(" ", "_", prov), "_seasonality_comparison.png"),
      plot     = p,
      path     = graphs_dir,
      width    = 14,
      height   = fig_height,
      dpi      = dpi
    )

    cli::cli_alert_success("Overlay graph saved: {.val {prov}}")
    p
  })

  # Summary plot — all provinces averaged
  summary_data <- merged_df |>
    dplyr::group_by(province, Date, Type) |>
    dplyr::summarise(
      Mean_Seasonality = mean(Percent_Seasonality, na.rm = TRUE),
      .groups = "drop"
    )

  summary_plot <- ggplot2::ggplot(
    summary_data,
    ggplot2::aes(x = Date, y = Mean_Seasonality, color = Type)
  ) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::facet_wrap(~ province, ncol = 2, scales = "free_y") +
    ggplot2::scale_color_manual(
      values = c("Cases" = "#E74C3C", "Rainfall" = "#3498DB")
    ) +
    ggplot2::scale_x_date(
      breaks      = seq(min(summary_data$Date, na.rm = TRUE),
                        max(summary_data$Date, na.rm = TRUE),
                        by = "4 months"),
      date_labels = "%b %Y"
    ) +
    ggplot2::labs(
      title    = glue::glue("{adm0_name}: Average Seasonality \u2014 All Provinces"),
      subtitle = "Mean Percent Seasonality of Cases vs Rainfall",
      x = NULL, y = "Mean Percent Seasonality (%)", color = "Type"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title       = ggplot2::element_text(size = 16, face = "bold", hjust = 0.5),
      strip.text       = ggplot2::element_text(size = 10, face = "bold"),
      legend.position  = "bottom",
      panel.grid.minor = ggplot2::element_blank(),
      axis.text.x      = ggplot2::element_text(angle = 45, hjust = 1)
    )

  ggplot2::ggsave(
    filename = glue::glue("{iso3}_ALL_PROVINCES_summary_comparison.png"),
    plot     = summary_plot,
    path     = graphs_dir,
    width    = 14, height = 12, dpi = dpi
  )

  cli::cli_alert_success("Summary overlay graph saved.")
  invisible(c(plots, list(.summary = summary_plot)))
}


#' @keywords internal
.run_smc_maps_step <- function(
    dirs, iso3, adm0_name, type,
    block_frequency, location_summary, tbl_root,
    case_stratification,
    smc_eligible_districts, smc_additional_districts, smc_remove_districts,
    fig_root, dpi
) {
  cli::cli_alert_info("SMC maps \u2192 {.val {type}}")

  freq <- .get_or_read_block_freq(block_frequency, tbl_root, iso3, type,
                                  case_stratification)

  eligible <- .derive_smc_eligibility(
    location_summary         = location_summary,
    smc_eligible_districts   = smc_eligible_districts,
    smc_additional_districts = smc_additional_districts,
    smc_remove_districts     = smc_remove_districts
  )

  spatial  <- sntutils::read_snt_data(
    path         = here::here(dirs$admin_shp, "processed"),
    data_name    = glue::glue("{iso3}_shp_list"),
    file_formats = "qs2"
  )$final_spat_vec

  adm2_sp <- spatial$adm2
  adm1_sp <- spatial$adm1

  # SMC figures → val_fig/rain_seas/ (rainfall) or val_fig/ov5/ (cases)
  smc_fig_dir <- file.path(fig_root, .type_smc_subdir(type))
  .ensure_dir(smc_fig_dir)

  month_lkp <- c(apr=4L,may=5L,jun=6L,jul=7L,aug=8L,sep=9L)

  block_data <- freq |>
    dplyr::mutate(
      adm1   = stringr::str_trim(stringr::str_extract(district, "^[^-]+")),
      adm2   = stringr::str_trim(stringr::str_extract(district, "(?<=-).*$")),
      smc_yn = dplyr::if_else(adm2 %in% eligible, 1L, 0L)
    ) |>
    dplyr::filter(smc_yn == 1L) |>
    dplyr::arrange(adm1, adm2, duration, block_freq, median_max_prop) |>
    dplyr::group_by(adm1, adm2, duration) |>
    dplyr::mutate(
      mostfreq = dplyr::if_else(
        dplyr::row_number() == dplyr::n(), 1L, NA_integer_
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::filter(mostfreq == 1L) |>
    dplyr::mutate(
      month1      = stringr::str_sub(block, 1, 3),
      firmonth    = month_lkp[month1],
      median_cats = dplyr::case_when(
        median_max_prop <  40                            ~ 1L,
        median_max_prop >= 40 & median_max_prop <  50    ~ 2L,
        median_max_prop >= 50 & median_max_prop <  60    ~ 3L,
        median_max_prop >= 60 & median_max_prop <  70    ~ 4L,
        median_max_prop >= 70 & median_max_prop <  80    ~ 5L,
        median_max_prop >= 80 & median_max_prop <= 100   ~ 6L,
        TRUE ~ NA_integer_
      )
    )

  # ── colour palettes shared across all SMC maps ───────────────────────────────

  month_pal <- c(
    "4" = "#7c4aa5ff", "5" = "#4169E1", "6" = "#32CD32",
    "7" = "#FFA500",   "8" = "#FFFF00", "9" = "#FF0000"
  )
  month_lbl <- c(
    "4" = "April", "5" = "May",    "6" = "June",
    "7" = "July",  "8" = "August", "9" = "September"
  )

  cov_pal <- c(
    "1" = "#E6E6D3", "2" = "#F4E3C1", "3" = "#87CEEB",
    "4" = "#4c73e6ff", "5" = "#1313c9ff", "6" = "#0b0b70ff"
  )

  cov_lbl <- c(
    "1" = "20-40%", "2" = "40-50%", "3" = "50-60%",
    "4" = "60-70%", "5" = "70-80%",
    "6" = "80-100%"
  )

  # Shared theme for all SMC maps
  smc_theme <- ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      plot.title       = ggplot2::element_text(face = "bold", size = 16, hjust = 0),
      plot.subtitle    = ggplot2::element_text(size = 11, hjust = 0,
                                                margin = ggplot2::margin(t = 4, b = 6)),
      legend.position  = "right",
      legend.title     = ggplot2::element_text(face = "bold", size = 11),
      legend.text      = ggplot2::element_text(size = 10),
      plot.margin      = ggplot2::margin(5, 15, 5, 5)
    )

  durations     <- sort(unique(block_data$duration))
  timing_maps   <- list()
  coverage_maps <- list()

  for (dur in durations) {
    dur_data <- block_data |> dplyr::filter(duration == dur)
    map_data <- adm2_sp |>
      dplyr::left_join(dur_data, by = "adm2") |>
      dplyr::mutate(
        firmonth_f  = factor(as.character(firmonth), levels = as.character(4:9)),
        median_cats_f = factor(as.character(median_cats), levels = as.character(1:6))
      )

    # ── Timing map ─────────────────────────────────────────────────────────────
    present_months <- as.character(sort(unique(stats::na.omit(map_data$firmonth))))

    timing_maps[[as.character(dur)]] <- ggplot2::ggplot() +
      ggplot2::geom_sf(
        data      = map_data,
        ggplot2::aes(fill = firmonth_f),
        color     = "grey30",
        linewidth = 0.25
      ) +
      ggplot2::geom_sf(
        data      = adm1_sp,
        fill      = NA,
        color     = "black",
        linewidth = 0.9
      ) +
      ggplot2::scale_fill_manual(
        values   = month_pal[present_months],
        labels   = month_lbl[present_months],
        na.value = "white",
        name     = "First Month",
        drop     = FALSE
      ) +
      ggplot2::coord_sf(datum = NA) +
      ggplot2::labs(
        title = glue::glue(
          "Ideal first month \u2014 {dur} cycles ({type})"
        )
      ) +
      smc_theme

    ggplot2::ggsave(
      file.path(smc_fig_dir, glue::glue("{type}_block_{dur}m.png")),
      timing_maps[[as.character(dur)]],
      width = 10, height = 8, dpi = dpi
    )

    # ── Coverage map ───────────────────────────────────────────────────────────
    present_cats <- as.character(sort(unique(stats::na.omit(map_data$median_cats))))

    coverage_maps[[as.character(dur)]] <- ggplot2::ggplot() +
      ggplot2::geom_sf(
        data      = map_data,
        ggplot2::aes(fill = median_cats_f),
        color     = "grey30",
        linewidth = 0.25
      ) +
      ggplot2::geom_sf(
        data      = adm1_sp,
        fill      = NA,
        color     = "black",
        linewidth = 0.9
      ) +
      ggplot2::scale_fill_manual(
        values   = cov_pal[present_cats],
        labels   = cov_lbl[present_cats],
        na.value = "white",
        name     = "% Covered",
        drop     = FALSE
      ) +
      ggplot2::coord_sf(datum = NA) +
      ggplot2::labs(
        title = glue::glue(
          "% of {type} covered \u2014 {dur}-month window"
        )
      ) +
      smc_theme

    ggplot2::ggsave(
      file.path(smc_fig_dir, glue::glue("{type}_prop_{dur}m.png")),
      coverage_maps[[as.character(dur)]],
      width = 10, height = 8, dpi = dpi
    )

    cli::cli_alert_success(
      "SMC maps saved: {dur}-month window \u2192 {.path {smc_fig_dir}}"
    )
  }

  # ── MINIMUM BEST BLOCK ───────────────────────────────────────────────────────
  # For each district, find the shortest window that covers >= 60% of the
  # annual metric. If no window reaches 60%, use the best available (rule 1).
  #
  # Rule 1 (max < 60):  flag the single block with the highest median_max_prop
  # Rule 2 (max >= 60): flag the block with the smallest duration that still
  #                     reaches >= 60%, breaking ties by lowest median_max_prop

  cli::cli_alert_info("Computing minimum best blocks...")

  min_summary <- block_data |>
    dplyr::group_by(adm1, adm2) |>
    dplyr::mutate(
      max_prop = max(median_max_prop, na.rm = TRUE),
      rule = dplyr::case_when(
        max_prop <  60 ~ 1L,
        max_prop >= 60 ~ 2L,
        TRUE           ~ NA_integer_
      )
    ) |>
    dplyr::ungroup()

  min_summary <- min_summary |>
    dplyr::group_by(adm1, adm2) |>
    dplyr::mutate(
      # Rule 1: best available (highest prop) when nothing hits 60%
      minimum_best = dplyr::case_when(
        rule == 1L & median_max_prop == max_prop ~ 1L,
        TRUE ~ NA_integer_
      ),
      # Rule 2 helper: only values >= 60% are eligible
      freq60 = dplyr::if_else(
        rule == 2L & median_max_prop >= 60,
        median_max_prop, NA_real_
      )
    ) |>
    dplyr::mutate(
      # min_freq60: the lowest eligible proportion in this group (same value for
      # all rows — this is how the original script's sort works: arrange by this
      # scalar then freq60 effectively sorts purely by freq60 ascending, picking
      # the block that JUST reaches 60% — the most conservative choice)
      min_freq60 = min(freq60, na.rm = TRUE),
      min_freq60 = dplyr::if_else(is.infinite(min_freq60), NA_real_, min_freq60),
      min_freq60 = dplyr::if_else(median_max_prop < 60, NA_real_, min_freq60)
    ) |>
    # Arrange exactly as original: by min_freq60 (scalar group minimum),
    # then freq60 ascending — picks the row whose proportion just reaches 60%
    dplyr::arrange(adm1, adm2, min_freq60, freq60) |>
    dplyr::mutate(
      minimum_best = dplyr::if_else(
        is.na(minimum_best) &
          dplyr::row_number() == 1L &
          rule == 2L &
          !is.na(min_freq60),
        1L,
        minimum_best
      )
    ) |>
    dplyr::ungroup() |>
    # Recompute median_cats with type-correct upper bound for category 6
    dplyr::mutate(
      median_cats = dplyr::case_when(
        median_max_prop <  40                          ~ 1L,
        median_max_prop >= 40 & median_max_prop <  50  ~ 2L,
        median_max_prop >= 50 & median_max_prop <  60  ~ 3L,
        median_max_prop >= 60 & median_max_prop <  70  ~ 4L,
        median_max_prop >= 70 & median_max_prop <  80  ~ 5L,
        median_max_prop >= 80 & median_max_prop <= 100 ~ 6L,
        TRUE ~ NA_integer_
      )
    )

  # ── save minimum best block table ────────────────────────────────────────────

  tbl_sub <- file.path(tbl_root, .type_tbl_subdir(type, case_stratification))
  .ensure_dir(tbl_sub)

  export_min <- min_summary |>
    dplyr::select(
      adm1, adm2, duration, block,
      block_freq, years, median_max_prop, minimum_best
    )

  sntutils::write_snt_data(
    export_min,
    tbl_sub,
    glue::glue("{iso3}_{type}_data_minimum_best_block"),
    "xlsx"
  )

  cli::cli_alert_success(
    "Minimum best block table saved \u2192 {.path {tbl_sub}}"
  )

  # ── build minimum maps ───────────────────────────────────────────────────────
  # Three maps: (1) duration, (2) first month, (3) % coverage
  # Using only the minimum_best == 1 records

  min_final <- min_summary |>
    dplyr::filter(minimum_best == 1L)

  min_spatial <- adm2_sp |>
    dplyr::left_join(min_final, by = "adm2") |>
    dplyr::mutate(
      firmonth_f    = factor(as.character(firmonth),    levels = as.character(4:9)),
      median_cats_f = factor(as.character(median_cats), levels = as.character(1:6))
    )

  # Set duration as factor using only levels present in the data
  dur_levels <- sort(unique(stats::na.omit(min_final$duration)))
  min_spatial$duration <- factor(min_spatial$duration, levels = dur_levels)

  # Colour palette for duration — up to 3 levels (3m, 4m, 5m)
  dur_colours <- c(
    "3" = "#d8c974ff",
    "4" = "#e25ae2ff",
    "5" = "#6e1e6eff"
  )
  dur_labels <- c(
    "3" = "3 months (4 cycles)",
    "4" = "4 months (5 cycles)",
    "5" = "5 months (6 cycles)"
  )
  dur_colours <- dur_colours[as.character(dur_levels)]
  dur_labels  <- dur_labels[as.character(dur_levels)]

  type_label_min <- if (type == "rainfall") "Rainfall" else "Cases"

  # Map 1: Minimum duration required
  map_min_duration <- ggplot2::ggplot() +
    ggplot2::geom_sf(
      data      = min_spatial,
      ggplot2::aes(fill = duration),
      color     = "grey30",
      linewidth = 0.25
    ) +
    ggplot2::geom_sf(
      data      = adm1_sp,
      fill      = NA,
      color     = "black",
      linewidth = 0.9
    ) +
    ggplot2::scale_fill_manual(
      values   = dur_colours,
      labels   = dur_labels,
      na.value = "white",
      name     = "Duration",
      drop     = FALSE
    ) +
    ggplot2::coord_sf(datum = NA) +
    ggplot2::labs(
      title    = glue::glue(
        "Minimum months required to \ncover ~60% of {type_label_min}"
      )
    ) +
    smc_theme

  ggplot2::ggsave(
    file.path(smc_fig_dir, glue::glue("{type}_cycles_minimum.png")),
    map_min_duration, width = 10, height = 8, dpi = dpi
  )

  # Map 2: First month of minimum best block
  present_min_months <- as.character(
    sort(unique(stats::na.omit(min_final$firmonth)))
  )

  map_min_firstmonth <- ggplot2::ggplot() +
    ggplot2::geom_sf(
      data      = min_spatial,
      ggplot2::aes(fill = firmonth_f),
      color     = "grey30",
      linewidth = 0.25
    ) +
    ggplot2::geom_sf(
      data      = adm1_sp,
      fill      = NA,
      color     = "black",
      linewidth = 0.9
    ) +
    ggplot2::scale_fill_manual(
      values   = month_pal[present_min_months],
      labels   = month_lbl[present_min_months],
      na.value = "white",
      name     = "First Month",
      drop     = FALSE
    ) +
    ggplot2::coord_sf(datum = NA) +
    ggplot2::labs(
      title    = "First month for the minimum number of cycles",
      subtitle = glue::glue("Based on {tolower(type_label_min)}")
    ) +
    smc_theme

  ggplot2::ggsave(
    file.path(smc_fig_dir, glue::glue("{type}_block_minimum.png")),
    map_min_firstmonth, width = 10, height = 8, dpi = dpi
  )

  # Map 3: % of metric covered by the minimum best block
  present_min_cats <- as.character(
    sort(unique(stats::na.omit(min_final$median_cats)))
  )

  map_min_coverage <- ggplot2::ggplot() +
    ggplot2::geom_sf(
      data      = min_spatial,
      ggplot2::aes(fill = median_cats_f),
      color     = "grey30",
      linewidth = 0.25
    ) +
    ggplot2::geom_sf(
      data      = adm1_sp,
      fill      = NA,
      color     = "black",
      linewidth = 0.9
    ) +
    ggplot2::scale_fill_manual(
      values   = cov_pal[present_min_cats],
      labels   = cov_lbl[present_min_cats],
      na.value = "white",
      name     = "% Covered",
      drop     = FALSE
    ) +
    ggplot2::coord_sf(datum = NA) +
    ggplot2::labs(
      title    = glue::glue("% of {type_label_min} Covered (minimum block)"),
      subtitle = "Median proportion captured by the minimum best block"
    ) +
    smc_theme

  ggplot2::ggsave(
    file.path(smc_fig_dir, glue::glue("{type}_prop_minimum.png")),
    map_min_coverage, width = 10, height = 8, dpi = dpi
  )

  cli::cli_alert_success(
    "Minimum best block maps saved \u2192 {.path {smc_fig_dir}}"
  )

  invisible(list(
    timing_maps      = timing_maps,
    coverage_maps    = coverage_maps,
    eligible         = eligible,
    min_best_table   = export_min,
    min_best_maps    = list(
      duration   = map_min_duration,
      firstmonth = map_min_firstmonth,
      coverage   = map_min_coverage
    )
  ))
}


#' @keywords internal
.run_case_rain_plots_step <- function(
    dirs, iso3, adm0_name,
    adm1_var, adm2_var, year_var, month_var,
    case_var_ov5, case_var_u5, rainfall_total_var,
    case_rain_s_date, case_rain_e_date,
    s_year, e_year,
    panels_per_row,
    fig_root, dpi
) {
  # ── output dirs ─────────────────────────────────────────────────────────────
  # Median → val_fig/cas_v_pluie/
  # Crude  → val_fig/cas_v_pluie_crude/

  median_dir <- file.path(fig_root, "cas_v_pluie")
  crude_dir  <- file.path(fig_root, "cas_v_pluie_crude")
  .ensure_dir(median_dir)
  .ensure_dir(crude_dir)

  # ── resolve date filter ──────────────────────────────────────────────────────
  # Prefer explicit case_rain_s/e_date; fall back to s_year/e_year; else NA.
  # Use plain if/else — dplyr::case_when() is for vectors inside mutate(),
  # not for scalar conditionals, and returns length-0 when all inputs are NULL.

  s_date_resolved <- if (!is.null(case_rain_s_date)) {
    as.Date(case_rain_s_date)
  } else if (!is.null(s_year)) {
    as.Date(sprintf("%04d-01-01", s_year))
  } else {
    NA_real_
  }

  e_date_resolved <- if (!is.null(case_rain_e_date)) {
    as.Date(case_rain_e_date)
  } else if (!is.null(e_year)) {
    as.Date(sprintf("%04d-12-31", e_year))
  } else {
    NA_real_
  }

  # ── load raw data ────────────────────────────────────────────────────────────

  case_raw <- sntutils::read_snt_data(
    dirs$dhis2,
    glue::glue("{iso3}_dhis2_processed"),
    "xlsx"
  )

  required_case <- c(adm1_var, adm2_var, year_var, month_var,
                     case_var_ov5, case_var_u5)
  missing_case  <- setdiff(required_case, names(case_raw))
  if (length(missing_case) > 0) {
    cli::cli_abort(c(
      "Missing column{?s} in DHIS2 data:",
      "x" = "{.var {missing_case}}",
      "i" = "Use {.arg case_var_ov5}, {.arg case_var_u5} to map your column names."
    ))
  }

  rain_raw <- sntutils::read_snt_data(
    dirs$climate,
    glue::glue("{iso3}_rainfall_processed"),
    "xlsx"
  )

  required_rain <- c(adm1_var, adm2_var, year_var, month_var, rainfall_total_var)
  missing_rain  <- setdiff(required_rain, names(rain_raw))
  if (length(missing_rain) > 0) {
    cli::cli_abort(c(
      "Missing column{?s} in rainfall data:",
      "x" = "{.var {missing_rain}}",
      "i" = "Use {.arg rainfall_total_var} to map your column name."
    ))
  }

  # ── aggregate case data ──────────────────────────────────────────────────────

  case_df <- case_raw |>
    dplyr::filter(
      dplyr::if_all(dplyr::all_of(c(case_var_ov5, case_var_u5)), ~ !is.na(.))
    ) |>
    dplyr::select(dplyr::all_of(required_case)) |>
    dplyr::rename(
      adm1  = !!adm1_var,
      adm2  = !!adm2_var,
      year  = !!year_var,
      month = !!month_var,
      conf_ov5 = !!case_var_ov5,
      conf_u5  = !!case_var_u5
    ) |>
    dplyr::group_by(adm1, adm2, year, month) |>
    dplyr::summarise(
      conf_ov5 = sum(conf_ov5, na.rm = TRUE),
      conf_u5  = sum(conf_u5,  na.rm = TRUE),
      .groups  = "drop"
    ) |>
    dplyr::mutate(date = as.Date(sprintf("%04d-%02d-01", year, month)))

  # ── aggregate rainfall data ──────────────────────────────────────────────────

  rain_df <- rain_raw |>
    dplyr::select(dplyr::all_of(required_rain)) |>
    dplyr::rename(
      adm1  = !!adm1_var,
      adm2  = !!adm2_var,
      year  = !!year_var,
      month = !!month_var,
      total_rainfall_mm = !!rainfall_total_var
    ) |>
    dplyr::group_by(adm1, adm2, year, month) |>
    dplyr::summarise(
      total_rainfall_mm = sum(total_rainfall_mm, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(date = as.Date(sprintf("%04d-%02d-01", year, month)))

  # Apply date filter to rainfall
  if (!is.na(s_date_resolved)) rain_df <- rain_df |>
      dplyr::filter(date >= s_date_resolved)
  if (!is.na(e_date_resolved)) rain_df <- rain_df |>
      dplyr::filter(date <= e_date_resolved)

  # ── MEDIAN PLOT DATA ─────────────────────────────────────────────────────────
  # Join → filter to period → group by month (1-12) → take median across years

  combined_raw <- rain_df |>
    dplyr::left_join(
      case_df |> dplyr::select(adm1, adm2, year, month, conf_ov5, conf_u5),
      by = c("adm1", "adm2", "year", "month")
    )

  df_median <- combined_raw |>
    dplyr::group_by(adm1, adm2, month) |>
    dplyr::summarise(
      conf_ov5          = stats::median(conf_ov5,          na.rm = TRUE),
      conf_u5           = stats::median(conf_u5,           na.rm = TRUE),
      total_rainfall_mm = stats::median(total_rainfall_mm, na.rm = TRUE),
      .groups = "drop"
    )

  # ── CRUDE PLOT DATA ──────────────────────────────────────────────────────────
  # Full join on actual dates — rainfall drives date range, cases overlay

  case_date_filtered <- case_df
  if (!is.na(s_date_resolved))
    case_date_filtered <- case_date_filtered |> dplyr::filter(date >= s_date_resolved)
  if (!is.na(e_date_resolved))
    case_date_filtered <- case_date_filtered |> dplyr::filter(date <= e_date_resolved)

  combined_crude <- dplyr::full_join(
    rain_df         |> dplyr::select(adm1, adm2, date, total_rainfall_mm),
    case_date_filtered |> dplyr::select(adm1, adm2, date, conf_ov5, conf_u5),
    by = c("adm1", "adm2", "date")
  ) |>
    dplyr::filter(!is.na(date))

  # ── GENERATE PLOTS ───────────────────────────────────────────────────────────

  adm1_regions <- sort(unique(df_median$adm1))

  median_plots <- purrr::map(purrr::set_names(adm1_regions), function(region) {
    p      <- .build_median_adm1_plot(df_median, region, panels_per_row)
    n_adm2 <- dplyr::n_distinct(df_median$adm2[df_median$adm1 == region])
    n_rows <- ceiling(n_adm2 / panels_per_row)

    ggplot2::ggsave(
      filename = glue::glue("Cases_v_rainfall_median_{region}.png"),
      plot     = p,
      path     = median_dir,
      width    = max(12, n_adm2 * 5),
      height   = 3.5 * n_rows + 1.5,
      dpi      = dpi,
      bg       = "white"
    )

    cli::cli_alert_success("Median plot saved: {.val {region}}")
    p
  })

  adm1_crude <- sort(unique(combined_crude$adm1))

  crude_plots <- purrr::map(purrr::set_names(adm1_crude), function(region) {
    p      <- .build_crude_adm1_plot(combined_crude, region, panels_per_row)
    n_adm2 <- dplyr::n_distinct(combined_crude$adm2[combined_crude$adm1 == region])
    n_rows <- ceiling(n_adm2 / panels_per_row)

    ggplot2::ggsave(
      filename = glue::glue("Cases_v_rainfall_crude_{region}.png"),
      plot     = p,
      path     = crude_dir,
      width    = 15,
      height   = 3.5 * n_rows + 1.5,
      dpi      = dpi,
      bg       = "white"
    )

    cli::cli_alert_success("Crude plot saved: {.val {region}}")
    p
  })

  invisible(list(median = median_plots, crude = crude_plots))
}


# ==============================================================================
# INTERNAL HELPERS — case vs rainfall plot builders
# ==============================================================================

#' Single district panel — median version (x = month 1–12)
#' @keywords internal
.build_median_adm2_plot <- function(data, adm2_name,
                                    show_y_left = TRUE, show_y_right = TRUE) {
  d             <- data |> dplyr::filter(adm2 == adm2_name)
  max_cases     <- max(c(d$conf_ov5, d$conf_u5), na.rm = TRUE)
  max_rain      <- max(d$total_rainfall_mm, na.rm = TRUE)
  scale_factor  <- if (max_rain > 0) max_cases / max_rain else 1

  ggplot2::ggplot(d, ggplot2::aes(x = month)) +
    ggplot2::geom_line(
      ggplot2::aes(y = total_rainfall_mm * scale_factor, color = "Pluie"),
      linewidth = 1.2
    ) +
    ggplot2::geom_line(
      ggplot2::aes(y = conf_ov5, color = "Cas >5 ans"),
      linewidth = 1.2
    ) +
    ggplot2::geom_line(
      ggplot2::aes(y = conf_u5, color = "Cas <5 ans"),
      linewidth = 1.2
    ) +
    ggplot2::scale_y_continuous(
      name   = if (show_y_left) "Cas confirm\u00e9s (m\u00e9diane)" else NULL,
      labels = scales::comma,
      sec.axis = ggplot2::sec_axis(
        transform = ~ . / scale_factor,
        name      = if (show_y_right) "Pluie (mm)" else NULL,
        labels    = scales::comma
      )
    ) +
    ggplot2::scale_x_continuous(breaks = 1:12, labels = 1:12) +
    ggplot2::scale_color_manual(
      name   = "",
      values = c("Cas >5 ans" = "#E74C3C", "Cas <5 ans" = "#F4D03F",
                 "Pluie" = "#3498DB")
    ) +
    ggplot2::ggtitle(adm2_name) +
    ggplot2::xlab("") +
    ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(
      plot.title         = ggplot2::element_text(hjust = 0.5, face = "bold", size = 13),
      legend.position    = "none",
      panel.grid.minor   = ggplot2::element_blank(),
      panel.border       = ggplot2::element_rect(color = "gray80", fill = NA,
                                                  linewidth = 0.5),
      axis.title.y.left  = if (show_y_left)  ggplot2::element_text(color = "black", size = 12)
                           else               ggplot2::element_blank(),
      axis.title.y.right = if (show_y_right) ggplot2::element_text(color = "black", size = 12)
                           else               ggplot2::element_blank(),
      axis.text          = ggplot2::element_text(size = 10, face = "bold")
    )
}


#' Assemble all district panels for one adm1 region — median version
#' @keywords internal
.build_median_adm1_plot <- function(data, adm1_name, panels_per_row = 3) {
  adm1_data    <- data |> dplyr::filter(adm1 == adm1_name)
  adm2_regions <- sort(unique(adm1_data$adm2))
  n            <- length(adm2_regions)

  panel_list <- purrr::imap(
    adm2_regions,
    ~ .build_median_adm2_plot(
      data         = adm1_data,
      adm2_name    = .x,
      show_y_left  = (.y == 1),
      show_y_right = (.y == n)
    )
  )

  legend_plot   <- .build_median_adm2_plot(adm1_data, adm2_regions[1]) +
    ggplot2::theme(legend.position = "bottom")
  shared_legend <- cowplot::get_legend(legend_plot)

  combined <- patchwork::wrap_plots(panel_list, ncol = panels_per_row) +
    patchwork::plot_annotation(
      title = adm1_name,
      theme = ggplot2::theme(
        plot.title = ggplot2::element_text(hjust = 0.5, face = "bold", size = 14)
      )
    )

  cowplot::plot_grid(combined, shared_legend, ncol = 1, rel_heights = c(1, 0.08))
}


#' Single district panel — crude version (x = real dates)
#' @keywords internal
.build_crude_adm2_plot <- function(data, adm2_name,
                                   show_y_left = TRUE, show_y_right = TRUE) {
  d            <- data |> dplyr::filter(adm2 == adm2_name)
  max_cases    <- max(c(d$conf_ov5, d$conf_u5), na.rm = TRUE)
  max_rain     <- max(d$total_rainfall_mm, na.rm = TRUE)
  scale_factor <- if (max_rain > 0) max_cases / max_rain else 1

  ggplot2::ggplot(d, ggplot2::aes(x = date)) +
    ggplot2::geom_line(
      ggplot2::aes(y = total_rainfall_mm * scale_factor, color = "Pluie"),
      linewidth = 1.0, na.rm = TRUE
    ) +
    ggplot2::geom_line(
      ggplot2::aes(y = conf_ov5, color = "Cas >5 ans"),
      linewidth = 1.0, na.rm = TRUE
    ) +
    ggplot2::geom_line(
      ggplot2::aes(y = conf_u5, color = "Cas <5 ans"),
      linewidth = 1.0, na.rm = TRUE
    ) +
    ggplot2::scale_y_continuous(
      name   = if (show_y_left) "Cas confirm\u00e9s" else NULL,
      labels = scales::comma,
      sec.axis = ggplot2::sec_axis(
        transform = ~ . / scale_factor,
        name      = if (show_y_right) "Pluie (mm)" else NULL,
        labels    = scales::comma
      )
    ) +
    ggplot2::scale_x_date(
      date_breaks = "6 months",
      date_labels = "%b %Y",
      expand      = ggplot2::expansion(mult = c(0, 0.01))
    ) +
    ggplot2::scale_color_manual(
      name   = "",
      values = c("Cas >5 ans" = "#E74C3C", "Cas <5 ans" = "#F4D03F",
                 "Pluie" = "#3498DB")
    ) +
    ggplot2::ggtitle(adm2_name) +
    ggplot2::xlab(NULL) +
    ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(
      plot.title         = ggplot2::element_text(hjust = 0.5, face = "bold", size = 11),
      legend.position    = "none",
      panel.grid.minor   = ggplot2::element_blank(),
      panel.border       = ggplot2::element_rect(color = "gray80", fill = NA,
                                                  linewidth = 0.5),
      axis.text.x        = ggplot2::element_text(angle = 45, hjust = 1, size = 10),
      axis.text.y        = ggplot2::element_text(size = 10),
      axis.title.y.left  = if (show_y_left)  ggplot2::element_text(color = "black", size = 13)
                           else               ggplot2::element_blank(),
      axis.title.y.right = if (show_y_right) ggplot2::element_text(color = "black", size = 13)
                           else               ggplot2::element_blank()
    )
}


#' Assemble all district panels for one adm1 region — crude version
#' @keywords internal
.build_crude_adm1_plot <- function(data, adm1_name, panels_per_row = 3) {
  adm1_data    <- data |> dplyr::filter(adm1 == adm1_name)
  adm2_regions <- sort(unique(adm1_data$adm2))
  n            <- length(adm2_regions)

  panel_list <- purrr::imap(
    adm2_regions,
    ~ .build_crude_adm2_plot(
      data         = adm1_data,
      adm2_name    = .x,
      show_y_left  = (.y %% panels_per_row == 1),
      show_y_right = (.y %% panels_per_row == 0 || .y == n)
    )
  )

  legend_plot   <- .build_crude_adm2_plot(adm1_data, adm2_regions[1]) +
    ggplot2::theme(legend.position = "bottom",
                   legend.text = ggplot2::element_text(size = 12, face = "bold"))
  shared_legend <- cowplot::get_legend(legend_plot)

  combined <- patchwork::wrap_plots(panel_list, ncol = panels_per_row) +
    patchwork::plot_annotation(
      title = toupper(adm1_name),
      theme = ggplot2::theme(
        plot.title = ggplot2::element_text(hjust = 0.5, face = "bold", size = 14)
      )
    )

  cowplot::plot_grid(combined, shared_legend, ncol = 1, rel_heights = c(1, 0.06))
}


# ==============================================================================
# INTERNAL HELPERS — path resolution and utilities
# ==============================================================================

#' Resolve all data and output directories
#' @keywords internal
.resolve_dirs <- function(paths, base_path, climate_dir, dhis2_dir, admin_shp_dir) {

  if (!is.null(paths)) {
    return(list(
      climate             = here::here(paths$climate, "processed"),
      dhis2               = here::here(paths$dhis2,   "processed"),
      admin_shp           = paths$admin_shp,
      val_fig             = paths$val_fig,
      val_tbl             = paths$val_tbl,
      use_sntutils_output = TRUE
    ))
  }

  if (!is.null(base_path)) {
    if (!dir.exists(base_path)) {
      cli::cli_abort(c(
        "{.arg base_path} does not exist.",
        "x" = "{.path {base_path}}"
      ))
    }
    p <- sntutils::setup_project_paths(base_path = base_path, quiet = TRUE)
    return(list(
      climate             = here::here(p$climate, "processed"),
      dhis2               = here::here(p$dhis2,   "processed"),
      admin_shp           = p$admin_shp,
      val_fig             = p$val_fig,
      val_tbl             = p$val_tbl,
      use_sntutils_output = TRUE
    ))
  }

  if (!is.null(climate_dir) && !is.null(dhis2_dir)) {
    missing_dirs <- c(
      if (!dir.exists(climate_dir)) climate_dir,
      if (!dir.exists(dhis2_dir))   dhis2_dir
    )
    if (length(missing_dirs) > 0) {
      cli::cli_abort(c(
        "Director{?y/ies} not found:",
        "x" = "{.path {missing_dirs}}"
      ))
    }
    return(list(
      climate             = climate_dir,
      dhis2               = dhis2_dir,
      admin_shp           = admin_shp_dir,
      val_fig             = NULL,
      val_tbl             = NULL,
      use_sntutils_output = FALSE
    ))
  }

  cli::cli_abort(c(
    "Cannot resolve data paths.",
    "i" = "Provide {.arg paths}, {.arg base_path}, or both {.arg climate_dir} + {.arg dhis2_dir}."
  ))
}


#' Create a directory only if it does not already exist
#' @keywords internal
.ensure_dir <- function(path) {
  if (!is.null(path) && !dir.exists(path)) {
    dir.create(path, recursive = TRUE)
    cli::cli_alert_info("Created subfolder: {.path {path}}")
  }
  invisible(path)
}


#' Consistent naming helpers
#' @keywords internal
.type_prefix      <- function(type, strat = "ov5") {
  switch(type, rainfall = "rainfall", cases = paste0("case_", strat))
}
.type_seas_subdir <- function(type) switch(type, rainfall = "rain_seas",  cases = "case_seas")
.type_smc_subdir  <- function(type) switch(type, rainfall = "rain_seas",  cases = "ov5")
.type_tbl_subdir  <- function(type, strat = "ov5") {
  switch(type, rainfall = "rainfall", cases = paste0("cases_", strat, "_block"))
}


#' Load and aggregate raw source data to adm/year/month level
#' @keywords internal
.load_analysis_data <- function(
    dirs, iso3, type, adm1_var, adm2_var, year_var, month_var, value_var
) {
  cfg <- switch(type,
    rainfall = list(
      folder   = dirs$climate,
      filename = glue::glue("{iso3}_rainfall_processed"),
      col_src  = if (!is.null(value_var)) value_var else "mean_rainfall_mm"
    ),
    cases = list(
      folder   = dirs$dhis2,
      filename = glue::glue("{iso3}_dhis2_processed"),
      col_src  = if (!is.null(value_var)) value_var else "conf"
    )
  )

  df <- sntutils::read_snt_data(cfg$folder, cfg$filename, "xlsx")

  required <- c(adm1_var, adm2_var, year_var, month_var, cfg$col_src)
  missing  <- setdiff(required, names(df))
  if (length(missing) > 0) {
    cli::cli_abort(c(
      "Missing column{?s} in {.val {cfg$filename}}:",
      "x" = "{.var {missing}}",
      "i" = "Use {.arg *_var} arguments to map your column names."
    ))
  }

  if (!is.numeric(df[[cfg$col_src]])) {
    cli::cli_abort(c(
      "Column {.var {cfg$col_src}} must be numeric.",
      "x" = "Found: {.cls {class(df[[cfg$col_src]])}}."
    ))
  }

  n_neg <- sum(df[[cfg$col_src]] < 0, na.rm = TRUE)
  if (n_neg > 0) {
    cli::cli_abort(c(
      "{n_neg} negative value{?s} in {.var {cfg$col_src}}.",
      "x" = "Metric values must be \u2265 0."
    ))
  }

  df |>
    dplyr::select(dplyr::all_of(required)) |>
    dplyr::rename(
      adm1  = !!adm1_var,
      adm2  = !!adm2_var,
      year  = !!year_var,
      month = !!month_var
    ) |>
    dplyr::filter(
      dplyr::if_all(dplyr::all_of(c("adm1","adm2","year","month")), ~ !is.na(.x))
    ) |>
    dplyr::group_by(adm1, adm2, year, month) |>
    dplyr::summarise(
      value = sum(.data[[cfg$col_src]], na.rm = TRUE),
      .groups = "drop"
    )
}


#' Filter data to a year or date range
#' @keywords internal
.filter_years <- function(df, s_year, e_year, col = "year") {
  if (!is.null(s_year)) df <- df |> dplyr::filter(.data[[col]] >= s_year)
  if (!is.null(e_year)) df <- df |> dplyr::filter(.data[[col]] <= e_year)
  df
}


#' Paste admin columns into a single admin_group string
#' @keywords internal
.build_admin_group <- function(df, admin_cols) {
  df |>
    dplyr::mutate(
      admin_group = paste(!!!rlang::syms(admin_cols), sep = " | ")
    )
}


#' Read detailed seasonality results from memory or disk
#' @keywords internal
.get_or_read_detailed <- function(in_memory, tbl_root, iso3, type,
                                  strat = "ov5") {
  if (!is.null(in_memory)) return(in_memory)

  type_pfx  <- .type_prefix(type, strat)
  file_name <- glue::glue("{iso3}_{type_pfx}_detailed_seasonality_results")
  cli::cli_alert_info(
    "Reading {.val {type}} detailed results from disk ({.val {file_name}})..."
  )
  result <- tryCatch(
    sntutils::read_snt_data(tbl_root, file_name, "xlsx"),
    error = function(e) NULL
  )
  if (is.null(result)) {
    cli::cli_abort(c(
      "Cannot find {.val {type}} detailed seasonality results.",
      "i" = "Expected: {.path {file.path(tbl_root, paste0(file_name, '.xlsx'))}}",
      "i" = "Add {.val 'seasonality'} to {.arg steps}, or run it before {.val 'graphs'}."
    ))
  }
  result
}


#' Read block frequency from memory or disk
#' @keywords internal
.get_or_read_block_freq <- function(in_memory, tbl_root, iso3, type,
                                    strat = "ov5") {
  if (!is.null(in_memory)) return(in_memory)

  tbl_sub   <- file.path(tbl_root, .type_tbl_subdir(type, strat))
  file_name <- glue::glue("{iso3}_malaria_block_frequency_analysis")
  cli::cli_alert_info(
    "Reading {.val {type}} block frequency from disk ({.val {file_name}})..."
  )
  result <- tryCatch(
    sntutils::read_snt_data(tbl_sub, file_name, "xlsx"),
    error = function(e) NULL
  )
  if (is.null(result)) {
    cli::cli_abort(c(
      "Cannot find {.val {type}} block frequency results.",
      "i" = "Expected: {.path {file.path(tbl_sub, paste0(file_name, '.xlsx'))}}",
      "i" = "Add {.val 'blocks'} to {.arg steps}, or run it before {.val 'smc_maps'}."
    ))
  }
  result
}


# ==============================================================================
# INTERNAL HELPERS — seasonality analysis
# ==============================================================================

#' Generate rolling 4-month + 12-month block definitions
#' @keywords internal
.generate_rolling_blocks <- function(df, analysis_start_month) {
  last_year  <- max(df$year)
  last_month <- max(df$month[df$year == last_year])
  start_year <- min(df$year)
  mnames     <- c("Jan","Feb","Mar","Apr","May","Jun",
                  "Jul","Aug","Sep","Oct","Nov","Dec")
  total_months <- nrow(dplyr::distinct(df[, c("year","month")]))

  blocks    <- data.frame()
  cur_year  <- start_year
  cur_month <- analysis_start_month

  for (i in seq_len(total_months)) {
    e4m <- cur_month + 3;  e4y <- cur_year
    if (e4m > 12) { e4y <- e4y + 1; e4m <- e4m - 12 }

    e12m <- cur_month + 11; e12y <- cur_year
    if (e12m > 12) { e12y <- e12y + 1; e12m <- e12m - 12 }

    if ((e12y * 12 + e12m) > (last_year * 12 + last_month)) break

    blocks <- rbind(blocks, data.frame(
      block_number    = i,
      start_4m_year   = cur_year,  start_4m_month  = cur_month,
      end_4m_year     = e4y,       end_4m_month    = e4m,
      start_12m_year  = cur_year,  start_12m_month = cur_month,
      end_12m_year    = e12y,      end_12m_month   = e12m,
      date_range      = paste0(
        mnames[cur_month], " ", cur_year, " ",
        mnames[e4m],       " ", e4y
      ),
      stringsAsFactors = FALSE
    ))

    cur_month <- cur_month + 1
    if (cur_month > 12) { cur_month <- 1; cur_year <- cur_year + 1 }
  }

  cli::cli_alert_success("Generated {nrow(blocks)} rolling block{?s}.")
  blocks
}


#' Calculate seasonality % per admin unit per block
#' @keywords internal
.calculate_seasonality <- function(df, blocks, seasonality_threshold) {
  admin_groups <- unique(df$admin_group)
  detailed     <- data.frame()

  for (admin_unit in admin_groups) {
    unit    <- df |> dplyr::filter(admin_group == admin_unit)
    unit_ym <- unit$year * 12 + unit$month

    for (i in seq_len(nrow(blocks))) {
      b   <- blocks[i, ]
      s4  <- b$start_4m_year  * 12 + b$start_4m_month
      e4  <- b$end_4m_year    * 12 + b$end_4m_month
      s12 <- b$start_12m_year * 12 + b$start_12m_month
      e12 <- b$end_12m_year   * 12 + b$end_12m_month

      t4  <- sum(unit$value[unit_ym >= s4  & unit_ym <= e4],  na.rm = TRUE)
      t12 <- sum(unit$value[unit_ym >= s12 & unit_ym <= e12], na.rm = TRUE)
      pct <- ifelse(t12 > 0, (t4 / t12) * 100, 0)

      row        <- data.frame(
        Block               = i,
        DateRange           = b$date_range,
        StartMonth          = month.abb[b$start_4m_month],
        StartYear           = b$start_4m_year,
        Total_4M            = t4,
        Total_12M           = t12,
        Percent_Seasonality = round(pct, 2),
        Seasonal            = as.integer(pct >= seasonality_threshold),
        stringsAsFactors    = FALSE
      )
      parts      <- strsplit(admin_unit, " \\| ")[[1]]
      row$adm1   <- parts[1]
      row$adm2   <- if (length(parts) >= 2) parts[2] else NA_character_
      detailed   <- rbind(detailed, row)
    }
  }
  detailed
}


#' Yearly summary from detailed block results
#' @keywords internal
.build_yearly_summary <- function(detailed_results) {
  detailed_results$StartYear <- as.integer(
    sapply(detailed_results$DateRange, function(x) strsplit(x, " ")[[1]][2])
  )

  detailed_results |>
    dplyr::group_by(adm1, adm2, StartYear) |>
    dplyr::summarise(
      Year                        = dplyr::first(StartYear),
      SeasonalCount               = sum(Seasonal, na.rm = TRUE),
      total_blocks_in_year        = 12L,
      at_least_one_seasonal_block = as.integer(SeasonalCount > 0),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      year_period = paste0(
        "(Jan ", Year, "\u2013Apr ", Year,
        ", Dec ", Year, "\u2013Mar ", Year + 1, ")"
      )
    ) |>
    dplyr::arrange(Year, adm1, adm2)
}


#' Classify each location as Seasonal / Not Seasonal
#' @keywords internal
.build_location_summary <- function(yearly_summary, max_non_seasonal_years) {
  yearly_summary |>
    dplyr::group_by(adm1, adm2) |>
    dplyr::summarise(
      SeasonalYears    = sum(at_least_one_seasonal_block, na.rm = TRUE),
      TotalYears       = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      NonSeasonalYears = TotalYears - SeasonalYears,
      Seasonality      = ifelse(
        NonSeasonalYears <= max_non_seasonal_years,
        "Seasonal", "Not Seasonal"
      )
    ) |>
    dplyr::arrange(adm1, adm2)
}


#' Build all seasonality maps (years count + binary + cumulative thresholds)
#' @keywords internal
.build_seasonality_maps <- function(
    dirs, iso3, adm0_name, type,
    location_summary, available_years,
    n_cumulative_maps, cumulative_thresholds,
    fig_dir, dpi
) {
  spatial <- sntutils::read_snt_data(
    path         = here::here(dirs$admin_shp, "processed"),
    data_name    = glue::glue("{iso3}_shp_list"),
    file_formats = "qs2"
  )$final_spat_vec

  adm2_sp    <- spatial$adm2
  adm1_sp    <- spatial$adm1
  merged     <- adm2_sp |> dplyr::left_join(location_summary, by = "adm2")
  type_label <- if (type == "rainfall") "Rainfall" else "Cases"
  yr_range   <- glue::glue("{min(available_years)}\u2013{max(available_years)}")
  n_years    <- length(available_years)

  # ── Map 1: Seasonal years count (choropleth 0–11) ───────────────────────────

  merged$category <- factor(
    as.character(tidyr::replace_na(merged$SeasonalYears, 0)),
    levels = as.character(0:11)
  )

  cat_pal <- c(
    "0"="#ffffff","1"="#ffeda0","2"="#fed976","3"="#feb24c",
    "4"="#fd8d3c","5"="#fc4e2a","6"="#e31a1c","7"="#bd0026",
    "8"="#800026","9"="#67001f","10"="#4d0016","11"="#33000d"
  )
  cat_n   <- table(merged$category)
  cat_n   <- cat_n[cat_n > 0]
  cat_pal <- cat_pal[names(cat_pal) %in% names(cat_n)]
  cat_lbl <- paste0(
    names(cat_n), " year",
    ifelse(as.integer(names(cat_n)) != 1, "s", ""),
    " (n=", cat_n, ")"
  )
  names(cat_lbl) <- names(cat_n)

  p1 <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = merged, ggplot2::aes(fill = category),
                     color = "grey30", linewidth = 0.25) +
    ggplot2::geom_sf(data = adm1_sp, fill = NA, color = "black", linewidth = 0.9) +
    ggplot2::scale_fill_manual(
      values = cat_pal, labels = cat_lbl, drop = FALSE,
      name   = glue::glue("Years with\nseasonal {tolower(type_label)} peaks")
    ) +
    ggplot2::coord_sf(datum = NA) +
    ggplot2::labs(
      title    = glue::glue("Seasonality of {type_label} in {adm0_name}"),
      subtitle = glue::glue(
        "Number of years with seasonal peaks eligible for SMC ({yr_range})"
      )
    ) +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      plot.title       = ggplot2::element_text(face = "bold", size = 18),
      plot.margin      = ggplot2::margin(5, 15, 2, 2)
    )

  ggplot2::ggsave(
    file.path(fig_dir, glue::glue("{iso3}_{type}_seasonality_years_map.png")),
    p1, width = 10, height = 8, dpi = dpi
  )

  # ── Map 2: Binary Seasonal / Not Seasonal ───────────────────────────────────

  merged$Seasonality <- factor(
    merged$Seasonality, levels = c("Seasonal", "Not Seasonal")
  )

  cls_pal <- c("Seasonal" = "#2d8659", "Not Seasonal" = "#f7f7f7")
  cls_n   <- table(merged$Seasonality)
  cls_n   <- cls_n[cls_n > 0]
  cls_lbl <- c(
    "Seasonal"     = paste0("SMC Seasonality (n=", cls_n["Seasonal"],     ")"),
    "Not Seasonal" = paste0("Non-Seasonal (n=",    cls_n["Not Seasonal"], ")")
  )
  cls_lbl <- cls_lbl[names(cls_lbl) %in% names(cls_n)]

  p2 <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = merged, ggplot2::aes(fill = Seasonality),
                     color = "grey30", linewidth = 0.25) +
    ggplot2::geom_sf(data = adm1_sp, fill = NA, color = "black", linewidth = 0.9) +
    ggplot2::scale_fill_manual(
      values = cls_pal[names(cls_pal) %in% names(cls_n)],
      labels = cls_lbl, drop = FALSE
    ) +
    ggplot2::coord_sf(datum = NA) +
    ggplot2::labs(
      title    = glue::glue("Malaria Seasonality Classification in {adm0_name}"),
      subtitle = glue::glue("Based on {tolower(type_label)} ({yr_range})")
    ) +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      plot.title       = ggplot2::element_text(face = "bold", size = 18),
      legend.position  = "right"
    )

  ggplot2::ggsave(
    file.path(fig_dir, glue::glue("{iso3}_{type}_seasonality_classification_map.png")),
    p2, width = 10, height = 8, dpi = dpi
  )

  # ── Maps 3+: Cumulative threshold maps ──────────────────────────────────────
  # Auto-generate thresholds if not manually supplied.
  # For 12 years data and n_cumulative_maps = 4:
  #   step 1 → {11}
  #   step 2 → {11, 10}
  #   step 3 → {11, 10, 9}
  #   step 4 → {11, 10, 9, 8}

  if (is.null(cumulative_thresholds)) {
    if (n_cumulative_maps >= n_years) {
      cli::cli_warn(c(
        "{.arg n_cumulative_maps} ({n_cumulative_maps}) \u2265 data span ({n_years} years).",
        "i" = "Reducing to {n_years - 1} cumulative map{?s}."
      ))
      n_cumulative_maps <- n_years - 1
    }
    top_year              <- n_years - 1
    cumulative_thresholds <- purrr::map(
      seq_len(n_cumulative_maps),
      ~ seq(from = top_year, by = -1, length.out = .x)
    )
  }

  colors_cumul <- c("Seasonal" = "#2d8659", "Not Seasonal" = "#f7f7f7")
  cumul_maps   <- list()

  for (thresh in cumulative_thresholds) {

    thresh_int   <- as.integer(thresh)
    thresh_label <- paste(thresh_int, collapse = " + ")
    safe_label   <- gsub("[^0-9]", "_", thresh_label)
    safe_label   <- gsub("_+", "_", safe_label)
    safe_label   <- gsub("^_|_$", "", safe_label)

    merged_thresh <- merged |>
      dplyr::mutate(
        Seasonality = dplyr::if_else(
          SeasonalYears %in% thresh_int,
          "Seasonal", "Not Seasonal"
        ),
        Seasonality = factor(Seasonality, levels = c("Seasonal", "Not Seasonal"))
      )

    t_n   <- table(merged_thresh$Seasonality)
    t_n   <- t_n[t_n > 0]
    t_pal <- colors_cumul[names(colors_cumul) %in% names(t_n)]
    t_lbl <- c(
      "Seasonal"     = paste0("SMC Seasonality (n=", t_n["Seasonal"],     ")"),
      "Not Seasonal" = paste0("Non-Seasonal (n=",    t_n["Not Seasonal"], ")")
    )
    t_lbl <- t_lbl[names(t_lbl) %in% names(t_n)]

    p_cumul <- ggplot2::ggplot() +
      ggplot2::geom_sf(
        data      = merged_thresh,
        ggplot2::aes(fill = Seasonality),
        color     = "grey30",
        linewidth = 0.25
      ) +
      ggplot2::geom_sf(
        data      = adm1_sp,
        fill      = NA,
        color     = "black",
        linewidth = 0.9
      ) +
      ggplot2::scale_fill_manual(
        values = t_pal,
        labels = t_lbl,
        drop   = FALSE
      ) +
      ggplot2::coord_sf(datum = NA) +
      ggplot2::labs(
        title    = glue::glue(
          "Malaria Seasonality \u2014 {thresh_label} years"
        ),
        subtitle = glue::glue(
          "Based on {tolower(type_label)} ({yr_range})"
        ),
        fill     = NULL
      ) +
      ggplot2::theme_minimal(base_size = 14) +
      ggplot2::theme(
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        plot.title       = ggplot2::element_text(face = "bold", size = 18, hjust = 0),
        plot.subtitle    = ggplot2::element_text(size = 11, hjust = 0,
                                                  margin = ggplot2::margin(t = 6)),
        legend.position  = "right",
        legend.title     = ggplot2::element_text(face = "bold", size = 12),
        legend.text      = ggplot2::element_text(size = 10),
        plot.margin      = ggplot2::margin(5, 15, 2, 2)
      )

    out_file <- file.path(
      fig_dir,
      glue::glue("{iso3}_{type}_seasonality_{safe_label}.png")
    )

    ggplot2::ggsave(out_file, p_cumul, width = 10, height = 8, dpi = dpi)
    cli::cli_alert_success(
      "Cumulative map: {thresh_label} years \u2192 {.path {out_file}}"
    )

    cumul_maps[[thresh_label]] <- p_cumul
  }

  cli::cli_alert_success(
    "All seasonality maps saved \u2192 {.path {fig_dir}}"
  )

  invisible(list(
    years_map          = p1,
    classification_map = p2,
    cumulative_maps    = cumul_maps
  ))
}


#' Calculate 3m / 4m / 5m rolling concentration windows per district per year
#' @keywords internal
.calculate_rolling_windows <- function(df, value_col = "value") {
  get_lbl <- function(start, len) {
    abbr <- c("jan","feb","mar","apr","may","jun",
              "jul","aug","sep","oct","nov","dec")
    paste(abbr[start:(start + len - 1)], collapse = "-")
  }

  make_blocks <- function(lkp, starts, len) {
    purrr::map(starts, function(s) {
      list(
        name = get_lbl(s, len),
        val  = sum(lkp[as.character(s:(s + len - 1))], na.rm = TRUE)
      )
    })
  }

  years     <- sort(unique(df$year))
  districts <- sort(unique(df$district))
  summary   <- data.frame()
  detailed  <- data.frame()

  for (yr in years) {
    yr_data <- df |> dplyr::filter(year == yr)

    for (dist in districts) {
      d     <- yr_data |> dplyr::filter(district == dist) |> dplyr::arrange(month)
      if (nrow(d) == 0) next
      total <- sum(d[[value_col]], na.rm = TRUE)
      if (total == 0) next

      lkp <- stats::setNames(d[[value_col]], as.character(d$month))
      b3  <- make_blocks(lkp, 4:9, 3)
      b4  <- make_blocks(lkp, 4:9, 4)
      b5  <- make_blocks(lkp, 4:8, 5)

      v3  <- purrr::map_dbl(b3, "val")
      v4  <- purrr::map_dbl(b4, "val")
      v5  <- purrr::map_dbl(b5, "val")
      n3  <- purrr::map_chr(b3, "name")
      n4  <- purrr::map_chr(b4, "name")
      n5  <- purrr::map_chr(b5, "name")

      summary <- rbind(summary, data.frame(
        year         = yr, district = dist, total = total,
        max_3m       = max(v3), max_4m = max(v4), max_5m = max(v5),
        pct_3m       = max(v3)/total*100,
        pct_4m       = max(v4)/total*100,
        pct_5m       = max(v5)/total*100,
        max_3m_block = n3[which.max(v3)],
        max_4m_block = n4[which.max(v4)],
        max_5m_block = n5[which.max(v5)],
        stringsAsFactors = FALSE
      ))

      det_row <- data.frame(district = dist, years = yr, stringsAsFactors = FALSE)
      for (x in c(b3, b4, b5)) det_row[[x$name]] <- x$val / total * 100
      det_row$max_3m       <- max(v3)/total*100
      det_row$max_4m       <- max(v4)/total*100
      det_row$max_5m       <- max(v5)/total*100
      det_row$max_3m_block <- n3[which.max(v3)]
      det_row$max_4m_block <- n4[which.max(v4)]
      det_row$max_5m_block <- n5[which.max(v5)]
      detailed <- rbind(detailed, det_row)
    }
  }

  cli::cli_alert_success(
    "Rolling windows: {nrow(summary)} district-year record{?s}."
  )
  invisible(list(summary = summary, detailed = detailed))
}


#' Build block frequency table
#' @keywords internal
.build_block_frequency <- function(summary_tbl, detailed_tbl) {
  freq <- data.frame()

  for (dist in unique(detailed_tbl$district)) {
    det         <- detailed_tbl |> dplyr::filter(district == dist)
    sm          <- summary_tbl  |> dplyr::filter(district == dist)
    total_years <- length(unique(det$years))

    for (dur in c(3L, 4L, 5L)) {
      blk_col <- paste0("max_", dur, "m_block")
      pct_col <- paste0("pct_",  dur, "m")

      for (blk in unique(det[[blk_col]])) {
        blk_yrs  <- det$years[det[[blk_col]] == blk]
        freq_pct <- length(blk_yrs) / total_years * 100
        med_prop <- stats::median(
          sm[[pct_col]][sm$year %in% blk_yrs], na.rm = TRUE
        )

        freq <- rbind(freq, data.frame(
          district        = dist,
          duration        = dur,
          block           = blk,
          block_freq      = round(freq_pct, 2),
          years           = paste(blk_yrs, collapse = ", "),
          median_max_prop = round(med_prop, 2),
          stringsAsFactors = FALSE
        ))
      }
    }
  }

  freq |> dplyr::arrange(district, duration, dplyr::desc(block_freq))
}


#' Derive final SMC-eligible district list
#' @keywords internal
.derive_smc_eligibility <- function(
    location_summary,
    smc_eligible_districts,
    smc_additional_districts,
    smc_remove_districts
) {
  if (!is.null(smc_eligible_districts)) {
    eligible <- smc_eligible_districts
    cli::cli_alert_info(
      "SMC base list: user-supplied ({length(eligible)} district{?s})."
    )
  } else if (!is.null(location_summary)) {
    eligible <- location_summary$adm2[
      !is.na(location_summary$Seasonality) &
      location_summary$Seasonality == "Seasonal"
    ]
    cli::cli_alert_info(
      "SMC base list: derived from seasonality ({length(eligible)} district{?s})."
    )
  } else {
    cli::cli_abort(c(
      "Cannot derive SMC eligibility.",
      "i" = "Provide {.arg smc_eligible_districts} or run {.val 'seasonality'} first."
    ))
  }

  if (!is.null(smc_additional_districts)) {
    n_before <- length(eligible)
    eligible <- unique(c(eligible, smc_additional_districts))
    cli::cli_alert_info(
      "Added {length(eligible) - n_before} district{?s} via {.arg smc_additional_districts}."
    )
  }

  if (!is.null(smc_remove_districts)) {
    n_before <- length(eligible)
    eligible <- setdiff(eligible, smc_remove_districts)
    cli::cli_alert_info(
      "Removed {n_before - length(eligible)} district{?s} via {.arg smc_remove_districts}."
    )
  }

  if (length(eligible) == 0) {
    cli::cli_warn(c(
      "No SMC-eligible districts remain.",
      "i" = "Check {.arg smc_remove_districts} and the seasonality classification."
    ))
  }

  eligible
}


# ==============================================================================
# HEATMAP FUNCTIONS
# Included here so this file is fully self-contained — no sourcing required.
#
# Public function:
#   run_heatmap_analysis()  — standalone heatmap for a single type
#
# Internal helpers (also called by .run_heatmap_step() in the pipeline):
#   .load_heatmap_data()    — read, validate, NA-filter, aggregate
#   .prepare_heatmap_data() — compute monthly % of annual total
#   .build_heatmap_plot()   — construct ggplot object
#   .save_heatmap_plot()    — write PNG to disk
# ==============================================================================

#' Generate a Seasonality Heatmap for Rainfall or Cases
#'
#' Standalone heatmap function. Produces a monthly distribution heatmap showing
#' each month's value as a percentage of that year's annual total. Can be called
#' directly without running the full pipeline.
#'
#' @param iso3 Character. ISO3 country code in lowercase (e.g. \code{"gha"}).
#' @param adm0_name Character. Country display name for the plot title.
#' @param type Character. One of \code{"rainfall"} or \code{"cases"}.
#'   Default: \code{"rainfall"}.
#' @param paths Named list from \code{sntutils::setup_project_paths()}.
#'   Default: \code{NULL}.
#' @param base_path Character or \code{NULL}. Base project directory for
#'   \code{sntutils::setup_project_paths()}. Default: \code{NULL}.
#' @param climate_dir Character or \code{NULL}. Direct path to rainfall folder.
#'   Default: \code{NULL}.
#' @param dhis2_dir Character or \code{NULL}. Direct path to DHIS2 folder.
#'   Default: \code{NULL}.
#' @param adm1_var Character. Column name for admin level 1. Default:
#'   \code{"adm1"}.
#' @param adm2_var Character. Column name for admin level 2. Default:
#'   \code{"adm2"}.
#' @param year_var Character. Column name for year. Default: \code{"year"}.
#' @param month_var Character. Column name for month. Default: \code{"month"}.
#' @param value_var Character or \code{NULL}. Metric column override. Default:
#'   \code{NULL} (auto-inferred from \code{type}).
#' @param s_year Integer or \code{NULL}. First year to include. Default:
#'   \code{NULL}.
#' @param e_year Integer or \code{NULL}. Last year to include. Default:
#'   \code{NULL}.
#' @param adm_level Character. Y-axis level. One of \code{"adm1"} (default)
#'   or \code{"adm2"}.
#' @param drop_na_cols Character vector. Structural columns that must be
#'   non-NA. Default: \code{c("adm1", "adm2", "year", "month")}.
#' @param drop_na_value Logical. Drop NAs in the metric column. Default:
#'   \code{TRUE}.
#' @param viridis_option Character. Viridis palette. Default: \code{"viridis"}.
#' @param x_breaks_by Numeric. X-axis tick interval in years. Default:
#'   \code{0.5}.
#' @param output_dir Character. Output folder. Only used for Option C (no
#'   sntutils). Default: \code{here::here("outputs", "figures")}.
#' @param filename Character or \code{NULL}. Output filename. Auto-generated
#'   if \code{NULL}. Default: \code{NULL}.
#' @param width Numeric. Plot width in inches. Default: \code{12}.
#' @param height Numeric. Plot height in inches. Default: \code{8}.
#' @param dpi Numeric. Plot resolution. Default: \code{400}.
#'
#' @return A named list (invisibly): \code{plot}, \code{data},
#'   \code{output_path}.
#'
#' @examples
#' # paths <- sntutils::setup_project_paths()
#' # run_heatmap_analysis(
#' #   iso3 = "gha", adm0_name = "Ghana",
#' #   paths = paths, type = "rainfall"
#' # )
#'
#' @export
run_heatmap_analysis <- function(
    iso3,
    adm0_name,
    type        = c("rainfall", "cases"),

    # ── paths ────────────────────────────────────────────────────────────────
    paths       = NULL,
    base_path   = NULL,
    climate_dir = NULL,
    dhis2_dir   = NULL,

    # ── column overrides ─────────────────────────────────────────────────────
    adm1_var    = "adm1",
    adm2_var    = "adm2",
    year_var    = "year",
    month_var   = "month",
    value_var   = NULL,

    # ── year range ────────────────────────────────────────────────────────────
    s_year      = NULL,
    e_year      = NULL,

    # ── aesthetics ────────────────────────────────────────────────────────────
    adm_level      = c("adm1", "adm2"),
    drop_na_cols   = c("adm1", "adm2", "year", "month"),
    drop_na_value  = TRUE,
    viridis_option = "viridis",
    x_breaks_by    = 0.5,

    # ── output ────────────────────────────────────────────────────────────────
    output_dir  = here::here("outputs", "figures"),
    filename    = NULL,
    width       = 12,
    height      = 8,
    dpi         = 400
) {

  type      <- match.arg(type)
  adm_level <- match.arg(adm_level)

  if (!is.character(iso3) || nchar(trimws(iso3)) == 0)
    cli::cli_abort("{.arg iso3} must be a non-empty character string.")

  if (!is.character(adm0_name) || nchar(trimws(adm0_name)) == 0)
    cli::cli_abort("{.arg adm0_name} must be a non-empty character string.")

  valid_viridis <- c("viridis", "magma", "plasma", "inferno", "cividis")
  if (!viridis_option %in% valid_viridis) {
    cli::cli_abort(c(
      "{.arg viridis_option} must be one of {.val {valid_viridis}}.",
      "x" = "Got {.val {viridis_option}}."
    ))
  }

  cli::cli_alert_info(
    "Heatmap: {.val {adm0_name}} | {.val {type}}"
  )

  # Resolve dirs using the shared helper — same logic as the full pipeline
  dirs <- .resolve_dirs(
    paths         = paths,
    base_path     = base_path,
    climate_dir   = climate_dir,
    dhis2_dir     = dhis2_dir,
    admin_shp_dir = NULL
  )

  # Determine output directory — sntutils users write to val_fig directly
  out_dir <- if (dirs$use_sntutils_output) dirs$val_fig else output_dir

  df_raw <- .load_heatmap_data(
    dirs          = dirs,
    iso3          = iso3,
    type          = type,
    adm1_var      = adm1_var,
    adm2_var      = adm2_var,
    year_var      = year_var,
    month_var     = month_var,
    value_var     = value_var,
    drop_na_cols  = drop_na_cols,
    drop_na_value = drop_na_value
  ) |>
    .filter_years(s_year, e_year)

  df_ready <- .prepare_heatmap_data(df_raw, adm_level = adm_level)

  p <- .build_heatmap_plot(
    df             = df_ready,
    adm0_name      = adm0_name,
    type           = type,
    adm_level      = adm_level,
    viridis_option = viridis_option,
    x_breaks_by    = x_breaks_by
  )

  out <- .save_heatmap_plot(
    plot       = p,
    output_dir = out_dir,
    filename   = filename,
    iso3       = iso3,
    type       = type,
    width      = width,
    height     = height,
    dpi        = dpi
  )

  cli::cli_alert_success(
    "Heatmap complete: {.val {adm0_name}} ({.val {type}})"
  )

  invisible(list(plot = p, data = df_ready, output_path = out))
}


#' Load and clean heatmap source data
#'
#' Reads the raw xlsx, validates columns, removes NA rows, and aggregates to
#' adm1/adm2/year/month level. Returns a tibble with a generic \code{value}
#' column suitable for either rainfall or cases.
#'
#' @param dirs Named list from \code{.resolve_dirs()}.
#' @param iso3 Character. ISO3 country code.
#' @param type Character. One of \code{"rainfall"} or \code{"cases"}.
#' @param adm1_var,adm2_var,year_var,month_var Character. Column name overrides.
#' @param value_var Character or \code{NULL}. Metric column name override.
#' @param drop_na_cols Character vector. Structural columns to check for NAs.
#' @param drop_na_value Logical. Whether to drop NAs in the metric column.
#'
#' @return A tibble with columns: \code{adm1}, \code{adm2}, \code{year},
#'   \code{month}, \code{yearmon}, \code{value}.
#' @keywords internal
.load_heatmap_data <- function(
    dirs,
    iso3,
    type,
    adm1_var,
    adm2_var,
    year_var,
    month_var,
    value_var,
    drop_na_cols,
    drop_na_value
) {

  # ── type config ──────────────────────────────────────────────────────────────

  cfg <- switch(type,
    rainfall = list(
      folder   = dirs$climate,
      filename = glue::glue("{iso3}_rainfall_processed"),
      col_src  = if (!is.null(value_var)) value_var else "mean_rainfall_mm"
    ),
    cases = list(
      folder   = dirs$dhis2,
      filename = glue::glue("{iso3}_dhis2_processed"),
      col_src  = if (!is.null(value_var)) value_var else "conf"
    )
  )

  # ── read ─────────────────────────────────────────────────────────────────────

  df <- sntutils::read_snt_data(cfg$folder, cfg$filename, "xlsx")

  # ── validate columns ─────────────────────────────────────────────────────────

  required_cols <- c(adm1_var, adm2_var, year_var, month_var, cfg$col_src)
  missing_cols  <- setdiff(required_cols, names(df))

  if (length(missing_cols) > 0) {
    cli::cli_abort(c(
      "Required column{?s} not found in {.val {cfg$filename}}:",
      "x" = "{.var {missing_cols}}",
      "i" = "Use the {.arg *_var} arguments to map your column names."
    ))
  }

  df <- df |>
    dplyr::select(
      dplyr::all_of(c(adm1_var, adm2_var, year_var, month_var, cfg$col_src))
    ) |>
    dplyr::rename(
      adm1  = !!adm1_var,
      adm2  = !!adm2_var,
      year  = !!year_var,
      month = !!month_var
    )

  # ── validate metric column ───────────────────────────────────────────────────

  if (!is.numeric(df[[cfg$col_src]])) {
    cli::cli_abort(c(
      "Column {.var {cfg$col_src}} must be numeric.",
      "x" = "Found type: {.cls {class(df[[cfg$col_src]])}}."
    ))
  }

  if (any(df[[cfg$col_src]] < 0, na.rm = TRUE)) {
    n_neg <- sum(df[[cfg$col_src]] < 0, na.rm = TRUE)
    cli::cli_abort(c(
      "Column {.var {cfg$col_src}} contains {n_neg} negative value{?s}.",
      "x" = "Metric values must be \u2265 0.",
      "i" = "Check your source data for data entry errors."
    ))
  }

  # ── NA handling ───────────────────────────────────────────────────────────────

  n_before <- nrow(df)

  # Remap drop_na_cols to standardised names in case user supplied original names
  std_drop_cols <- drop_na_cols |>
    gsub(pattern = adm1_var,  replacement = "adm1") |>
    gsub(pattern = adm2_var,  replacement = "adm2") |>
    gsub(pattern = year_var,  replacement = "year") |>
    gsub(pattern = month_var, replacement = "month")

  valid_drop_cols <- intersect(std_drop_cols, names(df))

  df <- df |>
    dplyr::filter(
      dplyr::if_all(dplyr::all_of(valid_drop_cols), ~ !is.na(.x))
    )

  if (drop_na_value) {
    df <- df |> dplyr::filter(!is.na(.data[[cfg$col_src]]))
  }

  n_dropped <- n_before - nrow(df)
  if (n_dropped > 0) {
    cli::cli_warn(c(
      "Dropped {n_dropped} row{?s} containing NA value{?s}.",
      "i" = "{n_before} \u2192 {nrow(df)} rows remaining.",
      "i" = "Set {.arg drop_na_value = FALSE} to retain rows with missing values."
    ))
  }

  if (nrow(df) == 0) {
    cli::cli_abort(c(
      "No rows remain after NA removal.",
      "x" = "All {n_before} row{?s} were dropped.",
      "i" = "Check your source data or review {.arg drop_na_cols}."
    ))
  }

  cli::cli_alert_success(
    "Loaded {nrow(df)} row{?s} from {.val {cfg$filename}}."
  )

  # ── aggregate and attach yearmon ──────────────────────────────────────────────

  df |>
    dplyr::group_by(adm1, adm2, year, month) |>
    dplyr::summarise(
      value = sum(.data[[cfg$col_src]], na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      yearmon = zoo::as.yearmon(sprintf("%04d-%02d", year, month))
    ) |>
    dplyr::arrange(adm1, adm2, yearmon)
}


#' Compute monthly proportions for heatmap
#'
#' Joins annual totals back to the monthly data and expresses each month's
#' value as a percentage of that year's total per admin unit. The
#' \code{adm_level} argument controls whether the y-axis groups by adm1 alone
#' or combines adm1 and adm2 into a \code{"Region ~ District"} label.
#'
#' @param df A tibble as returned by \code{.load_heatmap_data()}.
#' @param adm_level Character. One of \code{"adm1"} or \code{"adm2"}.
#'
#' @return The input tibble with additional columns: \code{annual_total},
#'   \code{prop_m}, \code{adm_label}.
#' @keywords internal
.prepare_heatmap_data <- function(df, adm_level = c("adm1", "adm2")) {

  adm_level <- match.arg(adm_level)

  # ── annual totals grouped at the correct level ───────────────────────────────

  group_cols <- if (adm_level == "adm1") {
    c("adm1", "year")
  } else {
    c("adm1", "adm2", "year")
  }

  annual_totals <- df |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
    dplyr::summarise(
      annual_total = sum(value, na.rm = TRUE),
      .groups = "drop"
    )

  # ── join proportions and build adm_label ─────────────────────────────────────

  join_cols <- setdiff(group_cols, "year")

  prepared <- df |>
    dplyr::left_join(annual_totals, by = c(join_cols, "year")) |>
    dplyr::mutate(
      # Add yearmon here so this function works whether the caller used
      # .load_heatmap_data() (which adds yearmon) or .load_analysis_data()
      # (the shared pipeline loader, which does not).
      yearmon   = zoo::as.yearmon(sprintf("%04d-%02d", year, month)),
      prop_m    = value / annual_total * 100,
      adm_label = if (adm_level == "adm2") {
        paste0(adm1, " ~ ", adm2)
      } else {
        adm1
      },
      adm_label = factor(
        adm_label,
        levels = rev(sort(unique(adm_label)))
      )
    ) |>
    dplyr::arrange(adm1, adm2)

  # ── warn on zero annual totals ────────────────────────────────────────────────

  n_zero <- sum(annual_totals$annual_total == 0, na.rm = TRUE)
  if (n_zero > 0) {
    cli::cli_warn(c(
      "{n_zero} admin-year combination{?s} have an annual total of zero.",
      "i" = "Proportions for these units will be {.val NaN}.",
      "i" = "This may indicate missing data for an entire year."
    ))
  }

  cli::cli_alert_success(
    "Proportions computed: {dplyr::n_distinct(prepared$adm_label)} {adm_level} unit{?s}."
  )

  prepared
}


#' Build the heatmap ggplot object
#'
#' @param df A tibble as returned by \code{.prepare_heatmap_data()}.
#' @param adm0_name Character. Country display name for the plot title.
#' @param type Character. One of \code{"rainfall"} or \code{"cases"}.
#' @param adm_level Character. One of \code{"adm1"} or \code{"adm2"}.
#'   Controls the y-axis label.
#' @param viridis_option Character. Viridis palette option.
#' @param x_breaks_by Numeric. Interval between x-axis ticks in years.
#'
#' @return A \code{ggplot} object.
#' @keywords internal
.build_heatmap_plot <- function(
    df, adm0_name, type, adm_level, viridis_option, x_breaks_by
) {

  # ── type-specific labels ──────────────────────────────────────────────────────

  legend_label <- switch(type,
    rainfall = "% of Annual\nRainfall",
    cases    = "% of Annual\nCases"
  )

  title_label <- switch(type,
    rainfall = glue::glue("{adm0_name}: Monthly Distribution of Rainfall"),
    cases    = glue::glue("{adm0_name}: Monthly Distribution of Cases")
  )

  y_label <- switch(adm_level,
    adm1 = "Region",
    adm2 = "Region ~ District"
  )

  # ── build ─────────────────────────────────────────────────────────────────────

  ggplot2::ggplot(
    df,
    ggplot2::aes(x = yearmon, y = adm_label, fill = prop_m)
  ) +
    ggplot2::geom_tile(color = "white", linewidth = 0.5) +
    ggplot2::scale_fill_viridis_c(
      name   = legend_label,
      option = viridis_option,
      labels = scales::percent_format(scale = 1)
    ) +
    zoo::scale_x_yearmon(
      breaks = seq(
        from = min(df$yearmon),
        to   = max(df$yearmon),
        by   = x_breaks_by
      ),
      format = "%b %Y"
    ) +
    ggplot2::labs(
      x     = NULL,
      y     = y_label,
      title = title_label
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      axis.text.y     = ggplot2::element_text(size = 9, hjust = 1, face = "bold"),
      axis.text.x     = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1),
      axis.title.y    = ggplot2::element_text(size = 10, face = "bold"),
      plot.title      = ggplot2::element_text(size = 13, face = "bold", hjust = 0.5),
      legend.title    = ggplot2::element_text(size = 10, face = "bold"),
      legend.text     = ggplot2::element_text(size = 9),
      panel.grid      = ggplot2::element_blank(),
      legend.position = "right",
      plot.margin     = ggplot2::margin(10, 10, 10, 10)
    )
}


#' Save the heatmap plot to disk
#'
#' @param plot A \code{ggplot} object.
#' @param output_dir Character. Output directory path.
#' @param filename Character or \code{NULL}. Output filename. Auto-generated
#'   as \code{"{ISO3}_{type}_heatmap.png"} if \code{NULL}.
#' @param iso3 Character. ISO3 code (for auto-generated filename).
#' @param type Character. Data type (for auto-generated filename).
#' @param width,height Numeric. Plot dimensions in inches.
#' @param dpi Numeric. Plot resolution.
#'
#' @return The full output path (invisibly).
#' @keywords internal
.save_heatmap_plot <- function(
    plot, output_dir, filename, iso3, type, width, height, dpi
) {

  if (is.null(filename)) {
    filename <- glue::glue("{toupper(iso3)}_{type}_heatmap.png")
  }

  # Only create if it genuinely does not exist — respects sntutils root dirs
  .ensure_dir(output_dir)

  output_path <- file.path(output_dir, filename)

  ggplot2::ggsave(
    filename = output_path,
    plot     = plot,
    width    = width,
    height   = height,
    dpi      = dpi
  )

  cli::cli_alert_success("Heatmap saved: {.path {output_path}}")
  invisible(output_path)
}


# ==============================================================================
# Additional packages required for case_rain_plots step: patchwork, cowplot
# ==============================================================================