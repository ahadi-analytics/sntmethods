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
# PACKAGE CHECKS
# ==============================================================================

#' Check that a required package is installed
#' @keywords internal
.check_pkg <- function(pkg, call_from = NULL) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    where <- if (!is.null(call_from)) glue::glue(" (called from {call_from})") else ""
    cli::cli_abort(c(
      "Required package {.pkg {pkg}} is not installed{where}.",
      "i" = "Install with: {.code install.packages('{pkg}')}"
    ))
  }
  invisible(TRUE)
}

#' Warn about missing optional packages without aborting
#' @keywords internal
.warn_pkg <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cli::cli_warn(c(
      "Optional package {.pkg {pkg}} is not installed.",
      "i" = "Some functionality may be unavailable.",
      "i" = "Install with: {.code install.packages('{pkg}')}"
    ))
    return(FALSE)
  }
  TRUE
}

#' Upfront soft-check for all packages the pipeline may use
#' @keywords internal
.check_pipeline_deps <- function(steps) {
  # Always required
  for (pkg in c("dplyr", "purrr", "ggplot2", "glue", "cli",
                "sntutils", "zoo", "scales", "viridisLite")) {
    .check_pkg(pkg, "run_seasonality_pipeline")
  }
  # Step-specific optional packages — warn but don't abort yet;
  # hard abort happens inside the step if the package is truly missing.
  if (any(c("seasonality", "blocks", "smc_maps") %in% steps)) {
    .warn_pkg("sf")
    .warn_pkg("qs2")
  }
  if ("graphs" %in% steps || "case_rain_plots" %in% steps) {
    .warn_pkg("lubridate")
    .warn_pkg("tidyr")
    .warn_pkg("cowplot")
    .warn_pkg("patchwork")
  }
}


# ==============================================================================
# MASTER FUNCTION
# ==============================================================================

#' Run the Full Malaria Seasonality Analysis Pipeline
#'
#' @description
#' A single-call pipeline that characterises malaria transmission seasonality
#' for a given country using processed rainfall and DHIS2 case data.  It
#' produces a complete set of publication-ready figures and versioned Excel/qs2
#' tables covering six analytical steps:
#'
#' \enumerate{
#'   \item \strong{Heatmap} — monthly distribution of rainfall or cases as a
#'     percentage of the annual total, displayed as a district-by-time tile plot.
#'   \item \strong{Seasonality classification} — rolling 4-month windows are
#'     tested against an annual threshold to classify each district-year as
#'     seasonal or not, producing binary and cumulative choropleth maps.
#'   \item \strong{Block concentration analysis} — identifies the 2-, 3-, 4-,
#'     and 5-month windows that capture the highest proportion of annual
#'     rainfall or cases, summarised per district across all years.
#'   \item \strong{Rolling overlay graphs} — per-province faceted line plots
#'     overlaying rainfall and case seasonality percentages over time.
#'   \item \strong{SMC eligibility and timing maps} — choropleth maps showing
#'     the optimal SMC start month and coverage per district for each cycle
#'     length, plus minimum-best-block summaries.
#'   \item \strong{Cases vs rainfall plots} — dual-axis time-series plots
#'     (both median seasonal profile and crude monthly series) of confirmed
#'     cases against total rainfall, one faceted figure per province.
#' }
#'
#' Steps that depend on earlier steps read results from memory when available,
#' and fall back to reading previously saved files from disk when not — so you
#' can re-run individual steps without re-running the whole pipeline.
#'
#' @section Supplying paths (choose one of three options):
#' The pipeline needs to know where to find processed rainfall, DHIS2, and
#' shapefile data, and where to write outputs.  Supply paths using \strong{one}
#' of the following three options — they are checked in order and the first
#' non-NULL option wins:
#'
#' \strong{Option A — sntutils paths object (recommended):}
#' \preformatted{
#' paths <- sntutils::setup_project_paths()
#' run_seasonality_pipeline(..., paths = paths)
#' }
#' All input and output directories are resolved automatically from the
#' standard SNT project folder structure.
#'
#' \strong{Option B — base path only:}
#' \preformatted{
#' run_seasonality_pipeline(..., base_path = Sys.getenv("MY_PROJECT_DIR"))
#' }
#' \code{sntutils::setup_project_paths()} is called internally on
#' \code{base_path}. Use this when you have not yet called
#' \code{setup_project_paths()} in your session.
#'
#' \strong{Option C — explicit directory paths:}
#' \preformatted{
#' run_seasonality_pipeline(
#'   ...,
#'   climate_dir   = "path/to/climate/processed",
#'   dhis2_dir     = "path/to/dhis2/processed",
#'   admin_shp_dir = "path/to/shapefiles"   # only needed for smc_maps
#' )
#' }
#' Outputs are written under \code{output_dir} (default:
#' \code{here::here("outputs")}). Use Option C when working outside a standard
#' SNT project structure.
#'
#' @section Input data requirements:
#' Both the rainfall and DHIS2 files must be \code{.xlsx} files named
#' \code{{iso3}_rainfall_processed.xlsx} and \code{{iso3}_dhis2_processed.xlsx}
#' respectively, located in the \code{processed/} subfolder of the climate and
#' DHIS2 directories.  At minimum they must contain columns for admin level 1,
#' admin level 2, year, month, and the metric of interest.  Column names can be
#' remapped via the \code{*_var} arguments if they differ from the defaults.
#'
#' @section Choosing max_non_seasonal_years:
#' This is the only required argument with no default.  It controls how
#' strictly a district must be seasonal across the full study period to be
#' classified as \emph{Seasonal}.  The formula is:
#' \preformatted{max_non_seasonal_years = total_years - min_seasonal_years}
#' For example, with 12 years of data and a rule of "seasonal in at least 10
#' out of 12 years", use \code{max_non_seasonal_years = 2}.  A value of
#' \code{0} means every single year must show a seasonal peak.
#'
#' @param iso3 Character. ISO3 country code in \strong{lowercase}
#'   (e.g. \code{"gin"} for Guinea, \code{"civ"} for Côte d'Ivoire).
#'   Used to construct input filenames and output file prefixes.
#' @param adm0_name Character. Country display name used in plot titles and
#'   subtitles (e.g. \code{"Guinea"}).
#' @param paths Named list from \code{sntutils::setup_project_paths()}.
#'   When supplied, all input and output paths are resolved automatically.
#'   Default: \code{NULL}. See the \emph{Supplying paths} section above.
#' @param base_path Character or \code{NULL}. Root project directory passed to
#'   \code{sntutils::setup_project_paths()} internally. Only used when
#'   \code{paths} is \code{NULL}. Default: \code{NULL}.
#' @param climate_dir Character or \code{NULL}. Direct path to the folder
#'   containing \code{{iso3}_rainfall_processed.xlsx}. Only used when both
#'   \code{paths} and \code{base_path} are \code{NULL}. Default: \code{NULL}.
#' @param dhis2_dir Character or \code{NULL}. Direct path to the folder
#'   containing \code{{iso3}_dhis2_processed.xlsx}. Only used when both
#'   \code{paths} and \code{base_path} are \code{NULL}. Default: \code{NULL}.
#' @param admin_shp_dir Character or \code{NULL}. Direct path to the processed
#'   shapefiles folder. Only required when running the \code{"smc_maps"} step
#'   with Option C paths. Default: \code{NULL}.
#' @param type Character. Which data to analyse. One of \code{"both"}
#'   (default, runs rainfall and cases), \code{"rainfall"}, or \code{"cases"}.
#'   Note: the \code{"graphs"} step requires \code{"both"} and is automatically
#'   skipped if only one type is selected.
#' @param steps Character vector. Which pipeline steps to run. Any subset of
#'   \code{c("heatmap", "seasonality", "blocks", "graphs", "smc_maps",
#'   "case_rain_plots")}. Steps are always executed in the order listed above
#'   regardless of the order supplied here. Default: all six steps.
#' @param s_year Integer or \code{NULL}. First calendar year to include in the
#'   analysis. Rows with \code{year < s_year} are dropped. Default: \code{NULL}
#'   (uses all available years).
#' @param e_year Integer or \code{NULL}. Last calendar year to include.
#'   Default: \code{NULL} (uses all available years).
#' @param adm1_var Character. Name of the admin level 1 column in the source
#'   files. Default: \code{"adm1"}.
#' @param adm2_var Character. Name of the admin level 2 (district) column.
#'   Default: \code{"adm2"}.
#' @param adm3_var Character or \code{NULL}. Name of the admin level 3 column.
#'   When supplied, adm3 is joined into the block analysis output tables
#'   (summary, detailed, frequency). The column must exist in both source files.
#'   Default: \code{NULL}.
#' @param year_var Character. Name of the year column. Default: \code{"year"}.
#' @param month_var Character. Name of the month column (integer 1–12).
#'   Default: \code{"month"}.
#' @param value_var_rainfall Character or \code{NULL}. Rainfall metric column
#'   to use in the seasonality and block analyses. Default: \code{NULL}
#'   (uses \code{"mean_rainfall_mm"}).
#' @param value_var_cases Character or \code{NULL}. Cases metric column to use.
#'   Should match \code{case_stratification}: e.g. use \code{"conf_ov5"} when
#'   \code{case_stratification = "ov5"}. Default: \code{NULL} (uses
#'   \code{"conf"}).
#' @param adm_level Character. Y-axis grouping level for the heatmap. One of
#'   \code{"adm2"} (default) or \code{"adm1"}. Automatically switched to
#'   \code{"adm1"} when more than 40 adm2 units are present.
#' @param viridis_option Character. Viridis palette for the heatmap fill scale.
#'   One of \code{"viridis"} (default), \code{"magma"}, \code{"plasma"},
#'   \code{"inferno"}, or \code{"cividis"}.
#' @param x_breaks_by Numeric. Interval between x-axis tick marks on the
#'   heatmap, in years. Default: \code{0.5} (every 6 months).
#' @param analysis_start_month Integer (1–12). Calendar month at which rolling
#'   windows begin. Default: \code{1} (January). Change to \code{7} for
#'   southern-hemisphere countries where the rainy season spans the calendar
#'   year boundary.
#' @param seasonality_threshold Numeric (0–100). Minimum percentage of the
#'   annual total that a 4-month window must contain to be counted as a
#'   seasonal peak. Default: \code{60}.
#' @param max_non_seasonal_years Integer. \strong{Required — no default.}
#'   Maximum number of years in which a district may fail to show a seasonal
#'   peak and still be classified as Seasonal overall.
#'   Formula: \code{total_years - min_seasonal_years}.
#'   Example: 12 years of data, require seasonal in \eqn{\geq} 10 years
#'   \eqn{\Rightarrow} \code{max_non_seasonal_years = 2}.
#' @param min_years_required Integer. Minimum number of years of data needed
#'   to run the seasonality step. An error is thrown if fewer years are found
#'   after applying \code{s_year}/\code{e_year} filters. Default: \code{6}.
#' @param n_cumulative_maps Integer. Number of cumulative threshold maps to
#'   produce in addition to the binary classification map. Maps count down
#'   from \code{n_years - 1} seasonal years. For example, with 12 years and
#'   \code{n_cumulative_maps = 4} the thresholds are \{11\}, \{11,10\},
#'   \{11,10,9\}, \{11,10,9,8\}. Default: \code{4}.
#' @param cumulative_thresholds List of integer vectors or \code{NULL}.
#'   Manual override for the cumulative threshold maps. Each vector element
#'   specifies which \code{SeasonalYears} values are coloured as Seasonal on
#'   that map. When \code{NULL}, auto-generated from \code{n_cumulative_maps}.
#'   Example: \code{list(c(11), c(11, 10), c(11, 10, 9))}.
#' @param smc_eligible_districts Character vector or \code{NULL}. An explicit
#'   list of adm2 district names to treat as SMC-eligible, bypassing the
#'   derived classification. Only used when \code{smc_eligibility_source =
#'   "manual"}. Default: \code{NULL}.
#' @param smc_additional_districts Character vector or \code{NULL}. District
#'   names to add to the derived eligible list after derivation. Applied before
#'   \code{smc_remove_districts}. Default: \code{NULL}.
#' @param smc_remove_districts Character vector or \code{NULL}. District names
#'   to remove from the eligible list. Applied last, after additions.
#'   Default: \code{NULL}.
#' @param smc_eligibility_source Character. Determines which seasonality
#'   classification is used to derive SMC-eligible districts. One of:
#'   \itemize{
#'     \item \code{"rainfall"} (default) — districts classified as Seasonal
#'       by the \strong{rainfall} analysis. Used for both the rainfall and
#'       cases SMC maps. This is the standard field practice.
#'     \item \code{"cases"} — districts classified as Seasonal by the
#'       \strong{cases} analysis.
#'     \item \code{"manual"} — \code{smc_eligible_districts} is used verbatim.
#'       Requires \code{smc_eligible_districts} to be non-\code{NULL}.
#'   }
#' @param case_stratification Character. Age group for cases outputs. Controls
#'   the output subfolder name and file prefixes so that multiple
#'   stratifications can be run back-to-back without overwriting each other.
#'   One of \code{"ov5"} (over 5, default), \code{"u5"} (under 5), or
#'   \code{"all"} (all ages). \strong{Remember to also set}
#'   \code{value_var_cases} to the matching column (e.g.
#'   \code{value_var_cases = "conf_u5"} when \code{case_stratification = "u5"}).
#' @param case_var_ov5 Character. Column name for confirmed over-5 cases in the
#'   DHIS2 file. Used by the \code{"case_rain_plots"} step only. Default:
#'   \code{"conf_ov5"}.
#' @param case_var_u5 Character. Column name for confirmed under-5 cases.
#'   Default: \code{"conf_u5"}.
#' @param rainfall_total_var Character. Column name for total monthly rainfall
#'   (mm) in the rainfall file. Used by \code{"case_rain_plots"} only. Default:
#'   \code{"total_rainfall_mm"}.
#' @param case_rain_s_date Character or \code{NULL}. Start date for the
#'   \strong{median} cases vs rainfall plots, in \code{"YYYY-MM-DD"} format
#'   (e.g. \code{"2015-01-01"}). Also used as the fallback start date for the
#'   crude plots when \code{crude_s_date} is not supplied. Default: \code{NULL}
#'   (uses \code{s_year} if provided, otherwise all available data).
#' @param case_rain_e_date Character or \code{NULL}. End date for the median
#'   plots, \code{"YYYY-MM-DD"} format. Default: \code{NULL}.
#' @param crude_s_date Character or \code{NULL}. \strong{Independent} start
#'   date for the crude (time-series) cases vs rainfall plots only, in
#'   \code{"YYYY-MM-DD"} format. Use this when rainfall and case datasets cover
#'   different year ranges and you need both series to begin at the same point
#'   — for example, if rainfall starts in 2014 but cases only from 2020, set
#'   \code{crude_s_date = "2020-01-01"}. When supplied, fully overrides
#'   \code{case_rain_s_date} for the crude plots. Default: \code{NULL}.
#' @param crude_e_date Character or \code{NULL}. Independent end date for the
#'   crude plots, \code{"YYYY-MM-DD"} format. Default: \code{NULL}.
#' @param panels_per_row Integer. Default number of district panels per row in
#'   the faceted cases vs rainfall plots. Automatically scaled up for provinces
#'   with many districts (\eqn{\geq}9 \eqn{\rightarrow} 4, \eqn{\geq}15
#'   \eqn{\rightarrow} 5, \eqn{\geq}30 \eqn{\rightarrow} 6). Default: \code{3}.
#' @param output_dir Character. Root output folder used only with Option C
#'   (direct paths). Figures are written to \code{output_dir/figures} and
#'   tables to \code{output_dir/tables}. Ignored when \code{paths} or
#'   \code{base_path} is supplied. Default: \code{here::here("outputs")}.
#' @param include_date Logical. Whether to append a date stamp to saved output
#'   filenames via \code{sntutils::write_snt_data()}. Enables versioning so
#'   re-runs do not overwrite previous outputs. Default: \code{TRUE}.
#' @param n_saved Integer. Maximum number of dated versions of each output
#'   file to retain on disk. Older versions beyond this limit are pruned
#'   automatically. Default: \code{3}.
#' @param dpi Numeric. Resolution (dots per inch) for all saved PNG figures.
#'   Default: \code{400}.
#'
#' @return A named list returned invisibly. Each element corresponds to a step
#'   that was run:
#'   \describe{
#'     \item{\code{heatmap}}{Named by type (\code{"rainfall"}, \code{"cases"}).
#'       Each element is a list with \code{plot} (ggplot object),
#'       \code{data} (prepared data frame), and \code{output_path} (file path).}
#'     \item{\code{seasonality}}{Named by type. Each element contains
#'       \code{detailed_results}, \code{yearly_summary},
#'       \code{location_summary}, and \code{maps}.}
#'     \item{\code{blocks}}{Named by type. Each element contains
#'       \code{summary}, \code{detailed}, and \code{frequency} data frames.}
#'     \item{\code{graphs}}{Named list of ggplot objects, one per province,
#'       plus \code{.summary} for the all-provinces overview.}
#'     \item{\code{smc_maps}}{Named by type. Each element contains
#'       \code{timing_maps}, \code{coverage_maps}, \code{eligible} (character
#'       vector of eligible district names), \code{min_best_table}, and
#'       \code{min_best_maps}.}
#'     \item{\code{case_rain_plots}}{A list with elements \code{median} and
#'       \code{crude}, each a named list of ggplot objects per province.}
#'   }
#'   All figures are also saved to disk. Tables are saved as versioned
#'   \code{.xlsx} and \code{.qs2} files.
#'
#' @examples
#' \dontrun{
#' # ── Minimal full run (sntutils project) ─────────────────────────────────────
#' paths <- sntutils::setup_project_paths()
#'
#' run_seasonality_pipeline(
#'   iso3                   = "gin",
#'   adm0_name              = "Guinea",
#'   paths                  = paths,
#'   max_non_seasonal_years = 2   # seasonal in >= 10 of 12 years
#' )
#'
#' # ── Rainfall only, restrict to 2015–2022 ────────────────────────────────────
#' run_seasonality_pipeline(
#'   iso3                   = "gin",
#'   adm0_name              = "Guinea",
#'   paths                  = paths,
#'   type                   = "rainfall",
#'   s_year                 = 2015,
#'   e_year                 = 2022,
#'   max_non_seasonal_years = 2
#' )
#'
#' # ── Cases under-5 stratification ────────────────────────────────────────────
#' run_seasonality_pipeline(
#'   iso3                   = "gin",
#'   adm0_name              = "Guinea",
#'   paths                  = paths,
#'   max_non_seasonal_years = 2,
#'   case_stratification    = "u5",
#'   value_var_cases        = "conf_u5"
#' )
#'
#' # ── Run only the cases vs rainfall plots ────────────────────────────────────
#' # Useful when rainfall starts 2014 but case data only from 2020.
#' run_seasonality_pipeline(
#'   iso3                   = "gin",
#'   adm0_name              = "Guinea",
#'   paths                  = paths,
#'   max_non_seasonal_years = 2,
#'   steps                  = "case_rain_plots",
#'   crude_s_date           = "2020-01-01",
#'   crude_e_date           = "2024-12-31"
#' )
#'
#' # ── SMC maps using cases-derived eligibility ─────────────────────────────────
#' run_seasonality_pipeline(
#'   iso3                   = "gin",
#'   adm0_name              = "Guinea",
#'   paths                  = paths,
#'   max_non_seasonal_years = 2,
#'   smc_eligibility_source = "cases"
#' )
#'
#' # ── Manually override which districts are SMC-eligible ───────────────────────
#' run_seasonality_pipeline(
#'   iso3                     = "gin",
#'   adm0_name                = "Guinea",
#'   paths                    = paths,
#'   max_non_seasonal_years   = 2,
#'   smc_eligibility_source   = "manual",
#'   smc_eligible_districts   = c("BOFFA", "KINDIA", "COYAH"),
#'   smc_additional_districts = c("FORECARIAH"),
#'   smc_remove_districts     = c("COYAH")
#' )
#'
#' # ── Option C: direct paths, no sntutils project structure ───────────────────
#' run_seasonality_pipeline(
#'   iso3                   = "gin",
#'   adm0_name              = "Guinea",
#'   climate_dir            = "data/climate/processed",
#'   dhis2_dir              = "data/dhis2/processed",
#'   admin_shp_dir          = "data/shapefiles",
#'   output_dir             = "outputs",
#'   max_non_seasonal_years = 2
#' )
#' }
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
    adm3_var           = NULL,
    year_var           = "year",
    month_var          = "month",
    value_var_rainfall = NULL,
    value_var_cases    = NULL,

    # ── heatmap aesthetics ────────────────────────────────────────────────────
    adm_level      = c("adm2", "adm1"),
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
    smc_eligibility_source   = c("rainfall", "cases", "manual"),

    # ── case stratification ───────────────────────────────────────────────────
    case_stratification = "ov5",

    # ── case vs rainfall plot options ─────────────────────────────────────────
    case_var_ov5       = "conf_ov5",
    case_var_u5        = "conf_u5",
    rainfall_total_var = "total_rainfall_mm",
    case_rain_s_date   = NULL,
    case_rain_e_date   = NULL,
    crude_s_date       = NULL,
    crude_e_date       = NULL,
    panels_per_row     = 3,

    # ── output ────────────────────────────────────────────────────────────────
    output_dir   = here::here("outputs"),
    include_date = TRUE,
    n_saved      = 3,
    dpi          = 400
) {

  # ── upfront package checks ──────────────────────────────────────────────────
  .check_pipeline_deps(steps)

  # ── validate inputs ─────────────────────────────────────────────────────────

  type                   <- match.arg(type)
  adm_level              <- match.arg(adm_level)
  smc_eligibility_source <- match.arg(smc_eligibility_source)

  # Validate: "manual" requires smc_eligible_districts to be supplied
  if (smc_eligibility_source == "manual" && is.null(smc_eligible_districts)) {
    cli::cli_abort(c(
      "{.arg smc_eligibility_source = 'manual'} requires {.arg smc_eligible_districts}.",
      "i" = "Provide a character vector of district names, or change {.arg smc_eligibility_source}."
    ))
  }

  valid_steps <- c("heatmap", "seasonality", "blocks",
                   "graphs", "smc_maps", "case_rain_plots")
  bad_steps <- setdiff(steps, valid_steps)
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

  if (!is.numeric(dpi) || dpi <= 0)
    cli::cli_abort("{.arg dpi} must be a positive number.")

  if (!is.numeric(n_saved) || n_saved < 1)
    cli::cli_abort("{.arg n_saved} must be a positive integer.")

  # Validate year range
  if (!is.null(s_year) && !is.null(e_year) && s_year > e_year) {
    cli::cli_abort(c(
      "{.arg s_year} must be \u2264 {.arg e_year}.",
      "x" = "Got {.arg s_year} = {.val {s_year}}, {.arg e_year} = {.val {e_year}}."
    ))
  }

  # ── step dependency checks ──────────────────────────────────────────────────

  if ("smc_maps" %in% steps &&
      !("blocks" %in% steps) &&
      !("seasonality" %in% steps) &&
      is.null(smc_eligible_districts) &&
      smc_eligibility_source != "manual") {
    cli::cli_abort(c(
      "Step {.val smc_maps} has unresolved dependencies.",
      "i" = "Add {.val 'blocks'} and {.val 'seasonality'} to {.arg steps}, or",
      "i" = "supply {.arg smc_eligible_districts} with {.arg smc_eligibility_source = 'manual'},",
      "i" = "or run those steps before {.val 'smc_maps'}."
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

  if (dirs$use_sntutils_output) {
    fig_root <- dirs$val_fig
    tbl_root <- dirs$val_tbl
  } else {
    fig_root <- file.path(output_dir, "figures")
    tbl_root <- file.path(output_dir, "tables")
    .ensure_dir(fig_root)
    .ensure_dir(tbl_root)
  }

  # ── write options bundle (passed to every step) ────────────────────────────
  # Mirrors how the MBG pipeline standardises sntutils::write_snt_data() args.

  write_opts <- list(include_date = include_date, n_saved = n_saved)

  # ── types to run ────────────────────────────────────────────────────────────

  types_to_run <- if (type == "both") c("rainfall", "cases") else type

  # ── announce ────────────────────────────────────────────────────────────────

  cli::cli_h1("Seasonality Pipeline: {adm0_name} ({toupper(iso3)})")
  cli::cli_alert_info("Steps:   {.val {steps}}")
  cli::cli_alert_info("Type(s): {.val {types_to_run}}")
  cli::cli_alert_info("Output:  {.path { .relative_path(fig_root)}}")

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
        write_opts             = write_opts,
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
        dirs                = dirs,
        iso3                = iso3,
        type                = .x,
        s_year              = s_year,
        e_year              = e_year,
        adm1_var            = adm1_var,
        adm2_var            = adm2_var,
        adm3_var            = adm3_var,
        year_var            = year_var,
        month_var           = month_var,
        value_var           = if (.x == "rainfall") value_var_rainfall else value_var_cases,
        case_stratification = case_stratification,
        tbl_root            = tbl_root,
        write_opts          = write_opts
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

    # Resolve which location_summary drives eligibility for each type.
    # When smc_eligibility_source = "rainfall", the cases map also uses the
    # rainfall-derived eligible districts (the most common field practice).
    # When smc_eligibility_source = "cases",  both maps use cases-derived.
    # When smc_eligibility_source = "manual",  smc_eligible_districts is used
    # directly and location_summary is ignored by .derive_smc_eligibility().
    .pick_location_summary <- function(type) {
      if (smc_eligibility_source == "manual") return(NULL)
      src <- if (smc_eligibility_source == "cases") "cases" else "rainfall"
      results$seasonality[[src]]$location_summary
    }

    results$smc_maps <- purrr::map(
      purrr::set_names(types_to_run),
      ~ .run_smc_maps_step(
        dirs                     = dirs,
        iso3                     = iso3,
        adm0_name                = adm0_name,
        type                     = .x,
        block_frequency          = results$blocks[[.x]]$frequency,
        location_summary         = .pick_location_summary(.x),
        tbl_root                 = tbl_root,
        case_stratification      = case_stratification,
        smc_eligible_districts   = smc_eligible_districts,
        smc_additional_districts = smc_additional_districts,
        smc_remove_districts     = smc_remove_districts,
        fig_root                 = fig_root,
        write_opts               = write_opts,
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
      crude_s_date       = crude_s_date,
      crude_e_date       = crude_e_date,
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

  n_adm2 <- dplyr::n_distinct(df_raw$adm2)
  if (n_adm2 > 40 && adm_level == "adm2") {
    cli::cli_warn(c(
      "{n_adm2} adm2 units detected \u2014 exceeds 40.",
      "i" = "Switching heatmap to {.val adm1} level for readability."
    ))
    adm_level <- "adm1"
  }

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
    plot = p, output_dir = fig_root, filename = NULL,
    iso3 = iso3, type = type, width = 12, height = 8, dpi = 500
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
    fig_root, tbl_root, write_opts, dpi
) {
  cli::cli_alert_info("Seasonality \u2192 {.val {type}}")

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

  blocks           <- .generate_rolling_blocks(df, analysis_start_month)
  detailed_results <- .calculate_seasonality(df, blocks, seasonality_threshold)
  yearly_summary   <- .build_yearly_summary(detailed_results)
  location_summary <- .build_location_summary(yearly_summary, max_non_seasonal_years)

  n_s  <- sum(location_summary$Seasonality == "Seasonal",     na.rm = TRUE)
  n_ns <- sum(location_summary$Seasonality == "Not Seasonal", na.rm = TRUE)
  cli::cli_alert_success("Classification: {n_s} seasonal, {n_ns} not seasonal.")

  # ── save tables ─────────────────────────────────────────────────────────────
  # Uses versioned write_snt_data() with date-stamping and auto-pruning.

  type_pfx <- .type_prefix(type, case_stratification)

  .write_snt <- function(obj, name) {
    sntutils::write_snt_data(
      obj          = obj,
      path         = tbl_root,
      data_name    = glue::glue("{iso3}_{name}"),
      file_formats = c("xlsx", "qs2"),
      include_date = write_opts$include_date,
      n_saved      = write_opts$n_saved
    )
  }

  # Attach data dictionaries — mirrors MBG pipeline's build_dictionary pattern
  .write_snt(
    list(
      data          = detailed_results,
      data_dict     = sntutils::build_dictionary(data = detailed_results)
    ),
    glue::glue("{type_pfx}_detailed_seasonality_results")
  )
  .write_snt(
    list(
      data          = yearly_summary,
      data_dict     = sntutils::build_dictionary(data = yearly_summary)
    ),
    glue::glue("{type_pfx}_yearly_seasonality_summary")
  )
  .write_snt(
    list(
      data          = location_summary,
      data_dict     = sntutils::build_dictionary(data = location_summary)
    ),
    glue::glue("{type_pfx}_location_seasonality_summary")
  )

  cli::cli_alert_success(
    "Seasonality tables saved \u2192 {.path { .relative_path(tbl_root)}}"
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
    adm1_var, adm2_var, adm3_var = NULL, year_var, month_var, value_var,
    case_stratification,
    tbl_root, write_opts
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

  # ── optional adm3 lookup (mirrors MBG's clean join pattern) ─────────────────

  adm3_lookup <- .resolve_adm3_lookup(
    dirs     = dirs,
    iso3     = iso3,
    type     = type,
    adm1_var = adm1_var,
    adm2_var = adm2_var,
    adm3_var = adm3_var
  )

  windows       <- .calculate_rolling_windows(df)
  summary_tbl   <- windows$summary
  detailed_tbl  <- windows$detailed
  frequency_tbl <- .build_block_frequency(summary_tbl, detailed_tbl)

  # Join adm3 into all three tables when available
  if (!is.null(adm3_lookup)) {
    adm3_only <- adm3_lookup |> dplyr::select(district, adm3)
    summary_tbl   <- summary_tbl   |>
      dplyr::left_join(adm3_only, by = "district") |>
      dplyr::select(district, adm1, adm2, adm3, dplyr::everything())
    detailed_tbl  <- detailed_tbl  |>
      dplyr::left_join(adm3_only, by = "district") |>
      dplyr::select(district, adm1, adm2, adm3, years, dplyr::everything())
    frequency_tbl <- frequency_tbl |>
      dplyr::left_join(adm3_only, by = "district") |>
      dplyr::select(district, adm1, adm2, adm3, dplyr::everything())
  }

  # ── save tables ─────────────────────────────────────────────────────────────

  tbl_sub <- file.path(tbl_root, .type_tbl_subdir(type, case_stratification))
  .ensure_dir(tbl_sub)

  .write_block <- function(obj, name) {
    sntutils::write_snt_data(
      obj          = list(
        data      = obj,
        data_dict = sntutils::build_dictionary(data = obj)
      ),
      path         = tbl_sub,
      data_name    = glue::glue("{iso3}_{name}"),
      file_formats = c("xlsx", "qs2"),
      include_date = write_opts$include_date,
      n_saved      = write_opts$n_saved
    )
  }

  if (type == "rainfall") {
    .write_block(summary_tbl,   "malaria_rainfall_block_analysis")
    .write_block(detailed_tbl,  "malaria_detailed_yearly_block_analysis")
  } else {
    .write_block(summary_tbl,   "malaria_cases_block_analysis")
    .write_block(detailed_tbl,  "malaria_cases_detailed_yearly_block_analysis")
  }
  .write_block(frequency_tbl, "malaria_block_frequency_analysis")

  cli::cli_alert_success(
    "Block tables saved \u2192 {.path { .relative_path(tbl_sub)}}"
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
  rain_df <- .get_or_read_detailed(rain_detailed, tbl_root, iso3, "rainfall",
                                   case_stratification)
  case_df <- .get_or_read_detailed(case_detailed,  tbl_root, iso3, "cases",
                                   case_stratification)

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

  graphs_dir <- file.path(fig_root, "cas_rain_graphs")
  .ensure_dir(graphs_dir)

  provinces <- sort(unique(merged_df$province))

  plots <- purrr::map(purrr::set_names(provinces), function(prov) {
    prov_data  <- merged_df |> dplyr::filter(province == prov)
    n_dist     <- dplyr::n_distinct(prov_data$district)

    facet_ncol <- dplyr::case_when(
      n_dist >= 30 ~ 6L,
      n_dist >= 15 ~ 4L,
      n_dist >= 6  ~ 3L,
      TRUE         ~ 2L
    )
    n_rows     <- ceiling(n_dist / facet_ncol)
    fig_height <- max(6, n_rows * 4)
    fig_width  <- facet_ncol * 5

    p <- ggplot2::ggplot(
      prov_data,
      ggplot2::aes(x = Date, y = Percent_Seasonality, color = Type)
    ) +
      ggplot2::geom_line(linewidth = 0.8) +
      ggplot2::facet_wrap(~ district, ncol = facet_ncol, scales = "free_y") +
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
      .snt_theme_minimal()

    ggplot2::ggsave(
      filename  = paste0(gsub(" ", "_", prov), "_seasonality_comparison.png"),
      plot      = p,
      path      = graphs_dir,
      width     = fig_width,
      height    = fig_height,
      dpi       = dpi,
      limitsize = FALSE
    )
    sntutils::compress_png(
      path = file.path(graphs_dir, paste0(gsub(" ", "_", prov), "_seasonality_comparison.png"))
    )

    cli::cli_alert_success("Overlay graph saved: {.val {prov}}")
    p
  })

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
      title    = glue::glue("{adm0_name}: Average Seasonality - All Provinces"),
      subtitle = "Mean Percent Seasonality of Cases vs Rainfall",
      x = NULL, y = "Mean Percent Seasonality (%)", color = "Type"
    ) +
    .snt_theme_minimal()

  ggplot2::ggsave(
    filename  = glue::glue("{iso3}_ALL_PROVINCES_summary_comparison.png"),
    plot      = summary_plot,
    path      = graphs_dir,
    width     = 14, height = 12, dpi = dpi,
    limitsize = FALSE
  )
  sntutils::compress_png(
    path = file.path(graphs_dir, glue::glue("{iso3}_ALL_PROVINCES_summary_comparison.png"))
  )

  cli::cli_alert_success("Summary overlay graph saved \u2192 {.path { .relative_path(graphs_dir)}}")
  invisible(c(plots, list(.summary = summary_plot)))
}


#' @keywords internal
.run_smc_maps_step <- function(
    dirs, iso3, adm0_name, type,
    block_frequency, location_summary, tbl_root,
    case_stratification,
    smc_eligible_districts, smc_additional_districts, smc_remove_districts,
    fig_root, write_opts, dpi
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

  spatial <- sntutils::read_snt_data(
    path         = here::here(dirs$admin_shp, "processed"),
    data_name    = glue::glue("{iso3}_shp_list"),
    file_formats = "qs2"
  )$final_spat_vec

  adm2_sp <- spatial$adm2
  adm1_sp <- spatial$adm1

  smc_fig_dir <- file.path(fig_root, .type_smc_subdir(type))
  .ensure_dir(smc_fig_dir)

  dims <- .compute_map_dims(adm2_sp)

  month_lkp <- c(apr = 4L, may = 5L, jun = 6L, jul = 7L, aug = 8L, sep = 9L)

  if (all(c("adm1", "adm2") %in% names(freq))) {
    freq_adm <- freq
  } else {
    cli::cli_warn(c(
      "Block frequency table is missing adm1/adm2 columns.",
      "i" = "Falling back to district string parsing \u2014 hyphenated names may not match."
    ))
    freq_adm <- freq |>
      dplyr::mutate(
        adm1 = stringr::str_trim(stringr::str_extract(district, "^[^-]+")),
        adm2 = stringr::str_trim(stringr::str_extract(district, "(?<=- ).*$"))
      )
  }

  block_data <- freq_adm |>
    dplyr::mutate(smc_yn = dplyr::if_else(adm2 %in% eligible, 1L, 0L)) |>
    dplyr::filter(smc_yn == 1L) |>
    dplyr::arrange(adm1, adm2, duration, block_freq, median_max_prop) |>
    dplyr::group_by(adm1, adm2, duration) |>
    dplyr::mutate(
      mostfreq = dplyr::if_else(dplyr::row_number() == dplyr::n(), 1L, NA_integer_)
    ) |>
    dplyr::ungroup() |>
    dplyr::filter(mostfreq == 1L) |>
    dplyr::mutate(
      month1      = stringr::str_sub(block, 1, 3),
      firmonth    = month_lkp[month1],
      median_cats = .categorise_median_prop(median_max_prop)
    )

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
    "4" = "60-70%", "5" = "70-80%", "6" = "80-100%"
  )

  durations     <- sort(unique(block_data$duration))
  timing_maps   <- list()
  coverage_maps <- list()

  for (dur in durations) {
    dur_data <- block_data |> dplyr::filter(duration == dur)
    map_data <- adm2_sp |>
      dplyr::left_join(dur_data, by = "adm2") |>
      dplyr::mutate(
        firmonth_f    = factor(as.character(firmonth),    levels = as.character(4:9)),
        median_cats_f = factor(as.character(median_cats), levels = as.character(1:6))
      )

    present_months <- as.character(sort(unique(stats::na.omit(map_data$firmonth))))
    tm <- .build_snt_map(
      adm2_sf     = map_data,
      fill_col    = "firmonth_f",
      fill_values = month_pal[present_months],
      fill_labels = month_lbl[present_months],
      adm1_sf     = adm1_sp,
      title       = glue::glue("Ideal first month {dur} cycles ({type})"),
      fill_label  = "First Month"
    )
    timing_maps[[as.character(dur)]] <- tm
    .save_map(tm, file.path(smc_fig_dir, glue::glue("{type}_block_{dur}m.png")),
              dims, dpi)

    present_cats <- as.character(sort(unique(stats::na.omit(map_data$median_cats))))
    cm <- .build_snt_map(
      adm2_sf     = map_data,
      fill_col    = "median_cats_f",
      fill_values = cov_pal[present_cats],
      fill_labels = cov_lbl[present_cats],
      adm1_sf     = adm1_sp,
      title       = glue::glue("% of {type} covered {dur}-month window"),
      fill_label  = "% Covered"
    )
    coverage_maps[[as.character(dur)]] <- cm
    .save_map(cm, file.path(smc_fig_dir, glue::glue("{type}_prop_{dur}m.png")),
              dims, dpi)

    cli::cli_alert_success(
      "SMC maps saved: {dur}-month window \u2192 {.path { .relative_path(smc_fig_dir)}}"
    )
  }

  cli::cli_alert_info("Computing minimum best blocks...")

  min_summary <- block_data |>
    dplyr::group_by(adm1, adm2) |>
    dplyr::mutate(
      max_prop = max(median_max_prop, na.rm = TRUE),
      rule     = dplyr::case_when(
        max_prop <  60 ~ 1L,
        max_prop >= 60 ~ 2L,
        TRUE           ~ NA_integer_
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::group_by(adm1, adm2) |>
    dplyr::mutate(
      minimum_best = dplyr::case_when(
        rule == 1L & median_max_prop == max_prop ~ 1L,
        TRUE ~ NA_integer_
      ),
      freq60 = dplyr::if_else(
        rule == 2L & median_max_prop >= 60,
        median_max_prop, NA_real_
      )
    ) |>
    dplyr::mutate(
      min_freq60 = suppressWarnings(min(freq60, na.rm = TRUE)),
      min_freq60 = dplyr::if_else(is.infinite(min_freq60), NA_real_, min_freq60),
      min_freq60 = dplyr::if_else(median_max_prop < 60, NA_real_, min_freq60)
    ) |>
    dplyr::arrange(min_freq60, freq60, .by_group = TRUE) |>
    dplyr::mutate(
      minimum_best = dplyr::if_else(
        is.na(minimum_best) & dplyr::row_number() == 1L &
          rule == 2L & !is.na(min_freq60),
        1L, minimum_best
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(median_cats = .categorise_median_prop(median_max_prop))

  tbl_sub <- file.path(tbl_root, .type_tbl_subdir(type, case_stratification))
  .ensure_dir(tbl_sub)

  export_min <- min_summary |>
    dplyr::select(adm1, adm2, duration, block, block_freq,
                  years, median_max_prop, minimum_best)

  sntutils::write_snt_data(
    obj          = list(
      data      = export_min,
      data_dict = sntutils::build_dictionary(data = export_min)
    ),
    path         = tbl_sub,
    data_name    = glue::glue("{iso3}_{type}_data_minimum_best_block"),
    file_formats = c("xlsx", "qs2"),
    include_date = write_opts$include_date,
    n_saved      = write_opts$n_saved
  )
  cli::cli_alert_success(
    "Minimum best block table saved \u2192 {.path { .relative_path(tbl_sub)}}"
  )

  min_final   <- min_summary |> dplyr::filter(minimum_best == 1L)
  min_spatial <- adm2_sp |>
    dplyr::left_join(min_final, by = "adm2") |>
    dplyr::mutate(
      firmonth_f    = factor(as.character(firmonth),    levels = as.character(4:9)),
      median_cats_f = factor(as.character(median_cats), levels = as.character(1:6))
    )

  dur_levels  <- sort(unique(stats::na.omit(min_final$duration)))
  min_spatial <- min_spatial |>
    dplyr::mutate(duration = factor(duration, levels = dur_levels))

  dur_colours <- c("2" = "#52b788ff", "3" = "#d8c974ff",
                   "4" = "#e25ae2ff", "5" = "#6e1e6eff")
  dur_labels  <- c(
    "2" = "2 months (3 cycles)", "3" = "3 months (4 cycles)",
    "4" = "4 months (5 cycles)", "5" = "5 months (6 cycles)"
  )
  dur_colours <- dur_colours[as.character(dur_levels)]
  dur_labels  <- dur_labels[as.character(dur_levels)]

  type_label_min <- if (type == "rainfall") "Rainfall" else "Cases"

  map_min_duration <- .build_snt_map(
    adm2_sf     = min_spatial,
    fill_col    = "duration",
    fill_values = dur_colours,
    fill_labels = dur_labels,
    adm1_sf     = adm1_sp,
    title       = glue::glue("Minimum months to cover ~60% of {type_label_min}"),
    subtitle    = "Minimum SMC cycles required per district",
    fill_label  = "Duration"
  )
  .save_map(map_min_duration,
            file.path(smc_fig_dir, glue::glue("{type}_cycles_minimum.png")),
            dims, dpi)

  present_min_months <- as.character(sort(unique(stats::na.omit(min_final$firmonth))))
  map_min_firstmonth <- .build_snt_map(
    adm2_sf     = min_spatial,
    fill_col    = "firmonth_f",
    fill_values = month_pal[present_min_months],
    fill_labels = month_lbl[present_min_months],
    adm1_sf     = adm1_sp,
    title       = "First month for the minimum number of cycles",
    subtitle    = glue::glue("Based on {tolower(type_label_min)}"),
    fill_label  = "First Month"
  )
  .save_map(map_min_firstmonth,
            file.path(smc_fig_dir, glue::glue("{type}_block_minimum.png")),
            dims, dpi)

  present_min_cats <- as.character(sort(unique(stats::na.omit(min_final$median_cats))))
  map_min_coverage <- .build_snt_map(
    adm2_sf     = min_spatial,
    fill_col    = "median_cats_f",
    fill_values = cov_pal[present_min_cats],
    fill_labels = cov_lbl[present_min_cats],
    adm1_sf     = adm1_sp,
    title       = glue::glue("% of {type_label_min} Covered (minimum block)"),
    subtitle    = "Median proportion captured by the minimum best block",
    fill_label  = "% Covered"
  )
  .save_map(map_min_coverage,
            file.path(smc_fig_dir, glue::glue("{type}_prop_minimum.png")),
            dims, dpi)

  cli::cli_alert_success(
    "Minimum best block maps saved \u2192 {.path { .relative_path(smc_fig_dir)}}"
  )

  invisible(list(
    timing_maps    = timing_maps,
    coverage_maps  = coverage_maps,
    eligible       = eligible,
    min_best_table = export_min,
    min_best_maps  = list(
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
    crude_s_date = NULL, crude_e_date = NULL,
    s_year, e_year,
    panels_per_row,
    fig_root, dpi
) {
  median_dir <- file.path(fig_root, "cas_v_pluie")
  crude_dir  <- file.path(fig_root, "cas_v_pluie_crude")
  .ensure_dir(median_dir)
  .ensure_dir(crude_dir)

  # ── resolve date filters ─────────────────────────────────────────────────────

  s_date_resolved <- .resolve_date(case_rain_s_date, s_year, "start")
  e_date_resolved <- .resolve_date(case_rain_e_date, e_year, "end")

  # ── load and validate raw data ───────────────────────────────────────────────

  case_raw <- sntutils::read_snt_data(
    dirs$dhis2, glue::glue("{iso3}_dhis2_processed"), "xlsx"
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
    dirs$climate, glue::glue("{iso3}_rainfall_processed"), "xlsx"
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

  # ── aggregate ────────────────────────────────────────────────────────────────

  case_df <- case_raw |>
    dplyr::filter(dplyr::if_all(
      dplyr::all_of(c(case_var_ov5, case_var_u5)), ~ !is.na(.)
    )) |>
    dplyr::select(dplyr::all_of(required_case)) |>
    dplyr::rename(
      adm1 = !!adm1_var, adm2 = !!adm2_var,
      year = !!year_var, month = !!month_var,
      conf_ov5 = !!case_var_ov5, conf_u5 = !!case_var_u5
    ) |>
    dplyr::group_by(adm1, adm2, year, month) |>
    dplyr::summarise(
      conf_ov5 = sum(conf_ov5, na.rm = TRUE),
      conf_u5  = sum(conf_u5,  na.rm = TRUE),
      .groups  = "drop"
    ) |>
    dplyr::mutate(date = as.Date(sprintf("%04d-%02d-01", year, month)))

  # Aggregate rainfall to adm/year/month WITHOUT any date filter yet.
  # The median window and the crude window are independent — applying one
  # filter before the other would discard rain rows the second path needs.
  rain_df_full <- rain_raw |>
    dplyr::select(dplyr::all_of(required_rain)) |>
    dplyr::rename(
      adm1 = !!adm1_var, adm2 = !!adm2_var,
      year = !!year_var, month = !!month_var,
      total_rainfall_mm = !!rainfall_total_var
    ) |>
    dplyr::group_by(adm1, adm2, year, month) |>
    dplyr::summarise(
      total_rainfall_mm = sum(total_rainfall_mm, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(date = as.Date(sprintf("%04d-%02d-01", year, month)))

  # ── median plot data ─────────────────────────────────────────────────────────
  # Apply the shared case_rain date window to rainfall for the median path.

  rain_median <- rain_df_full
  if (!is.na(s_date_resolved)) rain_median <- rain_median |> dplyr::filter(date >= s_date_resolved)
  if (!is.na(e_date_resolved)) rain_median <- rain_median |> dplyr::filter(date <= e_date_resolved)

  combined_raw <- rain_median |>
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

  # ── crude plot data ──────────────────────────────────────────────────────────
  # crude_s/e_date are independent of case_rain_s/e_date.  When the user
  # supplies crude_s_date / crude_e_date (e.g. because cases start in 2020
  # while rainfall starts in 2014), those values take full precedence.
  # Only fall back to the shared case_rain dates if the crude-specific ones
  # were not provided at all — never chain through the already-filtered copy.

  crude_s_resolved <- .resolve_date(crude_s_date, NULL, "start")
  if (is.na(crude_s_resolved))
    crude_s_resolved <- .resolve_date(case_rain_s_date, s_year, "start")

  crude_e_resolved <- .resolve_date(crude_e_date, NULL, "end")
  if (is.na(crude_e_resolved))
    crude_e_resolved <- .resolve_date(case_rain_e_date, e_year, "end")

  # Both series filtered from the full (unsliced) data independently
  crude_rain <- rain_df_full
  if (!is.na(crude_s_resolved)) crude_rain <- crude_rain |> dplyr::filter(date >= crude_s_resolved)
  if (!is.na(crude_e_resolved)) crude_rain <- crude_rain |> dplyr::filter(date <= crude_e_resolved)

  case_crude <- case_df
  if (!is.na(crude_s_resolved)) case_crude <- case_crude |> dplyr::filter(date >= crude_s_resolved)
  if (!is.na(crude_e_resolved)) case_crude <- case_crude |> dplyr::filter(date <= crude_e_resolved)

  combined_crude <- dplyr::full_join(
    crude_rain |> dplyr::select(adm1, adm2, date, total_rainfall_mm),
    case_crude |> dplyr::select(adm1, adm2, date, conf_ov5, conf_u5),
    by = c("adm1", "adm2", "date")
  ) |>
    dplyr::filter(!is.na(date))

  # ── generate plots ───────────────────────────────────────────────────────────

  adm1_regions <- sort(unique(df_median$adm1))

  median_plots <- purrr::map(purrr::set_names(adm1_regions), function(region) {
    n_adm2 <- dplyr::n_distinct(df_median$adm2[df_median$adm1 == region])
    ppr    <- .adapt_panels_per_row(n_adm2, panels_per_row)
    p      <- .build_median_adm1_plot(df_median, region, ppr)
    n_rows <- ceiling(n_adm2 / ppr)

    ggplot2::ggsave(
      filename  = glue::glue("Cases_v_rainfall_median_{region}.png"),
      plot      = p, path = median_dir,
      width = ppr * 5, height = 3.5 * n_rows + 1.5,
      dpi = dpi, bg = "white", limitsize = FALSE
    )
    sntutils::compress_png(
      path = file.path(median_dir, glue::glue("Cases_v_rainfall_median_{region}.png"))
    )
    cli::cli_alert_success("Median plot saved: {.val {region}}")
    p
  })

  adm1_crude <- sort(unique(combined_crude$adm1))

  if (length(adm1_crude) == 0) {
    cli::cli_warn(c(
      "No crude plots produced \u2014 combined_crude is empty.",
      "i" = "Check that {.arg crude_s_date}/{.arg crude_e_date} overlap with both datasets.",
      "i" = "Rainfall runs {.val {format(min(rain_df_full$date), '%Y-%m')}} \u2013 {.val {format(max(rain_df_full$date), '%Y-%m')}}.",
      "i" = "Cases run {.val {format(min(case_df$date), '%Y-%m')}} \u2013 {.val {format(max(case_df$date), '%Y-%m')}}."
    ))
  }

  crude_plots <- purrr::map(purrr::set_names(adm1_crude), function(region) {
    region_data <- combined_crude |> dplyr::filter(adm1 == region)

    # Warn if case columns are entirely NA for this region in this window —
    # this happens when crude_s_date predates the start of case reporting.
    cases_in_window <- any(!is.na(region_data$conf_ov5) | !is.na(region_data$conf_u5))
    if (!cases_in_window) {
      cli::cli_warn(c(
        "Crude plot for {.val {region}}: no case data in the selected date window.",
        "i" = "Only rainfall will be visible. Adjust {.arg crude_s_date} if needed."
      ))
    }
    n_adm2 <- dplyr::n_distinct(combined_crude$adm2[combined_crude$adm1 == region])
    ppr    <- .adapt_panels_per_row(n_adm2, panels_per_row)
    p      <- .build_crude_adm1_plot(combined_crude, region, ppr)
    n_rows <- ceiling(n_adm2 / ppr)

    ggplot2::ggsave(
      filename  = glue::glue("Cases_v_rainfall_crude_{region}.png"),
      plot      = p, path = crude_dir,
      width = ppr * 5, height = 3.5 * n_rows + 1.5,
      dpi = dpi, bg = "white", limitsize = FALSE
    )
    sntutils::compress_png(
      path = file.path(crude_dir, glue::glue("Cases_v_rainfall_crude_{region}.png"))
    )
    cli::cli_alert_success("Crude plot saved: {.val {region}}")
    p
  })

  invisible(list(median = median_plots, crude = crude_plots))
}


# ==============================================================================
# INTERNAL HELPERS — shared aesthetics
# ==============================================================================

#' Shared minimal ggplot theme for line/facet plots
#' @keywords internal
.snt_theme_minimal <- function(base_size = 12) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      plot.title       = ggplot2::element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle    = ggplot2::element_text(size = 11, hjust = 0.5, color = "gray30"),
      strip.text       = ggplot2::element_text(size = 10, face = "bold"),
      legend.position  = "bottom",
      panel.grid.minor = ggplot2::element_blank(),
      axis.text.x      = ggplot2::element_text(angle = 45, hjust = 1)
    )
}

#' \code{theme_void()} base, bottom
#' horizontal legend, strip text with breathing room, tight plot margins.
#'
#' @param title_size,subtitle_size,legend_title_size Font sizes.
#' @keywords internal
.snt_theme_map <- function(title_size = 18, subtitle_size = 14,
                           legend_title_size = 16, legend_text_size = 14) {
  ggplot2::theme_void() +
    ggplot2::theme(
      # ── titles ───────────────────────────────────────────────────────────────
      plot.title    = ggplot2::element_text(
        size   = title_size,
        face   = "bold",
        hjust  = 0,
        margin = ggplot2::margin(b = 8)
      ),
      plot.subtitle = ggplot2::element_text(
        size   = subtitle_size,
        hjust  = 0,
        margin = ggplot2::margin(b = 10)
      ),
      plot.caption  = ggplot2::element_text(
        size   = 8,
        hjust  = 1,
        color  = "grey50",
        margin = ggplot2::margin(t = 6)
      ),
      # ── legend ───────────────────────────────────────────────────────────────
      legend.position      = "bottom",
      legend.direction     = "horizontal",
      legend.title         = ggplot2::element_text(
        size   = legend_title_size,
        face   = "bold",
        margin = ggplot2::margin(b = 6)
      ),
      legend.text          = ggplot2::element_text(size = legend_text_size),
      legend.box.margin    = ggplot2::margin(t = 10),
      # ── facet strips (for any multi-panel map) ───────────────────────────────
      strip.text   = ggplot2::element_text(
        face   = "bold",
        size   = 10,
        margin = ggplot2::margin(t = 2, b = 6, l = 4, r = 4)
      ),
      strip.text.y = ggplot2::element_text(angle = -90),
      panel.spacing = grid::unit(4, "pt"),
      # ── outer margin ─────────────────────────────────────────────────────────
      plot.margin = ggplot2::margin(t = 5, r = 5, b = 5, l = 5)
    )
}


#' Derive map output dimensions from a country's bounding box
#'
#' Reads the bounding box of an \code{sf} object and returns \code{width} and
#' \code{height} in inches scaled so that the longer axis is capped at
#' \code{max_long_side} and the shorter axis reflects the true aspect ratio.
#' Replaces hardcoded \code{width = 10, height = 8} across all map saves.
#'
#' @param shp An \code{sf} object. Only the bounding box is used.
#' @param max_long_side Numeric. Maximum inches for the longer axis. Default \code{11}.
#' @param min_inches Numeric. Minimum inches for either axis. Default \code{5}.
#' @param extra_height Numeric. Extra inches added to height for the bottom
#'   legend and title. Default \code{1.8}.
#' @return Named numeric vector with elements \code{width} and \code{height}.
#' @keywords internal
.compute_map_dims <- function(shp, max_long_side = 11,
                              min_inches = 5, extra_height = 1.8) {
  bb      <- sf::st_bbox(shp)
  lon_ext <- as.numeric(bb["xmax"] - bb["xmin"])
  lat_ext <- as.numeric(bb["ymax"] - bb["ymin"])

  if (lon_ext <= 0 || lat_ext <= 0) return(c(width = 10, height = 8))

  aspect <- lon_ext / lat_ext

  if (aspect >= 1) {
    width  <- max_long_side
    height <- max_long_side / aspect
  } else {
    height <- max_long_side
    width  <- max_long_side * aspect
  }

  width  <- max(min_inches, width)
  height <- max(min_inches, height) + extra_height

  c(width = round(width, 1), height = round(height, 1))
}


#' Build a single SNT choropleth map (company standard)
#'
#' Central factory used by \code{.build_seasonality_maps()} and
#' \code{.run_smc_maps_step()}. 
#' \code{theme_void()} base, white district borders at \code{linewidth = 0.15},
#' \code{inherit.aes = FALSE} on the ADM1 overlay, and a bottom horizontal
#' legend via \code{guide_legend(nrow = 1, label.position = "bottom")}.
#'
#' @param adm2_sf  \code{sf} polygon object at ADM2 level containing the fill
#'   variable.
#' @param fill_col Character. Column in \code{adm2_sf} mapped to fill.
#'   Must already be a factor whose levels match \code{fill_values}.
#' @param fill_values Named character vector of colours.
#' @param fill_labels Named character vector of legend labels, or \code{NULL}
#'   to use the names of \code{fill_values}.
#' @param adm1_sf Optional \code{sf} polygon object overlaid as province
#'   outlines.
#' @param title,subtitle,caption Optional character scalars.
#' @param fill_label Character. Legend title. Default \code{NULL}.
#' @param na_value Colour for \code{NA} districts. Default \code{"grey92"}.
#' @return A \code{ggplot} object.
#' @keywords internal
.build_snt_map <- function(adm2_sf,
                           fill_col,
                           fill_values,
                           fill_labels   = NULL,
                           adm1_sf       = NULL,
                           title         = NULL,
                           subtitle      = NULL,
                           caption       = NULL,
                           fill_label    = NULL,
                           na_value      = "grey92") {

  p <- ggplot2::ggplot(adm2_sf) +
    ggplot2::geom_sf(
      ggplot2::aes(fill = .data[[fill_col]]),
      color     = "white",
      linewidth = 0.15,
      na.rm     = TRUE
    )

  if (!is.null(adm1_sf)) {
    p <- p +
      ggplot2::geom_sf(
        data        = adm1_sf,
        fill        = NA,
        color       = "black",
        linewidth   = 0.4,
        inherit.aes = FALSE
      )
  }

  p +
    ggplot2::scale_fill_manual(
      values   = fill_values,
      labels   = fill_labels %||% names(fill_values),
      na.value = na_value,
      name     = fill_label,
      drop     = FALSE,
      guide    = ggplot2::guide_legend(
        label.position = "bottom",
        title.position = "top",
        title.hjust    = 0.5,
        override.aes   = list(color = "grey40", linewidth = 0.3),
        nrow           = 1,
        byrow          = TRUE
      )
    ) +
    ggplot2::coord_sf(datum = NA) +
    ggplot2::labs(title = title, subtitle = subtitle, caption = caption) +
    .snt_theme_map(title_size = 18, subtitle_size = 14,
      legend_title_size = 16, legend_text_size = 14)
}


#' Save a map with responsive dimensions
#'
#' Thin wrapper around \code{ggplot2::ggsave()} that accepts the output of
#' \code{.compute_map_dims()} and logs a clean relative path.
#'
#' @param plot A \code{ggplot} object.
#' @param path Full file path including filename and extension.
#' @param dims Named numeric vector from \code{.compute_map_dims()}.
#' @param dpi Numeric. Resolution. Default \code{300}.
#' @keywords internal
.save_map <- function(plot, path, dims, dpi = 300) {
  ggplot2::ggsave(
    filename  = path,
    plot      = plot,
    width     = dims[["width"]],
    height    = dims[["height"]],
    dpi       = dpi,
    limitsize = FALSE
  )
  sntutils::compress_png(
    path = path
  )
  cli::cli_alert_success("Map saved \u2192 {.path { .relative_path(path)}}")
  invisible(path)
}

#' Categorise median_max_prop into 6 coverage bands
#' Extracted as a helper so the same logic isn't copy-pasted in two places.
#' @keywords internal
.categorise_median_prop <- function(x) {
  dplyr::case_when(
    x <  40                    ~ 1L,
    x >= 40 & x <  50          ~ 2L,
    x >= 50 & x <  60          ~ 3L,
    x >= 60 & x <  70          ~ 4L,
    x >= 70 & x <  80          ~ 5L,
    x >= 80 & x <= 100         ~ 6L,
    TRUE                       ~ NA_integer_
  )
}

#' Adapt panels_per_row to district count
#' Mirrors MBG pipeline's repeated case_when logic, extracted to a helper.
#' @keywords internal
.adapt_panels_per_row <- function(n_adm2, default_ppr) {
  dplyr::case_when(
    n_adm2 >= 30 ~ 6L,
    n_adm2 >= 15 ~ 5L,
    n_adm2 >= 9  ~ 4L,
    TRUE         ~ as.integer(default_ppr)
  )
}


# ==============================================================================
# INTERNAL HELPERS — case vs rainfall plot builders
# ==============================================================================

#' @keywords internal
.build_median_adm2_plot <- function(data, adm2_name,
                                    show_y_left = TRUE, show_y_right = TRUE) {
  d             <- data |> dplyr::filter(adm2 == adm2_name)
  max_cases     <- max(c(d$conf_ov5, d$conf_u5), na.rm = TRUE)
  max_rain      <- max(d$total_rainfall_mm, na.rm = TRUE)
  scale_factor  <- if (max_rain > 0) max_cases / max_rain else 1

  ggplot2::ggplot(d, ggplot2::aes(x = month)) +
    ggplot2::geom_line(ggplot2::aes(y = total_rainfall_mm * scale_factor,
                                    color = "Pluie"), linewidth = 1.2) +
    ggplot2::geom_line(ggplot2::aes(y = conf_ov5, color = "Cas >5 ans"), linewidth = 1.2) +
    ggplot2::geom_line(ggplot2::aes(y = conf_u5,  color = "Cas <5 ans"), linewidth = 1.2) +
    ggplot2::scale_y_continuous(
      name = if (show_y_left) "Cas confirm\u00e9s (m\u00e9diane)" else NULL,
      labels = scales::comma,
      sec.axis = ggplot2::sec_axis(
        transform = ~ . / scale_factor,
        name = if (show_y_right) "Pluie (mm)" else NULL,
        labels = scales::comma
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
      panel.border       = ggplot2::element_rect(color = "gray80", fill = NA, linewidth = 0.5),
      axis.title.y.left  = if (show_y_left)  ggplot2::element_text(color = "black", size = 12)
                           else               ggplot2::element_blank(),
      axis.title.y.right = if (show_y_right) ggplot2::element_text(color = "black", size = 12)
                           else               ggplot2::element_blank(),
      axis.text          = ggplot2::element_text(size = 10, face = "bold")
    )
}

#' @keywords internal
.build_median_adm1_plot <- function(data, adm1_name, panels_per_row = 3) {
  adm1_data    <- data |> dplyr::filter(adm1 == adm1_name)
  adm2_regions <- sort(unique(adm1_data$adm2))
  n            <- length(adm2_regions)

  panel_list <- purrr::imap(
    adm2_regions,
    \(adm2_name, idx) .build_median_adm2_plot(
      data         = adm1_data,
      adm2_name    = adm2_name,
      show_y_left  = (idx %% panels_per_row == 1),
      show_y_right = (idx %% panels_per_row == 0 | idx == n)
    )
  )

  legend_plot   <- .build_median_adm2_plot(adm1_data, adm2_regions[1]) +
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

#' @keywords internal
.build_crude_adm2_plot <- function(data, adm2_name,
                                   show_y_left = TRUE, show_y_right = TRUE) {
  d            <- data |> dplyr::filter(adm2 == adm2_name)
  max_cases    <- suppressWarnings(max(c(d$conf_ov5, d$conf_u5), na.rm = TRUE))
  max_rain     <- suppressWarnings(max(d$total_rainfall_mm,       na.rm = TRUE))

  # Guard: if case data is entirely absent (crude date window predates case
  # reporting), scale_factor would be -Inf / max_rain which silently corrupts
  # the dual-axis and produces a blank plot.  Fall back to rainfall-only.
  cases_present <- is.finite(max_cases) && max_cases > 0
  scale_factor  <- if (is.finite(max_rain) && max_rain > 0 && cases_present) {
    max_cases / max_rain
  } else if (is.finite(max_rain) && max_rain > 0) {
    1   # rainfall only — cases not yet available in this window
  } else {
    1
  }

  ggplot2::ggplot(d, ggplot2::aes(x = date)) +
    ggplot2::geom_line(ggplot2::aes(y = total_rainfall_mm * scale_factor,
                                    color = "Pluie"), linewidth = 1.0, na.rm = TRUE) +
    ggplot2::geom_line(ggplot2::aes(y = conf_ov5, color = "Cas >5 ans"),
                       linewidth = 1.0, na.rm = TRUE) +
    ggplot2::geom_line(ggplot2::aes(y = conf_u5,  color = "Cas <5 ans"),
                       linewidth = 1.0, na.rm = TRUE) +
    ggplot2::scale_y_continuous(
      name = if (show_y_left) "Cas confirm\u00e9s" else NULL,
      labels = scales::comma,
      sec.axis = ggplot2::sec_axis(
        transform = ~ . / scale_factor,
        name = if (show_y_right) "Pluie (mm)" else NULL,
        labels = scales::comma
      )
    ) +
    ggplot2::scale_x_date(
      date_breaks = "6 months", date_labels = "%b %Y",
      expand = ggplot2::expansion(mult = c(0, 0.01))
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
      panel.border       = ggplot2::element_rect(color = "gray80", fill = NA, linewidth = 0.5),
      axis.text.x        = ggplot2::element_text(angle = 45, hjust = 1, size = 10),
      axis.text.y        = ggplot2::element_text(size = 10),
      axis.title.y.left  = if (show_y_left)  ggplot2::element_text(color = "black", size = 13)
                           else               ggplot2::element_blank(),
      axis.title.y.right = if (show_y_right) ggplot2::element_text(color = "black", size = 13)
                           else               ggplot2::element_blank()
    )
}

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

#' Get a short project-relative path for CLI output
#' Mirrors MBG pipeline's .relative_path() helper for clean path reporting.
#' @keywords internal
.relative_path <- function(path) {
  path <- as.character(path)

  match <- regmatches(
    path,
    regexpr("(01_data|02_scripts|03_outputs|04_reports|05_metadata_docs)/.*$", path)
  )
  if (length(match) > 0 && nchar(match) > 0) return(match)

  rel <- tryCatch(
    as.character(fs::path_rel(path, start = getwd())),
    error = function(e) NULL
  )
  if (!is.null(rel) && !grepl("^\\.\\./\\.\\./\\.\\./", rel)) return(rel)

  basename(path)
}

#' Resolve a date scalar from explicit date string or year integer
#' Replaces repeated if/else chains in case_rain step.
#' @keywords internal
.resolve_date <- function(date_str, year_fallback, which = c("start", "end")) {
  which <- match.arg(which)
  if (!is.null(date_str)) return(as.Date(date_str))
  if (!is.null(year_fallback)) {
    suffix <- if (which == "start") "-01-01" else "-12-31"
    return(as.Date(sprintf("%04d%s", year_fallback, suffix)))
  }
  NA_real_
}

#' Resolve optional adm3 lookup table for block step
#' Mirrors the MBG pipeline's clean pattern of loading optional joins from
#' raw source files rather than scattering the logic across the step body.
#' @keywords internal
.resolve_adm3_lookup <- function(dirs, iso3, type, adm1_var, adm2_var, adm3_var) {
  if (is.null(adm3_var)) return(NULL)

  cfg_type <- switch(type,
    rainfall = list(folder = dirs$climate,
                    filename = glue::glue("{iso3}_rainfall_processed")),
    cases    = list(folder = dirs$dhis2,
                    filename = glue::glue("{iso3}_dhis2_processed"))
  )

  raw <- tryCatch(
    sntutils::read_snt_data(cfg_type$folder, cfg_type$filename, "xlsx"),
    error = function(e) {
      cli::cli_warn("Could not load source file for adm3 lookup: {e$message}")
      return(NULL)
    }
  )
  if (is.null(raw)) return(NULL)

  if (!all(c(adm3_var, adm1_var, adm2_var) %in% names(raw))) {
    cli::cli_warn(c(
      "{.arg adm3_var} ({.val {adm3_var}}) or parent columns not found in {.val {type}} source data.",
      "i" = "Proceeding without adm3."
    ))
    return(NULL)
  }

  raw |>
    dplyr::select(dplyr::all_of(c(adm1_var, adm2_var, adm3_var))) |>
    dplyr::rename(adm1 = !!adm1_var, adm2 = !!adm2_var, adm3 = !!adm3_var) |>
    dplyr::distinct() |>
    dplyr::mutate(district = paste(adm1, adm2, sep = " - "))
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

#' NULL coalescing operator
#' @keywords internal
`%||%` <- function(x, y) if (!is.null(x)) x else y

#' Load and aggregate raw source data to adm/year/month level
#' @keywords internal
.load_analysis_data <- function(
    dirs, iso3, type, adm1_var, adm2_var, year_var, month_var, value_var
) {
  cfg <- switch(type,
    rainfall = list(
      folder   = dirs$climate,
      filename = glue::glue("{iso3}_rainfall_processed"),
      col_src  = value_var %||% "mean_rainfall_mm"
    ),
    cases = list(
      folder   = dirs$dhis2,
      filename = glue::glue("{iso3}_dhis2_processed"),
      col_src  = value_var %||% "conf"
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
.get_or_read_detailed <- function(in_memory, tbl_root, iso3, type, strat = "ov5") {
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
.get_or_read_block_freq <- function(in_memory, tbl_root, iso3, type, strat = "ov5") {
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
# INTERNAL HELPERS — seasonality analysis (unchanged logic, cleaner structure)
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

  # ── responsive dimensions — one bbox read, reused for every map ──────────────
  dims <- .compute_map_dims(adm2_sp)

  # ── Map 1: Seasonal years count ──────────────────────────────────────────────

  merged$category <- factor(
    as.character(tidyr::replace_na(merged$SeasonalYears, 0)),
    levels = as.character(0:11)
  )

  cat_pal <- c(
    "0"  = "#ffffff", "1"  = "#ffeda0", "2"  = "#fed976", "3"  = "#feb24c",
    "4"  = "#fd8d3c", "5"  = "#fc4e2a", "6"  = "#e31a1c", "7"  = "#bd0026",
    "8"  = "#800026", "9"  = "#67001f", "10" = "#4d0016", "11" = "#33000d"
  )
  cat_n   <- table(merged$category)
  cat_n   <- cat_n[cat_n > 0]
  cat_pal <- cat_pal[names(cat_pal) %in% names(cat_n)]
  cat_lbl <- stats::setNames(
    paste0(names(cat_n), " year",
           ifelse(as.integer(names(cat_n)) != 1, "s", ""),
           " (n=", cat_n, ")"),
    names(cat_n)
  )

  p1 <- .build_snt_map(
    adm2_sf     = merged,
    fill_col    = "category",
    fill_values = cat_pal,
    fill_labels = cat_lbl,
    adm1_sf     = adm1_sp,
    title       = glue::glue("Seasonality of {type_label} in {adm0_name}"),
    subtitle    = glue::glue(
      "Number of years with seasonal peaks eligible for SMC ({yr_range})"
    ),
    fill_label  = glue::glue("Years with seasonal {tolower(type_label)} peaks")
  )
  .save_map(
    p1,
    file.path(fig_dir, glue::glue("{iso3}_{type}_seasonality_years_map.png")),
    dims, dpi
  )

  # ── Map 2: Binary Seasonal / Not Seasonal ────────────────────────────────────

  merged$Seasonality <- factor(merged$Seasonality,
                               levels = c("Seasonal", "Not Seasonal"))
  cls_pal <- c("Seasonal" = "#2d8659", "Not Seasonal" = "#f7f7f7")
  cls_n   <- table(merged$Seasonality)
  cls_n   <- cls_n[cls_n > 0]
  cls_lbl <- stats::setNames(
    c(paste0("SMC Seasonality (n=", cls_n["Seasonal"],     ")"),
      paste0("Non-Seasonal (n=",    cls_n["Not Seasonal"], ")")
    )[seq_along(cls_n)],
    names(cls_n)
  )

  p2 <- .build_snt_map(
    adm2_sf     = merged,
    fill_col    = "Seasonality",
    fill_values = cls_pal[names(cls_pal) %in% names(cls_n)],
    fill_labels = cls_lbl,
    adm1_sf     = adm1_sp,
    title       = glue::glue("Malaria Seasonality Classification in {adm0_name}"),
    subtitle    = glue::glue("Based on {tolower(type_label)} ({yr_range})")
  )
  .save_map(
    p2,
    file.path(fig_dir, glue::glue("{iso3}_{type}_seasonality_classification_map.png")),
    dims, dpi
  )

  # ── Maps 3+: Cumulative threshold maps ───────────────────────────────────────

  if (is.null(cumulative_thresholds)) {
    if (n_cumulative_maps >= n_years) {
      cli::cli_warn(c(
        "{.arg n_cumulative_maps} ({n_cumulative_maps}) \u2265 data span ({n_years} years).",
        "i" = "Reducing to {n_years - 1} cumulative map{?s}."
      ))
      n_cumulative_maps <- n_years - 1
    }
    top_year              <- n_years
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
    safe_label   <- gsub("_+", "_", gsub("[^0-9]", "_", thresh_label))
    safe_label   <- gsub("^_|_$", "", safe_label)

    merged_thresh <- merged |>
      dplyr::mutate(
        Seasonality = factor(
          dplyr::if_else(SeasonalYears %in% thresh_int, "Seasonal", "Not Seasonal"),
          levels = c("Seasonal", "Not Seasonal")
        )
      )

    t_n   <- table(merged_thresh$Seasonality)
    t_n   <- t_n[t_n > 0]
    t_pal <- colors_cumul[names(colors_cumul) %in% names(t_n)]
    t_lbl <- stats::setNames(
      c(paste0("SMC Seasonality (n=", t_n["Seasonal"],     ")"),
        paste0("Non-Seasonal (n=",    t_n["Not Seasonal"], ")")
      )[seq_along(t_n)],
      names(t_n)
    )

    p_cumul <- .build_snt_map(
      adm2_sf     = merged_thresh,
      fill_col    = "Seasonality",
      fill_values = t_pal,
      fill_labels = t_lbl,
      adm1_sf     = adm1_sp,
      title       = glue::glue("Malaria Seasonality {thresh_label} years"),
      subtitle    = glue::glue("Based on {tolower(type_label)} ({yr_range})")
    )

    out_file <- file.path(
      fig_dir,
      glue::glue("{iso3}_{type}_seasonality_{safe_label}.png")
    )
    .save_map(p_cumul, out_file, dims, dpi)

    cumul_maps[[thresh_label]] <- p_cumul
  }

  cli::cli_alert_success(
    "All seasonality maps saved \u2192 {.path { .relative_path(fig_dir)}}"
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
      b2  <- make_blocks(lkp, 4:10, 2)
      b3  <- make_blocks(lkp, 4:9, 3)
      b4  <- make_blocks(lkp, 4:9, 4)
      b5  <- make_blocks(lkp, 4:8, 5)

      v2  <- purrr::map_dbl(b2, "val")
      v3  <- purrr::map_dbl(b3, "val")
      v4  <- purrr::map_dbl(b4, "val")
      v5  <- purrr::map_dbl(b5, "val")
      n2  <- purrr::map_chr(b2, "name")
      n3  <- purrr::map_chr(b3, "name")
      n4  <- purrr::map_chr(b4, "name")
      n5  <- purrr::map_chr(b5, "name")

      summary <- rbind(summary, data.frame(
        year = yr, district = dist, adm1 = d$adm1[1], adm2 = d$adm2[1],
        total = total,
        max_2m = max(v2), max_3m = max(v3), max_4m = max(v4), max_5m = max(v5),
        pct_2m = max(v2)/total*100,
        pct_3m = max(v3)/total*100,
        pct_4m = max(v4)/total*100,
        pct_5m = max(v5)/total*100,
        max_2m_block = n2[which.max(v2)],
        max_3m_block = n3[which.max(v3)],
        max_4m_block = n4[which.max(v4)],
        max_5m_block = n5[which.max(v5)],
        stringsAsFactors = FALSE
      ))

      det_row <- data.frame(district = dist, years = yr, stringsAsFactors = FALSE)
      for (x in c(b2, b3, b4, b5)) det_row[[x$name]] <- x$val / total * 100
      det_row$max_2m       <- max(v2)/total*100
      det_row$max_3m       <- max(v3)/total*100
      det_row$max_4m       <- max(v4)/total*100
      det_row$max_5m       <- max(v5)/total*100
      det_row$max_2m_block <- n2[which.max(v2)]
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

    for (dur in c(2L, 3L, 4L, 5L)) {
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

  freq <- freq |> dplyr::arrange(district, duration, dplyr::desc(block_freq))

  adm_lookup <- summary_tbl |> dplyr::distinct(district, adm1, adm2)

  freq |>
    dplyr::left_join(adm_lookup, by = "district") |>
    dplyr::select(district, adm1, adm2, dplyr::everything())
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
# ==============================================================================

#' Generate a Seasonality Heatmap for Rainfall or Cases
#'
#' @description
#' Standalone heatmap function that can be called independently of the full
#' pipeline.  Produces a tile plot showing each month's value as a percentage
#' of that year's annual total, across all districts and years.  Districts are
#' arranged on the y-axis, time on the x-axis, and the fill colour encodes the
#' monthly concentration.
#'
#' This function is called internally by \code{run_seasonality_pipeline()} for
#' the \code{"heatmap"} step, but it is also useful for quickly checking data
#' coverage or producing a single heatmap without running the whole pipeline.
#'
#' @inheritParams run_seasonality_pipeline
#' @param type Character. Data type to visualise. One of \code{"rainfall"}
#'   (default) or \code{"cases"}.
#' @param value_var Character or \code{NULL}. Specific metric column to map to
#'   fill colour. Default: \code{NULL} (uses \code{"mean_rainfall_mm"} for
#'   rainfall, \code{"conf"} for cases).
#' @param drop_na_cols Character vector. Structural columns that must be
#'   non-\code{NA} for a row to be retained. Default:
#'   \code{c("adm1", "adm2", "year", "month")}.
#' @param drop_na_value Logical. Whether to also drop rows where the metric
#'   column itself is \code{NA}. Default: \code{TRUE}.
#' @param output_dir Character. Output folder when using Option C (direct
#'   paths). Ignored when \code{paths} or \code{base_path} is supplied.
#'   Default: \code{here::here("outputs", "figures")}.
#' @param filename Character or \code{NULL}. Output filename including
#'   extension. Auto-generated as \code{"{ISO3}_{type}_heatmap.png"} when
#'   \code{NULL}. Default: \code{NULL}.
#' @param width Numeric. Plot width in inches. Default: \code{12}.
#' @param height Numeric. Plot height in inches. Default: \code{8}.
#' @param dpi Numeric. Plot resolution. Default: \code{500}.
#'
#' @return A named list returned invisibly with three elements:
#'   \describe{
#'     \item{\code{plot}}{The ggplot object.}
#'     \item{\code{data}}{The prepared data frame with \code{prop_m} (monthly
#'       percentage) and \code{adm_label} columns.}
#'     \item{\code{output_path}}{Full file path where the PNG was saved.}
#'   }
#'
#' @examples
#' \dontrun{
#' paths <- sntutils::setup_project_paths()
#'
#' # Rainfall heatmap at district level
#' run_heatmap_analysis(
#'   iso3      = "gin",
#'   adm0_name = "Guinea",
#'   paths     = paths,
#'   type      = "rainfall"
#' )
#'
#' # Cases heatmap collapsed to region level
#' run_heatmap_analysis(
#'   iso3      = "gin",
#'   adm0_name = "Guinea",
#'   paths     = paths,
#'   type      = "cases",
#'   adm_level = "adm1",
#'   s_year    = 2018
#' )
#' }
#'
#' @export
run_heatmap_analysis <- function(
    iso3,
    adm0_name,
    type        = c("rainfall", "cases"),
    paths       = NULL,
    base_path   = NULL,
    climate_dir = NULL,
    dhis2_dir   = NULL,
    adm1_var    = "adm1",
    adm2_var    = "adm2",
    year_var    = "year",
    month_var   = "month",
    value_var   = NULL,
    s_year      = NULL,
    e_year      = NULL,
    adm_level      = c("adm2", "adm1"),
    drop_na_cols   = c("adm1", "adm2", "year", "month"),
    drop_na_value  = TRUE,
    viridis_option = "viridis",
    x_breaks_by    = 0.5,
    output_dir  = here::here("outputs", "figures"),
    filename    = NULL,
    width       = 12,
    height      = 8,
    dpi         = 500
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

  if (!is.null(s_year) && !is.null(e_year) && s_year > e_year) {
    cli::cli_abort(c(
      "{.arg s_year} must be \u2264 {.arg e_year}.",
      "x" = "Got s_year={.val {s_year}}, e_year={.val {e_year}}."
    ))
  }

  cli::cli_alert_info("Heatmap: {.val {adm0_name}} | {.val {type}}")

  dirs <- .resolve_dirs(
    paths         = paths,
    base_path     = base_path,
    climate_dir   = climate_dir,
    dhis2_dir     = dhis2_dir,
    admin_shp_dir = NULL
  )

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

  cli::cli_alert_success("Heatmap complete: {.val {adm0_name}} ({.val {type}})")
  invisible(list(plot = p, data = df_ready, output_path = out))
}

#' @keywords internal
.load_heatmap_data <- function(
    dirs, iso3, type, adm1_var, adm2_var, year_var, month_var,
    value_var, drop_na_cols, drop_na_value
) {
  cfg <- switch(type,
    rainfall = list(
      folder   = dirs$climate,
      filename = glue::glue("{iso3}_rainfall_processed"),
      col_src  = value_var %||% "mean_rainfall_mm"
    ),
    cases = list(
      folder   = dirs$dhis2,
      filename = glue::glue("{iso3}_dhis2_processed"),
      col_src  = value_var %||% "conf"
    )
  )

  df <- sntutils::read_snt_data(cfg$folder, cfg$filename, "xlsx")

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
    dplyr::select(dplyr::all_of(c(adm1_var, adm2_var, year_var, month_var, cfg$col_src))) |>
    dplyr::rename(
      adm1  = !!adm1_var,
      adm2  = !!adm2_var,
      year  = !!year_var,
      month = !!month_var
    )

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
      "x" = "Metric values must be \u2265 0."
    ))
  }

  n_before <- nrow(df)
  std_drop_cols <- drop_na_cols |>
    gsub(pattern = adm1_var,  replacement = "adm1") |>
    gsub(pattern = adm2_var,  replacement = "adm2") |>
    gsub(pattern = year_var,  replacement = "year") |>
    gsub(pattern = month_var, replacement = "month")

  valid_drop_cols <- intersect(std_drop_cols, names(df))
  df <- df |>
    dplyr::filter(dplyr::if_all(dplyr::all_of(valid_drop_cols), ~ !is.na(.x)))

  if (drop_na_value) df <- df |> dplyr::filter(!is.na(.data[[cfg$col_src]]))

  n_dropped <- n_before - nrow(df)
  if (n_dropped > 0) {
    cli::cli_warn(c(
      "Dropped {n_dropped} row{?s} containing NA value{?s}.",
      "i" = "{n_before} \u2192 {nrow(df)} rows remaining."
    ))
  }

  if (nrow(df) == 0) {
    cli::cli_abort(c(
      "No rows remain after NA removal.",
      "x" = "All {n_before} row{?s} were dropped.",
      "i" = "Check your source data or review {.arg drop_na_cols}."
    ))
  }

  cli::cli_alert_success("Loaded {nrow(df)} row{?s} from {.val {cfg$filename}}.")

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

#' @keywords internal
.prepare_heatmap_data <- function(df, adm_level = c("adm1", "adm2")) {
  adm_level <- match.arg(adm_level)

  group_cols <- c("adm1", "adm2", "year")

  annual_totals <- df |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
    dplyr::summarise(annual_total = sum(value, na.rm = TRUE), .groups = "drop")

  # join_cols <- setdiff(group_cols, "year")

  prepared <- df |>
    dplyr::left_join(annual_totals, by = group_cols) |>
    dplyr::mutate(
      yearmon   = zoo::as.yearmon(sprintf("%04d-%02d", year, month)),
      prop_m    = value / annual_total * 100,
      adm_label = if (adm_level == "adm2") paste(adm1, adm2, sep = " - ") else adm1,
      adm_label = factor(adm_label, levels = rev(sort(unique(adm_label))))
    ) |>
    dplyr::arrange(adm1, adm2)

  n_zero <- sum(annual_totals$annual_total == 0, na.rm = TRUE)
  if (n_zero > 0) {
    cli::cli_warn(c(
      "{n_zero} admin-year combination{?s} have an annual total of zero.",
      "i" = "Proportions for these units will be {.val NaN}."
    ))
  }

  cli::cli_alert_success(
    "Proportions computed: {dplyr::n_distinct(prepared$adm_label)} {adm_level} unit{?s}."
  )

  prepared
}

#' @keywords internal
.build_heatmap_plot <- function(df, adm0_name, type, adm_level,
                                viridis_option, x_breaks_by) {
  legend_label <- switch(type, rainfall = "% Rainfall", cases = "% Cases")
  title_label  <- switch(type,
    rainfall = glue::glue("{adm0_name}: Monthly Distribution of Rainfall"),
    cases    = glue::glue("{adm0_name}: Monthly Distribution of Cases")
  )
  y_label <- switch(adm_level, adm1 = "Region", adm2 = "District")

  ggplot2::ggplot(df, ggplot2::aes(x = yearmon, y = adm_label, fill = prop_m)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.5) +
    ggplot2::scale_fill_viridis_c(
      name   = legend_label,
      option = viridis_option,
      labels = scales::percent_format(scale = 1)
    ) +
    zoo::scale_x_yearmon(
      breaks = seq(min(df$yearmon), max(df$yearmon), by = x_breaks_by),
      format = "%b %Y"
    ) +
    ggplot2::labs(x = "Month", y = y_label, title = title_label) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      axis.text.y     = ggplot2::element_text(size = 6, hjust = 1, face = "bold"),
      axis.text.x     = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1),
      axis.title      = ggplot2::element_text(size = 11, face = "bold"),
      plot.title      = ggplot2::element_text(size = 13, face = "bold", hjust = 0.5),
      legend.title    = ggplot2::element_text(size = 10, face = "bold"),
      legend.text     = ggplot2::element_text(size = 9),
      panel.grid      = ggplot2::element_blank(),
      legend.position = "right",
      plot.margin     = ggplot2::margin(10, 10, 10, 10)
    )
}

#' @keywords internal
.save_heatmap_plot <- function(plot, output_dir, filename, iso3, type,
                               width, height, dpi) {
  if (is.null(filename)) {
    filename <- glue::glue("{toupper(iso3)}_{type}_heatmap.png")
  }
  .ensure_dir(output_dir)
  output_path <- file.path(output_dir, filename)
  ggplot2::ggsave(filename = output_path, plot = plot,
                  width = width, height = height, dpi = dpi)
  sntutils::compress_png(
    path = output_path
  )
  cli::cli_alert_success("Heatmap saved: {.path { .relative_path(output_path)}}")
  invisible(output_path)
}


# ==============================================================================
# Additional packages required for case_rain_plots step: patchwork, cowplot
# ==============================================================================