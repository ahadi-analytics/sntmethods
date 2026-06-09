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
#   figures → paths$val_fig/                          (heatmaps)
#           → paths$val_fig/rain_seas/                (rainfall seasonality maps + rainfall SMC maps)
#           → paths$val_fig/case_seas/                (cases seasonality maps)
#           → paths$val_fig/ov5/                      (cases SMC timing + coverage + minimum maps)
#           → paths$val_fig/case_rain_graphs/         (rolling % overlay graphs)
#           → paths$val_fig/case_v_rainfall/          (median cases vs rainfall)
#           → paths$val_fig/case_v_rainfall_crude/    (crude time-series cases vs rainfall)
#   tables  → paths$val_tbl/                          (seasonality summaries)
#           → paths$val_tbl/rainfall/                 (rainfall block tables)
#           → paths$val_tbl/cases_ov5_block/          (cases block tables)
#   cache   → paths$cache/                            ({iso3}_pipe_cache_{step}_{type}.qs2)
#             Stores per-step result objects so individual steps can be
#             re-run in a fresh R session without recomputing earlier steps.
#             Delete cache files manually to force a full recompute.
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
  # Step-specific optional packages, warn but don't abort yet;
  # hard abort happens inside the step if the package is truly missing.
  # sf is only needed for map-producing steps; blocks writes tables only.
  if (any(c("seasonality", "smc_maps") %in% steps)) {
    .warn_pkg("sf")
  }
  if (any(c("seasonality", "blocks", "smc_maps") %in% steps)) {
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
#' tables covering six analytical steps.  All analysis can be run at adm1,
#' adm2, or adm3 level via the \code{analysis_level} argument, making the
#' pipeline suitable for countries where sub-district granularity is required.
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
#' @section Output language:
#' By default all figure titles, subtitles, axis labels, and legend text are
#' produced in English (\code{lang = "en"}).  Set \code{lang} to any BCP-47
#' language code supported by Google Translate to produce outputs in that
#' language.  Translations are cached to \code{cache_path} so that re-runs
#' do not repeat API calls for strings already translated.  The default
#' \code{here::here("translation_cache")} is already a stable project
#' location; only change \code{cache_path} if you need the cache stored
#' elsewhere.
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
#' @param adm3_var Character or \code{NULL}. Name of the admin level 3 column
#'   in the source files. \strong{Required} when \code{analysis_level = "adm3"}.
#'   When \code{analysis_level = "adm2"} and \code{adm3_var} is supplied, adm3
#'   is joined as a label onto block output tables only. Default: \code{NULL}.
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
#'   \strong{Does not affect analytical outputs} — use \code{analysis_level}
#'   to control the grouping level of the analysis itself.
#' @param analysis_level Character. Administrative level at which all analytical
#'   steps (seasonality, blocks, overlay graphs, SMC maps, cases vs rainfall
#'   plots) are run. One of \code{"adm2"} (default), \code{"adm3"}, or
#'   \code{"adm1"}. When \code{"adm3"}, \code{adm3_var} must also be supplied.
#'   The heatmap is unaffected by this parameter — use \code{adm_level} for
#'   heatmap display granularity.
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
#' @param lang Character or \code{NULL}. BCP-47 language code for all figure
#'   text (titles, subtitles, axis labels, legend labels). Use \code{"en"} or
#'   \code{NULL} (default) for English, which skips translation entirely.
#'   Examples: \code{"fr"} (French), \code{"pt"} (Portuguese), \code{"ar"}
#'   (Arabic).  Requires an internet connection on first use; results are
#'   cached to \code{cache_path} so subsequent runs are instant.
#' @param graphs_s_year Integer or \code{NULL}. Step-4-only start year for the
#'   rolling overlay graphs.  When rainfall and case datasets cover different
#'   year ranges, use this to clip the overlay graphs independently of
#'   \code{s_year} (which applies to all other steps).  For example, if
#'   rainfall starts in 2013 but cases only from 2018, set
#'   \code{graphs_s_year = 2018} so the two lines begin at the same point.
#'   When set, the interactive alignment prompt (see \code{interactive_checks})
#'   is silently bypassed.  Default: \code{NULL} (uses \code{s_year}).
#' @param graphs_e_year Integer or \code{NULL}. Step-4-only end year.  Works
#'   identically to \code{graphs_s_year} but for the upper bound.
#'   Default: \code{NULL} (uses \code{e_year}).
#' @param use_pipeline_cache Logical. Set \code{TRUE} to enable automatic step
#'   caching, following the same pattern as \code{prep_geonames()}.  When
#'   \code{TRUE} the pipeline resolves the cache directory automatically from
#'   \code{paths$cache} (standard SNT project) — no path knowledge required.
#'   After each step the result is saved as a \code{.qs2} file; on the next
#'   run with the same \code{iso3} and \code{steps}, earlier steps are loaded
#'   from cache instead of being recomputed.  Cache files persist across R
#'   sessions; delete them manually to force a full recompute.  Default:
#'   \code{FALSE}.
#' @param pipeline_cache_dir Character or \code{NULL}. Advanced override: the
#'   directory where cache files are stored.  Under normal use you never need
#'   to set this — just use \code{use_pipeline_cache = TRUE}.  Useful when
#'   working outside a standard SNT project structure (Option C paths) and you
#'   want caching at a specific location.  Default: \code{NULL}.
#' @param interactive_checks Logical. When \code{TRUE} (default), steps 4 and 6
#'   detect whether the rainfall and case datasets cover different year ranges
#'   and — if running in an interactive R session — pause to show a formatted
#'   comparison table and prompt the user to choose how to proceed (auto-clip to
#'   the overlap, continue as-is, or enter custom bounds).  In a non-interactive
#'   context (e.g. \code{Rscript}, \code{rmarkdown::render()}) the check still
#'   runs but instead of prompting it emits a \code{cli::cli_warn()} message
#'   listing the mismatch and suggesting \code{graphs_s_year}/\code{graphs_e_year}.
#'   Set to \code{FALSE} to suppress all checks in scripted pipelines.
#' @param cache_path Character. Directory for the persistent translation cache
#'   used by \code{sntutils::translate_text()}.  Defaults to
#'   \code{here::here("translation_cache")}, which keeps translations across
#'   R sessions.  Use \code{tempdir()} if you prefer not to write to the
#'   project folder.
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
#' # ── adm3-level analysis (e.g. Rwanda) ───────────────────────────────────────
#' run_seasonality_pipeline(
#'   iso3                   = "rwa",
#'   adm0_name              = "Rwanda",
#'   paths                  = paths,
#'   max_non_seasonal_years = 2,
#'   analysis_level         = "adm3",
#'   adm3_var               = "adm3"
#' )
#'
#' # ── adm1-level analysis ──────────────────────────────────────────────────────
#' run_seasonality_pipeline(
#'   iso3                   = "gin",
#'   adm0_name              = "Guinea",
#'   paths                  = paths,
#'   max_non_seasonal_years = 2,
#'   analysis_level         = "adm1"
#' )
#'
#' # ── French outputs ───────────────────────────────────────────────────────────
#' run_seasonality_pipeline(
#'   iso3                   = "gin",
#'   adm0_name              = "Guinée",
#'   paths                  = paths,
#'   max_non_seasonal_years = 2,
#'   lang                   = "fr",
#'   cache_path             = here::here("translation_cache")
#' )
#'
#' # ── Enable step caching (prep_geonames-style: just set TRUE) ────────────────
#' # First run computes and caches each step automatically.
#' # Subsequent runs (e.g. steps = "smc_maps") load from cache.
#' run_seasonality_pipeline(
#'   iso3                   = "gin",
#'   adm0_name              = "Guinea",
#'   paths                  = paths,
#'   max_non_seasonal_years = 2,
#'   use_pipeline_cache     = TRUE   # ← that's all; path auto-resolved from paths$cache
#' )
#'
#' # ── Re-run only smc_maps using cached steps 2 & 3 ───────────────────────────
#' run_seasonality_pipeline(
#'   iso3                   = "gin",
#'   adm0_name              = "Guinea",
#'   paths                  = paths,
#'   max_non_seasonal_years = 2,
#'   use_pipeline_cache     = TRUE,
#'   steps                  = "smc_maps"  # steps 2 & 3 load from cache automatically
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
#' # ── Real-world caching workflow (Togo, French outputs) ───────────────────────
#' # Full run — all 6 steps cached automatically to paths$cache.
#' results <- run_seasonality_pipeline(
#'   iso3                   = "tgo",
#'   adm0_name              = "Togo",
#'   paths                  = paths,
#'   type                   = "both",
#'   s_year                 = 2013,
#'   e_year                 = 2023,
#'   max_non_seasonal_years = 3,     # seasonal in >= 8 of 11 years
#'   lang                   = "fr",
#'   use_pipeline_cache     = TRUE   # all step results saved to paths$cache
#' )
#'
#' # Later: add one district to the SMC list and regenerate only the maps.
#' # Steps 1-3 load from cache in seconds; step 5 runs fresh.
#' results_v2 <- run_seasonality_pipeline(
#'   iso3                     = "tgo",
#'   adm0_name                = "Togo",
#'   paths                    = paths,
#'   max_non_seasonal_years   = 3,
#'   lang                     = "fr",
#'   use_pipeline_cache       = TRUE,
#'   steps                    = "smc_maps",
#'   smc_additional_districts = c("KOZAH")
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

    # ── analysis level ────────────────────────────────────────────────────────
    analysis_level = c("adm2", "adm3", "adm1"),

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
    dpi          = 400,

    # ── step-specific year overrides (steps 4 & 6) ────────────────────────────
    # When rainfall and case data cover different years, these let you clip
    # the overlay graphs (step 4) to a specific range without affecting the
    # seasonality/block steps (steps 2 & 3). For step 6, use case_rain_s_date
    # / case_rain_e_date to control the median plots, and crude_s_date /
    # crude_e_date for the crude time-series plots.
    graphs_s_year = NULL,
    graphs_e_year = NULL,

    # ── pipeline step cache ────────────────────────────────────────────────────
    # Mirrors the prep_geonames() cache pattern: set use_pipeline_cache = TRUE
    # and the pipeline handles everything automatically.  When TRUE the cache
    # directory is resolved from paths$cache (or paths$val_tbl as a fallback)
    # so no path knowledge is required on the caller's side.
    #
    # What happens automatically:
    #   • After each step completes: result saved as {ISO3}_pipe_cache_{step}_{type}.qs2
    #   • Before each step runs:     if a matching cache file exists it is loaded
    #                                 and the step computation is skipped entirely
    #   • Cache files persist across R sessions — delete them manually to force
    #     a full recompute (same contract as prep_geonames cache files)
    #
    # pipeline_cache_dir is an escape hatch for non-standard project layouts.
    # Under normal use you never need to supply it — just set use_pipeline_cache = TRUE.
    use_pipeline_cache = FALSE,
    pipeline_cache_dir = NULL,

    # ── interactive alignment checks ──────────────────────────────────────────
    # When TRUE (default), steps 4 and 6 pause to show a date-range comparison
    # and prompt the user before continuing if rainfall and case data cover
    # different years.  Set to FALSE for scripted / non-interactive runs.
    interactive_checks = TRUE,

    # ── language ──────────────────────────────────────────────────────────────
    lang       = "en",
    cache_path = here::here("translation_cache")
) {

  # ── upfront package checks ──────────────────────────────────────────────────
  .check_pipeline_deps(steps)

  # ── validate inputs ─────────────────────────────────────────────────────────

  type                   <- match.arg(type)
  adm_level              <- match.arg(adm_level)
  analysis_level         <- match.arg(analysis_level)
  smc_eligibility_source <- match.arg(smc_eligibility_source)

  # # analysis_level = "adm3" requires adm3_var to be supplied
  # if (analysis_level == "adm3" && is.null(adm3_var)) {
  #   cli::cli_abort(c(
  #     "{.arg analysis_level = 'adm3'} requires {.arg adm3_var}.",
  #     "i" = "Supply the adm3 column name in your source data, e.g. {.code adm3_var = 'adm3'}."
  #   ))
  # }

  # Warn on adm1, unusual but supported
  if (analysis_level == "adm1") {
    cli::cli_warn(c(
      "Running analysis at adm1 level.",
      "i" = "All district-level outputs will be aggregated to adm1.",
      "i" = "Confirm this is intentional."
    ))
  }

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

  # ── resolve pipeline cache directory ────────────────────────────────────────
  # Following the prep_geonames() pattern: the user declares intent with a
  # single boolean (use_pipeline_cache = TRUE).  The pipeline then resolves the
  # storage location automatically from dirs$cache (= paths$cache in a standard
  # SNT project).  pipeline_cache_dir is only inspected when explicitly set,
  # giving advanced users a manual override without burdening the typical case.
  if (isTRUE(use_pipeline_cache) && is.null(pipeline_cache_dir)) {
    pipeline_cache_dir <- dirs$cache %||%
      file.path(
        if (!is.null(dirs$val_tbl)) dirname(dirs$val_tbl) else here::here(),
        "pipeline_cache"
      )
  }
  if (!is.null(pipeline_cache_dir)) .ensure_dir(pipeline_cache_dir)

  # ── write options bundle (passed to every step) ────────────────────────────
  write_opts <- list(include_date = include_date, n_saved = n_saved)

  # ── types to run ────────────────────────────────────────────────────────────
  types_to_run <- if (type == "both") c("rainfall", "cases") else type

  # ── announce ────────────────────────────────────────────────────────────────
  cli::cli_h1("Seasonality Pipeline: {adm0_name} ({toupper(iso3)})")
  cli::cli_alert_info("Steps:          {.val {steps}}")
  cli::cli_alert_info("Type(s):        {.val {types_to_run}}")
  cli::cli_alert_info("Analysis level: {.val {analysis_level}}")
  cli::cli_alert_info("Year range:     {.val {s_year %||% 'all'}} \u2013 {.val {e_year %||% 'all'}}")
  cli::cli_alert_info("Language:       {.val {lang %||% 'en'}}")
  cli::cli_alert_info("Figures \u2192 {.path { .relative_path(fig_root)}}")
  cli::cli_alert_info("Tables  \u2192 {.path { .relative_path(tbl_root)}}")
  if (!is.null(pipeline_cache_dir)) {
    cli::cli_alert_info("Cache   \u2192 {.path { .relative_path(pipeline_cache_dir)}} {.emph (enabled)}")
  } else {
    cli::cli_alert_info(
      "Cache   \u2192 {.emph disabled} (set {.code use_pipeline_cache = TRUE} to enable step caching)"
    )
  }

  results <- list()

  # ── STEP 1: HEATMAP ─────────────────────────────────────────────────────────

  if ("heatmap" %in% steps) {
    cli::cli_h2("Step [1/6] Heatmap")
    # Heatmap is fast; we don't cache it (plot objects are large for little gain).
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
        dpi            = dpi,
        lang           = lang,
        cache_path     = cache_path
      )
    )
    cli::cli_alert_success("Step 1 complete \u2713 Heatmap saved for: {.val {types_to_run}}")
  }

  # ── STEP 2: SEASONALITY ─────────────────────────────────────────────────────

  if ("seasonality" %in% steps) {
    cli::cli_h2("Step [2/6] Seasonality Analysis")

    # ── cache pickup — load any type already cached from a previous run ────────
    types_todo_2 <- character(0)
    for (.t in types_to_run) {
      if (!is.null(pipeline_cache_dir) && is.null(results$seasonality[[.t]])) {
        .cs <- .read_step_cache(iso3, "seasonality", .t, pipeline_cache_dir)
        if (!is.null(.cs)) {
          results$seasonality[[.t]] <- .cs
        } else {
          types_todo_2 <- c(types_todo_2, .t)
        }
      } else {
        types_todo_2 <- c(types_todo_2, .t)
      }
    }

    if (length(types_todo_2) > 0) {
      new_s2 <- purrr::map(
        purrr::set_names(types_todo_2),
        ~ .run_seasonality_step(
          dirs                   = dirs,
          iso3                   = iso3,
          adm0_name              = adm0_name,
          type                   = .x,
          s_year                 = s_year,
          e_year                 = e_year,
          adm1_var               = adm1_var,
          adm2_var               = adm2_var,
          adm3_var               = adm3_var,
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
          analysis_level         = analysis_level,
          fig_root               = fig_root,
          tbl_root               = tbl_root,
          write_opts             = write_opts,
          dpi                    = dpi,
          lang                   = lang,
          cache_path             = cache_path
        )
      )
      for (.t in types_todo_2) results$seasonality[[.t]] <- new_s2[[.t]]
      if (!is.null(pipeline_cache_dir)) {
        for (.t in types_todo_2)
          .write_step_cache(results$seasonality[[.t]], iso3, "seasonality", .t, pipeline_cache_dir)
      }
      cli::cli_alert_success("Step 2 complete \u2713 Seasonality classified for: {.val {types_todo_2}}")
    }
  }

  # ── STEP 3: BLOCKS ──────────────────────────────────────────────────────────

  if ("blocks" %in% steps) {
    cli::cli_h2("Step [3/6] Block Concentration Analysis")

    # ── cache pickup ───────────────────────────────────────────────────────────
    types_todo_3 <- character(0)
    for (.t in types_to_run) {
      if (!is.null(pipeline_cache_dir) && is.null(results$blocks[[.t]])) {
        .cb <- .read_step_cache(iso3, "blocks", .t, pipeline_cache_dir)
        if (!is.null(.cb)) {
          results$blocks[[.t]] <- .cb
        } else {
          types_todo_3 <- c(types_todo_3, .t)
        }
      } else {
        types_todo_3 <- c(types_todo_3, .t)
      }
    }

    if (length(types_todo_3) > 0) {
      new_b3 <- purrr::map(
        purrr::set_names(types_todo_3),
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
          analysis_level      = analysis_level,
          tbl_root            = tbl_root,
          write_opts          = write_opts
        )
      )
      for (.t in types_todo_3) results$blocks[[.t]] <- new_b3[[.t]]
      if (!is.null(pipeline_cache_dir)) {
        for (.t in types_todo_3)
          .write_step_cache(results$blocks[[.t]], iso3, "blocks", .t, pipeline_cache_dir)
      }
      cli::cli_alert_success("Step 3 complete \u2713 Block tables written for: {.val {types_todo_3}}")
    }
  }

  # ── STEP 4: GRAPHS (rolling % overlay) ──────────────────────────────────────

  if ("graphs" %in% steps) {
    cli::cli_h2("Step [4/6] Rolling % Overlay Graphs")

    # ── cache pickup ───────────────────────────────────────────────────────────
    if (!is.null(pipeline_cache_dir) && is.null(results$graphs)) {
      results$graphs <- .read_step_cache(iso3, "graphs", NULL, pipeline_cache_dir)
    }

    if (!is.null(results$graphs)) {
      cli::cli_alert_info("Step 4: loaded from cache \u2014 skipping recompute.")
    } else if (type != "both") {
      cli::cli_warn(c(
        "Step {.val graphs} requires both rainfall and cases.",
        "i" = "Skipping \u2014 {.arg type} is {.val {type}}.",
        "i" = "Set {.arg type = 'both'} to include overlay graphs."
      ))
    } else {
      # ── date-range alignment check ─────────────────────────────────────────
      # Extract year ranges from in-memory results or disk (best-effort).
      rain_yrs_4 <- tryCatch(
        sort(unique(results$seasonality[["rainfall"]]$detailed_results$StartYear)),
        error = function(e) integer(0)
      )
      case_yrs_4 <- tryCatch(
        sort(unique(results$seasonality[["cases"]]$detailed_results$StartYear)),
        error = function(e) integer(0)
      )

      g_s_year <- graphs_s_year
      g_e_year <- graphs_e_year

      if (length(rain_yrs_4) > 0 && length(case_yrs_4) > 0) {
        align4 <- .detect_year_mismatch(rain_yrs_4, case_yrs_4)
        if (!align4$aligned) {
          if (interactive_checks) {
            adj4   <- .prompt_date_alignment(align4, step = 4)
            g_s_year <- g_s_year %||% adj4$s_year
            g_e_year <- g_e_year %||% adj4$e_year
          } else {
            cli::cli_warn(c(
              "Step 4 date-range mismatch detected (interactive_checks = FALSE):",
              "i" = "Rainfall: {align4$rain_range[1]}\u2013{align4$rain_range[2]}",
              "i" = "Cases:    {align4$case_range[1]}\u2013{align4$case_range[2]}",
              "i" = "Overlap:  {align4$overlap_range[1]}\u2013{align4$overlap_range[2]}",
              "i" = "Use {.arg graphs_s_year}/{.arg graphs_e_year} to clip the overlay graphs."
            ))
          }
        }
      }

      results$graphs <- .run_graphs_step(
        iso3                = iso3,
        adm0_name           = adm0_name,
        s_year              = g_s_year,
        e_year              = g_e_year,
        rain_detailed       = results$seasonality$rainfall$detailed_results,
        case_detailed       = results$seasonality$cases$detailed_results,
        case_stratification = case_stratification,
        analysis_level      = analysis_level,
        tbl_root            = tbl_root,
        fig_root            = fig_root,
        dpi                 = dpi,
        lang                = lang,
        cache_path          = cache_path
      )
      if (!is.null(results$graphs)) {
        cli::cli_alert_success("Step 4 complete \u2713 Overlay graphs saved.")
        if (!is.null(pipeline_cache_dir))
          .write_step_cache(results$graphs, iso3, "graphs", NULL, pipeline_cache_dir)
      }
    }
  }

  # ── STEP 5: SMC MAPS ────────────────────────────────────────────────────────

  if ("smc_maps" %in% steps) {
    cli::cli_h2("Step [5/6] SMC Eligibility & Timing Maps")
    if (is.null(dirs$admin_shp)) {
      cli::cli_abort(c(
        "Step {.val smc_maps} requires a shapefile directory.",
        "x" = "No shapefile path was resolved.",
        "i" = "Fix options:",
        "i" = "  Option A: supply {.arg paths} from {.code sntutils::setup_project_paths()}",
        "i" = "  Option B: supply {.arg base_path} pointing to your project root",
        "i" = "  Option C: supply {.arg admin_shp_dir} explicitly",
        "i" = "The directory should contain a {.code processed/} subfolder with",
        "i" = "{.code {iso3}_shp_list.qs2}."
      ))
    }

    .pick_location_summary <- function(type) {
      if (smc_eligibility_source == "manual") return(NULL)
      src <- if (smc_eligibility_source == "cases") "cases" else "rainfall"
      results$seasonality[[src]]$location_summary
    }

    # ── cache pickup ───────────────────────────────────────────────────────────
    types_todo_5 <- character(0)
    for (.t in types_to_run) {
      if (!is.null(pipeline_cache_dir) && is.null(results$smc_maps[[.t]])) {
        .csmc <- .read_step_cache(iso3, "smc_maps", .t, pipeline_cache_dir)
        if (!is.null(.csmc)) {
          results$smc_maps[[.t]] <- .csmc
        } else {
          types_todo_5 <- c(types_todo_5, .t)
        }
      } else {
        types_todo_5 <- c(types_todo_5, .t)
      }
    }

    if (length(types_todo_5) > 0) {
      new_s5 <- purrr::map(
        purrr::set_names(types_todo_5),
        ~ .run_smc_maps_step(
          dirs                     = dirs,
          iso3                     = iso3,
          adm0_name                = adm0_name,
          type                     = .x,
          block_frequency          = results$blocks[[.x]]$frequency,
          location_summary         = .pick_location_summary(.x),
          tbl_root                 = tbl_root,
          case_stratification      = case_stratification,
          analysis_level           = analysis_level,
          smc_eligible_districts   = smc_eligible_districts,
          smc_additional_districts = smc_additional_districts,
          smc_remove_districts     = smc_remove_districts,
          fig_root                 = fig_root,
          write_opts               = write_opts,
          dpi                      = dpi,
          lang                     = lang,
          cache_path               = cache_path
        )
      )
      for (.t in types_todo_5) results$smc_maps[[.t]] <- new_s5[[.t]]
      if (!is.null(pipeline_cache_dir)) {
        for (.t in types_todo_5)
          .write_step_cache(results$smc_maps[[.t]], iso3, "smc_maps", .t, pipeline_cache_dir)
      }
      cli::cli_alert_success("Step 5 complete \u2713 SMC maps saved for: {.val {types_todo_5}}")
    }
  }

  # ── STEP 6: CASE vs RAINFALL PLOTS ──────────────────────────────────────────

  if ("case_rain_plots" %in% steps) {
    cli::cli_h2("Step [6/6] Cases vs Rainfall Plots")

    # ── cache pickup ───────────────────────────────────────────────────────────
    if (!is.null(pipeline_cache_dir) && is.null(results$case_rain_plots)) {
      results$case_rain_plots <- .read_step_cache(iso3, "case_rain_plots", NULL, pipeline_cache_dir)
    }

    if (!is.null(results$case_rain_plots)) {
      cli::cli_alert_info("Step 6: loaded from cache \u2014 skipping recompute.")
    } else {
      results$case_rain_plots <- .run_case_rain_plots_step(
        dirs               = dirs,
        iso3               = iso3,
        adm0_name          = adm0_name,
        adm1_var           = adm1_var,
        adm2_var           = adm2_var,
        adm3_var           = adm3_var,
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
        analysis_level     = analysis_level,
        panels_per_row     = panels_per_row,
        fig_root           = fig_root,
        dpi                = dpi,
        lang               = lang,
        cache_path         = cache_path,
        interactive_checks = interactive_checks
      )
      cli::cli_alert_success("Step 6 complete \u2713 Cases vs rainfall plots saved.")
      if (!is.null(pipeline_cache_dir))
        .write_step_cache(results$case_rain_plots, iso3, "case_rain_plots", NULL, pipeline_cache_dir)
    }
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
    adm_level, viridis_option, x_breaks_by, fig_root, dpi,
    lang, cache_path
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
    x_breaks_by    = x_breaks_by,
    lang           = lang,
    cache_path     = cache_path
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
    adm1_var, adm2_var, adm3_var = NULL, year_var, month_var, value_var,
    analysis_start_month, seasonality_threshold,
    max_non_seasonal_years, min_years_required,
    n_cumulative_maps, cumulative_thresholds,
    case_stratification,
    analysis_level = "adm2",
    fig_root, tbl_root, write_opts, dpi,
    lang, cache_path
) {
  cli::cli_alert_info("Seasonality \u2192 {.val {type}}")

  df <- .load_analysis_data(
    dirs = dirs, iso3 = iso3, type = type,
    adm1_var = adm1_var, adm2_var = adm2_var, adm3_var = adm3_var,
    year_var = year_var, month_var = month_var,
    value_var = value_var, analysis_level = analysis_level
  ) |>
    .filter_years(s_year, e_year) |>
    .build_admin_group(admin_cols = .level_cols(analysis_level))

  available_years <- sort(unique(df$year))
  n_years         <- length(available_years)

  if (n_years < min_years_required) {
    cli::cli_abort(c(
      "Insufficient data for {.val {type}} seasonality analysis.",
      "x" = "Need \u2265 {min_years_required} year{?s} of data; found {n_years}.",
      "i" = "Years present in your data: {.val {available_years}}",
      "i" = "Fix options:",
      "i" = "  1. Widen the date range: lower {.arg s_year} or raise {.arg e_year}",
      "i" = "  2. Lower the bar: set {.arg min_years_required} to {n_years} (not recommended)",
      "i" = "  3. Check your source file for missing years"
    ))
  }

  cli::cli_alert_info(
    "Data: {min(available_years)}\u2013{max(available_years)} ({n_years} years)"
  )

  blocks           <- .generate_rolling_blocks(df, analysis_start_month)
  detailed_results <- .calculate_seasonality(df, blocks, seasonality_threshold)
  yearly_summary   <- .build_yearly_summary(detailed_results, analysis_level)
  location_summary <- .build_location_summary(yearly_summary, max_non_seasonal_years,
                                               analysis_level)

  n_s  <- sum(location_summary$Seasonality == "Seasonal",     na.rm = TRUE)
  n_ns <- sum(location_summary$Seasonality == "Not Seasonal", na.rm = TRUE)
  cli::cli_alert_success("Classification: {n_s} seasonal, {n_ns} not seasonal.")

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
    analysis_level        = analysis_level,
    fig_dir               = seas_fig_dir,
    dpi                   = dpi,
    lang                  = lang,
    cache_path            = cache_path
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
    analysis_level = "adm2",
    tbl_root, write_opts
) {
  cli::cli_alert_info("Block analysis \u2192 {.val {type}}")

  df <- .load_analysis_data(
    dirs = dirs, iso3 = iso3, type = type,
    adm1_var = adm1_var, adm2_var = adm2_var, adm3_var = adm3_var,
    year_var = year_var, month_var = month_var,
    value_var = value_var, analysis_level = analysis_level
  ) |>
    .filter_years(s_year, e_year) |>
    dplyr::mutate(
      # district key uses only the two most-granular levels that make sense
      # for the analysis level, keeping it human-readable and unambiguous:
      #   adm1  → just the region name
      #   adm2  → "Region - District"
      #   adm3  → "District - Sub-district"  (adm2-adm3, NOT adm1-adm2-adm3)
      district = switch(analysis_level,
        adm1 = adm1,
        adm2 = paste(adm1, adm2, sep = " - "),
        adm3 = paste(adm2, adm3, sep = " - ")
      )
    )

  n_districts <- dplyr::n_distinct(df$district)
  n_years_blk <- dplyr::n_distinct(df$year)
  cli::cli_alert_info(
    "Block data: {n_districts} district{?s}, {n_years_blk} year{?s} ({min(df$year)}\u2013{max(df$year)})"
  )

  # adm3 lookup adds adm3 as a label on adm2-level tables.
  # When analysing at adm3 level, adm3 is already the district key — skip.
  adm3_lookup <- if (analysis_level != "adm3") {
    .resolve_adm3_lookup(
      dirs     = dirs,
      iso3     = iso3,
      type     = type,
      adm1_var = adm1_var,
      adm2_var = adm2_var,
      adm3_var = adm3_var
    )
  } else {
    NULL
  }

  windows       <- .calculate_rolling_windows(df)
  summary_tbl   <- windows$summary
  detailed_tbl  <- windows$detailed
  frequency_tbl <- .build_block_frequency(summary_tbl, detailed_tbl, analysis_level)

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
  cli::cli_alert_success(
    "Block step complete: {nrow(frequency_tbl)} frequency row{?s} across {n_districts} district{?s}."
  )

  invisible(list(
    summary   = summary_tbl,
    detailed  = detailed_tbl,
    frequency = frequency_tbl
  ))
}


#' @keywords internal
.run_graphs_step <- function(
    iso3, adm0_name, s_year, e_year,
    rain_detailed, case_detailed,
    case_stratification,
    analysis_level = "adm2",
    tbl_root, fig_root, dpi,
    lang, cache_path
) {
  # The graphs step overlays rainfall and cases at a sub-province level.
  # At adm1 there is no lower level to facet by, so the step cannot run.
  if (analysis_level == "adm1") {
    cli::cli_warn(c(
      "Step {.val graphs} is not supported at {.val adm1} analysis level.",
      "i" = "Overlay graphs require at least two admin levels (loop + panel).",
      "i" = "Skipping."
    ))
    return(invisible(NULL))
  }

  rain_df <- .get_or_read_detailed(rain_detailed, tbl_root, iso3, "rainfall",
                                   case_stratification)
  case_df <- .get_or_read_detailed(case_detailed,  tbl_root, iso3, "cases",
                                   case_stratification)

  # Quick sanity check — the analysis_level column must exist in both tables
  if (!analysis_level %in% names(rain_df)) {
    cli::cli_abort(c(
      "Column {.var {analysis_level}} not found in rainfall detailed results.",
      "x" = "Columns present: {.val {names(rain_df)}}",
      "i" = "This usually means the detailed results were saved at a different",
      "i" = "analysis level. Re-run the {.val 'seasonality'} step with",
      "i" = "{.arg analysis_level = '{analysis_level}'}."
    ))
  }
  if (!analysis_level %in% names(case_df)) {
    cli::cli_abort(c(
      "Column {.var {analysis_level}} not found in cases detailed results.",
      "x" = "Columns present: {.val {names(case_df)}}",
      "i" = "Re-run the {.val 'seasonality'} step with {.arg analysis_level = '{analysis_level}'}."
    ))
  }

  cli::cli_alert_info(
    "Overlay graphs: {dplyr::n_distinct(rain_df[[analysis_level]])} {analysis_level} unit{?s} in rainfall, {dplyr::n_distinct(case_df[[analysis_level]])} in cases."
  )

  # Rename the analysis-level column to "district" so downstream code is
  # agnostic to whether we are working at adm2 or adm3.
  rain_df <- rain_df |>
    .filter_years(s_year, e_year, col = "StartYear") |>
    dplyr::rename(province = adm1,
                  district = !!analysis_level,
                  Percent_Seasonality_Rainfall = Percent_Seasonality) |>
    dplyr::mutate(
      Date = lubridate::ymd(
        paste(StartYear, match(StartMonth, month.abb), "01", sep = "-")
      )
    )

  case_df <- case_df |>
    .filter_years(s_year, e_year, col = "StartYear") |>
    dplyr::rename(province = adm1,
                  district = !!analysis_level,
                  Percent_Seasonality_Cases = Percent_Seasonality) |>
    dplyr::mutate(
      Date = lubridate::ymd(
        paste(StartYear, match(StartMonth, month.abb), "01", sep = "-")
      )
    )

  # ── translated legend labels — built separately so they can be used as
  #    both named vector keys AND as display strings without any confusion ───
  lbl_cases    <- .tr("Cases",    lang, cache_path)
  lbl_rainfall <- .tr("Rainfall", lang, cache_path)

  merged_df <- case_df |>
    dplyr::select(province, district, Block, Date,
                  StartMonth, StartYear, Percent_Seasonality_Cases) |>
    dplyr::left_join(
      rain_df |> dplyr::select(province, district, Block, StartYear,
                               Percent_Seasonality_Rainfall),
      # Include StartYear in the key: Block is a sequential integer that restarts
      # in every district and repeats across years, so joining without it causes
      # many-to-many matches (one case row matching multiple rainfall rows for
      # the same block number in different years, and vice-versa).
      by = c("province", "district", "Block", "StartYear")
    ) |>
    tidyr::pivot_longer(
      cols      = c(Percent_Seasonality_Cases, Percent_Seasonality_Rainfall),
      names_to  = "Type",
      values_to = "Percent_Seasonality"
    ) |>
    dplyr::mutate(
      # Remap the raw column-name values to translated display labels
      Type = dplyr::case_when(
        Type == "Percent_Seasonality_Cases"    ~ lbl_cases,
        Type == "Percent_Seasonality_Rainfall" ~ lbl_rainfall,
        TRUE                                   ~ Type
      )
    )

  if (nrow(merged_df) == 0) {
    cli::cli_abort(c(
      "Overlay merge produced zero rows.",
      "x" = "No matching records between rainfall and cases detailed results.",
      "i" = "Check that both datasets cover the same years and admin units.",
      "i" = "Tip: inspect {.code results$seasonality$rainfall$detailed_results} and",
      "i" = "     {.code results$seasonality$cases$detailed_results} side-by-side."
    ))
  }

  n_na_rain <- sum(is.na(merged_df$Percent_Seasonality[merged_df$Type == lbl_rainfall]))
  n_na_case <- sum(is.na(merged_df$Percent_Seasonality[merged_df$Type == lbl_cases]))
  if (n_na_rain > 0 || n_na_case > 0) {
    cli::cli_warn(c(
      "After merging, {n_na_rain} rainfall and {n_na_case} cases value{?s} are NA.",
      "i" = "Lines for those periods will be broken in the overlay graphs.",
      "i" = "This is expected if the two datasets cover slightly different date ranges."
    ))
  }

  graphs_dir <- file.path(fig_root, "case_rain_graphs")
  .ensure_dir(graphs_dir)

  provinces <- sort(unique(merged_df$province))

  # ── pre-translate repeated strings once, outside the loop ──────────────────
  lbl_pct_seas  <- .tr("Percent Seasonality (%)",                       lang, cache_path)
  lbl_type      <- .tr("Type",                                          lang, cache_path)
  lbl_seas_sub  <- .tr("Percent Seasonality of Cases vs Rainfall by District",
                        lang, cache_path)

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

    lbl_title <- .tr(
      glue::glue("Seasonality Comparison: {prov}"),
      lang, cache_path
    )

    p <- ggplot2::ggplot(
      prov_data,
      ggplot2::aes(x = Date, y = Percent_Seasonality, color = Type)
    ) +
      ggplot2::geom_line(linewidth = 0.8) +
      ggplot2::facet_wrap(~ district, ncol = facet_ncol, scales = "free_y") +
      ggplot2::scale_color_manual(
        values = stats::setNames(
          c("#E74C3C", "#3498DB"),
          c(lbl_cases, lbl_rainfall)
        )
      ) +
      ggplot2::scale_x_date(
        breaks      = seq(min(prov_data$Date, na.rm = TRUE),
                          max(prov_data$Date, na.rm = TRUE),
                          by = "4 months"),
        date_labels = "%b %Y"
      ) +
      ggplot2::labs(
        title    = lbl_title,
        subtitle = lbl_seas_sub,
        x        = NULL,
        y        = lbl_pct_seas,
        color    = lbl_type
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
      path = file.path(graphs_dir, paste0(gsub(" ", "_", prov), "_seasonality_comparison.png")),
      verbosity = FALSE
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

  lbl_summary_title <- .tr(
    glue::glue("{adm0_name}: Average Seasonality - All Provinces"),
    lang, cache_path
  )
  lbl_summary_sub  <- .tr("Mean Percent Seasonality of Cases vs Rainfall",
                           lang, cache_path)
  lbl_mean_pct     <- .tr("Mean Percent Seasonality (%)", lang, cache_path)

  summary_plot <- ggplot2::ggplot(
    summary_data,
    ggplot2::aes(x = Date, y = Mean_Seasonality, color = Type)
  ) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::facet_wrap(~ province, ncol = 2, scales = "free_y") +
    ggplot2::scale_color_manual(
      values = stats::setNames(
        c("#E74C3C", "#3498DB"),
        c(lbl_cases, lbl_rainfall)
      )
    ) +
    ggplot2::scale_x_date(
      breaks      = seq(min(summary_data$Date, na.rm = TRUE),
                        max(summary_data$Date, na.rm = TRUE),
                        by = "4 months"),
      date_labels = "%b %Y"
    ) +
    ggplot2::labs(
      title    = lbl_summary_title,
      subtitle = lbl_summary_sub,
      x        = NULL,
      y        = lbl_mean_pct,
      color    = lbl_type
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
    path = file.path(graphs_dir, glue::glue("{iso3}_ALL_PROVINCES_summary_comparison.png")),
    verbosity = FALSE
  )

  cli::cli_alert_success("Summary overlay graph saved \u2192 {.path { .relative_path(graphs_dir)}}")
  invisible(c(plots, list(.summary = summary_plot)))
}


#' @keywords internal
.run_smc_maps_step <- function(
    dirs, iso3, adm0_name, type,
    block_frequency, location_summary, tbl_root,
    case_stratification,
    analysis_level = "adm2",
    smc_eligible_districts, smc_additional_districts, smc_remove_districts,
    fig_root, write_opts, dpi,
    lang, cache_path
) {
  cli::cli_alert_info("SMC maps \u2192 {.val {type}}")

  freq <- .get_or_read_block_freq(block_frequency, tbl_root, iso3, type,
                                  case_stratification)

  eligible <- .derive_smc_eligibility(
    location_summary         = location_summary,
    smc_eligible_districts   = smc_eligible_districts,
    smc_additional_districts = smc_additional_districts,
    smc_remove_districts     = smc_remove_districts,
    analysis_level           = analysis_level
  )

  spatial <- sntutils::read_snt_data(
    path         = here::here(dirs$admin_shp, "processed"),
    data_name    = glue::glue("{iso3}_shp_list"),
    file_formats = "qs2"
  )$final_spat_vec

  # Use the polygon layer that matches the analysis level.
  # The adm1 overlay always stays as the province boundary.
  analysis_sp <- spatial[[analysis_level]]
  adm1_sp     <- spatial$adm1

  smc_fig_dir <- file.path(fig_root, .type_smc_subdir(type))
  .ensure_dir(smc_fig_dir)

  dims <- .compute_map_dims(analysis_sp)

  month_lkp <- c(apr = 4L, may = 5L, jun = 6L, jul = 7L, aug = 8L, sep = 9L)

  lvl_cols <- .level_cols(analysis_level)

  if (all(lvl_cols %in% names(freq))) {
    freq_adm <- freq
  } else {
    cli::cli_warn(c(
      "Block frequency table is missing expected admin columns.",
      "i" = "Falling back to district string parsing \u2014 hyphenated names may not match."
    ))
    freq_adm <- freq |>
      dplyr::mutate(
        adm1 = stringr::str_trim(stringr::str_extract(district, "^[^-]+")),
        adm2 = stringr::str_trim(stringr::str_extract(district, "(?<=- ).*$"))
      )
  }

  block_data <- freq_adm |>
    dplyr::mutate(
      smc_yn = dplyr::if_else(.data[[analysis_level]] %in% eligible, 1L, 0L)
    ) |>
    dplyr::filter(smc_yn == 1L) |>
    dplyr::arrange(dplyr::across(dplyr::all_of(lvl_cols)), duration, block_freq, median_max_prop) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(lvl_cols, "duration")))) |>
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

  # ── palette keys are numeric strings — never translated ────────────────────
  # Display labels are built separately so translation doesn't break subsetting.
  month_pal <- c(
    "4" = "#7c4aa5ff", "5" = "#4169E1", "6" = "#32CD32",
    "7" = "#FFA500",   "8" = "#FFFF00", "9" = "#FF0000"
  )
  month_lbl <- stats::setNames(
    c(
      .tr("April",     lang, cache_path),
      .tr("May",       lang, cache_path),
      .tr("June",      lang, cache_path),
      .tr("July",      lang, cache_path),
      .tr("August",    lang, cache_path),
      .tr("September", lang, cache_path)
    ),
    c("4", "5", "6", "7", "8", "9")
  )

  cov_pal <- c(
    "1" = "#f7dc6f", "2" = "#e67e22", "3" = "#5dade2",
    "4" = "#4c73e6ff", "5" = "#1313c9ff", "6" = "#0b0b70ff"
  )
  cov_lbl <- stats::setNames(
    c(
      .tr("20-40%",   lang, cache_path),
      .tr("40-50%",   lang, cache_path),
      .tr("50-60%",   lang, cache_path),
      .tr("60-70%",   lang, cache_path),
      .tr("70-80%",   lang, cache_path),
      .tr("80-100%",  lang, cache_path)
    ),
    c("1", "2", "3", "4", "5", "6")
  )

  # ── pre-translate fill_label strings used in every loop iteration ──────────
  lbl_first_month <- .tr("First Month", lang, cache_path)
  lbl_pct_covered <- .tr("% Covered",   lang, cache_path)

  type_label <- if (type == "rainfall") "Rainfall" else "Cases"

  durations     <- sort(unique(block_data$duration))
  timing_maps   <- list()
  coverage_maps <- list()

  for (dur in durations) {
    dur_data <- block_data |> dplyr::filter(duration == dur)

    # Join on ALL admin columns (e.g. adm1+adm2+adm3 at adm3 level) so that
    # districts with the same leaf name but different parents are not conflated.
    smc_join_cols <- intersect(lvl_cols, names(analysis_sp))
    if (length(smc_join_cols) == 0) smc_join_cols <- analysis_level
    map_data <- analysis_sp |>
      dplyr::left_join(dur_data, by = smc_join_cols, relationship = "many-to-one") |>
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
      title       = .tr(
        glue::glue("Ideal first month {dur} cycles ({type})"),
        lang, cache_path
      ),
      fill_label  = lbl_first_month,
      dims        = dims
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
      title       = .tr(
        glue::glue("% of {type} covered {dur}-month window"),
        lang, cache_path
      ),
      fill_label  = lbl_pct_covered,
      dims        = dims
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
    dplyr::group_by(dplyr::across(dplyr::all_of(lvl_cols))) |>
    dplyr::mutate(
      max_prop = max(median_max_prop, na.rm = TRUE),
      rule     = dplyr::case_when(
        max_prop <  60 ~ 1L,
        max_prop >= 60 ~ 2L,
        TRUE           ~ NA_integer_
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::group_by(dplyr::across(dplyr::all_of(lvl_cols))) |>
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
    dplyr::select(dplyr::all_of(lvl_cols), duration, block, block_freq,
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
  min_spatial <- analysis_sp |>
    dplyr::left_join(min_final, by = smc_join_cols, relationship = "many-to-one") |>
    dplyr::mutate(
      firmonth_f    = factor(as.character(firmonth),    levels = as.character(4:9)),
      median_cats_f = factor(as.character(median_cats), levels = as.character(1:6))
    )

  dur_levels  <- sort(unique(stats::na.omit(min_final$duration)))
  min_spatial <- min_spatial |>
    dplyr::mutate(duration = factor(duration, levels = dur_levels))

  # ── duration palette keys stay numeric; display labels are translated ───────
  dur_colours <- c("2" = "#52b788ff", "3" = "#d8c974ff",
                   "4" = "#e25ae2ff", "5" = "#6e1e6eff")
  dur_labels  <- stats::setNames(
    c(
      .tr("2 months (3 cycles)", lang, cache_path),
      .tr("3 months (4 cycles)", lang, cache_path),
      .tr("4 months (5 cycles)", lang, cache_path),
      .tr("5 months (6 cycles)", lang, cache_path)
    ),
    c("2", "3", "4", "5")
  )
  dur_colours <- dur_colours[as.character(dur_levels)]
  dur_labels  <- dur_labels[as.character(dur_levels)]

  type_label_min <- if (type == "rainfall") "Rainfall" else "Cases"

  # ── pre-translate minimum-map strings ──────────────────────────────────────
  lbl_duration        <- .tr("Duration",   lang, cache_path)
  lbl_min_dur_title   <- .tr(
    glue::glue("Minimum months to cover ~60% of {type_label_min}"),
    lang, cache_path
  )
  lbl_min_dur_sub     <- .tr("Minimum SMC cycles required per district",
                              lang, cache_path)
  lbl_min_month_title <- .tr("First month for the minimum number of cycles",
                              lang, cache_path)
  lbl_min_month_sub   <- .tr(
    glue::glue("Based on {tolower(type_label_min)}"),
    lang, cache_path
  )
  lbl_min_cov_title   <- .tr(
    glue::glue("% of {type_label_min} Covered (minimum block)"),
    lang, cache_path
  )
  lbl_min_cov_sub     <- .tr(
    "Median proportion captured by the minimum best block",
    lang, cache_path
  )

  map_min_duration <- .build_snt_map(
    adm2_sf     = min_spatial,
    fill_col    = "duration",
    fill_values = dur_colours,
    fill_labels = dur_labels,
    adm1_sf     = adm1_sp,
    title       = lbl_min_dur_title,
    subtitle    = lbl_min_dur_sub,
    fill_label  = lbl_duration,
    dims        = dims
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
    title       = lbl_min_month_title,
    subtitle    = lbl_min_month_sub,
    fill_label  = lbl_first_month,
    dims        = dims
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
    title       = lbl_min_cov_title,
    subtitle    = lbl_min_cov_sub,
    fill_label  = lbl_pct_covered,
    dims        = dims
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
    adm1_var, adm2_var, adm3_var = NULL, year_var, month_var,
    case_var_ov5, case_var_u5, rainfall_total_var,
    case_rain_s_date, case_rain_e_date,
    crude_s_date = NULL, crude_e_date = NULL,
    s_year, e_year,
    analysis_level = "adm2",
    panels_per_row,
    fig_root, dpi,
    lang, cache_path,
    interactive_checks = TRUE
) {
  median_dir <- file.path(fig_root, "case_v_rainfall")
  crude_dir  <- file.path(fig_root, "case_v_rainfall_crude")
  .ensure_dir(median_dir)
  .ensure_dir(crude_dir)

  s_date_resolved <- .resolve_date(case_rain_s_date, s_year, "start")
  e_date_resolved <- .resolve_date(case_rain_e_date, e_year, "end")

  case_raw <- sntutils::read_snt_data(
    dirs$dhis2, glue::glue("{iso3}_dhis2_processed"), "xlsx"
  )

  required_case <- c(adm1_var, adm2_var,
                     if (!is.null(adm3_var)) adm3_var,
                     year_var, month_var, case_var_ov5, case_var_u5)
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

  required_rain <- c(adm1_var, adm2_var,
                     if (!is.null(adm3_var)) adm3_var,
                     year_var, month_var, rainfall_total_var)
  missing_rain  <- setdiff(required_rain, names(rain_raw))
  if (length(missing_rain) > 0) {
    cli::cli_abort(c(
      "Missing column{?s} in rainfall data:",
      "x" = "{.var {missing_rain}}",
      "i" = "Use {.arg rainfall_total_var} to map your column name."
    ))
  }

  # Build the admin grouping columns at the analysis level
  adm_grp_vars <- .level_cols(analysis_level)

  case_df <- case_raw |>
    dplyr::filter(dplyr::if_all(
      dplyr::all_of(c(case_var_ov5, case_var_u5)), ~ !is.na(.)
    )) |>
    dplyr::select(dplyr::all_of(required_case)) |>
    dplyr::rename(
      adm1 = !!adm1_var, adm2 = !!adm2_var,
      year = !!year_var, month = !!month_var,
      conf_ov5 = !!case_var_ov5, conf_u5 = !!case_var_u5
    )

  if (!is.null(adm3_var))
    case_df <- dplyr::rename(case_df, adm3 = !!adm3_var)

  case_df <- case_df |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(adm_grp_vars, "year", "month")))) |>
    dplyr::summarise(
      conf_ov5 = sum(conf_ov5, na.rm = TRUE),
      conf_u5  = sum(conf_u5,  na.rm = TRUE),
      .groups  = "drop"
    ) |>
    dplyr::mutate(date = as.Date(sprintf("%04d-%02d-01", year, month)))

  rain_df_full <- rain_raw |>
    dplyr::select(dplyr::all_of(required_rain)) |>
    dplyr::rename(
      adm1 = !!adm1_var, adm2 = !!adm2_var,
      year = !!year_var, month = !!month_var,
      total_rainfall_mm = !!rainfall_total_var
    )

  if (!is.null(adm3_var))
    rain_df_full <- dplyr::rename(rain_df_full, adm3 = !!adm3_var)

  rain_df_full <- rain_df_full |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(adm_grp_vars, "year", "month")))) |>
    dplyr::summarise(
      total_rainfall_mm = sum(total_rainfall_mm, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(date = as.Date(sprintf("%04d-%02d-01", year, month)))

  # ── date-range alignment check (Step 6) ────────────────────────────────────
  # Compare the year coverage of both raw datasets before filtering.
  # If they differ and no explicit dates were supplied, prompt (or warn) so
  # the user can decide how to align the overlay before the plots are drawn.
  case_yr6_all <- sort(unique(case_df$year))
  rain_yr6_all <- sort(unique(rain_df_full$year))
  align6 <- .detect_year_mismatch(rain_yr6_all, case_yr6_all)

  if (!align6$aligned) {
    # Only prompt if the user hasn't already specified explicit date bounds
    user_set_dates <- !is.null(case_rain_s_date) || !is.null(case_rain_e_date) ||
                      !is.null(s_year) || !is.null(e_year)
    if (!user_set_dates && interactive_checks && interactive()) {
      adj6 <- .prompt_date_alignment(align6, step = 6)
      if (!is.null(adj6$s_year)) {
        s_date_resolved <- as.Date(sprintf("%04d-01-01", adj6$s_year))
        e_date_resolved <- as.Date(sprintf("%04d-12-31", adj6$e_year))
        # Also clip crude plots to the same range unless already specified
        if (is.null(crude_s_date)) crude_s_date <- format(s_date_resolved)
        if (is.null(crude_e_date)) crude_e_date <- format(e_date_resolved)
      }
    } else if (!user_set_dates) {
      cli::cli_warn(c(
        "Step 6 date-range mismatch:",
        "i" = "Rainfall: {align6$rain_range[1]}\u2013{align6$rain_range[2]}",
        "i" = "Cases:    {align6$case_range[1]}\u2013{align6$case_range[2]}",
        "i" = "Overlap:  {align6$overlap_range[1]}\u2013{align6$overlap_range[2]}",
        "i" = "Plots will show gaps where one series has no data.",
        "i" = "Use {.arg case_rain_s_date}/{.arg case_rain_e_date} to clip, or set",
        "i" = "{.arg interactive_checks = TRUE} to be prompted next time."
      ))
    }
  }

  rain_median <- rain_df_full
  if (!is.na(s_date_resolved)) rain_median <- rain_median |> dplyr::filter(date >= s_date_resolved)
  if (!is.na(e_date_resolved)) rain_median <- rain_median |> dplyr::filter(date <= e_date_resolved)

  combined_raw <- rain_median |>
    dplyr::left_join(
      case_df |> dplyr::select(dplyr::all_of(c(adm_grp_vars, "year", "month",
                                                "conf_ov5", "conf_u5"))),
      by = c(adm_grp_vars, "year", "month")
    )

  df_median <- combined_raw |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(adm_grp_vars, "month")))) |>
    dplyr::summarise(
      conf_ov5          = stats::median(conf_ov5,          na.rm = TRUE),
      conf_u5           = stats::median(conf_u5,           na.rm = TRUE),
      total_rainfall_mm = stats::median(total_rainfall_mm, na.rm = TRUE),
      .groups = "drop"
    )

  crude_s_resolved <- .resolve_date(crude_s_date, NULL, "start")
  if (is.na(crude_s_resolved))
    crude_s_resolved <- .resolve_date(case_rain_s_date, s_year, "start")

  crude_e_resolved <- .resolve_date(crude_e_date, NULL, "end")
  if (is.na(crude_e_resolved))
    crude_e_resolved <- .resolve_date(case_rain_e_date, e_year, "end")

  crude_rain <- rain_df_full
  if (!is.na(crude_s_resolved)) crude_rain <- crude_rain |> dplyr::filter(date >= crude_s_resolved)
  if (!is.na(crude_e_resolved)) crude_rain <- crude_rain |> dplyr::filter(date <= crude_e_resolved)

  case_crude <- case_df
  if (!is.na(crude_s_resolved)) case_crude <- case_crude |> dplyr::filter(date >= crude_s_resolved)
  if (!is.na(crude_e_resolved)) case_crude <- case_crude |> dplyr::filter(date <= crude_e_resolved)

  combined_crude <- dplyr::full_join(
    crude_rain |> dplyr::select(dplyr::all_of(c(adm_grp_vars, "date",
                                                 "total_rainfall_mm"))),
    case_crude |> dplyr::select(dplyr::all_of(c(adm_grp_vars, "date",
                                                 "conf_ov5", "conf_u5"))),
    by = c(adm_grp_vars, "date")
  ) |>
    dplyr::filter(!is.na(date))

  # ── determine loop and panel columns ────────────────────────────────────────
  # At adm2: loop over adm1 regions, facet by adm2 districts (default)
  # At adm3: loop over adm2 districts, facet by adm3 sub-districts
  # At adm1: loop over adm1, no sub-grouping (panel_col = loop_col)
  loop_col  <- switch(analysis_level,
    adm1 = "adm1",
    adm2 = "adm1",
    adm3 = "adm2"
  )
  panel_col <- analysis_level

  # ── pre-translate all plot strings once before the region loops ─────────────
  lbl_ov5      <- .tr("Cases >5 years", lang, cache_path)
  lbl_u5       <- .tr("Cases <5 years", lang, cache_path)
  lbl_rainfall <- .tr("Rainfall",       lang, cache_path)
  lbl_med_y    <- .tr("Cases confirmed (median)", lang, cache_path)
  lbl_rain_mm  <- .tr("Rainfall (mm)",            lang, cache_path)
  lbl_cases_y  <- .tr("Cases confirmed",          lang, cache_path)

  loop_regions <- sort(unique(df_median[[loop_col]]))

  median_plots <- purrr::map(purrr::set_names(loop_regions), function(region) {
    region_data <- df_median |> dplyr::filter(.data[[loop_col]] == region)
    n_panels    <- dplyr::n_distinct(region_data[[panel_col]])
    ppr         <- .adapt_panels_per_row(n_panels, panels_per_row)
    p           <- .build_median_adm1_plot(
      data           = region_data,
      loop_name      = region,
      loop_col       = loop_col,
      panel_col      = panel_col,
      panels_per_row = ppr,
      lbl_ov5        = lbl_ov5,
      lbl_u5         = lbl_u5,
      lbl_rainfall   = lbl_rainfall,
      lbl_med_y      = lbl_med_y,
      lbl_rain_mm    = lbl_rain_mm
    )
    n_rows <- ceiling(n_panels / ppr)

    ggplot2::ggsave(
      filename  = glue::glue("Cases_v_rainfall_median_{region}.png"),
      plot      = p, path = median_dir,
      width = ppr * 5, height = 3.5 * n_rows + 1.5,
      dpi = dpi, bg = "white", limitsize = FALSE
    )
    sntutils::compress_png(
      path = file.path(median_dir, glue::glue("Cases_v_rainfall_median_{region}.png")),
      verbosity = FALSE
    )
    cli::cli_alert_success("Median plot saved: {.val {region}}")
    p
  })

  loop_crude <- sort(unique(combined_crude[[loop_col]]))

  if (length(loop_crude) == 0) {
    cli::cli_warn(c(
      "No crude plots produced \u2014 combined_crude is empty.",
      "i" = "Check that {.arg crude_s_date}/{.arg crude_e_date} overlap with both datasets.",
      "i" = "Rainfall runs {.val {format(min(rain_df_full$date), '%Y-%m')}} \u2013 {.val {format(max(rain_df_full$date), '%Y-%m')}}.",
      "i" = "Cases run {.val {format(min(case_df$date), '%Y-%m')}} \u2013 {.val {format(max(case_df$date), '%Y-%m')}}."
    ))
  }

  crude_plots <- purrr::map(purrr::set_names(loop_crude), function(region) {
    region_data <- combined_crude |> dplyr::filter(.data[[loop_col]] == region)

    cases_in_window <- any(!is.na(region_data$conf_ov5) | !is.na(region_data$conf_u5))
    if (!cases_in_window) {
      cli::cli_warn(c(
        "Crude plot for {.val {region}}: no case data in the selected date window.",
        "i" = "Only rainfall will be visible. Adjust {.arg crude_s_date} if needed."
      ))
    }
    n_panels <- dplyr::n_distinct(region_data[[panel_col]])
    ppr      <- .adapt_panels_per_row(n_panels, panels_per_row)
    p        <- .build_crude_adm1_plot(
      data           = region_data,
      loop_name      = region,
      loop_col       = loop_col,
      panel_col      = panel_col,
      panels_per_row = ppr,
      lbl_ov5        = lbl_ov5,
      lbl_u5         = lbl_u5,
      lbl_rainfall   = lbl_rainfall,
      lbl_cases_y    = lbl_cases_y,
      lbl_rain_mm    = lbl_rain_mm
    )
    n_rows <- ceiling(n_panels / ppr)

    ggplot2::ggsave(
      filename  = glue::glue("Cases_v_rainfall_crude_{region}.png"),
      plot      = p, path = crude_dir,
      width = ppr * 5, height = 3.5 * n_rows + 1.5,
      dpi = dpi, bg = "white", limitsize = FALSE
    )
    sntutils::compress_png(
      path = file.path(crude_dir, glue::glue("Cases_v_rainfall_crude_{region}.png")),
      verbosity = FALSE
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

#' \code{theme_void()} base, bottom horizontal legend, strip text with
#' breathing room, tight plot margins.
#' Font-size defaults match \code{facetted_maps.R} from the sntutils toolkit
#' (title=14, subtitle=11, legend_title=10).
#' @keywords internal
.snt_theme_map <- function(title_size = 17, subtitle_size = 13,
                           legend_title_size = 15, legend_text_size = 13) {
  ggplot2::theme_void() +
    ggplot2::theme(
      plot.title    = ggplot2::element_text(
        size   = title_size,
        face   = "bold",
        # No hjust override: theme_void default (hjust=0) is kept.
        # Left-alignment + scale parameter = enough canvas → no clipping.
        margin = ggplot2::margin(b = 8)
      ),
      plot.subtitle = ggplot2::element_text(
        size   = subtitle_size,
        color  = "grey30",
        margin = ggplot2::margin(b = 10)
      ),
      plot.caption  = ggplot2::element_text(
        size   = 8,
        hjust  = 1,
        color  = "grey50",
        margin = ggplot2::margin(t = 6)
      ),
      legend.position      = "bottom",
      legend.direction     = "horizontal",
      legend.title         = ggplot2::element_text(
        size   = legend_title_size,
        face   = "bold",
        margin = ggplot2::margin(b = 6)
      ),
      legend.text          = ggplot2::element_text(size = legend_text_size),
      legend.box.margin    = ggplot2::margin(t = 8),
      strip.text   = ggplot2::element_text(
        face   = "bold",
        size   = 10,
        margin = ggplot2::margin(t = 2, b = 6, l = 4, r = 4)
      ),
      strip.text.y = ggplot2::element_text(angle = -90),
      panel.spacing = grid::unit(4, "pt"),
      plot.margin = ggplot2::margin(t = 5, r = 5, b = 5, l = 5)
    )
}


#' Derive map output dimensions from a country's bounding box
#'
#' Returns a named numeric vector with three elements: \code{width}, \code{height}
#' (the ggplot coordinate-space dimensions in inches), and \code{scale} (a
#' multiplier passed to \code{ggsave()}).
#'
#' Using \code{scale > 1} is the technique borrowed from \code{facetted_maps.R}
#' in the sntutils toolkit.  The ggplot is designed in \code{width × height}
#' inches; \code{ggsave()} then renders it onto a canvas that is
#' \code{width * scale × height * scale} inches.  Text elements stay at their
#' pt sizes relative to the ggplot coordinates, but the larger canvas means
#' long titles no longer clip at the edge — the critical fix for narrow
#' countries such as Togo, Malawi, and Guinea.
#'
#' @keywords internal
.compute_map_dims <- function(shp, max_long_side = 11,
                              min_width = 7, min_height = 5,
                              extra_height = 2.5) {
  bb      <- sf::st_bbox(shp)
  lon_ext <- as.numeric(bb["xmax"] - bb["xmin"])
  lat_ext <- as.numeric(bb["ymax"] - bb["ymin"])

  if (lon_ext <= 0 || lat_ext <= 0) return(c(width = 7, height = 10, scale = 1))

  aspect <- lon_ext / lat_ext

  if (aspect >= 1) {
    width  <- max_long_side
    height <- max_long_side / aspect
  } else {
    height <- max_long_side
    width  <- max_long_side * aspect
  }

  width  <- max(min_width, width)
  height <- max(min_height, height) + extra_height

  # ── scale factor (the facetted_maps.R technique) ───────────────────────────
  # For narrow/tall countries, scale > 1 enlarges the saved canvas relative to
  # the ggplot coordinate space.  This gives long translated titles (French,
  # Portuguese, Arabic) room to breathe without needing to reduce the font size
  # to illegibility.  Wider countries need no scaling.
  plot_scale <- dplyr::case_when(
    aspect < 0.30 ~ 1.5,   # very narrow (Togo, Malawi)
    aspect < 0.50 ~ 1.3,   # moderately narrow (Guinea, Niger)
    aspect < 0.75 ~ 1.1,   # slightly taller than wide
    TRUE          ~ 1.0    # roughly square or wider
  )

  c(width = round(width, 1), height = round(height, 1), scale = plot_scale)
}


#' Build a single SNT choropleth map (company standard)
#'
#' @param dims Optional named numeric vector \code{c(width, height)} produced by
#'   \code{.compute_map_dims()}.  When supplied, font sizes and the legend row
#'   count are scaled to fit the canvas — important for narrow countries where
#'   a fixed nrow=1 legend overflows the map width.
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
                           na_value      = "grey92",
                           dims          = NULL) {

  # ── responsive typography ──────────────────────────────────────────────────
  # Base sizes match facetted_maps.R (title=14, subtitle=11, legend_title=10).
  # We scale them relative to a 7-inch reference width (the reference default).
  # The ggsave() scale parameter handles the canvas enlargement that prevents
  # title overflow — so font sizes here can stay at the reference values.
  canvas_width <- if (!is.null(dims)) dims[["width"]] else 7
  text_scale   <- min(1, canvas_width / 7)
  title_size         <- max(11, round(14 * text_scale))
  subtitle_size      <- max(9,  round(11 * text_scale))
  legend_title_size  <- max(8,  round(10 * text_scale))
  legend_text_size   <- max(8,  round(9  * text_scale))

  # ── responsive legend layout ───────────────────────────────────────────────
  # nrow=1 works for wide maps. For narrow countries the legend wraps to keep
  # all items visible. The effective saved canvas is width * scale inches, so
  # compare against that — not just the ggplot coordinate width.
  n_items      <- length(fill_values)
  effective_w  <- if (!is.null(dims)) dims[["width"]] * (dims[["scale"]] %||% 1) else 7
  legend_nrow  <- if (effective_w < 9) {
    max(1L, ceiling(n_items / 3L))
  } else {
    1L
  }

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
        nrow           = legend_nrow,
        byrow          = TRUE
      )
    ) +
    ggplot2::coord_sf(datum = NA) +
    ggplot2::labs(title = title, subtitle = subtitle, caption = caption) +
    .snt_theme_map(
      title_size        = title_size,
      subtitle_size     = subtitle_size,
      legend_title_size = legend_title_size,
      legend_text_size  = legend_text_size
    )
}


#' Save a map with responsive dimensions, scale factor, and PNG compression
#'
#' The \code{scale} element in \code{dims} (from \code{.compute_map_dims()}) is
#' passed to \code{ggsave()} following the \code{facetted_maps.R} pattern.
#' When scale > 1 the saved canvas is larger than the ggplot coordinate space,
#' so text elements designed for e.g. 7 inches are rendered on 10.5 inches —
#' the key technique that prevents long translated titles from clipping.
#' @keywords internal
.save_map <- function(plot, path, dims, dpi = 300) {
  plot_scale <- if (!is.null(dims[["scale"]])) dims[["scale"]] else 1
  ggplot2::ggsave(
    filename  = path,
    plot      = plot,
    width     = dims[["width"]],
    height    = dims[["height"]],
    dpi       = dpi,
    scale     = plot_scale,
    bg        = "white",
    limitsize = FALSE
  )
  sntutils::compress_png(path = path, verbosity = FALSE)
  cli::cli_alert_success("Map saved \u2192 {.path { .relative_path(path)}}")
  invisible(path)
}

#' Categorise median_max_prop into 6 coverage bands
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

#' Build a single panel for the median cases vs rainfall plot
#'
#' Accepts pre-translated label strings and dynamic column names so the same
#' function works at adm2 or adm3 panel level without duplication.
#'
#' @keywords internal
.build_median_adm2_plot <- function(data, panel_name,
                                    panel_col    = "adm2",
                                    show_y_left  = TRUE,
                                    show_y_right = TRUE,
                                    lbl_ov5      = "Cases >5 years",
                                    lbl_u5       = "Cases <5 years",
                                    lbl_rainfall = "Rainfall",
                                    lbl_med_y    = "Cases confirmed (median)",
                                    lbl_rain_mm  = "Rainfall (mm)") {
  d             <- data |> dplyr::filter(.data[[panel_col]] == panel_name)
  max_cases     <- max(c(d$conf_ov5, d$conf_u5), na.rm = TRUE)
  max_rain      <- max(d$total_rainfall_mm, na.rm = TRUE)
  scale_factor  <- if (max_rain > 0) max_cases / max_rain else 1

  ggplot2::ggplot(d, ggplot2::aes(x = month)) +
    ggplot2::geom_line(ggplot2::aes(y = total_rainfall_mm * scale_factor,
                                    color = lbl_rainfall), linewidth = 1.2) +
    ggplot2::geom_line(ggplot2::aes(y = conf_ov5, color = lbl_ov5), linewidth = 1.2) +
    ggplot2::geom_line(ggplot2::aes(y = conf_u5,  color = lbl_u5),  linewidth = 1.2) +
    ggplot2::scale_y_continuous(
      name = if (show_y_left) lbl_med_y else NULL,
      labels = scales::comma,
      sec.axis = ggplot2::sec_axis(
        transform = ~ . / scale_factor,
        name = if (show_y_right) lbl_rain_mm else NULL,
        labels = scales::comma
      )
    ) +
    ggplot2::scale_x_continuous(breaks = 1:12, labels = 1:12) +
    ggplot2::scale_color_manual(
      name   = "",
      values = stats::setNames(
        c("#E74C3C", "#F4D03F", "#3498DB"),
        c(lbl_ov5, lbl_u5, lbl_rainfall)
      )
    ) +
    ggplot2::ggtitle(panel_name) +
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

#' Build a faceted median plot for one loop region
#' @keywords internal
.build_median_adm1_plot <- function(data, loop_name,
                                    loop_col       = "adm1",
                                    panel_col      = "adm2",
                                    panels_per_row = 3,
                                    lbl_ov5        = "Cases >5 years",
                                    lbl_u5         = "Cases <5 years",
                                    lbl_rainfall   = "Rainfall",
                                    lbl_med_y      = "Cases confirmed (median)",
                                    lbl_rain_mm    = "Rainfall (mm)") {
  loop_data    <- data |> dplyr::filter(.data[[loop_col]] == loop_name)
  panel_units  <- sort(unique(loop_data[[panel_col]]))
  n            <- length(panel_units)

  panel_list <- purrr::imap(
    panel_units,
    \(pname, idx) .build_median_adm2_plot(
      data         = loop_data,
      panel_name   = pname,
      panel_col    = panel_col,
      show_y_left  = (idx %% panels_per_row == 1),
      show_y_right = (idx %% panels_per_row == 0 | idx == n),
      lbl_ov5      = lbl_ov5,
      lbl_u5       = lbl_u5,
      lbl_rainfall = lbl_rainfall,
      lbl_med_y    = lbl_med_y,
      lbl_rain_mm  = lbl_rain_mm
    )
  )

  legend_plot <- .build_median_adm2_plot(
    loop_data, panel_units[1],
    panel_col    = panel_col,
    lbl_ov5      = lbl_ov5, lbl_u5 = lbl_u5, lbl_rainfall = lbl_rainfall,
    lbl_med_y    = lbl_med_y, lbl_rain_mm = lbl_rain_mm
  ) +
    ggplot2::theme(legend.position = "bottom",
                   legend.text = ggplot2::element_text(size = 12, face = "bold"))
  shared_legend <- cowplot::get_legend(legend_plot)

  combined <- patchwork::wrap_plots(panel_list, ncol = panels_per_row) +
    patchwork::plot_annotation(
      title = toupper(loop_name),
      theme = ggplot2::theme(
        plot.title = ggplot2::element_text(hjust = 0.5, face = "bold", size = 14)
      )
    )

  cowplot::plot_grid(combined, shared_legend, ncol = 1, rel_heights = c(1, 0.06))
}

#' Build a single panel for the crude time-series cases vs rainfall plot
#'
#' Accepts pre-translated label strings and dynamic column names so the same
#' function works at adm2 or adm3 panel level without duplication.
#'
#' @keywords internal
.build_crude_adm2_plot <- function(data, panel_name,
                                   panel_col    = "adm2",
                                   show_y_left  = TRUE,
                                   show_y_right = TRUE,
                                   lbl_ov5      = "Cases >5 years",
                                   lbl_u5       = "Cases <5 years",
                                   lbl_rainfall = "Rainfall",
                                   lbl_cases_y  = "Cases confirmed",
                                   lbl_rain_mm  = "Rainfall (mm)") {
  d            <- data |> dplyr::filter(.data[[panel_col]] == panel_name)
  max_cases    <- suppressWarnings(max(c(d$conf_ov5, d$conf_u5), na.rm = TRUE))
  max_rain     <- suppressWarnings(max(d$total_rainfall_mm,       na.rm = TRUE))

  cases_present <- is.finite(max_cases) && max_cases > 0
  scale_factor  <- if (is.finite(max_rain) && max_rain > 0 && cases_present) {
    max_cases / max_rain
  } else if (is.finite(max_rain) && max_rain > 0) {
    1
  } else {
    1
  }

  ggplot2::ggplot(d, ggplot2::aes(x = date)) +
    ggplot2::geom_line(ggplot2::aes(y = total_rainfall_mm * scale_factor,
                                    color = lbl_rainfall), linewidth = 1.0, na.rm = TRUE) +
    ggplot2::geom_line(ggplot2::aes(y = conf_ov5, color = lbl_ov5),
                       linewidth = 1.0, na.rm = TRUE) +
    ggplot2::geom_line(ggplot2::aes(y = conf_u5,  color = lbl_u5),
                       linewidth = 1.0, na.rm = TRUE) +
    ggplot2::scale_y_continuous(
      name = if (show_y_left) lbl_cases_y else NULL,
      labels = scales::comma,
      sec.axis = ggplot2::sec_axis(
        transform = ~ . / scale_factor,
        name = if (show_y_right) lbl_rain_mm else NULL,
        labels = scales::comma
      )
    ) +
    ggplot2::scale_x_date(
      date_breaks = "6 months", date_labels = "%b %Y",
      expand = ggplot2::expansion(mult = c(0, 0.01))
    ) +
    ggplot2::scale_color_manual(
      name   = "",
      values = stats::setNames(
        c("#E74C3C", "#F4D03F", "#3498DB"),
        c(lbl_ov5, lbl_u5, lbl_rainfall)
      )
    ) +
    ggplot2::ggtitle(panel_name) +
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

#' Build a faceted crude time-series plot for one loop region
#' @keywords internal
.build_crude_adm1_plot <- function(data, loop_name,
                                   loop_col       = "adm1",
                                   panel_col      = "adm2",
                                   panels_per_row = 3,
                                   lbl_ov5        = "Cases >5 years",
                                   lbl_u5         = "Cases <5 years",
                                   lbl_rainfall   = "Rainfall",
                                   lbl_cases_y    = "Cases confirmed",
                                   lbl_rain_mm    = "Rainfall (mm)") {
  loop_data   <- data |> dplyr::filter(.data[[loop_col]] == loop_name)
  panel_units <- sort(unique(loop_data[[panel_col]]))
  n           <- length(panel_units)

  panel_list <- purrr::imap(
    panel_units,
    ~ .build_crude_adm2_plot(
      data         = loop_data,
      panel_name   = .x,
      panel_col    = panel_col,
      show_y_left  = (.y %% panels_per_row == 1),
      show_y_right = (.y %% panels_per_row == 0 || .y == n),
      lbl_ov5      = lbl_ov5,
      lbl_u5       = lbl_u5,
      lbl_rainfall = lbl_rainfall,
      lbl_cases_y  = lbl_cases_y,
      lbl_rain_mm  = lbl_rain_mm
    )
  )

  legend_plot <- .build_crude_adm2_plot(
    loop_data, panel_units[1],
    panel_col   = panel_col,
    lbl_ov5     = lbl_ov5, lbl_u5 = lbl_u5, lbl_rainfall = lbl_rainfall,
    lbl_cases_y = lbl_cases_y, lbl_rain_mm = lbl_rain_mm
  ) +
    ggplot2::theme(legend.position = "bottom",
                   legend.text = ggplot2::element_text(size = 12, face = "bold"))
  shared_legend <- cowplot::get_legend(legend_plot)

  combined <- patchwork::wrap_plots(panel_list, ncol = panels_per_row) +
    patchwork::plot_annotation(
      title = toupper(loop_name),
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
      # paths$cache is the standard SNT cache folder; may be NULL on older
      # sntutils versions that don't expose it yet.
      cache               = if (!is.null(paths$cache)) paths$cache else NULL,
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
      cache               = if (!is.null(p$cache)) p$cache else NULL,
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
      cache               = NULL,   # no auto-cache when using direct paths
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
    # This lookup is only called when analysis_level != "adm3", i.e. at adm2
    # level where district = paste(adm1, adm2, sep = " - ").
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

#' Return the admin column names up to and including the analysis level
#'
#' Used throughout the pipeline to build dynamic grouping keys so that the
#' same code supports adm1, adm2, and adm3 analysis without hardcoding.
#' @keywords internal
.level_cols <- function(analysis_level) {
  switch(analysis_level,
    adm1 = "adm1",
    adm2 = c("adm1", "adm2"),
    adm3 = c("adm1", "adm2", "adm3")
  )
}

#' NULL coalescing operator
#' @keywords internal
`%||%` <- function(x, y) if (!is.null(x)) x else y

#' Load and aggregate raw source data to adm/year/month level
#' @keywords internal
.load_analysis_data <- function(
    dirs, iso3, type, adm1_var, adm2_var, year_var, month_var, value_var,
    adm3_var = NULL, analysis_level = "adm2"
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

  # adm3 is only required from the source data when actually analysing at adm3
  # level. When analysis_level is adm1 or adm2, adm3_var is ignored here even
  # if it was supplied (it is only used as a label join in the block step).
  need_adm3 <- !is.null(adm3_var) && analysis_level == "adm3"
  adm_vars  <- c(adm1_var, adm2_var, if (need_adm3) adm3_var)
  required  <- c(adm_vars, year_var, month_var, cfg$col_src)
  missing   <- setdiff(required, names(df))
  if (length(missing) > 0) {
    cli::cli_abort(c(
      "Missing column{?s} in {.val {cfg$filename}} ({type} data):",
      "x" = "{.var {missing}}",
      "i" = "Columns present in your file: {.val {names(df)}}",
      "i" = "Use the {.arg *_var} arguments to map your column names, e.g.:",
      "i" = "  {.code adm1_var = 'province'} if your region column is called 'province'",
      "i" = "  {.code adm3_var = 'sector'}   if your sub-district column is called 'sector'",
      "i" = "  {.code value_var = 'cases'}   if your metric column is called 'cases'"
    ))
  }

  if (!is.numeric(df[[cfg$col_src]])) {
    cli::cli_abort(c(
      "Column {.var {cfg$col_src}} must be numeric.",
      "x" = "Found: {.cls {class(df[[cfg$col_src]])}}.",
      "i" = "If this is a character column (e.g. '1,234'), remove thousand-separator commas",
      "i" = "before running the pipeline, or coerce it with {.code as.numeric()}."
    ))
  }

  n_neg <- sum(df[[cfg$col_src]] < 0, na.rm = TRUE)
  if (n_neg > 0) {
    cli::cli_abort(c(
      "{n_neg} negative value{?s} in {.var {cfg$col_src}}.",
      "x" = "Metric values must be \u2265 0.",
      "i" = "Check for data entry errors or coding of missing data as -999 / -1.",
      "i" = "Replace negatives with NA or 0 before running the pipeline."
    ))
  }

  # Base rename: adm1, adm2, year, month
  df_out <- df |>
    dplyr::select(dplyr::all_of(required)) |>
    dplyr::rename(
      adm1  = !!adm1_var,
      adm2  = !!adm2_var,
      year  = !!year_var,
      month = !!month_var
    )

  # Conditionally rename adm3 to a standard name (only at adm3 analysis level)
  if (need_adm3) {
    df_out <- df_out |> dplyr::rename(adm3 = !!adm3_var)
  }

  # NA filter on structural columns present in the data
  struct_cols <- c("adm1", "adm2", if (need_adm3) "adm3", "year", "month")
  n_before_na <- nrow(df_out)
  df_out <- df_out |>
    dplyr::filter(dplyr::if_all(dplyr::all_of(struct_cols), ~ !is.na(.x)))
  n_dropped_na <- n_before_na - nrow(df_out)
  if (n_dropped_na > 0) {
    cli::cli_warn(c(
      "Dropped {n_dropped_na} row{?s} with NA in structural columns ({.val {struct_cols}}).",
      "i" = "{n_before_na} \u2192 {nrow(df_out)} rows remaining.",
      "i" = "This usually means incomplete records — check your source file for missing",
      "i" = "admin names, years, or months."
    ))
  }

  # Group by all admin levels that are present in the data
  grp_cols <- c("adm1", "adm2", if (need_adm3) "adm3", "year", "month")
  df_agg <- df_out |>
    dplyr::group_by(dplyr::across(dplyr::all_of(grp_cols))) |>
    dplyr::summarise(
      value = sum(.data[[cfg$col_src]], na.rm = TRUE),
      .groups = "drop"
    )

  n_units  <- dplyr::n_distinct(df_agg[[ tail(.level_cols(analysis_level), 1) ]])
  n_years  <- dplyr::n_distinct(df_agg$year)
  yr_range <- paste0(min(df_agg$year), "\u2013", max(df_agg$year))
  cli::cli_alert_success(
    "Loaded {.val {type}}: {nrow(df_agg)} row{?s} | {n_units} {analysis_level} unit{?s} | {n_years} year{?s} ({yr_range})"
  )

  df_agg
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


# ==============================================================================
# PIPELINE ORCHESTRATION HELPERS — date-range alignment checks (Steps 4 & 6)
# ==============================================================================

#' Compare two year-range vectors and report whether they are aligned
#'
#' Returns a list with four elements:
#' \describe{
#'   \item{aligned}{Logical — TRUE when both ranges are identical.}
#'   \item{rain_range}{Integer vector c(min, max) for rainfall years.}
#'   \item{case_range}{Integer vector c(min, max) for case years.}
#'   \item{overlap_range}{Integer vector c(min, max) of the shared years, or
#'     \code{NULL} when there is no overlap at all.}
#' }
#' @keywords internal
.detect_year_mismatch <- function(rain_years, case_years) {
  if (length(rain_years) == 0 || length(case_years) == 0) {
    return(list(aligned = TRUE,
                rain_range = integer(0), case_range = integer(0),
                overlap_range = integer(0)))
  }
  rng_r <- c(min(rain_years), max(rain_years))
  rng_c <- c(min(case_years), max(case_years))
  overlap <- intersect(rain_years, case_years)
  rng_o   <- if (length(overlap) > 0) c(min(overlap), max(overlap)) else integer(0)

  list(
    aligned       = identical(rng_r, rng_c),
    rain_range    = rng_r,
    case_range    = rng_c,
    overlap_range = rng_o
  )
}


#' Interactive date-range alignment prompt (used before Steps 4 & 6)
#'
#' Prints a formatted mismatch banner and — when the session is interactive
#' and \code{interactive_checks = TRUE} — prompts the user to choose how to
#' handle the discrepancy.  Returns a list with \code{s_year} and \code{e_year}
#' (both \code{NULL} if the user wants to proceed as-is).
#'
#' @param align_info List returned by \code{.detect_year_mismatch()}.
#' @param step Integer. Step number (4 or 6) — used only in the banner label.
#' @keywords internal
.prompt_date_alignment <- function(align_info, step) {
  rr <- align_info$rain_range
  cr <- align_info$case_range
  ov <- align_info$overlap_range

  # ── print mismatch banner ──────────────────────────────────────────────────
  cli::cli_rule(left = glue::glue("Step {step} \u2014 date-range mismatch detected"))
  cli::cli_alert_warning(
    "Rainfall : {rr[1]}\u2013{rr[2]}  ({rr[2]-rr[1]+1} years)"
  )
  cli::cli_alert_warning(
    "Cases    : {cr[1]}\u2013{cr[2]}  ({cr[2]-cr[1]+1} years)"
  )
  if (length(ov) == 2) {
    cli::cli_alert_info(
      "Overlap  : {ov[1]}\u2013{ov[2]}  ({ov[2]-ov[1]+1} years shared)"
    )
  } else {
    cli::cli_alert_danger("No overlapping years \u2014 the two datasets do not share any years.")
  }

  # ── prompt (only when running in an interactive session) ──────────────────
  if (!interactive()) {
    cli::cli_warn(c(
      "Non-interactive session \u2014 cannot prompt.",
      "i" = "Proceeding as-is. Use {.arg graphs_s_year}/{.arg graphs_e_year} (Step 4) or",
      "i" = "{.arg case_rain_s_date}/{.arg case_rain_e_date} (Step 6) to clip manually."
    ))
    return(list(s_year = NULL, e_year = NULL))
  }

  if (length(ov) < 2) {
    cli::cli_warn("No overlap \u2014 cannot auto-clip. Please check your source data.")
    return(list(s_year = NULL, e_year = NULL))
  }

  cat("\nOptions:\n")
  cat(glue::glue("  [1] Auto-clip to overlap ({ov[1]}\u2013{ov[2]}) \u2014 recommended\n"))
  cat("  [2] Continue as-is (gaps/flat lines where data is missing)\n")
  cat("  [3] Enter custom year bounds now\n\n")

  raw <- trimws(readline("Choice [1]: "))
  choice <- if (nchar(raw) == 0) "1" else raw

  if (choice == "1") {
    cli::cli_alert_success("Clipping to overlap: {ov[1]}\u2013{ov[2]}.")
    return(list(s_year = ov[1], e_year = ov[2]))

  } else if (choice == "3") {
    s_raw <- trimws(readline(glue::glue("  Start year [{ov[1]}]: ")))
    e_raw <- trimws(readline(glue::glue("  End year   [{ov[2]}]: ")))
    s_yr  <- if (nchar(s_raw) == 0) ov[1] else as.integer(s_raw)
    e_yr  <- if (nchar(e_raw) == 0) ov[2] else as.integer(e_raw)
    cli::cli_alert_success("Custom bounds: {s_yr}\u2013{e_yr}.")
    return(list(s_year = s_yr, e_year = e_yr))

  } else {
    cli::cli_alert_info("Proceeding as-is.")
    return(list(s_year = NULL, e_year = NULL))
  }
}


# ==============================================================================
# PIPELINE ORCHESTRATION HELPERS — step cache (read / write)
# ==============================================================================

#' Build the cache file path for a given step and type
#' @keywords internal
.cache_file_path <- function(iso3, step, type = NULL, cache_dir) {
  tag  <- if (!is.null(type)) paste0(step, "_", type) else step
  file.path(cache_dir, paste0(toupper(iso3), "_pipe_cache_", tag, ".qs2"))
}

#' Save a pipeline step result to the cache directory
#'
#' Called automatically after each step completes when \code{pipeline_cache_dir}
#' is non-NULL.  The file is named \code{{ISO3}_pipe_cache_{step}_{type}.qs2}
#' (or \code{{ISO3}_pipe_cache_{step}.qs2} when \code{type} is NULL).
#'
#' Delete cache files manually to force a full recompute:
#' \code{file.remove(list.files(paths$cache, pattern = "pipe_cache", full.names = TRUE))}
#'
#' @keywords internal
.write_step_cache <- function(obj, iso3, step, type = NULL, cache_dir) {
  if (is.null(cache_dir) || is.null(obj)) return(invisible(NULL))
  .check_pkg("qs2", ".write_step_cache")
  path <- .cache_file_path(iso3, step, type, cache_dir)
  tryCatch({
    qs2::qsave(obj, path)
    tag <- if (!is.null(type)) glue::glue("{step}/{type}") else step
    cli::cli_alert_success(
      "Cached step {.val {tag}} \u2192 {.path { .relative_path(path)}}"
    )
  }, error = function(e) {
    cli::cli_warn(c(
      "Failed to write cache for step {.val {step}}.",
      "i" = "{e$message}",
      "i" = "The pipeline will continue normally; only caching is affected."
    ))
  })
  invisible(path)
}

#' Load a pipeline step result from the cache directory
#'
#' Returns the cached object, or \code{NULL} if no cache file exists.
#' A success message is printed when a cache hit occurs.
#'
#' @keywords internal
.read_step_cache <- function(iso3, step, type = NULL, cache_dir) {
  if (is.null(cache_dir)) return(NULL)
  path <- .cache_file_path(iso3, step, type, cache_dir)
  if (!file.exists(path)) return(NULL)
  .check_pkg("qs2", ".read_step_cache")
  obj <- tryCatch(qs2::qread(path), error = function(e) {
    cli::cli_warn(c(
      "Cache file found but could not be read for step {.val {step}}.",
      "i" = "{e$message}",
      "i" = "Proceeding with a fresh computation."
    ))
    NULL
  })
  if (!is.null(obj)) {
    tag <- if (!is.null(type)) glue::glue("{step}/{type}") else step
    mtime <- format(file.mtime(path), "%Y-%m-%d %H:%M")
    cli::cli_alert_info(
      "Step {.val {tag}}: loaded from cache (saved {mtime}) \u2014 skipping recompute."
    )
  }
  obj
}


# ==============================================================================
# TRANSLATION HELPER
# ==============================================================================

#' Translate a string if the target language is not English
#'
#' A thin wrapper around \code{sntutils::translate_text()} that short-circuits
#' immediately when \code{lang = "en"} (or \code{NULL}), avoiding unnecessary
#' API calls for the common English-output case.  All pipeline functions that
#' produce plot text call this helper rather than \code{translate_text()}
#' directly, so the translation backend and cache location are controlled from
#' a single place.
#'
#' @param text    Character scalar. The English source string to translate.
#' @param lang    Character scalar. BCP-47 language code for the target
#'   language (e.g. \code{"fr"}, \code{"pt"}, \code{"ar"}).  When
#'   \code{"en"} or \code{NULL} the original string is returned unchanged.
#' @param cache_path Character scalar. Directory used by
#'   \code{sntutils::translate_text()} for its persistent RDS cache.  Pass
#'   a stable project path (e.g. \code{here::here("translation_cache")}) so
#'   translations survive between R sessions.
#'
#' @return The translated string, or \code{text} unchanged when
#'   \code{lang = "en"} / \code{NULL} or when translation fails.
#' @keywords internal
.tr <- function(text, lang, cache_path) {
  if (is.null(lang) || identical(lang, "en")) return(text)
  sntutils::translate_text(
    text            = text,
    target_language = lang,
    source_language = "en",
    cache_path      = cache_path
  )
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
      row$adm3   <- if (length(parts) >= 3) parts[3] else NA_character_
      detailed   <- rbind(detailed, row)
    }
  }
  detailed
}

#' Yearly summary from detailed block results
#' @keywords internal
.build_yearly_summary <- function(detailed_results, analysis_level = "adm2") {
  # StartYear is already set in .calculate_seasonality() directly from
  # b$start_4m_year — no need to re-parse it from the DateRange string.
  lvl_cols <- .level_cols(analysis_level)
  detailed_results |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(lvl_cols, "StartYear")))) |>
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
    dplyr::arrange(Year, dplyr::across(dplyr::all_of(lvl_cols)))
}

#' Classify each location as Seasonal / Not Seasonal
#' @keywords internal
.build_location_summary <- function(yearly_summary, max_non_seasonal_years,
                                    analysis_level = "adm2") {
  lvl_cols <- .level_cols(analysis_level)
  yearly_summary |>
    dplyr::group_by(dplyr::across(dplyr::all_of(lvl_cols))) |>
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
    dplyr::arrange(dplyr::across(dplyr::all_of(lvl_cols)))
}

#' Build all seasonality maps (years count + binary + cumulative thresholds)
#' @keywords internal
.build_seasonality_maps <- function(
    dirs, iso3, adm0_name, type,
    location_summary, available_years,
    n_cumulative_maps, cumulative_thresholds,
    analysis_level = "adm2",
    fig_dir, dpi,
    lang, cache_path
) {
  spatial <- sntutils::read_snt_data(
    path         = here::here(dirs$admin_shp, "processed"),
    data_name    = glue::glue("{iso3}_shp_list"),
    file_formats = "qs2"
  )$final_spat_vec

  # Use the polygon layer that matches the analysis level.
  # The adm1 overlay always stays as the province boundary.
  analysis_sp <- spatial[[analysis_level]]
  adm1_sp     <- spatial$adm1

  # Join on ALL admin columns up to and including the analysis level, not just
  # the terminal level alone.  Joining only on e.g. "adm3" causes many-to-many
  # warnings (and duplicate rows) whenever the same adm3 name appears under
  # different adm2 parents — which is common in Rwanda and similar countries.
  join_cols <- intersect(.level_cols(analysis_level), names(location_summary))
  spatial_join_cols <- intersect(join_cols, names(analysis_sp))
  if (!identical(sort(join_cols), sort(spatial_join_cols))) {
    cli::cli_warn(c(
      "Shapefile at {.val {analysis_level}} level is missing some admin join columns.",
      "i" = "Expected: {.val {join_cols}}",
      "i" = "Found in shapefile: {.val {spatial_join_cols}}",
      "i" = "Falling back to joining on {.val {spatial_join_cols}} only.",
      "i" = "Check that your shapefile has matching adm1/adm2/adm3 columns."
    ))
    join_cols <- spatial_join_cols
  }
  merged <- analysis_sp |>
    dplyr::left_join(location_summary, by = join_cols, relationship = "many-to-one")
  type_label <- if (type == "rainfall") "Rainfall" else "Cases"
  yr_range   <- glue::glue("{min(available_years)}\u2013{max(available_years)}")
  n_years    <- length(available_years)

  dims <- .compute_map_dims(analysis_sp)

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

  # ── translate the "N year(s) (n=X)" legend labels ───────────────────────────
  # The singular/plural "year"/"years" pattern is built first in English then
  # translated as a complete phrase so the grammar is correct in the target
  # language rather than stitching translated fragments together.
  cat_lbl <- stats::setNames(
    sapply(names(cat_n), function(k) {
      eng <- paste0(k, " year",
                    ifelse(as.integer(k) != 1, "s", ""),
                    " (n=", cat_n[k], ")")
      .tr(eng, lang, cache_path)
    }),
    names(cat_n)
  )

  p1 <- .build_snt_map(
    adm2_sf     = merged,
    fill_col    = "category",
    fill_values = cat_pal,
    fill_labels = cat_lbl,
    adm1_sf     = adm1_sp,
    title       = .tr(
      glue::glue("Seasonality of {type_label} in {adm0_name}"),
      lang, cache_path
    ),
    subtitle    = .tr(
      glue::glue("Number of years with seasonal peaks eligible for SMC ({yr_range})"),
      lang, cache_path
    ),
    fill_label  = .tr(
      glue::glue("Years with seasonal {tolower(type_label)} peaks"),
      lang, cache_path
    ),
    dims        = dims
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

  # ── translate classification legend labels built with paste0 ─────────────────
  cls_lbl <- stats::setNames(
    c(
      .tr(paste0("SMC Seasonality (n=", cls_n["Seasonal"],     ")"), lang, cache_path),
      .tr(paste0("Non-Seasonal (n=",    cls_n["Not Seasonal"], ")"), lang, cache_path)
    )[seq_along(cls_n)],
    names(cls_n)
  )

  p2 <- .build_snt_map(
    adm2_sf     = merged,
    fill_col    = "Seasonality",
    fill_values = cls_pal[names(cls_pal) %in% names(cls_n)],
    fill_labels = cls_lbl,
    adm1_sf     = adm1_sp,
    title       = .tr(
      glue::glue("Malaria Seasonality Classification in {adm0_name}"),
      lang, cache_path
    ),
    subtitle    = .tr(
      glue::glue("Based on {tolower(type_label)} ({yr_range})"),
      lang, cache_path
    ),
    dims        = dims
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

    # ── translate cumulative legend labels ────────────────────────────────────
    t_lbl <- stats::setNames(
      c(
        .tr(paste0("SMC Seasonality (n=", t_n["Seasonal"],     ")"), lang, cache_path),
        .tr(paste0("Non-Seasonal (n=",    t_n["Not Seasonal"], ")"), lang, cache_path)
      )[seq_along(t_n)],
      names(t_n)
    )

    p_cumul <- .build_snt_map(
      adm2_sf     = merged_thresh,
      fill_col    = "Seasonality",
      fill_values = t_pal,
      fill_labels = t_lbl,
      adm1_sf     = adm1_sp,
      title       = .tr(
        glue::glue("Malaria Seasonality {thresh_label} years"),
        lang, cache_path
      ),
      subtitle    = .tr(
        glue::glue("Based on {tolower(type_label)} ({yr_range})"),
        lang, cache_path
      ),
      dims        = dims
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

#' Calculate 2m / 3m / 4m / 5m rolling concentration windows per district per year
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
        adm3 = if ("adm3" %in% names(d)) d$adm3[1] else NA_character_,
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
.build_block_frequency <- function(summary_tbl, detailed_tbl,
                                   analysis_level = "adm2") {
  freq <- data.frame()
  lvl_cols <- .level_cols(analysis_level)

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

  # Build adm lookup from whichever admin columns are present in summary_tbl
  adm_lookup <- summary_tbl |>
    dplyr::distinct(dplyr::across(dplyr::all_of(c("district", lvl_cols))))

  freq |>
    dplyr::left_join(adm_lookup, by = "district") |>
    dplyr::select(district, dplyr::all_of(lvl_cols), dplyr::everything())
}

#' Derive final SMC-eligible district list
#' @keywords internal
.derive_smc_eligibility <- function(
    location_summary,
    smc_eligible_districts,
    smc_additional_districts,
    smc_remove_districts,
    analysis_level = "adm2"
) {
  if (!is.null(smc_eligible_districts)) {
    eligible <- smc_eligible_districts
    cli::cli_alert_info(
      "SMC base list: user-supplied ({length(eligible)} district{?s})."
    )
  } else if (!is.null(location_summary)) {
    # Pull from whichever admin level the analysis was run at
    eligible <- location_summary[[analysis_level]][
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
#' of that year's annual total, across all districts and years.
#'
#' @inheritParams run_seasonality_pipeline
#' @param type Character. Data type to visualise. One of \code{"rainfall"}
#'   (default) or \code{"cases"}.
#' @param value_var Character or \code{NULL}. Specific metric column to map to
#'   fill colour. Default: \code{NULL}.
#' @param drop_na_cols Character vector. Structural columns that must be
#'   non-\code{NA}. Default: \code{c("adm1", "adm2", "year", "month")}.
#' @param drop_na_value Logical. Whether to also drop rows where the metric
#'   column is \code{NA}. Default: \code{TRUE}.
#' @param output_dir Character. Output folder for Option C paths. Default:
#'   \code{here::here("outputs", "figures")}.
#' @param filename Character or \code{NULL}. Output filename. Auto-generated
#'   when \code{NULL}. Default: \code{NULL}.
#' @param width Numeric. Plot width in inches. Default: \code{12}.
#' @param height Numeric. Plot height in inches. Default: \code{8}.
#' @param dpi Numeric. Plot resolution. Default: \code{500}.
#' @param lang Character or \code{NULL}. BCP-47 language code for all figure
#'   text. Use \code{"en"} or \code{NULL} (default) for English.
#' @param cache_path Character. Directory for the persistent translation cache.
#'   Default: \code{here::here("translation_cache")}.
#'
#' @return A named list invisibly: \code{plot}, \code{data},
#'   \code{output_path}.
#'
#' @examples
#' \dontrun{
#' paths <- sntutils::setup_project_paths()
#'
#' run_heatmap_analysis(
#'   iso3      = "gin",
#'   adm0_name = "Guinea",
#'   paths     = paths,
#'   type      = "rainfall"
#' )
#'
#' # French output
#' run_heatmap_analysis(
#'   iso3      = "gin",
#'   adm0_name = "Guinée",
#'   paths     = paths,
#'   type      = "rainfall",
#'   lang      = "fr"
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
    dpi         = 500,
    lang        = "en",
    cache_path  = here::here("translation_cache")
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
    x_breaks_by    = x_breaks_by,
    lang           = lang,
    cache_path     = cache_path
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

#' Build the heatmap ggplot object with translated text
#' @keywords internal
.build_heatmap_plot <- function(df, adm0_name, type, adm_level,
                                viridis_option, x_breaks_by,
                                lang = "en", cache_path = tempdir()) {

  legend_label <- .tr(
    switch(type, rainfall = "% Rainfall", cases = "% Cases"),
    lang, cache_path
  )
  title_label  <- .tr(
    switch(type,
      rainfall = glue::glue("{adm0_name}: Monthly Distribution of Rainfall"),
      cases    = glue::glue("{adm0_name}: Monthly Distribution of Cases")
    ),
    lang, cache_path
  )
  y_label <- .tr(
    switch(adm_level, adm1 = "Region", adm2 = "District"),
    lang, cache_path
  )
  x_label <- .tr("Month", lang, cache_path)

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
    ggplot2::labs(x = x_label, y = y_label, title = title_label) +
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
  sntutils::compress_png(path = output_path, verbosity = FALSE)
  cli::cli_alert_success("Heatmap saved: {.path { .relative_path(output_path)}}")
  invisible(output_path)
}