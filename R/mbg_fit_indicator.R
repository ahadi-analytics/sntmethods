#' Fit a Single MBG Indicator (Generic, Pluggable Admin Levels)
#'
#' Generic, low-level wrapper around \code{mbg::MbgModelRunner} that takes
#' a pre-built cluster-level table plus a population raster and any
#' subset of admin-0/1/2/3 shapefiles, fits a single MBG model and
#' returns a comprehensive list of in-memory artefacts (cluster table,
#' mean/lower/upper rasters, admin-level long-format tibbles, the fitted
#' \code{MbgModelRunner} object, the id raster, aggregation tables, the
#' cell-prediction draws matrix and an \code{inputs} echo of every
#' parameter the function received).
#'
#' This function is intentionally generic and is not tied to DHS. The
#' optional \code{country_iso3}, \code{survey_year} and \code{source_label}
#' arguments are pure annotations: when supplied they are echoed onto the
#' admin tibble and used to build pipeline-style filenames; when
#' \code{NULL} they are dropped from filenames and written as \code{NA} in
#' the admin tibble.
#'
#' If \code{output_dir} is \code{NULL} (the default) nothing is written to
#' disk -- everything is returned in memory in the result list. Set
#' \code{output_dir} to a directory path to also write rasters, the
#' cluster file, the long-format admin file (qs2 + xlsx) and a data
#' dictionary, mirroring the conventions of \code{run_mbg_pipeline()}.
#'
#' If \code{cache_dir} is non-\code{NULL} (or auto-derived from
#' \code{output_dir}), three artefacts are cached: the id raster
#' (\code{.tif}), the per-level aggregation table(s) (\code{.parquet}),
#' and the cell-prediction draws matrix (\code{.qs2}). On subsequent
#' calls with the same key the cell-prediction matrix is reloaded and
#' the (expensive) INLA model fit is skipped, allowing the user to
#' re-summarise to additional admin levels without refitting.
#'
#' @param cluster_data Any data-frame-like object (data.frame, tibble,
#'   data.table, sf with x/y columns). Coerced internally via
#'   \code{data.table::as.data.table()}. Must contain columns
#'   \code{cluster_id}, \code{x}, \code{y}, \code{indicator},
#'   \code{samplesize} (or supply alternative names via
#'   \code{cluster_cols}).
#' @param indicator_name Character. Short slug used in filenames and
#'   admin-tibble columns (e.g. \code{"pfpr_mic_2_10"}).
#' @param population_raster A \code{terra::SpatRaster} used as the
#'   template raster for prediction and as weights for population-
#'   weighted aggregation. Required.
#' @param adm0_sf,adm1_sf,adm2_sf,adm3_sf Optional \code{sf} polygon
#'   layers for each admin level. At least one must be supplied. The
#'   default modelling level is the highest provided (i.e. \code{adm3}
#'   if supplied, otherwise \code{adm2}, etc.).
#' @param primary_level Character, one of \code{"adm0"}, \code{"adm1"},
#'   \code{"adm2"}, \code{"adm3"}. The level the MBG model is fitted on
#'   and the level cell-predictions are aggregated to first. Defaults to
#'   the highest level for which a shapefile was supplied.
#' @param output_levels Character vector of admin levels to summarise to.
#'   Defaults to all admin levels for which a shapefile was supplied.
#' @param covariates Optional named list of \code{terra::SpatRaster}
#'   objects to use as covariates. If \code{NULL}, an intercept-only
#'   constant raster is built automatically (pure spatial smoothing).
#' @param pixel_size Numeric. Informational pixel size in degrees,
#'   echoed in \code{$inputs} for downstream reporting. The actual
#'   prediction grid is inherited from \code{population_raster}; this
#'   argument does NOT resample the raster. Default \code{0.04166667}
#'   (\eqn{\approx} 5 km at the equator).
#' @param n_samples Integer. Number of posterior draws drawn from the
#'   fitted model. Default \code{250}.
#' @param seed Integer. RNG seed for reproducibility. Default \code{1}.
#' @param cluster_cols Named list mapping the canonical cluster columns
#'   to user-supplied column names. Defaults to
#'   \code{list(cluster_id = "cluster_id", x = "x", y = "y", indicator
#'   = "indicator", samplesize = "samplesize")}.
#' @param id_field Character. Column name in the shapefiles holding the
#'   polygon id (default \code{"shapeID"}; falls back to a unique row
#'   index if not present).
#' @param indicator_title Optional human-readable label for the
#'   indicator, used in column headers / messaging. Defaults to a
#'   title-cased version of \code{indicator_name}.
#' @param indicator_unit_scale Numeric. Scaling factor applied when
#'   computing admin-level point estimates and CIs from the smoothed
#'   probability surface. Default \code{100} (i.e. percentages).
#' @param country_iso3 Optional ISO3 country code. If supplied,
#'   uppercased, included in pipeline-style filenames and echoed onto
#'   the admin tibble; \code{country_iso2} and \code{dhs_code} are then
#'   resolved automatically via \code{countrycode}. If \code{NULL}, the
#'   function attempts to reverse-geocode the median cluster coordinate
#'   (against \code{adm0_sf} when supplied, otherwise against
#'   \code{rnaturalearth}) and resolve all three codes from there.
#'   Default \code{NULL}.
#' @param survey_year Optional integer survey year. Default \code{NULL}.
#' @param source_label Optional character data source label
#'   (\code{"DHS"}, \code{"MIS"}, \code{"routine"}, \ldots). Generic
#'   replacement for the older \code{survey_type} argument. Default
#'   \code{NULL}.
#' @param output_dir Optional path. If \code{NULL} (default), nothing is
#'   written to disk. If supplied, \code{rasters/}, \code{cluster_data/}
#'   and \code{final_data/} subfolders are created and pipeline-style
#'   files are written.
#' @param cache_dir Optional path for cached id-raster, aggregation
#'   table(s) and cell-prediction matrix. If \code{NULL} and
#'   \code{output_dir} is supplied, defaults to \code{file.path(
#'   output_dir, "cache")}. If both are \code{NULL}, no caching is
#'   performed.
#' @param use_cache Logical. If \code{FALSE}, caches are ignored on
#'   read (but still written if \code{cache_dir} is set). Default
#'   \code{TRUE}.
#' @param overwrite Logical. If \code{TRUE}, ignore existing cell-
#'   prediction cache entries and always refit. Default \code{FALSE}.
#' @param return_draws Logical. If \code{TRUE}, the full
#'   \code{n_pixel x n_samples} draws matrix is returned in
#'   \code{$cell_predictions$draws}. Default \code{FALSE} (memory-
#'   conservative).
#' @param verbose Logical. If \code{TRUE}, print step-by-step CLI
#'   messages. Default \code{TRUE}.
#' @param ... Additional arguments forwarded to
#'   \code{mbg::build_aggregation_table()} and
#'   \code{mbg::MbgModelRunner$new()} (filtered by each function's
#'   formals so unrecognised names are silently ignored).
#'
#' @return A named list with elements:
#'   \describe{
#'     \item{\code{cluster_data}}{The cleaned input cluster table
#'       (\code{data.table}).}
#'     \item{\code{cell_predictions}}{Named list with \code{mean},
#'       \code{lower}, \code{upper} \code{SpatRaster} objects, and
#'       optionally \code{draws} (the full posterior matrix) when
#'       \code{return_draws = TRUE}.}
#'     \item{\code{admin}}{Named list of long-format \code{tibble}s,
#'       one per \code{output_levels} entry, with columns \code{
#'       indicator, indicator_title, admin_level, admin_id, admin_name,
#'       mean, lower, upper, population, country_iso3, country_iso2,
#'       dhs_code, survey_year, source_label}. \code{country_iso3} /
#'       \code{country_iso2} / \code{dhs_code} are populated either from
#'       a user-supplied \code{country_iso3} (resolved via
#'       \code{countrycode}) or by reverse-geocoding the median cluster
#'       coordinate against \code{adm0_sf} (or \code{rnaturalearth} if
#'       not supplied). They fall back to \code{NA} when neither lookup
#'       succeeds.}
#'     \item{\code{model_runner}}{The fitted \code{MbgModelRunner}
#'       object, or \code{NULL} if loaded from cache.}
#'     \item{\code{id_raster}}{The pixel-id raster used by
#'       \code{mbg::build_aggregation_table()}.}
#'     \item{\code{aggregation_tables}}{Named list of aggregation
#'       tables (one per output level).}
#'     \item{\code{saved_files}}{Named list of paths written to disk
#'       (empty when \code{output_dir} is \code{NULL}).}
#'     \item{\code{cache_files}}{Named list of paths to cache
#'       artefacts (empty when \code{cache_dir} is \code{NULL}).}
#'     \item{\code{inputs}}{Named list echoing every input parameter
#'       for reproducibility.}
#'   }
#'
#' @examples
#' \dontrun{
#' # Minimal in-memory call (no disk writes)
#' fit <- fit_mbg_indicator(
#'   cluster_data      = pfpr_dt,
#'   indicator_name    = "pfpr_mic_2_10",
#'   population_raster = pop_rast,
#'   adm1_sf           = adm1, adm2_sf = adm2
#' )
#' fit$cell_predictions$mean
#' fit$admin$adm2
#'
#' # Pipeline-style call with disk + cache
#' fit2 <- fit_mbg_indicator(
#'   cluster_data      = pfpr_dt,
#'   indicator_name    = "pfpr_mic_2_10",
#'   population_raster = pop_rast,
#'   adm1_sf = adm1, adm2_sf = adm2, adm3_sf = adm3,
#'   primary_level     = "adm3",
#'   output_levels     = c("adm1", "adm2", "adm3"),
#'   country_iso3      = "TGO",
#'   survey_year       = 2017,
#'   source_label      = "MIS",
#'   output_dir        = "outputs/mbg_fit"
#' )
#' }
#'
#' @export
fit_mbg_indicator <- function(
  cluster_data,
  indicator_name,
  population_raster,
  adm0_sf = NULL,
  adm1_sf = NULL,
  adm2_sf = NULL,
  adm3_sf = NULL,
  primary_level = NULL,
  output_levels = NULL,
  covariates = NULL,
  pixel_size = 0.04166667,
  n_samples = 250,
  seed = 1,
  cluster_cols = list(
    cluster_id = "cluster_id",
    x          = "x",
    y          = "y",
    indicator  = "indicator",
    samplesize = "samplesize"
  ),
  id_field = "shapeID",
  indicator_title = NULL,
  indicator_unit_scale = 100,
  country_iso3 = NULL,
  survey_year = NULL,
  source_label = NULL,
  output_dir = NULL,
  cache_dir = NULL,
  use_cache = TRUE,
  overwrite = FALSE,
  return_draws = FALSE,
  verbose = TRUE,
  ...
) {

  ## ---- 1) Validation + canonicalisation ------------------------------------

  .check_spatial_pkg("terra", "fit_mbg_indicator")
  .check_spatial_pkg("sf",    "fit_mbg_indicator")
  .check_spatial_pkg("fs",    "fit_mbg_indicator")

  if (!requireNamespace("mbg", quietly = TRUE)) {
    cli::cli_abort(c(
      "Package {.pkg mbg} is required by {.fun fit_mbg_indicator}.",
      "i" = "Install it with {.code remotes::install_github('ihmeuw/mbg')} (or your fork)."
    ))
  }

  if (is.null(indicator_name) || !nzchar(indicator_name)) {
    cli::cli_abort("{.arg indicator_name} must be a non-empty character string.")
  }

  if (is.null(population_raster) || !inherits(population_raster, "SpatRaster")) {
    cli::cli_abort("{.arg population_raster} must be a {.cls SpatRaster}.")
  }

  # Numeric scalar validation
  .check_pos_num <- function(x, nm) {
    if (length(x) != 1L || !is.numeric(x) || !is.finite(x) || x <= 0) {
      cli::cli_abort("{.arg {nm}} must be a single positive finite number; got {.val {x}}.")
    }
  }
  .check_pos_num(indicator_unit_scale, "indicator_unit_scale")
  .check_pos_num(pixel_size,           "pixel_size")
  if (length(n_samples) != 1L || !is.numeric(n_samples) ||
        !is.finite(n_samples) || n_samples < 1) {
    cli::cli_abort("{.arg n_samples} must be a single positive integer; got {.val {n_samples}}.")
  }
  n_samples <- as.integer(n_samples)
  if (length(seed) != 1L || !is.numeric(seed) || !is.finite(seed)) {
    cli::cli_abort("{.arg seed} must be a single finite numeric value.")
  }

  # Collect supplied admin shapefiles
  admin_inputs <- list(
    adm0 = adm0_sf, adm1 = adm1_sf, adm2 = adm2_sf, adm3 = adm3_sf
  )
  admin_inputs <- admin_inputs[!vapply(admin_inputs, is.null, logical(1))]
  if (length(admin_inputs) == 0) {
    cli::cli_abort(
      "Provide at least one shapefile via {.arg adm0_sf}/{.arg adm1_sf}/{.arg adm2_sf}/{.arg adm3_sf}."
    )
  }
  for (lvl in names(admin_inputs)) {
    if (!inherits(admin_inputs[[lvl]], "sf")) {
      cli::cli_abort("{.arg {lvl}_sf} must be an {.cls sf} object.")
    }
  }

  # CRS consistency check across all spatial inputs. We treat NA CRS as
  # "unknown" and only error when two inputs disagree on a known CRS.
  pop_crs <- tryCatch(terra::crs(population_raster, proj = TRUE), error = function(e) NA_character_)
  .canon_crs <- function(x) {
    if (is.null(x) || is.na(x) || !nzchar(x)) return(NA_character_)
    out <- tryCatch(sf::st_crs(x)$proj4string, error = function(e) NA_character_)
    if (is.null(out) || is.na(out)) NA_character_ else out
  }
  pop_canon <- .canon_crs(pop_crs)
  for (lvl in names(admin_inputs)) {
    sf_crs <- tryCatch(sf::st_crs(admin_inputs[[lvl]])$proj4string,
                       error = function(e) NA_character_)
    sf_canon <- .canon_crs(sf_crs)
    if (!is.na(pop_canon) && !is.na(sf_canon) && !identical(pop_canon, sf_canon)) {
      cli::cli_abort(c(
        "CRS mismatch between {.arg population_raster} and {.arg {lvl}_sf}.",
        "i" = "Reproject inputs to a common CRS before calling {.fun fit_mbg_indicator}."
      ))
    }
  }

  # primary_level + output_levels resolution
  valid_levels <- c("adm0", "adm1", "adm2", "adm3")
  available_levels <- names(admin_inputs)            # ordered: adm0..adm3
  if (is.null(primary_level)) {
    primary_level <- utils::tail(available_levels, 1)
  }
  if (length(primary_level) != 1L || !primary_level %in% valid_levels) {
    cli::cli_abort(c(
      "{.arg primary_level} must be one of {.val {valid_levels}}.",
      "x" = "Got {.val {primary_level}}."
    ))
  }
  if (!primary_level %in% available_levels) {
    cli::cli_abort(c(
      "{.arg primary_level} = {.val {primary_level}} but no shapefile supplied for that level.",
      "i" = "Available levels: {.val {available_levels}}"
    ))
  }
  if (is.null(output_levels)) output_levels <- available_levels
  if (!all(output_levels %in% valid_levels)) {
    bad <- setdiff(output_levels, valid_levels)
    cli::cli_abort(c(
      "{.arg output_levels} contains invalid value(s): {.val {bad}}.",
      "i" = "Valid: {.val {valid_levels}}."
    ))
  }
  output_levels <- intersect(output_levels, available_levels)
  if (length(output_levels) == 0) {
    cli::cli_abort("None of the requested {.arg output_levels} have a shapefile supplied.")
  }

  # Optional metadata. iso2 / DHS code are populated either from the
  # user-supplied iso3 (via countrycode) or, when no iso3 is given, by
  # reverse-geocoding the median cluster coordinate -- see step 1b below.
  iso3_norm <- if (is.null(country_iso3) || !nzchar(country_iso3)) NULL else toupper(country_iso3)
  iso2_norm <- NULL
  dhs_norm  <- NULL
  year_norm <- if (is.null(survey_year)) NULL else as.integer(survey_year)
  src_norm  <- if (is.null(source_label) || !nzchar(source_label)) NULL else as.character(source_label)
  ind_title <- if (is.null(indicator_title)) {
    tools::toTitleCase(gsub("_", " ", indicator_name))
  } else {
    indicator_title
  }

  # Cluster-data coercion + column rename. Drop sf geometry first so
  # downstream code sees a flat table even when the user passes an sf
  # object with x/y columns.
  if (inherits(cluster_data, "sf")) {
    cluster_data <- sf::st_drop_geometry(cluster_data)
  }
  cd <- data.table::as.data.table(cluster_data)
  rename_map <- c(
    cluster_id = cluster_cols$cluster_id %||% "cluster_id",
    x          = cluster_cols$x          %||% "x",
    y          = cluster_cols$y          %||% "y",
    indicator  = cluster_cols$indicator  %||% "indicator",
    samplesize = cluster_cols$samplesize %||% "samplesize"
  )
  # Validate all source columns exist before any mutation, to give a
  # consistent error message and avoid partial in-place renames.
  missing_src <- vapply(
    rename_map, function(src) !src %in% names(cd), logical(1)
  )
  if (any(missing_src)) {
    bad <- rename_map[missing_src]
    cli::cli_abort(c(
      "Column(s) not found in {.arg cluster_data}:",
      stats::setNames(
        paste0("{.val ", bad, "}", " (cluster_cols$", names(bad), ")"),
        rep("x", length(bad))
      )
    ))
  }
  # Reject collisions where two source columns map to different canonical
  # names but one of those source columns is also a canonical name.
  for (canon in names(rename_map)) {
    src <- rename_map[[canon]]
    if (src == canon) next
    # If the canonical target already exists in the data and is not the
    # one being renamed away, the rename would silently overwrite it.
    if (canon %in% names(cd)) {
      cli::cli_abort(c(
        "Cluster column rename collision:",
        "x" = "{.val {src}} is set to be renamed to {.val {canon}} but a column called {.val {canon}} already exists in {.arg cluster_data}.",
        "i" = "Either rename / drop the existing {.val {canon}} column, or update {.arg cluster_cols} accordingly."
      ))
    }
  }
  for (canon in names(rename_map)) {
    src <- rename_map[[canon]]
    if (src != canon) data.table::setnames(cd, src, canon)
  }
  # Drop rows with missing coords / counts (use base subsetting so the
  # behaviour does not depend on whether data.table is attached to the
  # search path of the calling environment).
  keep <- !is.na(cd$x) & !is.na(cd$y) & cd$x != 0 & cd$y != 0 &
    !is.na(cd$indicator) & !is.na(cd$samplesize) & cd$samplesize > 0
  cd <- cd[keep, ]
  if (nrow(cd) == 0) {
    cli::cli_abort("After cleaning, {.arg cluster_data} has 0 usable rows.")
  }

  ## ---- 1b) Derive country metadata (iso3 / iso2 / DHS) from coords ---------
  #
  # When the caller does not supply country_iso3, attempt to reverse-geocode
  # the median cluster coordinate to a country. Median is robust to a
  # handful of mis-located GE clusters that DHS recodes occasionally
  # contain. Lookup prefers an explicit adm0_sf when supplied; otherwise
  # falls back to rnaturalearth. iso2 and the DHS 2-letter country code
  # are then resolved via countrycode.
  .derive_country_from_coords <- function(x, y, adm0_sf = NULL) {
    if (!requireNamespace("countrycode", quietly = TRUE)) return(list())
    med_x <- stats::median(x, na.rm = TRUE)
    med_y <- stats::median(y, na.rm = TRUE)
    if (!is.finite(med_x) || !is.finite(med_y) ||
          med_x < -180 || med_x > 180 ||
          med_y < -90  || med_y > 90) {
      return(list())
    }
    pt <- sf::st_sfc(sf::st_point(c(med_x, med_y)), crs = 4326)
    iso3 <- NA_character_
    if (!is.null(adm0_sf)) {
      a0 <- tryCatch(sf::st_transform(adm0_sf, 4326),
                     error = function(e) adm0_sf)
      hit <- tryCatch(
        suppressMessages(sf::st_intersects(pt, a0, sparse = FALSE))[1, ],
        error = function(e) logical(0)
      )
      if (length(hit) > 0 && any(hit)) {
        iso_col <- intersect(
          c("iso3", "ISO3", "ISO_A3", "ADM0_A3",
            "shapeGroup", "GID_0", "iso_a3"),
          names(a0)
        )[1]
        if (!is.na(iso_col)) {
          iso3 <- as.character(a0[[iso_col]][which(hit)[1]])
        }
      }
    }
    if ((is.na(iso3) || !nzchar(iso3)) &&
          requireNamespace("rnaturalearth", quietly = TRUE)) {
      world <- tryCatch(
        rnaturalearth::ne_countries(scale = "small", returnclass = "sf"),
        error = function(e) NULL
      )
      if (!is.null(world)) {
        hit <- tryCatch(
          suppressMessages(sf::st_intersects(pt, world, sparse = FALSE))[1, ],
          error = function(e) logical(0)
        )
        if (length(hit) > 0 && any(hit)) {
          iso3 <- as.character(world$iso_a3[which(hit)[1]])
        }
      }
    }
    if (is.na(iso3) || !nzchar(iso3)) return(list())
    iso2 <- suppressWarnings(countrycode::countrycode(
      iso3, origin = "iso3c", destination = "iso2c"))
    dhs  <- suppressWarnings(countrycode::countrycode(
      iso3, origin = "iso3c", destination = "dhs"))
    list(iso3 = iso3, iso2 = iso2, dhs = dhs)
  }

  if (is.null(iso3_norm)) {
    derived <- .derive_country_from_coords(cd$x, cd$y, adm0_sf)
    if (length(derived) > 0) {
      iso3_norm <- toupper(derived$iso3)
      iso2_norm <- if (!is.na(derived$iso2)) toupper(derived$iso2) else NULL
      dhs_norm  <- if (!is.na(derived$dhs))  toupper(derived$dhs)  else NULL
      if (isTRUE(verbose)) {
        cli::cli_alert_info(
          "Derived country from cluster coords: iso3 = {.val {iso3_norm}}, \\
          iso2 = {.val {iso2_norm %||% NA}}, dhs = {.val {dhs_norm %||% NA}}"
        )
      }
    }
  } else if (requireNamespace("countrycode", quietly = TRUE)) {
    i2 <- suppressWarnings(countrycode::countrycode(
      iso3_norm, origin = "iso3c", destination = "iso2c"))
    dh <- suppressWarnings(countrycode::countrycode(
      iso3_norm, origin = "iso3c", destination = "dhs"))
    if (!is.na(i2)) iso2_norm <- toupper(i2)
    if (!is.na(dh)) dhs_norm  <- toupper(dh)
  }

  ## ---- 2) Output / cache directories ---------------------------------------

  saved_dirs <- list()
  if (!is.null(output_dir)) {
    saved_dirs$rasters     <- fs::path(output_dir, "rasters")
    saved_dirs$cluster     <- fs::path(output_dir, "cluster_data")
    saved_dirs$final       <- fs::path(output_dir, "final_data")
    for (d in saved_dirs) fs::dir_create(d)
  }
  if (is.null(cache_dir) && !is.null(output_dir)) {
    cache_dir <- fs::path(output_dir, "cache")
  }
  if (!is.null(cache_dir)) fs::dir_create(cache_dir)

  ## ---- 3) Resolve primary shapefile + polygon id ---------------------------

  primary_sf <- admin_inputs[[primary_level]]
  primary_vect <- terra::vect(primary_sf)
  # Use a unique row index to avoid duplicate-name collisions. Pick a
  # fresh field name if .poly_id (or any candidate) is already in use.
  .pick_id_field <- function(vect) {
    candidates <- c(".poly_id",
                    paste0(".poly_id_", seq_len(20)))
    existing <- names(vect)
    free <- setdiff(candidates, existing)
    if (length(free) == 0) {
      cli::cli_abort("Could not allocate a unique polygon id field on the shapefile.")
    }
    free[1]
  }
  polygon_id_field <- .pick_id_field(primary_vect)
  primary_vect[[polygon_id_field]] <- seq_len(nrow(primary_vect))

  ## ---- 4) Build / load id raster (cached) ----------------------------------

  cache_files <- list()
  cache_key <- paste(
    iso3_norm   %||% "generic",
    primary_level,
    year_norm   %||% "any",
    sep = "_"
  )

  if (verbose) cli::cli_h2("MBG fit: {ind_title} ({primary_level})")

  id_raster_path <- NULL
  if (!is.null(cache_dir)) {
    id_raster_path <- fs::path(cache_dir, glue::glue("id_raster_{cache_key}.tif"))
  }

  build_id_raster <- utils::getFromNamespace("build_id_raster", "mbg")
  if (
    !is.null(id_raster_path) && isTRUE(use_cache) && !isTRUE(overwrite) &&
      fs::file_exists(id_raster_path)
  ) {
    if (verbose) cli::cli_alert_info("Using cached id raster ({cache_key})")
    id_raster <- terra::rast(id_raster_path)
  } else {
    if (verbose) cli::cli_alert_info("Building id raster ({cache_key}) ...")
    id_raster <- build_id_raster(
      polygons        = primary_vect,
      template_raster = population_raster
    )
    if (!is.null(id_raster_path)) {
      terra::writeRaster(id_raster, id_raster_path, overwrite = TRUE)
    }
  }
  if (!is.null(id_raster_path)) cache_files$id_raster <- as.character(id_raster_path)

  ## ---- 5) Covariates -------------------------------------------------------

  if (is.null(covariates)) {
    covariates <- list(intercept = terra::setValues(id_raster, 1))
  }
  if (!is.list(covariates) || is.null(names(covariates))) {
    cli::cli_abort("{.arg covariates} must be a named list of {.cls SpatRaster}.")
  }

  ## ---- 6) Build / load aggregation tables (cached, one per output level) --

  build_aggregation_table <- utils::getFromNamespace("build_aggregation_table", "mbg")
  agg_tables <- list()
  cache_files$agg_tables <- character()

  # Filter ... by formals of build_aggregation_table, excluding the
  # arguments we set explicitly below to avoid duplicated-argument errors
  # from do.call().
  dots <- list(...)
  if (is.null(names(dots))) names(dots) <- character(length(dots))
  if (any(names(dots) == "")) {
    cli::cli_abort(
      "All extra arguments passed via {.arg ...} must be named."
    )
  }
  agg_reserved <- c("polygons", "id_raster", "polygon_id_field", "verbose")
  agg_formals <- setdiff(names(formals(build_aggregation_table)), agg_reserved)
  agg_extra_args <- dots[intersect(names(dots), agg_formals)]

  for (lvl in output_levels) {
    lvl_sf   <- admin_inputs[[lvl]]
    lvl_vect <- terra::vect(lvl_sf)
    lvl_id_field <- .pick_id_field(lvl_vect)
    lvl_vect[[lvl_id_field]] <- seq_len(nrow(lvl_vect))

    agg_path <- NULL
    if (!is.null(cache_dir)) {
      agg_path <- fs::path(
        cache_dir, glue::glue("agg_table_{cache_key}_{lvl}.parquet")
      )
    }

    if (
      !is.null(agg_path) && isTRUE(use_cache) && !isTRUE(overwrite) &&
        fs::file_exists(agg_path)
    ) {
      if (verbose) cli::cli_alert_info("Using cached aggregation table ({lvl})")
      agg_tables[[lvl]] <- data.table::as.data.table(
        arrow::read_parquet(agg_path)
      )
    } else {
      if (verbose) cli::cli_alert_info("Building aggregation table ({lvl}) ...")
      agg_call_args <- c(
        list(
          polygons         = lvl_vect,
          id_raster        = id_raster,
          polygon_id_field = lvl_id_field,
          verbose          = isTRUE(verbose)
        ),
        agg_extra_args
      )
      agg_tables[[lvl]] <- do.call(build_aggregation_table, agg_call_args)
      if (!is.null(agg_path)) {
        arrow::write_parquet(agg_tables[[lvl]], agg_path)
      }
    }
    if (!is.null(agg_path)) {
      cache_files$agg_tables <- c(
        cache_files$agg_tables,
        stats::setNames(as.character(agg_path), lvl)
      )
    }
  }

  ## ---- 7) Fit MBG model (or load cached cell predictions) ------------------

  # Fingerprint covariates (names + cell sums) so cache invalidates when
  # the user swaps in different covariates.
  cov_fp <- {
    nms <- paste(sort(names(covariates) %||% ""), collapse = "|")
    sums <- vapply(
      covariates,
      function(r) {
        v <- tryCatch(
          sum(terra::values(r, mat = FALSE), na.rm = TRUE),
          error = function(e) NA_real_
        )
        if (!is.finite(v)) NA_real_ else v
      },
      numeric(1)
    )
    sums_str <- paste(sprintf("%.6g", sums), collapse = "|")
    substr(rlang::hash(paste(nms, sums_str, sep = "::")), 1, 8)
  }

  cell_pred_cache <- NULL
  if (!is.null(cache_dir)) {
    cell_pred_cache <- fs::path(
      cache_dir,
      glue::glue(
        "cell_pred_{cache_key}_{indicator_name}_n{n_samples}_seed{seed}_cov{cov_fp}.qs2"
      )
    )
  }

  cell_pred_loaded <- FALSE
  cell_preds <- list(mean = NULL, lower = NULL, upper = NULL, draws = NULL)
  model_runner <- NULL

  if (
    !is.null(cell_pred_cache) &&
      isTRUE(use_cache) && !isTRUE(overwrite) &&
      fs::file_exists(cell_pred_cache)
  ) {
    cached <- tryCatch(qs2::qs_read(cell_pred_cache), error = function(e) NULL)
    expected_n <- terra::ncell(id_raster)
    cache_ok <- !is.null(cached) &&
      is.numeric(cached$mean_vec)  && length(cached$mean_vec)  == expected_n &&
      is.numeric(cached$lower_vec) && length(cached$lower_vec) == expected_n &&
      is.numeric(cached$upper_vec) && length(cached$upper_vec) == expected_n
    draws_ok <- !isTRUE(return_draws) ||
      (is.matrix(cached$draws) && nrow(cached$draws) == expected_n)

    if (cache_ok && draws_ok) {
      if (verbose) cli::cli_alert_info("Using cached cell predictions ({indicator_name})")
      cell_preds$mean  <- terra::setValues(id_raster, cached$mean_vec)
      cell_preds$lower <- terra::setValues(id_raster, cached$lower_vec)
      cell_preds$upper <- terra::setValues(id_raster, cached$upper_vec)
      if (isTRUE(return_draws)) cell_preds$draws <- cached$draws
      cell_pred_loaded <- TRUE
    } else if (verbose) {
      reason <- if (!cache_ok) "shape mismatch" else "draws missing"
      cli::cli_alert_warning(
        "Cached cell predictions ignored ({reason}); refitting model."
      )
    }
  }

  if (!cell_pred_loaded) {
    MbgModelRunner <- utils::getFromNamespace("MbgModelRunner", "mbg")

    runner_reserved <- c(
      "input_data", "id_raster", "covariate_rasters",
      "aggregation_table", "aggregation_levels",
      "population_raster", "verbose"
    )
    init_fn <- MbgModelRunner$public_methods$initialize
    runner_formals <- if (is.function(init_fn)) {
      setdiff(names(formals(init_fn)), runner_reserved)
    } else {
      character()
    }
    runner_extra_args <- dots[intersect(names(dots), runner_formals)]

    # aggregation_levels: primary level cascades down to provided lower levels
    cascade_for <- function(lvl) {
      ordered <- c("adm3", "adm2", "adm1", "adm0")
      idx <- match(lvl, ordered)
      tail_levels <- ordered[idx:length(ordered)]
      intersect(tail_levels, available_levels)
    }
    agg_levels_runner <- list()
    cascade <- cascade_for(primary_level)
    if (length(cascade) == 0) cascade <- primary_level
    agg_levels_runner[[primary_level]] <- cascade

    if (verbose) cli::cli_alert_info("Fitting MBG model ...")
    runner_args <- c(
      list(
        input_data         = cd,
        id_raster          = id_raster,
        covariate_rasters  = covariates,
        aggregation_table  = agg_tables[[primary_level]],
        aggregation_levels = agg_levels_runner,
        population_raster  = population_raster,
        verbose            = isTRUE(verbose)
      ),
      runner_extra_args
    )
    model_runner <- do.call(MbgModelRunner$new, runner_args)

    if (isTRUE(verbose)) {
      model_runner$run_mbg_pipeline()
    } else {
      suppressMessages(invisible(utils::capture.output(
        model_runner$run_mbg_pipeline(), type = "output"
      )))
    }

    cp <- model_runner$grid_cell_predictions
    # Coerce to SpatRaster if needed
    .as_rast <- function(x) {
      if (is.null(x)) return(NULL)
      if (inherits(x, "SpatRaster")) x else terra::setValues(id_raster, as.vector(x))
    }
    cell_preds$mean  <- .as_rast(cp$cell_pred_mean)
    cell_preds$lower <- .as_rast(cp$cell_pred_lower)
    cell_preds$upper <- .as_rast(cp$cell_pred_upper)
    if (isTRUE(return_draws)) cell_preds$draws <- cp$cell_pred_draws

    # Cache cell predictions
    if (!is.null(cell_pred_cache)) {
      to_cache <- list(
        mean_vec  = terra::values(cell_preds$mean,  mat = FALSE),
        lower_vec = terra::values(cell_preds$lower, mat = FALSE),
        upper_vec = terra::values(cell_preds$upper, mat = FALSE),
        draws     = if (isTRUE(return_draws)) cp$cell_pred_draws else NULL,
        meta = list(
          indicator_name = indicator_name,
          n_samples = n_samples, seed = seed,
          cache_key = cache_key
        )
      )
      qs2::qs_save(to_cache, cell_pred_cache)
    }
  }

  if (!is.null(cell_pred_cache)) cache_files$cell_pred <- as.character(cell_pred_cache)

  ## ---- 8) Aggregate to each output level (long format) ---------------------

  multiplier <- as.numeric(indicator_unit_scale)

  admin_long <- list()
  for (lvl in output_levels) {
    lvl_sf <- admin_inputs[[lvl]]

    # Resolve admin name + id columns in the shapefile
    name_candidates <- c(
      lvl, paste0(toupper(lvl), "_NAME"), paste0(toupper(lvl), "_name"),
      "name", "NAME", "shapeName"
    )
    id_candidates <- c(
      id_field, paste0(toupper(lvl), "_PCODE"), paste0(toupper(lvl), "_ID"),
      "shapeID"
    )
    nm_col <- intersect(name_candidates, names(lvl_sf))[1]
    id_col <- intersect(id_candidates,   names(lvl_sf))[1]

    nm_vec <- if (!is.na(nm_col)) lvl_sf[[nm_col]] else seq_len(nrow(lvl_sf))
    id_vec <- if (!is.na(id_col)) lvl_sf[[id_col]] else seq_len(nrow(lvl_sf))

    pop_vals <- terra::extract(
      population_raster, lvl_sf, fun = sum, na.rm = TRUE
    )[[2]]
    pop_vals[is.na(pop_vals) | pop_vals <= 0] <- NA_real_

    # Population-weighted aggregation. Align population to the prediction
    # grid (resample if extents/resolutions differ) and use it as weights.
    pop_aligned <- tryCatch(
      terra::resample(population_raster, cell_preds$mean, method = "bilinear"),
      error = function(e) population_raster
    )
    .pop_weighted <- function(rast) {
      num <- terra::extract(rast * pop_aligned, lvl_sf,
                            fun = sum, na.rm = TRUE)[[2]]
      den <- terra::extract(pop_aligned, lvl_sf,
                            fun = sum, na.rm = TRUE)[[2]]
      out <- ifelse(is.na(den) | den <= 0, NA_real_, num / den)
      out
    }
    mean_vec  <- .pop_weighted(cell_preds$mean)  * multiplier
    lower_vec <- .pop_weighted(cell_preds$lower) * multiplier
    upper_vec <- .pop_weighted(cell_preds$upper) * multiplier

    admin_long[[lvl]] <- tibble::tibble(
      indicator       = indicator_name,
      indicator_title = ind_title,
      admin_level     = lvl,
      admin_id        = as.character(id_vec),
      admin_name      = as.character(nm_vec),
      mean            = mean_vec,
      lower           = lower_vec,
      upper           = upper_vec,
      population      = pop_vals,
      country_iso3    = if (is.null(iso3_norm)) NA_character_ else iso3_norm,
      country_iso2    = if (is.null(iso2_norm)) NA_character_ else iso2_norm,
      dhs_code        = if (is.null(dhs_norm))  NA_character_ else dhs_norm,
      survey_year     = if (is.null(year_norm)) NA_integer_  else year_norm,
      source_label    = if (is.null(src_norm))  NA_character_ else src_norm
    )
  }

  ## ---- 9) Optional disk writes (filenames mirror run_mbg_pipeline) ---------

  saved_files <- list()

  .sanitize_stem <- function(x) {
    if (is.null(x) || is.na(x)) return("")
    # Strip filesystem-illegal chars and collapse runs of separators.
    out <- gsub("[^A-Za-z0-9._-]+", "_", as.character(x))
    out <- gsub("_+", "_", out)
    out <- gsub("^_+|_+$", "", out)
    out
  }
  .stem <- function() {
    parts <- c(
      tolower(.sanitize_stem(iso3_norm)),
      tolower(.sanitize_stem(src_norm)),
      .sanitize_stem(year_norm),
      .sanitize_stem(indicator_name)
    )
    parts <- parts[!is.na(parts) & nzchar(parts)]
    stem_out <- paste(parts, collapse = "_")
    if (!nzchar(stem_out)) stem_out <- "mbg_output"
    stem_out
  }

  if (!is.null(output_dir)) {
    stem <- .stem()
    # Rasters
    raster_paths <- c(
      mean  = fs::path(saved_dirs$rasters, paste0(stem, "_mbg_mean.tif")),
      lower = fs::path(saved_dirs$rasters, paste0(stem, "_mbg_lower.tif")),
      upper = fs::path(saved_dirs$rasters, paste0(stem, "_mbg_upper.tif"))
    )
    terra::writeRaster(cell_preds$mean,  raster_paths[["mean"]],  overwrite = TRUE)
    terra::writeRaster(cell_preds$lower, raster_paths[["lower"]], overwrite = TRUE)
    terra::writeRaster(cell_preds$upper, raster_paths[["upper"]], overwrite = TRUE)
    # Preserve names when coercing fs_path -> character
    saved_files$rasters <- stats::setNames(
      as.character(raster_paths), names(raster_paths)
    )

    # Cluster file (qs2 + xlsx)
    cluster_basename <- paste0(stem, "_cluster_points")
    cluster_qs2 <- fs::path(saved_dirs$cluster,  paste0(cluster_basename, ".qs2"))
    cluster_xlsx <- fs::path(saved_dirs$cluster, paste0(cluster_basename, ".xlsx"))
    qs2::qs_save(tibble::as_tibble(cd), cluster_qs2)
    if (requireNamespace("writexl", quietly = TRUE)) {
      writexl::write_xlsx(tibble::as_tibble(cd), cluster_xlsx)
      saved_files$cluster_xlsx <- as.character(cluster_xlsx)
    }
    saved_files$cluster_qs2 <- as.character(cluster_qs2)

    # Final dataset (one xlsx with one tab per level + qs2 list)
    final_basename <- paste0(stem, "_mbg_indicators")
    final_qs2  <- fs::path(saved_dirs$final, paste0(final_basename, ".qs2"))
    final_xlsx <- fs::path(saved_dirs$final, paste0(final_basename, ".xlsx"))
    qs2::qs_save(admin_long, final_qs2)
    if (requireNamespace("writexl", quietly = TRUE)) {
      writexl::write_xlsx(admin_long, final_xlsx)
      saved_files$final_xlsx <- as.character(final_xlsx)
    }
    saved_files$final_qs2 <- as.character(final_qs2)

    # Data dictionary -- build on the union of all admin levels so the
    # dictionary reflects every column actually written.
    if (length(admin_long) > 0 &&
          requireNamespace("sntutils", quietly = TRUE)) {
      combined <- tryCatch(
        do.call(rbind, lapply(admin_long, tibble::as_tibble)),
        error = function(e) admin_long[[1]]
      )
      if (!is.null(combined) && nrow(combined) > 0) {
        dict <- tryCatch(
          sntutils::build_dictionary(data = combined),
          error = function(e) NULL
        )
        if (!is.null(dict)) {
          dict_path <- fs::path(
            saved_dirs$final, paste0(final_basename, "_dictionary.xlsx")
          )
          if (requireNamespace("writexl", quietly = TRUE)) {
            writexl::write_xlsx(dict, dict_path)
            saved_files$dictionary <- as.character(dict_path)
          }
        }
      }
    }

    if (verbose) {
      cli::cli_alert_success("Wrote outputs to {.file {output_dir}}")
    }
  }

  ## ---- 10) Build result list ----------------------------------------------

  inputs_echo <- list(
    indicator_name       = indicator_name,
    indicator_title      = ind_title,
    indicator_unit_scale = multiplier,
    primary_level        = primary_level,
    output_levels        = output_levels,
    available_levels     = available_levels,
    pixel_size           = pixel_size,
    n_samples            = n_samples,
    seed                 = seed,
    cluster_cols         = cluster_cols,
    id_field             = id_field,
    polygon_id_field     = polygon_id_field,
    country_iso3         = iso3_norm,
    country_iso2         = iso2_norm,
    dhs_code             = dhs_norm,
    survey_year          = year_norm,
    source_label         = src_norm,
    covariate_names      = names(covariates),
    covariates_fp        = cov_fp,
    output_dir           = output_dir,
    cache_dir            = cache_dir,
    cache_key            = cache_key,
    use_cache            = use_cache,
    overwrite            = overwrite,
    return_draws         = return_draws,
    loaded_from_cache    = isTRUE(cell_pred_loaded),
    extra_args           = names(dots)
  )

  res <- list(
    cluster_data       = cd,
    cell_predictions   = cell_preds,
    admin              = admin_long,
    model_runner       = model_runner,
    id_raster          = id_raster,
    aggregation_tables = agg_tables,
    saved_files        = saved_files,
    cache_files        = cache_files,
    inputs             = inputs_echo
  )

  invisible(res)
}
