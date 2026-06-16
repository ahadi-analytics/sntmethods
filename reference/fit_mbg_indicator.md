# Fit a Single MBG Indicator (Generic, Pluggable Admin Levels)

Generic, low-level wrapper around
[`mbg::MbgModelRunner`](https://henryspatialanalysis.github.io/mbg/reference/MbgModelRunner.html)
that takes a pre-built cluster-level table plus a population raster and
any subset of admin-0/1/2/3 shapefiles, fits a single MBG model and
returns a comprehensive list of in-memory artefacts (cluster table,
mean/lower/upper rasters, admin-level long-format tibbles, the fitted
`MbgModelRunner` object, the id raster, aggregation tables, the
cell-prediction draws matrix and an `inputs` echo of every parameter the
function received).

## Usage

``` r
fit_mbg_indicator(
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
  cluster_cols = list(cluster_id = "cluster_id", x = "x", y = "y", indicator =
    "indicator", samplesize = "samplesize"),
  id_field = "shapeID",
  indicator_title = NULL,
  indicator_unit_scale = 100,
  survey_year = NULL,
  source_label = NULL,
  output_dir = NULL,
  cache_dir = NULL,
  use_cache = TRUE,
  overwrite = FALSE,
  return_draws = FALSE,
  verbose = TRUE,
  ...
)
```

## Arguments

- cluster_data:

  Any data-frame-like object (data.frame, tibble, data.table, sf with
  x/y columns). Coerced internally via
  [`data.table::as.data.table()`](https://rdrr.io/pkg/data.table/man/as.data.table.html).
  Must contain columns `cluster_id`, `x`, `y`, `indicator`, `samplesize`
  (or supply alternative names via `cluster_cols`).

- indicator_name:

  Character. Short slug used in filenames and admin-tibble columns (e.g.
  `"pfpr_mic_2_10"`).

- population_raster:

  A
  [`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
  used as the template raster for prediction and as weights for
  population- weighted aggregation. Required.

- adm0_sf, adm1_sf, adm2_sf, adm3_sf:

  Optional `sf` polygon layers for each admin level. At least one must
  be supplied. The default modelling level is the highest provided (i.e.
  `adm3` if supplied, otherwise `adm2`, etc.).

- primary_level:

  Character, one of `"adm0"`, `"adm1"`, `"adm2"`, `"adm3"`. The level
  the MBG model is fitted on and the level cell-predictions are
  aggregated to first. Defaults to the highest level for which a
  shapefile was supplied.

- output_levels:

  Character vector of admin levels to summarise to. Defaults to all
  admin levels for which a shapefile was supplied.

- covariates:

  Optional named list of
  [`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
  objects to use as covariates. If `NULL`, an intercept-only constant
  raster is built automatically (pure spatial smoothing).

- pixel_size:

  Numeric. Informational pixel size in degrees, echoed in `$inputs` for
  downstream reporting. The actual prediction grid is inherited from
  `population_raster`; this argument does NOT resample the raster.
  Default `0.04166667` (\\\approx\\ 5 km at the equator).

- n_samples:

  Integer. Number of posterior draws drawn from the fitted model.
  Default `250`.

- seed:

  Integer. RNG seed for reproducibility. Default `1`.

- cluster_cols:

  Named list mapping the canonical cluster columns to user-supplied
  column names. Defaults to
  `list(cluster_id = "cluster_id", x = "x", y = "y", indicator = "indicator", samplesize = "samplesize")`.

- id_field:

  Character. Column name in the shapefiles holding the polygon id
  (default `"shapeID"`; falls back to a unique row index if not
  present).

- indicator_title:

  Optional human-readable label for the indicator, used in column
  headers / messaging. Defaults to a title-cased version of
  `indicator_name`.

- indicator_unit_scale:

  Numeric. Scaling factor applied when computing admin-level point
  estimates and CIs from the smoothed probability surface. Default `100`
  (i.e. percentages).

- survey_year:

  Optional integer survey year. Default `NULL`.

- source_label:

  Optional character data source label (`"DHS"`, `"MIS"`, `"routine"`,
  ...). Generic replacement for the older `survey_type` argument.
  Default `NULL`.

- output_dir:

  Optional path. If `NULL` (default), nothing is written to disk. If
  supplied, `rasters/`, `cluster_data/` and `final_data/` subfolders are
  created and pipeline-style files are written.

- cache_dir:

  Optional path for cached id-raster, aggregation table(s) and
  cell-prediction matrix. If `NULL` and `output_dir` is supplied,
  defaults to `file.path( output_dir, "cache")`. If both are `NULL`, no
  caching is performed.

- use_cache:

  Logical. If `FALSE`, caches are ignored on read (but still written if
  `cache_dir` is set). Default `TRUE`.

- overwrite:

  Logical. If `TRUE`, ignore existing cell- prediction cache entries and
  always refit. Default `FALSE`.

- return_draws:

  Logical. If `TRUE`, the full `n_pixel x n_samples` draws matrix is
  returned in `$cell_predictions$draws`. Default `FALSE` (memory-
  conservative).

- verbose:

  Logical. If `TRUE`, print step-by-step CLI messages. Default `TRUE`.

- ...:

  Additional arguments forwarded to
  [`mbg::build_aggregation_table()`](https://henryspatialanalysis.github.io/mbg/reference/build_aggregation_table.html)
  and `mbg::MbgModelRunner$new()` (filtered by each function's formals
  so unrecognised names are silently ignored).

## Value

A named list with elements:

- `cluster_data`:

  The cleaned input cluster table (`data.table`).

- `cell_predictions`:

  Named list with `mean`, `lower`, `upper` `SpatRaster` objects, and
  optionally `draws` (the full posterior matrix) when
  `return_draws = TRUE`.

- `admin`:

  Named list of long-format `tibble`s, one per `output_levels` entry,
  with columns
  ` indicator, indicator_title, admin_level, admin_id, admin_name, mean, lower, upper, population, country_iso3, country_iso2, dhs_code, survey_year, source_label`.
  `country_iso3` / `country_iso2` / `dhs_code` are derived by
  reverse-geocoding the median cluster coordinate against `adm0_sf` (or
  `rnaturalearth` as a fallback) and resolved via `countrycode`. They
  fall back to `NA` when the lookup fails.

- `model_runner`:

  The fitted `MbgModelRunner` object, or `NULL` if loaded from cache.

- `id_raster`:

  The pixel-id raster used by
  [`mbg::build_aggregation_table()`](https://henryspatialanalysis.github.io/mbg/reference/build_aggregation_table.html).

- `aggregation_tables`:

  Named list of aggregation tables (one per output level).

- `saved_files`:

  Named list of paths written to disk (empty when `output_dir` is
  `NULL`).

- `cache_files`:

  Named list of paths to cache artefacts (empty when `cache_dir` is
  `NULL`).

- `inputs`:

  Named list echoing every input parameter for reproducibility.

## Details

This function is intentionally generic and is not tied to DHS. The
country (`country_iso3` / `country_iso2` / `dhs_code`) is always derived
automatically from the median cluster coordinate via `adm0_sf` (when
supplied) or
[`rnaturalearth::ne_countries()`](https://docs.ropensci.org/rnaturalearth/reference/ne_countries.html)
as a fallback, then resolved through `countrycode`. The optional
`survey_year` and `source_label` arguments are pure annotations: when
supplied they are echoed onto the admin tibble and used to build
pipeline-style filenames; when `NULL` they are dropped from filenames
and written as `NA` in the admin tibble.

If `output_dir` is `NULL` (the default) nothing is written to disk –
everything is returned in memory in the result list. Set `output_dir` to
a directory path to also write rasters, the cluster file, the
long-format admin file (qs2 + xlsx) and a data dictionary, mirroring the
conventions of
[`run_mbg_pipeline()`](https://ahadi-analytics.github.io/sntmethods/reference/run_mbg_pipeline.md).

If `cache_dir` is non-`NULL` (or auto-derived from `output_dir`), three
artefacts are cached: the id raster (`.tif`), the per-level aggregation
table(s) (`.parquet`), and the cell-prediction draws matrix (`.qs2`). On
subsequent calls with the same key the cell-prediction matrix is
reloaded and the (expensive) INLA model fit is skipped, allowing the
user to re-summarise to additional admin levels without refitting.

## Examples

``` r
if (FALSE) { # \dontrun{
# Minimal in-memory call (no disk writes)
fit <- fit_mbg_indicator(
  cluster_data      = pfpr_dt,
  indicator_name    = "pfpr_mic_2_10",
  population_raster = pop_rast,
  adm1_sf           = adm1, adm2_sf = adm2
)
fit$cell_predictions$mean
fit$admin$adm2

# Pipeline-style call with disk + cache
fit2 <- fit_mbg_indicator(
  cluster_data      = pfpr_dt,
  indicator_name    = "pfpr_mic_2_10",
  population_raster = pop_rast,
  adm1_sf = adm1, adm2_sf = adm2, adm3_sf = adm3,
  primary_level     = "adm3",
  output_levels     = c("adm1", "adm2", "adm3"),
  survey_year       = 2017,
  source_label      = "MIS",
  output_dir        = "outputs/mbg_fit"
)
# country_iso3 / country_iso2 / dhs_code are derived from cluster coords.
} # }
```
