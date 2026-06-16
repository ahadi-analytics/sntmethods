# Run MBG Indicator Pipeline

Orchestrates the full MBG processing pipeline for DHS indicators across
one or more surveys in AHADI's parquet archive. This is a multi-survey,
multi-indicator production workflow - it is **not** a general-purpose
wrapper around
[`fit_mbg_indicator()`](https://ahadi-analytics.github.io/sntmethods/reference/fit_mbg_indicator.md).

## Usage

``` r
run_mbg_pipeline(
  country_iso3,
  adm0_sf,
  adm1_sf,
  adm2_sf,
  adm3_sf = NULL,
  pop_raster,
  path_dhs_parquet,
  table_out_path,
  raster_out_path,
  intermediate_out_path,
  pop_raster_u5 = NULL,
  pop_raster_1_2 = NULL,
  pop_raster_5_10 = NULL,
  pop_raster_10_20 = NULL,
  pop_raster_20plus = NULL,
  pop_raster_wra = NULL,
  country_iso2 = NULL,
  survey_year = NULL,
  min_year = "2005",
  survey_type = NULL,
  indicators = c("pfpr", "itn", "irs", "anc", "csb", "anemia", "iptp", "epi", "fever",
    "antimalarial"),
  aggregation_level = c("adm2", "adm3"),
  run_mbg = TRUE,
  save_rasters = TRUE,
  save_cluster_points = TRUE,
  cluster_points_out_path = NULL,
  cache = TRUE,
  csb_priority_method = c("all", "first", "public", "private"),
  custom_csb_indicator = NULL,
  verbose = TRUE,
  debug = FALSE
)
```

## Arguments

- country_iso3:

  Three-letter ISO country code (e.g., "bdi").

- adm0_sf:

  sf object with country boundary.

- adm1_sf:

  sf object with ADM1 boundaries.

- adm2_sf:

  sf object with ADM2 boundaries.

- adm3_sf:

  sf object with ADM3 boundaries (optional). Required when
  `aggregation_level = "adm3"`. Should contain columns for adm3 names
  and parent adm2/adm1/adm0 linkages.

- pop_raster:

  Total population raster(s). Can be:

  - Named list with years as names and file paths as values:
    `list("2019" = "path/to/pop_2019.tif", "2020" = "path/to/pop_2020.tif")`

  - Single file path (used for all years)

  - Already-loaded SpatRaster object (used for all years)

- path_dhs_parquet:

  Path to DHS parquet archive.

- table_out_path:

  Output directory for tables (CSV, XLSX).

- raster_out_path:

  Output directory for prediction rasters.

- intermediate_out_path:

  Output directory for cached intermediate outputs (aggregation tables,
  ID rasters).

- pop_raster_u5:

  Under-5 population raster(s) (optional). Same format as `pop_raster`.
  Used for indicators targeting children 0-59 months (PfPR, fever, CSB,
  ACT, antimalarial, anemia, U5MR, SMC, ITN use U5). If NULL, falls back
  to `pop_raster`.

- pop_raster_1_2:

  Population raster for children 12-23 months (optional). Same format as
  `pop_raster`. Used for EPI (immunization) indicators. Falls back to
  `pop_raster_u5`, then `pop_raster` if NULL.

- pop_raster_5_10:

  Population raster for ages 5-10 (optional). Same format as
  `pop_raster`. Used for `use_itn_5_10`. Falls back to `pop_raster` if
  NULL.

- pop_raster_10_20:

  Population raster for ages 10-20 (optional). Same format as
  `pop_raster`. Used for `use_itn_10_20`. Falls back to `pop_raster` if
  NULL.

- pop_raster_20plus:

  Population raster for ages 20+ (optional). Same format as
  `pop_raster`. Used for `use_itn_20plus`. Falls back to `pop_raster` if
  NULL.

- pop_raster_wra:

  Women of reproductive age (15-49) population raster (optional). Same
  format as `pop_raster`. Used for ANC, IPTp, and ITN use among pregnant
  women. Falls back to `pop_raster` if NULL.

- country_iso2:

  Two-letter DHS country code (e.g., "BU"). If NULL (default), derived
  automatically from `country_iso3` using the `countrycode` package.

- survey_year:

  Survey year(s) to process. Can be:

  - NULL: Process ALL available surveys with GPS data

  - Single integer: Process only that year (e.g., 2016)

  - Integer vector: Process specific years (e.g., c(2012, 2016))

- min_year:

  Minimum survey year to include. Surveys before this year will be
  excluded. Useful for filtering out older surveys that may lack key
  indicators. Default: NULL (no minimum).

- survey_type:

  Survey type(s) to process. Can be:

  - NULL (default): Auto-detect all available survey types (DHS, MIS,
    etc.)

  - Single string: Process only that type (e.g., "DHS")

  - Character vector: Process specific types (e.g., c("DHS", "MIS"))

- indicators:

  Character vector of indicator categories to process:

  - "pfpr": Parasite prevalence

  - "itn": ITN ownership/access/use

  - "irs": IRS coverage

  - "anc": ANC attendance

  - "csb": Care-seeking behavior

  - "act": ACT treatment (case management)

  - "anemia": Anemia prevalence

  - "iptp": IPTp doses

  - "epi": EPI vaccination

  - "u5mr": Under-5 mortality

  - "smc": SMC receipt

  - "fever": Fever prevalence (U5)

  - "antimalarial": Any antimalarial treatment (febrile U5)

  - "wealth": Wealth quintile distribution (proportion in
    poorest/richest)

  - "eff_cm": Effective coverage of case management (derived; auto-adds
    "csb" and "act" as dependencies)

- aggregation_level:

  Primary aggregation level for MBG outputs. One of:

  - "adm2": Aggregate to ADM2 level (default)

  - "adm3": Aggregate to ADM3 level (requires `adm3_sf`)

- run_mbg:

  Logical. If TRUE, runs MBG models. Default: TRUE.

- save_rasters:

  Logical. If TRUE, saves output rasters. Default: TRUE.

- save_cluster_points:

  Logical. If TRUE (default), saves the coordinate-level (cluster-point)
  datasets produced by `calc_*_mbg()` to disk in addition to the
  aggregated admin-level outputs. Set to FALSE to skip saving
  cluster-point files.

- cluster_points_out_path:

  Output directory for cluster-point (coordinate-level) datasets. If
  NULL (default), falls back to `table_out_path`. Use this to keep the
  per-cluster point files in a separate location from the combined
  admin-level outputs.

- cache:

  Logical. If TRUE (default), reuses cached intermediate outputs
  (aggregation tables, ID rasters) when available. Set to FALSE to force
  regeneration of all intermediate outputs.

- csb_priority_method:

  Character, one of "all" (default), "first", "public", or "private".
  Controls how overlapping care-seeking records are resolved in the CSB
  (and wealth-stratified CSB) indicators so each individual is assigned
  to at most one sector:

  - "all": Keep WHO methodology; overlaps allowed (csb_public and
    csb_private can both be 1 for the same child).

  - "first": Take the first recurring h32 source visited per child
    (alphabetical h32 order: h32a, h32b, ..., h32x). Mutually exclusive.

  - "public": Public-sector priority - if any public/CHW care, classify
    as public; otherwise private if any private; otherwise none.

  - "private": Private-sector priority - if any private care, classify
    as private; otherwise public if any public; otherwise none.

  With non-"all" values, csb_public + csb_private + csb_none sums to
  100%.

- custom_csb_indicator:

  Optional named list defining a user-specified, mutually-exclusive
  care-seeking partition fitted in addition to the built-in CSB family.
  When supplied, three derived indicators are produced: `<name>_dhis`
  (sought care at any user-listed DHIS source), `<name>_nondhis` (sought
  care at any user-listed non-DHIS source and not at any DHIS source),
  and `<name>_untreat` (did not seek care at any user-listed source).
  The list must have four character fields:

  - `name`: A short alphanumeric prefix used to build the three derived
    indicator codes (e.g. `"foo"` -\> `foo_dhis`, `foo_nondhis`,
    `foo_untreat`).

  - `dhis_locs`: Character vector of h32 entries that map to the DHIS
    bucket. Each entry may be either an h32 variable name (e.g.
    `"h32a"`) or a haven label string (case-insensitive). The two styles
    can be mixed in the same vector; variable-name matches take
    precedence over label matches when an h32 column is referenced both
    ways.

  - `nondhis_locs`: Character vector for the non-DHIS bucket (same
    dual-style semantics as `dhis_locs`).

  - `untreat_locs`: Character vector for the untreated bucket (same
    dual-style semantics as `dhis_locs`).

  When `custom_csb_indicator` is supplied, the partition is activated
  automatically: the `name` is added to `indicators` unless it (or one
  of the three derived codes) is already listed. The custom triple is
  always mutually exclusive at the child level (priority dhis \> nondhis
  \> untreat); admin-level estimates are rescaled to sum to 100% per
  admin unit. Default: NULL (disabled).

- verbose:

  Logical. If TRUE, prints detailed progress. Default: TRUE.

- debug:

  Logical. If TRUE, prints additional diagnostic messages for
  troubleshooting. Default: FALSE.

## Value

A list containing:

- final_dataset: Named list with `adm0`, `adm1`, and `adm2` (or `adm3`)
  tibbles in long format. Each tibble has standard columns: survey_id,
  iso3, iso2, survey_type, survey_year, adm0, adm1, adm2, type,
  geo_source, point, ci_l, ci_u, numerator, denominator,
  survey_numerator, survey_denominator, n_survey_clusters, indicator,
  indicator_code, numerator_description, denominator_description.

- mbg_estimates: MBG predictions at the specified aggregation level (if
  MBG was run)

- cluster_data: Raw cluster-level data

- raster_paths: Paths to saved rasters

- survey_metadata: Survey collection dates and metadata

## Details

See indicator-specific methodology files at
<https://github.com/ahadi-analytics/sntmethods/tree/master/inst/methods>

Pipeline steps:

1.  Discovers available surveys

2.  Loads survey data

3.  Prepares cluster-level data for each indicator

4.  Runs MBG models (if enabled)

5.  Generates outputs (rasters, CSVs, maps)

## When to use this

Use `run_mbg_pipeline()` when:

- You have a hive-partitioned DHS parquet archive laid out as documented
  in
  [`dhs_read()`](https://ahadi-analytics.github.io/sntmethods/reference/dhs_read.md) -
  the pipeline calls
  [`dhs_read()`](https://ahadi-analytics.github.io/sntmethods/reference/dhs_read.md)
  internally to discover and load surveys.

- You want to fit **many indicators across many surveys** end-to-end and
  have them returned as long-format tables stacked at `adm0` / `adm1` /
  `adm2`, with prediction rasters cached to disk.

- You have set up the optional INLA + `mbg` stack (see the [Get
  started](https://ahadi-analytics.github.io/sntmethods/articles/getting-started.html#mbg-dependencies-optional)
  vignette).

## When *not* to use this

Do **not** use `run_mbg_pipeline()` for ad-hoc, single-survey, or
single-indicator work. The pipeline assumes the parquet archive layout
from
[`dhs_read()`](https://ahadi-analytics.github.io/sntmethods/reference/dhs_read.md)
and is not designed to be pointed at one DHS file. For those cases:

- **One indicator, one survey, your own data.** Use
  [`fit_mbg_indicator()`](https://ahadi-analytics.github.io/sntmethods/reference/fit_mbg_indicator.md)
  directly. It takes a cluster-level data.table (produced by
  [`sntutils::read()`](https://ahadi-analytics.github.io/sntutils/reference/read.html) +
  a `prep_*_mbg()` helper, or built by hand) and runs a single INLA /
  MBG fit. No archive required.

      kr <- sntutils::read("TGKR81FL.DTA")
      ge <- sntutils::read("TGGE8AFL.dta")
      cl <- calc_pfpr_mbg(dhs_pr = kr, gps_data = ge)$pfpr_rdt_u5
      fit <- fit_mbg_indicator(input_data = cl, ...)

- **Survey-weighted DHS indicators without spatial modelling.** Use the
  [`calc_*_dhs()`](https://ahadi-analytics.github.io/sntmethods/reference/calc_pfpr_dhs.md)
  family - they accept any data frame and need no parquet archive and no
  INLA stack.

- **You only need one country, one survey, but want the pipeline
  shape.** Build a minimal one-survey parquet leaf following the
  directory layout in
  [`dhs_read()`](https://ahadi-analytics.github.io/sntmethods/reference/dhs_read.md)
  and then call `run_mbg_pipeline()` against it; this is usually only
  worth the setup if you plan to add more surveys later.

## Examples

``` r
if (FALSE) { # \dontrun{
results <- run_mbg_pipeline(
  country_iso3 = "bdi",
  adm0_sf = adm0,
  adm1_sf = adm1,
  adm2_sf = adm2,
  pop_raster = list("2016" = "/path/to/bdi_ppp_2016.tif"),
  path_dhs_parquet = "path/to/parquet",
  table_out_path = "path/to/output/tables",
  raster_out_path = "path/to/output/rasters",
  intermediate_out_path = "path/to/output/intermediate",
  survey_year = 2016,
  indicators = c("pfpr", "itn", "csb"),
  cache = TRUE
)
} # }
```
