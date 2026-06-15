# MBG Indicator Pipeline — Team Reference Guide

This is the team reference for the DHS MBG pipeline in `sntmethods`. It covers what the
pipeline produces, how to set it up, what every output file is, and how to load results
downstream. If your country has already been run, jump to [§5 Outputs](#5-outputs--full-reference).

---

## Table of Contents

1. [What the pipeline does](#1-what-the-pipeline-does)
2. [Prerequisites](#2-prerequisites)
3. [Setup](#3-setup)
   - [3.1 Population rasters](#31-population-rasters)
   - [3.2 DHS data path](#32-dhs-data-path)
   - [3.3 Project paths and shapefiles](#33-project-paths-and-shapefiles)
   - [3.4 Population raster path lists](#34-population-raster-path-lists)
4. [Running the pipeline](#4-running-the-pipeline)
5. [Outputs — full reference](#5-outputs--full-reference)
   - [5.1 Cluster point files](#51-cluster-point-files-table_out_path)
   - [5.2 Annual summary tables](#52-annual-summary-tables-table_out_path)
   - [5.3 Combined multi-year table](#53-combined-multi-year-table-table_out_path)
   - [5.4 Prediction rasters](#54-prediction-rasters-raster_out_path)
   - [5.5 Intermediate / cache files](#55-intermediate--cache-files-intermediate_out_path)
   - [5.6 Maps](#56-maps)
6. [Final dataset structure](#6-final-dataset-structure)
7. [Loading results in R](#7-loading-results-in-r)
8. [Indicators reference](#8-indicators-reference)
9. [Units and scaling](#9-units-and-scaling)
10. [Graceful skips](#10-graceful-skips)
11. [Inspecting DHS variables (`dhs_read` + `list_dhs_var_labels`)](#11-inspecting-dhs-variables-dhs_read--list_dhs_var_labels)
12. [Custom care-seeking partition (`custom_csb_indicator`)](#12-custom-care-seeking-partition-custom_csb_indicator)
13. [CSB priority methods (`csb_priority_method`)](#13-csb-priority-methods-csb_priority_method)
14. [Country-specific notes](#14-country-specific-notes)

---

## 1. What the pipeline does

`sntmethods::run_mbg_pipeline()` is the single entry point for all DHS-based spatial
indicator work. It:

1. Discovers all available DHS / MIS surveys for a country in the parquet archive
2. Loads the relevant recodes (KR, IR, PR, HR, BR) and GPS coords (GE) per survey year
3. Prepares cluster-level data per indicator — unweighted counts at each GPS cluster
4. Fits MBG spatial models (Gaussian process surfaces) to smooth the cluster
   observations into ~1 km² raster surfaces with 95% credible intervals
5. Aggregates pixels to ADM2 (or ADM3) using a population raster as weight
6. Writes cluster files, prediction rasters, and summary tables (qs2 + xlsx). PNG maps are a separate post-processing step (see §5.6).

Methodology per indicator lives at
<https://github.com/ahadi-analytics/sntmethods/tree/master/inst/methods>.

---

## 2. Prerequisites

| Requirement | What it is | Where it comes from |
|---|---|---|
| DHS parquet archive | All recodes (KR, IR, PR, HR, BR, GE) as partitioned parquet | Ahadi OneDrive — see §3.2 |
| Country shapefile (ADM0–ADM3) | Cleaned, validated `sf` polygons | Ahadi OneDrive SNT project folder |
| Total population raster | WorldPop 1 km, one per survey year | `sntutils::download_worldpop()` |
| Under-5 population raster | WorldPop age-band (0–4), one per survey year | `sntutils::download_worldpop_age_band()` |
| Other age-band rasters (optional) | 12–23 m, 5–10 y, 10–20 y, 20+ y, women 15–49 | `sntutils::download_worldpop_age_band()` |

If only the total-population raster is supplied, the pipeline falls back to it for any
age-band that is missing. For best accuracy, supply the age-specific raster that matches
each indicator's denominator (see §8 for the indicator → population mapping).

---

## 3. Setup

### 3.1 Population rasters

You need at minimum **total population** and **under-5 population**. Under-5 is built
by summing the single-year-of-age bands 0–4. Optional age-banded rasters refine the
denominators for EPI, ANC/IPTp, and the older ITN-use age groups.

```r
library(sntutils)

country_iso3 <- "bdi"  # parameterise at the top of your script

# total population (all ages)
sntutils::download_worldpop(
  country_codes = toupper(country_iso3),
  years         = 2000:2029,
  dest_dir      = here::here(paths$pop_worldpop, "raw")
)

# under-5 population (ages 0-4 summed to a single raster per year)
sntutils::download_worldpop_age_band(
  country_codes = toupper(country_iso3),
  years         = 2000:2029,
  age_range     = c(0, 4),
  out_dir       = here::here(paths$pop_worldpop, "raw", "aged_rasters")
)

# optional: women 15-49 (ANC, IPTp, pregnant ITN use)
sntutils::download_worldpop_age_band(
  country_codes = toupper(country_iso3),
  years         = 2000:2029,
  age_range     = c(15, 49),
  sex           = "f",
  out_dir       = here::here(paths$pop_worldpop, "raw", "aged_rasters")
)
```

After downloading, your `pop_worldpop` folder will look like:

```
pop_worldpop/
  raw/
    bdi_ppp_2010_1km_Aggregated_UNadj.tif
    bdi_ppp_2012_1km_Aggregated_UNadj.tif
    bdi_pop_2016_CN_1km_UA_v1.tif
    ...
    aged_rasters/
      bdi_total_00_04_2010.tif
      bdi_total_00_04_2012.tif
      bdi_total_00_04_2016.tif
      ...
```

> **Note:** WorldPop changed its naming convention partway through. Files up to ~2015
> tend to use `_ppp_`, later years use `_pop_`. Always check the actual filenames after
> the download finishes and update §3.4 to match.

### 3.2 DHS data path

DHS data lives as partitioned parquet on Ahadi OneDrive. Use `dhs_data_path()` to build an
OS-agnostic path:

```r
path_dhs_parquet <- here::here(dhs_data_path(), "01_data/parquet")
```

> **Windows path-length note:** Windows has a 256-character path limit that can block
> access to deeply nested OneDrive folders. Until DHS data are migrated to a cloud API,
> you may need to map the OneDrive root to a drive letter
> (e.g. `subst O: "C:\Users\you\OneDrive - Ahadi Analytics"`).

### 3.3 Project paths and shapefiles

```r
# build all standard SNT folder paths (tables, figures, shapefiles, etc.)
paths <- sntutils::setup_project_paths()

# load country shapefiles (ADM0 through ADM3 in one qs2 object)
shp_list <- sntutils::read_snt_data(
  data_name    = glue::glue("{country_iso3}_adm0_adm2_post2023"),
  path         = here::here(paths$admin_shp, "processed"),
  file_formats = "qs2"
)$final_spat_vec

adm0_sf <- shp_list$adm0
adm1_sf <- shp_list$adm1
adm2_sf <- shp_list$adm2
adm3_sf <- shp_list$adm3   # NULL if ADM3 not available
```

### 3.4 Population raster path lists

One named list entry per survey year. The name must match the survey year (as a
character string) the pipeline will process:

```r
# total population
pop_raster_paths <- list(
  "2010" = here::here(paths$pop_worldpop,
    glue::glue("raw/{country_iso3}_ppp_2010_1km_Aggregated_UNadj.tif")),
  "2012" = here::here(paths$pop_worldpop,
    glue::glue("raw/{country_iso3}_ppp_2012_1km_Aggregated_UNadj.tif")),
  "2016" = here::here(paths$pop_worldpop,
    glue::glue("raw/{country_iso3}_pop_2016_CN_1km_UA_v1.tif"))
)

# under-5 population
pop_raster_u5_paths <- list(
  "2010" = here::here(paths$pop_worldpop,
    glue::glue("raw/aged_rasters/{country_iso3}_total_00_04_2010.tif")),
  "2012" = here::here(paths$pop_worldpop,
    glue::glue("raw/aged_rasters/{country_iso3}_total_00_04_2012.tif")),
  "2016" = here::here(paths$pop_worldpop,
    glue::glue("raw/aged_rasters/{country_iso3}_total_00_04_2016.tif"))
)
```

The other age-band parameters (`pop_raster_1_2`, `pop_raster_5_10`, `pop_raster_10_20`,
`pop_raster_20plus`, `pop_raster_wra`) follow the same pattern. If omitted, the pipeline
falls back to `pop_raster_u5` (where appropriate) and finally `pop_raster`.

Each `pop_raster*` argument also accepts a single file path (used for all years) or an
already-loaded `terra::SpatRaster`.

---

## 4. Running the pipeline

```r
results_dhs <- sntmethods::run_mbg_pipeline(
  country_iso3          = toupper(country_iso3),  # e.g. "BDI"
  adm0_sf               = adm0_sf,
  adm1_sf               = adm1_sf,
  adm2_sf               = adm2_sf,
  adm3_sf               = adm3_sf,
  pop_raster            = pop_raster_paths,
  pop_raster_u5         = pop_raster_u5_paths,
  pop_raster_wra        = pop_raster_wra_paths,    # optional
  pop_raster_1_2        = pop_raster_1_2_paths,    # optional, for EPI
  path_dhs_parquet      = path_dhs_parquet,
  table_out_path        = here::here(paths$dhs, "processed"),
  raster_out_path       = paths$model_fig,
  intermediate_out_path = paths$model_tbl,
  min_year              = "2005",
  indicators            = c(
    "pfpr", "itn", "irs", "anc", "csb", "anemia", "iptp",
    "fever", "antimalarial", "act", "epi", "u5mr", "smc"
  ),
  aggregation_level     = "adm2",
  csb_priority_method   = "all",
  custom_csb_indicator  = NULL,                   # optional, see §12
  run_mbg               = TRUE,
  save_rasters          = TRUE,
  cache                 = TRUE,                   # FALSE forces full regen
  verbose               = TRUE,
  debug                 = FALSE
)
```

### Full parameter reference

| Parameter | Default | What it controls |
|---|---|---|
| `country_iso3` | — | Three-letter ISO code, e.g. `"BDI"` |
| `country_iso2` | `NULL` | Two-letter DHS code, e.g. `"BU"`. Auto-derived from `country_iso3` via `countrycode` if NULL |
| `adm0_sf`, `adm1_sf`, `adm2_sf` | — | Country boundary polygons |
| `adm3_sf` | `NULL` | Required only when `aggregation_level = "adm3"` |
| `pop_raster` | — | Total-population rasters: named list keyed by year, single path, or `SpatRaster` |
| `pop_raster_u5` | `NULL` | Under-5 (0–59 m) rasters. Falls back to `pop_raster` |
| `pop_raster_1_2` | `NULL` | 12–23 m rasters (EPI). Falls back to `pop_raster_u5`, then `pop_raster` |
| `pop_raster_5_10` | `NULL` | 5–10 y rasters (`itn_use_5_10`). Falls back to `pop_raster` |
| `pop_raster_10_20` | `NULL` | 10–20 y rasters (`itn_use_10_20`). Falls back to `pop_raster` |
| `pop_raster_20plus` | `NULL` | 20+ y rasters (`itn_use_20plus`). Falls back to `pop_raster` |
| `pop_raster_wra` | `NULL` | Women 15–49 rasters (ANC, IPTp, pregnant ITN). Falls back to `pop_raster` |
| `path_dhs_parquet` | — | Root path to the DHS parquet archive |
| `table_out_path` | — | Output dir for cluster files, annual tables, combined table (qs2 + xlsx) |
| `raster_out_path` | — | Output dir for prediction rasters (mean / lower / upper) |
| `intermediate_out_path` | — | Output dir for cached aggregation tables and ID rasters |
| `survey_year` | `NULL` | `NULL` = all available; integer = single year; vector = specific years |
| `min_year` | `"2005"` | Drop surveys before this year |
| `survey_type` | `NULL` | `NULL` = auto-detect (DHS, MIS, …); string = single type; vector = multiple |
| `indicators` | see §8 | Indicator categories to run |
| `aggregation_level` | `"adm2"` | `"adm2"` or `"adm3"` — finest level in `final_dataset` |
| `run_mbg` | `TRUE` | `FALSE` = prepare cluster data only (no model fitting) |
| `save_rasters` | `TRUE` | Write mean/lower/upper prediction rasters |
| `cache` | `TRUE` | Reuse intermediate aggregation tables and ID rasters when present |
| `csb_priority_method` | `"all"` | `"all"` / `"first"` / `"public"` / `"private"` (see §13) |
| `custom_csb_indicator` | `NULL` | Optional user-defined care-seeking partition (see §12) |
| `verbose` | `TRUE` | Print detailed progress |
| `debug` | `FALSE` | Print extra diagnostic messages |

### Return value

A list:

- `final_dataset` — named list with `adm0`, `adm1`, and `adm2` (or `adm3`) tibbles in
  **long** format. See §6.
- `mbg_estimates` — MBG predictions at the chosen `aggregation_level`
- `cluster_data` — raw cluster-level data, nested by year × indicator
- `raster_paths` — paths to saved rasters
- `survey_metadata` — survey collection dates and metadata
- `skipped_indicators` — see §10

---

## 5. Outputs — full reference

All paths below assume the `paths` object from §3.3. Burundi (`bdi`) examples are shown.

---

### 5.1 Cluster point files (`table_out_path`)

One file per indicator per survey year. Each file holds the raw GPS cluster
observations used as MBG model inputs — unweighted counts of events and sample sizes
at every surveyed cluster.

**Location:** `{paths$dhs}/processed/`

**Naming pattern:** `{country}_{indicator}_cluster_points_{survey_type}_{year}_v{YYYYMMDD}.qs2`

**Examples:**
```
bdi_pfpr_rdt_u5_cluster_points_dhs_2016_v20250226.qs2
bdi_pfpr_mic_u5_cluster_points_dhs_2016_v20250226.qs2
bdi_itn_access_cluster_points_dhs_2016_v20250226.qs2
bdi_itn_use_u5_cluster_points_dhs_2016_v20250226.qs2
bdi_anc_4plus_cluster_points_dhs_2016_v20250226.qs2
bdi_iptp_3plus_cluster_points_dhs_2016_v20250226.qs2
bdi_anemia_any_cluster_points_dhs_2016_v20250226.qs2
bdi_epi_dpt3_cluster_points_dhs_2016_v20250226.qs2
bdi_u5mr_cluster_points_dhs_2016_v20250226.qs2
bdi_smc_receipt_cluster_points_mis_2012_v20250226.qs2
```

> **Versioning:** the pipeline keeps the 3 most recent versions of each file; older
> versions are pruned automatically. Always load via `sntutils::read_snt_data()` —
> it resolves the date stamp for you.

**Columns:**

| Column | Type | Description |
|---|---|---|
| `cluster_id` | integer | DHS cluster identifier |
| `x` | numeric | Longitude (decimal degrees) |
| `y` | numeric | Latitude (decimal degrees) |
| `{numerator}` | integer | Count of events (e.g. `n_positive`, `n_with_access`, `n_vaccinated`, `n_deaths`) |
| `{denominator}` | integer | Sample size (e.g. `n_tested`, `n_individuals`, `n_children`, `n_exposed`) |
| `prop_raw` | numeric | Raw proportion = numerator / denominator |

Numerator / denominator naming by indicator group:

| Indicator group | Numerator | Denominator |
|---|---|---|
| `pfpr_*` | `n_positive` | `n_tested` |
| `itn_*` | `n_with_access` / `n_using` | `n_individuals` |
| `anc_*` | `n_anc` | `n_women` |
| `iptp_*` | `n_received_sp` | `n_women` |
| `irs_*` | `n_sprayed` | `n_households` |
| `anemia_*` | `n_anemic` | `n_tested_hb` |
| `epi_*` | `n_vaccinated` | `n_children` |
| `smc_*` | `n_received_smc` | `n_children` |
| `fever` | `n_fever` | `n_children` |
| `antimalarial` | `n_antimalarial` | `n_febrile` |
| `u5mr` | `n_deaths` | `n_exposed` |
| `csb_*` (built-in) | `n_seek_*` | `n_febrile` |
| `<custom>_dhis` / `_nondhis` / `_untreat` | `n_<bucket>` | `n_febrile` |

---

### 5.2 Annual summary tables (`table_out_path`)

One dataset per survey year with MBG-modelled estimates at ADM2 (or ADM3) for every
indicator processed in that year. Saved in both `.qs2` (fast binary) and `.xlsx` (with
the data dictionary on Sheet 2).

**Location:** `{paths$dhs}/processed/`

**Naming pattern:**
```
{country}_mbg_indicators_{year}_v{YYYYMMDD}.qs2
{country}_mbg_indicators_{year}_v{YYYYMMDD}.xlsx
```

**Examples:**
```
bdi_mbg_indicators_2010_v20250226.qs2
bdi_mbg_indicators_2010_v20250226.xlsx
bdi_mbg_indicators_2012_v20250226.qs2
bdi_mbg_indicators_2012_v20250226.xlsx
bdi_mbg_indicators_2016_v20250226.qs2
bdi_mbg_indicators_2016_v20250226.xlsx
```

The XLSX has two sheets:
- **Sheet 1 (`data`)** — the estimates table
- **Sheet 2 (`dictionary`)** — column-level metadata: label, units, DHS variable,
  recode file, numerator / denominator definition. For custom CSB sub-indicators, the
  `numerator_description` and `dhs_variables` columns embed the actual user-supplied
  h32 list, so the spec is fully traceable from the XLSX.

---

### 5.3 Combined multi-year table (`table_out_path`)

All survey years stacked into one table via `bind_rows()`. This is the file you load
for trend analysis and most downstream SNT work.

**Location:** `{paths$dhs}/processed/`

**Naming pattern:**
```
{country}_mbg_indicators_combined_v{YYYYMMDD}.qs2
{country}_mbg_indicators_combined_v{YYYYMMDD}.xlsx
```

**Examples:**
```
bdi_mbg_indicators_combined_v20250226.qs2
bdi_mbg_indicators_combined_v20250226.xlsx
```

> Only produced when more than one survey year is processed in the same run.

---

### 5.4 Prediction rasters (`raster_out_path`)

Continuous ~1 km² surfaces covering the whole country, produced by the MBG model.
Three rasters per indicator per year: mean prediction, lower 95% CI, upper 95% CI.

**Location:** `{paths$model_fig}/`

**Naming pattern:**
```
{country}_{indicator}_mbg_{survey_type}_{year}_mean.tif
{country}_{indicator}_mbg_{survey_type}_{year}_lower.tif
{country}_{indicator}_mbg_{survey_type}_{year}_upper.tif
```

**Examples:**
```
bdi_pfpr_rdt_u5_mbg_dhs_2016_mean.tif
bdi_pfpr_rdt_u5_mbg_dhs_2016_lower.tif
bdi_pfpr_rdt_u5_mbg_dhs_2016_upper.tif

bdi_itn_access_mbg_dhs_2016_mean.tif
bdi_u5mr_mbg_dhs_2016_mean.tif
```

**Units:**
- Most indicators (PfPR, ITN, ANC, IPTp, EPI, SMC, fever, antimalarial, anemia, CSB,
  custom CSB): **0–100** (percentage)
- U5MR: **0–1000** (per 1,000 live births)

> Rasters double as cache: if all three (`_mean`, `_lower`, `_upper`) already exist for
> an indicator-year pair and `cache = TRUE`, the pipeline skips re-fitting that model.

---

### 5.5 Intermediate / cache files (`intermediate_out_path`)

Pre-computed spatial lookup structures that make repeated runs fast. The expensive
step (rasterising ADM polygons + building pixel-to-admin mapping with population
weights) is done once and saved here.

**Location:** `{paths$model_tbl}/`

**Files:**

| File | Description |
|---|---|
| `{country}_id_raster_adm2.tif` | Integer raster, pixel value = ADM2 polygon ID |
| `{country}_id_raster_adm3.tif` | Same for ADM3 (only when `aggregation_level = "adm3"`) |
| `{country}_aggregation_table_adm2.parquet` | Pixel-to-ADM2 mapping with population weights |
| `{country}_aggregation_table_adm3.parquet` | Same for ADM3 |

**Examples:**
```
bdi_id_raster_adm2.tif
bdi_aggregation_table_adm2.parquet
```

> Leave `cache = TRUE` after the first run and these are reused on every subsequent
> run. Set `cache = FALSE` to force full regeneration (e.g. after a shapefile change).

---

### 5.6 Maps

`run_mbg_pipeline()` does **not** write PNG maps. It only writes GeoTIFFs to
`raster_out_path` and tables to `table_out_path`. Map rendering is a separate
post-processing step using the helpers in `R/mbg_outputs.R`:

- `plot_mbg_clusters()` — cluster-point map for a single indicator
- `generate_cluster_maps()` — batch cluster maps, optionally saved to disk
- `save_indicator_map()` — write a single ggplot/tmap object to file
- `generate_all_maps()` — batch all map types for one indicator
- `save_mbg_rasters()` — standalone raster writer (the pipeline uses this internally)

Typical pattern: load a raster from `raster_out_path/`, build a tmap or ggplot,
and pass it to `save_indicator_map()`. None of these are invoked by
`run_mbg_pipeline()` — call them yourself after the pipeline finishes.

---

## 6. Final dataset structure

`results_dhs$final_dataset` (and the `.qs2` / `.xlsx` files) is a **named list** with
`adm0`, `adm1`, `adm2`, and (if `aggregation_level = "adm3"`) `adm3` tibbles. Each
tibble is in **long format**: one row per admin × indicator × survey_year.

### Columns (every tibble)

| Column | Type | Description |
|---|---|---|
| `survey_id` | char | DHS survey ID (e.g., `"BU2016DHS"`) |
| `iso3` | char | Three-letter country code |
| `iso2` | char | Two-letter DHS country code |
| `survey_type` | char | `"DHS"`, `"MIS"`, … |
| `survey_year` | int | Survey year |
| `adm0` | char | Country name |
| `adm1` | char | ADM1 name |
| `adm2` | char | ADM2 name (`NA` in the `adm0` / `adm1` tibbles) |
| `adm3` | char | ADM3 name (only in `adm3` tibble) |
| `type` | char | `"mbg"` for modelled, `"survey"` for raw cluster aggregate |
| `geo_source` | char | Source identifier (e.g., `"DHS"`) |
| `point` | num | Point estimate |
| `ci_l` | num | Lower 95% CI |
| `ci_u` | num | Upper 95% CI |
| `numerator` | num | Modelled numerator (population × point) |
| `denominator` | num | Modelled denominator (population) |
| `survey_numerator` | int | Raw survey numerator (sum across clusters in unit) |
| `survey_denominator` | int | Raw survey denominator (sum across clusters in unit) |
| `n_survey_clusters` | int | Number of clusters supporting the unit estimate |
| `indicator` | char | Display label (e.g., `"PfPR (RDT, U5)"`) |
| `indicator_code` | char | Machine code (e.g., `pfpr_rdt_u5`, `csb_eff_dhis`) |
| `numerator_description` | char | Numerator definition; for custom CSB, embeds the user-supplied h32 list |
| `denominator_description` | char | Denominator definition |

### Example: filter and pivot

```r
# all PfPR estimates at adm2, every year, in long format
adm2_long <- results_dhs$final_dataset$adm2 |>
  dplyr::filter(stringr::str_starts(indicator_code, "pfpr"))

# pivot to wide if you want one row per adm2 × year
adm2_wide <- adm2_long |>
  tidyr::pivot_wider(
    id_cols     = c(iso3, survey_year, adm0, adm1, adm2),
    names_from  = indicator_code,
    values_from = c(point, ci_l, ci_u)
  )
```

---

## 7. Loading results in R

### Combined multi-year table

```r
# always use read_snt_data — it resolves the latest version automatically
combined <- sntutils::read_snt_data(
  data_name    = glue::glue("{country_iso3}_mbg_indicators_combined"),
  path         = here::here(paths$dhs, "processed"),
  file_formats = "qs2"
)$data

bdi_2016 <- combined$adm2 |> dplyr::filter(survey_year == 2016)
```

### A specific annual table

```r
bdi_2016 <- sntutils::read_snt_data(
  data_name    = "bdi_mbg_indicators_2016",
  path         = here::here(paths$dhs, "processed"),
  file_formats = "qs2"
)$data
```

### Cluster point data for a specific indicator

```r
pfpr_clusters_2016 <- sntutils::read_snt_data(
  data_name    = "bdi_pfpr_rdt_u5_cluster_points_dhs_2016",
  path         = here::here(paths$dhs, "processed"),
  file_formats = "qs2"
)$data
```

### Prediction rasters

```r
library(terra)

mean_r  <- terra::rast(here::here(paths$model_fig, "bdi_pfpr_rdt_u5_mbg_dhs_2016_mean.tif"))
lower_r <- terra::rast(here::here(paths$model_fig, "bdi_pfpr_rdt_u5_mbg_dhs_2016_lower.tif"))
upper_r <- terra::rast(here::here(paths$model_fig, "bdi_pfpr_rdt_u5_mbg_dhs_2016_upper.tif"))
```

### Pipeline return object (in-session)

```r
# adm2 long-format estimates
adm2_df <- results_dhs$final_dataset$adm2

# cluster-level data (nested: year -> indicator)
pfpr_clusters <- results_dhs$cluster_data[["2016"]][["pfpr_rdt_u5"]]

# what was skipped and why
results_dhs$skipped_indicators
```

---

## 8. Indicators reference

| `indicators` key | Sub-indicators produced | DHS recode | Pop type |
|---|---|---|---|
| `pfpr` | `pfpr_rdt_u5`, `pfpr_mic_u5`, `pfpr_rdt_5_10`, `pfpr_either_u5`, … | PR | `u5` |
| `itn` | `itn_ownership`, `itn_access`, `itn_use_all`, `itn_use_u5`, `itn_use_pregnant`, `itn_use_if_access`, `itn_use_5_10`, `itn_use_10_20`, `itn_use_20plus` | HR + PR | `all` / `u5` / `wra` / `5_10` / `10_20` / `20plus` |
| `irs` | `irs_coverage` | HR | `all` |
| `anc` | `anc_1plus`, `anc_3plus`, `anc_4plus`, `anc_8plus` | IR | `wra` |
| `iptp` | `iptp_1plus`, `iptp_2plus`, `iptp_3plus`, `iptp_4plus` | IR | `wra` |
| `anemia` | `anemia_any`, `anemia_moderate_plus`, `anemia_severe` | PR | `u5` |
| `epi` | `epi_bcg`, `epi_dpt3`, `epi_measles1`, `epi_polio3`, … | KR | `1_2` |
| `u5mr` | `u5mr` | BR | `u5` |
| `smc` | `smc_receipt` | KR | `u5` |
| `fever` | `fever` | KR | `u5` |
| `antimalarial` | `antimalarial`, `antimalarial_public` | KR | `u5` |
| `act` | `act`, `act_pub`, `act_among_am` | KR | `u5` |
| `csb` | `csb_any`, `csb_public`, `csb_pub_nochw`, `csb_chw`, `csb_private`, `csb_priv_formal`, `csb_pharmacy`, `csb_priv_informal`, `csb_priv_form_pha`, `csb_trained`, `csb_none` | KR | `u5` |
| `wealth` | `csb_q1` … `csb_q5` (wealth-stratified CSB) | HR + KR | `u5` |
| `eff_cm` | `eff_cm` (effective coverage of case management — **auto-adds `csb` and `act` as deps**) | KR | `u5` |
| **custom** (`custom_csb_indicator`) | `<name>_dhis`, `<name>_nondhis`, `<name>_untreat` | KR | `u5` |

You can pass individual ITN sub-indicators directly (e.g. `indicators = c("itn_access", "itn_use_u5")`)
for a faster run when you only need a subset. The same is true for `act_pub`,
`act_among_am`, and `antimalarial_public`.

When `custom_csb_indicator` is supplied, its `name` (e.g. `"csb_eff"`) is **auto-added**
to `indicators`, and KR is **forced into the file-type discovery** even if no other KR
indicator is requested. See §12.

---

## 9. Units and scaling

| Indicator group | Output scale | Example |
|---|---|---|
| PfPR, ITN, ANC, IPTp, IRS, EPI, SMC, fever, antimalarial, anemia, CSB, custom CSB | **0–100** (percentage) | `47.3` = 47.3% |
| U5MR | **0–1000** (per 1,000 live births) | `68.1` = 68.1 per 1,000 |

Applies to both `point` / `ci_l` / `ci_u` in `final_dataset` and to the pixel values in
the `.tif` rasters.

---

## 10. Graceful skips

Not every DHS / MIS survey contains every indicator variable. When a required variable
is absent (e.g. `hv253` for IRS, `hml43` / `ml13g` for SMC, `hml35` / `hml32` for PfPR
in older surveys, `b7` for U5MR in MIS files), the pipeline **skips that indicator
gracefully** — it does not crash. A warning is printed and the reason is recorded in
`results_dhs$skipped_indicators`:

```r
results_dhs$skipped_indicators
# $`2012`$irs
# [1] "IRS variable not available for this survey"
#
# $`2012`$smc
# [1] "No SMC variable found; checked hml43 and ml13g; SMC not available for this survey"
```

Skipped indicators simply produce no rows for that year in `final_dataset`. Expected
behaviour.

> **Custom CSB sub-indicators are always emitted.** Even when a bucket has zero
> numerator or fewer than 5 supporting clusters, the zero-fill helpers
> (`.zero_fill_custom_csb_cluster_data()` and `.zero_fill_custom_csb_adm_estimates()`)
> guarantee all three derived codes (`<name>_dhis`, `<name>_nondhis`, `<name>_untreat`)
> appear in cluster data, MBG estimates, rasters, and the XLSX summary. See §12.

---

## 11. Inspecting DHS variables (`dhs_read` + `list_dhs_var_labels`)

Before you can build a country-specific spec — most importantly a `custom_csb_indicator`
partition (§12) — you need to know which DHS variables exist in your country's recodes
and what their haven labels are. Two helpers do this.

### 11.1 `dhs_read()` — load a single DHS recode

```r
dhs_read(
  path,
  survey_id    = NULL,
  file_type    = NULL,        # mandatory
  country_code = NULL,
  survey_year  = NULL,
  survey_type  = NULL,
  verbose      = TRUE
)
```

- `path` — root of the parquet archive (same `path_dhs_parquet` from §3.2)
- `file_type` — **required.** One of `"PR"`, `"HR"`, `"IR"`, `"KR"`, `"GE"`, `"BR"`,
  `"MR"`, `"WI"`
- `country_code` — two-letter DHS code (e.g. `"BU"`, `"GH"`, `"GN"`)
- `survey_year` — integer
- `survey_type` — `"DHS"`, `"MIS"`, … optional

Single-survey direct reads preserve **all columns and haven labels** (which is what
`list_dhs_var_labels()` reads). Multi-survey open_dataset reads standardize haven
labels and keep only common columns.

```r
kr_data <- sntmethods::dhs_read(
  path        = path_dhs_parquet,
  file_type   = "KR",
  survey_type = "DHS"
)
```

### 11.2 `list_dhs_var_labels()` — enumerate variables and labels

```r
list_dhs_var_labels(
  data,
  pattern,
  regex            = FALSE,
  only_observed    = FALSE,
  duplicate_label  = TRUE
)
```

**Common usage** — list every `h32*` care-seeking source variable and its haven label:

```r
sntmethods::list_dhs_var_labels(kr_data, "h32") |>
  dplyr::select(variable, label) |>
  as.data.frame()
```

Output (illustrative):

```
   variable                              label
1      h32a           Government health centre
2      h32b               Government hospital
3      h32c            Private health facility
4      h32d                            Pharmacy
5      h32e                                CHW
6      h32f                  Traditional healer
7      h32i                          NGO clinic
...
```

**Other patterns:**

```r
# only h32 columns with at least one observed `1`, flagging duplicate labels
sntmethods::list_dhs_var_labels(kr_data, "h32", only_observed = TRUE)

# ITN source columns ml13a..ml13h via regex
sntmethods::list_dhs_var_labels(kr_data, "^ml13[a-z]$", regex = TRUE)
```

**Returned columns:** `variable`, `label`, `n_nonmissing`, `n_ones`, and (when
`duplicate_label = TRUE`) a `duplicate_label` flag identifying h32 columns whose haven
label is shared with at least one other matched variable. **When duplicates exist,
route by variable name rather than label** — see §12.

---

## 12. Custom care-seeking partition (`custom_csb_indicator`)

The built-in CSB family classifies care-seeking with a fixed (WHO-style) public /
private / CHW taxonomy. `custom_csb_indicator` lets you define **one extra
mutually-exclusive partition** that is fitted alongside the built-in CSB indicators —
useful, for example, to split DHIS-reporting facilities from non-DHIS sources for an
effective-coverage cascade.

When supplied, exactly **three derived sub-indicators** are produced:

- `<name>_dhis` — sought care at any user-listed DHIS source
- `<name>_nondhis` — sought care at any user-listed non-DHIS source **and not at any DHIS source**
- `<name>_untreat` — did not seek care at any user-listed source (residual bucket)

The triple is mutually exclusive at the child level (priority `dhis > nondhis > untreat`)
and admin-level estimates are rescaled to sum to 100% per admin unit.

### 12.1 Spec structure

```r
custom_csb_indicator = list(
  name         = "csb_eff",                     # required, character scalar
  dhis_locs    = c("h32a", "h32b", ...),        # required, character vector
  nondhis_locs = c("h32k", "h32l", ...),        # required, character vector
  untreat_locs = c("h32s", "h32t", "h32x")      # required, character vector
)
```

**Field rules:**

| Field | Rules |
|---|---|
| `name` | Must match `^csb_[a-z0-9_]+$`. Cannot collide with built-in CSB codes (`csb_any`, `csb_public`, `csb_pub_nochw`, `csb_chw`, `csb_private`, `csb_priv_formal`, `csb_pharmacy`, `csb_priv_informal`, `csb_priv_form_pha`, `csb_trained`, `csb_none`, `csb_q1`–`csb_q5`). Derived codes (`<name>_dhis` etc.) must not collide with any other requested indicator. |
| `dhis_locs` | Non-empty. Each entry is either an h32 variable name (`"h32a"`) or a haven label (case-insensitive, `"Government health centre"`). Mixed styles allowed. **Variable-name entries take precedence over label entries.** No NAs. Disjoint from `nondhis_locs` and `untreat_locs`. |
| `nondhis_locs` | Same rules as `dhs_locs`. |
| `untreat_locs` | Same rules but **may be empty** and **may contain NAs** (tolerated, ignored). |

### 12.2 Recommended workflow

**Step 1 — load the KR file and inspect h32:**

```r
kr_data <- sntmethods::dhs_read(
  path        = path_dhs_parquet,
  file_type   = "KR",
  survey_type = "DHS"
)

sntmethods::list_dhs_var_labels(kr_data, "h32") |>
  dplyr::select(variable, label) |>
  as.data.frame()
```

**Step 2 — decide which slots map to DHIS, non-DHIS, and untreated.** Use **variable
names** (not labels) whenever the country dictionary shows duplicate labels or
country-specific `"NA -"` placeholder labels (variable-name routing wins, even when
the label is unmapped).

**Step 3 — build the spec and pass it to the pipeline:**

```r
results_dhs <- sntmethods::run_mbg_pipeline(
  country_iso3          = "GIN",
  adm0_sf               = adm0_sf,
  adm1_sf               = adm1_sf,
  adm2_sf               = adm2_sf,
  pop_raster            = pop_raster_paths,
  pop_raster_u5         = pop_raster_u5_paths,
  path_dhs_parquet      = path_dhs_parquet,
  table_out_path        = here::here(paths$dhs, "processed"),
  raster_out_path       = paths$model_fig,
  intermediate_out_path = paths$model_tbl,
  indicators            = c("pfpr", "itn", "csb"),  # csb_eff added automatically
  custom_csb_indicator  = list(
    name         = "csb_eff",
    dhis_locs    = c("h32a", "h32b", "h32c", "h32d", "h32e",
                     "h32f", "h32i",                                # + h32i
                     "h32j"),
    nondhis_locs = c("h32k", "h32l",                                # + h32l
                     "h32m", "h32n",
                     "h32r"),                                       # + h32r
    untreat_locs = c("h32s", "h32t", "h32x")
  )
)
```

### 12.3 What the pipeline does for you

When `custom_csb_indicator` is non-NULL, the pipeline:

1. **Validates the spec** before any data are read
   (`.validate_custom_csb_indicator_spec()`): type checks, regex check on `name`,
   collision check against built-in CSB codes and other requested indicators,
   pairwise-disjointness of buckets.
2. **Auto-activates the partition** by adding `name` to `indicators` if it (or one of
   the three derived codes) is not already there.
3. **Forces KR file type** into the file-type discovery step — even if no built-in KR
   indicator is requested. This was a recent fix; previously, an ITN-only run with a
   custom CSB spec would log "Missing KR data" for every survey.
4. **Builds a per-survey slot-to-bucket lookup** (`.build_custom_csb_classification()`):
   reads haven labels from each h32 column in the KR file and matches against your
   `dhis_locs` / `nondhis_locs` / `untreat_locs`. Unmapped h32 columns are dropped with
   an info message; their children fall into `_untreat` via the residual rule.
5. **Classifies each febrile U5 child** (`.classify_custom_csb_from_h32()`) into
   exactly one bucket via the priority `dhis > nondhis > untreat`. Implementation uses
   `seq_len(nrow(data))` for sequential row IDs (a recent bugfix — `dplyr::row_number(data)`
   ranks values instead of generating positions, which broke when h32a had many ties)
   and `haven::zap_labels()` on each h32 column to avoid `haven_labelled` value
   comparison bugs.
6. **Aggregates to cluster level**, runs MBG on each of the three derived indicators,
   and writes cluster files, rasters, and dictionary rows.
7. **Zero-fills missing sub-indicators** so all three derived codes always appear in
   cluster_data, mbg_estimates, rasters, and the XLSX summary — even when MBG skipped a
   bucket (zero numerator or <5 clusters).
8. **Embeds traceability** in dictionary rows: each row of the dictionary records the
   actual user-supplied DHS variable list inside both `numerator_description` and
   `dhs_variables`, so the spec is reconstructable from the XLSX alone.

### 12.4 Output naming

For `name = "csb_eff"`, the outputs follow the standard CSB pattern:

```
# cluster files
{country}_csb_eff_dhis_cluster_points_{survey_type}_{year}_v{YYYYMMDD}.qs2
{country}_csb_eff_nondhis_cluster_points_{survey_type}_{year}_v{YYYYMMDD}.qs2
{country}_csb_eff_untreat_cluster_points_{survey_type}_{year}_v{YYYYMMDD}.qs2

# rasters
{country}_csb_eff_dhis_mbg_{survey_type}_{year}_{mean|lower|upper}.tif
{country}_csb_eff_nondhis_mbg_{survey_type}_{year}_{mean|lower|upper}.tif
{country}_csb_eff_untreat_mbg_{survey_type}_{year}_{mean|lower|upper}.tif

# in final_dataset (long-form rows in $adm2)
indicator_code = "csb_eff_dhis" | "csb_eff_nondhis" | "csb_eff_untreat"
```

---

## 13. CSB priority methods (`csb_priority_method`)

Controls how overlapping h32 records are resolved within the **built-in** CSB family
(and wealth-stratified CSB). It does **not** affect `custom_csb_indicator`, which
always uses `dhis > nondhis > untreat`.

| Method | Behaviour | Mutually exclusive? |
|---|---|---|
| `"all"` (default) | Keep WHO methodology; overlaps allowed (a child can be both `csb_public` and `csb_private`). `csb_public + csb_private + csb_none` may exceed 100%. | No |
| `"first"` | Take the first recurring h32 source in alphabetical order (`h32a`, `h32b`, …). | Yes (sums to 100%) |
| `"public"` | Public-sector priority — if any public/CHW care, classify as public; else private if any private; else none. | Yes (sums to 100%) |
| `"private"` | Private-sector priority — if any private care, classify as private; else public if any public; else none. | Yes (sums to 100%) |

---

## 14. Country-specific notes

This section documents known data gaps and pipeline configuration notes for countries
that have been audited against the DHS variable dictionary. See
`inst/countries/{ISO3}.yml` for full per-indicator detail.

### Cote d'Ivoire (CIV)

**Audited dictionary:** `civ_dhs_mis_dictionary.csv` (Feb 2025)
**Surveys in dictionary:** DHS 1994, DHS 1998, DHS 2012, DHS 2021

- Use `min_year = "2012"` — DHS 1994 and 1998 predate the malaria module entirely.
- **PfPR (`hml35`, `hml32`):** present as columns in DHS 2012 and 2021 PR datasets but
  100% missing (`n_unique = 0`). Confirm against raw recode; pipeline will likely skip
  pfpr.
- **ITN (`hml10`, `hml12`):** confirmed in DHS 2012 and 2021 (PR).
- **ITN access (`hml18`):** present in DHS 2012 and 2021 but labelled "Pregnancy from
  individual questionnaire" — label mismatch, not the ITN access variable. Do not use
  for `itn_access` without confirming against raw PR recode.
- **IRS (`hv253`):** absent across all years — graceful skip will fire.
- **malaria_dx / act_tested:** `h47` absent all years; `ml1` present in 2012/2021 but
  labelled "Times took Fansidar during pregnancy" (IPTp, wrong meaning). Both will be
  skipped.
- **Antimalarials:** variable naming differs by year — DHS 2012 uses `ml13a-h`; DHS
  2021 uses `h37a-h`. Both sets are partial (`f` and `g` variants absent in each year).
  ACT variable (`ml13e` / `h37e`) is present in its respective year.
- **Anemia (`hw53`, `hc56`):** `hw53` confirmed in KR for 2012/2021. `hc56` present in
  PR but 100% missing in both years — use `hw53`.
- **ANC (`m14_1`):** confirmed in all years (IR). Usable for 2012/2021.
- **IPTp (`m49a_1`, `ml1_1`):** confirmed in DHS 2012 and 2021 (IR).
- **EPI (`h2`, `h7`, `h9`):** confirmed in all years (KR).
- **U5MR (`b3`, `b5`, `b7`):** `b7` (age at death) is 100% missing in 1994–2012;
  partially present in 2021 (50% missing, expected for a mortality variable). Verify
  record count.
- **SMC (`hml43`, `ml13g`):** both absent — graceful skip required.

---

### Burkina Faso (BFA)

**Audited dictionary:** `bfa_dhs_mis_dictionary.csv` (Feb 2025)
**Surveys in dictionary:** DHS 1993 only (459 variables)
**Datasets present:** GE, PR, KR — HR and IR datasets are absent

**CRITICAL:** this dictionary covers only DHS 1993, which predates the modern malaria
module entirely. BFA has multiple more recent surveys (DHS 1998/99, 2003, 2010; MIS
2010, 2014, 2017, 2021). **Do not run the BFA malaria pipeline against DHS 1993.**
Regenerate the dictionary from later surveys before proceeding.

**Findings from DHS 1993:**

- **PfPR, ITN:** all variables absent — predates the malaria module.
- **IRS:** `hv253` absent; HR dataset not in dictionary.
- **ANC, IPTp:** IR dataset absent — cannot be assessed.
- **Fever / care-seeking:** `h22` present (KR); `h32a/b/c` present; `h32d/e` absent;
  non-standard h32 variants present (`h32f/i/k/l/t/x/y/z`).
- **Antimalarials:** only `h37a` (Fansidar) present. ACT and other drug variables
  absent. `ml13a-h` entirely absent.
- **malaria_dx:** `h47` and `ml1` absent — no malaria testing module.
- **Anemia:** `hw53` and `hc56` absent — no haemoglobin data.
- **EPI (`h2`, `h7`, `h9`):** confirmed in DHS 1993 KR.
- **U5MR (`b3`, `b5`):** confirmed. `b7` shows `n_unique = 0` (100% missing — expected
  for a mortality variable where most children are alive).
- **SMC:** both `hml43` and `ml13g` absent.

All indicators except EPI and U5MR (with caveats) will require graceful skips for DHS
1993. No malaria-programme indicators are usable from this survey year.
