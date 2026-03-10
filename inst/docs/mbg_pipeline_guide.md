# MBG Indicator Pipeline — Team Reference Guide

This guide covers the full DHS Model-Based Geostatistics (MBG) pipeline: what it produces,
how it is set up, where every output file lands, and how to load results for downstream use.
The pipeline has already been run for your country. Read the **Outputs** section to go straight
to the files you need.

---

## Table of Contents

1. [What the Pipeline Does](#1-what-the-pipeline-does)
2. [Prerequisites](#2-prerequisites)
3. [Step-by-step Setup](#3-step-by-step-setup)
   - [3.1 Population Rasters](#31-population-rasters)
   - [3.2 DHS Data Path](#32-dhs-data-path)
   - [3.3 Project Paths and Shapefiles](#33-project-paths-and-shapefiles)
   - [3.4 Population Raster Path Lists](#34-population-raster-path-lists)
4. [Running the Pipeline](#4-running-the-pipeline)
5. [Outputs — Full Reference](#5-outputs--full-reference)
   - [5.1 Cluster Point Files (`table_out_path`)](#51-cluster-point-files-table_out_path)
   - [5.2 Annual Summary Tables (`table_out_path`)](#52-annual-summary-tables-table_out_path)
   - [5.3 Combined Multi-Year Table (`table_out_path`)](#53-combined-multi-year-table-table_out_path)
   - [5.4 Prediction Rasters (`raster_out_path`)](#54-prediction-rasters-raster_out_path)
   - [5.5 Intermediate / Cache Files (`intermediate_out_path`)](#55-intermediate--cache-files-intermediate_out_path)
   - [5.6 Maps (`fig_out_path`)](#56-maps-fig_out_path)
6. [Final Dataset Structure](#6-final-dataset-structure)
7. [Loading Results in R](#7-loading-results-in-r)
8. [Indicators Reference](#8-indicators-reference)
9. [Units and Scaling](#9-units-and-scaling)
10. [Graceful Skips](#10-graceful-skips)
11. [Country-Specific Notes](#11-country-specific-notes)

---

## 1. What the Pipeline Does

`run_mbg_indicator_pipeline()` is the single entry point for all DHS-based spatial indicator
work. It:

1. **Discovers** all available DHS/MIS surveys for a country (parquet archive)
2. **Loads** the relevant recode files (KR, IR, PR, HR, BR) and GPS coordinates per survey year
3. **Prepares cluster-level data** for each indicator — unweighted counts at GPS cluster points,
   which is what MBG needs
4. **Runs MBG spatial models** (Gaussian process surface fitting) to smooth cluster observations
   into continuous raster surfaces with uncertainty bounds
5. **Aggregates** pixel-level raster predictions to ADM2 (or ADM3) level using population
   rasters as weights
6. **Saves** cluster files, rasters, maps, summary tables, and a combined multi-year dataset

---

## 2. Prerequisites

| Requirement | What it is | Where it comes from |
|---|---|---|
| DHS parquet archive | All recode files (KR, IR, PR, HR, BR, GE) stored as partitioned parquet | Ahadi OneDrive — see §3.2 |
| Country shapefile (ADM0–ADM3) | Cleaned, validated sf polygons | Ahadi OneDrive SNT project folder |
| Total population rasters | WorldPop 1 km rasters, one per survey year | Downloaded via `sntutils::download_worldpop()` |
| Under-5 population rasters | WorldPop age-band rasters (0–4), one per survey year | Downloaded via `sntutils::download_worldpop_age_band()` |

---

## 3. Step-by-step Setup

### 3.1 Population Rasters

Two sets of rasters are needed: **total population** and **under-5 population**. Under-5 rasters
are built by summing individual single-year-of-age bands for ages 0–4.

```r
library(sntutils)

country_iso3 <- "bdi"  # parameterise at the top of your script

# --- Total population (all ages) ---
sntutils::download_worldpop(
  country_codes = toupper(country_iso3),
  years         = 2000:2029,
  dest_dir      = here::here(paths$pop_worldpop, "raw")
)

# --- Under-5 population (ages 0–4, summed to single raster per year) ---
sntutils::download_worldpop_age_band(
  country_codes = toupper(country_iso3),
  years         = 2000:2029,
  age_range     = c(0, 4),
  out_dir       = here::here(paths$pop_worldpop, "raw", "aged_rasters")
)
```

After downloading, your `pop_worldpop` folder will contain:

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

> **Note:** The file naming convention for total-population rasters changed between WorldPop
> versions. Files up to ~2015 typically use `_ppp_` in the name, while later years use
> `_pop_`. Always check the exact filenames after downloading and update the path lists
> in §3.4 accordingly.

### 3.2 DHS Data Path

DHS data are stored as partitioned parquet files on Ahadi OneDrive. Use `ahadi_path()` to
build an OS-agnostic path:

```r
path_dhs_parquet <- here::here(
  sntmethods::ahadi_path(
    base = "Internal docs and resources/data/dhs_data"
  ),
  "01_data/parquet"
)
```

> **Windows path-length note:** Windows has a 256-character path limit that can block access
> to deeply nested OneDrive folders. Until DHS data are migrated to a cloud API, you may need
> to shorten the path by mapping the OneDrive root to a drive letter
> (e.g. `subst O: "C:\Users\you\OneDrive - Ahadi Analytics"`).

### 3.3 Project Paths and Shapefiles

```r
# Build all standard SNT folder paths (tables, figures, shapefiles, etc.)
paths <- sntutils::setup_project_paths(
  base_path = Sys.getenv("AHADI_ONEDRIVE_PROJECT"),
  quiet     = TRUE
)

# Load country shapefiles (ADM0 through ADM3 in one qs2 object)
shp_list <- sntutils::read_snt_data(
  data_name    = glue::glue("{country_iso3}_adm0_adm2_post2023"),
  path         = here::here(paths$admin_shp, "processed"),
  file_formats = "qs2"
)$final_spat_vec

adm0_sf <- shp_list$adm0
adm1_sf <- shp_list$adm1
adm2_sf <- shp_list$adm2
adm3_sf <- shp_list$adm3   # NULL if ADM3 not available for this country
```

### 3.4 Population Raster Path Lists

One named list entry per survey year. The key must exactly match the survey year the pipeline
will process (as a character string). Update for your country and the years of your surveys:

```r
# Total population
pop_raster_paths <- list(
  "2010" = here::here(
    paths$pop_worldpop,
    glue::glue("raw/{country_iso3}_ppp_2010_1km_Aggregated_UNadj.tif")
  ),
  "2012" = here::here(
    paths$pop_worldpop,
    glue::glue("raw/{country_iso3}_ppp_2012_1km_Aggregated_UNadj.tif")
  ),
  "2016" = here::here(
    paths$pop_worldpop,
    glue::glue("raw/{country_iso3}_pop_2016_CN_1km_UA_v1.tif")
  )
)

# Under-5 population
pop_raster_u5_paths <- list(
  "2010" = here::here(
    paths$pop_worldpop,
    glue::glue("raw/aged_rasters/{country_iso3}_total_00_04_2010.tif")
  ),
  "2012" = here::here(
    paths$pop_worldpop,
    glue::glue("raw/aged_rasters/{country_iso3}_total_00_04_2012.tif")
  ),
  "2016" = here::here(
    paths$pop_worldpop,
    glue::glue("raw/aged_rasters/{country_iso3}_total_00_04_2016.tif")
  )
)
```

---

## 4. Running the Pipeline

```r
results_dhs <- sntmethods::run_mbg_indicator_pipeline(
  country_iso3          = toupper(country_iso3),  # e.g. "BDI"
  adm0_sf               = adm0_sf,
  adm1_sf               = adm1_sf,
  adm2_sf               = adm2_sf,
  adm3_sf               = adm3_sf,
  pop_raster            = pop_raster_paths,
  pop_raster_u5         = pop_raster_u5_paths,
  path_dhs_parquet      = path_dhs_parquet,
  table_out_path        = here::here(paths$dhs, "processed"),
  fig_out_path          = paths$final_fig,
  intermediate_out_path = paths$model_tbl,
  raster_out_path       = paths$model_fig,
  min_year              = "2005",
  indicators            = c(
    "pfpr", "itn", "irs", "anc", "csb", "anemia", "iptp",
    "fever", "antimalarial", "act", "epi", "u5mr", "smc"
  ),
  run_mbg               = TRUE,
  save_rasters          = TRUE,
  generate_maps         = TRUE,
  cache                 = FALSE   # set TRUE on subsequent runs to reuse the model grid
)
```

**Key parameters:**

| Parameter | What it controls |
|---|---|
| `indicators` | Which indicator categories to run (see §8 for full list) |
| `min_year` | Exclude surveys before this year (useful to drop very old surveys missing key variables) |
| `run_mbg` | Set `FALSE` to prepare cluster data only (faster, no model fitting) |
| `cache` | `FALSE` on first run — builds model grid from scratch. `TRUE` on re-runs — reuses cached grid and skips re-computation |
| `save_rasters` | Write mean/lower/upper prediction rasters to `raster_out_path` |
| `aggregation_level` | `"adm2"` (default) or `"adm3"` — the finest level in `final_dataset` |

---

## 5. Outputs — Full Reference

All output paths below use the `paths` object from §3.3. For Burundi (`bdi`) with surveys in
2010, 2012, and 2016, concrete examples are shown.

---

### 5.1 Cluster Point Files (`table_out_path`)

**What they are:** One file per indicator per survey year. Each file contains the raw
GPS cluster observations used as MBG model inputs — unweighted counts of events and
sample sizes at each surveyed cluster location.

**Location:**
```
{paths$dhs}/processed/
```

**Naming pattern:**
```
{country}_{indicator}_cluster_points_{survey_type}_{year}_v{YYYYMMDD}.qs2
```

**Burundi examples:**
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

> **Versioning:** The pipeline keeps the 3 most recent versions of each file. Older versions
> are pruned automatically. Always use `sntutils::read_snt_data()` (not direct file paths)
> to load the latest version — it resolves the date stamp for you.

**Column structure of cluster point files:**

| Column | Type | Description |
|---|---|---|
| `cluster_id` | integer | DHS cluster identifier |
| `x` | numeric | Longitude (decimal degrees) |
| `y` | numeric | Latitude (decimal degrees) |
| `{numerator}` | integer | Count of events (name is indicator-specific, e.g. `n_positive`, `n_with_access`, `n_vaccinated`, `n_deaths`) |
| `{denominator}` | integer | Sample size (e.g. `n_tested`, `n_individuals`, `n_children`, `n_exposed`) |
| `prop_raw` | numeric | Raw proportion = numerator / denominator |

Numerator/denominator column names by indicator group:

| Indicator group | Numerator column | Denominator column |
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

---

### 5.2 Annual Summary Tables (`table_out_path`)

**What they are:** One dataset per survey year containing MBG-modelled estimates at ADM2
(or ADM3) level for all indicators processed in that year. Saved in both `.qs2` (fast binary)
and `.xlsx` (with embedded data dictionary on Sheet 2).

**Location:**
```
{paths$dhs}/processed/
```

**Naming pattern:**
```
{country}_mbg_indicators_{year}_v{YYYYMMDD}.qs2
{country}_mbg_indicators_{year}_v{YYYYMMDD}.xlsx
```

**Burundi examples:**
```
bdi_mbg_indicators_2010_v20250226.qs2
bdi_mbg_indicators_2010_v20250226.xlsx
bdi_mbg_indicators_2012_v20250226.qs2
bdi_mbg_indicators_2012_v20250226.xlsx
bdi_mbg_indicators_2016_v20250226.qs2
bdi_mbg_indicators_2016_v20250226.xlsx
```

The XLSX file has two sheets:
- **Sheet 1 (`data`):** The estimates table
- **Sheet 2 (`dictionary`):** Column-level metadata (label, units, DHS variable, recode file,
  numerator/denominator definition)

---

### 5.3 Combined Multi-Year Table (`table_out_path`)

**What it is:** All survey years stacked into one table via `bind_rows()`. This is the
primary file for trend analysis and most downstream SNT work.

**Location:**
```
{paths$dhs}/processed/
```

**Naming pattern:**
```
{country}_mbg_indicators_combined_v{YYYYMMDD}.qs2
{country}_mbg_indicators_combined_v{YYYYMMDD}.xlsx
```

**Burundi example:**
```
bdi_mbg_indicators_combined_v20250226.qs2
bdi_mbg_indicators_combined_v20250226.xlsx
```

> This file is only produced when more than one survey year is processed in the same run.
> It is the file you should load for most analyses — it has every indicator for every year
> in one place.

---

### 5.4 Prediction Rasters (`raster_out_path`)

**What they are:** Continuous raster surfaces at ~1 km² resolution covering the entire
country, produced by the MBG spatial model. Three rasters per indicator per year: mean
prediction, lower 95% credible interval, upper 95% credible interval.

**Location:**
```
{paths$model_fig}/
```

**Naming pattern:**
```
{country}_{indicator}_mbg_{survey_type}_{year}_mean.tif
{country}_{indicator}_mbg_{survey_type}_{year}_lower.tif
{country}_{indicator}_mbg_{survey_type}_{year}_upper.tif
```

**Burundi examples:**
```
bdi_pfpr_rdt_u5_mbg_dhs_2016_mean.tif
bdi_pfpr_rdt_u5_mbg_dhs_2016_lower.tif
bdi_pfpr_rdt_u5_mbg_dhs_2016_upper.tif

bdi_itn_access_mbg_dhs_2016_mean.tif
bdi_itn_access_mbg_dhs_2016_lower.tif
bdi_itn_access_mbg_dhs_2016_upper.tif

bdi_u5mr_mbg_dhs_2016_mean.tif
bdi_u5mr_mbg_dhs_2016_lower.tif
bdi_u5mr_mbg_dhs_2016_upper.tif
```

**Raster units:**
- Most indicators (PfPR, ITN, ANC, IPTp, etc.): **0–100** (percentage scale)
- U5MR: **0–1000** (per 1,000 live births)

> Rasters are also used by the caching mechanism. If all three files (`_mean`, `_lower`,
> `_upper`) already exist for an indicator-year pair and `cache = TRUE`, the pipeline skips
> re-fitting the MBG model for that combination.

---

### 5.5 Intermediate / Cache Files (`intermediate_out_path`)

**What they are:** Pre-computed spatial lookup structures that make repeated pipeline runs
fast. The most expensive computations (rasterising ADM2 polygons, building pixel-to-admin
mapping tables) are done once and saved here.

**Location:**
```
{paths$model_tbl}/
```

**Files produced:**

| File | Description |
|---|---|
| `{country}_id_raster_adm2.tif` | Integer raster where each pixel value = the ADM2 polygon it belongs to |
| `{country}_id_raster_adm3.tif` | Same for ADM3 (only when `aggregation_level = "adm3"`) |
| `{country}_aggregation_table_adm2.parquet` | Pixel-to-ADM2 mapping table with population weights |
| `{country}_aggregation_table_adm3.parquet` | Same for ADM3 |

**Burundi examples:**
```
bdi_id_raster_adm2.tif
bdi_aggregation_table_adm2.parquet
```

> Set `cache = TRUE` after the first run and these files will be reused, saving substantial
> time on every subsequent run.

---

### 5.6 Maps (`fig_out_path`)

**What they are:** Choropleth maps of modelled estimates at ADM2 level, generated when
`generate_maps = TRUE`.

**Location:**
```
{paths$final_fig}/
```

**Naming pattern:**
```
{country}_{indicator}_mbg_{survey_type}_{year}_adm2.png
{country}_{indicator}_mbg_{survey_type}_{year}_raster.png
{country}_{indicator}_mbg_{survey_type}_{year}_adm2_clusters.png
```

**Burundi examples:**
```
bdi_pfpr_rdt_u5_mbg_dhs_2016_adm2.png          # ADM2 choropleth of mean estimate
bdi_pfpr_rdt_u5_mbg_dhs_2016_raster.png        # Continuous raster map
bdi_pfpr_rdt_u5_mbg_dhs_2016_adm2_clusters.png # Choropleth with cluster dots overlaid
```

---

## 6. Final Dataset Structure

`results_dhs$final_dataset` (and the combined `.qs2`/`.xlsx` files) have the following
column layout:

### Identifier columns (always present)

| Column | Type | Example |
|---|---|---|
| `iso3_code` | character | `"BDI"` |
| `dhs_code` | character | `"BU"` |
| `adm0` | character | `"Burundi"` |
| `adm1` | character | `"Bujumbura Mairie"` |
| `adm2` | character | `"Muha"` |
| `adm3` | character | `"Kanyosha"` (only when `aggregation_level = "adm3"`) |
| `survey_year` | integer | `2016` |
| `survey_type` | character | `"DHS"` |

### MBG estimate columns (per indicator)

Pattern: `{indicator}_{statistic}` where statistic is `mean`, `lower`, or `upper`.

```
pfpr_rdt_u5_mean       pfpr_rdt_u5_lower       pfpr_rdt_u5_upper
pfpr_mic_u5_mean       pfpr_mic_u5_lower       pfpr_mic_u5_upper
itn_ownership_mean     itn_ownership_lower     itn_ownership_upper
itn_access_mean        itn_access_lower        itn_access_upper
itn_use_u5_mean        itn_use_u5_lower        itn_use_u5_upper
itn_use_if_access_mean itn_use_if_access_lower itn_use_if_access_upper
anc_1plus_mean         anc_1plus_lower         anc_1plus_upper
anc_4plus_mean         anc_4plus_lower         anc_4plus_upper
iptp_3plus_mean        iptp_3plus_lower        iptp_3plus_upper
irs_coverage_mean      irs_coverage_lower      irs_coverage_upper
anemia_any_mean        anemia_any_lower        anemia_any_upper
epi_dpt3_mean          epi_dpt3_lower          epi_dpt3_upper
epi_measles1_mean      epi_measles1_lower      epi_measles1_upper
u5mr_mean              u5mr_lower              u5mr_upper
smc_receipt_mean       smc_receipt_lower       smc_receipt_upper
fever_mean             fever_lower             fever_upper
```

### Cluster statistics columns (per indicator)

Aggregated from cluster data to ADM2 level, appended alongside MBG estimates.

Pattern: `n_tested_{indicator}`, `n_pos_{indicator}`, `n_clusters_{indicator}`, `{indicator}_raw`

```
n_tested_pfpr_rdt_u5    n_pos_pfpr_rdt_u5    n_clusters_pfpr_rdt_u5    pfpr_rdt_u5_raw
n_tested_itn_access     n_pos_itn_access     n_clusters_itn_access     itn_access_raw
n_tested_anc_4plus      n_pos_anc_4plus      n_clusters_anc_4plus      anc_4plus_raw
...
```

---

## 7. Loading Results in R

### Load the combined multi-year table

```r
# Always use read_snt_data — it resolves the latest version automatically
combined <- sntutils::read_snt_data(
  data_name    = glue::glue("{country_iso3}_mbg_indicators_combined"),
  path         = here::here(paths$dhs, "processed"),
  file_formats = "qs2"
)$data

# Filter to a single year
bdi_2016 <- combined |> dplyr::filter(survey_year == 2016)
```

### Load a specific annual table

```r
bdi_2016 <- sntutils::read_snt_data(
  data_name    = "bdi_mbg_indicators_2016",
  path         = here::here(paths$dhs, "processed"),
  file_formats = "qs2"
)$data
```

### Load cluster point data for a specific indicator

```r
pfpr_clusters_2016 <- sntutils::read_snt_data(
  data_name    = "bdi_pfpr_rdt_u5_cluster_points_dhs_2016",
  path         = here::here(paths$dhs, "processed"),
  file_formats = "qs2"
)$data
```

### Load a prediction raster

```r
library(terra)

pfpr_raster_mean <- terra::rast(
  here::here(paths$model_fig, "bdi_pfpr_rdt_u5_mbg_dhs_2016_mean.tif")
)

# Uncertainty bounds
pfpr_raster_lower <- terra::rast(
  here::here(paths$model_fig, "bdi_pfpr_rdt_u5_mbg_dhs_2016_lower.tif")
)
pfpr_raster_upper <- terra::rast(
  here::here(paths$model_fig, "bdi_pfpr_rdt_u5_mbg_dhs_2016_upper.tif")
)
```

### Access results from the pipeline return object (in-session)

```r
# Final combined ADM2 dataset (same as the saved combined file)
final_df <- results_dhs$final_dataset

# Cluster-level data (in memory, one list per year per indicator)
pfpr_clusters <- results_dhs$cluster_data[["2016"]][["pfpr_rdt_u5"]]

# Skipped indicators and reasons
results_dhs$skipped_indicators
# e.g. list("2012" = list(irs = "IRS variable not available for this survey"))
```

---

## 8. Indicators Reference

| `indicators` key | Sub-indicators produced | DHS recode | Age group |
|---|---|---|---|
| `pfpr` | `pfpr_rdt_u5`, `pfpr_mic_u5`, `pfpr_rdt_5_10`, `pfpr_either_u5`, … | PR | 6–59 months; 5–10 years; 6–119 months |
| `itn` | `itn_ownership`, `itn_access`, `itn_use_all`, `itn_use_u5`, `itn_use_pregnant`, `itn_use_if_access` | HR + PR | All ages / U5 / pregnant |
| `irs` | `irs_coverage` | HR | Household |
| `anc` | `anc_1plus`, `anc_3plus`, `anc_4plus`, `anc_8plus` | IR | Women with recent birth |
| `iptp` | `iptp_1plus`, `iptp_2plus`, `iptp_3plus`, `iptp_4plus` | IR | Women with recent birth |
| `anemia` | `anemia_any`, `anemia_moderate_plus`, `anemia_severe` | PR | 6–59 months |
| `epi` | `epi_bcg`, `epi_dpt3`, `epi_measles1`, `epi_polio3`, … | KR | 12–23 months |
| `u5mr` | `u5mr` | BR | Under 5 |
| `smc` | `smc_receipt` | KR | Under 5 |
| `fever` | `fever` | KR | Under 5 |
| `antimalarial` | `antimalarial`, `antimalarial_public` | KR | Febrile U5 |
| `act` | `act`, `act_public`, `act_among_am` | KR | Febrile U5 |
| `csb` | `csb` (care-seeking) | KR | Febrile U5 |
| `irs` | `irs_coverage` | HR | Household |

Individual ITN sub-indicators can be passed directly (e.g. `indicators = c("itn_access", "itn_use_u5")`)
for faster runs when you only need specific sub-indicators. Similarly, `act_public`, `act_among_am`,
and `antimalarial_public` can be requested as top-level pipeline indicators.

---

## 9. Units and Scaling

| Indicator group | Output scale | Example value |
|---|---|---|
| PfPR, ITN, ANC, IPTp, IRS, EPI, SMC, fever, antimalarial, anemia | **0–100** (percentage) | `47.3` = 47.3% |
| U5MR | **0–1000** (per 1,000 live births) | `68.1` = 68.1 per 1,000 live births |

This applies to both the `_mean`/`_lower`/`_upper` columns in the final dataset and to pixel
values in the `.tif` rasters.

---

## 10. Graceful Skips

Not every DHS/MIS survey contains every indicator variable. When a required variable is absent
(e.g. `hv253` for IRS, `hml43`/`ml13g` for SMC, `hml35`/`hml32` for PfPR in older surveys,
`b7` for U5MR in MIS files), the pipeline **skips that indicator gracefully** rather than
crashing. A warning is printed and the reason is recorded in `results_dhs$skipped_indicators`.

```r
# Check what was skipped and why
results_dhs$skipped_indicators
# Example output:
# $`2012`
# $`2012`$irs
# [1] "IRS variable not available for this survey"
#
# $`2012`$smc
# [1] "No SMC variable found; checked hml43 and ml13g; SMC not available for this survey"
```

Skipped indicators simply produce no rows for that year in the final dataset (the column exists
but all values are `NA` for that year). This is expected and correct behaviour.

---

## 11. Country-Specific Notes

This section documents known data gaps and pipeline configuration notes for countries that have
been audited against the DHS variable dictionary. See `inst/countries/{ISO3}.yml` for full
per-indicator detail.

### Cote d'Ivoire (CIV)

**Audited dictionary:** `civ_dhs_mis_dictionary.csv` (Feb 2025)
**Surveys in dictionary:** DHS 1994, DHS 1998, DHS 2012, DHS 2021

**Key findings:**

- Use `min_year = "2012"` — DHS 1994 and 1998 predate the malaria module entirely.
- **PfPR (`hml35`, `hml32`):** Present as columns in DHS 2012 and 2021 PR datasets but
  100% missing (`n_unique = 0`). The malaria testing module may not have been implemented
  in these DHS rounds. Confirm against raw recode; pipeline will likely skip pfpr.
- **ITN (`hml10`, `hml12`):** Confirmed present in DHS 2012 and 2021 (PR).
- **ITN access (`hml18`):** Present in DHS 2012 and 2021 but labeled "Pregnancy from
  individual questionnaire" — label mismatch, not the ITN access variable. Do not use for
  `itn_access` without confirming against raw PR recode.
- **IRS (`hv253`):** Absent across all years — graceful skip will fire.
- **malaria_dx / act_tested:** `h47` absent all years; `ml1` present in 2012/2021 but
  labeled "Times took Fansidar during pregnancy" (IPTp, wrong meaning). Both steps will
  be skipped.
- **Antimalarials:** Variable naming differs by year — DHS 2012 uses `ml13a-h`; DHS 2021
  uses `h37a-h`. Both sets are partial (f and g variants absent in each year). ACT variable
  (`ml13e` / `h37e`) is present in its respective year.
- **Anemia (`hw53`, `hc56`):** `hw53` confirmed in KR for 2012/2021. `hc56` present in PR
  but 100% missing in both years — use `hw53` for anemia calculations.
- **ANC (`m14_1`):** Confirmed present in all years (IR). Usable for DHS 2012/2021.
- **IPTp (`m49a_1`, `ml1_1`):** Confirmed in DHS 2012 and 2021 (IR).
- **EPI (`h2`, `h7`, `h9`):** Confirmed in all years (KR).
- **U5MR (`b3`, `b5`, `b7`):** `b7` (age at death) is 100% missing in 1994-2012; partially
  present in 2021 (50% missing, as expected for a mortality variable). Verify record count.
- **SMC (`hml43`, `ml13g`):** Both absent — graceful skip required.

---

### Burkina Faso (BFA)

**Audited dictionary:** `bfa_dhs_mis_dictionary.csv` (Feb 2025)
**Surveys in dictionary:** DHS 1993 only (459 variables)
**Datasets present:** GE, PR, KR — HR and IR datasets are absent

**CRITICAL:** This dictionary covers only DHS 1993, which predates the modern malaria
module entirely. Burkina Faso has multiple more recent surveys (DHS 1998/99, 2003, 2010;
MIS 2010, 2014, 2017, 2021). **Do not run the BFA malaria pipeline against DHS 1993.**
Regenerate the dictionary from later surveys before proceeding.

**Key findings from DHS 1993 dictionary:**

- **PfPR, ITN:** All variables absent — predates the malaria module.
- **IRS:** `hv253` absent; HR dataset not in dictionary.
- **ANC, IPTp:** IR dataset absent from dictionary — cannot be assessed.
- **Fever / care-seeking:** `h22` present (KR); `h32a/b/c` present; `h32d/e` absent;
  non-standard h32 variants present (`h32f/i/k/l/t/x/y/z`).
- **Antimalarials:** Only `h37a` (Fansidar) present. ACT and other drug variables absent.
  `ml13a-h` entirely absent.
- **malaria_dx:** `h47` and `ml1` absent — no malaria testing module.
- **Anemia:** `hw53` and `hc56` absent — no haemoglobin data.
- **EPI (`h2`, `h7`, `h9`):** Confirmed present in DHS 1993 KR.
- **U5MR (`b3`, `b5`):** Confirmed. `b7` shows `n_unique = 0` (100% missing — expected for
  a mortality variable where most children are alive).
- **SMC:** Both `hml43` and `ml13g` absent.

All indicators except EPI and u5mr (with caveats) will require graceful skips for DHS 1993.
No malaria programme indicators are usable from this survey year.
