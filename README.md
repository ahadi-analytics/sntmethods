# sntmethods

<!-- badges: start -->
[![R-CMD-check](https://github.com/ahadi-analytics/sntmethods/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ahadi-analytics/sntmethods/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

Analytical methods for Sub-National Tailoring (SNT) of malaria control strategies. Transforms DHS survey microdata and routine health facility data into actionable sub-national estimates for program planning.

## What this package does

**sntmethods** provides three core analytical workflows:

### 1. DHS survey analysis

Calculate 100+ survey-weighted indicators from Demographic and Health Surveys (DHS/MIS) microdata across 16 indicator domains. Covers malaria interventions, maternal and child health, and socioeconomic status. All estimates include confidence intervals, design effects, and admin-level stratification. Every indicator family includes a machine-readable data dictionary.

### 2. Spatial modeling (MBG)

Prepare cluster-level data and run model-based geostatistics (MBG) to produce continuous raster surfaces and population-weighted admin-level estimates. The `run_mbg_pipeline()` function orchestrates the full workflow in a single call:

- Auto-discovers surveys from a DHS parquet archive
- Prepares cluster-level data for each indicator
- Fits INLA/MBG models and generates prediction rasters
- Aggregates to admin-2 (or admin-3) using population-weighted zonal statistics
- Rolls up to admin-1 and admin-0 estimates
- Outputs long-format tables with numerator/denominator counts, confidence intervals, and indicator metadata

Supports 14 indicator families: `pfpr`, `itn`, `irs`, `csb`, `act`, `antimalarial`, `fever`, `anc`, `iptp`, `epi`, `u5mr`, `anemia`, `smc`, and `eff_cm` (effective case management).

### 3. Routine data analysis

Estimate malaria incidence from health facility data using the **N0-N5 cascade framework**, which progressively adjusts for testing gaps, reporting completeness, and care-seeking behavior. Also includes **test positivity rate (TPR)** calculation with structured fallback logic (rolling averages, spatial proxies, temporal proxies) and **STL trend decomposition** with Mann-Kendall tests and Sen's slope estimation for detecting trends in monthly time series.

## Indicator coverage

### DHS survey indicators

| Domain | Key indicators | DHS file | Functions |
|--------|---------------|----------|-----------|
| **Parasite prevalence** | PfPR by RDT/microscopy, age groups (u5, 5-10, u10, 2-10) | PR | `calc_pfpr_dhs()` |
| **ITN** | Ownership, access, use (all ages + u5/pregnant/age bands), use-if-access | HR, PR | `calc_itn_dhs()` |
| **IRS** | Household spraying coverage | HR | `calc_irs_dhs()` |
| **Fever** | Fever prevalence in children under 5 | KR | `calc_fever_dhs()` |
| **Care-seeking** | By sector (public, private, CHW, pharmacy, trained, none) | KR | `calc_csb_dhs()` |
| **Malaria testing** | RDT/microscopy testing among febrile children | KR | `calc_malaria_dx_dhs()` |
| **Antimalarials** | Any antimalarial treatment among febrile U5 | KR | `calc_antimalarial_dhs()` |
| **ACT treatment** | ACT receipt by source, ACT among antimalarials, ACT among care seekers | KR | `calc_act_dhs()` |
| **Case management** | Effective coverage (fever -> care-seeking -> testing -> treatment cascade) | KR | `calc_case_management_dhs()` |
| **ANC** | Antenatal care visits (1+/2+/3+/4+/8+) | IR | `calc_anc_dhs()` |
| **IPTp** | Intermittent preventive treatment doses (1+/2+/3+/4+) | IR | `calc_iptp_dhs()` |
| **EPI vaccines** | BCG, DPT, polio, measles, pentavalent, pneumococcal, rotavirus, IPV, HepB, yellow fever, malaria vaccine, fully vaccinated, zero-dose | KR | `calc_epi_dhs()` |
| **Under-5 mortality** | U5MR per 1,000 live births (via `DHS.rates`) | BR | `calc_u5mr_dhs()` |
| **Anemia** | Any, moderate+, severe (children 6-59 months) | PR | `calc_severe_anemia_dhs()` |
| **SMC** | Seasonal malaria chemoprevention coverage | KR | `calc_smc_dhs()` |
| **Wealth** | Quintile distribution, Gini coefficient (Brown formula) | HR | `calc_wealth_dhs()` |

Each survey indicator also has an MBG counterpart (`calc_*_mbg()` / `prep_*_mbg()`) for cluster-level spatial modeling.

### Routine health facility indicators

| Domain | What it does | Function |
|--------|-------------|----------|
| **Malaria incidence** | N0-N5 cascade: crude -> testing-adjusted -> reporting-adjusted -> care-seeking-adjusted | `calc_incidence()` |
| **Test positivity rate** | TPR with 5-level fallback hierarchy (rolling, district, prev year, region, national) | `calc_tpr()` |
| **Trend analysis** | STL decomposition + Mann-Kendall trend test + Sen's slope on grouped time series | `run_grouped_stl_trend()` |
| **Incidence data prep** | Validate and structure facility data for the incidence cascade | `create_incidence()`, `check_incidence()` |
| **TPR validation** | Audit proxy TPR calculations and fallback logic | `validate_tpr_proxies()` |

## Installation

```r
# Install from GitHub
remotes::install_github("ahadi-analytics/sntmethods")
```

### System dependencies (spatial features)

MBG and spatial functions require GDAL, GEOS, and PROJ:

```bash
# macOS
brew install gdal geos proj

# Ubuntu/Debian
sudo apt-get install -y libgdal-dev libgeos-dev libproj-dev libudunits2-dev
```

### MBG dependencies (optional)

For spatial modeling, install these additional R packages:

```r
# INLA (from r-inla.org)
install.packages("INLA",
  repos = c(INLA = "https://inla.r-inla-download.org/R/stable",
            CRAN = "https://cloud.r-project.org"))

# MBG engine
remotes::install_github("ihmeuw/mbg")
```

## Quick start

### DHS survey analysis

```r
library(sntmethods)

# Read DHS data from parquet archive
hr <- dhs_read(path = "path/to/parquet", file_type = "HR",
               country_code = "BU", survey_year = 2016)
pr <- dhs_read(path = "path/to/parquet", file_type = "PR",
               country_code = "BU", survey_year = 2016)
kr <- dhs_read(path = "path/to/parquet", file_type = "KR",
               country_code = "BU", survey_year = 2016)

# ITN indicators (42+ indicators, survey-weighted)
itn <- calc_itn_dhs(dhs_hr = hr, dhs_pr = pr)

# PfPR by RDT and microscopy
pfpr <- calc_pfpr_dhs(dhs_pr = pr)

# Full case management cascade
fever <- calc_fever_dhs(dhs_kr = kr)
csb   <- calc_csb_dhs(dhs_kr = kr)
act   <- calc_act_dhs(dhs_kr = kr)
cm    <- calc_case_management_dhs(dhs_kr = kr)
```

Every `calc_*_dhs()` function returns a long-format list of tibbles (`adm0`, `adm1`, etc.) with columns for `indicator`, `indicator_code`, `point`, `ci_l`, `ci_u`, `numerator`, `denominator`, and grouping variables (`adm1`, `survey_year`, etc.).

### Data dictionaries

```r
# Unified, machine-readable dictionary covering every DHS indicator domain
dhs_dictionary()
```

### Malaria incidence from routine data

```r
# Step 1: Calculate TPR with structured fallback logic
tpr_data <- calc_tpr(
  data = facility_data,
  hf_var = "hf_uid",
  adm1_var = "region",
  adm2_var = "district",
  conf_var = "confirmed",
  test_var = "tested"
)

# Step 2: Calculate incidence (N0-N5 cascade)
incidence <- calc_incidence(
  data = facility_data_with_tpr,
  levels = c("N0", "N1", "N2", "N3", "N4", "N5"),
  pop_var = "population",
  conf_var = "confirmed",
  test_var = "tested",
  pres_var = "presumed",
  reprate_var = "reporting_rate"
)
```

### Trend analysis

```r
# STL decomposition with Mann-Kendall test on monthly data
trends <- run_grouped_stl_trend(
  data = monthly_district_data,
  group_col = c("adm1", "adm2"),
  date_col = "date",
  indicators = list(
    list(col = "n2_incidence", type = "incidence"),
    list(col = "tpr",          type = "positivity")
  )
)
```

### MBG spatial pipeline

```r
# Full pipeline: cluster data -> MBG models -> raster surfaces -> admin estimates
results <- run_mbg_pipeline(
  country_iso3 = "bdi",
  adm0_sf = adm0, adm1_sf = adm1, adm2_sf = adm2,
  pop_raster = list("2016" = "path/to/bdi_pop_2016.tif"),
  pop_raster_u5 = list("2016" = "path/to/bdi_pop_u5_2016.tif"),
  path_dhs_parquet = "path/to/parquet",
  table_out_path = "output/tables",
  raster_out_path = "output/rasters",
  intermediate_out_path = "output/intermediate",
  survey_year = 2016,
  indicators = c("pfpr", "itn", "csb", "act", "eff_cm")
)

# Results are long-format tables at each admin level
results$final_dataset$adm0
results$final_dataset$adm1
results$final_dataset$adm2
```

Age-specific population rasters can be supplied for more accurate denominators: `pop_raster_u5` (child indicators), `pop_raster_wra` (ANC/IPTp), `pop_raster_1_2` (EPI 12-23 months), and age-band rasters for ITN stratification.

## Function naming conventions

| Pattern | Purpose | Example |
|---------|---------|---------|
| `calc_*_dhs()` | Survey-weighted DHS estimates (long format) | `calc_itn_dhs()` |
| `calc_*_mbg()` | Run MBG model for indicator family | `calc_itn_mbg()` |
| `prep_*_mbg()` | Prepare cluster-level data for MBG | `prep_itn_mbg()` |
| `dhs_dictionary()` | Unified dictionary across all DHS domains | `dhs_dictionary()` |
| `fit_mbg_indicator()` | All-in-one MBG fit + admin aggregation | `fit_mbg_indicator()` |
| `run_mbg_pipeline()` | Full MBG pipeline (all indicators) | `run_mbg_pipeline()` |
| `calc_incidence()` | Routine data incidence (N0-N5 cascade) | `calc_incidence()` |
| `calc_tpr()` | Test positivity rate with fallbacks | `calc_tpr()` |

## Methodology

Detailed methodology for each indicator is documented in YAML files at [`inst/methods/`](inst/methods/). These cover DHS variable mappings, inclusion criteria, calculation logic, and references to WHO/WMR standards.

| File | Domain |
|------|--------|
| `pfpr_dhs.yml` | Parasite prevalence |
| `itn_dhs.yml` | ITN ownership/access/use |
| `irs_dhs.yml` | Indoor residual spraying |
| `fever_dhs.yml` | Fever prevalence |
| `csb_dhs.yml` | Care-seeking behavior |
| `malaria_dx_dhs.yml` | Malaria diagnostic testing |
| `antimalarial_dhs.yml` | Antimalarial treatment |
| `act_dhs.yml` | ACT treatment |
| `anc_dhs.yml` | Antenatal care |
| `iptp_dhs.yml` | IPTp dosing |
| `epi_dhs.yml` | EPI vaccination |
| `u5mr_dhs.yml` | Under-5 mortality |
| `anemia_dhs.yml` | Anemia prevalence |
| `smc_dhs.yml` | SMC coverage |
| `wealth_dhs.yml` | Wealth index |
| `incidence.yml` | Incidence cascade (N0-N5) |
| `tpr.yml` | Test positivity rate |
| `reporting_rate.yml` | Reporting rate calculations |
| `outlier_detection.yml` | Outlier detection methods |
| `active_status.yml` | Facility activity classification |

Additional documentation:

- [`inst/docs/mbg_pipeline_guide.md`](inst/docs/mbg_pipeline_guide.md) -- Team reference for the MBG pipeline (setup, outputs, indicators)
- [`inst/docs/wmr_output_specification.md`](inst/docs/wmr_output_specification.md) -- World Malaria Report output format specification
- [`inst/countries/`](inst/countries/) -- Country-specific survey configuration (variable mappings, eligibility notes)

## Example scripts

Working examples are included in [`inst/scripts/`](inst/scripts/):

| Script | What it shows |
|--------|--------------|
| `example_dhs_analysis.R` | Full DHS survey analysis workflow |
| `example_tpr_incidence.R` | TPR and incidence calculation from routine data |
| `mbg_pfpr2_10_dhs.R` | MBG spatial model for PfPR 2-10 |
| `mbg_itn_access_dhs.R` | MBG spatial model for ITN access |
| `process_mis_2021_tgo.R` | Processing a Togo MIS 2021 survey |
| `diagnose_act_togo.R` | Diagnosing ACT variable detection issues |

## Related packages

- [**sntutils**](https://github.com/ahadi-analytics/sntutils) -- Companion utilities (data I/O, dictionaries, formatting)
- [**DHS.rates**](https://CRAN.R-project.org/package=DHS.rates) -- Standard DHS mortality rate calculations
- [**mbg**](https://github.com/ihmeuw/mbg) -- Model-based geostatistics engine (IHME)

## License

MIT
