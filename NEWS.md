# sntmethods 0.8.0

First public release.

## DHS survey analysis

* `calc_*_dhs()` functions produce survey-weighted, design-correct estimates
  across 16 indicator domains (ITN, IRS, PfPR, ACT, antimalarials, fever care,
  ANC, IPTp, EPI, SMC, anemia, U5MR, case management, CSB, wealth). All return
  long-format `list(adm0, adm1, ...)` tables with confidence intervals, design
  effects and admin-level stratification.
* Every indicator family ships a machine-readable data dictionary
  (`act_dictionary()`, `itn_dictionary()`, …) plus the unified
  `dhs_dictionary()`.
* `dhs_read()` reads single-survey parquet files directly to preserve value
  labels and survey-specific variables.
* ACT handled as a drug *class* across multiple variables (combination therapy
  only; monotherapies excluded).
* ITN `use_if_access` (use among those with access) calculated by default with
  standard age groups (`u5`, `5_14`, `ov15`).

## Spatial modeling (MBG)

* `run_mbg_pipeline()` orchestrates the full model-based geostatistics workflow:
  survey discovery, cluster-data preparation, INLA/MBG fitting, raster
  prediction, and population-weighted aggregation to admin-2/3 with roll-up to
  admin-1 and admin-0.
* `calc_*_mbg()` / `prep_*_mbg()` prepare cluster-level data; `fit_mbg_indicator()`
  fits a single indicator; `aggregate_raster_to_admin()` performs zonal
  statistics.
* Mapping helpers: `generate_all_maps()`, `generate_indicator_map()`,
  `plot_mbg_clusters()`.
* U5MR correctly expressed per 1,000 live births; intervention-coverage
  indicators as percentages.

## Routine health facility data

* `calc_tpr()` with structured fallback logic and `validate_tpr_proxies()`.
* `calc_incidence()` implementing the N0–N5 incidence cascade, with
  `create_incidence()` and `check_incidence()`.
* `calculate_reporting_metrics()`, `classify_facility_activity()`,
  `get_active_facilities()` and `detect_outliers()` for reporting-rate and
  data-quality analytics.
* `run_grouped_stl_trend()` for STL decomposition with Mann-Kendall trend tests.

## Infrastructure

* New hex sticker logo at `man/figures/logo.png`.
* `pkgdown` site and deploy workflow.
* Hard dependencies fail fast via `rlang::check_installed()` for optional
  (`Suggests`) packages.
