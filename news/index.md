# Changelog

## sntmethods 0.8.0

First public release.

### DHS survey analysis

- `calc_*_dhs()` functions produce survey-weighted, design-correct
  estimates across 16 indicator domains (ITN, IRS, PfPR, ACT,
  antimalarials, fever care, ANC, IPTp, EPI, SMC, anemia, U5MR, case
  management, CSB, wealth). All return long-format
  `list(adm0, adm1, ...)` tables with confidence intervals, design
  effects and admin-level stratification.
- Every indicator family ships a machine-readable data dictionary
  ([`act_dictionary()`](https://ahadi-analytics.github.io/sntmethods/reference/act_dictionary.md),
  [`itn_dictionary()`](https://ahadi-analytics.github.io/sntmethods/reference/itn_dictionary.md),
  …) plus the unified
  [`dhs_dictionary()`](https://ahadi-analytics.github.io/sntmethods/reference/dhs_dictionary.md).
- [`dhs_read()`](https://ahadi-analytics.github.io/sntmethods/reference/dhs_read.md)
  reads single-survey parquet files directly to preserve value labels
  and survey-specific variables.
- ACT handled as a drug *class* across multiple variables (combination
  therapy only; monotherapies excluded).
- ITN `use_if_access` (use among those with access) calculated by
  default with standard age groups (`u5`, `5_14`, `ov15`).

### Spatial modeling (MBG)

- [`run_mbg_pipeline()`](https://ahadi-analytics.github.io/sntmethods/reference/run_mbg_pipeline.md)
  orchestrates the full model-based geostatistics workflow: survey
  discovery, cluster-data preparation, INLA/MBG fitting, raster
  prediction, and population-weighted aggregation to admin-2/3 with
  roll-up to admin-1 and admin-0.
- `calc_*_mbg()` / `prep_*_mbg()` prepare cluster-level data;
  [`fit_mbg_indicator()`](https://ahadi-analytics.github.io/sntmethods/reference/fit_mbg_indicator.md)
  fits a single indicator;
  [`aggregate_raster_to_admin()`](https://ahadi-analytics.github.io/sntmethods/reference/aggregate_raster_to_admin.md)
  performs zonal statistics.
- Mapping helpers:
  [`generate_all_maps()`](https://ahadi-analytics.github.io/sntmethods/reference/generate_all_maps.md),
  [`generate_indicator_map()`](https://ahadi-analytics.github.io/sntmethods/reference/generate_indicator_map.md),
  [`plot_mbg_clusters()`](https://ahadi-analytics.github.io/sntmethods/reference/plot_mbg_clusters.md).
- U5MR correctly expressed per 1,000 live births; intervention-coverage
  indicators as percentages.

### Routine health facility data

- [`calc_tpr()`](https://ahadi-analytics.github.io/sntmethods/reference/calc_tpr.md)
  with structured fallback logic and
  [`validate_tpr_proxies()`](https://ahadi-analytics.github.io/sntmethods/reference/validate_tpr_proxies.md).
- [`calc_incidence()`](https://ahadi-analytics.github.io/sntmethods/reference/calc_incidence.md)
  implementing the N0–N5 incidence cascade, with
  [`create_incidence()`](https://ahadi-analytics.github.io/sntmethods/reference/create_incidence.md)
  and
  [`check_incidence()`](https://ahadi-analytics.github.io/sntmethods/reference/check_incidence.md).
- [`calculate_reporting_metrics()`](https://ahadi-analytics.github.io/sntmethods/reference/sntutils_reexports.md),
  [`classify_facility_activity()`](https://ahadi-analytics.github.io/sntmethods/reference/sntutils_reexports.md),
  [`get_active_facilities()`](https://ahadi-analytics.github.io/sntmethods/reference/sntutils_reexports.md)
  and
  [`detect_outliers()`](https://ahadi-analytics.github.io/sntmethods/reference/sntutils_reexports.md)
  for reporting-rate and data-quality analytics.
- [`run_grouped_stl_trend()`](https://ahadi-analytics.github.io/sntmethods/reference/run_grouped_stl_trend.md)
  for STL decomposition with Mann-Kendall trend tests.

### Infrastructure

- New hex sticker logo at `man/figures/logo.png`.
- `pkgdown` site and deploy workflow.
- Hard dependencies fail fast via
  [`rlang::check_installed()`](https://rlang.r-lib.org/reference/is_installed.html)
  for optional (`Suggests`) packages.
