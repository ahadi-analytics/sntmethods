# Package index

## DHS survey indicators

Survey-weighted, design-correct estimates from DHS/MIS microdata. Each
returns long-format admin-level tables with confidence intervals and
design effects.

- [`calc_itn_dhs()`](https://ahadi-analytics.github.io/sntmethods/reference/calc_itn_dhs.md)
  : Calculate ITN Indicators from DHS Data
- [`calc_irs_dhs()`](https://ahadi-analytics.github.io/sntmethods/reference/calc_irs_dhs.md)
  : Calculate IRS Coverage from DHS Data
- [`calc_pfpr_dhs()`](https://ahadi-analytics.github.io/sntmethods/reference/calc_pfpr_dhs.md)
  : Calculate PfPR Indicators from DHS Data
- [`calc_act_dhs()`](https://ahadi-analytics.github.io/sntmethods/reference/calc_act_dhs.md)
  : Calculate ACT Treatment Indicators from DHS Data
- [`calc_antimalarial_dhs()`](https://ahadi-analytics.github.io/sntmethods/reference/calc_antimalarial_dhs.md)
  : Calculate Antimalarial Treatment from DHS Data (Standardized)
- [`calc_fever_dhs()`](https://ahadi-analytics.github.io/sntmethods/reference/calc_fever_dhs.md)
  : Calculate Fever Prevalence from DHS Data
- [`calc_malaria_dx_dhs()`](https://ahadi-analytics.github.io/sntmethods/reference/calc_malaria_dx_dhs.md)
  : Calculate Malaria Diagnostic Testing from DHS Data (Standardized)
- [`calc_case_management_dhs()`](https://ahadi-analytics.github.io/sntmethods/reference/calc_case_management_dhs.md)
  : Calculate Effective Coverage of Case Management from DHS Data
- [`calc_anc_dhs()`](https://ahadi-analytics.github.io/sntmethods/reference/calc_anc_dhs.md)
  : Calculate ANC Coverage from DHS Data (standardized long-format
  output)
- [`calc_iptp_dhs()`](https://ahadi-analytics.github.io/sntmethods/reference/calc_iptp_dhs.md)
  : Calculate IPTp Coverage from DHS Data (standardized long-format
  output)
- [`calc_epi_dhs()`](https://ahadi-analytics.github.io/sntmethods/reference/calc_epi_dhs.md)
  : Calculate EPI Coverage from DHS Data (Standardized)
- [`calc_smc_dhs()`](https://ahadi-analytics.github.io/sntmethods/reference/calc_smc_dhs.md)
  : Calculate SMC Coverage from DHS Data
- [`calc_severe_anemia_dhs()`](https://ahadi-analytics.github.io/sntmethods/reference/calc_severe_anemia_dhs.md)
  : Calculate Severe Anemia Prevalence from DHS Data (Standardized)
- [`calc_u5mr_dhs()`](https://ahadi-analytics.github.io/sntmethods/reference/calc_u5mr_dhs.md)
  : Calculate U5MR from DHS Data (Standardized Long Format)
- [`calc_csb_dhs()`](https://ahadi-analytics.github.io/sntmethods/reference/calc_csb_dhs.md)
  : Calculate Care-Seeking Behavior from DHS Data ( Methodology)
- [`calc_csb_by_wealth_dhs()`](https://ahadi-analytics.github.io/sntmethods/reference/calc_csb_by_wealth_dhs.md)
  : Calculate Care-Seeking Behavior by Wealth Quintile from DHS Data
- [`calc_wealth_dhs()`](https://ahadi-analytics.github.io/sntmethods/reference/calc_wealth_dhs.md)
  : Calculate Wealth Quintile Distributions from DHS Data
- [`calculate_dhs_gini()`](https://ahadi-analytics.github.io/sntmethods/reference/calculate_dhs_gini.md)
  : Calculate Gini Coefficient Using DHS Brown Formula Methodology

## DHS data access and dictionaries

Read DHS parquet archives, inspect variable labels, and look up
indicator metadata.

- [`dhs_read()`](https://ahadi-analytics.github.io/sntmethods/reference/dhs_read.md)
  : Read a DHS recode from a hive-partitioned parquet archive
- [`dhs_dictionary()`](https://ahadi-analytics.github.io/sntmethods/reference/dhs_dictionary.md)
  : Master DHS Indicator Dictionary
- [`dhs_model_datasets()`](https://ahadi-analytics.github.io/sntmethods/reference/dhs_model_datasets.md)
  : Get DHS Model Dataset URLs and Metadata
- [`make_dhs_raw_dictionary()`](https://ahadi-analytics.github.io/sntmethods/reference/make_dhs_raw_dictionary.md)
  : Create Data Dictionary for DHS Raw Datasets
- [`list_dhs_var_labels()`](https://ahadi-analytics.github.io/sntmethods/reference/list_dhs_var_labels.md)
  : List DHS variables and their haven labels by name pattern
- [`join_dhs_coords()`](https://ahadi-analytics.github.io/sntmethods/reference/join_dhs_coords.md)
  : Join DHS GPS Coordinates to Person Records Data

## MBG pipeline and modeling

Model-based geostatistics workflow - from cluster data to fitted models,
prediction rasters and population-weighted admin estimates.

- [`run_mbg_pipeline()`](https://ahadi-analytics.github.io/sntmethods/reference/run_mbg_pipeline.md)
  : Run MBG Indicator Pipeline
- [`mbg_pipeline`](https://ahadi-analytics.github.io/sntmethods/reference/mbg_pipeline.md)
  [`run_mbg_indicator_pipeline`](https://ahadi-analytics.github.io/sntmethods/reference/mbg_pipeline.md)
  : MBG Indicator Pipeline
- [`fit_mbg_indicator()`](https://ahadi-analytics.github.io/sntmethods/reference/fit_mbg_indicator.md)
  : Fit a Single MBG Indicator (Generic, Pluggable Admin Levels)
- [`build_final_dataset()`](https://ahadi-analytics.github.io/sntmethods/reference/build_final_dataset.md)
  : Build Final ADM2 Dataset
- [`aggregate_raster_to_admin()`](https://ahadi-analytics.github.io/sntmethods/reference/aggregate_raster_to_admin.md)
  : Aggregate Raster to Administrative Level
- [`save_mbg_cluster_data()`](https://ahadi-analytics.github.io/sntmethods/reference/save_mbg_cluster_data.md)
  : Save MBG Cluster Data
- [`save_mbg_rasters()`](https://ahadi-analytics.github.io/sntmethods/reference/save_mbg_rasters.md)
  : Save MBG Rasters

## MBG cluster-data preparation

Prepare cluster-level inputs for each indicator family used by the MBG
pipeline.

- [`calc_itn_mbg()`](https://ahadi-analytics.github.io/sntmethods/reference/calc_itn_mbg.md)
  : Prepare ITN Data for MBG Analysis
- [`calc_irs_mbg()`](https://ahadi-analytics.github.io/sntmethods/reference/calc_irs_mbg.md)
  : Prepare IRS Data for MBG Analysis
- [`calc_pfpr_mbg()`](https://ahadi-analytics.github.io/sntmethods/reference/calc_pfpr_mbg.md)
  : Prepare PfPR Data for MBG Analysis
- [`calc_act_mbg()`](https://ahadi-analytics.github.io/sntmethods/reference/calc_act_mbg.md)
  : Prepare ACT and Antimalarial Data for MBG Analysis
- [`calc_antimalarial_mbg()`](https://ahadi-analytics.github.io/sntmethods/reference/calc_antimalarial_mbg.md)
  : Prepare Antimalarial Treatment Data for MBG Analysis
- [`calc_fever_mbg()`](https://ahadi-analytics.github.io/sntmethods/reference/calc_fever_mbg.md)
  : Prepare Fever Prevalence Data for MBG Analysis
- [`calc_malaria_dx_mbg()`](https://ahadi-analytics.github.io/sntmethods/reference/calc_malaria_dx_mbg.md)
  : Prepare Malaria Diagnostic Testing Data for MBG Analysis
- [`calc_anc_mbg()`](https://ahadi-analytics.github.io/sntmethods/reference/calc_anc_mbg.md)
  : Prepare ANC Data for MBG Analysis
- [`calc_iptp_mbg()`](https://ahadi-analytics.github.io/sntmethods/reference/calc_iptp_mbg.md)
  : Prepare IPTp Data for MBG Analysis
- [`calc_epi_mbg()`](https://ahadi-analytics.github.io/sntmethods/reference/calc_epi_mbg.md)
  : Prepare EPI (Vaccination) Data for MBG Analysis
- [`calc_smc_mbg()`](https://ahadi-analytics.github.io/sntmethods/reference/calc_smc_mbg.md)
  : Prepare SMC Data for MBG Analysis
- [`calc_anemia_mbg()`](https://ahadi-analytics.github.io/sntmethods/reference/calc_anemia_mbg.md)
  : Prepare Anemia Data for MBG Analysis
- [`calc_u5mr_mbg()`](https://ahadi-analytics.github.io/sntmethods/reference/calc_u5mr_mbg.md)
  : Prepare U5MR Data for MBG Analysis
- [`calc_csb_mbg()`](https://ahadi-analytics.github.io/sntmethods/reference/calc_csb_mbg.md)
  : Prepare Care-Seeking Behavior Data for MBG Analysis
- [`calc_csb_by_wealth_mbg()`](https://ahadi-analytics.github.io/sntmethods/reference/calc_csb_by_wealth_mbg.md)
  : Prepare Care-Seeking Behavior Data by Wealth Quintile for MBG
  Analysis
- [`calc_wealth_mbg()`](https://ahadi-analytics.github.io/sntmethods/reference/calc_wealth_mbg.md)
  : Prepare Wealth Quintile Distribution Data for MBG Analysis
- [`prep_itn_mbg()`](https://ahadi-analytics.github.io/sntmethods/reference/prep_itn_mbg.md)
  : Prepare Single ITN Indicator for MBG
- [`prep_irs_mbg()`](https://ahadi-analytics.github.io/sntmethods/reference/prep_irs_mbg.md)
  : Prepare IRS Data for MBG (Alias)
- [`prep_pfpr_mbg()`](https://ahadi-analytics.github.io/sntmethods/reference/prep_pfpr_mbg.md)
  : Prepare Single PfPR Indicator for MBG
- [`prep_act_mbg()`](https://ahadi-analytics.github.io/sntmethods/reference/prep_act_mbg.md)
  : Prepare Single ACT Indicator for MBG
- [`prep_antimalarial_mbg()`](https://ahadi-analytics.github.io/sntmethods/reference/prep_antimalarial_mbg.md)
  : Prepare Single Antimalarial Indicator for MBG
- [`prep_fever_mbg()`](https://ahadi-analytics.github.io/sntmethods/reference/prep_fever_mbg.md)
  : Prepare Single Fever Indicator for MBG
- [`prep_malaria_dx_mbg()`](https://ahadi-analytics.github.io/sntmethods/reference/prep_malaria_dx_mbg.md)
  : Prepare Single Malaria Dx Indicator for MBG
- [`prep_anc_mbg()`](https://ahadi-analytics.github.io/sntmethods/reference/prep_anc_mbg.md)
  : Prepare Single ANC Indicator for MBG
- [`prep_iptp_mbg()`](https://ahadi-analytics.github.io/sntmethods/reference/prep_iptp_mbg.md)
  : Prepare Single IPTp Indicator for MBG
- [`prep_epi_mbg()`](https://ahadi-analytics.github.io/sntmethods/reference/prep_epi_mbg.md)
  : Prepare Single EPI Indicator for MBG
- [`prep_smc_mbg()`](https://ahadi-analytics.github.io/sntmethods/reference/prep_smc_mbg.md)
  : Prepare SMC Data for MBG (Alias)
- [`prep_anemia_mbg()`](https://ahadi-analytics.github.io/sntmethods/reference/prep_anemia_mbg.md)
  : Prepare Single Anemia Indicator for MBG
- [`prep_csb_mbg()`](https://ahadi-analytics.github.io/sntmethods/reference/prep_csb_mbg.md)
  : Prepare Single CSB Indicator for MBG
- [`prep_csb_by_wealth_mbg()`](https://ahadi-analytics.github.io/sntmethods/reference/prep_csb_by_wealth_mbg.md)
  : Prepare Single CSB Indicator by Wealth Quintile for MBG
- [`prep_wealth_mbg()`](https://ahadi-analytics.github.io/sntmethods/reference/prep_wealth_mbg.md)
  : Prepare Single Wealth Indicator for MBG

## MBG mapping and visualization

Render indicator surfaces and cluster diagnostics from MBG outputs.

- [`generate_all_maps()`](https://ahadi-analytics.github.io/sntmethods/reference/generate_all_maps.md)
  : Generate All Map Types for Indicator
- [`generate_indicator_map()`](https://ahadi-analytics.github.io/sntmethods/reference/generate_indicator_map.md)
  : Generate Indicator Map
- [`save_indicator_map()`](https://ahadi-analytics.github.io/sntmethods/reference/save_indicator_map.md)
  : Save Indicator Map
- [`plot_mbg_clusters()`](https://ahadi-analytics.github.io/sntmethods/reference/plot_mbg_clusters.md)
  : Plot MBG Cluster Map
- [`plot_mbg_clusters_all()`](https://ahadi-analytics.github.io/sntmethods/reference/plot_mbg_clusters_all.md)
  : Plot All MBG Cluster Maps

## Routine data - incidence

Test positivity and the N0-N5 malaria incidence cascade from routine
surveillance.

- [`calc_tpr()`](https://ahadi-analytics.github.io/sntmethods/reference/calc_tpr.md)
  : Calculate Test Positivity Rate from Routine Health Facility Data
- [`validate_tpr_proxies()`](https://ahadi-analytics.github.io/sntmethods/reference/validate_tpr_proxies.md)
  : Validate TPR Proxy Quality Using Leave-One-Out Cross-Validation
- [`calc_incidence()`](https://ahadi-analytics.github.io/sntmethods/reference/calc_incidence.md)
  : Calculate Malaria Incidence from Routine Health Facility Data
  (N0-N5)
- [`create_incidence()`](https://ahadi-analytics.github.io/sntmethods/reference/create_incidence.md)
  : Create an SNT Incidence Object from Tibble
- [`check_incidence()`](https://ahadi-analytics.github.io/sntmethods/reference/check_incidence.md)
  : Check Incidence Trends

## Routine data - reporting and quality

Reporting-rate metrics, facility activity classification and outlier
detection.

- [`calculate_reporting_metrics()`](https://ahadi-analytics.github.io/sntmethods/reference/sntutils_reexports.md)
  [`classify_facility_activity()`](https://ahadi-analytics.github.io/sntmethods/reference/sntutils_reexports.md)
  [`detect_outliers()`](https://ahadi-analytics.github.io/sntmethods/reference/sntutils_reexports.md)
  [`get_active_facilities()`](https://ahadi-analytics.github.io/sntmethods/reference/sntutils_reexports.md)
  : Re-exports from sntutils

## Trend analysis

Seasonal-trend decomposition with non-parametric trend testing.

- [`run_grouped_stl_trend()`](https://ahadi-analytics.github.io/sntmethods/reference/run_grouped_stl_trend.md)
  : Run STL decomposition and trend tests on grouped time series data

## Utilities

Small shared helpers used across SNT pipelines.

- [`dhs_data_path()`](https://ahadi-analytics.github.io/sntmethods/reference/dhs_data_path.md)
  : Resolve path inside AHADI OneDrive shared library
- [`cmc_to_date()`](https://ahadi-analytics.github.io/sntmethods/reference/cmc_to_date.md)
  : Convert DHS Century Month Code to Date
- [`normalize_zscore()`](https://ahadi-analytics.github.io/sntmethods/reference/normalize_zscore.md)
  : Normalize a numeric vector using z-score standardization
