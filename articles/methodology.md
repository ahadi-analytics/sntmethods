# Methodology & conventions

This article is the reference layer: it explains how indicators are
specified, where to look up variable mappings, the function naming
scheme, and the two kinds of dictionaries the package provides.

## Function naming conventions

| Pattern | Purpose | Example |
|----|----|----|
| `calc_*_dhs()` | Survey-weighted DHS estimates (long format) | [`calc_itn_dhs()`](https://ahadi-analytics.github.io/sntmethods/reference/calc_itn_dhs.md) |
| `calc_*_mbg()` | Run MBG model for an indicator family | [`calc_itn_mbg()`](https://ahadi-analytics.github.io/sntmethods/reference/calc_itn_mbg.md) |
| `prep_*_mbg()` | Prepare cluster-level data for MBG | [`prep_itn_mbg()`](https://ahadi-analytics.github.io/sntmethods/reference/prep_itn_mbg.md) |
| [`fit_mbg_indicator()`](https://ahadi-analytics.github.io/sntmethods/reference/fit_mbg_indicator.md) | All-in-one MBG fit + admin aggregation | [`fit_mbg_indicator()`](https://ahadi-analytics.github.io/sntmethods/reference/fit_mbg_indicator.md) |
| [`run_mbg_pipeline()`](https://ahadi-analytics.github.io/sntmethods/reference/run_mbg_pipeline.md) | Full MBG pipeline (all indicators) | [`run_mbg_pipeline()`](https://ahadi-analytics.github.io/sntmethods/reference/run_mbg_pipeline.md) |
| [`dhs_dictionary()`](https://ahadi-analytics.github.io/sntmethods/reference/dhs_dictionary.md) | Unified dictionary across all DHS domains | [`dhs_dictionary()`](https://ahadi-analytics.github.io/sntmethods/reference/dhs_dictionary.md) |
| [`calc_incidence()`](https://ahadi-analytics.github.io/sntmethods/reference/calc_incidence.md) | Routine-data incidence (N0-N5 cascade) | [`calc_incidence()`](https://ahadi-analytics.github.io/sntmethods/reference/calc_incidence.md) |
| [`calc_tpr()`](https://ahadi-analytics.github.io/sntmethods/reference/calc_tpr.md) | Test positivity rate with fallbacks | [`calc_tpr()`](https://ahadi-analytics.github.io/sntmethods/reference/calc_tpr.md) |

## Two kinds of dictionaries

**Indicator dictionaries** describe the *outputs* - each indicator’s
numerator, denominator, codes, and metadata. Use
[`dhs_dictionary()`](https://ahadi-analytics.github.io/sntmethods/reference/dhs_dictionary.md)
for the unified view, or per-domain helpers
([`itn_dictionary()`](https://ahadi-analytics.github.io/sntmethods/reference/itn_dictionary.md),
[`act_dictionary()`](https://ahadi-analytics.github.io/sntmethods/reference/act_dictionary.md),
[`pfpr_dictionary()`](https://ahadi-analytics.github.io/sntmethods/reference/pfpr_dictionary.md),
…):

``` r

library(sntmethods)
dhs_dictionary()
```

**Raw variable dictionaries** describe the *inputs* - every variable
present in a specific DHS recode, with its label. Build these with
[`make_dhs_raw_dictionary()`](https://ahadi-analytics.github.io/sntmethods/reference/make_dhs_raw_dictionary.md)
(or spot-check with
[`list_dhs_var_labels()`](https://ahadi-analytics.github.io/sntmethods/reference/list_dhs_var_labels.md))
**before** computing indicators, to confirm a survey actually carries
the variables an indicator needs:

``` r

kr <- dhs_read(path = path_dhs_parquet, file_type = "KR",
               country_code = "TG", survey_year = 2017)
make_dhs_raw_dictionary(kr)     # full variable list for this recode
```

See [DHS survey analysis → inspect the
variables](https://ahadi-analytics.github.io/sntmethods/articles/dhs-survey-analysis.html#step-0b-inspect-the-variables-before-building-indicators)
for the recommended “Step 0” workflow.

## Methodology specifications (`inst/methods/`)

Detailed methodology for each indicator lives in machine-readable YAML
at
[`inst/methods/`](https://github.com/ahadi-analytics/sntmethods/tree/master/inst/methods).
Each file documents DHS variable mappings, inclusion criteria,
calculation logic, and references to WHO / World Malaria Report
standards.

| File                    | Domain                           |
|-------------------------|----------------------------------|
| `pfpr_dhs.yml`          | Parasite prevalence              |
| `itn_dhs.yml`           | ITN ownership/access/use         |
| `irs_dhs.yml`           | Indoor residual spraying         |
| `fever_dhs.yml`         | Fever prevalence                 |
| `csb_dhs.yml`           | Care-seeking behaviour           |
| `malaria_dx_dhs.yml`    | Malaria diagnostic testing       |
| `antimalarial_dhs.yml`  | Antimalarial treatment           |
| `act_dhs.yml`           | ACT treatment                    |
| `anc_dhs.yml`           | Antenatal care                   |
| `iptp_dhs.yml`          | IPTp dosing                      |
| `epi_dhs.yml`           | EPI vaccination                  |
| `u5mr_dhs.yml`          | Under-5 mortality                |
| `anemia_dhs.yml`        | Anemia prevalence                |
| `smc_dhs.yml`           | SMC coverage                     |
| `wealth_dhs.yml`        | Wealth index                     |
| `incidence.yml`         | Incidence cascade (N0-N5)        |
| `tpr.yml`               | Test positivity rate             |
| `reporting_rate.yml`    | Reporting-rate calculations      |
| `outlier_detection.yml` | Outlier detection methods        |
| `active_status.yml`     | Facility activity classification |

## Methodological notes

- **Direct survey estimates stay at adm0/adm1.** DHS/MIS weights are
  representative at region level; sub-region estimates come from the
  [MBG
  pipeline](https://ahadi-analytics.github.io/sntmethods/articles/spatial-modeling.md),
  not direct aggregation.
- **ACT** is a drug *class* across multiple variables - only
  artemisinin-based *combination* therapies count; monotherapies are
  excluded.
- **U5MR** is reported per 1,000 live births; intervention-coverage
  indicators are percentages (0-100).
- **ITN `use_if_access`** (use among those with access) is computed by
  default with standard age groups (`u5`, `5_14`, `ov15`).

## Country configuration

Country-specific survey settings (variable overrides, eligibility notes)
live in
[`inst/countries/`](https://github.com/ahadi-analytics/sntmethods/tree/master/inst/countries).

## See also

- [Get
  started](https://ahadi-analytics.github.io/sntmethods/articles/getting-started.md)
  · [DHS survey
  analysis](https://ahadi-analytics.github.io/sntmethods/articles/dhs-survey-analysis.md)
  · [Spatial
  modeling](https://ahadi-analytics.github.io/sntmethods/articles/spatial-modeling.md)
  · [Routine
  data](https://ahadi-analytics.github.io/sntmethods/articles/routine-incidence.md)
  · [Trend
  analysis](https://ahadi-analytics.github.io/sntmethods/articles/trend-analysis.md)
- [Reference](https://ahadi-analytics.github.io/sntmethods/reference/index.html)
  for every exported function. \`\`\`
