# Prepare Wealth Quintile Distribution Data for MBG Analysis

Prepares cluster-level wealth quintile distribution data for Model-Based
Geostatistics (MBG) analysis. Calculates proportions of households in
each wealth quintile, aggregated to cluster level.

## Usage

``` r
calc_wealth_mbg(
  dhs_hr,
  gps_data,
  indicators = c("prop_poorest", "prop_richest"),
  survey_vars = list(cluster = "hv001", wealth_quintile = "hv270"),
  gps_vars = list(cluster = "DHSCLUST", lat = "LATNUM", lon = "LONGNUM")
)
```

## Arguments

- dhs_hr:

  DHS Household Records (HR) dataset.

- gps_data:

  DHS GPS dataset with cluster coordinates.

- indicators:

  Character vector of indicators to calculate:

  - "prop_poorest" or "prop_q1": Proportion in poorest quintile (Q1)

  - "prop_poorer" or "prop_q2": Proportion in second quintile (Q2)

  - "prop_middle" or "prop_q3": Proportion in middle quintile (Q3)

  - "prop_richer" or "prop_q4": Proportion in fourth quintile (Q4)

  - "prop_richest" or "prop_q5": Proportion in richest quintile (Q5)

  Default: c("prop_poorest", "prop_richest") for equity analysis.

- survey_vars:

  Named list mapping DHS variable names:

  - cluster: Cluster ID (default: "hv001")

  - wealth_quintile: Wealth quintile variable (default: "hv270")

- gps_vars:

  Named list for GPS variable mapping.

## Value

A named list of data.tables (one per indicator), each with columns:

- cluster_id: Cluster identifier

- indicator: Numerator count (households in quintile)

- samplesize: Denominator count (all households)

- x: Longitude

- y: Latitude

## Details

This function prepares wealth distribution indicators for spatial
modeling. Unlike the survey-weighted
[`calc_wealth_dhs()`](https://ahadi-analytics.github.io/sntmethods/reference/calc_wealth_dhs.md),
this uses simple cluster-level counts without survey weights - MBG
handles spatial smoothing internally.

**Pipeline Integration:** This function IS called by
[`run_mbg_pipeline()`](https://ahadi-analytics.github.io/sntmethods/reference/run_mbg_pipeline.md)
when you specify `indicators = "wealth"` or individual codes like
`"prop_poorest"`.

Methodology: Uses DHS wealth quintile variable (hv270 in HR recode)
which classifies households into 5 quintiles based on wealth index
factor scores.

## Output Structure

For `indicators = c("prop_poorest", "prop_richest")`:


    list(
      prop_poorest = data.table(cluster_id, indicator, samplesize, x, y),
      prop_richest = data.table(cluster_id, indicator, samplesize, x, y)
    )

## See also

- [`calc_wealth_dhs()`](https://ahadi-analytics.github.io/sntmethods/reference/calc_wealth_dhs.md)
  for survey-weighted wealth estimates with CIs

- [`run_mbg_pipeline()`](https://ahadi-analytics.github.io/sntmethods/reference/run_mbg_pipeline.md)
  for automated pipeline processing

## Examples

``` r
if (FALSE) { # \dontrun{
# Poorest quintile distribution for equity mapping
wealth_poorest <- calc_wealth_mbg(
  dhs_hr = hr_data,
  gps_data = gps_data,
  indicators = "prop_poorest"
)

# Compare poorest vs richest for inequality analysis
wealth_inequality <- calc_wealth_mbg(
  dhs_hr = hr_data,
  gps_data = gps_data,
  indicators = c("prop_poorest", "prop_richest")
)

# Via pipeline
results <- run_mbg_pipeline(
  country_iso3 = "gin",
  indicators = "wealth",
  ...
)
} # }
```
