# Prepare Single Wealth Indicator for MBG

Convenience wrapper around
[`calc_wealth_mbg()`](https://ahadi-analytics.github.io/sntmethods/reference/calc_wealth_mbg.md)
to prepare a single wealth quintile distribution indicator.

## Usage

``` r
prep_wealth_mbg(
  dhs_hr,
  gps_data,
  indicator = "prop_poorest",
  survey_vars = list(cluster = "hv001", wealth_quintile = "hv270"),
  gps_vars = list(cluster = "DHSCLUST", lat = "LATNUM", lon = "LONGNUM")
)
```

## Arguments

- dhs_hr:

  DHS Household Records (HR) dataset.

- gps_data:

  DHS GPS dataset with cluster coordinates.

- indicator:

  Single indicator name. Default: "prop_poorest".

- survey_vars:

  Named list mapping DHS variable names:

  - cluster: Cluster ID (default: "hv001")

  - wealth_quintile: Wealth quintile variable (default: "hv270")

- gps_vars:

  Named list for GPS variable mapping.

## Value

Named list with single data.table containing columns: cluster_id,
indicator, samplesize, x, y

## Examples

``` r
if (FALSE) { # \dontrun{
# Poorest quintile distribution only
poorest <- prep_wealth_mbg(
  dhs_hr = hr_data,
  gps_data = gps_data,
  indicator = "prop_poorest"
)
} # }
```
