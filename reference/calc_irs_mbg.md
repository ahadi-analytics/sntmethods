# Prepare IRS Data for MBG Analysis

Prepares cluster-level Indoor Residual Spraying (IRS) coverage data for
Model-Based Geostatistics (MBG) analysis. Calculates the proportion of
households sprayed in the last 12 months.

## Usage

``` r
calc_irs_mbg(
  dhs_hr,
  gps_data,
  survey_vars = list(cluster = "hv001", irs = "hv253"),
  gps_vars = list(cluster = "DHSCLUST", lat = "LATNUM", lon = "LONGNUM")
)
```

## Arguments

- dhs_hr:

  DHS Household Records dataset.

- gps_data:

  DHS GPS dataset with cluster coordinates.

- survey_vars:

  Named list mapping DHS variable names.

- gps_vars:

  Named list for GPS variable mapping.

## Value

A data.table with columns:

- cluster_id: Cluster identifier

- indicator: Number of households sprayed

- samplesize: Total number of households

- x: Longitude

- y: Latitude

## Details

Methodology:
<https://github.com/ahadi-analytics/sntmethods/blob/master/inst/methods/irs_dhs.yml>

IRS coverage is measured using variable hv253 (household sprayed in last
12 months). This is a household-level indicator.

## Examples

``` r
if (FALSE) { # \dontrun{
irs_mbg <- calc_irs_mbg(
  dhs_hr = hr_data,
  gps_data = gps_data
)
} # }
```
