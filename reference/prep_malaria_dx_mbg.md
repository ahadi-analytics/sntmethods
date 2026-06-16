# Prepare Single Malaria Dx Indicator for MBG

Convenience wrapper around
[`calc_malaria_dx_mbg()`](https://ahadi-analytics.github.io/sntmethods/reference/calc_malaria_dx_mbg.md)
to prepare a single malaria diagnostic testing indicator for MBG
analysis.

## Usage

``` r
prep_malaria_dx_mbg(
  dhs_kr,
  gps_data,
  indicator = "malaria_dx",
  survey_vars = list(cluster = "v001", age = "hw1", fever = "h22", malaria_dx = "h47"),
  gps_vars = list(cluster = "DHSCLUST", lat = "LATNUM", lon = "LONGNUM")
)
```

## Arguments

- dhs_kr:

  DHS Children's Recode (KR) dataset.

- gps_data:

  DHS GPS dataset with cluster coordinates.

- indicator:

  Single indicator name. Default: "malaria_dx".

- survey_vars:

  Named list mapping DHS variable names:

  - `cluster`: Cluster ID (default: "v001")

  - `age`: Child's age in months (default: "hw1")

  - `fever`: Fever in last 2 weeks (default: "h22")

  - `malaria_dx`: Blood taken for malaria test (default: "h47")

- gps_vars:

  Named list for GPS variable mapping.

## Value

A data.table with columns: cluster_id, indicator, samplesize, x, y
