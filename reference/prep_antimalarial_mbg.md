# Prepare Single Antimalarial Indicator for MBG

Convenience wrapper around
[`calc_antimalarial_mbg()`](https://ahadi-analytics.github.io/sntmethods/reference/calc_antimalarial_mbg.md)
to prepare antimalarial treatment data for MBG analysis.

## Usage

``` r
prep_antimalarial_mbg(
  dhs_kr,
  gps_data,
  indicator = "antimalarial",
  survey_vars = list(cluster = "v001", age = "hw1", fever = "h22"),
  gps_vars = list(cluster = "DHSCLUST", lat = "LATNUM", lon = "LONGNUM")
)
```

## Arguments

- dhs_kr:

  DHS Children's Recode (KR) dataset.

- gps_data:

  DHS GPS dataset with cluster coordinates.

- indicator:

  Single indicator name. Default: "antimalarial".

- survey_vars:

  Named list mapping DHS variable names:

  - `cluster`: Cluster ID (default: "v001")

  - `age`: Child's age in months (default: "hw1")

  - `fever`: Fever in last 2 weeks (default: "h22")

- gps_vars:

  Named list for GPS variable mapping.

## Value

A data.table with columns: cluster_id, indicator, samplesize, x, y
