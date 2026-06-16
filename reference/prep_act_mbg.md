# Prepare Single ACT Indicator for MBG

Convenience wrapper around
[`calc_act_mbg()`](https://ahadi-analytics.github.io/sntmethods/reference/calc_act_mbg.md)
to prepare a single ACT indicator for MBG analysis.

## Usage

``` r
prep_act_mbg(
  dhs_kr,
  gps_data,
  indicator = "act",
  survey_vars = list(cluster = "v001", age = "hw1", fever = "h22", alive = "b5", act =
    "ml13e", test = "ml13a"),
  gps_vars = list(cluster = "DHSCLUST", lat = "LATNUM", lon = "LONGNUM")
)
```

## Arguments

- dhs_kr:

  DHS Children's Recode (KR) dataset.

- gps_data:

  DHS GPS dataset with cluster coordinates.

- indicator:

  Single indicator name. Default: "act".

- survey_vars:

  Named list mapping DHS variable names:

  - `cluster`: Cluster ID (default: "v001")

  - `age`: Child's age in months (default: "hw1")

  - `fever`: Fever in last 2 weeks (default: "h22")

  - `alive`: Child is alive (default: "b5")

  - `act`: ACT variable (default: "ml13e")

  - `test`: Diagnostic test variable (default: "ml13a")

- gps_vars:

  Named list for GPS variable mapping.

## Value

A data.table with columns: cluster_id, indicator, samplesize, x, y
