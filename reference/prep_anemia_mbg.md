# Prepare Single Anemia Indicator for MBG

Prepare Single Anemia Indicator for MBG

## Usage

``` r
prep_anemia_mbg(
  dhs_pr,
  gps_data,
  indicator = "any",
  age_min = 6,
  age_max = 59,
  survey_vars = list(cluster = "hv001", age = "hc1", present = "hv103", mother = "hv042",
    hemoglobin = "hc56"),
  gps_vars = list(cluster = "DHSCLUST", lat = "LATNUM", lon = "LONGNUM")
)
```

## Arguments

- dhs_pr:

  DHS Person Records dataset.

- gps_data:

  DHS GPS dataset with cluster coordinates.

- indicator:

  Single indicator name. Default: "any".

- age_min:

  Minimum age in months (default: 6).

- age_max:

  Maximum age in months (default: 59).

- survey_vars:

  Named list mapping DHS variable names.

- gps_vars:

  Named list for GPS variable mapping.

## Value

A data.table with columns: cluster_id, indicator, samplesize, x, y
