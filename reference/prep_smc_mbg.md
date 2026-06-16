# Prepare SMC Data for MBG (Alias)

Prepare SMC Data for MBG (Alias)

## Usage

``` r
prep_smc_mbg(
  dhs_kr,
  gps_data,
  survey_vars = list(cluster = "v001", age = "hw1", smc_primary = "hml43", smc_alt =
    "ml13g"),
  gps_vars = list(cluster = "DHSCLUST", lat = "LATNUM", lon = "LONGNUM")
)
```

## Arguments

- dhs_kr:

  DHS Children's Recode (KR) dataset.

- gps_data:

  DHS GPS dataset with cluster coordinates.

- survey_vars:

  Named list mapping DHS variable names.

- gps_vars:

  Named list for GPS variable mapping.

## Value

A data.table with columns: cluster_id, indicator, samplesize, x, y
