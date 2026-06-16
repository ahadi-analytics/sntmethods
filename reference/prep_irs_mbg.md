# Prepare IRS Data for MBG (Alias)

Alias for calc_irs_mbg for consistent naming.

## Usage

``` r
prep_irs_mbg(
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

A data.table with columns: cluster_id, indicator, samplesize, x, y
