# Prepare Single ITN Indicator for MBG

Simplified function to prepare a single ITN indicator for MBG.

## Usage

``` r
prep_itn_mbg(
  dhs_hr,
  dhs_pr,
  gps_data,
  indicator = "access_itn",
  survey_vars = list(cluster = "hv001", hhid = "hhid", hhsize = "hv013", age = "hv105",
    sex = "hv104", pregnant = "hml18", itn_use = "hml12", itn_prefix = "hml10_",
    itn_treated_prefix = "hml7_"),
  gps_vars = list(cluster = "DHSCLUST", lat = "LATNUM", lon = "LONGNUM"),
  seed = NULL
)
```

## Arguments

- dhs_hr:

  DHS Household Records dataset.

- dhs_pr:

  DHS Person Records dataset.

- gps_data:

  DHS GPS dataset with cluster coordinates.

- indicator:

  Single indicator name. Default: "access_itn".

- survey_vars:

  Named list mapping DHS variable names.

- gps_vars:

  Named list for GPS variable mapping.

- seed:

  Deprecated. Previously used for probabilistic access assignment.
  Access is now calculated deterministically following standard DHS
  methodology.

## Value

A data.table with columns: cluster_id, indicator, samplesize, x, y
