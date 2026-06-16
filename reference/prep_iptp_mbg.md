# Prepare Single IPTp Indicator for MBG

Prepare Single IPTp Indicator for MBG

## Usage

``` r
prep_iptp_mbg(
  dhs_ir,
  gps_data,
  doses = 3,
  birth_window_months = 36,
  survey_vars = list(cluster = "v001", interview_date = "v008", birth_date = "b3_01",
    sp_doses = "ml1_1", sp_taken = "m49a_1"),
  gps_vars = list(cluster = "DHSCLUST", lat = "LATNUM", lon = "LONGNUM")
)
```

## Arguments

- dhs_ir:

  DHS Individual Recode (IR) or Children's Recode (KR) dataset.

- gps_data:

  DHS GPS dataset with cluster coordinates.

- doses:

  Minimum doses for cumulative indicator (1, 2, or 3).

- birth_window_months:

  Months to look back for births. Default: 36.

- survey_vars:

  Named list mapping DHS variable names.

- gps_vars:

  Named list for GPS variable mapping.

## Value

A data.table with columns: cluster_id, indicator, samplesize, x, y
