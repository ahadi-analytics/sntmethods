# Prepare Single ANC Indicator for MBG

Simplified function to prepare a single ANC indicator.

## Usage

``` r
prep_anc_mbg(
  dhs_ir,
  gps_data,
  threshold = 4,
  birth_window_months = 36,
  survey_vars = list(cluster = "v001", interview_date = "v008", birth_date = "b3_01",
    anc_visits = "m14_1"),
  gps_vars = list(cluster = "DHSCLUST", lat = "LATNUM", lon = "LONGNUM")
)
```

## Arguments

- dhs_ir:

  DHS Individual Recode dataset.

- gps_data:

  DHS GPS dataset with cluster coordinates.

- threshold:

  Minimum number of ANC visits (1, 4, or 8). Default: 4.

- birth_window_months:

  Number of months to look back for births. Default: 36 (3 years). Max
  60 (5 years).

- survey_vars:

  Named list mapping DHS variable names.

- gps_vars:

  Named list for GPS variable mapping.

## Value

A data.table with columns: cluster_id, indicator, samplesize, x, y
