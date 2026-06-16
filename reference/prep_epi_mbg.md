# Prepare Single EPI Indicator for MBG

Prepare Single EPI Indicator for MBG

## Usage

``` r
prep_epi_mbg(
  dhs_kr,
  gps_data,
  vaccine = "measles1",
  age_min_months = 12,
  age_max_months = 23,
  survey_vars = list(cluster = "v001", age = "hw1", bcg = "h2", polio0 = "h0", dpt1 =
    "h3", dpt2 = "h4", dpt3 = "h5", polio1 = "h6", polio2 = "h7", polio3 = "h8", measles1
    = "h9", measles2 = "h9a", vita1 = "h33", vita2 = "h33a", malaria = "h68", penta1 =
    "h51", penta2 = "h52", penta3 = "h53", pneumo1 = "h54", pneumo2 = "h55", pneumo3 =
    "h56", rota1 = "h57", rota2 = "h58", rota3 = "h59", ipv = "h60", hepb0 = "h50",
    yellowfever = "h61", any = "h10"),
  gps_vars = list(cluster = "DHSCLUST", lat = "LATNUM", lon = "LONGNUM")
)
```

## Arguments

- dhs_kr:

  DHS Children's Recode (KR) dataset.

- gps_data:

  DHS GPS dataset with cluster coordinates.

- vaccine:

  Single vaccine name. Default: "measles1".

- age_min_months:

  Minimum age in months (default: 12).

- age_max_months:

  Maximum age in months (default: 23).

- survey_vars:

  Named list mapping DHS variable names.

- gps_vars:

  Named list for GPS variable mapping.

## Value

A data.table with columns: cluster_id, indicator, samplesize, x, y
