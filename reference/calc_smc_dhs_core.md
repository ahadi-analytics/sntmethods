# Calculate SMC Coverage from DHS Data

Estimates Seasonal Malaria Chemoprevention (SMC) coverage among children
under 5 using survey-weighted methods.

## Usage

``` r
calc_smc_dhs_core(
  dhs_kr,
  survey_vars = list(cluster = "v021", weight = "v005", stratum = "v022", age = "hw1",
    smc_primary = "hml43", smc_alt = "ml13g"),
  region_var = NULL,
  gps_data = NULL,
  gps_vars = list(cluster = "DHSCLUST", lat = "LATNUM", lon = "LONGNUM"),
  shapefile = NULL,
  admin_level = NULL,
  join_nearest = TRUE
)
```

## Arguments

- dhs_kr:

  DHS Children's Recode (KR) dataset.

- survey_vars:

  Named list mapping DHS variable names.

- region_var:

  Optional column name to use as grouping variable.

- gps_data:

  Optional DHS GPS dataset.

- gps_vars:

  Named list for GPS variables.

- shapefile:

  Optional sf object with administrative boundaries.

- admin_level:

  Character vector of admin columns.

- join_nearest:

  Logical.

## Value

Tibble with SMC estimates including: dhs_smc, dhs_smc_low, dhs_smc_upp,
dhs_n_smc_eligible, dhs_n_smc_received.

## See also

[`calc_smc_mbg()`](https://ahadi-analytics.github.io/sntmethods/reference/calc_smc_mbg.md)
for cluster-level MBG inputs
