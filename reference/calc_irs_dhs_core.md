# Calculate IRS Coverage from DHS Data

Estimates Indoor Residual Spraying (IRS) coverage at the household level
using survey-weighted methods from DHS Household Records data.

## Usage

``` r
calc_irs_dhs_core(
  dhs_hr,
  survey_vars = list(cluster = "hv021", weight = "hv005", stratum = "hv022", irs =
    "hv253"),
  region_var = NULL,
  gps_data = NULL,
  gps_vars = list(cluster = "DHSCLUST", lat = "LATNUM", lon = "LONGNUM"),
  shapefile = NULL,
  admin_level = NULL,
  join_nearest = TRUE
)
```

## Arguments

- dhs_hr:

  DHS Household Records (HR) dataset.

- survey_vars:

  Named list mapping DHS variable names. Required keys:

  - `cluster`: Cluster/PSU ID (default: "hv021")

  - `weight`: Survey weight (default: "hv005")

  - `stratum`: Stratum variable (default: "hv022")

  - `irs`: IRS variable (default: "hv253")

- region_var:

  Optional column name to use as grouping variable.

- gps_data:

  Optional DHS GPS dataset.

- gps_vars:

  Named list for GPS variables.

- shapefile:

  Optional sf object with administrative boundaries.

- admin_level:

  Character vector of admin columns from shapefile.

- join_nearest:

  Logical; if TRUE, assigns clusters outside polygons to nearest admin
  unit.

## Value

Tibble with IRS estimates including: dhs_irs, dhs_irs_low, dhs_irs_upp,
dhs_n_households_irs, dhs_n_sprayed.

## See also

[`calc_irs_mbg()`](https://ahadi-analytics.github.io/sntmethods/reference/calc_irs_mbg.md)
for cluster-level MBG inputs
