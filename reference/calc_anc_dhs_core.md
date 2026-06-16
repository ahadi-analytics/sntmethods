# Calculate ANC Coverage from DHS Data

Estimates Antenatal Care (ANC) attendance rates using survey-weighted
methods from DHS Individual Recode data.

## Usage

``` r
calc_anc_dhs_core(
  dhs_ir,
  survey_vars = list(cluster = "v021", weight = "v005", stratum = "v022", interview_date
    = "v008", birth_date = "b3_01", anc_visits = "m14_1"),
  birth_window_months = 24,
  region_var = NULL,
  gps_data = NULL,
  gps_vars = list(cluster = "DHSCLUST", lat = "LATNUM", lon = "LONGNUM"),
  shapefile = NULL,
  admin_level = NULL,
  join_nearest = TRUE
)
```

## Arguments

- dhs_ir:

  DHS Individual Recode (IR) dataset.

- survey_vars:

  Named list mapping DHS variable names. Required keys:

  - `cluster`: Cluster/PSU ID (default: "v021")

  - `weight`: Survey weight (default: "v005")

  - `stratum`: Stratum variable (default: "v022")

  - `interview_date`: Interview date CMC (default: "v008")

  - `birth_date`: Birth date CMC (default: "b3_01")

  - `anc_visits`: Number of ANC visits (default: "m14_1")

- birth_window_months:

  Maximum months since last birth. Default: 24.

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

  Logical; if TRUE, assigns unmatched clusters.

## Value

Tibble with ANC estimates including: dhs_anc_1plus, dhs_anc_2plus,
dhs_anc_3plus, dhs_anc_4plus, dhs_anc_8plus (each with \_low, \_upp),
dhs_n_recent_births.

## See also

[`calc_anc_mbg()`](https://ahadi-analytics.github.io/sntmethods/reference/calc_anc_mbg.md)
for cluster-level MBG inputs
