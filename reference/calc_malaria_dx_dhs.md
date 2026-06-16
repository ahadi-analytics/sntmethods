# Calculate Malaria Diagnostic Testing from DHS Data (Standardized)

Estimates the proportion of febrile children under 5 who had blood taken
for malaria testing. Returns standardized long-format output as
`list(adm0, adm1)`.

## Usage

``` r
calc_malaria_dx_dhs(
  dhs_kr,
  survey_vars = list(cluster = "v021", weight = "v005", stratum = "v022", age = "hw1",
    fever = "h22", malaria_dx = "h47"),
  region_var = NULL,
  gps_data = NULL,
  gps_vars = list(cluster = "DHSCLUST", lat = "LATNUM", lon = "LONGNUM"),
  shapefile = NULL,
  admin_level = NULL,
  join_nearest = TRUE,
  ci_method = "logit"
)
```

## Arguments

- dhs_kr:

  DHS children's recode (KR) dataset (data.frame or tibble).

- survey_vars:

  Named list mapping DHS variable names. Required keys:

  - `cluster`: Cluster/PSU ID (default: "v021")

  - `weight`: Survey weight (default: "v005")

  - `stratum`: Stratum variable (default: "v022")

  - `age`: Child's age in months (default: "hw1")

  - `fever`: Had fever in last 2 weeks (default: "h22")

  - `malaria_dx`: Blood taken for malaria test (default: "h47")

- region_var:

  Optional column name in `dhs_kr` to use as grouping variable (e.g.,
  "v024" for region).

- gps_data:

  Optional DHS GPS dataset with cluster coordinates.

- gps_vars:

  Named list for GPS variables (cluster, lat, lon).

- shapefile:

  Optional sf object with administrative boundaries.

- admin_level:

  Character vector of admin columns from shapefile (e.g., c("adm1",
  "adm2")).

- join_nearest:

  Logical; if TRUE, assigns clusters outside polygons to nearest admin
  unit. Default: TRUE.

- ci_method:

  CI method for svyciprop. Default: "logit".

## Value

Named list with `adm0` (national) and optionally `adm1` (regional)
tibbles in standardized long format.

## See also

[`malaria_dx_dictionary()`](https://ahadi-analytics.github.io/sntmethods/reference/malaria_dx_dictionary.md)
for indicator definitions,
[`calc_malaria_dx_dhs_core()`](https://ahadi-analytics.github.io/sntmethods/reference/calc_malaria_dx_dhs_core.md)
for backward-compatible wide output
