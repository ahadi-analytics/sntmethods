# Calculate Malaria Diagnostic Testing from DHS Data

Estimates the proportion of febrile children under 5 who had blood taken
for malaria testing using survey-weighted methods. This is step 2 of the
case management cascade.

## Usage

``` r
calc_malaria_dx_dhs_core(
  dhs_kr,
  survey_vars = list(cluster = "v021", weight = "v005", stratum = "v022", age = "hw1",
    fever = "h22", malaria_dx = "h47"),
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

## Value

Tibble with malaria diagnosis estimates by grouping level, including:

- Grouping variables (region, admin level, or national)

- `dhs_malaria_dx`: Proportion tested among febrile children

- `dhs_malaria_dx_low`, `dhs_malaria_dx_upp`: 95\\

- `dhs_n_febrile`: Number of febrile children (denominator)

- `dhs_n_tested`: Number who had blood taken

## Details

Methodology:
<https://github.com/ahadi-analytics/sntmethods/blob/master/inst/methods/malaria_dx_dhs.yml>

This function measures whether a diagnostic TEST was performed (h47),
distinct from PfPR which measures test RESULTS. The denominator is
febrile U5 children (h22 == 1).

## See also

[`calc_act_dhs()`](https://ahadi-analytics.github.io/sntmethods/reference/calc_act_dhs.md)
for ACT treatment (step 4),
[`calc_case_management_dhs()`](https://ahadi-analytics.github.io/sntmethods/reference/calc_case_management_dhs.md)
for the full cascade

## Examples

``` r
if (FALSE) { # \dontrun{
dx_results <- calc_malaria_dx_dhs_core(
  dhs_kr = kr_data,
  region_var = "v024"
)
} # }
```
