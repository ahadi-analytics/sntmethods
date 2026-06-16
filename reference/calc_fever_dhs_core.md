# Calculate Fever Prevalence from DHS Data

Estimates fever prevalence among children under 5 using survey-weighted
methods. This is step 0 of the case management cascade.

## Usage

``` r
calc_fever_dhs_core(
  dhs_kr,
  survey_vars = list(cluster = "v021", weight = "v005", stratum = "v022", age = "hw1",
    fever = "h22", alive = "b5"),
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

  - `alive`: Child survival status (default: "b5")

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

Tibble with fever estimates by grouping level, including:

- Grouping variables (region, admin level, or national)

- `dhs_fever`: Proportion with fever among U5 children

- `dhs_fever_low`, `dhs_fever_upp`: 95\\

- `dhs_n_children`: Number of U5 children (denominator)

- `dhs_n_fever`: Number of febrile children

## Details

Methodology:
<https://github.com/ahadi-analytics/sntmethods/blob/master/inst/methods/fever_dhs.yml>

This function calculates fever prevalence among alive U5 children. The
denominator is ALL alive children under 5, not just febrile children.
This differs from CSB/ACT which use febrile children as denominator.

Fever prevalence is the entry point (step 0) of the case management
cascade: Fever -\> Sought care -\> Tested -\> Any antimalarial -\> ACT.

## See also

[`calc_csb_dhs()`](https://ahadi-analytics.github.io/sntmethods/reference/calc_csb_dhs.md)
for care-seeking behavior (step 1),
[`calc_case_management_dhs()`](https://ahadi-analytics.github.io/sntmethods/reference/calc_case_management_dhs.md)
for the full cascade

## Examples

``` r
if (FALSE) { # \dontrun{
fever_results <- calc_fever_dhs_core(
  dhs_kr = kr_data,
  region_var = "v024"
)
} # }
```
