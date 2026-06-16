# Calculate Fever Prevalence from DHS Data

Computes fever prevalence among alive U5 children from DHS Children's
Recode (KR) data. Returns survey-weighted proportions with logit
confidence intervals in standardized long format.

## Usage

``` r
calc_fever_dhs(
  dhs_kr,
  survey_vars = list(cluster = "v021", weight = "v005", stratum = "v022", age = "hw1",
    fever = "h22", alive = "b5"),
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

- ci_method:

  Method for confidence intervals. Default: "logit".

## Value

Named list with:

- `adm0`:

  National-level estimates (always present)

- `adm1`:

  Admin-1 estimates (when `region_var` provided)

Each tibble contains standardized columns: survey_id, iso3, iso2,
survey_type, survey_year, adm0, adm1, type, geo_source, point, ci_l,
ci_u, numerator, denominator, indicator, indicator_code,
numerator_description, denominator_description, denominator_code.

## See also

[`fever_dictionary()`](https://ahadi-analytics.github.io/sntmethods/reference/fever_dictionary.md)
for indicator metadata,
[`calc_fever_dhs_core()`](https://ahadi-analytics.github.io/sntmethods/reference/calc_fever_dhs_core.md)
for legacy wide-format output

## Examples

``` r
if (FALSE) { # \dontrun{
fever <- calc_fever_dhs(dhs_kr = kr_data, region_var = "v024")
fever$adm0
fever$adm1
} # }
```
