# Calculate Wealth Quintile Distributions from DHS Data

Computes wealth quintile proportions (Q1-Q5) and Gini coefficient from
DHS Household Records data. Returns survey-weighted estimates in
standardized long format with `list(adm0, adm1)` structure.

## Usage

``` r
calc_wealth_dhs(
  dhs_hr,
  survey_vars = list(cluster = "hv001", weight = "hv005", stratum = "hv022", adm1 =
    "hv024", adm2 = NULL, wealth_quintile = "hv270", wealth_score = "hv271", hh_members =
    "hv012"),
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

- dhs_hr:

  DHS Household Records dataset in tidy format.

- survey_vars:

  Named list mapping DHS variable names.

- region_var:

  Optional column name for subnational grouping (e.g., "hv024").
  Auto-falls back to "hv024" if no spatial params.

- gps_data:

  Optional DHS GPS dataset with cluster coordinates.

- gps_vars:

  Named list for GPS variables (cluster, lat, lon).

- shapefile:

  Optional sf object with administrative boundaries.

- admin_level:

  Character vector specifying aggregation levels.

- join_nearest:

  Logical; if `TRUE`, assigns clusters outside all polygons to nearest
  administrative unit.

- ci_method:

  Method for confidence intervals. Default: "logit".

## Value

Named list of tibbles:

- `adm0`:

  National-level estimates (always present)

- `adm1`:

  Admin-1 estimates (when region_var or shapefile used)

Each tibble contains columns: survey_id, iso3, iso2, survey_type,
survey_year, adm0, adm1, type, geo_source, point, ci_l, ci_u, numerator,
denominator, indicator, indicator_code, numerator_description,
denominator_description, denominator_code.

## See also

[`wealth_dictionary()`](https://ahadi-analytics.github.io/sntmethods/reference/wealth_dictionary.md)
for indicator definitions,
[`calc_wealth_dhs_core()`](https://ahadi-analytics.github.io/sntmethods/reference/calc_wealth_dhs_core.md)
for the legacy wide-format output
