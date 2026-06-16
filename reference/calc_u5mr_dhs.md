# Calculate U5MR from DHS Data (Standardized Long Format)

Estimates under-5 mortality rate (U5MR) from DHS Children's Recode data
using the DHS.rates package. Returns results in standardized long format
with `list(adm0, adm1)` structure.

## Usage

``` r
calc_u5mr_dhs(
  dhs_kr,
  survey_vars = list(cluster = "v021", weight = "v005", stratum = "v022", interview_date
    = "v008", birth_date = "b3", age_at_death = "b7"),
  period_years = 5,
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

  DHS Children's Recode (KR) dataset in tidy format.

- survey_vars:

  Named list mapping DHS variable names. See
  [`calc_u5mr_dhs_core()`](https://ahadi-analytics.github.io/sntmethods/reference/calc_u5mr_dhs_core.md).

- period_years:

  Years before survey to calculate rates (default: 5).

- region_var:

  Optional column name for subnational grouping (e.g., "v024").
  Auto-falls back to "v024" if no spatial params.

- gps_data:

  Optional DHS GPS dataset with cluster coordinates.

- gps_vars:

  Named list for GPS variables (cluster, lat, lon).

- shapefile:

  Optional sf object with administrative boundaries.

- admin_level:

  Character vector specifying aggregation levels.

- join_nearest:

  Logical; if TRUE, assigns clusters outside all polygons to nearest
  administrative unit.

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

## Details

U5MR is computed via
[`DHS.rates::chmort()`](https://rdrr.io/pkg/DHS.rates/man/chmort.html)
(synthetic cohort life table method), NOT via `svyciprop()`, because it
is a rate per 1000 live births rather than a proportion.

## See also

[`u5mr_dictionary()`](https://ahadi-analytics.github.io/sntmethods/reference/u5mr_dictionary.md)
for indicator definitions,
[`calc_u5mr_dhs_core()`](https://ahadi-analytics.github.io/sntmethods/reference/calc_u5mr_dhs_core.md)
for the legacy wide-format output
