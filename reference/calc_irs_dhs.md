# Calculate IRS Coverage from DHS Data

Computes IRS coverage among households from DHS Household Records (HR)
data. Returns survey-weighted proportions with logit confidence
intervals in standardized long format.

## Usage

``` r
calc_irs_dhs(
  dhs_hr,
  survey_vars = list(cluster = "hv021", weight = "hv005", stratum = "hv022", irs =
    "hv253"),
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

[`irs_dictionary()`](https://ahadi-analytics.github.io/sntmethods/reference/irs_dictionary.md)
for indicator metadata,
[`calc_irs_dhs_core()`](https://ahadi-analytics.github.io/sntmethods/reference/calc_irs_dhs_core.md)
for legacy wide-format output

## Examples

``` r
if (FALSE) { # \dontrun{
irs <- calc_irs_dhs(dhs_hr = hr_data, region_var = "hv024")
irs$adm0
irs$adm1
} # }
```
