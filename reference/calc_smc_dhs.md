# Calculate SMC Coverage from DHS Data

Computes SMC coverage among eligible children from DHS Children's Recode
(KR) data. Returns survey-weighted proportions with logit confidence
intervals in standardized long format.

## Usage

``` r
calc_smc_dhs(
  dhs_kr,
  survey_vars = list(cluster = "v021", weight = "v005", stratum = "v022", age = "hw1",
    smc_primary = "hml43", smc_alt = "ml13g"),
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

[`smc_dictionary()`](https://ahadi-analytics.github.io/sntmethods/reference/smc_dictionary.md)
for indicator metadata,
[`calc_smc_dhs_core()`](https://ahadi-analytics.github.io/sntmethods/reference/calc_smc_dhs_core.md)
for legacy wide-format output

## Examples

``` r
if (FALSE) { # \dontrun{
smc <- calc_smc_dhs(dhs_kr = kr_data, region_var = "v024")
smc$adm0
smc$adm1
} # }
```
