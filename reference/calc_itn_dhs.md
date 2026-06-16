# Calculate ITN Indicators from DHS Data

Computes the full set of ITN indicators from DHS Household Records (HR)
and Person Records (PR) data. Returns survey-weighted proportions with
logit confidence intervals in long format.

## Usage

``` r
calc_itn_dhs(
  dhs_hr,
  dhs_pr,
  survey_vars = list(cluster = "hv001", weight = "hv005", stratum = "hv022", hhid =
    "hhid", hhsize = "hv013", age = "hv105", sex = "hv104", pregnant = "hml18", itn_use =
    "hml12", itn_prefix = "hml10_", itn_treated_prefix = "hml7_", wealth = "hv270",
    residence = "hv025"),
  region_var = NULL,
  gps_data = NULL,
  gps_vars = list(cluster = "DHSCLUST", lat = "LATNUM", lon = "LONGNUM"),
  shapefile = NULL,
  admin_level = NULL,
  join_nearest = TRUE,
  indicators = NULL,
  age_breaks = NULL,
  age_labels = NULL,
  ci_method = "logit"
)
```

## Arguments

- dhs_hr:

  DHS Household Records dataset (HR).

- dhs_pr:

  DHS Person Records dataset (PR).

- survey_vars:

  Named list mapping DHS variable names. Required keys:

  - `cluster`: Cluster ID (default: "hv001")

  - `weight`: Survey weight (default: "hv005")

  - `stratum`: Stratum variable (default: "hv022")

  - `hhid`: Household ID (default: "hhid")

  - `hhsize`: Household size (default: "hv013")

  - `age`: Age in years (default: "hv105")

  - `sex`: Sex (default: "hv104")

  - `pregnant`: Pregnancy status (default: "hml18")

  - `itn_use`: Slept under ITN last night (default: "hml12")

  - `itn_prefix`: Prefix for ITN net variables (default: "hml10\_")

  - `wealth`: Wealth index quintile (default: "hv270")

  - `residence`: Urban/rural (default: "hv025")

- region_var:

  Optional column name for subnational grouping (e.g., "hv024").
  Auto-falls back to "hv024" if no spatial params.

- gps_data:

  Optional DHS GPS dataset with cluster coordinates.

- gps_vars:

  Named list for GE variables: cluster, lat, lon.

- shapefile:

  Optional sf object with administrative boundaries.

- admin_level:

  Character vector of admin columns from shapefile.

- join_nearest:

  Logical; assign unmatched clusters to nearest polygon.

- indicators:

  Character vector of indicator names to compute. If NULL (default),
  computes all indicators from
  [`itn_dictionary()`](https://ahadi-analytics.github.io/sntmethods/reference/itn_dictionary.md).

- age_breaks:

  Optional numeric vector of age group boundaries (e.g., c(0, 5, 15,
  Inf)). Creates additional USE_ITN_AGE\_\* indicators.

- age_labels:

  Optional character vector of labels for age groups (e.g., c("U5",
  "5_14", "OV15")). Must have length = length(age_breaks) - 1.

- ci_method:

  Method for confidence intervals. Default: "logit".

## Value

Named list of tibbles, one per admin level:

- `adm0`:

  National-level estimates (always present)

- `adm1`:

  Admin-1 estimates (when `region_var` or shapefile used)

- `adm2`:

  Admin-2 estimates (when shapefile with adm2 used)

Each tibble contains columns: survey_id, iso3, iso2, survey_type,
survey_year, adm0, adm1, adm2, type, geo_source, point, ci_l, ci_u,
numerator, denominator, indicator, indicator_code,
numerator_description, denominator_description, denominator_code.

## Details

Computes up to 42+ ITN indicators following WHO methodology, organised
in 6 categories: ENOUGH_ITN (household sufficient nets), WITH_ITN
(household ownership), ACCESS_ITN (population access), USE_ITN_CHU5
(under 5 use), USE_ITN_PREGNANT (pregnant women use), and USE_ITN
(general population use). Each category includes 7 subgroup splits:
overall, LOW_WEALTH, HIGH_WEALTH, NON_LOW_WEALTH, NON_HIGH_WEALTH,
RURAL, URBAN.

When `age_breaks` and `age_labels` are provided, additional
USE_ITN_AGE\_\* indicators are computed for each age group.

## See also

[`itn_dictionary()`](https://ahadi-analytics.github.io/sntmethods/reference/itn_dictionary.md)
for indicator definitions
