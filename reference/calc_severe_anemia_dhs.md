# Calculate Severe Anemia Prevalence from DHS Data (Standardized)

Computes anemia indicators from DHS Person Records (PR) data. Returns
survey-weighted proportions with logit confidence intervals in
standardized long format.

## Usage

``` r
calc_severe_anemia_dhs(
  dhs_pr,
  survey_vars = list(cluster = "hv001", weight = "hv005", stratum = "hv022", adm1 =
    "hv024", adm2 = NULL, age = "hc1", hemoglobin = "hc56", hemoglobin_adj = "hw53",
    present = "hv103", mother = "hv042"),
  altitude_adjusted = TRUE,
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

- dhs_pr:

  DHS Person Records dataset (data.frame or tibble).

- survey_vars:

  Named list mapping DHS variable names. Required keys:

  - `cluster`: Cluster ID (default: "hv001")

  - `weight`: Survey weight (default: "hv005", divided by 1,000,000)

  - `stratum`: Stratum variable (default: "hv022")

  - `adm1`: First administrative level (default: "hv024")

  - `age`: Child's age in months (default: "hc1")

  - `hemoglobin`: Raw hemoglobin in tenths of g/dL (default: "hc56")

  - `hemoglobin_adj`: Altitude-adjusted hemoglobin (default: "hw53")

  - `present`: Present in household (default: "hv103")

  - `mother`: Mother listed in household (default: "hv042")

- altitude_adjusted:

  Logical. If TRUE (default), uses altitude-adjusted hemoglobin variable
  (hw53). If FALSE, uses raw hemoglobin (hc56).

- region_var:

  Optional column name for subnational grouping (e.g., "hv024"). If
  NULL, defaults to survey_vars\$adm1.

- gps_data:

  Optional DHS GE (GPS) cluster dataset used to attach admin-unit labels
  when `shapefile` is supplied. Default `NULL`.

- gps_vars:

  Named list mapping cluster/lat/lon column names in `gps_data`.
  Defaults to the standard DHS GE names (`DHSCLUST`, `LATNUM`,
  `LONGNUM`).

- shapefile:

  Optional `sf` polygon dataset whose attributes carry admin labels for
  the cluster-to-admin spatial join. When `NULL` (default) the spatial
  join step is skipped.

- admin_level:

  Character vector of admin column names in `shapefile` to retain (e.g.
  `c("adm1", "adm2")`). Default `NULL` (use all).

- join_nearest:

  Logical. If `TRUE` (default), clusters that fall outside any polygon
  are re-assigned to the nearest polygon. If `FALSE`, unmatched clusters
  are left as `NA`.

- ci_method:

  Method for confidence intervals. Default: "logit".

## Value

Named list of tibbles:

- `adm0`:

  National-level estimates (always present)

- `adm1`:

  Admin-1 estimates (when `region_var` or adm1 available)

Each tibble contains columns: survey_id, iso3, iso2, survey_type,
survey_year, adm0, adm1, type, geo_source, point, ci_l, ci_u, numerator,
denominator, indicator, indicator_code, numerator_description,
denominator_description, denominator_code.

## Details

Computes six anemia indicators following WHO thresholds. See
[`severe_anemia_dictionary()`](https://ahadi-analytics.github.io/sntmethods/reference/severe_anemia_dictionary.md)
for the full indicator list.

## See also

[`severe_anemia_dictionary()`](https://ahadi-analytics.github.io/sntmethods/reference/severe_anemia_dictionary.md)
for indicator metadata,
[`calc_severe_anemia_dhs_core()`](https://ahadi-analytics.github.io/sntmethods/reference/calc_severe_anemia_dhs_core.md)
for legacy wide-format output

## Examples

``` r
if (FALSE) { # \dontrun{
anemia <- calc_severe_anemia_dhs(dhs_pr = pr_data, region_var = "hv024")
anemia$adm0
anemia$adm1
} # }
```
