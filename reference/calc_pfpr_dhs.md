# Calculate PfPR Indicators from DHS Data

Computes PfPR (Plasmodium falciparum Parasite Rate) indicators from DHS
Person Records (PR) data. Returns survey-weighted proportions with logit
confidence intervals in standardized long format.

## Usage

``` r
calc_pfpr_dhs(
  dhs_pr,
  survey_vars = list(cluster = "hv021", weight = "hv005", stratum = "hv022", adm1 =
    "hv024", adm2 = NULL, age = "hc1", present = "hv103", mother = "hv042", rdt =
    "hml35", mic = "hml32"),
  region_var = NULL,
  gps_data = NULL,
  gps_vars = list(cluster = "DHSCLUST", lat = "LATNUM", lon = "LONGNUM"),
  shapefile = NULL,
  admin_level = NULL,
  join_nearest = TRUE,
  indicators = NULL,
  ci_method = "logit"
)
```

## Arguments

- dhs_pr:

  DHS Person Records (PR) dataset (data.frame or tibble).

- survey_vars:

  Named list mapping DHS variable names. Required keys:

  - `cluster`: PSU ID (default: "hv021")

  - `weight`: Survey weight (default: "hv005")

  - `stratum`: Stratum variable (default: "hv022")

  - `adm1`: Admin-1 variable (default: "hv024")

  - `age`: Child's age in months (default: "hc1")

  - `present`: Present in household (default: "hv103")

  - `mother`: Mother listed in household (default: "hv042")

  - `rdt`: RDT result (default: "hml35")

  - `mic`: Microscopy result (default: "hml32")

- region_var:

  Optional column name for subnational grouping (e.g., "hv024").

- gps_data:

  Optional DHS GE dataset with cluster coordinates.

- gps_vars:

  Named list for GE variables: cluster, lat, lon.

- shapefile:

  Optional sf object with administrative boundaries.

- admin_level:

  Character vector of admin columns from shapefile.

- join_nearest:

  Logical; if TRUE, assigns unmatched clusters to nearest admin unit.
  Default: TRUE.

- indicators:

  Character vector of indicator names to compute. If NULL (default),
  computes all indicators from
  [`pfpr_dictionary`](https://ahadi-analytics.github.io/sntmethods/reference/pfpr_dictionary.md).

- ci_method:

  Method for confidence intervals. Default: "logit".

## Value

Named list of tibbles, one per admin level:

- `adm0`:

  National-level estimates (always present)

- `adm1`:

  Admin-1 estimates (when region_var or shapefile)

- `adm2`:

  Admin-2 estimates (when shapefile with adm2)

Each tibble has standard columns: survey_id, iso3, iso2, survey_type,
survey_year, adm0, type, geo_source, point, ci_l, ci_u, numerator,
denominator, indicator, indicator_code, numerator_description,
denominator_description, denominator_code.

## Details

Computes PfPR for children aged 6-59 months using RDT (hml35) and/or
microscopy (hml32) results. Follows the same output pattern as
[`calc_act_dhs`](https://ahadi-analytics.github.io/sntmethods/reference/calc_act_dhs.md)
and
[`calc_itn_dhs`](https://ahadi-analytics.github.io/sntmethods/reference/calc_itn_dhs.md).

## See also

[`pfpr_dictionary`](https://ahadi-analytics.github.io/sntmethods/reference/pfpr_dictionary.md)
for indicator definitions

## Examples

``` r
if (FALSE) { # \dontrun{
result <- calc_pfpr_dhs(dhs_pr = pr_data)
result$adm0  # national PfPR estimates

# With subnational
result <- calc_pfpr_dhs(dhs_pr = pr_data, region_var = "hv024")
result$adm1  # regional PfPR
} # }
```
