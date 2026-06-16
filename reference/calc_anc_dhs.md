# Calculate ANC Coverage from DHS Data (standardized long-format output)

Computes ANC 1+/2+/3+/4+/8+ coverage indicators nationally and
optionally by subnational region, returning the standardized
`list(adm0, adm1)` output.

## Usage

``` r
calc_anc_dhs(
  dhs_ir,
  survey_vars = list(cluster = "v021", weight = "v005", stratum = "v022", interview_date
    = "v008", birth_date = "b3_01", anc_visits = "m14_1"),
  birth_window_months = 24,
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

- dhs_ir:

  DHS Individual Recode (IR) dataset.

- survey_vars:

  Named list mapping DHS variable names. Required keys:

  - `cluster`: Cluster/PSU ID (default: "v021")

  - `weight`: Survey weight (default: "v005")

  - `stratum`: Stratum variable (default: "v022")

  - `interview_date`: Interview date CMC (default: "v008")

  - `birth_date`: Birth date CMC (default: "b3_01")

  - `anc_visits`: Number of ANC visits (default: "m14_1")

- birth_window_months:

  Maximum months since last birth. Default: 24.

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

  Logical; if TRUE, assigns unmatched clusters.

- ci_method:

  CI method for svyciprop. Default: "logit".

## Value

Named list with `adm0` tibble and optionally `adm1` tibble in
standardized long format.
