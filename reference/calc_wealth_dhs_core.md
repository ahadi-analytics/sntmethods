# Calculate Core Wealth Quintile Distributions from DHS Data

Core function that calculates wealth quintile distributions and Gini
coefficients using DHS household data. When GPS data and shapefile are
provided, performs spatial joins to aggregate at administrative levels.

## Usage

``` r
calc_wealth_dhs_core(
  dhs_hr,
  survey_vars = list(cluster = "hv001", weight = "hv005", stratum = "hv022", adm1 =
    "hv024", adm2 = NULL, wealth_quintile = "hv270", wealth_score = "hv271", hh_members =
    "hv012"),
  gps_data = NULL,
  gps_vars = list(cluster = "DHSCLUST", lat = "LATNUM", lon = "LONGNUM"),
  shapefile = NULL,
  admin_level = NULL,
  join_nearest = TRUE
)
```

## Arguments

- dhs_hr:

  DHS Household Records dataset in tidy format (data.frame or tibble).

- survey_vars:

  Named list mapping DHS variable names. Required keys:

  - `cluster`: Cluster ID (default: "hv001")

  - `weight`: Survey weight (default: "hv005", divided by 1,000,000)

  - `stratum`: Survey stratum (default: "hv022")

  - `adm1`: First administrative level (default: "hv024")

  - `adm2`: Second administrative level (default: NULL)

  - `wealth_quintile`: Wealth quintile variable (default: "hv270")

  - `wealth_score`: Wealth index factor score (default: "hv271")

  - `hh_members`: De jure household members (default: "hv012")

- gps_data:

  Optional DHS GPS dataset. If provided with shapefile, enables spatial
  aggregation.

- gps_vars:

  Named list for GPS variables (cluster, lat, lon).

- shapefile:

  Optional sf object with administrative boundaries.

- admin_level:

  Character vector of admin columns in shapefile.

- join_nearest:

  Logical; if TRUE, assigns unmatched clusters to nearest polygon.
  Default TRUE.

## Value

A tibble with wealth quintile distributions and Gini coefficients by
administrative unit or cluster.
