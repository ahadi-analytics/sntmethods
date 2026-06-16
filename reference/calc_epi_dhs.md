# Calculate EPI Coverage from DHS Data (Standardized)

Estimates vaccination coverage using survey-weighted methods from DHS
Children's Recode data. Returns standardized long-format output as
`list(adm0, adm1)`.

## Usage

``` r
calc_epi_dhs(
  dhs_kr,
  indicators = NULL,
  age_min_months = 12,
  age_max_months = 23,
  survey_vars = list(cluster = "v021", weight = "v005", stratum = "v022", age = "hw1",
    bcg = "h2", dpt1 = "h3", dpt2 = "h4", dpt3 = "h5", polio0 = "h0", polio1 = "h6",
    polio2 = "h7", polio3 = "h8", measles1 = "h9", measles2 = "h9a", vita1 = "h33", vita2
    = "h33a", malaria = "h68", penta1 = "h51", penta2 = "h52", penta3 = "h53", pneumo1 =
    "h54", pneumo2 = "h55", pneumo3 = "h56", rota1 = "h57", rota2 = "h58", rota3 = "h59",
    ipv = "h60", hepb0 = "h50", yellowfever = "h61", any = "h10"),
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

- indicators:

  Character vector of vaccines to calculate, or NULL for all available.
  Options include: "bcg", "dpt1", "dpt2", "dpt3", "polio0", "polio1",
  "polio2", "polio3", "measles1", "measles2", "vita1", "vita2",
  "malaria", "penta1", "penta2", "penta3", "pneumo1", "pneumo2",
  "pneumo3", "rota1", "rota2", "rota3", "ipv", "hepb0", "yellowfever",
  "any", "never_vaccinated", "fully_vaccinated".

- age_min_months:

  Minimum age in months (default: 12).

- age_max_months:

  Maximum age in months (default: 23).

- survey_vars:

  Named list mapping DHS variable names.

- region_var:

  Optional column name to use as grouping variable.

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

  CI method for svyciprop. Default: "logit".

## Value

Named list with `adm0` (national) and optionally `adm1` (regional)
tibbles in standardized long format.

## See also

[`epi_dictionary()`](https://ahadi-analytics.github.io/sntmethods/reference/epi_dictionary.md)
for indicator definitions,
[`calc_epi_dhs_core()`](https://ahadi-analytics.github.io/sntmethods/reference/calc_epi_dhs_core.md)
for backward-compatible wide output
