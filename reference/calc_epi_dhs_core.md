# Calculate EPI Coverage from DHS Data

Estimates vaccination coverage using survey-weighted methods from DHS
Children's Recode data.

## Usage

``` r
calc_epi_dhs_core(
  dhs_kr,
  indicators = c("bcg", "dpt3", "measles1", "fully_vaccinated"),
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
  join_nearest = TRUE
)
```

## Arguments

- dhs_kr:

  DHS Children's Recode (KR) dataset.

- indicators:

  Character vector of vaccines to calculate. Options: "bcg", "dpt1",
  "dpt2", "dpt3", "polio0", "polio1", "polio2", "polio3", "measles1",
  "measles2", "vita1", "vita2", "malaria", "any", "never_vaccinated",
  "fully_vaccinated". Default: c("bcg", "dpt3", "measles1",
  "fully_vaccinated").

- age_min_months:

  Minimum age in months (default: 12).

- age_max_months:

  Maximum age in months (default: 23).

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

## Value

Tibble with EPI estimates. For each vaccine: `dhs_epi_<vaccine>`,
`dhs_epi_<vaccine>_low`, `dhs_epi_<vaccine>_upp`. Plus
`dhs_n_epi_eligible`.

## See also

[`calc_epi_mbg()`](https://ahadi-analytics.github.io/sntmethods/reference/calc_epi_mbg.md)
for cluster-level MBG inputs
