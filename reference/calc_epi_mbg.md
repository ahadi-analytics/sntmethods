# Prepare EPI (Vaccination) Data for MBG Analysis

Prepares cluster-level vaccination coverage data for MBG analysis.
Calculates coverage for standard EPI vaccines plus malaria vaccine.

## Usage

``` r
calc_epi_mbg(
  dhs_kr,
  gps_data,
  indicators = c("bcg", "dpt3", "measles1"),
  age_min_months = 12,
  age_max_months = 23,
  survey_vars = list(cluster = "v001", age = "hw1", bcg = "h2", polio0 = "h0", dpt1 =
    "h3", dpt2 = "h4", dpt3 = "h5", polio1 = "h6", polio2 = "h7", polio3 = "h8", measles1
    = "h9", measles2 = "h9a", vita1 = "h33", vita2 = "h33a", malaria = "h68", penta1 =
    "h51", penta2 = "h52", penta3 = "h53", pneumo1 = "h54", pneumo2 = "h55", pneumo3 =
    "h56", rota1 = "h57", rota2 = "h58", rota3 = "h59", ipv = "h60", hepb0 = "h50",
    yellowfever = "h61", any = "h10"),
  gps_vars = list(cluster = "DHSCLUST", lat = "LATNUM", lon = "LONGNUM")
)
```

## Arguments

- dhs_kr:

  DHS Children's Recode (KR) dataset.

- gps_data:

  DHS GPS dataset with cluster coordinates.

- indicators:

  Character vector of vaccines to calculate:

  - "bcg": BCG vaccine

  - "dpt1", "dpt2", "dpt3": DPT doses 1-3 (falls back to pentavalent)

  - "polio0": OPV birth dose

  - "polio1", "polio2", "polio3": Polio doses 1-3

  - "measles1", "measles2": Measles doses 1-2

  - "vita1", "vita2": Vitamin A doses 1-2

  - "malaria": Malaria vaccine (RTS,S/R21)

  - "penta1", "penta2", "penta3": Pentavalent doses 1-3

  - "pneumo1", "pneumo2", "pneumo3": Pneumococcal doses 1-3

  - "rota1", "rota2", "rota3": Rotavirus doses 1-3

  - "ipv": Inactivated Polio Vaccine

  - "hepb0": Hepatitis B birth dose

  - "yellowfever": Yellow Fever vaccine

  - "any": Any vaccination (h10 \>= 1)

  - "never_vaccinated": Zero-dose (h10 == 0)

  - "fully_vaccinated": Basic fully vaccinated

  Default: c("bcg", "dpt3", "measles1").

- age_min_months:

  Minimum age in months (default: 12).

- age_max_months:

  Maximum age in months (default: 23).

- survey_vars:

  Named list mapping DHS variable names.

- gps_vars:

  Named list for GPS variable mapping.

## Value

A list of data.tables (one per vaccine).

## Details

Methodology:
<https://github.com/ahadi-analytics/sntmethods/blob/master/inst/methods/epi_dhs.yml>

Standard EPI target age is 12-23 months (children who have had time to
complete basic vaccination schedule). Malaria vaccine (RTS,S) is
measured by h68 variable.

## Examples

``` r
if (FALSE) { # \dontrun{
epi_mbg <- calc_epi_mbg(
  dhs_kr = kr_data,
  gps_data = gps_data,
  indicators = c("bcg", "dpt3", "measles1")
)
} # }
```
