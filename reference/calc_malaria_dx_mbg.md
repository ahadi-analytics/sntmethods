# Prepare Malaria Diagnostic Testing Data for MBG Analysis

Prepares cluster-level malaria diagnostic testing data for MBG analysis.
Calculates counts of febrile children under 5 who had blood taken for
malaria testing, at each survey cluster.

## Usage

``` r
calc_malaria_dx_mbg(
  dhs_kr,
  gps_data,
  indicators = "malaria_dx",
  survey_vars = list(cluster = "v001", age = "hw1", fever = "h22", malaria_dx = "h47"),
  gps_vars = list(cluster = "DHSCLUST", lat = "LATNUM", lon = "LONGNUM")
)
```

## Arguments

- dhs_kr:

  DHS Children's Recode (KR) dataset.

- gps_data:

  DHS GPS dataset with cluster coordinates.

- indicators:

  Character vector of indicators to calculate:

  - "malaria_dx": Blood taken for malaria test among febrile U5

  Default: "malaria_dx".

- survey_vars:

  Named list mapping DHS variable names:

  - `cluster`: Cluster ID (default: "v001")

  - `age`: Child's age in months (default: "hw1")

  - `fever`: Fever in last 2 weeks (default: "h22")

  - `malaria_dx`: Blood taken for malaria test (default: "h47")

- gps_vars:

  Named list for GPS variable mapping.

## Value

A named list of data.tables (one per indicator), each with columns:

- cluster_id: Cluster identifier

- indicator: Numerator count (children tested)

- samplesize: Denominator count (febrile U5 children)

- x: Longitude

- y: Latitude

## Details

Methodology:
<https://github.com/ahadi-analytics/sntmethods/blob/master/inst/methods/malaria_dx_dhs.yml>

## See also

[`calc_malaria_dx_dhs_core()`](https://ahadi-analytics.github.io/sntmethods/reference/calc_malaria_dx_dhs_core.md)
for survey-weighted estimates,
[`calc_act_mbg()`](https://ahadi-analytics.github.io/sntmethods/reference/calc_act_mbg.md)
for ACT treatment

## Examples

``` r
if (FALSE) { # \dontrun{
dx_mbg <- calc_malaria_dx_mbg(
  dhs_kr = kr_data,
  gps_data = gps_data
)
} # }
```
