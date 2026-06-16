# Prepare Fever Prevalence Data for MBG Analysis

Prepares cluster-level fever prevalence data for MBG analysis.
Calculates counts of alive children under 5 who had fever in the last 2
weeks, at each survey cluster.

## Usage

``` r
calc_fever_mbg(
  dhs_kr,
  gps_data,
  indicators = "fever",
  survey_vars = list(cluster = "v001", age = "hw1", fever = "h22", alive = "b5"),
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

  - "fever": Fever prevalence among alive U5 children

  Default: "fever".

- survey_vars:

  Named list mapping DHS variable names:

  - `cluster`: Cluster ID (default: "v001")

  - `age`: Child's age in months (default: "hw1")

  - `fever`: Fever in last 2 weeks (default: "h22")

  - `alive`: Child survival status (default: "b5")

- gps_vars:

  Named list for GPS variable mapping.

## Value

A named list of data.tables (one per indicator), each with columns:

- cluster_id: Cluster identifier

- indicator: Numerator count (children with fever)

- samplesize: Denominator count (all alive U5 children)

- x: Longitude

- y: Latitude

## Details

Methodology:
<https://github.com/ahadi-analytics/sntmethods/blob/master/inst/methods/fever_dhs.yml>

## See also

[`calc_fever_dhs_core()`](https://ahadi-analytics.github.io/sntmethods/reference/calc_fever_dhs_core.md)
for survey-weighted estimates,
[`calc_csb_mbg()`](https://ahadi-analytics.github.io/sntmethods/reference/calc_csb_mbg.md)
for care-seeking behavior

## Examples

``` r
if (FALSE) { # \dontrun{
fever_mbg <- calc_fever_mbg(
  dhs_kr = kr_data,
  gps_data = gps_data
)
} # }
```
