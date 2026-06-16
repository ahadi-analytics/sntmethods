# Prepare Antimalarial Treatment Data for MBG Analysis

Prepares cluster-level antimalarial treatment data for MBG analysis.
Uses a dictionary-driven approach matching the indicator codes from
[`calc_antimalarial_dhs`](https://ahadi-analytics.github.io/sntmethods/reference/calc_antimalarial_dhs.md).

## Usage

``` r
calc_antimalarial_mbg(
  dhs_kr,
  gps_data,
  indicators = "antimalarial",
  survey_vars = list(cluster = "v001", age = "hw1", fever = "h22"),
  gps_vars = list(cluster = "DHSCLUST", lat = "LATNUM", lon = "LONGNUM")
)
```

## Arguments

- dhs_kr:

  DHS Children's Recode (KR) dataset.

- gps_data:

  DHS GPS dataset with cluster coordinates.

- indicators:

  Character vector of indicators to calculate. See
  `.antimalarial_mbg_dictionary()` for the full list of standardized
  indicator codes. Default: `"antimalarial"`.

- survey_vars:

  Named list mapping DHS variable names:

  - `cluster`: Cluster ID (default: "v001")

  - `age`: Child's age in months (default: "hw1")

  - `fever`: Fever in last 2 weeks (default: "h22")

- gps_vars:

  Named list for GPS variable mapping.

## Value

A named list of data.tables (one per indicator), each with columns:

- cluster_id: Cluster identifier

- indicator: Numerator count (children receiving antimalarial)

- samplesize: Denominator count (febrile U5 children)

- x: Longitude

- y: Latitude

## Details

Methodology:
<https://github.com/ahadi-analytics/sntmethods/blob/master/inst/methods/antimalarial_dhs.yml>

All dictionary-based indicators share the same data preparation
pipeline:

1.  Filter to febrile U5 children (via `.prepare_antimalarial_data()`)

2.  Classify care-seeking sectors if needed (via
    `.classify_csb_from_h32()`)

3.  Apply per-indicator filters and aggregate to cluster-level counts

## See also

[`calc_antimalarial_dhs()`](https://ahadi-analytics.github.io/sntmethods/reference/calc_antimalarial_dhs.md)
for survey-weighted estimates,
[`calc_act_mbg()`](https://ahadi-analytics.github.io/sntmethods/reference/calc_act_mbg.md)
for ACT-specific treatment

## Examples

``` r
if (FALSE) { # \dontrun{
am_mbg <- calc_antimalarial_mbg(
  dhs_kr = kr_data,
  gps_data = gps_data,
  indicators = c("antimalarial", "antimalarial_public")
)
} # }
```
