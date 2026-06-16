# Prepare ACT and Antimalarial Data for MBG Analysis

Prepares cluster-level ACT (Artemisinin-based Combination Therapy),
antimalarial treatment, and malaria diagnostic data for MBG analysis.
Uses a dictionary-driven approach matching the indicator codes from
[`calc_act_dhs`](https://ahadi-analytics.github.io/sntmethods/reference/calc_act_dhs.md).

## Usage

``` r
calc_act_mbg(
  dhs_kr,
  gps_data,
  dhs_pr = NULL,
  indicators = c("act", "act_tested"),
  survey_vars = list(cluster = "v001", age = "hw1", fever = "h22", alive = "b5", act =
    "ml13e", test = "ml13a"),
  gps_vars = list(cluster = "DHSCLUST", lat = "LATNUM", lon = "LONGNUM")
)
```

## Arguments

- dhs_kr:

  DHS Children's Recode (KR) dataset.

- gps_data:

  DHS GPS dataset with cluster coordinates.

- dhs_pr:

  Optional DHS Person Recode (PR) dataset. Required for
  `"febrile_rdt_pos"` and `"febrile_rdt_pos_act"` indicators (provides
  hml35).

- indicators:

  Character vector of indicators to calculate. See
  `.act_mbg_dictionary()` for the full list of standardized indicator
  codes. Legacy names `"act_pub"` and `"act_among_am"` are also
  accepted. Special indicators:

  - `"act_tested"`: ACT among test-positive

  - `"febrile_rdt_pos"`: RDT positivity (requires dhs_pr)

  - `"febrile_rdt_pos_act"`: ACT among RDT-positive (requires dhs_pr)

  Default: `c("act", "act_tested")`.

- survey_vars:

  Named list mapping DHS variable names:

  - `cluster`: Cluster ID (default: "v001")

  - `age`: Child's age in months (default: "hw1")

  - `fever`: Fever in last 2 weeks (default: "h22")

  - `alive`: Child is alive (default: "b5")

  - `act`: ACT variable (default: "ml13e")

  - `test`: Diagnostic test variable (default: "ml13a")

- gps_vars:

  Named list for GPS variable mapping.

## Value

A named list of data.tables (one per indicator), each with columns:

- cluster_id: Cluster identifier

- indicator: Numerator count

- samplesize: Denominator count

- x: Longitude

- y: Latitude

## Details

All dictionary-based indicators share the same data preparation
pipeline:

1.  Filter to febrile U5 children (via `.prepare_act_data()`)

2.  Classify care-seeking sectors (via `.classify_csb_from_h32()`)

3.  Build antimalarial composite from ml13/h37 series

4.  Build malaria diagnostic flag from ml1/h47

5.  Apply per-indicator filters and aggregate to cluster-level counts

The dictionary includes three indicator families:

- **ACT** (`act_*`): ACT receipt among febrile U5, with sector and AM
  filters

- **Antimalarial** (`antimal_*`): Antimalarial receipt among febrile U5,
  with sector filters

- **Malaria diagnostic** (`mal_dx_*`): Malaria diagnostic test (ml1/h47)
  among AM recipients, with sector filters

## See also

[`calc_act_dhs()`](https://ahadi-analytics.github.io/sntmethods/reference/calc_act_dhs.md)
for survey-weighted estimates,
[`calc_csb_mbg()`](https://ahadi-analytics.github.io/sntmethods/reference/calc_csb_mbg.md)
for care-seeking behavior

## Examples

``` r
if (FALSE) { # \dontrun{
act_mbg <- calc_act_mbg(
  dhs_kr = kr_data,
  gps_data = gps_data,
  indicators = c("act_pub_am", "act_trained_am", "antimal_chw",
                  "mal_dx_am", "mal_dx_pub_am")
)
} # }
```
