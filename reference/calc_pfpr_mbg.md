# Prepare PfPR Data for MBG Analysis

Prepares cluster-level malaria parasite prevalence data for Model-Based
Geostatistics (MBG) analysis. Uses a dictionary-driven approach matching
the indicator codes from
[`calc_pfpr_dhs`](https://ahadi-analytics.github.io/sntmethods/reference/calc_pfpr_dhs.md).

## Usage

``` r
calc_pfpr_mbg(
  dhs_pr,
  gps_data,
  indicators = NULL,
  test_type = NULL,
  age_groups = NULL,
  survey_vars = list(cluster = "hv001", age = "hc1", present = "hv103", mother = "hv042",
    rdt = "hml35", mic = "hml32"),
  gps_vars = list(cluster = "DHSCLUST", lat = "LATNUM", lon = "LONGNUM")
)
```

## Arguments

- dhs_pr:

  DHS Person Records dataset (data.frame or tibble).

- gps_data:

  DHS GPS dataset with cluster coordinates.

- indicators:

  Character vector of indicator codes to calculate. Available codes
  (`<test>` = `rdt` or `mic`):

  - `"pfpr_<test>"`: 6-59 months (standard DHS PfPR)

  - `"pfpr_<test>_u5"`: 0-59 months

  - `"pfpr_<test>_5_10"`: 60-119 months

  - `"pfpr_<test>_u10"`: 0-119 months

  - `"pfpr_<test>_2_10"`: 24-119 months (PfPR2-10 reference)

  Default: all indicators from the dictionary.

- test_type:

  **Deprecated**. Character. Use `indicators` instead. When provided,
  translated to indicator codes for backward compatibility. One of
  `"rdt"`, `"mic"`, `"both"`, or `"either"`.

- age_groups:

  **Deprecated**. Named list of age ranges. Use `indicators` instead.
  When provided alongside `test_type`, translated to indicator codes.

- survey_vars:

  Named list mapping DHS variable names. Required keys:

  - cluster: Cluster ID (default: "hv001")

  - age: Age in months (default: "hc1")

  - present: Present in household (default: "hv103")

  - mother: Mother listed in household (default: "hv042")

  - rdt: RDT result variable (default: "hml35")

  - mic: Microscopy result variable (default: "hml32")

- gps_vars:

  Named list for GPS variable mapping.

## Value

A named list of data.tables (one per indicator), each with columns:

- cluster_id: Cluster identifier

- indicator: Number of positive tests (numerator for MBG)

- samplesize: Number of children tested (denominator for MBG)

- x: Longitude

- y: Latitude

## Details

All dictionary-based indicators share the same data preparation pipeline
via `.prepare_pfpr_data()`, the same shared helper used by the
survey-weighted
[`calc_pfpr_dhs()`](https://ahadi-analytics.github.io/sntmethods/reference/calc_pfpr_dhs.md)
function. Positivity definitions are identical:

- RDT positive: `rdt_res == 1` (hml35 == 1)

- Microscopy positive: `mic_res == 1` (hml32 == 1, Pf only)

- Either: positive on RDT OR microscopy

Unlike the survey-weighted function, this uses simple cluster-level
counts because MBG handles spatial smoothing and uncertainty internally.

Methodology:
<https://github.com/ahadi-analytics/sntmethods/blob/master/inst/methods/pfpr_dhs.yml>

## See also

[`calc_pfpr_dhs()`](https://ahadi-analytics.github.io/sntmethods/reference/calc_pfpr_dhs.md)
for survey-weighted estimates

## Examples

``` r
if (FALSE) { # \dontrun{
# New-style: specify exact indicator codes
pfpr_mbg <- calc_pfpr_mbg(
  dhs_pr = pr_data,
  gps_data = gps_data,
  indicators = c("pfpr_rdt_u5", "pfpr_mic_u5")
)

# Legacy style (still works, with deprecation warning)
pfpr_mbg <- calc_pfpr_mbg(
  dhs_pr = pr_data,
  gps_data = gps_data,
  test_type = "rdt",
  age_groups = list(u5 = c(6, 59))
)
} # }
```
