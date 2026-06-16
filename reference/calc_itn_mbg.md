# Prepare ITN Data for MBG Analysis

Prepares cluster-level ITN ownership, access, and use data for
Model-Based Geostatistics (MBG) analysis. Aggregates to cluster counts
WITHOUT survey weights - MBG handles spatial smoothing internally.

## Usage

``` r
calc_itn_mbg(
  dhs_hr,
  dhs_pr,
  gps_data,
  indicators = NULL,
  survey_vars = list(cluster = "hv001", hhid = "hhid", hhsize = "hv013", age = "hv105",
    sex = "hv104", pregnant = "hml18", itn_use = "hml12", itn_prefix = "hml10_",
    itn_treated_prefix = "hml7_"),
  gps_vars = list(cluster = "DHSCLUST", lat = "LATNUM", lon = "LONGNUM"),
  seed = NULL
)
```

## Arguments

- dhs_hr:

  DHS Household Records dataset.

- dhs_pr:

  DHS Person Records dataset.

- gps_data:

  DHS GPS dataset with cluster coordinates.

- indicators:

  Character vector of indicators to calculate. Options from
  `.itn_mbg_dictionary()`:

  - `"with_itn"`: Households with at least one ITN (HR)

  - `"enough_itn"`: Households with enough ITNs for every 2 people (HR)

  - `"access_itn"`: Population with access to ITN – binary indicator
    (PR)

  - `"use_itn"`: Population that used ITN last night (PR)

  - `"use_itn_chu5"`: Under-5 children that used ITN (PR)

  - `"use_itn_5_10"`: Children 5-9 years that used ITN (PR)

  - `"use_itn_10_20"`: Adolescents 10-19 years that used ITN (PR)

  - `"use_itn_20plus"`: Adults 20+ that used ITN (PR)

  - `"use_itn_preg"`: Pregnant women that used ITN (PR)

  - `"use_itn_if_access"`: Of those with access, proportion that used
    ITN (PR)

  Default: all indicators.

- survey_vars:

  Named list mapping DHS variable names.

- gps_vars:

  Named list for GPS variable mapping.

- seed:

  Deprecated. Previously used for probabilistic access assignment.
  Access is now calculated deterministically following standard DHS
  methodology.

## Value

A list of data.tables (one per indicator), each with columns:

- cluster_id: Cluster identifier

- indicator: Numerator count

- samplesize: Denominator count

- x: Longitude

- y: Latitude

## Details

Uses a dictionary-driven approach matching the indicator codes from
[`calc_itn_dhs`](https://ahadi-analytics.github.io/sntmethods/reference/calc_itn_dhs.md).
The dictionary mirrors the DHS `.itn_conditions()` – same outcome
variables, same filters, same data sources (HR vs PR).

Methodology:
<https://github.com/ahadi-analytics/sntmethods/blob/master/inst/methods/itn_dhs.yml>

This function prepares data for MBG spatial modeling. Unlike the survey-
weighted
[`calc_itn_dhs()`](https://ahadi-analytics.github.io/sntmethods/reference/calc_itn_dhs.md)
function, this uses simple cluster-level counts.

ITN access is calculated using the standard DHS deterministic assignment
method:

1.  Calculate potential users per household: min(ITNs \* 2,
    household_size)

2.  Sort individuals within each household by ITN use (users first)

3.  Assign access to the first N individuals where N = potential_users

This method guarantees that use \<= access at the individual level,
since anyone who used an ITN is prioritised for access assignment.

## See also

[`calc_itn_dhs()`](https://ahadi-analytics.github.io/sntmethods/reference/calc_itn_dhs.md)
for survey-weighted estimates, `.itn_mbg_dictionary()` for indicator
definitions

## Examples

``` r
if (FALSE) { # \dontrun{
itn_mbg <- calc_itn_mbg(
  dhs_hr = hr_data,
  dhs_pr = pr_data,
  gps_data = gps_data,
  indicators = c("access_itn", "use_itn_chu5")
)
} # }
```
