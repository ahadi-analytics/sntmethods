# Prepare Care-Seeking Behavior Data by Wealth Quintile for MBG Analysis

Prepares cluster-level care-seeking behavior data stratified by wealth
quintile for MBG analysis. Calculates proportions of febrile children
who sought care, separately for each wealth quintile.

## Usage

``` r
calc_csb_by_wealth_mbg(
  dhs_kr,
  gps_data,
  indicators = c("any", "public", "private", "none"),
  quintiles = 1:5,
  wealth_var = "v190",
  csb_priority_method = c("all", "first", "public", "private"),
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

  Character vector of indicators to calculate:

  - "any": Sought care anywhere (public or private)

  - "public": Public sector including CHW

  - "pub_nochw": Public sector excluding CHW

  - "chw": Community health worker only

  - "private": Any private sector

  - "priv_formal": Private formal sector only

  - "pharmacy": Pharmacy / drug shop only

  - "priv_informal": Private informal only

  - "priv_form_pha": Private formal or pharmacy

  - "trained": Trained provider (public + formal + pharmacy)

  - "none": Did not seek care

  Default: c("any", "public", "private", "none").

- quintiles:

  Numeric vector of wealth quintiles to include. Default: 1:5 (all
  quintiles). Use c(1) for poorest only, c(1,2) for poorest + poorer,
  etc.

- wealth_var:

  Name of wealth quintile variable in dhs_kr. Default: "v190".

- csb_priority_method:

  Character, one of "all" (default), "first", "public", or "private".
  Controls how overlapping care-seeking records are resolved so each
  individual is assigned to at most one sector. See
  [`calc_csb_mbg()`](https://ahadi-analytics.github.io/sntmethods/reference/calc_csb_mbg.md)
  for details. With non-"all" values, csb_public + csb_private +
  csb_none sums to 100% within each quintile.

- survey_vars:

  Named list mapping DHS variable names.

- gps_vars:

  Named list for GPS variable mapping.

## Value

A nested list structure: first level keys are quintiles (e.g., "q1",
"q2"), second level keys are indicators (e.g., "csb_public_q1"). Each
leaf is a data.table with columns:

- cluster_id: Cluster identifier

- indicator: Numerator count (children who sought care)

- samplesize: Denominator count (all febrile children in quintile)

- x: Longitude

- y: Latitude

## Details

This function extends
[`calc_csb_mbg()`](https://ahadi-analytics.github.io/sntmethods/reference/calc_csb_mbg.md)
to provide wealth-stratified estimates. Each quintile produces separate
MBG-ready outputs with cluster-level numerators and denominators.

**Important:** This is a standalone utility function for specialized
wealth stratification analysis. It is NOT called by
[`run_mbg_pipeline()`](https://ahadi-analytics.github.io/sntmethods/reference/run_mbg_pipeline.md).
Use this function directly when you need wealth-disaggregated indicators
for custom MBG modeling or equity analysis.

Methodology:
<https://github.com/ahadi-analytics/sntmethods/blob/master/inst/methods/csb_dhs.yml>

## Output Structure

The function returns a list where each indicator-quintile combination
gets its own data.table. For example, with indicators = c("public",
"private") and quintiles = c(1, 5):


    list(
      csb_public_q1 = data.table(...),
      csb_public_q5 = data.table(...),
      csb_private_q1 = data.table(...),
      csb_private_q5 = data.table(...)
    )

## See also

- [`calc_csb_mbg()`](https://ahadi-analytics.github.io/sntmethods/reference/calc_csb_mbg.md)
  for non-stratified care-seeking MBG data

- [`calc_csb_by_wealth_dhs()`](https://ahadi-analytics.github.io/sntmethods/reference/calc_csb_by_wealth_dhs.md)
  for wealth-stratified survey-weighted estimates

- [`calc_csb_dhs()`](https://ahadi-analytics.github.io/sntmethods/reference/calc_csb_dhs.md)
  for standard survey-weighted estimates

## Examples

``` r
if (FALSE) { # \dontrun{
# Care-seeking by wealth for poorest quintile only
csb_poorest <- calc_csb_by_wealth_mbg(
  dhs_kr = kr_data,
  gps_data = gps_data,
  indicators = c("public", "private"),
  quintiles = 1
)

# Compare poorest vs richest quintiles
csb_comparison <- calc_csb_by_wealth_mbg(
  dhs_kr = kr_data,
  gps_data = gps_data,
  indicators = c("any", "public", "none"),
  quintiles = c(1, 5)
)
} # }
```
