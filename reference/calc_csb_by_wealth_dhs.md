# Calculate Care-Seeking Behavior by Wealth Quintile from DHS Data

Estimates care-seeking behavior for febrile children under 5, stratified
by wealth quintile. Uses WHO World Malaria Report methodology with
survey-weighted estimates.

## Usage

``` r
calc_csb_by_wealth_dhs(
  dhs_kr,
  survey_vars = list(cluster = "v021", weight = "v005", stratum = "v022", age = "hw1",
    fever = "h22", alive = "b5"),
  quintiles = 1:5,
  wealth_var = "v190",
  csb_priority_method = c("all", "first", "public", "private"),
  region_var = NULL,
  ci_method = "logit"
)
```

## Arguments

- dhs_kr:

  DHS children's recode (KR) dataset in tidy format.

- survey_vars:

  Named list mapping DHS variable names. See
  [`calc_csb_dhs_core()`](https://ahadi-analytics.github.io/sntmethods/reference/calc_csb_dhs_core.md)
  for details.

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

- region_var:

  Optional column name for regional grouping.

- ci_method:

  Method for confidence intervals. Default: "logit".

## Value

Named list of tibbles with two levels:

- `adm0`:

  National-level estimates by wealth quintile (always present)

- `adm1`:

  Admin-1 estimates by wealth quintile (when region_var provided)

Each tibble contains columns: survey_id, iso3, iso2, survey_type,
survey_year, adm0, adm1 (if applicable), wealth_quintile, type,
geo_source, point, ci_l, ci_u, numerator, denominator, indicator,
indicator_code, numerator_description, denominator_description,
denominator_code.

## Details

This function extends
[`calc_csb_dhs()`](https://ahadi-analytics.github.io/sntmethods/reference/calc_csb_dhs.md)
to provide wealth-stratified estimates. Each wealth quintile produces
separate survey-weighted estimates with confidence intervals.

Methodology:
<https://github.com/ahadi-analytics/sntmethods/blob/master/inst/methods/csb_dhs.yml>

## Indicators

The function calculates these indicators (overlapping, not mutually
exclusive):

- `csb_any`: Sought care anywhere

- `csb_public`: Public sector (including CHW)

- `csb_pub_nochw`: Public sector excluding CHW

- `csb_chw`: Community health worker

- `csb_private`: Any private sector

- `csb_priv_formal`: Private formal sector

- `csb_pharmacy`: Pharmacy/drug shop

- `csb_priv_informal`: Private informal

- `csb_priv_form_pha`: Private formal or pharmacy

- `csb_trained`: Trained provider

- `csb_none`: Did not seek care

## See also

- [`calc_csb_by_wealth_mbg()`](https://ahadi-analytics.github.io/sntmethods/reference/calc_csb_by_wealth_mbg.md)
  for wealth-stratified MBG cluster data

- [`calc_csb_dhs()`](https://ahadi-analytics.github.io/sntmethods/reference/calc_csb_dhs.md)
  for standard survey-weighted estimates

- [`calc_csb_mbg()`](https://ahadi-analytics.github.io/sntmethods/reference/calc_csb_mbg.md)
  for non-stratified MBG data

## Examples

``` r
if (FALSE) { # \dontrun{
# Care-seeking for poorest quintile only
csb_poorest <- calc_csb_by_wealth_dhs(
  dhs_kr = kr_data,
  quintiles = 1
)

# Compare all quintiles nationally
csb_all <- calc_csb_by_wealth_dhs(
  dhs_kr = kr_data,
  quintiles = 1:5
)

# Regional estimates for poorest and richest
csb_regional <- calc_csb_by_wealth_dhs(
  dhs_kr = kr_data,
  quintiles = c(1, 5),
  region_var = "v024"
)
} # }
```
