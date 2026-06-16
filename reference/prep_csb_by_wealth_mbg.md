# Prepare Single CSB Indicator by Wealth Quintile for MBG

Convenience wrapper around
[`calc_csb_by_wealth_mbg()`](https://ahadi-analytics.github.io/sntmethods/reference/calc_csb_by_wealth_mbg.md)
to prepare a single care-seeking indicator stratified by wealth
quintile.

## Usage

``` r
prep_csb_by_wealth_mbg(
  dhs_kr,
  gps_data,
  indicator = "public",
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

- indicator:

  Single indicator name. Default: "public".

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

Named list of data.tables, one per quintile, each with columns:
cluster_id, indicator, samplesize, x, y

## Examples

``` r
if (FALSE) { # \dontrun{
# Public care-seeking for poorest quintile only
csb_pub_q1 <- prep_csb_by_wealth_mbg(
  dhs_kr = kr_data,
  gps_data = gps_data,
  indicator = "public",
  quintiles = 1
)
} # }
```
