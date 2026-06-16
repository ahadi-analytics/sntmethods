# Prepare U5MR Data for MBG Analysis

Prepares cluster-level Under-5 Mortality Rate (U5MR) data for MBG
analysis using
[`DHS.rates::chmort()`](https://rdrr.io/pkg/DHS.rates/man/chmort.html)
for the mortality calculation. This follows the standard DHS
synthetic-cohort life-table methodology with 8 age segments.

## Usage

``` r
calc_u5mr_mbg(
  dhs_br,
  gps_data,
  period_years = 5,
  survey_vars = list(cluster = "v001", interview_date = "v008", birth_date = "b3",
    age_at_death = "b7"),
  gps_vars = list(cluster = "DHSCLUST", lat = "LATNUM", lon = "LONGNUM")
)
```

## Arguments

- dhs_br:

  DHS Birth Recode (BR) dataset. Must contain the standard DHS variables
  needed by
  [`DHS.rates::chmort()`](https://rdrr.io/pkg/DHS.rates/man/chmort.html):
  cluster ID (v001), interview date in CMC (v008), child's date of birth
  in CMC (b3), and child's age at death in months (b7).

- gps_data:

  DHS GPS dataset with cluster coordinates.

- period_years:

  Number of years to look back for the mortality reference period.
  Default: 5 (standard DHS 5-year window). Passed to
  [`DHS.rates::chmort()`](https://rdrr.io/pkg/DHS.rates/man/chmort.html)
  as `Period = period_years * 12`.

- survey_vars:

  Named list mapping DHS variable names. Keys:

  - cluster: Cluster ID variable (default: "v001")

  - interview_date: Date of interview in CMC (default: "v008")

  - birth_date: Child's date of birth in CMC (default: "b3")

  - age_at_death: Child's age at death in months (default: "b7")

- gps_vars:

  Named list for GPS variable mapping.

## Value

A list with one data.table named "u5mr" containing:

- cluster_id: Cluster identifier

- indicator: Estimated number of deaths (derived from U5MR and exposure)

- samplesize: Number of births exposed in the 0-60 month window

- x: Longitude

- y: Latitude

- u5mr: U5MR per 1,000 live births

Returns NULL if required variables are missing or data is insufficient.

## Details

Methodology:
<https://github.com/ahadi-analytics/sntmethods/blob/master/inst/methods/u5mr_dhs.yml>

[`DHS.rates::chmort()`](https://rdrr.io/pkg/DHS.rates/man/chmort.html)
computes childhood mortality rates (NNMR, PNNMR, IMR, CMR, U5MR) using
the standard DHS synthetic-cohort approach. When called with
`Class = "v001"` (cluster ID), it produces per-cluster U5MR estimates.
The function uses 8 age segments (0-1, 1-3, 3-6, 6-12, 12-24, 24-36,
36-48, 48-60 months) and applies partial-exposure weighting at period
boundaries.

Because `chmort()` internally computes a design effect (DEFT) via
[`survey::svydesign()`](https://rdrr.io/pkg/survey/man/svydesign.html),
and per-cluster subsets contain only a single PSU, this function creates
synthetic PSU and strata columns with two pseudo-PSUs per cluster.
Uniform weights are applied so that the cluster-level rates are
unweighted – appropriate for MBG, which handles spatial smoothing and
uncertainty internally.

**Important:** The `indicator` and `samplesize` columns are used by MBG
to model the death proportion (indicator/samplesize). The MBG pipeline
automatically converts model outputs to "per 1,000" units to match
epidemiological standards and the scale of the `u5mr` column.

## See also

[`calc_u5mr_dhs()`](https://ahadi-analytics.github.io/sntmethods/reference/calc_u5mr_dhs.md)
for survey-weighted estimates

## Examples

``` r
if (FALSE) { # \dontrun{
u5mr_mbg <- calc_u5mr_mbg(
  dhs_br = br_data,
  gps_data = gps_data
)
# Access combined U5MR
u5mr_mbg$u5mr
} # }
```
