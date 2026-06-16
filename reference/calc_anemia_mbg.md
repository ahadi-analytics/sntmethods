# Prepare Anemia Data for MBG Analysis

Prepares cluster-level anemia prevalence data for MBG analysis. Supports
multiple severity thresholds (mild, moderate, severe) and both
cumulative and exclusive categories.

## Usage

``` r
calc_anemia_mbg(
  dhs_pr,
  gps_data,
  indicators = c("any", "moderate_plus", "severe"),
  age_min = 6,
  age_max = 59,
  survey_vars = list(cluster = "hv001", age = "hc1", present = "hv103", mother = "hv042",
    hemoglobin = "hc56"),
  gps_vars = list(cluster = "DHSCLUST", lat = "LATNUM", lon = "LONGNUM")
)
```

## Arguments

- dhs_pr:

  DHS Person Records dataset.

- gps_data:

  DHS GPS dataset with cluster coordinates.

- indicators:

  Character vector of indicators to calculate:

  - Cumulative (child has hemoglobin below threshold):

    - "any": Any anemia (Hb \< 11 g/dL)

    - "moderate_plus": Moderate or severe (Hb \< 10 g/dL)

    - "severe": Severe only (Hb \< 8 g/dL)

  - Exclusive (child is in exactly this category):

    - "mild_only": Mild only (10 \<= Hb \< 11)

    - "moderate_only": Moderate only (8 \<= Hb \< 10)

    - "severe_only": Same as severe (Hb \< 8)

  Default: c("any", "moderate_plus", "severe").

- age_min:

  Minimum age in months (default: 6).

- age_max:

  Maximum age in months (default: 59).

- survey_vars:

  Named list mapping DHS variable names.

- gps_vars:

  Named list for GPS variable mapping.

## Value

A list of data.tables (one per indicator), each with columns:

- cluster_id: Cluster identifier

- indicator: Number with anemia at that threshold

- samplesize: Total number of children tested

- x: Longitude

- y: Latitude

## Details

Methodology:
<https://github.com/ahadi-analytics/sntmethods/blob/master/inst/methods/anemia_dhs.yml>

Anemia thresholds follow WHO definitions for children 6-59 months:

- Mild: 10.0-10.9 g/dL

- Moderate: 7.0-9.9 g/dL

- Severe: \< 7.0 g/dL

Note: DHS uses slightly different cutoffs. This function uses:

- Any anemia: \< 11 g/dL

- Moderate+: \< 10 g/dL

- Severe: \< 8 g/dL

Hemoglobin values in DHS are altitude-adjusted and stored in g/dL \* 10.

## See also

[`calc_severe_anemia_dhs()`](https://ahadi-analytics.github.io/sntmethods/reference/calc_severe_anemia_dhs.md)
for survey-weighted estimates

## Examples

``` r
if (FALSE) { # \dontrun{
anemia_mbg <- calc_anemia_mbg(
  dhs_pr = pr_data,
  gps_data = gps_data,
  indicators = c("any", "severe")
)
} # }
```
