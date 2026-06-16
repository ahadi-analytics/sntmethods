# Prepare ANC Data for MBG Analysis

Prepares cluster-level Antenatal Care (ANC) attendance data for
Model-Based Geostatistics (MBG) analysis. Calculates the proportion of
women who had at least N ANC visits during their most recent pregnancy.

## Usage

``` r
calc_anc_mbg(
  dhs_ir,
  gps_data,
  indicators = c("anc_1plus", "anc_2plus", "anc_3plus", "anc_4plus"),
  birth_window_months = 36,
  survey_vars = list(cluster = "v001", interview_date = "v008", birth_date = "b3_01",
    anc_visits = "m14_1"),
  gps_vars = list(cluster = "DHSCLUST", lat = "LATNUM", lon = "LONGNUM")
)
```

## Arguments

- dhs_ir:

  DHS Individual Recode dataset.

- gps_data:

  DHS GPS dataset with cluster coordinates.

- indicators:

  Character vector of indicators to calculate:

  - "anc_1plus": At least 1 ANC visit

  - "anc_2plus": At least 2 ANC visits

  - "anc_3plus": At least 3 ANC visits

  - "anc_4plus": At least 4 ANC visits

  - "anc_8plus": At least 8 ANC visits (2016 WHO recommendation)

  Default: c("anc_1plus", "anc_2plus", "anc_3plus", "anc_4plus").

- birth_window_months:

  Number of months to look back for births. Default: 36 (3 years). Max
  60 (5 years).

- survey_vars:

  Named list mapping DHS variable names.

- gps_vars:

  Named list for GPS variable mapping.

## Value

A list of data.tables (one per indicator), each with columns:

- cluster_id: Cluster identifier

- indicator: Number of women meeting threshold

- samplesize: Total number of women with recent births

- x: Longitude

- y: Latitude

## Details

Methodology:
<https://github.com/ahadi-analytics/sntmethods/blob/master/inst/methods/anc_dhs.yml>

This function uses data on most recent births within the specified
window. ANC visits are measured using m14 (number of antenatal visits).

## Examples

``` r
if (FALSE) { # \dontrun{
anc_mbg <- calc_anc_mbg(
  dhs_ir = ir_data,
  gps_data = gps_data,
  indicators = c("anc_1plus", "anc_4plus")
)
} # }
```
