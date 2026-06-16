# Prepare SMC Data for MBG Analysis

Prepares cluster-level Seasonal Malaria Chemoprevention (SMC) receipt
data for MBG analysis. SMC coverage among children under 5.

## Usage

``` r
calc_smc_mbg(
  dhs_kr,
  gps_data,
  survey_vars = list(cluster = "v001", age = "hw1", smc_primary = "hml43", smc_alt =
    "ml13g"),
  gps_vars = list(cluster = "DHSCLUST", lat = "LATNUM", lon = "LONGNUM")
)
```

## Arguments

- dhs_kr:

  DHS Children's Recode (KR) dataset.

- gps_data:

  DHS GPS dataset with cluster coordinates.

- survey_vars:

  Named list mapping DHS variable names.

- gps_vars:

  Named list for GPS variable mapping.

## Value

A data.table with columns:

- cluster_id: Cluster identifier

- indicator: Number of children who received SMC

- samplesize: Total number of children in analysis

- x: Longitude

- y: Latitude

## Details

Methodology:
<https://github.com/ahadi-analytics/sntmethods/blob/master/inst/methods/smc_dhs.yml>

SMC variable availability varies by survey. Common variables include:

- hml43: SMC in malaria season (DHS-7+)

- ml13g: Received antimalarial for prevention

This function first checks which SMC-related variables are available and
uses the most appropriate one.

## Examples

``` r
if (FALSE) { # \dontrun{
smc_mbg <- calc_smc_mbg(
  dhs_kr = kr_data,
  gps_data = gps_data
)
} # }
```
