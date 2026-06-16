# Prepare Care-Seeking Behavior Data for MBG Analysis

Prepares cluster-level care-seeking behavior data for MBG analysis.
Calculates proportions of febrile children who sought care at various
source types.

## Usage

``` r
calc_csb_mbg(
  dhs_kr,
  gps_data,
  indicators = c("any", "public", "private", "none"),
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

  Character vector of indicators to calculate. standardized sector
  breakdown:

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

- csb_priority_method:

  Character, one of "all" (default), "first", "public", or "private".
  Controls how overlapping care-seeking records are resolved so each
  individual is assigned to at most one sector:

  - "all": Keep WHO methodology; overlaps allowed (csb_public and
    csb_private can both be 1 for the same child).

  - "first": Take the first recurring h32 source visited per child
    (alphabetical h32 order: h32a, h32b, ..., h32x). Mutually exclusive.

  - "public": If child sought any public/CHW care, classify as public;
    otherwise private if any private; otherwise none.

  - "private": If child sought any private care, classify as private;
    otherwise public if any public; otherwise none.

  With non-"all" values, csb_public + csb_private + csb_none sums to
  100%.

- survey_vars:

  Named list mapping DHS variable names.

- gps_vars:

  Named list for GPS variable mapping.

## Value

A list of data.tables (one per indicator), each with columns:

- cluster_id: Cluster identifier

- indicator: Numerator count

- samplesize: Denominator count

- x: Longitude

- y: Latitude

## Details

Methodology:
<https://github.com/ahadi-analytics/sntmethods/blob/master/inst/methods/csb_dhs.yml>

This function uses KR data on children under 5 who had fever in the last
2 weeks. Care-seeking is determined using h32 variables.

Note: Care-seeking indicators (except "none") are NOT mutually
exclusive. A child can appear in both "public" and "private" if they
visited both.

## See also

[`calc_csb_dhs()`](https://ahadi-analytics.github.io/sntmethods/reference/calc_csb_dhs.md)
for survey-weighted estimates

## Examples

``` r
if (FALSE) { # \dontrun{
csb_mbg <- calc_csb_mbg(
  dhs_kr = kr_data,
  gps_data = gps_data,
  indicators = c("public", "none")
)
} # }
```
