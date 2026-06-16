# Prepare Single CSB Indicator for MBG

Prepare Single CSB Indicator for MBG

## Usage

``` r
prep_csb_mbg(
  dhs_kr,
  gps_data,
  indicator = "public",
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

A data.table with columns: cluster_id, indicator, samplesize, x, y
