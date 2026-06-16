# Build Final ADM2 Dataset

Combines multiple indicator estimates into a single ADM2-level dataset.

## Usage

``` r
build_final_dataset(
  adm2_sf,
  adm1_estimates = NULL,
  mbg_estimates = NULL,
  survey_dates = NULL,
  survey_year = NULL
)
```

## Arguments

- adm2_sf:

  sf object with ADM2 boundaries.

- adm1_estimates:

  Named list of ADM1 survey-weighted estimates.

- mbg_estimates:

  Named list of MBG ADM2 predictions.

- survey_dates:

  Data frame with survey date information.

- survey_year:

  Survey year.

## Value

Data frame with one row per ADM2 and columns for each indicator.
