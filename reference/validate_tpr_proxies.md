# Validate TPR Proxy Quality Using Leave-One-Out Cross-Validation

Evaluates the accuracy of TPR proxy estimates by comparing them against
actual facility-level TPR values. Uses leave-one-out cross-validation to
calculate what each proxy level (adm2, adm1, prev_year, rolling, adm0)
would have estimated for facility-months with valid raw TPR data.

## Usage

``` r
validate_tpr_proxies(
  data,
  hf_var = "hf_uid",
  adm0_var = "adm0",
  adm1_var = "adm1",
  adm2_var = "adm2",
  year_var = "year",
  month_var = "month",
  conf_var = "conf",
  test_var = "test",
  min_facilities = 2,
  generate_plots = TRUE
)
```

## Arguments

- data:

  Output from
  [`calc_tpr()`](https://ahadi-analytics.github.io/sntmethods/reference/calc_tpr.md)
  with `include_flags = TRUE`.

- hf_var:

  Column name for health facility unique identifier. Should match the
  value used in
  [`calc_tpr()`](https://ahadi-analytics.github.io/sntmethods/reference/calc_tpr.md).
  Default is "hf_uid".

- adm0_var:

  Column name for national/country level. Default is "adm0".

- adm1_var:

  Column name for first administrative level/region. Default is "adm1".

- adm2_var:

  Column name for second administrative level/district. Default is
  "adm2".

- year_var:

  Column name for year. Default is "year".

- month_var:

  Column name for month. Default is "month".

- conf_var:

  Column name for confirmed cases. Default is "conf".

- test_var:

  Column name for tests. Default is "test".

- min_facilities:

  Minimum number of facilities in an admin unit to include in
  validation. Default is 2.

- generate_plots:

  Logical; if TRUE (default), generates diagnostic plots. Set to FALSE
  for metrics only.

## Value

A list containing:

- `metrics`: Data frame with validation metrics for each proxy level

- `validation_data`: Data frame with actual vs proxy comparisons

- `plots`: List of ggplot objects (if generate_plots = TRUE):

  - `scatter`: Actual vs proxy scatterplots

  - `error_dist`: Error distribution by proxy level

  - `mae_by_tests`: MAE vs number of tests

  - `stability_map`: Error patterns by test count and TPR value

- `summary`: Character vector with key findings

## Examples

``` r
# result <- calc_tpr(facility_data, include_flags = TRUE)
# validation <- validate_tpr_proxies(result)
# validation$metrics
# validation$plots$scatter
```
