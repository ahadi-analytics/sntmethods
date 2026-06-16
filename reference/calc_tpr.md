# Calculate Test Positivity Rate from Routine Health Facility Data

Calculates malaria Test Positivity Rate (TPR) at the health
facility-month level and applies structured fallback logic to derive
proxy values when confirmed or tested data are missing. Input data must
be at the health facility-month level, with one observation per facility
per reporting month. Returns a validated TPR dataset with quality flags
and source tracking.

## Usage

``` r
calc_tpr(
  data,
  hf_var = "hf_uid",
  adm0_var = NULL,
  adm1_var = "adm1",
  adm2_var = "adm2",
  date_var = "date",
  conf_var = "conf",
  test_var = "test",
  extreme_threshold = c(0.01, 0.99),
  include_flags = FALSE,
  activity_indicators = c("conf", "test"),
  activity_method = 3,
  nonreport_window = 6,
  fallback_method = c("rolling", "adm2", "prev_year", "adm1", "adm0"),
  prev_year_window = 2,
  fallback_triggers = c("missing", "extreme", "low_test")
)
```

## Arguments

- data:

  Routine health facility data at the facility-month level (data.frame
  or tibble). Must contain one row per facility per month.

- hf_var:

  Column name for health facility unique identifier (default: "hf_uid").

- adm0_var:

  Column name for national/country level. If NULL (default), creates a
  single "country" value for all records.

- adm1_var:

  Column name for first administrative level/region (default: "adm1").

- adm2_var:

  Column name for second administrative level/district (default:
  "adm2").

- date_var:

  Column name for date of reporting period (default: "date").

- conf_var:

  Column name for number of confirmed malaria cases (default: "conf").

- test_var:

  Column name for number of individuals tested (default: "test").

- extreme_threshold:

  Numeric vector of length 2 specifying lower and upper bounds for
  flagging extreme TPR values (default: `c(0.01, 0.99)`).

- include_flags:

  Logical; if `TRUE`, includes all quality flag columns in the output.
  If `FALSE` (default), returns only the core TPR variables without
  flags.

- activity_indicators:

  Character vector of indicator columns used to determine facility
  activity. Default is `c("conf", "test")`.

- activity_method:

  Numeric. Classification method for facility activity (1, 2, or 3).
  Default is 3 (dynamic activation/inactivation). Used to flag inactive
  facility-months which are excluded from proxy calculations.

- nonreport_window:

  Integer. Number of consecutive non-reporting periods before a facility
  is considered inactive (for method 3). Default is 6.

- fallback_method:

  Character vector specifying which proxy fallback levels to use and
  their order. Valid options: "rolling" (3-month rolling average from
  same facility), "adm2" (district-level), "adm1" (regional-level),
  "prev_year" (same month previous year), "adm0" (national-level).
  Default is `c("rolling", "adm2", "prev_year", "adm1", "adm0")`.
  Proxies are applied sequentially in the order specified. Set to NULL
  to disable all fallbacks (raw TPR only).

- prev_year_window:

  Integer specifying the seasonal window in months for previous year
  fallback. 0 = exact month match only (default), 1 = +/-1 month window
  (3-month average), 2 = +/-2 months (5-month average), etc. Maximum
  value is 6. When window \> 0, averages all available months within the
  window from the previous year, weighted by test counts.

- fallback_triggers:

  Character vector specifying when to apply proxy fallbacks. Valid
  options: "missing" (conf/test is NA), "extreme" (TPR outside
  extreme_threshold), "low_test" (test \< 5). Default is
  `c("missing", "extreme", "low_test")`. Set to NULL to disable all
  triggers (keep raw TPR values without replacement). Note: impossible
  values (conf \> test) are always included in fallback along with
  missing values. Inactive facilities are always excluded.

## Value

A list containing three elements:

- `data`: A tibble with TPR estimates per facility-month including:

  - `hf_uid`: Health facility identifier

  - `adm0`: National/country level

  - `adm1`: First administrative level

  - `adm2`: Second administrative level

  - `date`: Standardised date (first of month)

  - `year`: Year extracted from date

  - `month`: Month extracted from date

  - `conf`: Confirmed cases

  - `test`: Number tested

  - `tpr`: Final validated or proxy TPR (0-1 scale)

  - `tpr_source`: Source of TPR value (facility_raw, proxy_adm2,
    proxy_adm1, proxy_prev_year, or proxy_adm0)

  - `flag_tpr_valid`: TRUE if raw TPR could be calculated

  - `flag_tpr_extreme`: TRUE if TPR outside extreme_threshold

  - `flag_tpr_proxy`: TRUE if proxy value was used

  - `flag_tpr_missing`: TRUE if no TPR could be assigned

  - `flag_conf_gt_test`: TRUE if conf \> test (impossible value)

  - `flag_zero_test`: TRUE if test == 0

  - `flag_low_test`: TRUE if test \< 5

  - `flag_missing_conf`: TRUE if conf is NA

  - `flag_inactive`: TRUE if facility-month is inactive

## Details

The default fallback hierarchy for proxy TPR values is:

1.  Rolling 3-month average from same facility (+/-1 month)

2.  District (adm2) TPR from same month

3.  Same month from previous year (facility-level)

4.  Regional (adm1) TPR from same month

5.  National (adm0) TPR from same month

## Reporting Rates

This function does not calculate reporting rates. If you need reporting
rates for analysis or filtering, calculate them separately using
[`sntutils::calculate_reporting_metrics()`](https://ahadi-analytics.github.io/sntutils/reference/calculate_reporting_metrics.html)
and join to your data before calling `calc_tpr()`.

Example:


    # Calculate reporting rate at district level
    reprate_data <- sntutils::calculate_reporting_metrics(
      data = facility_data,
      vars_of_interest = "conf",
      x_var = "date",
      y_var = "adm2",
      hf_col = "hf_uid",
      key_indicators = c("conf", "test")
    )

    # Join back to facility data (include all admin levels for proper join)
    data_with_reprate <- facility_data |>
      dplyr::left_join(
        reprate_data |> dplyr::select(adm0, adm1, adm2, date, reprate),
        by = c("adm0", "adm1", "adm2", "date")
      )

    # Now filter or use reprate in your analysis
    tpr_result <- calc_tpr(data_with_reprate) |>
      dplyr::filter(reprate >= 0.8)

## Examples

``` r
# Example with minimal data
# facility_data <- tibble::tibble(
#   hf_uid = c("HF001", "HF002", "HF003"),
#   adm1 = rep("RegionA", 3),
#   adm2 = rep("DistrictX", 3),
#   date = as.Date(c("2023-01-01", "2023-02-01", "2023-01-01")),
#   conf = c(10, 15, NA),
#   test = c(100, 120, 80),
#   report_complete = c(1, 1, 1)
# )
#
# result <- calc_tpr(facility_data)
# tpr_data <- result$data
# tpr_dict <- result$dict
# result$metadata
```
