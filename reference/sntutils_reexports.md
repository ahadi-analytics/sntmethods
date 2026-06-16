# Re-exports from sntutils

These functions are imported from sntutils and re-exported here for
convenience in SNT Methods workflows.

## Usage

``` r
calculate_reporting_metrics(
  data,
  vars_of_interest,
  x_var,
  y_var = NULL,
  hf_col = NULL,
  key_indicators = c("allout", "conf", "test", "treat", "pres"),
  method = 3,
  nonreport_window = 6,
  reporting_rule = "any_non_na",
  require_all = FALSE,
  weighting = FALSE,
  weight_var = NULL,
  weight_window = 12,
  exclude_current_x = TRUE,
  cold_start = "median_within_y"
)

classify_facility_activity(
  data,
  hf_col,
  date_col = "date",
  key_indicators = c("test", "pres", "conf"),
  method = 1,
  nonreport_window = 6,
  reporting_rule = "any_non_na",
  binary_classification = FALSE
)

detect_outliers(
  data,
  column,
  record_id = "record_id",
  admin_level = c("adm1", "adm2"),
  spatial_level = "hf_uid",
  date = "date",
  time_mode = c("across_time", "within_year", "seasonal"),
  value_type = c("count", "rate"),
  strictness = c("balanced", "lenient", "strict", "advanced"),
  methods = c("iqr", "median", "mean", "consensus"),
  sd_multiplier = 3,
  mad_constant = 1.4826,
  mad_multiplier = 9,
  iqr_multiplier = 2,
  consensus_rule = 3,
  output_profile = c("standard", "lean", "audit"),
  min_n = 8,
  reporting_rate_col = NULL,
  reporting_rate_min = 0.5,
  key_indicators_hf = NULL,
  classify_outbreaks = FALSE,
  outbreak_min_run = 2,
  outbreak_prop_tolerance = 0.9,
  outbreak_max_gap = 12,
  verbose = TRUE
)

get_active_facilities(
  data,
  hf_col,
  date_col = "date",
  key_indicators = c("allout", "conf", "test", "treat", "pres"),
  method = 3,
  nonreport_window = 6,
  reporting_rule = "any_non_na",
  return_summary = FALSE
)
```

## Arguments

- data:

  A data frame containing health facility data.

- vars_of_interest:

  Character vector of variable names to assess reporting (used for
  numerator).

- x_var:

  Character. Name of the primary grouping variable (e.g., time period).

- y_var:

  Character. Optional. Name of the second grouping variable (e.g.,
  district).

- hf_col:

  Character. Optional (defaults to NULL). Name of the column containing
  unique health facility IDs. When provided, enables facility-level
  analysis and filtering of inactive facilities (if key_indicators are
  specified). Can be used with or without y_var. Required when weighting
  = TRUE.

- key_indicators:

  Optional. Character vector of indicators used to define facility
  activity in scenario 1. Defaults to
  `c("allout", "conf", "test", "treat", "pres")`.

- method:

  Character or numeric. Classification method for facility activity
  status. Can be numeric (1, 2, 3) or character ("method1", "method2",
  "method3"). Defaults to 3. See
  [`classify_facility_activity`](https://ahadi-analytics.github.io/sntutils/reference/classify_facility_activity.html)
  for details.

- nonreport_window:

  Integer. Minimum number of consecutive non-reporting months to
  classify a facility as inactive in method 3. Defaults to 6.

- reporting_rule:

  Character. Defines what counts as reporting: `"any_non_na"` (default,
  counts NA as non-reporting, 0 counts as reported) or `"positive_only"`
  (requires \>0 value to count as reported).

- require_all:

  Logical. When TRUE and multiple vars_of_interest are provided,
  calculates the proportion of facilities reporting ALL variables
  (complete data). When FALSE (default), calculates per-variable
  reporting rates. Only applies to facility-level analysis (when hf_col
  is provided).

- weighting:

  Logical. Whether to use weighted reporting rates. When TRUE,
  facilities are weighted by their typical size, giving more importance
  to larger facilities in the overall reporting rate calculation. This
  provides a volume-adjusted measure of data completeness. Default is
  FALSE.

- weight_var:

  Character. Name of the variable to use as proxy for facility size
  (e.g., "allout" for total outpatients, "test" for tests done). This
  should be a count variable that reflects facility activity/size. If
  NULL and weighting is TRUE, will auto-select from allout, test, conf
  (in that order).

- weight_window:

  Integer. Number of periods for rolling window to calculate typical
  facility size. A facility's weight is based on its average size over
  the past weight_window periods. Larger windows provide more stable
  weights but may miss recent changes. Default is 12.

- exclude_current_x:

  Logical. Whether to exclude current period when calculating weights.
  If TRUE, prevents current reporting from influencing its own weight
  (avoids circularity). Default is TRUE.

- cold_start:

  Character. Method for handling facilities with insufficient history
  (\< weight_window periods). Options:

  - "median_within_y" (default): Uses median size of facilities within
    the same y_var group (e.g., same district)

  - "median_global": Uses median size across all facilities

- date_col:

  Character. Column storing observation dates. Defaults to "date".

- binary_classification:

  Logical. If TRUE, collapses categories into "Active" vs "Inactive".
  Defaults to FALSE.

- column:

  Name of the numeric column to evaluate.

- record_id:

  Unique record identifier column.

- admin_level:

  Character vector of administrative level columns for parallel
  grouping, ordered from higher to lower resolution. Defaults to
  `c("adm1", "adm2")`.

- spatial_level:

  Character string specifying the finest spatial unit for analysis
  (e.g., "hf_uid" for facility-level). When specified, `admin_level`
  defines grouping boundaries while `spatial_level` defines the unit of
  analysis. This prevents excessive grouping while maintaining spatial
  granularity. Default is `hf_uid`.

- date:

  Date column (Date, POSIXt, or parseable character string). Year,
  month, and yearmon are automatically derived from this column.

- time_mode:

  Pooling strategy: `"across_time"`, `"within_year"`, or `"seasonal"`.
  Seasonal mode groups by month across all years (e.g., all Januaries
  together), useful for detecting values that are unusual for a specific
  month regardless of year.

- value_type:

  Indicator type: `"count"` or `"rate"`. Counts floor lower bounds at 0.

- strictness:

  Strictness preset: `"lenient"`, `"balanced"`, `"strict"`, or
  `"advanced"`. Presets map to method multipliers. If not `"advanced"`,
  any manual multipliers are **ignored**.

- methods:

  Character vector specifying which outlier detection methods to use:
  "iqr" (Interquartile Range), "median" (Median Absolute Deviation),
  "mean" (Mean +/- SD), and/or "consensus". Default is
  `c("iqr", "median", "mean", "consensus")`. For consensus, at least two
  other methods must be selected.

- sd_multiplier:

  Width (in SD units) for the mean method (used only when
  `strictness = "advanced"`).

- mad_constant:

  Constant passed to [`stats::mad()`](https://rdrr.io/r/stats/mad.html)
  in advanced mode (default 1.4826).

- mad_multiplier:

  Width multiplier for the MAD method (advanced mode).

- iqr_multiplier:

  Tukey fence multiplier for the IQR method (advanced mode).

- consensus_rule:

  Number of methods that must agree (`1`, `2`, or `3`) for the consensus
  flag to call an outlier. Default `2`.

- output_profile:

  Controls the amount of detail returned: `"lean"` (minimal columns: id,
  admin, date, value, consensus flag, reason), `"standard"` (lean +
  per-method flags + bounds + seasonality mode), `"audit"` (all columns
  for full reproducibility). Default `"standard"`.

- min_n:

  Minimum observations required in the active comparison bucket before
  flagging is attempted (applies to any seasonal bucket or fallback).

- reporting_rate_col:

  Optional column with reporting completeness in `[0, 1]`.

- reporting_rate_min:

  Minimum acceptable reporting rate. Rows below the threshold receive
  `reason = "low_reporting"` and are not flagged.

- key_indicators_hf:

  Optional character vector of indicator names used to determine
  facility activeness. If supplied, the function uses a fast path to
  filter out inactive facility-months. A facility-month is considered
  active if ANY of the specified key indicators have non-NA values.
  Inactive facility-months are excluded from outlier detection. If
  `NULL` (default), activeness filtering is skipped. Typical indicators
  include `"allout"`, `"test"`, or `"conf"`. This adjustment prevents
  false positives caused by facilities that start or stop reporting
  mid-period.

- classify_outbreaks:

  Logical. When `TRUE` (default), applies outbreak classification to
  distinguish between isolated outliers and sustained outbreak patterns.
  Consecutive outliers meeting the outbreak criteria are reclassified
  from "outlier" to "outbreak". This is particularly useful for
  epidemiological surveillance to identify disease outbreak patterns.
  Set to `FALSE` to disable outbreak classification.

- outbreak_min_run:

  Integer. Minimum number of consecutive outliers required to classify
  as an outbreak (default `2`). Must be \>= 2.

- outbreak_prop_tolerance:

  Numeric. Proportional tolerance for outbreak consistency (default
  `0.9`). Values within this tolerance of the run median are considered
  consistent. Range: (0, 1).

- outbreak_max_gap:

  Integer. Maximum allowed gap (non-outlier months) between outliers
  that can still be considered part of the same outbreak (default `1`).
  For example, with `outbreak_max_gap = 12`, the pattern
  "outlier-normal-outlier-outlier" would be classified as one outbreak
  of length 3, rather than separate incidents. Set to `0` for strict
  consecutive-only outbreaks. Useful for real-world data with reporting
  gaps.

- verbose:

  Logical. When `TRUE`, prints an informative summary showing which
  methods are being applied, the pooling strategy, strictness settings,
  guardrails, and consensus rule. Default is `FALSE`.

- return_summary:

  Logical. If TRUE, returns a summary tibble instead of filtered data.
  Default is FALSE
