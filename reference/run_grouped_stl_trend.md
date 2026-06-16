# Run STL decomposition and trend tests on grouped time series data

Performs STL decomposition, Mann-Kendall trend testing, and Sen's slope
estimation for multiple indicators within grouped time series data.

## Usage

``` r
run_grouped_stl_trend(
  data,
  group_col,
  date_col,
  indicators,
  freq = 12,
  normalize_fun = normalize_zscore,
  stl_window = "periodic"
)
```

## Arguments

- data:

  A data.frame containing the time series data.

- group_col:

  Character vector of grouping column names.

- date_col:

  Name of the date column.

- indicators:

  A list of indicator specifications. Each element must contain:

  - col: column name of the indicator

  - type: indicator type label

- freq:

  Number of observations per year. Default is 12.

- normalize_fun:

  Function used to normalize indicator values.

- stl_window:

  STL seasonal window. Default is "periodic".

## Value

A data.table containing STL components and trend statistics.

## Examples

``` r
if (FALSE) { # \dontrun{
res <- run_grouped_stl_trend(
  data = monthly_adm2_incid,
  group_col = c("adm1", "adm2"),
  date_col = "date",
  indicators = indicators
)
} # }
```
