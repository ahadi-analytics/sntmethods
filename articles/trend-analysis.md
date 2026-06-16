# Trend analysis

[`run_grouped_stl_trend()`](https://ahadi-analytics.github.io/sntmethods/reference/run_grouped_stl_trend.md)
detects trends in grouped monthly time series - for example incidence
and TPR by district. For each group and indicator it runs an **STL
decomposition** (seasonal-trend decomposition using loess) to separate
seasonality from the underlying trend, then tests that trend with the
**Mann-Kendall** non-parametric test and estimates its magnitude with
**Sen’s slope**.

This is a natural follow-on to the
[routine-data](https://ahadi-analytics.github.io/sntmethods/articles/routine-incidence.md)
workflow: feed it the monthly incidence/TPR series you produced there.

## Usage

``` r

library(sntmethods)

trends <- run_grouped_stl_trend(
  data      = monthly_district_data,
  group_col = c("adm1", "adm2"),
  date_col  = "date",
  indicators = list(
    list(col = "n2_incidence", type = "incidence"),
    list(col = "tpr",          type = "positivity")
  )
)
```

- `group_col` - one or more columns that define each series (e.g. admin
  units).
- `date_col` - the monthly date column.
- `indicators` - a list of indicators to analyse, each naming the value
  column and its `type` (used for sensible defaults and labelling).

## What you get back

For each group × indicator, the result reports the decomposed components
plus the trend test:

- the **Mann-Kendall** statistic and p-value (is there a monotonic
  trend?),
- **Sen’s slope** (how steep, robust to outliers),
- and the direction/significance classification you can map or tabulate.

Because the output is tidy and keyed by your grouping columns, it joins
straight back onto admin geometries for trend maps, or into a table of
which districts are improving or worsening.

## See also

- [Routine data: incidence &
  TPR](https://ahadi-analytics.github.io/sntmethods/articles/routine-incidence.md)
  to produce the series.
- [Reference](https://ahadi-analytics.github.io/sntmethods/reference/index.html)
  for
  [`run_grouped_stl_trend()`](https://ahadi-analytics.github.io/sntmethods/reference/run_grouped_stl_trend.md).
  \`\`\`
