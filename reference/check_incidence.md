# Check Incidence Trends

Creates diagnostic plots to visualize incidence trends across the
cascade levels (N0-N5) over time, faceted by location (adm1 ~ adm2).

## Usage

``` r
check_incidence(incidence_output, ncol = 4)
```

## Arguments

- incidence_output:

  Output from
  [`calc_incidence()`](https://ahadi-analytics.github.io/sntmethods/reference/calc_incidence.md)
  containing monthly and annual aggregations at different admin levels.

- ncol:

  Integer. Number of columns for facet layout. Default is 4. All
  locations are always shown; this controls the grid layout.

## Value

A list containing:

- monthly_plot:

  ggplot2 object showing monthly incidence (N0-N5) by date

- annual_plot:

  ggplot2 object showing annual incidence (N0-N5) by year

## Examples

``` r
# result <- calc_incidence(data)
# plots <- check_incidence(result)
# plots$monthly_plot
# plots$annual_plot
```
