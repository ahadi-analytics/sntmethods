# Plot Method for SNT Incidence Objects

Creates a time series plot of incidence rates by level. If multiple
administrative units are present, will facet by adm2 (up to 12 units).

## Usage

``` r
# S3 method for class 'snt_incidence'
plot(x, level = NULL, by = "adm2", max_facets = 12, ...)
```

## Arguments

- x:

  An object of class `snt_incidence`.

- level:

  Character. Which incidence level to plot (default: highest available).

- by:

  Character. Grouping variable for plot (default: "adm2"). Set to NULL
  for aggregate plot.

- max_facets:

  Numeric. Maximum number of facets to display (default: 12).

- ...:

  Additional arguments passed to ggplot2 functions.

## Value

A ggplot2 object.
