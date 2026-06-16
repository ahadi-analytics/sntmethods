# Aggregate Facility-Month Data to Admin-Month Level - Internal

Aggregates facility-month level data to administrative unit-month level.
This is used as the first step in the incidence cascade to produce
output at the admin-month level rather than facility-month level.

## Usage

``` r
.aggregate_to_admin_month(df, group_by = NULL)
```

## Arguments

- df:

  Data frame with standardized column names at facility-month level

## Value

Data frame aggregated to admin-month level with:

- Sum: pop, conf, test, pres

- Recalculated: tpr = sum(conf)/sum(test)

- Mean: reprate

- First: cs_public, cs_private, cs_none

- Diagnostics: n_facilities, n_facilities_with_conf,
  n_facilities_with_tpr
