# Aggregate Facility Data to Annual Level - Internal

Aggregates facility-month data to facility-year level.

## Usage

``` r
.aggregate_facility_to_annual(
  df,
  scale_factor,
  cs_none_divisor = 2,
  group_by = NULL
)
```

## Arguments

- df:

  Data frame with facility-month level data

- scale_factor:

  Numeric. Scale factor for incidence calculation

- cs_none_divisor:

  Numeric. Divisor for cs_none in N5 calculation

## Value

Aggregated tibble at facility-year level
