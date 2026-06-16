# Aggregate Incidence Data to Specified Admin Level - Internal

Aggregates monthly incidence data to a specified administrative level
(adm0, adm1, adm2, or adm3).

## Usage

``` r
.aggregate_to_level(
  df,
  admin_level,
  scale_factor,
  time_period = "monthly",
  cs_none_divisor = 2,
  group_by = NULL
)
```

## Arguments

- df:

  Data frame with monthly incidence data

- admin_level:

  Character. One of "adm0", "adm1", "adm2", "adm3"

- scale_factor:

  Numeric. Scale factor for incidence calculation

- time_period:

  Character. Either "monthly" or "annual"

- cs_none_divisor:

  Numeric. Divisor for cs_none in N5 calculation

## Value

Aggregated tibble at specified admin level
