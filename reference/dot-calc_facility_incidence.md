# Calculate Incidence at Facility Level - Internal

Calculates N0-N5 incidence at facility-month level.

## Usage

``` r
.calc_facility_incidence(df, levels, scale_factor, cs_none_divisor = 2)
```

## Arguments

- df:

  Data frame with facility-month level data

- levels:

  Character vector of requested incidence levels

- scale_factor:

  Numeric. Scale factor for incidence calculation

- cs_none_divisor:

  Numeric. Divisor for cs_none in N5 calculation

## Value

Data frame with facility-level incidence calculations
