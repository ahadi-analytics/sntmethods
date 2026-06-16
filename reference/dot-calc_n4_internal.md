# Calculate N4 (Public + Non-Seekers Adjusted Incidence) - Internal

Calculates N4 by applying only the non-seeker adjustment (excludes
private sector).

## Usage

``` r
.calc_n4_internal(df, scale_factor)
```

## Arguments

- df:

  Data frame with standardized column names at admin-month level

- scale_factor:

  Denominator for incidence rate

## Value

Data frame with n4_cases and n4_incidence columns added

## Details

N4 = N2 \* (1 + CS_None/CS_Pub)
