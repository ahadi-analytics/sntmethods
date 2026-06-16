# Calculate N5 (Conservative Non-Seekers Adjusted Incidence) - Internal

Calculates N5 by applying a reduced non-seeker adjustment (excludes
private sector and divides cs_none by a configurable divisor).

## Usage

``` r
.calc_n5_internal(df, scale_factor, cs_none_divisor = 2)
```

## Arguments

- df:

  Data frame with standardized column names at admin-month level

- scale_factor:

  Denominator for incidence rate

- cs_none_divisor:

  Numeric divisor for cs_none (default: 2)

## Value

Data frame with n5_cases and n5_incidence columns added

## Details

N5 = N2 \* (1 + (CS_None / cs_none_divisor) / CS_Pub)
