# Calculate N3 (Care-Seeking-Adjusted Incidence) - Internal

Calculates N3 by applying the CSB adjustment to N2 cases.

## Usage

``` r
.calc_n3_internal(df, scale_factor)
```

## Arguments

- df:

  Data frame with standardized column names at admin-month level

- scale_factor:

  Denominator for incidence rate

## Value

Data frame with n3_cases and n3_incidence columns added

## Details

N3 = N2 \* (1 + CS_Priv/CS_Pub + CS_None/CS_Pub)
