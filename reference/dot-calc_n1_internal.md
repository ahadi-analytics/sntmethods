# Calculate N1 (Testing-Adjusted Incidence) - Internal

N1 = conf + (pres \* tpr)

## Usage

``` r
.calc_n1_internal(df, scale_factor)
```

## Arguments

- df:

  Data frame with standardized column names

- scale_factor:

  Denominator for incidence rate

## Value

Data frame with n1_cases and n1_incidence columns added

## Details

When n1_cases already exists (from facility-level aggregation), it is
used directly to calculate incidence. Otherwise, N1 cases are calculated
from the aggregated values (legacy/fallback behavior).
