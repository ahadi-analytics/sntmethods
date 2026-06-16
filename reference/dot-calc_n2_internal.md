# Calculate N2 (Reporting-Adjusted Incidence) - Internal

N2 = N1 / reprate

## Usage

``` r
.calc_n2_internal(df, scale_factor)
```

## Arguments

- df:

  Data frame with standardized column names

- scale_factor:

  Denominator for incidence rate

## Value

Data frame with n2_cases and n2_incidence columns added
