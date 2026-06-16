# Normalize a numeric vector using z-score standardization

standardizes a numeric vector by subtracting the mean and dividing by
the standard deviation. this is intended for preparing time series prior
to trend analysis such as mann-kendall or sen's slope, especially when
results need to be comparable across indicators.

## Usage

``` r
normalize_zscore(vec, na_on_fail = TRUE)
```

## Arguments

- vec:

  numeric vector to be normalized.

- na_on_fail:

  logical. if TRUE, returns a vector of NA_real\_ when normalization is
  undefined. if FALSE, throws an error.

## Value

a numeric vector of the same length as `vec`, containing z-scores or
NA_real\_ values if normalization is not possible.

## Details

the function fails fast on non-numeric input. for undefined
normalization cases (all missing values or zero variance), behavior is
controlled via `na_on_fail`.

## Examples

``` r
normalize_zscore(c(1, 2, 3, 4))
#> [1] -1.1618950 -0.3872983  0.3872983  1.1618950

normalize_zscore(c(5, 5, 5), na_on_fail = TRUE)
#> [1] NA NA NA

try(
  normalize_zscore(c(5, 5, 5), na_on_fail = FALSE)
)
#> Error in normalize_zscore(c(5, 5, 5), na_on_fail = FALSE) : 
#>   standard deviation is zero or NA. cannot normalize.

normalize_zscore(c(NA_real_, NA_real_))
#> [1] NA NA
```
