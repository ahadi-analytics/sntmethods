# Calculate Gini Coefficient Using DHS Brown Formula Methodology

Calculates the Gini coefficient for wealth inequality following the DHS
methodology. Uses the Brown formula with configurable number of wealth
groups (default 100) to provide a standardized measure of inequality.

## Usage

``` r
calculate_dhs_gini(wealth_scores, weights, population, n_groups = 100)
```

## Arguments

- wealth_scores:

  Numeric vector of wealth index factor scores (hv271).

- weights:

  Numeric vector of survey sampling weights.

- population:

  Numeric vector of household population sizes (de jure members).

- n_groups:

  Integer specifying the number of wealth groups for calculation
  (default: 100, following DHS methodology).

## Value

Numeric Gini coefficient value between 0 (perfect equality) and 1
(maximum inequality). Returns NA_real\_ if insufficient data.

## Examples

``` r
# Example with synthetic data
wealth_scores <- rnorm(100, mean = 0, sd = 1)
weights <- rep(1, 100)
population <- sample(3:8, 100, replace = TRUE)

gini <- calculate_dhs_gini(wealth_scores, weights, population)
```
