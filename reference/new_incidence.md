# Create a New SNT Incidence Object

Constructor for `snt_incidence` S3 class. Creates a structured object
containing incidence data with metadata tracking scale, method, formula,
and calculation parameters.

## Usage

``` r
new_incidence(data, scale, levels, formulas, version = "1.0.0")
```

## Arguments

- data:

  Tibble with incidence calculations (output from
  [`calc_incidence()`](https://ahadi-analytics.github.io/sntmethods/reference/calc_incidence.md)).

- scale:

  Numeric. Scale factor used (e.g., 1000, 10000).

- levels:

  Character vector of incidence levels calculated (e.g., c("N0", "N1")).

- formulas:

  Named list of formulas for each level calculated.

- version:

  Character. Package version used for calculation.

## Value

An object of class `snt_incidence` with:

- `data`: Tibble with incidence calculations

- `meta`: List of metadata (scale, levels, formulas, version,
  created_at)
