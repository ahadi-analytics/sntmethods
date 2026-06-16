# Convert SNT Incidence Object to Tibble

Extracts the data component from an `snt_incidence` object and returns
it as a tibble, discarding metadata.

## Usage

``` r
# S3 method for class 'snt_incidence'
as_tibble(x, ...)
```

## Arguments

- x:

  An object of class `snt_incidence`.

- ...:

  Additional arguments (ignored).

## Value

A tibble with incidence data.
